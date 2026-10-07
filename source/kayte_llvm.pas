unit kayte_llvm;

// LLVM backend for `kayte --llvm`: translates a program's bytecode to
// LLVM IR and builds it with clang, for the host or any --target triple.
//
// The IR mirrors what kayte_native.pas generates as C: every instruction
// is a call into the runtime (source/native/kayte_native_rt.c, built here
// with KAYTE_RT_LIBRARY so its rt_* entry points are exported), each
// instruction gets a basic block, jumps are branches, and indirect jumps
// (RETURN, and QT "run" calling a handler SUB) go through one switch at
// the "dispatch" block. LLVM's optimizer merges the blocks; the runtime
// keeps the value model identical to `kayte --run` and --native.
//
// Targets: anything clang and the runtime support - macOS, Linux, Windows
// (mingw), iOS / tvOS / watchOS / visionOS, and WebAssembly (WASI). For
// cross targets the right toolchain is picked where possible:
//   - Apple platforms: the SDK from `xcrun --sdk ... --show-sdk-path`
//   - "<triple>-clang" on PATH if there is one (e.g. llvm-mingw for Windows)
//   - WASI: the sysroot in $KAYTE_WASI_SYSROOT or Homebrew's wasi-libc
//   - Linux from another OS: lld, plus the sysroot you give
// Anything else goes in $KAYTE_LLVM_FLAGS (e.g. "--sysroot=/path"); the
// compiler is $KAYTE_CLANG if set, else clang.

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Process, BytecodeTypes;

// The program as LLVM IR text for Target (a triple, or '' for whatever the
// compiler defaults to). The IR defines kayte_main(start), which the
// runtime's main() calls (again, at a CATCH, after a caught error).
function GenerateLLVM(AProgram: TByteCodeProgram; const Target: string): string;

// Builds AProgram into the executable OutputFile for Target ('' = host).
// An OutputFile ending in ".ll" only writes the IR. With KeepIR the IR is
// left next to the output (OutputFile + '.ll'). Returns False and sets
// ErrorMsg on failure.
function CompileWithLLVM(AProgram: TByteCodeProgram; const OutputFile, Target: string;
  KeepIR, Verbose: Boolean; out ErrorMsg: string): Boolean;

implementation

uses
  kayte_native, kayte_qt6;

// An LLVM c"..." string body: printable ASCII as is, everything else
// (and " and \) as \XX hex escapes.
function IRBytes(const S: string): string;
var
  C: Char;
begin
  Result := '';
  for C in S do
    if (C >= ' ') and (C <= '~') and (C <> '"') and (C <> '\') then
      Result := Result + C
    else
      Result := Result + '\' + IntToHex(Ord(C), 2);
end;

function GenerateLLVM(AProgram: TByteCodeProgram; const Target: string): string;
var
  Code, Globals: TStringList;
  Instrs: TBCInstructionArray;
  Count, I, MaxVar, SubIdx, StrCount: Integer;
  IsDispatch: array of Boolean;
  Ins: TBCInstruction;
  Text, SubName, QtLib, Next, Cases, TripleLine: string;

  procedure MarkDispatch(Target: Integer);
  begin
    if (Target >= 0) and (Target < Count) then
      IsDispatch[Target] := True;
  end;

  // Block for a jump target; past the end (or negative) means "stop".
  function Block(Target: Integer): string;
  begin
    if (Target < 0) or (Target >= Count) then
      Result := '%end'
    else
      Result := '%L' + IntToStr(Target);
  end;

  // A private NUL-terminated constant; returns its name.
  function StrConst(const S: string): string;
  begin
    Result := '@.str.' + IntToStr(StrCount);
    Inc(StrCount);
    Globals.Add(Format('%s = private unnamed_addr constant [%d x i8] c"%s\00"',
      [Result, Length(S) + 1, IRBytes(S)]));
  end;

  procedure Emit(const Line: string);
  begin
    Code.Add('  ' + Line);
  end;

  procedure EmitError(const Msg: string);
  begin
    Emit(Format('call void (ptr, ...) @rt_error(ptr %s)', [StrConst(Msg)]));
    Emit('unreachable');
  end;

begin
  Instrs := AProgram.Instructions;
  Count := Length(Instrs);
  IsDispatch := nil;
  SetLength(IsDispatch, Count);
  StrCount := 0;

  // Blocks reachable through the dispatch switch: where RETURN resumes,
  // QT statements (a "run" handler returns to its statement) and SUB
  // entries (QT "on" handlers).
  for I := 0 to Count - 1 do
    case Instrs[I].OpCode of
      BC_CALL: MarkDispatch(I + 1);
      BC_QT: MarkDispatch(I);
      BC_TRY: MarkDispatch(Instrs[I].Operand1); // a caught error resumes at its CATCH
    end;
  for I := 0 to AProgram.SubroutineMap.Count - 1 do
    MarkDispatch(AProgram.SubroutineMap.Data[I]);

  MaxVar := -1;
  for I := 0 to AProgram.VariableMap.Count - 1 do
    if AProgram.VariableMap.Data[I] > MaxVar then
      MaxVar := AProgram.VariableMap.Data[I];

  // Only meaningful when the program runs on this machine; elsewhere the
  // library is found next to the program or on the library path.
  if Target = '' then
    QtLib := FindQtLibrary
  else
    QtLib := '';

  if Target <> '' then
    TripleLine := 'target triple = "' + Target + '"' + LineEnding + LineEnding
  else
    TripleLine := '';

  Globals := TStringList.Create;
  Code := TStringList.Create;
  try
    // The SUB table and Qt fallback the runtime reads (extern there).
    Text := '';
    for I := 0 to AProgram.SubroutineMap.Count - 1 do
      Text := Text + 'ptr ' + StrConst(AProgram.SubroutineMap.Keys[I]) + ', ';
    Globals.Add(Format('@K_nsubs = constant i32 %d', [AProgram.SubroutineMap.Count]));
    Globals.Add(Format('@K_sub_names = constant [%d x ptr] [%sptr null]', [AProgram.SubroutineMap.Count + 1, Text]));
    Text := '';
    for I := 0 to AProgram.SubroutineMap.Count - 1 do
      Text := Text + 'i32 ' + IntToStr(AProgram.SubroutineMap.Data[I]) + ', ';
    Globals.Add(Format('@K_sub_addrs = constant [%d x i32] [%si32 0]', [AProgram.SubroutineMap.Count + 1, Text]));
    // A SUB's entry is its BC_ENTER, whose operand is the parameter count.
    Text := '';
    for I := 0 to AProgram.SubroutineMap.Count - 1 do
      Text := Text + 'i32 ' + IntToStr(Instrs[AProgram.SubroutineMap.Data[I]].Operand1) + ', ';
    Globals.Add(Format('@K_sub_params = constant [%d x i32] [%si32 0]', [AProgram.SubroutineMap.Count + 1, Text]));
    Globals.Add('@K_qt_lib_fallback = constant ptr ' + StrConst(QtLib));
    Globals.Add(Format('@K_nvars_init = constant i32 %d', [MaxVar + 1]));

    // start < 0: from the beginning; else the label of a CATCH.
    Code.Add('define i32 @kayte_main(i32 %start) {');
    Code.Add('entry:');
    Emit('%target = alloca i32');
    Emit('store i32 %start, ptr %target');
    Emit('%fresh = icmp slt i32 %start, 0');
    Emit('br i1 %fresh, label ' + Block(0) + ', label %dispatch');

    for I := 0 to Count - 1 do
    begin
      Ins := Instrs[I];
      Code.Add(Format('L%d:', [I]));
      Next := 'br label ' + Block(I + 1); // most instructions fall through
      case Ins.OpCode of
        BC_NOP: ;
        BC_HALT: Next := 'br label %end';
        BC_LOAD_INT:
          begin
            if (Ins.Operand1 < 0) or (Ins.Operand1 > High(AProgram.IntegerLiterals)) then
              raise Exception.CreateFmt('integer literal index %d out of range', [Ins.Operand1]);
            Emit(Format('call void @rt_push_int(i64 %d)', [AProgram.IntegerLiterals[Ins.Operand1]]));
          end;
        BC_LOAD_STRING:
          begin
            Text := LiteralText(AProgram, Ins.Operand1);
            Emit(Format('call void @rt_push_lit(ptr %s, i64 %d)', [StrConst(Text), Length(Text)]));
          end;
        BC_LOAD_VAR: Emit(Format('call void @rt_load(i32 %d)', [Ins.Operand1]));
        BC_STORE_VAR, BC_ASSIGN: Emit(Format('call void @rt_store(i32 %d)', [Ins.Operand1]));
        BC_ADD: Emit('call void @rt_add()');
        BC_SUB: Emit('call void @rt_arith(i32 0)');
        BC_MUL: Emit('call void @rt_arith(i32 1)');
        BC_DIV: Emit('call void @rt_arith(i32 2)');
        BC_CONCAT: Emit('call void @rt_concat()');
        BC_NEG: Emit('call void @rt_neg()');
        BC_NOT: Emit('call void @rt_not()');
        BC_CMP_EQ: Emit('call void @rt_cmp(i32 0)');
        BC_CMP_NEQ: Emit('call void @rt_cmp(i32 1)');
        BC_CMP_LT: Emit('call void @rt_cmp(i32 2)');
        BC_CMP_GT: Emit('call void @rt_cmp(i32 3)');
        BC_CMP_LE: Emit('call void @rt_cmp(i32 4)');
        BC_CMP_GE: Emit('call void @rt_cmp(i32 5)');
        BC_PRINT: Emit(Format('call void @rt_print(i32 %d)', [Ins.Operand1]));
        BC_POP: Emit('call void @rt_drop()');
        BC_JUMP: Next := 'br label ' + Block(Ins.Operand1);
        BC_JUMP_IF_FALSE:
          begin
            Emit(Format('%%c%d = call i32 @rt_pop_truthy()', [I]));
            Emit(Format('%%f%d = icmp eq i32 %%c%d, 0', [I, I]));
            Next := Format('br i1 %%f%d, label %s, label %s', [I, Block(Ins.Operand1), Block(I + 1)]);
          end;
        BC_PROCESS: Emit(Format('call void @rt_process(i32 %d, i32 %d)', [Ins.Operand1, Ins.Operand2]));
        BC_QT:
          begin
            // Returns 1 when "run" is calling a handler SUB: then jump to
            // the target it stored, which returns here afterwards.
            Emit(Format('%%q%d = call i32 @rt_qt(i32 %d, i32 %d, i32 %d, ptr %%target, i32 %d)',
              [I, Ins.Operand1, Ins.Operand2, I, Ord(Ins.Operand3 = QT_STATEMENT_QML)]));
            Emit(Format('%%j%d = icmp ne i32 %%q%d, 0', [I, I]));
            Next := Format('br i1 %%j%d, label %%dispatch, label %s', [I, Block(I + 1)]);
          end;
        BC_CALL:
          if Ins.Operand1 < 0 then
          begin
            EmitError('CALL to an undefined SUB');
            Next := '';
          end
          else
          begin
            Emit(Format('call void @rt_call(i32 %d, i32 %d)', [I + 1, Ins.Operand2]));
            Next := 'br label ' + Block(Ins.Operand1);
          end;
        BC_ENTER:
          begin
            SubName := '?';
            for SubIdx := 0 to AProgram.SubroutineMap.Count - 1 do
              if AProgram.SubroutineMap.Data[SubIdx] = I then
                SubName := AProgram.SubroutineMap.Keys[SubIdx];
            Emit(Format('call void @rt_enter(i32 %d, ptr %s)', [Ins.Operand1, StrConst(SubName)]));
          end;
        BC_RETURN:
          begin
            Emit(Format('%%r%d = call i32 @rt_return()', [I]));
            Emit(Format('store i32 %%r%d, ptr %%target', [I]));
            Next := 'br label %dispatch';
          end;
        BC_INPUT: Emit('call void @rt_input()');
        BC_QPRINT: Emit(Format('call void @rt_qprint(i32 %d)', [Ins.Operand1]));
        BC_LOAD_FLOAT: Emit(Format('call void @rt_push_flt(i64 %d)', [Int64(QWord(LongWord(Ins.Operand1)) or
          (QWord(LongWord(Ins.Operand2)) shl 32))]));
        BC_IDIV: Emit('call void @rt_arith(i32 3)');
        BC_MOD: Emit('call void @rt_arith(i32 4)');
        BC_INDEX_GET: Emit('call void @rt_index_get()');
        BC_INDEX_SET: Emit('call void @rt_index_set()');
        BC_BUILTIN: Emit(Format('call void @rt_builtin(i32 %d, i32 %d)', [Ins.Operand1, Ins.Operand2]));
        BC_TRY: Emit(Format('call void @rt_try(i32 %d)', [Ins.Operand1]));
        BC_TRY_END: Emit('call void @rt_try_end()');
        BC_THROW:
          begin
            Emit('call void @rt_throw()');
            Emit('unreachable');
            Next := '';
          end;
      else
        raise Exception.CreateFmt('opcode %d is not supported by the LLVM backend', [Ord(Ins.OpCode)]);
      end;
      if Next <> '' then
        Emit(Next);
    end;

    Code.Add('dispatch:');
    Emit('%t = load i32, ptr %target');
    Cases := '';
    for I := 0 to Count - 1 do
      if IsDispatch[I] then
        Cases := Cases + Format(' i32 %d, label %%L%d', [I, I]);
    // RETURN from a CALL that was the program's last instruction.
    Cases := Cases + Format(' i32 %d, label %%end', [Count]);
    Emit('switch i32 %t, label %bad [' + Cases + ' ]');
    Code.Add('bad:');
    Emit(Format('call void (ptr, ...) @rt_error(ptr %s, i32 %%t)', [StrConst('internal error: bad jump target %d')]));
    Emit('unreachable');
    Code.Add('end:');
    Emit('%rc = call i32 @rt_finish()');
    Emit('ret i32 %rc');
    Code.Add('}');

    Result :=
      '; Generated by kayte --llvm from "' + AProgram.ProgramTitle + '". Do not edit.' + LineEnding +
      '; Link with source/native/kayte_native_rt.c built with -DKAYTE_RT_LIBRARY.' + LineEnding + LineEnding +
      TripleLine +
      Globals.Text + LineEnding +
      'declare void @rt_input()' + LineEnding +
      'declare void @rt_push_flt(i64)' + LineEnding +
      'declare void @rt_qprint(i32)' + LineEnding +
      'declare void @rt_index_get()' + LineEnding +
      'declare void @rt_index_set()' + LineEnding +
      'declare void @rt_builtin(i32, i32)' + LineEnding +
      'declare void @rt_try(i32)' + LineEnding +
      'declare void @rt_try_end()' + LineEnding +
      'declare void @rt_throw() noreturn' + LineEnding +
      'declare void @rt_push_int(i64)' + LineEnding +
      'declare void @rt_push_lit(ptr, i64)' + LineEnding +
      'declare void @rt_load(i32)' + LineEnding +
      'declare void @rt_store(i32)' + LineEnding +
      'declare void @rt_add()' + LineEnding +
      'declare void @rt_arith(i32)' + LineEnding +
      'declare void @rt_concat()' + LineEnding +
      'declare void @rt_neg()' + LineEnding +
      'declare void @rt_not()' + LineEnding +
      'declare void @rt_cmp(i32)' + LineEnding +
      'declare void @rt_print(i32)' + LineEnding +
      'declare void @rt_drop()' + LineEnding +
      'declare i32 @rt_pop_truthy()' + LineEnding +
      'declare void @rt_process(i32, i32)' + LineEnding +
      'declare i32 @rt_qt(i32, i32, i32, ptr, i32)' + LineEnding +
      'declare void @rt_call(i32, i32)' + LineEnding +
      'declare void @rt_enter(i32, ptr)' + LineEnding +
      'declare i32 @rt_return()' + LineEnding +
      'declare i32 @rt_finish()' + LineEnding +
      'declare void @rt_error(ptr, ...) noreturn' + LineEnding + LineEnding +
      Code.Text;
  finally
    Code.Free;
    Globals.Free;
  end;
end;

// Runs a command, returning its trimmed stdout ('' on failure).
function CommandOutput(const Exe: string; const Args: array of string): string;
begin
  Result := '';
  if RunCommand(Exe, Args, Result, [poNoConsole]) then
    Result := Trim(Result)
  else
    Result := '';
end;

// The Apple SDK name for a triple, or '' if it isn't an Apple target.
function AppleSdk(const Target: string): string;
var
  T: string;
begin
  T := LowerCase(Target);
  Result := '';
  if Pos('-apple-', T) = 0 then
    Exit;
  if Pos('-apple-ios', T) > 0 then
  begin
    if Pos('simulator', T) > 0 then Result := 'iphonesimulator' else Result := 'iphoneos';
  end
  else if Pos('-apple-tvos', T) > 0 then
  begin
    if Pos('simulator', T) > 0 then Result := 'appletvsimulator' else Result := 'appletvos';
  end
  else if Pos('-apple-watchos', T) > 0 then
  begin
    if Pos('simulator', T) > 0 then Result := 'watchsimulator' else Result := 'watchos';
  end
  else if Pos('-apple-xros', T) > 0 then
  begin
    if Pos('simulator', T) > 0 then Result := 'xrsimulator' else Result := 'xros';
  end
  else
    Result := 'macosx';
end;

function WasiSysroot: string;
var
  Prefix: string;
begin
  Result := GetEnvironmentVariable('KAYTE_WASI_SYSROOT');
  if Result <> '' then
    Exit;
  Prefix := CommandOutput('brew', ['--prefix', 'wasi-libc']);
  if (Prefix <> '') and DirectoryExists(Prefix + '/share/wasi-sysroot') then
    Result := Prefix + '/share/wasi-sysroot';
end;

// Splits $KAYTE_LLVM_FLAGS on spaces.
function ExtraFlags: TStringArray;
var
  Parts: TStringList;
  I: Integer;
begin
  Result := nil;
  Parts := TStringList.Create;
  try
    Parts.Delimiter := ' ';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := GetEnvironmentVariable('KAYTE_LLVM_FLAGS');
    for I := 0 to Parts.Count - 1 do
      if Parts[I] <> '' then
        Result := Concat(Result, [Parts[I]]);
  finally
    Parts.Free;
  end;
end;

function HasTry(AProgram: TByteCodeProgram): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AProgram.Instructions) do
    if AProgram.Instructions[I].OpCode = BC_TRY then
      Exit(True);
  Result := False;
end;

function CompileWithLLVM(AProgram: TByteCodeProgram; const OutputFile, Target: string;
  KeepIR, Verbose: Boolean; out ErrorMsg: string): Boolean;
var
  RtDir, IRFile, CC, Output, Sdk, SdkPath, Sysroot, Triple, IRTriple: string;
  Src: TStringList;
  Args: array of string;
begin
  Result := False;
  ErrorMsg := '';
  Triple := LowerCase(Target);
  // "wasm32-wasi" is the old name of WASI preview 1; current sysroots only
  // have headers for "wasm32-wasip1" (and p2, ...).
  if Triple = 'wasm32-wasi' then
    Triple := 'wasm32-wasip1';

  RtDir := FindRuntimeDir;
  if RtDir = '' then
  begin
    ErrorMsg := 'cannot find kayte_native_rt.c - set KAYTE_NATIVE_RT to the directory containing it '
      + '(source/native in the Kayte repository)';
    Exit;
  end;

  // Compiler: $KAYTE_CLANG, else a "<triple>-clang" cross compiler on PATH
  // (llvm-mingw names them so), else clang.
  CC := GetEnvironmentVariable('KAYTE_CLANG');
  if (CC = '') and (Triple <> '') then
    CC := ExeSearch(Triple + '-clang', GetEnvironmentVariable('PATH'));
  if CC = '' then
    CC := 'clang';

  // The IR records its target in clang's normalized spelling (e.g.
  // aarch64-unknown-linux-gnu), so compiling it raises no mismatch warning.
  IRTriple := '';
  if Triple <> '' then
  begin
    if ExtractFileName(CC) = Triple + '-clang' then
      IRTriple := CommandOutput(CC, ['-print-target-triple'])
    else
      IRTriple := CommandOutput(CC, ['-target', Triple, '-print-target-triple']);
    if IRTriple = '' then
      IRTriple := Triple;
  end;

  if SameText(ExtractFileExt(OutputFile), '.ll') then
    IRFile := OutputFile
  else if KeepIR then
    IRFile := OutputFile + '.ll'
  else
    IRFile := GetTempFileName(GetTempDir, 'kayte') + '.ll';

  Src := TStringList.Create;
  try
    try
      Src.Text := GenerateLLVM(AProgram, IRTriple);
    except
      on E: Exception do
      begin
        ErrorMsg := 'cannot translate program: ' + E.Message;
        Exit;
      end;
    end;
    Src.SaveToFile(IRFile);
  finally
    Src.Free;
  end;

  if IRFile = OutputFile then
  begin
    if Verbose then
      Writeln('  Generated LLVM IR only; link it with ', RtDir, 'kayte_native_rt.c built with -DKAYTE_RT_LIBRARY');
    Exit(True);
  end;

  Args := ['-O2', '-std=c11', '-w', '-DKAYTE_RT_LIBRARY', '-I', RtDir, RtDir + 'kayte_native_rt.c', IRFile,
           '-o', OutputFile, '-lm'];
  if (Triple <> '') and (ExtractFileName(CC) <> Triple + '-clang') then
    Args := Concat(['-target', Triple], Args);

  Sdk := AppleSdk(Triple);
  if Sdk <> '' then
  begin
    SdkPath := CommandOutput('xcrun', ['--sdk', Sdk, '--show-sdk-path']);
    if SdkPath = '' then
    begin
      ErrorMsg := 'the ' + Sdk + ' SDK was not found (xcrun --sdk ' + Sdk + ' --show-sdk-path) - install Xcode';
      Exit;
    end;
    Args := Concat(Args, ['-isysroot', SdkPath]);
    if Sdk <> 'macosx' then
      Args := Concat(Args, ['-framework', 'CoreFoundation']);
  end
  else if Pos('wasi', Triple) > 0 then
  begin
    Sysroot := WasiSysroot;
    if Sysroot = '' then
    begin
      ErrorMsg := 'no WASI sysroot - install one (brew install wasi-libc wasi-runtimes) or set KAYTE_WASI_SYSROOT';
      Exit;
    end;
    Args := Concat(Args, ['--sysroot=' + Sysroot]);
    // TRY needs setjmp / longjmp, which WebAssembly has through its
    // exception handling (run it with e.g. "wasmtime -W exceptions=y").
    // Programs without TRY don't depend on that.
    if HasTry(AProgram) then
      Args := Concat(Args, ['-DKAYTE_WASM_SJLJ', '-mllvm', '-wasm-enable-sjlj',
        '-mllvm', '-wasm-use-legacy-eh=false', '-lsetjmp']);
  end
  else if Pos('linux', Triple) > 0 then
  begin
    Args := Concat(Args, ['-ldl']);
    {$IFNDEF LINUX}
    Args := Concat(Args, ['-fuse-ld=lld']); // cross: the host's linker can't make ELF
    {$ENDIF}
  end
  else if Triple = '' then
  begin
    {$IFDEF LINUX}
    Args := Concat(Args, ['-ldl']);
    {$ENDIF}
  end;
  Args := Concat(Args, ExtraFlags);

  if Verbose then
    Writeln('  ', CC, ' ', string.Join(' ', Args));

  try
    Output := '';
    if not RunCommand(CC, Args, Output, [poStderrToOutPut]) then
    begin
      ErrorMsg := 'clang ("' + CC + '") failed:' + LineEnding + Output;
      if (Pos('linux', Triple) > 0) and (Pos('sysroot', Output + GetEnvironmentVariable('KAYTE_LLVM_FLAGS')) = 0) then
        ErrorMsg := ErrorMsg + LineEnding + 'Cross-compiling for Linux needs its headers and libraries: '
          + 'set KAYTE_LLVM_FLAGS="--sysroot=<linux sysroot>".';
      Exit;
    end;
  finally
    if (IRFile <> OutputFile) and not KeepIR then
      DeleteFile(IRFile);
  end;

  if Verbose and KeepIR then
    Writeln('  LLVM IR kept at ', IRFile);
  Result := True;
end;

end.
