unit kayte_native;

// Native compiler for `kayte --native`: translates a program's bytecode to
// C and builds it with the system C compiler into a standalone executable.
//
// The generated C is one main() with a label for every instruction that
// is a jump/call target and a goto for every jump, preceded by the
// program's literals and SUB table and followed by an #include of the
// runtime (source/native/kayte_native_rt.c), which implements the value
// model and statements exactly as the bytecode VM does. So any program
// that runs with `kayte --run` - including QT programs, which reach
// libkayte_qt6 through the runtime - compiles with --native and behaves
// the same, but runs as native code with no interpreter.
//
// Indirect jumps (RETURN, and QT "run" calling a handler SUB) go through a
// switch over every label id they can reach, at the K_dispatch label.
//
// Requirements at compile time: a C compiler ($KAYTE_CC, default "cc")
// and the runtime source - looked up in $KAYTE_NATIVE_RT, next to the
// kayte executable, then in the source tree relative to it.

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Process, BytecodeTypes;

// Compiles AProgram to the executable OutputFile. Returns False and sets
// ErrorMsg on failure. With KeepC, the generated C is left next to the
// output (OutputFile + '.c') for inspection. An OutputFile ending in ".c"
// only writes the C (it #includes kayte_native_rt.c), to be built by
// another toolchain - e.g. into an iOS app by scripts/build-kayte-ios.sh.
function CompileToNative(AProgram: TByteCodeProgram; const OutputFile: string;
  KeepC, Verbose: Boolean; out ErrorMsg: string): Boolean;

// Shared with the LLVM backend (kayte_llvm.pas):
// The directory holding kayte_native_rt.c (with a trailing separator), or ''.
function FindRuntimeDir: string;
// String literal Index without the quotes it's stored with.
function LiteralText(AProgram: TByteCodeProgram; Index: Integer): string;

implementation

uses
  kayte_qt6;

const
  RuntimeFile = 'kayte_native_rt.c';

function FindRuntimeDir: string;
var
  ExeDir, Dir: string;
  Candidates: array of string;
begin
  ExeDir := ExtractFilePath(ExpandFileName(ParamStr(0)));
  Candidates := [GetEnvironmentVariable('KAYTE_NATIVE_RT'),
                 ExeDir,
                 ExeDir + '../source/native/',      // bin/kayte
                 ExeDir + '../../source/native/'];  // build/macos/kayte
  for Dir in Candidates do
    if (Dir <> '') and FileExists(IncludeTrailingPathDelimiter(Dir) + RuntimeFile) then
      Exit(ExpandFileName(IncludeTrailingPathDelimiter(Dir)));
  Result := '';
end;

// A C string literal for S, escaping everything but plain printable ASCII
// (octal escapes are always 3 digits, so a following digit can't extend
// them; '?' is escaped to rule out trigraphs).
function CLiteral(const S: string): string;
var
  C: Char;
begin
  Result := '"';
  for C in S do
    case C of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      '?': Result := Result + '\?';
      ' '..'!', '#'..'>', '@'..'[', ']'..'~': Result := Result + C;
    else
      Result := Result + '\' + OctStr(Ord(C), 3);
    end;
  Result := Result + '"';
end;

function CInt64(V: Int64): string;
begin
  if V = Low(Int64) then
    Result := '(-INT64_C(9223372036854775807) - 1)'
  else
    Result := 'INT64_C(' + IntToStr(V) + ')';
end;

// String literals are stored with their source quotes; strip them like
// TVirtualMachine.GetStringLiteral does.
function LiteralText(AProgram: TByteCodeProgram; Index: Integer): string;
begin
  if (Index < 0) or (Index >= AProgram.StringLiterals.Count) then
    raise Exception.CreateFmt('string literal index %d out of range', [Index]);
  Result := AProgram.StringLiterals[Index];
  if (Length(Result) >= 2) and (Result[1] = '"') and (Result[Length(Result)] = '"') then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

function GenerateC(AProgram: TByteCodeProgram): string;
var
  Code: TStringList;
  Instrs: TBCInstructionArray;
  Count, I, MaxVar, SubIdx: Integer;
  IsLabel, IsDispatch: array of Boolean;
  HasTry: Boolean;
  Ins: TBCInstruction;
  SubName, QtLib: string;

  // goto for a direct jump; a target at/after the end means "stop".
  function GotoTarget(Target: Integer): string;
  begin
    if (Target < 0) or (Target >= Count) then
      Result := 'goto K_end;'
    else
      Result := Format('goto L%d;', [Target]);
  end;

  procedure MarkLabel(Target: Integer; Dispatch: Boolean);
  begin
    if (Target >= 0) and (Target < Count) then
    begin
      IsLabel[Target] := True;
      if Dispatch then
        IsDispatch[Target] := True;
    end;
  end;

  procedure Emit(const Line: string);
  begin
    Code.Add('    ' + Line);
  end;

begin
  Instrs := AProgram.Instructions;
  Count := Length(Instrs);
  HasTry := False;
  IsLabel := nil;
  IsDispatch := nil;
  SetLength(IsLabel, Count);
  SetLength(IsDispatch, Count);

  // Labels are only emitted where something jumps to.
  for I := 0 to Count - 1 do
    case Instrs[I].OpCode of
      BC_JUMP, BC_JUMP_IF_FALSE:
        MarkLabel(Instrs[I].Operand1, False);
      BC_CALL:
        begin
          MarkLabel(Instrs[I].Operand1, False);
          MarkLabel(I + 1, True); // RETURN resumes here
        end;
      BC_QT:
        MarkLabel(I, True); // a QT "run" handler returns to its statement
      BC_TRY:
        begin
          MarkLabel(Instrs[I].Operand1, True); // a caught error resumes at its CATCH
          HasTry := True;
        end;
    end;
  for I := 0 to AProgram.SubroutineMap.Count - 1 do
    MarkLabel(AProgram.SubroutineMap.Data[I], True); // QT "on" handlers

  MaxVar := -1;
  for I := 0 to AProgram.VariableMap.Count - 1 do
    if AProgram.VariableMap.Data[I] > MaxVar then
      MaxVar := AProgram.VariableMap.Data[I];

  QtLib := FindQtLibrary;

  Code := TStringList.Create;
  try
    Code.Add('/* Generated by kayte --native from "' + AProgram.ProgramTitle + '". Do not edit. */');
    Code.Add('#include <stdint.h>');
    Code.Add('');
    Code.Add(Format('static const int K_nsubs = %d;', [AProgram.SubroutineMap.Count]));
    Code.Add('static const char *const K_sub_names[] = {');
    for I := 0 to AProgram.SubroutineMap.Count - 1 do
      Code.Add('    ' + CLiteral(AProgram.SubroutineMap.Keys[I]) + ',');
    Code.Add('    0};');
    Code.Add('static const int K_sub_addrs[] = {');
    for I := 0 to AProgram.SubroutineMap.Count - 1 do
      Code.Add(Format('    %d,', [AProgram.SubroutineMap.Data[I]]));
    Code.Add('    0};');
    // A SUB's entry is its BC_ENTER, whose operand is the parameter count.
    Code.Add('static const int K_sub_params[] = {');
    for I := 0 to AProgram.SubroutineMap.Count - 1 do
      Code.Add(Format('    %d,', [Instrs[AProgram.SubroutineMap.Data[I]].Operand1]));
    Code.Add('    0};');
    Code.Add('static const char *const K_qt_lib_fallback = ' + CLiteral(QtLib) + ';');
    Code.Add('');
    Code.Add('#include "' + RuntimeFile + '"');
    Code.Add('');
    Code.Add('int main(int argc, char **argv)');
    Code.Add('{');
    Code.Add('    int target = 0;');
    Code.Add('    (void)argc;');
    Code.Add(Format('    rt_init(%d, argv[0]);', [MaxVar + 1]));
    if HasTry then
    begin
      // rt_error longjmps back here when a TRY is active (see rt_catch).
      Code.Add('#ifndef KAYTE_NO_SETJMP');
      Code.Add('    if (setjmp(K_jb)) {');
      Code.Add('        target = K_catch_target;');
      Code.Add('        goto K_dispatch;');
      Code.Add('    }');
      Code.Add('#endif');
    end;

    for I := 0 to Count - 1 do
    begin
      Ins := Instrs[I];
      if IsLabel[I] then
        Code.Add(Format('L%d:', [I]));
      case Ins.OpCode of
        BC_NOP: ;
        BC_HALT: Emit('goto K_end;');
        BC_LOAD_INT:
          begin
            if (Ins.Operand1 < 0) or (Ins.Operand1 >= Length(AProgram.IntegerLiterals)) then
              raise Exception.CreateFmt('integer literal index %d out of range', [Ins.Operand1]);
            Emit('rt_push_int(' + CInt64(AProgram.IntegerLiterals[Ins.Operand1]) + ');');
          end;
        BC_LOAD_STRING:
          begin
            SubName := LiteralText(AProgram, Ins.Operand1);
            Emit(Format('rt_push_lit(%s, %d);', [CLiteral(SubName), Length(SubName)]));
          end;
        BC_LOAD_VAR: Emit(Format('rt_load(%d);', [Ins.Operand1]));
        BC_STORE_VAR, BC_ASSIGN: Emit(Format('rt_store(%d);', [Ins.Operand1]));
        BC_ADD: Emit('rt_add();');
        BC_SUB: Emit('rt_arith(0);');
        BC_MUL: Emit('rt_arith(1);');
        BC_DIV: Emit('rt_arith(2);');
        BC_CONCAT: Emit('rt_concat();');
        BC_NEG: Emit('rt_neg();');
        BC_NOT: Emit('rt_not();');
        BC_CMP_EQ: Emit('rt_cmp(0);');
        BC_CMP_NEQ: Emit('rt_cmp(1);');
        BC_CMP_LT: Emit('rt_cmp(2);');
        BC_CMP_GT: Emit('rt_cmp(3);');
        BC_CMP_LE: Emit('rt_cmp(4);');
        BC_CMP_GE: Emit('rt_cmp(5);');
        BC_PRINT: Emit(Format('rt_print(%d);', [Ins.Operand1]));
        BC_POP: Emit('rt_drop();');
        BC_JUMP: Emit(GotoTarget(Ins.Operand1));
        BC_JUMP_IF_FALSE: Emit('if (!rt_pop_truthy()) ' + GotoTarget(Ins.Operand1));
        BC_PROCESS: Emit(Format('rt_process(%d, %d);', [Ins.Operand1, Ins.Operand2]));
        BC_QT: Emit(Format('if (rt_qt(%d, %d, %d, &target, %d)) goto K_dispatch;',
          [Ins.Operand1, Ins.Operand2, I, Ord(Ins.Operand3 = QT_STATEMENT_QML)]));
        BC_CALL:
          if Ins.Operand1 < 0 then
            Emit('rt_error("CALL to an undefined SUB");')
          else
            Emit(Format('rt_call(%d, %d); %s', [I + 1, Ins.Operand2, GotoTarget(Ins.Operand1)]));
        BC_ENTER:
          begin
            SubName := '?';
            for SubIdx := 0 to AProgram.SubroutineMap.Count - 1 do
              if AProgram.SubroutineMap.Data[SubIdx] = I then
                SubName := AProgram.SubroutineMap.Keys[SubIdx];
            Emit(Format('rt_enter(%d, %s);', [Ins.Operand1, CLiteral(SubName)]));
          end;
        BC_RETURN: Emit('target = rt_return(); goto K_dispatch;');
        BC_INPUT: Emit('rt_input();');
        BC_QPRINT: Emit(Format('rt_qprint(%d);', [Ins.Operand1]));
        BC_LOAD_FLOAT: Emit('rt_push_flt(' + CInt64(Int64(QWord(LongWord(Ins.Operand1)) or
          (QWord(LongWord(Ins.Operand2)) shl 32))) + ');');
        BC_IDIV: Emit('rt_arith(3);');
        BC_MOD: Emit('rt_arith(4);');
        BC_INDEX_GET: Emit('rt_index_get();');
        BC_INDEX_SET: Emit('rt_index_set();');
        BC_BUILTIN: Emit(Format('rt_builtin(%d, %d);', [Ins.Operand1, Ins.Operand2]));
        BC_TRY: Emit(Format('rt_try(%d);', [Ins.Operand1]));
        BC_TRY_END: Emit('rt_try_end();');
        BC_THROW: Emit('rt_throw();');
      else
        raise Exception.CreateFmt('opcode %d is not supported by the native compiler', [Ord(Ins.OpCode)]);
      end;
    end;

    Code.Add('K_end:');
    Code.Add('    fflush(stdout);');
    Code.Add('    return 0;');
    Code.Add('K_dispatch:');
    Code.Add('    switch (target) {');
    for I := 0 to Count - 1 do
      if IsDispatch[I] then
        Code.Add(Format('    case %d: goto L%d;', [I, I]));
    Code.Add(Format('    case %d: goto K_end;', [Count])); // RETURN from a CALL that ended the program
    Code.Add('    default: rt_error("internal error: bad jump target %d", target);');
    Code.Add('    }');
    Code.Add('}');
    Result := Code.Text;
  finally
    Code.Free;
  end;
end;

function CompileToNative(AProgram: TByteCodeProgram; const OutputFile: string;
  KeepC, Verbose: Boolean; out ErrorMsg: string): Boolean;
var
  RtDir, CFile, CC, Output: string;
  Src: TStringList;
  Args: array of string;
begin
  Result := False;
  ErrorMsg := '';

  RtDir := FindRuntimeDir;
  if RtDir = '' then
  begin
    ErrorMsg := 'cannot find ' + RuntimeFile + ' - set KAYTE_NATIVE_RT to the directory containing it '
      + '(source/native in the Kayte repository)';
    Exit;
  end;

  if SameText(ExtractFileExt(OutputFile), '.c') then
    CFile := OutputFile
  else if KeepC then
    CFile := OutputFile + '.c'
  else
    CFile := GetTempFileName(GetTempDir, 'kayte') + '.c';

  Src := TStringList.Create;
  try
    try
      Src.Text := GenerateC(AProgram);
    except
      on E: Exception do
      begin
        ErrorMsg := 'cannot translate program: ' + E.Message;
        Exit;
      end;
    end;
    Src.SaveToFile(CFile);
  finally
    Src.Free;
  end;

  if CFile = OutputFile then
  begin
    if Verbose then
      Writeln('  Generated C only; the runtime it includes is in ', RtDir);
    Exit(True);
  end;

  CC := GetEnvironmentVariable('KAYTE_CC');
  if CC = '' then
    CC := 'cc';
  Args := ['-O2', '-std=c11', '-w', '-I', RtDir, CFile, '-o', OutputFile, '-lm'];
  {$IFDEF LINUX}
  Args := Concat(Args, ['-ldl']);
  {$ENDIF}

  if Verbose then
    Writeln('  ', CC, ' ', string.Join(' ', Args));

  try
    Output := '';
    if not RunCommand(CC, Args, Output, [poStderrToOutPut]) then
    begin
      ErrorMsg := 'C compiler "' + CC + '" failed:' + LineEnding + Output;
      Exit;
    end;
  finally
    if not KeepC then
      DeleteFile(CFile);
  end;

  if Verbose and KeepC then
    Writeln('  Generated C kept at ', CFile);
  Result := True;
end;

end.
