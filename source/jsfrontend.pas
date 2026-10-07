unit jsfrontend;

// JavaScript-like front end for Kayte: compiles .kjs (or .js) source to the
// same bytecode as the BASIC parser (source/parser.pas), so the program runs
// on the VM and builds with --native / --llvm / for iOS unchanged.
//
//   function greet(who, times) {
//     for (let i = 1; i <= times; i++) {
//       console.log("hello " + who + " #" + i);
//     }
//     return times * 2;
//   }
//   let n = greet("Ada", 2);
//   if (n > 3 && n % 2 == 0) print("even:", n); else print("odd");
//
// Supported: var / let / const, function declarations with return values,
// if / else, while, do...while, for (and the counting form
// "for i from 1 to 10 [step 2] { ... }"), break / continue, "call f()" (an
// optional prefix for a call statement), blocks, the
// operators + - * / % == != === !== < > <= >= && || ! ?: += -= *= /= %=
// ++ --, integers, strings ("..." / '...' / `...` without ${}), true / false
// / null, and comments. Built-ins: print / console.log, alert / msgbox,
// qt(...), qml(...) and process(...) - the QT, QML and PROCESS statements,
// which return their result when used in an expression.
//
// How it maps onto the bytecode (which has no locals, return values or
// boolean operators of its own):
//   - Functions are SUBs. "return x" stores x in a hidden variable that the
//     caller reads right after its CALL.
//   - Parameters and var / let / const inside a function are local to it
//     (stored as "<function>:<name>"); other names are globals, as in JS.
//     A function saves its locals on the evaluation stack when called and
//     restores them on return, so each call has its own - recursion works.
//   - &&, ||, ?: and % compile to jumps / arithmetic on the existing opcodes.
//   - Numbers are 64-bit integers ("/" divides integers).
//
// Errors are reported like the BASIC parser's - all of them, with
// line:column - and then compilation stops (EKayteParseError).

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fgl, BytecodeTypes, Assembler, Parser;

// True for file names the JavaScript-like front end compiles (.kjs, .js).
function IsJSSourceFile(const FileName: string): Boolean;

// Compiles Source to a bytecode program (the caller owns it). FileName is
// only used in error messages. Prints each error and raises
// EKayteParseError if there were any.
function CompileJS(const Source, FileName: string): TByteCodeProgram;

implementation

type
  TJSTokenKind = (jtEOF, jtIdent, jtNumber, jtString, jtPunct);

  TJSToken = record
    Kind: TJSTokenKind;
    Text: string;   // identifier / punctuator text, or the decoded string
    Line, Col: Integer;
  end;

  TIntList = specialize TFPGList<Integer>;

  // Jumps to patch when a loop ends (break) or reaches its next iteration
  // (continue).
  TLoop = class
    Breaks, Continues: TIntList;
    constructor Create;
    destructor Destroy; override;
  end;

  TPendingJSCall = record
    Key: string;  // upper-case function name (SUB key)
    Name: string; // as written, for messages
    ArgCount, InstrIndex, Line, Col: Integer;
  end;

  EJSError = class(Exception)
  public
    Line, Col: Integer;
  end;

  TJSCompiler = class
  private
    FSrc, FFile: string;
    FPos, FLine, FCol: Integer;
    FTok, FNext: TJSToken;
    FAsm: TAssembler;
    FErrors: Integer;
    FFunction: string;          // function being compiled, '' at top level
    FLocals: TStringList;       // its parameters, declared names and temps
    FReturnJumps: array of Integer; // its "return"s, to patch to its exit code
    FConsts: TStringList;       // const names in scope (qualified)
    FLoops: TFPList;            // of TLoop
    FParamCounts: TStringList;  // SUB key -> parameter count (Objects)
    FCalls: array of TPendingJSCall;
    FTempCount: Integer;

    procedure Fail(const Msg: string; Line, Col: Integer);
    procedure FailHere(const Msg: string);

    // Lexer
    function ScanToken: TJSToken;
    procedure Advance;
    function IsPunct(const P: string): Boolean;
    function IsIdent(const Name: string): Boolean;
    procedure Expect(const P: string);
    function ExpectIdent(const What: string): string;
    procedure SkipSemicolon;

    // Code generation helpers
    function Here: Integer;
    procedure Emit(Op: TByteCodeOp; const Operands: array of Integer);
    function EmitJump(Op: TByteCodeOp): Integer;
    procedure PatchTo(InstrIndex, Target: Integer);
    procedure PushInt(V: Int64);
    procedure PushStr(const S: string);
    function VarSlot(const Name: string): Integer;
    function NewTemp: Integer;
    function Resolve(const Name: string): string;
    procedure Declare(const Name: string; IsConst: Boolean);
    procedure CheckAssignable(const Name: string);

    // Statements
    procedure Statement;
    procedure Block;
    procedure VarDeclaration(IsConst: Boolean);
    procedure FunctionDeclaration;
    procedure IfStatement;
    procedure WhileStatement;
    procedure DoWhileStatement;
    procedure ForStatement;
    procedure BreakContinue(IsBreak: Boolean);
    procedure ReturnStatement;
    procedure SimpleStatement;
    procedure Recover;

    // Expressions (each leaves one value on the stack)
    procedure Expression;
    procedure Conditional;
    procedure LogicalOr;
    procedure LogicalAnd;
    procedure Equality;
    procedure Relational;
    procedure Additive;
    procedure Multiplicative;
    procedure UnaryExpr;
    procedure Primary;
    function Arguments: Integer;
    // A call by name; with WantValue its result is left on the stack.
    procedure CallByName(const Name: string; Line, Col: Integer; WantValue: Boolean);
    function IsBuiltin(const Name: string): Boolean;

    procedure ResolveCalls;
  public
    constructor Create(const Source, FileName: string);
    destructor Destroy; override;
    function Compile: TByteCodeProgram;
  end;

const
  RetVar = '__ret'; // a function's return value, read right after its CALL

{ TLoop }

constructor TLoop.Create;
begin
  Breaks := TIntList.Create;
  Continues := TIntList.Create;
end;

destructor TLoop.Destroy;
begin
  Breaks.Free;
  Continues.Free;
  inherited Destroy;
end;

{ TJSCompiler }

constructor TJSCompiler.Create(const Source, FileName: string);
begin
  inherited Create;
  FSrc := Source;
  FFile := FileName;
  FPos := 1;
  FLine := 1;
  FCol := 1;
  FAsm := TAssembler.Create;
  FAsm.SetProgramTitle('Kayte Program');
  FLocals := TStringList.Create;
  FLocals.CaseSensitive := True;
  FConsts := TStringList.Create;
  FConsts.CaseSensitive := True;
  FLoops := TFPList.Create;
  FParamCounts := TStringList.Create;
end;

destructor TJSCompiler.Destroy;
var
  I: Integer;
begin
  for I := 0 to FLoops.Count - 1 do
    TLoop(FLoops[I]).Free;
  FLoops.Free;
  FParamCounts.Free;
  FConsts.Free;
  FLocals.Free;
  FAsm.Free; // frees the program unless Compile handed it over
  inherited Destroy;
end;

procedure TJSCompiler.Fail(const Msg: string; Line, Col: Integer);
var
  E: EJSError;
begin
  E := EJSError.CreateFmt('%s:%d:%d: %s', [FFile, Line, Col, Msg]);
  E.Line := Line;
  E.Col := Col;
  raise E;
end;

procedure TJSCompiler.FailHere(const Msg: string);
var
  Found: string;
begin
  case FTok.Kind of
    jtEOF: Found := 'end of file';
    jtString: Found := 'a string';
  else
    Found := '"' + FTok.Text + '"';
  end;
  Fail(Msg + ', found ' + Found, FTok.Line, FTok.Col);
end;

{ ---- Lexer ---- }

function TJSCompiler.ScanToken: TJSToken;
const
  Puncts: array[0..30] of string = ('===', '!==', '==', '!=', '<=', '>=', '&&', '||', '++', '--', '+=', '-=',
    '*=', '/=', '%=', '{', '}', '(', ')', '[', ']', ';', ',', '.', '?', ':', '=', '<', '>', '!', '+');
  Singles = '-*/%';
var
  C, Quote: Char;
  S: string;
  I: Integer;

  function Peek(Offset: Integer): Char;
  begin
    if FPos + Offset <= Length(FSrc) then
      Result := FSrc[FPos + Offset]
    else
      Result := #0;
  end;

  procedure Step;
  begin
    if FSrc[FPos] = #10 then
    begin
      Inc(FLine);
      FCol := 1;
    end
    else
      Inc(FCol);
    Inc(FPos);
  end;

begin
  // Whitespace and comments
  while FPos <= Length(FSrc) do
  begin
    C := FSrc[FPos];
    if C in [' ', #9, #10, #13] then
      Step
    else if (C = '/') and (Peek(1) = '/') then
      while (FPos <= Length(FSrc)) and (FSrc[FPos] <> #10) do
        Step
    else if (C = '/') and (Peek(1) = '*') then
    begin
      Result.Line := FLine;
      Result.Col := FCol;
      Step;
      Step;
      while (FPos <= Length(FSrc)) and not ((FSrc[FPos] = '*') and (Peek(1) = '/')) do
        Step;
      if FPos > Length(FSrc) then
        Fail('unterminated /* comment', Result.Line, Result.Col);
      Step;
      Step;
    end
    else
      Break;
  end;

  Result.Line := FLine;
  Result.Col := FCol;
  Result.Text := '';
  if FPos > Length(FSrc) then
  begin
    Result.Kind := jtEOF;
    Exit;
  end;

  C := FSrc[FPos];
  if C in ['A'..'Z', 'a'..'z', '_', '$'] then
  begin
    Result.Kind := jtIdent;
    while (FPos <= Length(FSrc)) and (FSrc[FPos] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$']) do
    begin
      Result.Text := Result.Text + FSrc[FPos];
      Step;
    end;
  end
  else if C in ['0'..'9'] then
  begin
    Result.Kind := jtNumber;
    while (FPos <= Length(FSrc)) and (FSrc[FPos] in ['0'..'9', '_']) do
    begin
      if FSrc[FPos] <> '_' then
        Result.Text := Result.Text + FSrc[FPos];
      Step;
    end;
    if (FPos <= Length(FSrc)) and (FSrc[FPos] = '.') and (Peek(1) in ['0'..'9']) then
      Fail('floating-point numbers aren''t supported yet (numbers are integers)', Result.Line, Result.Col);
  end
  else if C in ['"', '''', '`'] then
  begin
    Result.Kind := jtString;
    Quote := C;
    Step;
    S := '';
    while (FPos <= Length(FSrc)) and (FSrc[FPos] <> Quote) do
    begin
      C := FSrc[FPos];
      if (C = #10) and (Quote <> '`') then
        Fail('unterminated string', Result.Line, Result.Col);
      if (Quote = '`') and (C = '$') and (Peek(1) = '{') then
        Fail('${...} in template strings isn''t supported yet - use "text " + value', FLine, FCol);
      if C = '\' then
      begin
        Step;
        if FPos > Length(FSrc) then
          Break;
        C := FSrc[FPos];
        case C of
          'n': C := #10;
          't': C := #9;
          'r': C := #13;
          '0': C := #0;
        end;
      end;
      S := S + C;
      Step;
    end;
    if FPos > Length(FSrc) then
      Fail('unterminated string', Result.Line, Result.Col);
    Step; // closing quote
    Result.Text := S;
  end
  else
  begin
    Result.Kind := jtPunct;
    for I := Low(Puncts) to High(Puncts) do
      if Copy(FSrc, FPos, Length(Puncts[I])) = Puncts[I] then
      begin
        Result.Text := Puncts[I];
        Break;
      end;
    if (Result.Text = '') and (Pos(C, Singles) > 0) then
      Result.Text := C;
    if Result.Text = '' then
      Fail('unexpected character "' + C + '"', FLine, FCol);
    for I := 1 to Length(Result.Text) do
      Step;
  end;
end;

procedure TJSCompiler.Advance;
begin
  FTok := FNext;
  FNext := ScanToken;
end;

function TJSCompiler.IsPunct(const P: string): Boolean;
begin
  Result := (FTok.Kind = jtPunct) and (FTok.Text = P);
end;

function TJSCompiler.IsIdent(const Name: string): Boolean;
begin
  Result := (FTok.Kind = jtIdent) and (FTok.Text = Name);
end;

procedure TJSCompiler.Expect(const P: string);
begin
  if not IsPunct(P) then
    FailHere('expected "' + P + '"');
  Advance;
end;

function TJSCompiler.ExpectIdent(const What: string): string;
begin
  if FTok.Kind <> jtIdent then
    FailHere('expected ' + What);
  Result := FTok.Text;
  Advance;
end;

// Semicolons end statements but are optional, as in JavaScript.
procedure TJSCompiler.SkipSemicolon;
begin
  if IsPunct(';') then
    Advance;
end;

{ ---- Code generation helpers ---- }

function TJSCompiler.Here: Integer;
begin
  Result := FAsm.CurrentInstructionIndex;
end;

procedure TJSCompiler.Emit(Op: TByteCodeOp; const Operands: array of Integer);
begin
  FAsm.Emit(Op, Operands);
end;

function TJSCompiler.EmitJump(Op: TByteCodeOp): Integer;
begin
  Result := Here;
  Emit(Op, [-1]);
end;

procedure TJSCompiler.PatchTo(InstrIndex, Target: Integer);
begin
  FAsm.PatchJumpTarget(InstrIndex, Target);
end;

procedure TJSCompiler.PushInt(V: Int64);
begin
  Emit(BC_LOAD_INT, [FAsm.CurrentProgram.AddIntegerLiteral(V)]);
end;

procedure TJSCompiler.PushStr(const S: string);
begin
  // String literals are stored with surrounding quotes, which the VM and
  // the native backends strip.
  Emit(BC_LOAD_STRING, [FAsm.CurrentProgram.AddStringConstant('"' + S + '"')]);
end;

function TJSCompiler.VarSlot(const Name: string): Integer;
begin
  Result := FAsm.CurrentProgram.AddVariable(Name);
end;

// A variable only the compiler uses. Inside a function it's one of the
// function's locals, so a recursive call can't overwrite it. ('#' can't
// appear in a JavaScript name, so it never clashes with the program's.)
function TJSCompiler.NewTemp: Integer;
var
  Name: string;
begin
  Inc(FTempCount);
  Name := '#t' + IntToStr(FTempCount);
  if (FFunction <> '') and (FLocals.IndexOf(Name) < 0) then
    FLocals.Add(Name);
  Result := VarSlot(Resolve(Name));
end;

// The variable a name refers to here: the function's own (parameter or
// declared inside it), else the global.
function TJSCompiler.Resolve(const Name: string): string;
begin
  if (FFunction <> '') and (FLocals.IndexOf(Name) >= 0) then
    Result := FFunction + ':' + Name
  else
    Result := Name;
end;

procedure TJSCompiler.Declare(const Name: string; IsConst: Boolean);
begin
  if IsBuiltin(Name) then
    Fail('"' + Name + '" is a built-in and can''t be redeclared', FTok.Line, FTok.Col);
  if (FFunction <> '') and (FLocals.IndexOf(Name) < 0) then
    FLocals.Add(Name);
  if IsConst then
    FConsts.Add(Resolve(Name));
end;

procedure TJSCompiler.CheckAssignable(const Name: string);
begin
  if FConsts.IndexOf(Resolve(Name)) >= 0 then
    Fail('"' + Name + '" is a const and can''t be assigned', FTok.Line, FTok.Col);
end;

{ ---- Statements ---- }

procedure TJSCompiler.Statement;
begin
  if FTok.Kind = jtIdent then
  begin
    if (FTok.Text = 'var') or (FTok.Text = 'let') then
    begin
      VarDeclaration(False);
      Exit;
    end;
    if FTok.Text = 'const' then
    begin
      VarDeclaration(True);
      Exit;
    end;
    if FTok.Text = 'function' then
    begin
      FunctionDeclaration;
      Exit;
    end;
    if FTok.Text = 'if' then begin IfStatement; Exit; end;
    if FTok.Text = 'while' then begin WhileStatement; Exit; end;
    if FTok.Text = 'do' then begin DoWhileStatement; Exit; end;
    if FTok.Text = 'for' then begin ForStatement; Exit; end;
    if FTok.Text = 'break' then begin BreakContinue(True); Exit; end;
    if FTok.Text = 'continue' then begin BreakContinue(False); Exit; end;
    if FTok.Text = 'return' then begin ReturnStatement; Exit; end;
    // "call f(...)" - Kayte's own spelling of a call statement.
    if (FTok.Text = 'call') and (FNext.Kind = jtIdent) then
      Advance;
  end;
  if IsPunct('{') then
  begin
    Block;
    Exit;
  end;
  if IsPunct(';') then
  begin
    Advance; // empty statement
    Exit;
  end;
  SimpleStatement;
  SkipSemicolon;
end;

procedure TJSCompiler.Block;
begin
  Expect('{');
  while not IsPunct('}') do
  begin
    if FTok.Kind = jtEOF then
      FailHere('expected "}"');
    try
      Statement;
    except
      on E: EJSError do
      begin
        Inc(FErrors);
        Writeln('JS Error: ', E.Message);
        Recover;
      end;
    end;
  end;
  Advance; // }
end;

// var / let / const name [= value] [, name [= value]]...
procedure TJSCompiler.VarDeclaration(IsConst: Boolean);
var
  Name: string;
begin
  Advance; // var / let / const
  repeat
    Name := ExpectIdent('a variable name');
    if IsPunct('=') then
    begin
      Advance;
      Expression;
    end
    else if IsConst then
      FailHere('a const needs a value (const ' + Name + ' = ...)')
    else
      PushInt(0); // JavaScript's "undefined" - 0 here
    Declare(Name, IsConst);
    Emit(BC_STORE_VAR, [VarSlot(Resolve(Name))]);
    if not IsPunct(',') then
      Break;
    Advance;
  until False;
  SkipSemicolon;
end;

// function name(a, b) { ... } - a SUB whose parameters and declarations
// are local. The VM has no per-call storage, so on entry the function
// saves its locals' current values on the evaluation stack and restores
// them on the way out - which is what makes recursion work. Its locals are
// only all known after the body, so the entry code comes after it:
//   BC_JUMP past-it-all
//   BC_ENTER paramcount           <- entry address (SubroutineMap)
//   BC_JUMP entry
// body:   ...                     return: result -> #result, jump to exit
// exit:   #result -> __ret, restore the locals (reverse order), BC_RETURN
// entry:  arguments -> temps, save the locals, parameters := arguments,
//         other locals := 0, BC_JUMP body
procedure TJSCompiler.FunctionDeclaration;
const
  ResultName = '#result'; // a function's return value while it runs
var
  Name, Key: string;
  Params: TStringList;
  JumpOver, ToEntry, BodyStart, I: Integer;
  Line, Col: Integer;

  function ArgTemp(Index: Integer): Integer;
  begin
    // Only used between entry and the parameter stores, with no call in
    // between, so one global per parameter is safe.
    Result := VarSlot(Format('#arg$%s$%d', [Key, Index]));
  end;

begin
  Line := FTok.Line;
  Col := FTok.Col;
  Advance; // function
  if FFunction <> '' then
    Fail('functions can''t be nested inside "' + FFunction + '" - declare them at the top level', Line, Col);
  Name := ExpectIdent('a function name');
  if IsBuiltin(Name) then
    Fail('"' + Name + '" is a built-in and can''t be redefined', Line, Col);
  Key := UpperCase(Name);
  if FAsm.CurrentProgram.SubroutineMap.IndexOf(Key) >= 0 then
    Fail('function "' + Name + '" is already defined (function names ignore case)', Line, Col);

  Params := TStringList.Create;
  Params.CaseSensitive := True;
  try
    Expect('(');
    if not IsPunct(')') then
      repeat
        if (FTok.Kind = jtIdent) and (Params.IndexOf(FTok.Text) >= 0) then
          FailHere('parameter "' + FTok.Text + '" is listed twice');
        Params.Add(ExpectIdent('a parameter name'));
        if not IsPunct(',') then
          Break;
        Advance;
      until False;
    Expect(')');

    JumpOver := EmitJump(BC_JUMP); // top-level code runs past the function
    FAsm.CurrentProgram.SubroutineMap.Add(Key, Here);
    FParamCounts.AddObject(Key, TObject(PtrInt(Params.Count)));
    Emit(BC_ENTER, [Params.Count]);
    ToEntry := EmitJump(BC_JUMP);
    BodyStart := Here;

    FFunction := Name;
    FLocals.Clear;
    FLocals.Add(ResultName); // FLocals[0]
    FLocals.AddStrings(Params);
    FReturnJumps := nil;
    try
      Block;
    finally
      // Exit: every return lands here (and so does falling off the end,
      // which returns 0 - #result starts at 0).
      for I := 0 to High(FReturnJumps) do
        PatchTo(FReturnJumps[I], Here);
      Emit(BC_LOAD_VAR, [VarSlot(Resolve(ResultName))]);
      Emit(BC_STORE_VAR, [VarSlot(RetVar)]);
      for I := FLocals.Count - 1 downto 0 do
        Emit(BC_STORE_VAR, [VarSlot(Resolve(FLocals[I]))]);
      Emit(BC_RETURN, []);

      // Entry: arguments were pushed in order, so the last is on top.
      PatchTo(ToEntry, Here);
      for I := Params.Count - 1 downto 0 do
        Emit(BC_STORE_VAR, [ArgTemp(I)]);
      for I := 0 to FLocals.Count - 1 do
        Emit(BC_LOAD_VAR, [VarSlot(Resolve(FLocals[I]))]); // save the caller's values
      for I := 0 to FLocals.Count - 1 do
      begin
        if Params.IndexOf(FLocals[I]) >= 0 then
          Emit(BC_LOAD_VAR, [ArgTemp(Params.IndexOf(FLocals[I]))])
        else
          PushInt(0); // fresh locals (and #result) start at 0
        Emit(BC_STORE_VAR, [VarSlot(Resolve(FLocals[I]))]);
      end;
      Emit(BC_JUMP, [BodyStart]);
      PatchTo(JumpOver, Here);

      FFunction := '';
      FLocals.Clear;
      FReturnJumps := nil;
    end;
  finally
    Params.Free;
  end;
end;

procedure TJSCompiler.IfStatement;
var
  ToElse, ToEnd: Integer;
begin
  Advance; // if
  Expect('(');
  Expression;
  Expect(')');
  ToElse := EmitJump(BC_JUMP_IF_FALSE);
  Statement;
  if IsIdent('else') then
  begin
    Advance;
    ToEnd := EmitJump(BC_JUMP);
    PatchTo(ToElse, Here);
    Statement;
    PatchTo(ToEnd, Here);
  end
  else
    PatchTo(ToElse, Here);
end;

procedure TJSCompiler.WhileStatement;
var
  Top, ToExit, I: Integer;
  Loop: TLoop;
begin
  Advance; // while
  Top := Here;
  Expect('(');
  Expression;
  Expect(')');
  ToExit := EmitJump(BC_JUMP_IF_FALSE);
  Loop := TLoop.Create;
  FLoops.Add(Loop);
  try
    Statement;
    Emit(BC_JUMP, [Top]);
    PatchTo(ToExit, Here);
    for I := 0 to Loop.Breaks.Count - 1 do
      PatchTo(Loop.Breaks[I], Here);
    for I := 0 to Loop.Continues.Count - 1 do
      PatchTo(Loop.Continues[I], Top);
  finally
    FLoops.Remove(Loop);
    Loop.Free;
  end;
end;

procedure TJSCompiler.DoWhileStatement;
var
  Top, CondAt, I: Integer;
  Loop: TLoop;
begin
  Advance; // do
  Top := Here;
  Loop := TLoop.Create;
  FLoops.Add(Loop);
  try
    Statement;
    if not IsIdent('while') then
      FailHere('expected "while" after the do { ... } body');
    Advance;
    CondAt := Here;
    Expect('(');
    Expression;
    Expect(')');
    SkipSemicolon;
    // Loop while the condition holds: jump out when false, else back up.
    I := EmitJump(BC_JUMP_IF_FALSE);
    Emit(BC_JUMP, [Top]);
    PatchTo(I, Here);
    for I := 0 to Loop.Breaks.Count - 1 do
      PatchTo(Loop.Breaks[I], Here);
    for I := 0 to Loop.Continues.Count - 1 do
      PatchTo(Loop.Continues[I], CondAt);
  finally
    FLoops.Remove(Loop);
    Loop.Free;
  end;
end;

// for (init; condition; update) body. The update is compiled after the
// body (where it runs), so its tokens are parsed then: the body is reached
// by a jump over the update code.
procedure TJSCompiler.ForStatement;
var
  CondTop, ToExit, ToBody, UpdateTop, I, Slot, Limit, StepVar: Integer;
  Loop: TLoop;
  Name: string;
  Negative: Boolean;
begin
  Advance; // for

  // for i from A to B [step S] { ... } - counts from A to B inclusive
  // (down when the step is negative). B and S are evaluated once.
  if (FTok.Kind = jtIdent) and (FNext.Kind = jtIdent) and (FNext.Text = 'from') then
  begin
    Name := FTok.Text;
    Advance;
    Advance; // from
    Declare(Name, False);
    CheckAssignable(Name);
    Slot := VarSlot(Resolve(Name));
    Expression;
    Emit(BC_STORE_VAR, [Slot]);
    if not IsIdent('to') then
      FailHere('expected "to" (for ' + Name + ' from A to B)');
    Advance;
    Expression;
    Limit := NewTemp;
    Emit(BC_STORE_VAR, [Limit]);
    Negative := False;
    StepVar := -1;
    if IsIdent('step') then
    begin
      Advance;
      Negative := IsPunct('-');
      Expression;
      StepVar := NewTemp;
      Emit(BC_STORE_VAR, [StepVar]);
    end;

    CondTop := Here;
    Emit(BC_LOAD_VAR, [Slot]);
    Emit(BC_LOAD_VAR, [Limit]);
    if Negative then Emit(BC_CMP_GE, []) else Emit(BC_CMP_LE, []);
    ToExit := EmitJump(BC_JUMP_IF_FALSE);

    Loop := TLoop.Create;
    FLoops.Add(Loop);
    try
      Statement;
      UpdateTop := Here;
      Emit(BC_LOAD_VAR, [Slot]);
      if StepVar >= 0 then Emit(BC_LOAD_VAR, [StepVar]) else PushInt(1);
      Emit(BC_ADD, []);
      Emit(BC_STORE_VAR, [Slot]);
      Emit(BC_JUMP, [CondTop]);
      PatchTo(ToExit, Here);
      for I := 0 to Loop.Breaks.Count - 1 do
        PatchTo(Loop.Breaks[I], Here);
      for I := 0 to Loop.Continues.Count - 1 do
        PatchTo(Loop.Continues[I], UpdateTop);
    finally
      FLoops.Remove(Loop);
      Loop.Free;
    end;
    Exit;
  end;

  Expect('(');
  if IsPunct(';') then
    Advance
  else if IsIdent('var') or IsIdent('let') then
    VarDeclaration(False) // consumes the ;
  else if IsIdent('const') then
    FailHere('a for loop''s counter can''t be const')
  else
  begin
    SimpleStatement;
    Expect(';');
  end;

  CondTop := Here;
  if IsPunct(';') then
    PushInt(1) // no condition: loop until break
  else
    Expression;
  Expect(';');
  ToExit := EmitJump(BC_JUMP_IF_FALSE);
  ToBody := EmitJump(BC_JUMP);

  // The update runs after each iteration, then the condition again.
  UpdateTop := Here;
  if not IsPunct(')') then
  begin
    SimpleStatement;
    while IsPunct(',') do
    begin
      Advance;
      SimpleStatement;
    end;
  end;
  Expect(')');
  Emit(BC_JUMP, [CondTop]);

  PatchTo(ToBody, Here);
  Loop := TLoop.Create;
  FLoops.Add(Loop);
  try
    Statement;
    Emit(BC_JUMP, [UpdateTop]);
    PatchTo(ToExit, Here);
    for I := 0 to Loop.Breaks.Count - 1 do
      PatchTo(Loop.Breaks[I], Here);
    for I := 0 to Loop.Continues.Count - 1 do
      PatchTo(Loop.Continues[I], UpdateTop);
  finally
    FLoops.Remove(Loop);
    Loop.Free;
  end;
end;

procedure TJSCompiler.BreakContinue(IsBreak: Boolean);
var
  Loop: TLoop;
begin
  if FLoops.Count = 0 then
  begin
    if IsBreak then
      Fail('"break" is only allowed inside a loop', FTok.Line, FTok.Col)
    else
      Fail('"continue" is only allowed inside a loop', FTok.Line, FTok.Col);
  end;
  Advance;
  Loop := TLoop(FLoops[FLoops.Count - 1]);
  if IsBreak then
    Loop.Breaks.Add(EmitJump(BC_JUMP))
  else
    Loop.Continues.Add(EmitJump(BC_JUMP));
  SkipSemicolon;
end;

procedure TJSCompiler.ReturnStatement;
begin
  if FFunction = '' then
    Fail('"return" is only allowed inside a function', FTok.Line, FTok.Col);
  Advance; // return
  if IsPunct(';') or IsPunct('}') or (FTok.Kind = jtEOF) then
    PushInt(0)
  else
    Expression;
  // The result goes in the function's own #result (a local, so a recursive
  // call can't overwrite it); the exit code passes it on and restores the
  // caller's locals.
  Emit(BC_STORE_VAR, [VarSlot(Resolve('#result'))]);
  SetLength(FReturnJumps, Length(FReturnJumps) + 1);
  FReturnJumps[High(FReturnJumps)] := EmitJump(BC_JUMP);
  SkipSemicolon;
end;

// An expression used as a statement: an assignment (= += -= *= /= %=),
// ++ / --, or a call. Also used for a for loop's init / update parts.
procedure TJSCompiler.SimpleStatement;
var
  Name, Op: string;
  Line, Col, Slot: Integer;
begin
  // ++x / --x
  if IsPunct('++') or IsPunct('--') then
  begin
    Op := FTok.Text;
    Advance;
    Name := ExpectIdent('a variable after ' + Op);
    CheckAssignable(Name);
    Slot := VarSlot(Resolve(Name));
    Emit(BC_LOAD_VAR, [Slot]);
    PushInt(1);
    if Op = '++' then Emit(BC_ADD, []) else Emit(BC_SUB, []);
    Emit(BC_STORE_VAR, [Slot]);
    Exit;
  end;

  if FTok.Kind <> jtIdent then
    FailHere('expected a statement');
  Name := FTok.Text;
  Line := FTok.Line;
  Col := FTok.Col;
  Advance;

  // console.log(...)
  if (Name = 'console') and IsPunct('.') then
  begin
    Advance;
    if not IsIdent('log') then
      FailHere('only console.log is supported');
    Advance;
    Name := 'console.log';
  end;

  if IsPunct('(') then
  begin
    CallByName(Name, Line, Col, False);
    Exit;
  end;

  if FTok.Kind = jtPunct then
    Op := FTok.Text
  else
    Op := '';
  if IsBuiltin(Name) then
    Fail('"' + Name + '" is a built-in function - call it with (...)', Line, Col);
  if (Op = '=') or (Op = '+=') or (Op = '-=') or (Op = '*=') or (Op = '/=') or (Op = '%=') then
  begin
    CheckAssignable(Name);
    Advance;
    Slot := VarSlot(Resolve(Name));
    if Op = '=' then
      Expression
    else if Op = '%=' then
    begin
      // x %= y: the remainder, with the sign of x
      Emit(BC_LOAD_VAR, [Slot]);
      Expression;
      Emit(BC_MOD, []);
    end
    else
    begin
      Emit(BC_LOAD_VAR, [Slot]);
      Expression;
      case Op[1] of
        '+': Emit(BC_ADD, []);
        '-': Emit(BC_SUB, []);
        '*': Emit(BC_MUL, []);
        '/': Emit(BC_IDIV, []); // numbers are integers: "/" divides whole numbers
      end;
    end;
    Emit(BC_STORE_VAR, [Slot]);
  end
  else if (Op = '++') or (Op = '--') then
  begin
    CheckAssignable(Name);
    Advance;
    Slot := VarSlot(Resolve(Name));
    Emit(BC_LOAD_VAR, [Slot]);
    PushInt(1);
    if Op = '++' then Emit(BC_ADD, []) else Emit(BC_SUB, []);
    Emit(BC_STORE_VAR, [Slot]);
  end
  else if Op = '.' then
    Fail('objects and properties (' + Name + '.x) aren''t supported yet', Line, Col)
  else if Op = '[' then
    Fail('arrays (' + Name + '[...]) aren''t supported yet', Line, Col)
  else
    Fail('expected =, ++, -- or a call after "' + Name + '" (an expression alone does nothing)', Line, Col);
end;

// After an error: skip to the end of the statement (a ; at this nesting
// level) or the } that closes the current block, so parsing can go on.
procedure TJSCompiler.Recover;
var
  Depth: Integer;
begin
  Depth := 0;
  try
    while FTok.Kind <> jtEOF do
    begin
      if IsPunct('{') then
        Inc(Depth)
      else if IsPunct('}') then
      begin
        if Depth = 0 then
          Exit;
        Dec(Depth);
        if Depth = 0 then
        begin
          Advance;
          Exit;
        end;
      end
      else if IsPunct(';') and (Depth = 0) then
      begin
        Advance;
        Exit;
      end;
      Advance;
    end;
  except
    // The lexer can't get past this point; stop checking here.
    on E: EJSError do
    begin
      Inc(FErrors);
      Writeln('JS Error: ', E.Message);
      raise EKayteParseError.CreateFmt('%d error(s) - compilation stopped, nothing was written', [FErrors]);
    end;
  end;
end;

{ ---- Expressions ---- }

procedure TJSCompiler.Expression;
begin
  Conditional;
end;

// c ? a : b
procedure TJSCompiler.Conditional;
var
  ToElse, ToEnd: Integer;
begin
  LogicalOr;
  if IsPunct('?') then
  begin
    Advance;
    ToElse := EmitJump(BC_JUMP_IF_FALSE);
    Conditional;
    Expect(':');
    ToEnd := EmitJump(BC_JUMP);
    PatchTo(ToElse, Here);
    Conditional;
    PatchTo(ToEnd, Here);
  end;
end;

// a || b  ->  1 if either is true, else 0 (b only evaluated if needed)
procedure TJSCompiler.LogicalOr;
var
  TryRight, ToFalse, ToEnd1, ToEnd2: Integer;
begin
  LogicalAnd;
  while IsPunct('||') do
  begin
    Advance;
    TryRight := EmitJump(BC_JUMP_IF_FALSE);
    PushInt(1);
    ToEnd1 := EmitJump(BC_JUMP);
    PatchTo(TryRight, Here);
    LogicalAnd;
    ToFalse := EmitJump(BC_JUMP_IF_FALSE);
    PushInt(1);
    ToEnd2 := EmitJump(BC_JUMP);
    PatchTo(ToFalse, Here);
    PushInt(0);
    PatchTo(ToEnd1, Here);
    PatchTo(ToEnd2, Here);
  end;
end;

// a && b  ->  1 if both are true, else 0 (b only evaluated if needed)
procedure TJSCompiler.LogicalAnd;
var
  False1, False2, ToEnd: Integer;
begin
  Equality;
  while IsPunct('&&') do
  begin
    Advance;
    False1 := EmitJump(BC_JUMP_IF_FALSE);
    Equality;
    False2 := EmitJump(BC_JUMP_IF_FALSE);
    PushInt(1);
    ToEnd := EmitJump(BC_JUMP);
    PatchTo(False1, Here);
    PatchTo(False2, Here);
    PushInt(0);
    PatchTo(ToEnd, Here);
  end;
end;

procedure TJSCompiler.Equality;
var
  Op: string;
begin
  Relational;
  while IsPunct('==') or IsPunct('!=') or IsPunct('===') or IsPunct('!==') do
  begin
    Op := FTok.Text;
    Advance;
    Relational;
    if Op[1] = '=' then Emit(BC_CMP_EQ, []) else Emit(BC_CMP_NEQ, []);
  end;
end;

procedure TJSCompiler.Relational;
var
  Op: string;
begin
  Additive;
  while IsPunct('<') or IsPunct('>') or IsPunct('<=') or IsPunct('>=') do
  begin
    Op := FTok.Text;
    Advance;
    Additive;
    if Op = '<' then Emit(BC_CMP_LT, [])
    else if Op = '>' then Emit(BC_CMP_GT, [])
    else if Op = '<=' then Emit(BC_CMP_LE, [])
    else Emit(BC_CMP_GE, []);
  end;
end;

procedure TJSCompiler.Additive;
var
  Op: string;
begin
  Multiplicative;
  while IsPunct('+') or IsPunct('-') do
  begin
    Op := FTok.Text;
    Advance;
    Multiplicative;
    // + adds numbers and concatenates strings, as in JavaScript.
    if Op = '+' then Emit(BC_ADD, []) else Emit(BC_SUB, []);
  end;
end;

procedure TJSCompiler.Multiplicative;
var
  Op: string;
begin
  UnaryExpr;
  while IsPunct('*') or IsPunct('/') or IsPunct('%') do
  begin
    Op := FTok.Text;
    Advance;
    if Op = '%' then
    begin
      // a % b: the remainder, with the sign of a
      UnaryExpr;
      Emit(BC_MOD, []);
      Continue;
    end;
    UnaryExpr;
    if Op = '*' then Emit(BC_MUL, []) else Emit(BC_IDIV, []); // integers: "/" divides whole numbers
  end;
end;

procedure TJSCompiler.UnaryExpr;
begin
  if IsPunct('!') then
  begin
    Advance;
    UnaryExpr;
    Emit(BC_NOT, []);
  end
  else if IsPunct('-') then
  begin
    Advance;
    UnaryExpr;
    Emit(BC_NEG, []);
  end
  else if IsPunct('+') then
  begin
    Advance;
    UnaryExpr;
  end
  else if IsPunct('++') or IsPunct('--') then
    FailHere('++ and -- work as statements (x++;), not inside expressions')
  else
    Primary;
end;

procedure TJSCompiler.Primary;
var
  Name: string;
  Line, Col: Integer;
  V: Int64;
begin
  case FTok.Kind of
    jtNumber:
      begin
        if not TryStrToInt64(FTok.Text, V) then
          FailHere('number too large (numbers are 64-bit integers)');
        PushInt(V);
        Advance;
      end;
    jtString:
      begin
        PushStr(FTok.Text);
        Advance;
      end;
    jtIdent:
      begin
        Name := FTok.Text;
        Line := FTok.Line;
        Col := FTok.Col;
        if (Name = 'true') then begin PushInt(1); Advance; Exit; end;
        if (Name = 'false') or (Name = 'null') or (Name = 'undefined') then begin PushInt(0); Advance; Exit; end;
        if (Name = 'function') or (Name = 'var') or (Name = 'let') or (Name = 'const') or (Name = 'if') or
           (Name = 'while') or (Name = 'for') or (Name = 'return') then
          FailHere('expected a value');
        Advance;
        if IsPunct('(') then
          CallByName(Name, Line, Col, True)
        else if IsPunct('.') then
          Fail('objects and properties (' + Name + '.x) aren''t supported yet', Line, Col)
        else if IsPunct('[') then
          Fail('arrays (' + Name + '[...]) aren''t supported yet', Line, Col)
        else if IsPunct('=') then
          Fail('assignment isn''t an expression here - write it as its own statement', FTok.Line, FTok.Col)
        else
          Emit(BC_LOAD_VAR, [VarSlot(Resolve(Name))]);
      end;
    jtPunct:
      if IsPunct('(') then
      begin
        Advance;
        Expression;
        Expect(')');
      end
      else if IsPunct('[') then
        FailHere('arrays aren''t supported yet')
      else if IsPunct('{') then
        FailHere('object literals aren''t supported yet')
      else
        FailHere('expected a value');
  else
    FailHere('expected a value');
  end;
end;

// ( expr, expr, ... ) - pushes each; returns how many.
function TJSCompiler.Arguments: Integer;
begin
  Result := 0;
  Expect('(');
  if not IsPunct(')') then
    repeat
      Expression;
      Inc(Result);
      if not IsPunct(',') then
        Break;
      Advance;
    until False;
  Expect(')');
end;

function TJSCompiler.IsBuiltin(const Name: string): Boolean;
begin
  Result := (Name = 'print') or (Name = 'console.log') or (Name = 'alert') or (Name = 'msgbox') or
            (Name = 'qt') or (Name = 'qml') or (Name = 'process');
end;

procedure TJSCompiler.CallByName(const Name: string; Line, Col: Integer; WantValue: Boolean);
var
  N, Dest: Integer;
  Call: TPendingJSCall;
begin
  if (Name = 'print') or (Name = 'console.log') or (Name = 'alert') or (Name = 'msgbox') then
  begin
    if WantValue then
      Fail(Name + '() doesn''t return a value', Line, Col);
    if (Name = 'alert') or (Name = 'msgbox') then
      PushStr('[MsgBox]'); // like the BASIC MSGBOX statement
    N := Arguments;
    if (Name = 'alert') or (Name = 'msgbox') then
      Inc(N);
    Emit(BC_PRINT, [N]);
    Exit;
  end;

  if (Name = 'qt') or (Name = 'qml') or (Name = 'process') then
  begin
    N := Arguments;
    if N = 0 then
      Fail(Name + '() needs at least a command', Line, Col);
    // The statement stores its result in a variable (dest) or, with -1,
    // discards it - PROCESS then prints the output instead.
    if WantValue then
      Dest := NewTemp
    else
      Dest := -1;
    if Name = 'process' then
      Emit(BC_PROCESS, [N, Dest])
    else if Name = 'qt' then
      Emit(BC_QT, [N, Dest, QT_STATEMENT_QT])
    else
      Emit(BC_QT, [N, Dest, QT_STATEMENT_QML]);
    if WantValue then
      Emit(BC_LOAD_VAR, [Dest]);
    Exit;
  end;

  // A user function: a SUB call, resolved once every function is known.
  N := Arguments;
  Call.Key := UpperCase(Name);
  Call.Name := Name;
  Call.ArgCount := N;
  Call.Line := Line;
  Call.Col := Col;
  Call.InstrIndex := Here;
  Emit(BC_CALL, [-1, N]);
  SetLength(FCalls, Length(FCalls) + 1);
  FCalls[High(FCalls)] := Call;
  if WantValue then
    Emit(BC_LOAD_VAR, [VarSlot(RetVar)]);
end;

procedure TJSCompiler.ResolveCalls;
var
  I, Idx, Expected: Integer;
  Subs: TStringIntMap;
begin
  Subs := FAsm.CurrentProgram.SubroutineMap;
  for I := 0 to High(FCalls) do
  begin
    Idx := Subs.IndexOf(FCalls[I].Key);
    if Idx < 0 then
    begin
      Inc(FErrors);
      Writeln(Format('JS Error: %s:%d:%d: call to undefined function "%s"',
        [FFile, FCalls[I].Line, FCalls[I].Col, FCalls[I].Name]));
      Continue;
    end;
    Expected := PtrInt(FParamCounts.Objects[FParamCounts.IndexOf(FCalls[I].Key)]);
    if Expected <> FCalls[I].ArgCount then
    begin
      Inc(FErrors);
      Writeln(Format('JS Error: %s:%d:%d: %s() takes %d argument(s), but this call passes %d',
        [FFile, FCalls[I].Line, FCalls[I].Col, FCalls[I].Name, Expected, FCalls[I].ArgCount]));
      Continue;
    end;
    FAsm.PatchJumpTarget(FCalls[I].InstrIndex, Subs.Data[Idx]);
  end;
end;

function TJSCompiler.Compile: TByteCodeProgram;
begin
  Result := nil;
  try
    FNext := ScanToken;
    Advance;
  except
    on E: EJSError do
    begin
      Writeln('JS Error: ', E.Message);
      raise EKayteParseError.Create('1 error(s) - compilation stopped, nothing was written');
    end;
  end;

  while FTok.Kind <> jtEOF do
    try
      if IsPunct('}') then
        FailHere('unexpected "}"');
      Statement;
    except
      on E: EJSError do
      begin
        Inc(FErrors);
        Writeln('JS Error: ', E.Message);
        if IsPunct('}') then
          Advance
        else
          Recover;
      end;
    end;

  ResolveCalls;
  if FErrors > 0 then
    raise EKayteParseError.CreateFmt('%d error(s) - compilation stopped, nothing was written', [FErrors]);
  Result := FAsm.GetProgram;
end;

{ Public }

function IsJSSourceFile(const FileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := (Ext = '.kjs') or (Ext = '.js');
end;

function CompileJS(const Source, FileName: string): TByteCodeProgram;
var
  C: TJSCompiler;
begin
  C := TJSCompiler.Create(Source, ExtractFileName(FileName));
  try
    Result := C.Compile;
  finally
    C.Free;
  end;
end;

end.
