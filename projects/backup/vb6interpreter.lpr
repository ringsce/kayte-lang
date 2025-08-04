program vb6interpreter;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, StrUtils, Contnrs, // <- Contnrs gives you TStack,
  //InterpreterUtils in '../source/InterpreterUtils.pas',
  InterpreterCore, fgl, VBCompiler, Forms, Interfaces,
  BytecodeTypes, BytecodeVM, Lexer,             // Your new lexer unit
  Parser, TokenDefs,StdCtrls, UKfrmParser;
  //, SynVBHighlighter;
  // dhtml enable when its right, dhtml is webasm technology not ready

type
  TSubroutineMap = specialize TFPGMap<String, Integer>;

  TVarType = (vtInteger, vtString);

  TVariable = record
    vtype: TVarType;
    intValue: Integer;
    strValue: String;
  end;

  PVariable = ^TVariable;

  TLoop = record
    startLine: Integer;
    varName: String;
    conditionVal: Integer;
  end;

  TLoopStack = array of TLoop;

var
  Vars: TStringList;
  Labels: TStringList;
  Code: TStringList;
  Subs: TSubroutineMap;
  Stack: TStack; // You may need to define this type or use a generic one like TStack<T>
  LoopStack: TLoopStack;
  pc: Integer; // Program Counter
  currentLine: String;
  // NEW: Lexer and Parser instances - MOVED HERE from inside the try block
  MyLexer: TLexer;
  MyParser: TParser;


(*procedure TSubroutineMap.Add(SubName: String; LineNumber: Integer);
begin
  Self.AddObject(SubName, TObject(PtrInt(LineNumber)));
end; Remove later
*)


procedure SetVar(const name: String; val: TVariable);
var
  idx: Integer;
  varPtr: PVariable;
begin
  idx := Vars.IndexOf(name);
  if idx = -1 then
  begin
    New(varPtr);
    varPtr^ := val;
    Vars.AddObject(name, TObject(varPtr));
  end
  else
  begin
    varPtr := PVariable(Vars.Objects[idx]);
    varPtr^ := val;
  end;
end;

function GetVar(const name: String): TVariable;
var
  idx: Integer;
  varPtr: PVariable;
begin
  idx := Vars.IndexOf(name);
  if idx = -1 then
  begin
    WriteLn('Error: Variable ', name, ' not defined');
    Halt(1);
  end;
  varPtr := PVariable(Vars.Objects[idx]);
  Result := varPtr^;
end;

procedure LoadCode;
begin
  try
    Code.LoadFromFile('program.bas');
    WriteLn('Loaded code from program.bas');
  except
    on E: Exception do
    begin
      WriteLn('Error loading code: ', E.Message);
      Halt(1);
    end;
  end;
end;

// --- NEW: BuildSubs Procedure Placeholder ---
procedure BuildSubs(const ACode: TStringList; ASubs: TSubroutineMap);
var
  i: Integer;
  Line: String;
  SubName: String;
  StartPos: Integer;
  EndPos: Integer;
begin
  WriteLn('Building subroutines (placeholder implementation)...');
  // This is a placeholder. You need to implement the actual logic here.
  // Example: Iterate through ACode, find "Sub" declarations, and add them to ASubs.
  // For example:
  // for i := 0 to ACode.Count - 1 do
  // begin
  //   Line := Trim(ACode[i]);
  //   if StartsText('Sub ', Line) then
  //   begin
  //     // Extract subroutine name
  //     StartPos := Pos(' ', Line) + 1;
  //     EndPos := Pos('(', Line, StartPos);
  //     if EndPos = 0 then
  //       EndPos := Length(Line) + 1; // If no parameters, go to end of line
  //     SubName := Trim(Copy(Line, StartPos, EndPos - StartPos));
  //     ASubs.Add(SubName, i); // Add subroutine name and its starting line number
  //     WriteLn('Found Sub: ', SubName, ' at line ', i);
  //   end;
  // end;
end;
// --- END NEW ---

(*
procedure BuildLabels;
begin
  // Placeholder for BuildLabels
  WriteLn('Building labels (placeholder implementation)...');
end;

procedure ExecuteLine(const ALine: String; var APC: Integer);
begin
  // Placeholder for ExecuteLine
  WriteLn('Executing line (placeholder): ', ALine);
end;
*)


begin
  // Initialize interpreter components
  Code := TStringList.Create;
  Labels := TStringList.Create;
  Vars := TStringList.Create;
  Subs := TSubroutineMap.Create;
  Stack := TStack.Create;

  try
    // Load the source code first
    LoadCode;

    // NEW: Lexer and Parser instances - Instantiated here after being declared above
    // Corrected: Pass the 'Code' TStringList to the TLexer.Create constructor
    MyLexer := TLexer.Create(Code);

    // Corrected: Call TParser.Create without parameters.
    MyParser := TParser.Create(MyLexer);
    // If your TParser needs the lexer, add a line like this after creation:
    // MyParser.SetLexer(MyLexer); // Or MyParser.Lexer := MyLexer; depending on your TParser implementation


    // Build labels and subroutines
    BuildLabels; // This still needs to be defined if not already in an included unit
    BuildSubs(Code, Subs); // Now this procedure is defined to accept Code and Subs

    pc := 0;
    while pc < Code.Count do
    begin
      currentLine := Code[pc];
      ExecuteLine(currentLine, pc); // This still needs to be defined if not already in an included unit
      Inc(pc);
    end;

  finally
    // Clean up
    Code.Free;
    Labels.Free;
    Vars.Free;
    Subs.Free;
    Stack.Free;
    MyParser.Free; // Free the parser (which doesn't own lexer)
    MyLexer.Free;  // Free the lexer
    // RuntimeForms.Free; // This line is commented out because RuntimeForms is not declared
  end;

  ReadLn; // Keep console open
end.

