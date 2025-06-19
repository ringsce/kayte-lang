program vb6interpreter;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, StrUtils, Contnrs, // <- Contnrs gives you TStack,
  InterpreterUtils, InterpreterCore, fgl, VBCompiler, Forms, Interfaces,
  BytecodeTypes, BytecodeVM, Lexer,         // Your new lexer unit
  Parser, dhtml, tokenDefs;        // Your new parser unit;
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


(* Testing *)


begin
  // Initialize interpreter components
  Code := TStringList.Create;
  Labels := TStringList.Create;
  Vars := TStringList.Create;
  Subs := TSubroutineMap.Create;
  Stack := TStack.Create;
  // NEW: Lexer and Parser instances
  MyLexer: TLexer;
  MyParser: TParser;


  try
    // Load the source code
    LoadCode; // Assumes this fills the global Code TStringList

    // Build labels and subroutines
    BuildLabels(Code, Labels);
    BuildSubs(Code, Subs);

    pc := 0;
    while pc < Code.Count do
    begin
      currentLine := Code[pc];
      ExecuteLine(currentLine, pc);
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
    RuntimeForms.Free; // Free runtime forms
  end;

  ReadLn; // Keep console open
end.

