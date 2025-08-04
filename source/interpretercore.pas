unit InterpreterCore;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, // For TStringList (Code, Labels, Vars) and general utilities
  StrUtils,          // For string manipulation like StartsText, ContainsText
  Contnrs,           // For TStack if you're using it for call stack or expression evaluation
  fgl,               // For TFPGMap
  BytecodeTypes,     // For TBCValue, TOpCode, TBCValueType, etc.
  Lexer,             // For TLexer, TToken, TTokenType
  Parser,            // For TParser
  TokenDefs;         // For TToken, TTokenType, GetTokenTypeName

// Forward declarations of global variables from vb6interpreter.lpr that InterpreterCore might need
// These should ideally be passed as parameters or accessed via a central 'InterpreterState' object
// but for quick compilation based on your current lpr, we'll assume global visibility.
// In a real project, consider passing these as parameters to ExecuteLine or wrapping them in a class.
var
  Code: TStringList;
  Labels: TStringList;
  Vars: TStringList; // Assuming TVariable records are stored here
  Subs: specialize TFPGMap<String, Integer>; // Assuming this type is defined in lpr
  Stack: TStack; // Assuming TStack is defined (e.g., TStack<T>)
  pc: Integer; // Program Counter
  MyLexer: TLexer; // Assuming these are global in lpr
  MyParser: TParser; // Assuming these are global in lpr

// Assuming TVariable and PVariable are defined in vb6interpreter.lpr or a common unit
type
  TVarType = (vtInteger, vtString);

  TVariable = record
    vtype: TVarType;
    intValue: Integer;
    strValue: String;
  end;

  PVariable = ^TVariable;

// SetVar and GetVar are declared here, their implementation is below in the 'implementation' section.
// The 'forward' directive is not needed here because they are declared in the interface.
procedure SetVar(const name: String; val: TVariable);
function GetVar(const name: String): TVariable;

// Main execution procedure for a single line of code
procedure ExecuteLine(const ALine: String; var APC: Integer);

implementation

// Implementations of SetVar and GetVar
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


// --- ExecuteLine Implementation ---
procedure ExecuteLine(const ALine: String; var APC: Integer);
var
  CurrentToken: TToken;
  StatementType: String;
  // You'll need more variables here to handle different statement types
begin
  WriteLn(Format('Executing line %d: "%s"', [APC + 1, ALine])); // Debug output

  // Reset the lexer with the current line
  // This approach assumes the lexer processes one line at a time.
  // If your lexer processes the entire source code at once, you'd feed it the whole 'Code' TStringList
  // and then call GetNextToken repeatedly.
  // For now, let's assume the lexer is designed to work with the full Code TStringList passed at creation.
  // So, we would just advance the parser.

  // In a full interpreter, you would feed ALine to the lexer,
  // then feed tokens from the lexer to the parser, and then execute the parsed statement.
  // For a line-by-line execution, you might re-initialize the lexer for each line.

  // Simplified approach for demonstration:
  // 1. You would typically create a new TLexer for just this line, or reset the existing one.
  //    MyLexer.SetSourceString(ALine); // If your lexer has a method to set source string for a single line
  //    MyLexer.Reset; // Reset the lexer for the new line

  // 2. Then, you'd get tokens and parse:
  //    MyParser.ParseStatement(ALine); // Assuming ParseStatement takes the line or uses the global lexer

  // For now, let's just simulate some basic command recognition
  StatementType := AnsiUpperCase(Trim(ALine));

  if StartsText('PRINT', StatementType) then
  begin
    WriteLn('  -> Interpreting PRINT statement.');
    // Example: PRINT "Hello"
    // Extract "Hello" and print it.
    // This would involve more sophisticated parsing.
  end
  else if StartsText('DIM', StatementType) then
  begin
    WriteLn('  -> Interpreting DIM statement.');
    // Example: DIM myVar AS INTEGER
    // Extract myVar and its type, then call SetVar with a default value.
  end
  else if StartsText('LET', StatementType) or (Pos('=', StatementType) > 0) then
  begin
    WriteLn('  -> Interpreting Assignment statement.');
    // Example: myVar = 10
    // Extract variable name and value, then call SetVar.
  end
  else if StartsText('GOTO', StatementType) then
  begin
    WriteLn('  -> Interpreting GOTO statement.');
    // Example: GOTO MyLabel
    // Find MyLabel in Labels and set APC to its line number - 1 (because APC will be incremented)
    // This requires access to the Labels map.
    // Example: APC := Labels.IndexOf(Trim(Copy(StatementType, 5, MaxInt))) - 1;
  end
  else if StartsText('INPUT', StatementType) then
  begin
    WriteLn('  -> Interpreting INPUT statement.');
    // Example: INPUT "Enter value:", myVar
    // Prompt user, read input, and set variable.
  end
  else if StartsText('SUB', StatementType) then
  begin
    WriteLn('  -> Interpreting SUB definition (skipping for now).');
    // For SUB definitions, you typically skip lines until 'End Sub'
    // This requires more complex control flow, likely handled by the parser.
  end
  else if StartsText('END', StatementType) then
  begin
    WriteLn('  -> Interpreting END statement. Halting.');
    Halt(0); // Program ends
  end
  else
  begin
    WriteLn('  -> Unknown statement or empty line.');
  end;

  // In a real scenario, after executing the line, if it wasn't a GOTO/GOSUB/RETURN,
  // the program counter (APC) would simply increment in the main loop.
  // If it was a GOTO/GOSUB, APC would be explicitly set by that command.
end;

end.

