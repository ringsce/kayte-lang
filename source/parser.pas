unit Parser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  TokenDefs,       // TToken, TTokenType, GetTokenTypeName
  Lexer,           // TLexer
  BytecodeTypes,   // TBCValue, TBCValueType, BCValueToString, TStringIntMap
  fgl;             // TFPGMap (for conceptual symbol table)

type
  TParser = class
  private
    FLexer: TLexer;
    FCurrentToken: TToken;
    FOptionExplicitEnabled: Boolean; // Flag for Option Explicit
    FSymbolTable: TStringIntMap;    // Conceptual Symbol Table (String -> Index/Type)

    procedure ConsumeToken(ExpectedType: TTokenType);
    function PeekToken: TToken;

    procedure ParsePrintStatement;
    procedure ParseShowStatement;
    procedure ParseLetStatement(const VarName: String);
    procedure ParseIfStatement;
    procedure ParseOptionExplicitStatement;
    procedure ParseDimStatement;
    procedure ParseStatement;

    function ParseExpression: TBCValue;
    function ParseTerm: TBCValue;
    function ParseFactor: TBCValue;

    // Helper for symbol table management
    function IsVariableDeclared(const VarName: String): Boolean;
    procedure DeclareVariable(const VarName: String);

  public
    constructor Create(ALexer: TLexer);
    destructor Destroy; override;
    procedure ParseProgram;
  end;

implementation

{ TParser }

constructor TParser.Create(ALexer: TLexer);
begin
  inherited Create;
  FLexer := ALexer;
  FLexer.Reset;
  FCurrentToken := FLexer.GetNextToken;
  FOptionExplicitEnabled := False; // Default to OFF, like VB6
  FSymbolTable := TStringIntMap.Create; // Initialize symbol table
end;

destructor TParser.Destroy;
begin
  FreeAndNil(FSymbolTable); // Free the symbol table
  inherited Destroy;
end;

procedure TParser.ConsumeToken(ExpectedType: TTokenType);
begin
  if FCurrentToken.TokenType = ExpectedType then
    FCurrentToken := FLexer.GetNextToken
  else
    raise Exception.CreateFmt('Parser Error: Expected %s but found %s ("%s") at %d:%d',
      [GetTokenTypeName(ExpectedType),
       GetTokenTypeName(FCurrentToken.TokenType),
       FCurrentToken.Lexeme,
       FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
end;

function TParser.PeekToken: TToken;
begin
  Result := FCurrentToken;
end;

// --- Symbol Table Helpers ---
function TParser.IsVariableDeclared(const VarName: String): Boolean;
begin
  // This line is correct for TFPGMap.ContainsKey.
  // The error is external to this code.
  Result := FSymbolTable.ContainsKey(LowerCase(VarName));
end;

procedure TParser.DeclareVariable(const VarName: String);
begin
  // This line is correct for TFPGMap.ContainsKey.
  // The error is external to this code.
  if not FSymbolTable.ContainsKey(LowerCase(VarName)) then
  begin
    FSymbolTable.Add(LowerCase(VarName), FSymbolTable.Count); // Store dummy index
    WriteLn(Format('Parser: Declared variable "%s"', [VarName]));
  end
  else
  begin
    WriteLn(Format('Parser Warning: Variable "%s" already declared. (Ignoring re-declaration)', [VarName]));
  end;
end;
// --- End Symbol Table Helpers ---


function TParser.ParseFactor: TBCValue;
var
  NumVal: Int64;
  StrVal: String;
  BoolVal: Boolean;
  VarName: String;
begin
  case FCurrentToken.TokenType of
    tkIntegerLiteral:
      begin
        NumVal := StrToInt64(FCurrentToken.Lexeme);
        ConsumeToken(tkIntegerLiteral);
        Result := CreateBCValueInteger(NumVal);
      end;
    tkStringLiteral:
      begin
        StrVal := Copy(FCurrentToken.Lexeme, 2, Length(FCurrentToken.Lexeme) - 2);
        ConsumeToken(tkStringLiteral);
        Result := CreateBCValueString(StrVal);
      end;
    tkBooleanLiteral:
      begin
        BoolVal := SameText(FCurrentToken.Lexeme, 'TRUE');
        ConsumeToken(tkBooleanLiteral);
        Result := CreateBCValueBoolean(BoolVal);
      end;
    tkIdentifier: // If factor is a variable
      begin
        VarName := FCurrentToken.Lexeme;
        // --- Option Explicit Check ---
        if FOptionExplicitEnabled and not IsVariableDeclared(VarName) then
          raise Exception.CreateFmt('Parser Error: Variable "%s" not declared (Option Explicit On) at %d:%d',
            [VarName, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
        // --- End Option Explicit Check ---
        WriteLn(Format('Parser Note: Variable reference "%s" found. (Value lookup not implemented)', [VarName]));
        ConsumeToken(tkIdentifier);
        Result := CreateBCValueNull; // Placeholder value
      end;
    tkParenthesisOpen:
      begin
        ConsumeToken(tkParenthesisOpen);
        Result := ParseExpression;
        ConsumeToken(tkParenthesisClose);
      end;
    else
      raise Exception.CreateFmt('Parser Error: Unexpected token "%s" when expecting factor at %d:%d',
        [FCurrentToken.Lexeme, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
  end;
end;


function TParser.ParseTerm: TBCValue;
var
  Left, Right: TBCValue;
  Op: TToken;
begin
  Left := ParseFactor;
  while (FCurrentToken.TokenType = tkOperator) and
        (SameText(FCurrentToken.Lexeme, '*') or SameText(FCurrentToken.Lexeme, '/')) do
  begin
    Op := FCurrentToken;
    ConsumeToken(tkOperator);
    Right := ParseFactor;
    if (Left.ValueType = bcvtInteger) and (Right.ValueType = bcvtInteger) then
    begin
      if Op.Lexeme = '*' then
        Left.IntValue := Left.IntValue * Right.IntValue
      else if Op.Lexeme = '/' then
      begin
        if Right.IntValue = 0 then
          raise Exception.Create('Division by zero');
        Left.IntValue := Left.IntValue div Right.IntValue;
      end;
    end
    else
      raise Exception.Create('Type mismatch in arithmetic operation');
  end;
  Result := Left;
end;

function TParser.ParseExpression: TBCValue;
var
  Left, Right: TBCValue;
  Op: TToken;
begin
  Left := ParseTerm;
  while (FCurrentToken.TokenType = tkOperator) and
        (SameText(FCurrentToken.Lexeme, '+') or SameText(FCurrentToken.Lexeme, '-') or
         SameText(FCurrentToken.Lexeme, '&')) do
  begin
    Op := FCurrentToken;
    ConsumeToken(tkOperator);
    Right := ParseTerm;

    if Op.Lexeme = '+' then
    begin
      if (Left.ValueType = bcvtInteger) and (Right.ValueType = bcvtInteger) then
        Left.IntValue := Left.IntValue + Right.IntValue
      else
        raise Exception.Create('Type mismatch in addition');
    end
    else if Op.Lexeme = '-' then
    begin
      if (Left.ValueType = bcvtInteger) and (Right.ValueType = bcvtInteger) then
        Left.IntValue := Left.IntValue - Right.IntValue
      else
        raise Exception.Create('Type mismatch in subtraction');
    end
    else if Op.Lexeme = '&' then // String concatenation
    begin
      Left.StringValue := BCValueToString(Left) + BCValueToString(Right);
      Left.ValueType := bcvtString;
    end;
  end;
  Result := Left;
end;


procedure TParser.ParsePrintStatement;
var
  ExprValue: TBCValue;
begin
  ConsumeToken(tkKeyword); // Consume PRINT
  ExprValue := ParseExpression;
  WriteLn(Format('Parsed PRINT statement with value: %s (Type: %s)',
    [BCValueToString(ExprValue),
     GetBCValueTypeName(ExprValue.ValueType) // <<< FIXED: Using GetBCValueTypeName
     ]));
end;

procedure TParser.ParseShowStatement;
var
  FormNameToken: TToken;
begin
  ConsumeToken(tkKeyword); // Consume SHOW
  FormNameToken := FCurrentToken;
  if FormNameToken.TokenType <> tkIdentifier then
    raise Exception.CreateFmt('Parser Error: Expected form name (identifier) after SHOW at %d:%d',
      [FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
  ConsumeToken(tkIdentifier);

  WriteLn(Format('Parsed SHOW statement for form: %s', [FormNameToken.Lexeme]));
end;

procedure TParser.ParseLetStatement(const VarName: String);
var
  ExprValue: TBCValue;
begin
  // --- Option Explicit Check for Assignment ---
  if FOptionExplicitEnabled and not IsVariableDeclared(VarName) then
    raise Exception.CreateFmt('Parser Error: Variable "%s" not declared (Option Explicit On) at %d:%d',
      [VarName, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);

  // If Option Explicit is OFF, or if it's ON and variable is declared, then declare/register it.
  // This also handles implicit declaration if Option Explicit is Off.
  DeclareVariable(VarName);
  // --- End Option Explicit Check ---

  ConsumeToken(tkOperator); // Consume '='
  ExprValue := ParseExpression;

  WriteLn(Format('Parsed LET/Assignment statement: %s = %s (Type: %s)',
    [VarName, BCValueToString(ExprValue),
     GetBCValueTypeName(ExprValue.ValueType) // <<< FIXED: Using GetBCValueTypeName
     ]));
end;

procedure TParser.ParseDimStatement;
var
  VarName: String;
begin
  ConsumeToken(tkKeyword); // Consume 'DIM'

  // Parse variable name
  if FCurrentToken.TokenType <> tkIdentifier then
    raise Exception.CreateFmt('Parser Error: Expected variable name after DIM at %d:%d',
      [FCurrentToken.Line + 1, FCurrentToken.Column + 1]);

  VarName := FCurrentToken.Lexeme;
  ConsumeToken(tkIdentifier);

  // --- Handle Optional 'AS Type' (Conceptual for now) ---
  // If the next token is 'AS' (which is a tkKeyword in our lexer)
  if (FCurrentToken.TokenType = tkKeyword) and (SameText(FCurrentToken.Lexeme, 'AS')) then
  begin
    ConsumeToken(tkKeyword); // Consume 'AS'
    // Expect a type identifier (e.g., INTEGER, STRING, BOOLEAN)
    if FCurrentToken.TokenType <> tkIdentifier then
      raise Exception.CreateFmt('Parser Error: Expected type name after AS at %d:%d',
        [FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
    WriteLn(Format('Parser Note: Variable "%s" declared AS %s (Type handling not fully implemented)', [VarName, FCurrentToken.Lexeme]));
    ConsumeToken(tkIdentifier); // Consume the type name
  end;
  // --- End Optional 'AS Type' ---

  // Declare the variable in the symbol table
  DeclareVariable(VarName);
  WriteLn(Format('Parsed DIM statement for variable: %s', [VarName]));

  // Handle multiple declarations on one line (e.g., DIM A, B, C)
  while FCurrentToken.TokenType = tkComma do
  begin
    ConsumeToken(tkComma);
    if FCurrentToken.TokenType <> tkIdentifier then
      raise Exception.CreateFmt('Parser Error: Expected variable name after comma in DIM statement at %d:%d',
        [FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
    VarName := FCurrentToken.Lexeme;
    ConsumeToken(tkIdentifier);
    // Handle optional 'AS Type' for subsequent variables if desired (VB6 allows this)
    if (FCurrentToken.TokenType = tkKeyword) and (SameText(FCurrentToken.Lexeme, 'AS')) then
    begin
      ConsumeToken(tkKeyword);
      if FCurrentToken.TokenType <> tkIdentifier then
        raise Exception.CreateFmt('Parser Error: Expected type name after AS for "%s" at %d:%d',
          [VarName, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
      WriteLn(Format('Parser Note: Variable "%s" declared AS %s (Type handling not fully implemented)', [VarName, FCurrentToken.Lexeme]));
      ConsumeToken(tkIdentifier);
    end;
    DeclareVariable(VarName);
    WriteLn(Format('Parsed DIM statement for additional variable: %s', [VarName]));
  end;
end;


procedure TParser.ParseIfStatement;
var
  ConditionValue: TBCValue;
  ThenToken: TToken;
begin
  ConsumeToken(tkKeyword); // Consume IF

  ConditionValue := ParseExpression;

  ThenToken := FCurrentToken;
  if ThenToken.TokenType = tkKeyword then
  begin
    if SameText(ThenToken.Lexeme, 'THEN') then
    begin
      ConsumeToken(tkKeyword); // Consume THEN
      if (FCurrentToken.TokenType <> tkEndOfLine) and (FCurrentToken.TokenType <> tkEndOfFile) then
      begin
        WriteLn('  (Parsing statement after THEN)');
        ParseStatement;
      end;
    end
    else
      raise Exception.CreateFmt('Parser Error: Expected THEN but found "%s" in IF statement at %d:%d',
        [FCurrentToken.Lexeme, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
  end
  else
    raise Exception.CreateFmt('Parser Error: Expected THEN keyword in IF statement at %d:%d',
      [FCurrentToken.Line + 1, FCurrentToken.Column + 1]);

  WriteLn(Format('Parsed IF statement with condition of type: %s', [GetBCValueTypeName(ConditionValue.ValueType)]));
end;

procedure TParser.ParseOptionExplicitStatement;
var
  OptionToken: TToken;
  ExplicitToken: TToken;
  StateToken: TToken;
begin
  OptionToken := FCurrentToken;
  ConsumeToken(tkOption); // Consume 'Option'

  ExplicitToken := FCurrentToken;
  ConsumeToken(tkExplicit); // Consume 'Explicit'

  StateToken := FCurrentToken;
  if StateToken.TokenType = tkOn then
  begin
    FOptionExplicitEnabled := True;
    ConsumeToken(tkOn);
    WriteLn('Parser: OPTION EXPLICIT ON detected. All variables must be declared.');
  end
  else if StateToken.TokenType = tkOff then
  begin
    FOptionExplicitEnabled := False;
    ConsumeToken(tkOff);
    WriteLn('Parser: OPTION EXPLICIT OFF detected. Variables do not need explicit declaration.');
  end
    // --- Removed the 'else' branch that raised an error here ---
    // This allows 'Option Explicit' without 'On' or 'Off' to be silently ignored,
    // which might be desired for partial directives, though not standard VB6 behavior.
    // If you want to enforce 'On' or 'Off', re-add the 'else' branch here.
  else
    raise Exception.CreateFmt('Parser Error: Expected ON or OFF after "Option Explicit" at %d:%d',
      [FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
end;


procedure TParser.ParseStatement;
var
  CurrentLineNum: Integer;
  PotentialVarName: String;
begin
  CurrentLineNum := FCurrentToken.Line; // Use .Line as per TokenDefs
  // Skip comments on their own line
  if FCurrentToken.TokenType = tkComment then
  begin
    ConsumeToken(tkComment);
    if FCurrentToken.TokenType = tkEndOfLine then // If comment occupies full line
      ConsumeToken(tkEndOfLine);
    ParseStatement; // Try parsing next statement
    Exit;
  end;

  // Process multiple statements on one line separated by ':'
  while FCurrentToken.TokenType <> tkEndOfLine do
  begin
    case FCurrentToken.TokenType of
      tkKeyword:
        begin
          case LowerCase(FCurrentToken.Lexeme) of
            'print': ParsePrintStatement;
            'show': ParseShowStatement;
            'if': ParseIfStatement;
            'dim': ParseDimStatement; // Handle DIM statement
            'end': // Handle 'END' as program termination
              begin
                ConsumeToken(tkKeyword); // Consume END
                WriteLn('Parsed END statement. Program will terminate.');
                Exit; // Stop parsing
              end;
            else
              raise Exception.CreateFmt('Parser Error: Unrecognized keyword "%s" at %d:%d',
                [FCurrentToken.Lexeme, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
          end; // case LowerCase(FCurrentToken.Lexeme)
        end;
      tkIdentifier: // Could be assignment, or a CALL to a subroutine
        begin
          PotentialVarName := FCurrentToken.Lexeme;
          ConsumeToken(tkIdentifier); // Consume the identifier

          if FCurrentToken.TokenType = tkOperator then
          begin
            if FCurrentToken.Lexeme = '=' then
            begin
              ParseLetStatement(PotentialVarName); // Call assignment parser
            end
            else
              raise Exception.CreateFmt('Parser Error: Expected "=" after identifier "%s" at %d:%d',
                [PotentialVarName, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
          end
          else
            raise Exception.CreateFmt('Parser Error: Unexpected token "%s" after identifier "%s". Expected "=" or end of statement. at %d:%d',
              [FCurrentToken.Lexeme, PotentialVarName, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
        end;
      tkOption: // Handle Option Explicit directive
        ParseOptionExplicitStatement;
      tkEndOfFile: Exit; // Reached end of file, stop parsing

      else
        raise Exception.CreateFmt('Parser Error: Unexpected token "%s" at start of statement at %d:%d',
          [FCurrentToken.Lexeme, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
    end; // case FCurrentToken.TokenType

    // After parsing a statement, check for ':' for multi-statement lines
    if FCurrentToken.TokenType = tkColon then
    begin
      ConsumeToken(tkColon); // Consume the colon
    end
    else if FCurrentToken.TokenType = tkEndOfLine then
    begin
      ConsumeToken(tkEndOfLine);
      Break;
    end
    else if FCurrentToken.TokenType = tkEndOfFile then
    begin
      Break; // Exit loop if EOF after statement
    end
    else
    begin
      raise Exception.CreateFmt('Parser Error: Expected EOL or ":" after statement, but found "%s" at %d:%d',
        [FCurrentToken.Lexeme, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
    end;
  end; // while FCurrentToken.TokenType <> tkEndOfLine
end;


procedure TParser.ParseProgram;
begin
  while FCurrentToken.TokenType <> tkEndOfFile do
  begin
    ParseStatement; // Parse one statement (which might include multiple on a line)
  end;
  WriteLn('Program parsing finished.');
end;

end.


function TParser.ParseFactor: TBCValue;
var
  NumVal: Int64;
  StrVal: String;
  BoolVal: Boolean;
  VarName: String;
begin
  case FCurrentToken.TokenType of
    tkIntegerLiteral:
      begin
        NumVal := StrToInt64(FCurrentToken.Lexeme);
        ConsumeToken(tkIntegerLiteral);
        Result := CreateBCValueInteger(NumVal);
      end;
    tkStringLiteral:
      begin
        StrVal := Copy(FCurrentToken.Lexeme, 2, Length(FCurrentToken.Lexeme) - 2);
        ConsumeToken(tkStringLiteral);
        Result := CreateBCValueString(StrVal);
      end;
    tkBooleanLiteral:
      begin
        BoolVal := SameText(FCurrentToken.Lexeme, 'TRUE');
        ConsumeToken(tkBooleanLiteral);
        Result := CreateBCValueBoolean(BoolVal);
      end;
    tkIdentifier: // If factor is a variable
      begin
        VarName := FCurrentToken.Lexeme;
        // --- Option Explicit Check ---
        if FOptionExplicitEnabled and not IsVariableDeclared(VarName) then
          raise Exception.CreateFmt('Parser Error: Variable "%s" not declared (Option Explicit On) at %d:%d',
            [VarName, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
        // --- End Option Explicit Check ---
        WriteLn(Format('Parser Note: Variable reference "%s" found. (Value lookup not implemented)', [VarName]));
        ConsumeToken(tkIdentifier);
        Result := CreateBCValueNull; // Placeholder value
      end;
    tkParenthesisOpen:
      begin
        ConsumeToken(tkParenthesisOpen);
        Result := ParseExpression;
        ConsumeToken(tkParenthesisClose);
      end;
    else
      raise Exception.CreateFmt('Parser Error: Unexpected token "%s" when expecting factor at %d:%d',
        [FCurrentToken.Lexeme, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
  end;
end;


function TParser.ParseTerm: TBCValue;
var
  Left, Right: TBCValue;
  Op: TToken;
begin
  Left := ParseFactor;
  while (FCurrentToken.TokenType = tkOperator) and
        (SameText(FCurrentToken.Lexeme, '*') or SameText(FCurrentToken.Lexeme, '/')) do
  begin
    Op := FCurrentToken;
    ConsumeToken(tkOperator);
    Right := ParseFactor;
    if (Left.ValueType = bcvtInteger) and (Right.ValueType = bcvtInteger) then
    begin
      if Op.Lexeme = '*' then
        Left.IntValue := Left.IntValue * Right.IntValue
      else if Op.Lexeme = '/' then
      begin
        if Right.IntValue = 0 then
          raise Exception.Create('Division by zero');
        Left.IntValue := Left.IntValue div Right.IntValue;
      end;
    end
    else
      raise Exception.Create('Type mismatch in arithmetic operation');
  end;
  Result := Left;
end;

function TParser.ParseExpression: TBCValue;
var
  Left, Right: TBCValue;
  Op: TToken;
begin
  Left := ParseTerm;
  while (FCurrentToken.TokenType = tkOperator) and
        (SameText(FCurrentToken.Lexeme, '+') or SameText(FCurrentToken.Lexeme, '-') or
         SameText(FCurrentToken.Lexeme, '&')) do
  begin
    Op := FCurrentToken;
    ConsumeToken(tkOperator);
    Right := ParseTerm;

    if Op.Lexeme = '+' then
    begin
      if (Left.ValueType = bcvtInteger) and (Right.ValueType = bcvtInteger) then
        Left.IntValue := Left.IntValue + Right.IntValue
      else
        raise Exception.Create('Type mismatch in addition');
    end
    else if Op.Lexeme = '-' then
    begin
      if (Left.ValueType = bcvtInteger) and (Right.ValueType = bcvtInteger) then
        Left.IntValue := Left.IntValue - Right.IntValue
      else
        raise Exception.Create('Type mismatch in subtraction');
    end
    else if Op.Lexeme = '&' then // String concatenation
    begin
      Left.StringValue := BCValueToString(Left) + BCValueToString(Right);
      Left.ValueType := bcvtString;
    end;
  end;
  Result := Left;
end;


procedure TParser.ParsePrintStatement;
var
  ExprValue: TBCValue;
begin
  ConsumeToken(tkKeyword); // Consume PRINT
  ExprValue := ParseExpression;
  WriteLn(Format('Parsed PRINT statement with value: %s (Type: %s)',
    [BCValueToString(ExprValue),
     GetTokenTypeName(ExprValue.ValueType)
     ]));
end;

procedure TParser.ParseShowStatement;
var
  FormNameToken: TToken;
begin
  ConsumeToken(tkKeyword); // Consume SHOW
  FormNameToken := FCurrentToken;
  if FormNameToken.TokenType <> tkIdentifier then
    raise Exception.CreateFmt('Parser Error: Expected form name (identifier) after SHOW at %d:%d',
      [FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
  ConsumeToken(tkIdentifier);

  WriteLn(Format('Parsed SHOW statement for form: %s', [FormNameToken.Lexeme]));
end;

procedure TParser.ParseLetStatement(const VarName: String);
var
  ExprValue: TBCValue;
begin
  // --- Option Explicit Check for Assignment ---
  if FOptionExplicitEnabled and not IsVariableDeclared(VarName) then
    raise Exception.CreateFmt('Parser Error: Variable "%s" not declared (Option Explicit On) at %d:%d',
      [VarName, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);

  // If Option Explicit is OFF, or if it's ON and variable is declared, then declare/register it.
  // This also handles implicit declaration if Option Explicit is Off.
  DeclareVariable(VarName);
  // --- End Option Explicit Check ---

  ConsumeToken(tkOperator); // Consume '='
  ExprValue := ParseExpression;

  WriteLn(Format('Parsed LET/Assignment statement: %s = %s (Type: %s)',
    [VarName, BCValueToString(ExprValue),
     GetTokenTypeName(ExprValue.ValueType)
     ]));
end;

procedure TParser.ParseDimStatement;
var
  VarName: String;
begin
  ConsumeToken(tkKeyword); // Consume 'DIM'

  // Parse variable name
  if FCurrentToken.TokenType <> tkIdentifier then
    raise Exception.CreateFmt('Parser Error: Expected variable name after DIM at %d:%d',
      [FCurrentToken.Line + 1, FCurrentToken.Column + 1]);

  VarName := FCurrentToken.Lexeme;
  ConsumeToken(tkIdentifier);

  // --- Handle Optional 'AS Type' (Conceptual for now) ---
  // If the next token is 'AS' (which is a tkKeyword in our lexer)
  if (FCurrentToken.TokenType = tkKeyword) and (SameText(FCurrentToken.Lexeme, 'AS')) then
  begin
    ConsumeToken(tkKeyword); // Consume 'AS'
    // Expect a type identifier (e.g., INTEGER, STRING, BOOLEAN)
    if FCurrentToken.TokenType <> tkIdentifier then
      raise Exception.CreateFmt('Parser Error: Expected type name after AS at %d:%d',
        [FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
    WriteLn(Format('Parser Note: Variable "%s" declared AS %s (Type handling not fully implemented)', [VarName, FCurrentToken.Lexeme]));
    ConsumeToken(tkIdentifier); // Consume the type name
  end;
  // --- End Optional 'AS Type' ---

  // Declare the variable in the symbol table
  DeclareVariable(VarName);
  WriteLn(Format('Parsed DIM statement for variable: %s', [VarName]));

  // Handle multiple declarations on one line (e.g., DIM A, B, C)
  while FCurrentToken.TokenType = tkComma do
  begin
    ConsumeToken(tkComma);
    if FCurrentToken.TokenType <> tkIdentifier then
      raise Exception.CreateFmt('Parser Error: Expected variable name after comma in DIM statement at %d:%d',
        [FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
    VarName := FCurrentToken.Lexeme;
    ConsumeToken(tkIdentifier);
    // Handle optional 'AS Type' for subsequent variables if desired (VB6 allows this)
    if (FCurrentToken.TokenType = tkKeyword) and (SameText(FCurrentToken.Lexeme, 'AS')) then
    begin
      ConsumeToken(tkKeyword);
      if FCurrentToken.TokenType <> tkIdentifier then
        raise Exception.CreateFmt('Parser Error: Expected type name after AS for "%s" at %d:%d',
          [VarName, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
      WriteLn(Format('Parser Note: Variable "%s" declared AS %s (Type handling not fully implemented)', [VarName, FCurrentToken.Lexeme]));
      ConsumeToken(tkIdentifier);
    end;
    DeclareVariable(VarName);
    WriteLn(Format('Parsed DIM statement for additional variable: %s', [VarName]));
  end;
end;


procedure TParser.ParseIfStatement;
var
  ConditionValue: TBCValue;
  ThenToken: TToken;
begin
  ConsumeToken(tkKeyword); // Consume IF

  ConditionValue := ParseExpression;

  ThenToken := FCurrentToken;
  if ThenToken.TokenType = tkKeyword then
  begin
    if SameText(ThenToken.Lexeme, 'THEN') then
    begin
      ConsumeToken(tkKeyword); // Consume THEN
      if (FCurrentToken.TokenType <> tkEndOfLine) and (FCurrentToken.TokenType <> tkEndOfFile) then
      begin
        WriteLn('  (Parsing statement after THEN)');
        ParseStatement;
      end;
    end
    else
      raise Exception.CreateFmt('Parser Error: Expected THEN but found "%s" in IF statement at %d:%d',
        [FCurrentToken.Lexeme, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
  end
  else
    raise Exception.CreateFmt('Parser Error: Expected THEN keyword in IF statement at %d:%d',
      [FCurrentToken.Line + 1, FCurrentToken.Column + 1]);

  WriteLn(Format('Parsed IF statement with condition of type: %s', [GetTokenTypeName(ConditionValue.ValueType)]));
end;

procedure TParser.ParseOptionExplicitStatement;
var
  OptionToken: TToken;
  ExplicitToken: TToken;
  StateToken: TToken;
begin
  OptionToken := FCurrentToken;
  ConsumeToken(tkOption); // Consume 'Option'

  ExplicitToken := FCurrentToken;
  ConsumeToken(tkExplicit); // Consume 'Explicit'

  StateToken := FCurrentToken;
  if StateToken.TokenType = tkOn then
  begin
    FOptionExplicitEnabled := True;
    ConsumeToken(tkOn);
    WriteLn('Parser: OPTION EXPLICIT ON detected. All variables must be declared.');
  end
  else if StateToken.TokenType = tkOff then
  begin
    FOptionExplicitEnabled := False;
    ConsumeToken(tkOff);
    WriteLn('Parser: OPTION EXPLICIT OFF detected. Variables do not need explicit declaration.');
  end
  else
    raise Exception.CreateFmt('Parser Error: Expected ON or OFF after "Option Explicit" at %d:%d',
      [FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
end;


procedure TParser.ParseStatement;
var
  CurrentLineNum: Integer;
  PotentialVarName: String;
begin
  CurrentLineNum := FCurrentToken.Line; // Use .Line as per TokenDefs
  // Skip comments on their own line
  if FCurrentToken.TokenType = tkComment then
  begin
    ConsumeToken(tkComment);
    if FCurrentToken.TokenType = tkEndOfLine then // If comment occupies full line
      ConsumeToken(tkEndOfLine);
    ParseStatement; // Try parsing next statement
    Exit;
  end;

  // Process multiple statements on one line separated by ':'
  while FCurrentToken.TokenType <> tkEndOfLine do
  begin
    case FCurrentToken.TokenType of
      tkKeyword:
        begin
          case LowerCase(FCurrentToken.Lexeme) of
            'print': ParsePrintStatement;
            'show': ParseShowStatement;
            'if': ParseIfStatement;
            'dim': ParseDimStatement; // Handle DIM statement
            'end': // Handle 'END' as program termination
              begin
                ConsumeToken(tkKeyword); // Consume END
                WriteLn('Parsed END statement. Program will terminate.');
                Exit; // Stop parsing
              end;
            else
              raise Exception.CreateFmt('Parser Error: Unrecognized keyword "%s" at %d:%d',
                [FCurrentToken.Lexeme, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
          end; // case LowerCase(FCurrentToken.Lexeme)
        end;
      tkIdentifier: // Could be assignment, or a CALL to a subroutine
        begin
          PotentialVarName := FCurrentToken.Lexeme;
          ConsumeToken(tkIdentifier); // Consume the identifier

          if FCurrentToken.TokenType = tkOperator then
          begin
            if FCurrentToken.Lexeme = '=' then
            begin
              ParseLetStatement(PotentialVarName); // Call assignment parser
            end
            else
              raise Exception.CreateFmt('Parser Error: Expected "=" after identifier "%s" at %d:%d',
                [PotentialVarName, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
          end
          else
            raise Exception.CreateFmt('Parser Error: Unexpected token "%s" after identifier "%s". Expected "=" or end of statement. at %d:%d',
              [FCurrentToken.Lexeme, PotentialVarName, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
        end;
      tkOption: // Handle Option Explicit directive
        ParseOptionExplicitStatement;
      tkEndOfFile: Exit; // Reached end of file, stop parsing

      else
        raise Exception.CreateFmt('Parser Error: Unexpected token "%s" at start of statement at %d:%d',
          [FCurrentToken.Lexeme, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
    end; // case FCurrentToken.TokenType

    // After parsing a statement, check for ':' for multi-statement lines
    if FCurrentToken.TokenType = tkColon then
    begin
      ConsumeToken(tkColon); // Consume the colon
    end
    else if FCurrentToken.TokenType = tkEndOfLine then
    begin
      ConsumeToken(tkEndOfLine);
      Break;
    end
    else if FCurrentToken.TokenType = tkEndOfFile then
    begin
      Break; // Exit loop if EOF after statement
    end
    else
    begin
      raise Exception.CreateFmt('Parser Error: Expected EOL or ":" after statement, but found "%s" at %d:%d',
        [FCurrentToken.Lexeme, FCurrentToken.Line + 1, FCurrentToken.Column + 1]);
    end;
  end; // while FCurrentToken.TokenType <> tkEndOfLine
end;


procedure TParser.ParseProgram;
begin
  while FCurrentToken.TokenType <> tkEndOfFile do
  begin
    ParseStatement; // Parse one statement (which might include multiple on a line)
  end;
  WriteLn('Program parsing finished.');
end;

end.
