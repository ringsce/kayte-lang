unit Parser;

interface

uses
  SysUtils, Classes,
  TokenDefs,     // <--- Correctly uses for TToken, TTokenType
  Lexer,         // For TLexer
  BytecodeTypes, // <--- Correctly uses for TBCValue and CreateBCValue* functions
  TypInfo;       // For GetEnumName

// Remove 'Forms' unless you actually need it in the interface for GUI elements.
// Remove the explicit declarations of CreateBCValue* functions here. They are in BytecodeTypes.pas

type
  TParser = class
  private
    // --- ALL FIELDS MUST BE DECLARED FIRST IN THIS SECTION ---
    FLexer: TLexer;
    FCurrentToken: TToken; // <--- Make sure this line is here, AT THE TOP OF 'private'

    // --- THEN ALL METHODS AND PROPERTIES ---
    procedure ConsumeToken(ExpectedType: TTokenType);
    function PeekToken: TToken;

    procedure ParsePrintStatement;
    procedure ParseShowStatement;
    procedure ParseLetStatement(const VarName: String);
    procedure ParseIfStatement;

    function ParseExpression: TBCValue; forward; // Forward declarations are fine here
    function ParseTerm: TBCValue; forward;
    function ParseFactor: TBCValue; forward;

  public // Public methods are fine after private methods, but public fields would also go at the top of 'public'
    constructor Create(ALexer: TLexer);
    destructor Destroy; override;
    procedure ParseProgram;
  end;


implementation

{ TParser }

constructor TParser.Create(ALexer: TLexer);
begin
  inherited Create;
  FLexer := ALexer; // Assign, not own
  FLexer.Reset;
  FCurrentToken := FLexer.GetNextToken; // Get first token
end;

destructor TParser.Destroy;
begin
  // FLexer is not owned here
  inherited Destroy;
end;

procedure TParser.ConsumeToken(ExpectedType: TTokenType);
begin
  if FCurrentToken.TokenType = ExpectedType then
    FCurrentToken := FLexer.GetNextToken
  else
    raise Exception.CreateFmt('Parser Error: Expected %s but found %s ("%s") at %d:%d',
      [GetEnumName(TypeInfo(TTokenType), Ord(ExpectedType)), // GetEnumName is now visible
       GetEnumName(TypeInfo(TTokenType), Ord(FCurrentToken.TokenType)), // GetEnumName is now visible
       FCurrentToken.Lexeme,
       FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);
end;

function TParser.PeekToken: TToken;
begin
  // This is a simplification. A real parser often needs true look-ahead from lexer.
  // For now, assume GetNextToken moves the internal lexer state.
  // A better Peek: FLexer would have a buffer of tokens.
  Result := FCurrentToken;
end;

function TParser.ParseFactor: TBCValue;
var
  NumVal: Integer;
  StrVal: String;
  BoolVal: Boolean;
begin
  case FCurrentToken.TokenType of
    tkIntegerLiteral:
      begin
        NumVal := StrToInt(FCurrentToken.Lexeme); // <-- Changed from .Value to .Lexeme
        ConsumeToken(tkIntegerLiteral);
        Result := CreateBCValueInteger(NumVal); // Assuming CreateBCValueInteger helper is used
      end;
    tkStringLiteral:
      begin
        // Remove quotes from string literal value for internal use
        StrVal := Copy(FCurrentToken.Lexeme, 2, Length(FCurrentToken.Lexeme) - 2); // <-- Changed from .Value to .Lexeme
        ConsumeToken(tkStringLiteral);
        Result := CreateBCValueString(StrVal); // Assuming CreateBCValueString helper is used
      end;
    tkBooleanLiteral:
      begin
        BoolVal := SameText(FCurrentToken.Lexeme, 'TRUE'); // <-- Changed from .Value to .Lexeme
        ConsumeToken(tkBooleanLiteral);
        Result := CreateBCValueBoolean(BoolVal); // Assuming CreateBCValueBoolean helper is used
      end;
    tkIdentifier: // If factor is a variable
      begin
        // In a real VM, this would generate OP_PUSH_VAR
        // For now, assume it's an error or get value from existing system
        WriteLn(Format('Parser Note: Variable reference "%s" found. (Value lookup not implemented)', [FCurrentToken.Lexeme])); // <-- Changed from .Value to .Lexeme
        // You would typically call a GetVar function here or generate code to do so
        // For now, return a null value
        ConsumeToken(tkIdentifier);
        Result := CreateBCValueNull; // Assuming CreateBCValueNull helper is used
      end;
    tkParenthesisOpen:
      begin
        ConsumeToken(tkParenthesisOpen);
        Result := ParseExpression; // Handle ( Expression )
        ConsumeToken(tkParenthesisClose);
      end;
    else
      raise Exception.CreateFmt('Parser Error: Unexpected token "%s" when expecting factor at %d:%d',
        [FCurrentToken.Lexeme, FCurrentToken.LineNum + 1, FCurrentToken.ColNum]); // <-- Changed from .Value to .Lexeme
  end;
end;


function TParser.ParseTerm: TBCValue;
var
  Left, Right: TBCValue;
  Op: TToken;
begin
  Left := ParseFactor;
  while (FCurrentToken.TokenType = tkOperator) and
        (SameText(FCurrentToken.Lexeme, '*') or SameText(FCurrentToken.Lexeme, '/')) do // <-- Changed .Value to .Lexeme
  begin
    Op := FCurrentToken;
    ConsumeToken(tkOperator);
    Right := ParseFactor;
    // Perform operation (for direct interpretation) or generate opcode (for VM)
    // Simplified for now, just example for integer multiplication
    if (Left.ValueType = bcvtInteger) and (Right.ValueType = bcvtInteger) then
    begin
      if Op.Lexeme = '*' then // <-- Changed .Value to .Lexeme
        Left.IntValue := Left.IntValue * Right.IntValue
      else if Op.Lexeme = '/' then // <-- Changed .Value to .Lexeme
      begin
        if Right.IntValue = 0 then
          raise Exception.Create('Division by zero');
        Left.IntValue := Left.IntValue div Right.IntValue; // Integer division
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
        (SameText(FCurrentToken.Value, '+') or SameText(FCurrentToken.Value, '-') or
         SameText(FCurrentToken.Value, '&')) do // Handle '&' for string concat
  begin
    Op := FCurrentToken;
    ConsumeToken(tkOperator);
    Right := ParseTerm;

    // Perform operation or generate opcode
    if Op.Value = '+' then
    begin
      if (Left.ValueType = bcvtInteger) and (Right.ValueType = bcvtInteger) then
        Left.IntValue := Left.IntValue + Right.IntValue
      else
        raise Exception.Create('Type mismatch in addition');
    end
    else if Op.Value = '-' then
    begin
      if (Left.ValueType = bcvtInteger) and (Right.ValueType = bcvtInteger) then
        Left.IntValue := Left.IntValue - Right.IntValue
      else
        raise Exception.Create('Type mismatch in subtraction');
    end
    else if Op.Value = '&' then // String concatenation
    begin
      // Ensure values are convertible to strings
      Left.ValueType := bcvtString;
      Right.ValueType := bcvtString; // Simplified: assumes both are strings
      // You'd need conversion logic here if they aren't already strings
      if Assigned(Left.StrValuePtr) and Assigned(Right.StrValuePtr) then
        Left.StrValuePtr^ := Left.StrValuePtr^ + Right.StrValuePtr^
      else if Assigned(Left.StrValuePtr) then
        Left.StrValuePtr^ := Left.StrValuePtr^ + IntToStr(Right.IntValue) // Example mixed type
      else if Assigned(Right.StrValuePtr) then
        Left.StrValuePtr^ := IntToStr(Left.IntValue) + Right.StrValuePtr^; // Example mixed type
      // For proper String concatenation from other types you'd need functions like Str() or CStr()
    end;
  end;
  Result := Left;
end;


procedure TParser.ParsePrintStatement;
var
  ExprValue: TBCValue;
begin
  ConsumeToken(tkKeyword); // Consume PRINT
  ExprValue := ParseExpression; // Parse what to print
  // Here, you would either:
  // 1. Directly print the value for a direct interpreter
  //    case ExprValue.ValueType of
  //      bcvtInteger: WriteLn(ExprValue.IntValue);
  //      bcvtString: WriteLn(ExprValue.StrValuePtr^);
  //      bcvtBoolean: WriteLn(ExprValue.BoolValue);
  //    end;
  // 2. Generate OP_PRINT bytecode for a VM
  WriteLn(Format('Parsed PRINT statement with value: %s (Type: %s)',
    [GetEnumName(TypeInfo(TBCValueType), Ord(ExprValue.ValueType)), ExprValue.ValueType.ToString])); // Placeholder
  ExprValue.Destroy; // Free the TBCValue's internal string if it's a string
end;

procedure TParser.ParseShowStatement;
var
  FormNameToken: TToken;
begin
  ConsumeToken(tkKeyword); // Consume SHOW
  FormNameToken := FCurrentToken; // Get the form name
  if FormNameToken.TokenType <> tkIdentifier then
    raise Exception.CreateFmt('Parser Error: Expected form name (identifier) after SHOW at %d:%d',
      [FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);
  ConsumeToken(tkIdentifier); // Consume the identifier

  // Here, you would either:
  // 1. Directly find/create and show the form for a direct interpreter (similar to old ExecuteLine)
  // 2. Generate OP_SHOW_FORM bytecode for a VM (operand would be index to FormMap/StringLiterals)
  WriteLn(Format('Parsed SHOW statement for form: %s', [FormNameToken.Value])); // Placeholder
end;

procedure TParser.ParseLetStatement(const VarName: String);
var
  ExprValue: TBCValue;
begin
  // Assume 'LET' or implicit assignment 'Var = Value'
  // If 'LET' is explicitly used: ConsumeToken(tkKeyword); // Consume LET (optional)

  // VarName is already known, now expect '='
  ConsumeToken(tkOperator); // Consume '='
  ExprValue := ParseExpression; // Parse the value to assign

  // Here, you would either:
  // 1. Directly set the variable value in your runtime Vars list
  //    SetVar(VarName, ExprValue); // You'd need to adapt SetVar to use TBCValue
  // 2. Generate OP_POP_VAR and OP_PUSH_VAR (if var on stack) or specific assignment opcode for VM
  WriteLn(Format('Parsed LET/Assignment statement: %s = %s (Type: %s)',
    [VarName, GetEnumName(TypeInfo(TBCValueType), Ord(ExprValue.ValueType)), ExprValue.ValueType.ToString])); // Placeholder
  ExprValue.Destroy; // Free the TBCValue's internal string
end;


procedure TParser.ParseIfStatement;
var
  ConditionValue: TBCValue;
  ThenToken: TToken;
  // If you implement a full IF, you'd need to handle:
  // - Nested IFs
  // - ELSEIF / ELSE
  // - Multi-line IF blocks vs single-line IFs
begin
  ConsumeToken(tkKeyword); // Consume IF

  ConditionValue := ParseExpression; // Parse the condition (e.g., X > 5)
  // In a real parser, this would generate bytecode for evaluation and a conditional jump

  ThenToken := FCurrentToken;
  if ThenToken.TokenType = tkKeyword then
  begin
    if SameText(ThenToken.Value, 'THEN') then
    begin
      ConsumeToken(tkKeyword); // Consume THEN
      // Now parse the statement(s) that follow THEN
      // For simplicity, assume single statement on same line for now
      // A full IF would handle EOL and new lines for block IFs.
      if (FCurrentToken.TokenType <> tkEndOfLine) and (FCurrentToken.TokenType <> tkEndOfFile) then
      begin
        WriteLn('  (Parsing statement after THEN)');
        ParseStatement; // Recursively parse the statement after THEN
      end;
    end
    else
      raise Exception.CreateFmt('Parser Error: Expected THEN but found "%s" in IF statement at %d:%d',
        [FCurrentToken.Value, FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);
  end
  else
    raise Exception.CreateFmt('Parser Error: Expected THEN keyword in IF statement at %d:%d',
      [FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);

  // For multi-line IFs, you would then look for ELSEIF, ELSE, END IF
  // This simple parser assumes single line IF.
  WriteLn(Format('Parsed IF statement with condition of type: %s', [GetEnumName(TypeInfo(TBCValueType), Ord(ConditionValue.ValueType))])); // Placeholder
  ConditionValue.Destroy; // Free any string in condition
end;


procedure TParser.ParseStatement;
var
  CurrentLineNum: Integer;
begin
  CurrentLineNum := FCurrentToken.LineNum;
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
          case LowerCase(FCurrentToken.Value) of
            'print': ParsePrintStatement;
            'show': ParseShowStatement;
            'if': ParseIfStatement;
            'end': // Handle 'END' as program termination
              begin
                ConsumeToken(tkKeyword); // Consume END
                WriteLn('Parsed END statement. Program will terminate.');
                Exit; // Stop parsing
              end;
            // Add more keywords and their respective parsing procedures
            else
              raise Exception.CreateFmt('Parser Error: Unrecognized keyword "%s" at %d:%d',
                [FCurrentToken.Value, FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);
          end; // case LowerCase(FCurrentToken.Value)
        end;
      tkIdentifier: // Could be assignment, or a CALL to a subroutine
        begin
          // Peek ahead to see if it's an assignment (e.g., Var = Expr)
          // This simplified parser doesn't have true lookahead in ConsumeToken,
          // so it's a bit tricky. A more robust parser would use a lookahead buffer.
          // For now, assume if it's an identifier not followed by '=' it's a syntax error
          // Or, assume it's an assignment if '=' is the next actual token.
          // This requires a real PeekToken in the lexer that returns next token without advancing.
          // For now, we'll consume the identifier and check the next token.

          // Simplified: assume 'Identifier' means 'LET Identifier = Value' syntax
          // In VB6, 'Let' is optional. So 'MyVar = 10' is an assignment.
          // If the next token is '=', it's an assignment.
            PotentialVarName: String;
            NextTok: TToken;
          begin
            PotentialVarName := FCurrentToken.Value;
            ConsumeToken(tkIdentifier); // Consume the identifier
            if FCurrentToken.TokenType = tkOperator then
            begin
              if FCurrentToken.Value = '=' then
              begin
                ParseLetStatement(PotentialVarName); // Call assignment parser
              end
              else
                raise Exception.CreateFmt('Parser Error: Expected "=" after identifier "%s" at %d:%d',
                  [PotentialVarName, FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);
            end
            else
              raise Exception.CreateFmt('Parser Error: Unexpected token "%s" after identifier "%s". Expected "=" or end of statement. at %d:%d',
                [FCurrentToken.Value, PotentialVarName, FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);
          end;
        end;
      tkEndOfFile: Exit; // Reached end of file, stop parsing

      else
        raise Exception.CreateFmt('Parser Error: Unexpected token "%s" at start of statement at %d:%d',
          [FCurrentToken.Value, FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);
    end; // case FCurrentToken.TokenType

    // After parsing a statement, check for ':' for multi-statement lines
    if FCurrentToken.TokenType = tkColon then
    begin
      ConsumeToken(tkColon); // Consume the colon
      // Continue loop to parse next statement on the same line
    end
    else if FCurrentToken.TokenType = tkEndOfLine then
    begin
      // Consume EOL and exit for this line
      ConsumeToken(tkEndOfLine);
      Break;
    end
    else if FCurrentToken.TokenType = tkEndOfFile then
    begin
      Break; // Exit loop if EOF after statement
    end
    else
    begin
      // If we are not at EOL, Colon or EOF, it's a syntax error
      raise Exception.CreateFmt('Parser Error: Expected EOL or ":" after statement, but found "%s" at %d:%d',
        [FCurrentToken.Value, FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);
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
