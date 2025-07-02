unit Parser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  TokenDefs,       // <--- Correctly uses for TToken, TTokenType
  Lexer,           // For TLexer
  BytecodeTypes in '../source/BytecodeTypes.pas',   // <--- Correctly uses for TBCValue and CreateBCValue* functions (for interface types)
  TypInfo,         // For GetEnumName and TypeInfo
  fgl,             // If you use FGL (Free Pascal Generics Library) - keep if needed, otherwise remove
  Forms, StdCtrls,  // For TForm, TEdit (used in TVM)
  FKfrmRuntime;    // For TKfrmRuntime (used in TVM)

type
  IInterpreterCallback = interface
    ['{F2C6C1B0-9A1A-4D7E-9F4F-7B3B5C9D0E1F}'] // Generate a new GUID for your actual project
    procedure Log(const Msg: String);
  end;

  TVM = class(TObject)
  private
    FCallback: IInterpreterCallback;
    FKfrmRuntime: TKfrmRuntime; // Instance of your runtime handler

  public
    destructor Destroy; override;
    constructor Create(ACallback: IInterpreterCallback; AKfrmRuntime: TKfrmRuntime);
    procedure ExecuteInterpretedFunction(const AFunctionName: String);
  end;

  TParser = class
  private
    FLexer: TLexer;
    FCurrentToken: TToken;

    procedure ConsumeToken(ExpectedType: TTokenType);
    function PeekToken: TToken;

    procedure ParsePrintStatement;
    procedure ParseShowStatement;
    procedure ParseLetStatement(const VarName: String);
    procedure ParseIfStatement;
    procedure ParseStatement; // Declaration for ParseStatement

    function ParseExpression: TBCValue;
    function ParseTerm: TBCValue;
    function ParseFactor: TBCValue;

  public
    constructor Create(ALexer: TLexer);
    destructor Destroy; override;
    procedure ParseProgram;
  end;

implementation

// Add units needed only in the implementation section here

{ TVM }

constructor TVM.Create(ACallback: IInterpreterCallback; AKfrmRuntime: TKfrmRuntime);
begin
  inherited Create;
  FCallback := ACallback;
  FKfrmRuntime := AKfrmRuntime; // Assign the passed runtime instance
  if Assigned(FCallback) then
    FCallback.Log('VM: Virtual Machine created and initialized.');
end;

destructor TVM.Destroy;
begin
  if Assigned(FCallback) then
    FCallback.Log('VM: Virtual Machine destroyed.');
  FKfrmRuntime := nil;
  FCallback := nil;
  inherited Destroy;
end;


procedure TVM.ExecuteInterpretedFunction(const AFunctionName: String);
  var
    LoadedLoginForm: TForm;
    UsernameEdit: TEdit;
    PasswordEdit: TEdit;
  begin
    if Assigned(FCallback) then
      FCallback.Log(SysUtils.Format('VM: Attempting to execute interpreted function: %s', [AFunctionName]));

    if AFunctionName = 'HandleLoginButton' then
    begin
      if Assigned(FCallback) then
      begin
        FCallback.Log('VM: Executing interpreted HandleLoginButton logic.');

        LoadedLoginForm := FKfrmRuntime.GetLoadedForm('LoginForm');
        if Assigned(LoadedLoginForm) then
        begin
          UsernameEdit := TEdit(FKfrmRuntime.GetControlFromForm(LoadedLoginForm, 'EditUsername'));
          PasswordEdit := TEdit(FKfrmRuntime.GetControlFromForm(LoadedLoginForm, 'EditPassword'));

          if Assigned(UsernameEdit) and Assigned(PasswordEdit) then
          begin
            FCallback.Log(SysUtils.Format('Interpreted Login - Username: "%s", Password: "%s"', [UsernameEdit.Text, PasswordEdit.Text]));

            if (UsernameEdit.Text = 'admin') and (PasswordEdit.Text = 'password') then
              FCallback.Log('VM: Login Successful (interpreted)')
            else
              FCallback.Log('VM: Login Failed (interpreted)');
          end;
        end;
      end;
    end
    else if AFunctionName = 'HandleCancelButton' then
    begin
      if Assigned(FCallback) then
      begin
        FCallback.Log('VM: Executing interpreted HandleCancelButton logic. Closing form.');
        FKfrmRuntime.CloseKfrmForm('LoginForm');
      end;
    end
    else
    begin
      if Assigned(FCallback) then
        FCallback.Log(SysUtils.Format('VM: Interpreted function "%s" not found or not handled by VM.', [AFunctionName]));
    end;
  end;


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
      [GetEnumName(TypeInfo(TTokenType), Ord(ExpectedType)),
       GetEnumName(TypeInfo(TTokenType), Ord(FCurrentToken.TokenType)),
       FCurrentToken.Lexeme,
       FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);
end;

function TParser.PeekToken: TToken;
begin
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
        NumVal := StrToInt(FCurrentToken.Lexeme);
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
        WriteLn(Format('Parser Note: Variable reference "%s" found. (Value lookup not implemented)', [FCurrentToken.Lexeme]));
        ConsumeToken(tkIdentifier);
        Result := CreateBCValueNull;
      end;
    tkParenthesisOpen:
      begin
        ConsumeToken(tkParenthesisOpen);
        Result := ParseExpression; // Handle ( Expression )
        ConsumeToken(tkParenthesisClose);
      end;
    else
      raise Exception.CreateFmt('Parser Error: Unexpected token "%s" when expecting factor at %d:%d',
        [FCurrentToken.Lexeme, FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);
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
      // !!! FIXED: Now using TBCValue.AsString and TBCValue.StringValue directly !!!
      Left.StringValue := Left.AsString + Right.AsString;
      Left.ValueType := bcvtString; // Ensure the result type is string
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
  WriteLn(Format('Parsed PRINT statement with value: %s (Type: %s)',
    [ExprValue.AsString, // Use AsString for display
     GetEnumName(TypeInfo(TBCValueType), Ord(ExprValue.ValueType))
     ]));
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

  WriteLn(Format('Parsed SHOW statement for form: %s', [FormNameToken.Lexeme]));
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

  WriteLn(Format('Parsed LET/Assignment statement: %s = %s (Type: %s)',
    [VarName, ExprValue.AsString, // Use AsString for display
     GetEnumName(TypeInfo(TBCValueType), Ord(ExprValue.ValueType))
     ]));
end;


procedure TParser.ParseIfStatement;
var
  ConditionValue: TBCValue;
  ThenToken: TToken;
begin
  ConsumeToken(tkKeyword); // Consume IF

  ConditionValue := ParseExpression; // Parse the condition (e.g., X > 5)
  // In a real parser, this would generate bytecode for evaluation and a conditional jump

  ThenToken := FCurrentToken;
  if ThenToken.TokenType = tkKeyword then
  begin
    if SameText(ThenToken.Lexeme, 'THEN') then
    begin
      ConsumeToken(tkKeyword); // Consume THEN
      // Now parse the statement(s) that follow THEN
      if (FCurrentToken.TokenType <> tkEndOfLine) and (FCurrentToken.TokenType <> tkEndOfFile) then
      begin
        WriteLn('  (Parsing statement after THEN)');
        ParseStatement; // Recursively parse the statement after THEN
      end;
    end
    else
      raise Exception.CreateFmt('Parser Error: Expected THEN but found "%s" in IF statement at %d:%d',
        [FCurrentToken.Lexeme, FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);
  end
  else
    raise Exception.CreateFmt('Parser Error: Expected THEN keyword in IF statement at %d:%d',
      [FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);

  WriteLn(Format('Parsed IF statement with condition of type: %s', [GetEnumName(TypeInfo(TBCValueType), Ord(ConditionValue.ValueType))]));
end;


procedure TParser.ParseStatement;
var
  CurrentLineNum: Integer;
  PotentialVarName: String;
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
          case LowerCase(FCurrentToken.Lexeme) of
            'print': ParsePrintStatement;
            'show': ParseShowStatement;
            'if': ParseIfStatement;
            'end': // Handle 'END' as program termination
              begin
                ConsumeToken(tkKeyword); // Consume END
                WriteLn('Parsed END statement. Program will terminate.');
                Exit; // Stop parsing
              end;
            else
              raise Exception.CreateFmt('Parser Error: Unrecognized keyword "%s" at %d:%d',
                [FCurrentToken.Lexeme, FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);
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
                [PotentialVarName, FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);
          end
          else
            raise Exception.CreateFmt('Parser Error: Unexpected token "%s" after identifier "%s". Expected "=" or end of statement. at %d:%d',
              [FCurrentToken.Lexeme, PotentialVarName, FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);
        end;
      tkEndOfFile: Exit; // Reached end of file, stop parsing

      else
        raise Exception.CreateFmt('Parser Error: Unexpected token "%s" at start of statement at %d:%d',
          [FCurrentToken.Lexeme, FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);
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
        [FCurrentToken.Lexeme, FCurrentToken.LineNum + 1, FCurrentToken.ColNum]);
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

