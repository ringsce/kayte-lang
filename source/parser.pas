unit Parser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TokenDefs, Lexer, AST; // Include Lexer unit

type
  TParser = class
  private
    FLexer: TLexer;
    FCurrentToken: TToken;
    FPreviousToken: TToken; // Added to store the previously consumed token

    procedure Advance;
    procedure Match(ExpectedType: TTokenType);
    function Check(TokenType: TTokenType): Boolean;
    function MatchAny(const Types: array of TTokenType): Boolean;

    // Parsing rules (non-terminals)
    procedure LProgram; // Renamed from Program to avoid keyword conflict
    procedure Statement;
    procedure OptionStatement; // Added for Option Explicit
    procedure DeclarationStatement;
    procedure AssignmentStatement;
    procedure PrintStatement;
    procedure InputStatement;
    procedure MsgBoxStatement;
    procedure CallStatement;
    procedure GoToStatement;
    procedure GoSubStatement;
    procedure ReturnStatement;
    procedure WhileStatement;
    procedure ForStatement;
    procedure SubDefinition;
    procedure FunctionDefinition;
    procedure FormDefinition;
    procedure ShowStatement;
    procedure HideStatement;

     // Recursive-descent expression parsing methods
    function Expression: TExpressionNode;
    function Equality: TExpressionNode;
    function Comparison: TExpressionNode;
    function Term: TExpressionNode;
    function Factor: TExpressionNode;
    function Unary: TExpressionNode;
    function Primary: TExpressionNode;

    function Statement: TStatementNode;
    function ParseBlock: TStatementNodeList;// Helper for error reporting
    procedure Error(const Message: String);

    procedure NextToken;
    function Match(ExpectedType: TTokenType): TToken;
    function ParseExpression: TExpressionNode;


  public
    constructor Create(ALexer: TLexer);
    destructor Destroy; override;
    procedure Parse;
    procedure IfStatement;

  end;

implementation

{ TParser }

constructor TParser.Create(ALexer: TLexer);
begin
  inherited Create;
  FLexer := ALexer; // Parser does not own the lexer
  // Initialize FCurrentToken and FPreviousToken
  // Get the first token to start parsing
  FCurrentToken := FLexer.GetNextToken;
  FPreviousToken := FCurrentToken; // Initialize previous token
end;

destructor TParser.Destroy;
begin
  inherited Destroy;
end;

procedure TParser.Parse;
begin
  LProgram; // Call the renamed procedure
  if FCurrentToken.TokenType <> tkEndOfFile then
    Error('Unexpected token at end of program: ' + FCurrentToken.Lexeme);
end;

procedure TParser.Advance;
begin
  FPreviousToken := FCurrentToken; // Save the current token as previous
  FCurrentToken := FLexer.GetNextToken; // Get the next token
end;

procedure TParser.Match(ExpectedType: TTokenType);
begin
  if FCurrentToken.TokenType = ExpectedType then
    Advance
  else
    raise Exception.CreateFmt('Parser Error: Expected %s but found %s ("%s") at %d:%d',
      [GetTokenTypeName(ExpectedType),
       GetTokenTypeName(FCurrentToken.TokenType),
       FCurrentToken.Lexeme,
       FLexer.CurrentLine + 1, FLexer.CurrentColumn + 1]); // +1 for 1-based indexing
end;

function TParser.Check(TokenType: TTokenType): Boolean;
begin
  Result := FCurrentToken.TokenType = TokenType;
end;

function TParser.MatchAny(const Types: array of TTokenType): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(Types) to High(Types) do
  begin
    if FCurrentToken.TokenType = Types[i] then
    begin
      Result := True;
      Break;
    end;
  end;
  if Result then Advance;
end;

procedure TParser.Error(const Message: String);
begin
  raise Exception.CreateFmt('Parser Error: %s at %d:%d (Token: "%s" Type: %s)',
    [Message, FCurrentToken.Line + 1, FCurrentToken.Column + 1, FCurrentToken.Lexeme, GetTokenTypeName(FCurrentToken.TokenType)]);
end;

{ Parsing Rules }

procedure TParser.LProgram; // Renamed procedure
begin
  while FCurrentToken.TokenType <> tkEndOfFile do
  begin
    Statement;
    // Consume EndOfLine tokens between statements, but allow EOF immediately after a statement
    while Check(tkEndOfLine) do
      Advance;
  end;
end;

procedure TParser.Statement;
begin
  case FCurrentToken.TokenType of
    tkKeywordOption: OptionStatement; // Handle Option Explicit
    tkKeyword:
      case AnsiUpperCase(FCurrentToken.Lexeme) of
        'REM': Match(tkComment); // REM comments are handled by lexer, but parser consumes if it sees it
        'DIM', 'REDIM': DeclarationStatement;
        'PRINT': PrintStatement;
        'INPUT': InputStatement;
        'MSGBOX': MsgBoxStatement;
        'CALL': CallStatement;
        'GOTO': GoToStatement;
        'GOSUB': GoSubStatement;
        'RETURN': ReturnStatement;
        'IF': IfStatement;
        'WHILE': WhileStatement;
        'FOR': ForStatement;
        'SUB': SubDefinition;
        'FUNCTION': FunctionDefinition;
        'FORM': FormDefinition;
        'SHOW': ShowStatement;
        'HIDE': HideStatement;
        'END': // 'END' can be part of 'END SUB', 'END FUNCTION', 'END IF', 'END SELECT', 'END FORM'
          begin
            Advance; // Consume 'END'
            if MatchAny([tkKeyword, tkIdentifier]) then // Check for SUB, FUNCTION, IF, SELECT, FORM
            begin
              case AnsiUpperCase(FPreviousToken.Lexeme + ' ' + FCurrentToken.Lexeme) of
                'END SUB', 'END FUNCTION', 'END IF', 'END SELECT', 'END FORM':
                  Advance; // Consume the second part (SUB, FUNCTION, IF, SELECT, FORM)
                else
                  // It was just 'END' or an invalid combination, treat as a simple END keyword
                  // For now, we'll just consume the second part if it was a keyword/identifier
                  // A more robust parser would check for valid combinations.
              end;
            end;
          end;
        else
          // If it's a keyword not handled above, it might be an assignment starting with an identifier
          // or an error. For simplicity, assume it might be an assignment if it's not a known statement start.
          // This might need refinement for a full VB6 grammar.
          if Check(tkIdentifier) then
            AssignmentStatement // Try to parse as assignment if it's an identifier
          else
            Error('Unexpected keyword: ' + FCurrentToken.Lexeme);
      end;
    tkIdentifier: AssignmentStatement; // Must be an assignment
    tkEndOfLine: Advance; // Allow empty lines
    tkComment: Advance; // Allow comments as statements
    tkEndOfFile: Exit; // Reached end of file, stop parsing statements
    else
      Error('Unexpected token at start of statement: ' + FCurrentToken.Lexeme);
  end;
end;

procedure TParser.OptionStatement;
begin
  Match(tkKeywordOption);
  Match(tkKeywordExplicit);
  if MatchAny([tkKeywordOn, tkKeywordOff]) then
  begin
    // Option Explicit On/Off consumed
  end
  else
    Error('Expected ON or OFF after OPTION EXPLICIT');
end;

procedure TParser.DeclarationStatement;
begin
  // DIM or REDIM
  MatchAny([tkKeyword]); // Consumes DIM or REDIM
  Match(tkIdentifier); // Variable name
  if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'AS') then
  begin
    Advance; // Consume AS
    Match(tkIdentifier); // Type name (e.g., Integer, String)
  end;
end;

procedure TParser.AssignmentStatement;
begin
  Match(tkIdentifier); // Variable name
  Match(tkOperator); // Expect '='
  Expression; // The value being assigned
end;

procedure TParser.PrintStatement;
begin
  Match(tkKeyword); // Consumes PRINT
  Expression; // What to print
  // Optional: multiple expressions separated by commas or semicolons
  while MatchAny([tkComma, tkColon]) do // Using tkColon for simplicity, could be tkSemicolon if defined
    Expression;
end;

procedure TParser.InputStatement;
begin
  Match(tkKeyword); // Consumes INPUT
  if Check(tkStringLiteral) then
    Advance; // Optional prompt string
  Match(tkIdentifier); // Variable to store input
end;

procedure TParser.MsgBoxStatement;
begin
  Match(tkKeyword); // Consumes MSGBOX
  Expression; // Message to display
  // Optional arguments for MsgBox (buttons, title, etc.)
  while Check(tkComma) do
  begin
    Advance;
    Expression;
  end;
end;

procedure TParser.CallStatement;
begin
  Match(tkKeyword); // Consumes CALL
  Match(tkIdentifier); // Sub/Function name
  if Check(tkParenthesisOpen) then
  begin
    Advance; // Consume '('
    if not Check(tkParenthesisClose) then
    begin
      Expression; // First argument
      while Check(tkComma) do
      begin
        Advance;
        Expression; // Subsequent arguments
      end;
    end;
    Match(tkParenthesisClose); // Consume ')'
  end;
end;

procedure TParser.GoToStatement;
begin
  Match(tkKeyword); // Consumes GOTO
  Match(tkIdentifier); // Label name
end;

procedure TParser.GoSubStatement;
begin
  Match(tkKeyword); // Consumes GOSUB
  Match(tkIdentifier); // Label name
end;

procedure TParser.ReturnStatement;
begin
  Match(tkKeyword); // Consumes RETURN
end;


{ TParser }

procedure TParser.NextToken;
begin
  FCurrentToken := FLexer.GetNextToken;
end;

function TParser.PeekToken: TToken;
begin
  Result := FLexer.PeekNextToken;
end;

function TParser.Match(ExpectedType: TTokenType): TToken;
begin
  if FCurrentToken.Type_ = ExpectedType then
  begin
    Result := FCurrentToken;
    NextToken;
  end
  else
  begin
    raise Exception.Create('Syntax Error: Expected ' + TTokenType.ToString(ExpectedType));
  end;
end;

//----------------------------------------------------------------------
// Expression Parsing Methods
//----------------------------------------------------------------------

function TParser.Expression: TExpressionNode;
begin
  Result := Equality;
end;

function TParser.Equality: TExpressionNode;
var
  Node: TExpressionNode;
  OperatorToken: TToken;
  RightHandSide: TExpressionNode;
begin
  Node := Comparison;
  while (FCurrentToken.Lexeme = '=') or (FCurrentToken.Lexeme = '<>') do
  begin
    OperatorToken := FCurrentToken;
    NextToken;
    RightHandSide := Comparison;
    Node := TBinaryOpNode.Create(OperatorToken.Lexeme, Node, RightHandSide);
  end;
  Result := Node;
end;

function TParser.Comparison: TExpressionNode;
var
  Node: TExpressionNode;
  OperatorToken: TToken;
  RightHandSide: TExpressionNode;
begin
  Node := Term;
  while (FCurrentToken.Lexeme = '>') or (FCurrentToken.Lexeme = '<') do
  begin
    OperatorToken := FCurrentToken;
    NextToken;
    RightHandSide := Term;
    Node := TBinaryOpNode.Create(OperatorToken.Lexeme, Node, RightHandSide);
  end;
  Result := Node;
end;

function TParser.Term: TExpressionNode;
var
  Node: TExpressionNode;
  OperatorToken: TToken;
  RightHandSide: TExpressionNode;
begin
  Node := Factor;
  while (FCurrentToken.Lexeme = '+') or (FCurrentToken.Lexeme = '-') do
  begin
    OperatorToken := FCurrentToken;
    NextToken;
    RightHandSide := Factor;
    Node := TBinaryOpNode.Create(OperatorToken.Lexeme, Node, RightHandSide);
  end;
  Result := Node;
end;

function TParser.Factor: TExpressionNode;
var
  Node: TExpressionNode;
  OperatorToken: TToken;
  RightHandSide: TExpressionNode;
begin
  Node := Unary;
  while (FCurrentToken.Lexeme = '*') or (FCurrentToken.Lexeme = '/') do
  begin
    OperatorToken := FCurrentToken;
    NextToken;
    RightHandSide := Unary;
    Node := TBinaryOpNode.Create(OperatorToken.Lexeme, Node, RightHandSide);
  end;
  Result := Node;
end;

function TParser.Unary: TExpressionNode;
var
  OperatorToken: TToken;
  RightHandSide: TExpressionNode;
begin
  if (FCurrentToken.Lexeme = '-') or (FCurrentToken.Lexeme.ToUpper = 'NOT') then
  begin
    OperatorToken := FCurrentToken;
    NextToken;
    RightHandSide := Unary;
    Node := TUnaryOpNode.Create(OperatorToken.Lexeme, RightHandSide); // Assumes TUnaryOpNode exists
  end
  else
  begin
    Result := Primary;
  end;
end;

function TParser.Primary: TExpressionNode;
begin
  // Placeholder logic for primary expressions
  // This would handle numbers, strings, identifiers, etc.
  // For now, we'll just create a dummy node.
  Result := nil;
  NextToken;
end;

//----------------------------------------------------------------------
// Statement Parsing Methods
//----------------------------------------------------------------------
function TParser.Statement: TStatementNode;
begin
  Result := nil;
end;

function TParser.ParseBlock: TStatementNodeList;
begin
  Result := TStatementNodeList.Create;
end;

procedure TParser.IfStatement;
var
  IfCondition: TExpressionNode;
  IfBlock: TStatementNodeList;
  ElseIfCondition: TExpressionNode;
  ElseIfBlock: TStatementNodeList;
  ElseBlock: TStatementNodeList;
begin
  // A. Parse the initial IF...THEN block
  Match(tkKeyword); // Consumes 'IF'
  IfCondition := Expression; // Parse the boolean condition
  Match(tkKeyword); // Consumes 'THEN'

  // (Remaining implementation as provided in your prompt)
end;

procedure TParser.WhileStatement;
begin
  Match(tkKeyword); // Consumes WHILE
  Expression; // Condition
  Match(tkEndOfLine); // Must be a block WHILE
  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'WEND')) do
  begin
    Statement;
    while Check(tkEndOfLine) do Advance;
  end;
  Match(tkKeyword); // Consumes WEND
end;

procedure TParser.ForStatement;
begin
  Match(tkKeyword); // Consumes FOR
  Match(tkIdentifier); // Loop variable
  Match(tkOperator); // Expect '='
  Expression; // Start value
  Match(tkKeyword); // Consumes TO
  Expression; // End value
  if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'STEP') then
  begin
    Advance; // Consume STEP
    Expression; // Step value
  end;
  Match(tkEndOfLine); // Must be a block FOR
  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'NEXT')) do
  begin
    Statement;
    while Check(tkEndOfLine) do Advance;
  end;
  Match(tkKeyword); // Consumes NEXT
  if Check(tkIdentifier) then
    Advance; // Optional: consume loop variable name after NEXT
end;

procedure TParser.SubDefinition;
begin
  Match(tkKeyword); // Consumes SUB
  Match(tkIdentifier); // Sub name
  Match(tkParenthesisOpen);
  // Parameters (simplified: just identifiers for now)
  if Check(tkIdentifier) then
  begin
    Advance;
    while Check(tkComma) do
    begin
      Advance;
      Match(tkIdentifier);
    end;
  end;
  Match(tkParenthesisClose);
  Match(tkEndOfLine); // End of SUB signature
  // Body of the sub
  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'END') and (AnsiUpperCase(FLexer.GetNextToken.Lexeme) = 'SUB')) do
  begin
    Statement;
    while Check(tkEndOfLine) do Advance;
  end;
  Match(tkKeyword); // Consume END
  Match(tkKeyword); // Consume SUB
end;

procedure TParser.FunctionDefinition;
begin
  Match(tkKeyword); // Consumes FUNCTION
  Match(tkIdentifier); // Function name
  Match(tkParenthesisOpen);
  // Parameters (simplified: just identifiers for now)
  if Check(tkIdentifier) then
  begin
    Advance;
    while Check(tkComma) do
    begin
      Advance;
      Match(tkIdentifier);
    end;
  end;
  Match(tkParenthesisClose);
  if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'AS') then
  begin
    Advance; // Consume AS
    Match(tkIdentifier); // Type name
  end;
  Match(tkEndOfLine); // End of FUNCTION signature
  // Body of the function
  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'END') and (AnsiUpperCase(FLexer.GetNextToken.Lexeme) = 'FUNCTION')) do
  begin
    Statement;
    while Check(tkEndOfLine) do Advance;
  end;
  Match(tkKeyword); // Consume END
  Match(tkKeyword); // Consume FUNCTION
end;

procedure TParser.FormDefinition;
begin
  Match(tkKeyword); // Consumes FORM
  Match(tkIdentifier); // Form name
  Match(tkEndOfLine);
  // Form elements (simplified: any statements within a form block)
  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'END') and (AnsiUpperCase(FLexer.GetNextToken.Lexeme) = 'FORM')) do
  begin
    Statement;
    while Check(tkEndOfLine) do Advance;
  end;
  Match(tkKeyword); // Consume END
  Match(tkKeyword); // Consume FORM
end;

procedure TParser.ShowStatement;
begin
  Match(tkKeyword); // Consumes SHOW
  Match(tkIdentifier); // Form or control name
end;

procedure TParser.HideStatement;
begin
  Match(tkKeyword); // Consumes HIDE
  Match(tkIdentifier); // Form or control name
end;

function TParser.Expression: TToken;
begin
  Result := Comparison;
end;

function TParser.Comparison: TToken;
var
  Left: TToken;
begin
  Left := Term;
  while Check(tkOperator) do // First, check if the current token is an operator
  begin
    // Now, check if this operator is one of the comparison operators
    case AnsiUpperCase(FCurrentToken.Lexeme) of
      '=', '<', '>', '<=', '>=', '<>', 'IS':
        begin
          Advance; // Consume the operator (it will now be in FPreviousToken)
          // Operator := FPreviousToken; // If you need to store it, it's in FPreviousToken
          Term; // Parse the right operand
          // In a real parser, you'd build an AST node here using Left, Operator, and the result of Term
        end;
      else
        Break; // Not a comparison operator, exit the loop
    end;
  end;
  Result := Left; // Return the left side of the comparison
end;

function TParser.Term: TToken;
var
  Left: TToken;
begin
  Left := Factor;
  while Check(tkOperator) do // First, check if the current token is an operator
  begin
    // Now, check if this operator is one of the addition/subtraction/concatenation operators
    case AnsiUpperCase(FCurrentToken.Lexeme) of
      '+', '-', '&': // & for string concatenation
        begin
          Advance; // Consume the operator
          // Operator := FPreviousToken;
          Factor; // Parse the right operand
          // Build AST node
        end;
      else
        Break; // Not an addition/subtraction/concatenation operator, exit the loop
    end;
  end;
  Result := Left;
end;

function TParser.Factor: TToken;
var
  Left: TToken;
begin
  Left := Primary;
  while Check(tkOperator) do // First, check if the current token is an operator
  begin
    // Now, check if this operator is one of the multiplication/division operators
    case AnsiUpperCase(FCurrentToken.Lexeme) of
      '*', '/':
        begin
          Advance; // Consume the operator
          // Operator := FPreviousToken;
          Primary; // Parse the right operand
          // Build AST node
        end;
      else
        Break; // Not a multiplication/division operator, exit the loop
    end;
  end;
  Result := Left;
end;

function TParser.Primary: TToken;
begin
  case FCurrentToken.TokenType of
    tkIntegerLiteral, tkStringLiteral, tkBooleanLiteral, tkIdentifier:
      begin
        Result := FCurrentToken;
        Advance;
      end;
    tkParenthesisOpen:
      begin
        Advance; // Consume '('
        Result := Expression;
        Match(tkParenthesisClose); // Consume ')'
      end;
    else
      Error('Expected expression, found ' + FCurrentToken.Lexeme);
  end;
end;

end.

