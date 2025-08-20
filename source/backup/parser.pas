unit Parser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TokenDefs, Lexer, AST;

type
  TParser = class
  private
    FLexer: TLexer;
    FCurrentToken: TToken;
    FPreviousToken: TToken;

    procedure Advance;
    procedure Match(ExpectedType: TTokenType);
    function Check(TokenType: TTokenType): Boolean;
    function MatchAny(const Types: array of TTokenType): Boolean;

    // Parsing rules (non-terminals)
    procedure LProgram;
    function Statement: TStatementNode;
    procedure OptionStatement;
    procedure DeclarationStatement;
    procedure AssignmentStatement;
    procedure PrintStatement;
    procedure InputStatement;
    procedure MsgBoxStatement;
    procedure CallStatement;
    procedure GoToStatement;
    procedure GoSubStatement;
    procedure ReturnStatement;
    procedure IfStatement;
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

    // Helper functions
    procedure Error(const Message: String);
    procedure NextToken;
    function PeekToken: TToken;

  public
    constructor Create(ALexer: TLexer);
    destructor Destroy; override;
    procedure Parse;
  end;

implementation

{ TParser }

constructor TParser.Create(ALexer: TLexer);
begin
  inherited Create;
  FLexer := ALexer;
  FCurrentToken := FLexer.GetNextToken;
  FPreviousToken := FCurrentToken;
end;

destructor TParser.Destroy;
begin
  inherited Destroy;
end;

procedure TParser.Parse;
begin
  LProgram;
  if FCurrentToken.TokenType <> tkEndOfFile then
    Error('Unexpected token at end of program: ' + FCurrentToken.Lexeme);
end;

procedure TParser.NextToken;
begin
  FCurrentToken := FLexer.GetNextToken;
end;

procedure TParser.Advance;
begin
  FPreviousToken := FCurrentToken;
  FCurrentToken := FLexer.GetNextToken;
end;

function TParser.PeekToken: TToken;
begin
  // This is the correct way to call the function
  // as it is part of the TLexer class.
  Result := FLexer.PeekNextToken;
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
       FLexer.CurrentLine + 1, FLexer.CurrentColumn + 1]);
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

//----------------------------------------------------------------------
// Parsing Rules
//----------------------------------------------------------------------

(*
  Procedure: TParser.LProgram
  Description:
    This is the main entry point for parsing a Kayte program. It first checks
    for and handles any top-level directives like 'option explicit' before
    proceeding to parse the main statements and declarations.

  Changes:
    - Added a check for the 'option' keyword at the very beginning of the
      program, ensuring that the directive is handled correctly and is not
      mistaken for a standard instruction.
*)
procedure TParser.LProgram;
begin
  // First, check for program-level options like 'option explicit'.
  // This must be done before parsing any statements to prevent the
  // directive from being treated as an unknown instruction.
  if Check(tkKeywordOption) then
  begin
    OptionStatement;
  end;

  // Now, parse the rest of the program, which should consist of
  // declarations, subroutines, function definitions, and statements.
  while not Check(tkEndOfFile) do
  begin
    // Call the main statement-parsing procedure.
    Statement;
  end;
end;

function TParser.Statement: TStatementNode;
begin
  // Initialize Result to nil to avoid the warning
  Result := nil;

  // This is a placeholder. You'll need to implement logic to create
  // a specific TStatementNode (e.g., TAssignmentStatementNode) here.
end;

(* Options ON/OFF *)
procedure TParser.OptionStatement;
begin
  Match(tkKeywordOption);

  case CurrentToken.Kind of
    tkKeywordExplicit:
      begin
        Match(tkKeywordExplicit);
        if MatchAny([tkKeywordOn, tkKeywordOff]) then
          ExplicitMode := (LastToken.Kind = tkKeywordOn)
        else
          Error('Expected ON or OFF after OPTION EXPLICIT');
      end;

    tkKeywordStrict:
      begin
        Match(tkKeywordStrict);
        if MatchAny([tkKeywordOn, tkKeywordOff]) then
          StrictMode := (LastToken.Kind = tkKeywordOn)
        else
          Error('Expected ON or OFF after OPTION STRICT');
      end;

    tkKeywordCompare:
      begin
        Match(tkKeywordCompare);
        if MatchAny([tkKeywordText, tkKeywordBinary]) then
          CompareMode := LastToken.Kind
        else
          Error('Expected TEXT or BINARY after OPTION COMPARE');
      end;

  else
    Error('Unknown OPTION directive');
  end;
end;

procedure TParser.DeclarationStatement;
begin
  MatchAny([tkKeyword]);
  Match(tkIdentifier);
  if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'AS') then
  begin
    Advance;
    Match(tkIdentifier);
  end;
end;

procedure TParser.AssignmentStatement;
begin
  Match(tkIdentifier);
  Match(tkOperator);
  Expression;
end;

procedure TParser.PrintStatement;
begin
  Match(tkKeyword);
  Expression;
  while MatchAny([tkComma, tkColon]) do
    Expression;
end;

procedure TParser.InputStatement;
begin
  Match(tkKeyword);
  if Check(tkStringLiteral) then
    Advance;
  Match(tkIdentifier);
end;

procedure TParser.MsgBoxStatement;
begin
  Match(tkKeyword);
  Expression;
  while Check(tkComma) do
  begin
    Advance;
    Expression;
  end;
end;

procedure TParser.CallStatement;
begin
  Match(tkKeyword);
  Match(tkIdentifier);
  if Check(tkParenthesisOpen) then
  begin
    Advance;
    if not Check(tkParenthesisClose) then
    begin
      Expression;
      while Check(tkComma) do
      begin
        Advance;
        Expression;
      end;
    end;
    Match(tkParenthesisClose);
  end;
end;

procedure TParser.GoToStatement;
begin
  Match(tkKeyword);
  Match(tkIdentifier);
end;

procedure TParser.GoSubStatement;
begin
  Match(tkKeyword);
  Match(tkIdentifier);
end;

procedure TParser.ReturnStatement;
begin
  Match(tkKeyword);
end;

procedure TParser.IfStatement;
begin
  Match(tkKeyword);
  Expression;
  Match(tkKeyword);
end;

procedure TParser.WhileStatement;
begin
  Match(tkKeyword);
  Expression;
  Match(tkEndOfLine);
  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'WEND')) do
  begin
    Statement;
    while Check(tkEndOfLine) do Advance;
  end;
  Match(tkKeyword);
end;

procedure TParser.ForStatement;
begin
  Match(tkKeyword);
  Match(tkIdentifier);
  Match(tkOperator);
  Expression;
  Match(tkKeyword);
  Expression;
  if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'STEP') then
  begin
    Advance;
    Expression;
  end;
  Match(tkEndOfLine);
  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'NEXT')) do
  begin
    Statement;
    while Check(tkEndOfLine) do Advance;
  end;
  Match(tkKeyword);
  if Check(tkIdentifier) then
    Advance;
end;

procedure TParser.SubDefinition;
begin
  Match(tkKeyword);
  Match(tkIdentifier);
  Match(tkParenthesisOpen);
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
  Match(tkEndOfLine);
  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'END')) do
  begin
    Statement;
    while Check(tkEndOfLine) do Advance;
  end;
  Match(tkKeyword);
  Match(tkKeyword);
end;

procedure TParser.FunctionDefinition;
begin
  Match(tkKeyword);
  Match(tkIdentifier);
  Match(tkParenthesisOpen);
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
    Advance;
    Match(tkIdentifier);
  end;
  Match(tkEndOfLine);
  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'END')) do
  begin
    Statement;
    while Check(tkEndOfLine) do Advance;
  end;
  Match(tkKeyword);
  Match(tkKeyword);
end;

procedure TParser.FormDefinition;
begin
  Match(tkKeyword);
  Match(tkIdentifier);
  Match(tkEndOfLine);
  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'END')) do
  begin
    Statement;
    while Check(tkEndOfLine) do Advance;
  end;
  Match(tkKeyword);
  Match(tkKeyword);
end;

procedure TParser.ShowStatement;
begin
  Match(tkKeyword);
  Match(tkIdentifier);
end;

procedure TParser.HideStatement;
begin
  Match(tkKeyword);
  Match(tkIdentifier);
end;

//----------------------------------------------------------------------
// Expression Parsing Methods (Corrected)
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
  while (FCurrentToken.TokenType = tkOperator) and ((FCurrentToken.Lexeme = '=') or (FCurrentToken.Lexeme = '<>')) do
  begin
    OperatorToken := FCurrentToken;
    Advance;
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
  while (FCurrentToken.TokenType = tkOperator) and ((FCurrentToken.Lexeme = '>') or (FCurrentToken.Lexeme = '<') or (FCurrentToken.Lexeme = '>=') or (FCurrentToken.Lexeme = '<=') or (FCurrentToken.Lexeme.ToUpper = 'IS')) do
  begin
    OperatorToken := FCurrentToken;
    Advance;
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
  while (FCurrentToken.TokenType = tkOperator) and ((FCurrentToken.Lexeme = '+') or (FCurrentToken.Lexeme = '-') or (FCurrentToken.Lexeme = '&')) do
  begin
    OperatorToken := FCurrentToken;
    Advance;
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
  while (FCurrentToken.TokenType = tkOperator) and ((FCurrentToken.Lexeme = '*') or (FCurrentToken.Lexeme = '/')) do
  begin
    OperatorToken := FCurrentToken;
    Advance;
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
  // Initialize the result variable at the start of the function
  Result := nil;

  if (FCurrentToken.TokenType = tkOperator) and ((FCurrentToken.Lexeme = '-') or (FCurrentToken.Lexeme.ToUpper = 'NOT')) then
  begin
    OperatorToken := FCurrentToken;
    Advance;
    RightHandSide := Unary;
    Result := TUnaryOpNode.Create(OperatorToken.Lexeme, RightHandSide);
  end
  else
  begin
    Result := Primary;
  end;
end;

function TParser.Primary: TExpressionNode;
var
  Token: TToken;
begin
  Token := FCurrentToken;
  case Token.TokenType of
    tkIntegerLiteral, tkStringLiteral, tkBooleanLiteral, tkIdentifier:
      begin
        Advance;
        Result := TLiteralNode.Create(Token.Lexeme, Token.TokenType);
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

