unit Parser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TokenDefs, Lexer, AST, BytecodeTypes, Assembler;

type
  TParser = class
  private
    FLexer: TLexer;
    FCurrentToken: TToken;
    FPreviousToken: TToken;
    FAssembler: TAssembler;

    procedure Advance;
    procedure Match(ExpectedType: TTokenType);
    function Check(TokenType: TTokenType): Boolean;
    function MatchAny(const Types: array of TTokenType): Boolean;

    // Parsing rules (non-terminals)
    function Statement: TStatementNode;
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
    procedure WithStatement;  // NEW: Handle WITH blocks for object member access
    procedure SubDefinition;
    procedure FunctionDefinition;
    procedure ClassDefinition;  // NEW: Handle CLASS definitions
    procedure FormDefinition;
    procedure ShowStatement;
    procedure HideStatement;
    procedure OptionStatement;  // NEW: Handle OPTION directives

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

    // Bytecode helper functions
    function Op_Variable(const VarName: string): Integer;
    function Op_StringLiteral(const Literal: string): Integer;

  public
    constructor Create(ALexer: TLexer);
    destructor Destroy; override;
    function Parse: TByteCodeProgram;
  end;

implementation

{ TParser }

constructor TParser.Create(ALexer: TLexer);
begin
  inherited Create;

  if ALexer = nil then
    raise Exception.Create('Lexer cannot be nil');

  FLexer := ALexer;

  try
    FCurrentToken := FLexer.GetNextToken;
    WriteLn('DEBUG: First token type: ', Ord(FCurrentToken.TokenType));
    WriteLn('DEBUG: First token lexeme: ', FCurrentToken.Lexeme);
  except
    on E: Exception do
    begin
      WriteLn('ERROR in parser constructor: ', E.Message);
      raise;
    end;
  end;

  FPreviousToken := FCurrentToken;
  FAssembler := TAssembler.Create;
end;

destructor TParser.Destroy;
begin
  FAssembler.Free;
  inherited Destroy;
end;

function TParser.Parse: TByteCodeProgram;
var
  IterationCount: Integer;
begin
  IterationCount := 0;

  // Set the program title
  FAssembler.SetProgramTitle('Kayte Program');

  // Skip comments at the beginning of the file
  while FCurrentToken.TokenType = tkComment do
    Advance;

  WriteLn('DEBUG: Starting main parse loop');
  WriteLn('DEBUG: First token - Type=', Ord(FCurrentToken.TokenType),
          ' Lexeme="', FCurrentToken.Lexeme, '"');

  // The main parsing loop
  while FCurrentToken.TokenType <> tkEndOfFile do
  begin
    Inc(IterationCount);
    WriteLn('DEBUG: === Iteration ', IterationCount, ' ===');
    WriteLn('DEBUG: Current token - Type=', Ord(FCurrentToken.TokenType),
            ' Lexeme="', FCurrentToken.Lexeme, '"');

    // Safety check to prevent infinite loops during debugging
    if IterationCount > 100 then
    begin
      WriteLn('ERROR: Too many iterations, possible infinite loop!');
      Break;
    end;

    try
      // Skip blank lines and comments
      while (FCurrentToken.TokenType = tkEndOfLine) or
            (FCurrentToken.TokenType = tkComment) do
      begin
        WriteLn('DEBUG: Skipping token type ', Ord(FCurrentToken.TokenType));
        Advance;
        WriteLn('DEBUG: After skip - Type=', Ord(FCurrentToken.TokenType),
                ' Lexeme="', FCurrentToken.Lexeme, '"');
      end;

      if FCurrentToken.TokenType = tkEndOfFile then
      begin
        WriteLn('DEBUG: EOF detected, breaking');
        Break;
      end;

      // Handle special token types first
      case FCurrentToken.TokenType of
        tkOptionExplicitOn, tkOptionExplicitOff:
          begin
            WriteLn('DEBUG: Handling Option Explicit token');
            Advance;
            WriteLn('DEBUG: After Advance - Type=', Ord(FCurrentToken.TokenType),
                    ' Lexeme="', FCurrentToken.Lexeme, '"');
            Continue;
          end;
        tkComment:
          begin
            WriteLn('DEBUG: Handling Comment in case statement');
            Advance;
            Continue;
          end;
      end;

      // Then handle keyword-based statements
      WriteLn('DEBUG: Processing keyword: "', AnsiUpperCase(FCurrentToken.Lexeme), '"');

      case AnsiUpperCase(FCurrentToken.Lexeme) of
        'END':
          begin
            WriteLn('DEBUG: END statement');
            Advance;
          end;
        'OPTION':
          OptionStatement;
        'PRINT':
          PrintStatement;
        'INPUT':
          InputStatement;
        'MSGBOX':
          MsgBoxStatement;
        'LET':
          AssignmentStatement;
        'SET':
          begin
            WriteLn('DEBUG: SET statement for object assignment');
            Advance; // Consume SET
            AssignmentStatement;
          end;
        'GOTO':
          GoToStatement;
        'GOSUB':
          GoSubStatement;
        'RETURN':
          ReturnStatement;
        'IF':
          IfStatement;
        'WHILE':
          WhileStatement;
        'FOR':
          ForStatement;
        'WITH':
          WithStatement;
        'SUB':
          SubDefinition;
        'FUNCTION':
          FunctionDefinition;
        'CLASS':
          ClassDefinition;
        'FORM':
          FormDefinition;
        'SHOW':
          ShowStatement;
        'HIDE':
          HideStatement;
        'DIM', 'PUBLIC', 'PRIVATE':
          DeclarationStatement;
        'CALL':
          CallStatement;
      else
        WriteLn('DEBUG: In else clause');
        if FCurrentToken.TokenType = tkIdentifier then
        begin
          WriteLn('DEBUG: Identifier token');
          // Could be a label or an assignment without LET
          if PeekToken.TokenType = tkOperator then
          begin
            WriteLn('DEBUG: Assignment without LET');
            AssignmentStatement;
          end
          else
          begin
            WriteLn('DEBUG: Label definition: ', FCurrentToken.Lexeme);
            FAssembler.DefineLabel(FCurrentToken.Lexeme);
            Advance;
          end;
        end
        else if FCurrentToken.TokenType = tkEndOfLine then
        begin
          WriteLn('DEBUG: Extra EndOfLine');
          Advance;
        end
        else
        begin
          WriteLn('DEBUG: ERROR - Unexpected token');
          Error('Unexpected token: ' + FCurrentToken.Lexeme);
        end;
      end;

      // Skip any end-of-line markers after the statement
      WriteLn('DEBUG: Checking for trailing EndOfLine');
      while Check(tkEndOfLine) do
      begin
        WriteLn('DEBUG: Skipping trailing EndOfLine');
        Advance;
      end;

      WriteLn('DEBUG: End of iteration');

    except
      on E: Exception do
      begin
        Writeln('Parser Error: ', E.Message);
        // Skip to next line on error
        while (FCurrentToken.TokenType <> tkEndOfLine) and
              (FCurrentToken.TokenType <> tkEndOfFile) do
          Advance;
        if FCurrentToken.TokenType = tkEndOfLine then
          Advance;
      end;
    end;
  end;

  WriteLn('DEBUG: Exited main loop after ', IterationCount, ' iterations');

  // Finalize the program and return the bytecode
  Result := FAssembler.GetProgram;
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
       FCurrentToken.Line, FCurrentToken.Column]);
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
    [Message, FCurrentToken.Line, FCurrentToken.Column, FCurrentToken.Lexeme, GetTokenTypeName(FCurrentToken.TokenType)]);
end;

// Helper functions for bytecode operand encoding
function TParser.Op_Variable(const VarName: string): Integer;
begin
  // Add or get variable from the program's variable table
  Result := FAssembler.GetProgram.AddVariable(VarName);
end;

function TParser.Op_StringLiteral(const Literal: string): Integer;
begin
  // Add string literal to the program's constant pool
  Result := FAssembler.GetProgram.AddStringConstant(Literal);
end;

//----------------------------------------------------------------------
// Parsing Rules
//----------------------------------------------------------------------

function TParser.Statement: TStatementNode;
begin
  Result := nil;
end;

procedure TParser.OptionStatement;
begin
  // Match OPTION keyword
  Advance;

  // Consume the rest of the OPTION statement (e.g., "Explicit On", "Base 0", etc.)
  // These are compile-time directives and don't generate bytecode
  while not Check(tkEndOfLine) and not Check(tkEndOfFile) do
    Advance;

  // OPTION statements just set compiler flags, no bytecode needed
end;

procedure TParser.IfStatement;
begin
  WriteLn('DEBUG: IfStatement - Current token: ', FCurrentToken.Lexeme);

  Match(tkKeyword); // Consume IF
  WriteLn('DEBUG: IfStatement - Parsing condition');
  Expression; // Parse the condition

  // Expect THEN
  if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'THEN') then
  begin
    WriteLn('DEBUG: IfStatement - Found THEN');
    Advance; // Consume THEN
  end
  else
    Error('Expected THEN after IF condition');

  // For single-line IF: IF condition THEN statement
  // For now, just skip to end of line
  // TODO: Handle ELSE and multi-line IF...END IF blocks

  WriteLn('DEBUG: IfStatement - Complete');
end;

procedure TParser.DeclarationStatement;
begin
  WriteLn('DEBUG: DeclarationStatement - Current token: ', FCurrentToken.Lexeme);

  // Consume DIM/PUBLIC/PRIVATE keyword
  Advance;

  // Handle variable list: DIM a, b, c AS INTEGER
  repeat
    if not Check(tkIdentifier) then
      Error('Expected variable name in declaration');

    WriteLn('DEBUG: Declaring variable: ', FCurrentToken.Lexeme);

    // Add variable to the program
    Op_Variable(FCurrentToken.Lexeme);

    Advance; // Consume variable name

    // Check for comma (more variables)
    if Check(tkComma) then
    begin
      WriteLn('DEBUG: Found comma, expecting another variable');
      Advance; // Consume comma
      Continue; // Get next variable
    end
    else
      Break; // No more variables

  until False;

  // Handle optional AS type
  if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'AS') then
  begin
    Advance; // Consume AS
    if not Check(tkIdentifier) then
      Error('Expected type name after AS');
    WriteLn('DEBUG: Type: ', FCurrentToken.Lexeme);
    Advance; // Consume type name
  end;

  WriteLn('DEBUG: DeclarationStatement complete');
end;

procedure TParser.AssignmentStatement;
var
  VarName: string;
begin
  VarName := FCurrentToken.Lexeme;
  Advance; // Consume identifier
  if not Check(tkOperator) then
    Error('Expected assignment operator');
  Advance; // Consume operator
  // Emit bytecode for expression evaluation
  Expression;
  // Emit bytecode for assignment
  FAssembler.Emit(BC_ASSIGN, [Op_Variable(VarName)]);
end;

procedure TParser.PrintStatement;
begin
  Advance; // Consume PRINT keyword

  // Handle empty PRINT (just prints a newline)
  if Check(tkEndOfLine) or Check(tkEndOfFile) then
  begin
    FAssembler.Emit(BC_PRINT, []);
    Exit;
  end;

  // Handle first expression
  if Check(tkStringLiteral) then
  begin
    FAssembler.Emit(BC_LOAD_STRING, [Op_StringLiteral(FCurrentToken.Lexeme)]);
    Advance;
  end
  else if Check(tkIdentifier) then
  begin
    FAssembler.Emit(BC_LOAD_VAR, [Op_Variable(FCurrentToken.Lexeme)]);
    Advance;
  end
  else
  begin
    Expression;
  end;

  // Handle additional arguments separated by commas
  while Check(tkComma) do
  begin
    Advance; // Consume comma

    if Check(tkStringLiteral) then
    begin
      FAssembler.Emit(BC_LOAD_STRING, [Op_StringLiteral(FCurrentToken.Lexeme)]);
      Advance;
    end
    else if Check(tkIdentifier) then
    begin
      FAssembler.Emit(BC_LOAD_VAR, [Op_Variable(FCurrentToken.Lexeme)]);
      Advance;
    end
    else
    begin
      Expression;
    end;
  end;

  FAssembler.Emit(BC_PRINT, []);
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

procedure TParser.WhileStatement;
begin
  WriteLn('DEBUG: WhileStatement - Current token: ', FCurrentToken.Lexeme);

  Match(tkKeyword); // Consume WHILE
  Expression;
  Match(tkEndOfLine);

  WriteLn('DEBUG: WhileStatement - Entering body loop');

  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'WEND')) do
  begin
    WriteLn('DEBUG: WhileStatement body - token: ', FCurrentToken.Lexeme);

    // Check for EOF
    if FCurrentToken.TokenType = tkEndOfFile then
    begin
      Error('Unexpected end of file in WHILE loop');
      Break;
    end;

    // Skip blank lines
    if Check(tkEndOfLine) then
    begin
      Advance;
      Continue;
    end;

    // Skip comments
    if Check(tkComment) then
    begin
      Advance;
      Continue;
    end;

    // Process statement
    case AnsiUpperCase(FCurrentToken.Lexeme) of
      'PRINT': PrintStatement;
      'LET': AssignmentStatement;
      'DIM': DeclarationStatement;
      'IF': IfStatement;
      'FOR': ForStatement;
      'CALL': CallStatement;
      'GOTO': GoToStatement;
      'GOSUB': GoSubStatement;
      'RETURN': ReturnStatement;
    else
      if FCurrentToken.TokenType = tkIdentifier then
      begin
        if PeekToken.TokenType = tkOperator then
          AssignmentStatement
        else
        begin
          FAssembler.DefineLabel(FCurrentToken.Lexeme);
          Advance;
        end;
      end
      else
        Error('Unexpected token in WHILE body: ' + FCurrentToken.Lexeme);
    end;

    while Check(tkEndOfLine) do
      Advance;
  end;

  WriteLn('DEBUG: WhileStatement - Found WEND');
  Match(tkKeyword); // WEND
  WriteLn('DEBUG: WhileStatement - Complete');
end;

procedure TParser.ForStatement;
begin
  WriteLn('DEBUG: ForStatement - Current token: ', FCurrentToken.Lexeme);

  Match(tkKeyword); // Consume FOR
  Match(tkIdentifier); // Loop variable
  Match(tkOperator); // =
  Expression; // Start value
  Match(tkKeyword); // TO
  Expression; // End value

  if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'STEP') then
  begin
    Advance;
    Expression; // Step value
  end;

  Match(tkEndOfLine);

  WriteLn('DEBUG: ForStatement - Entering body loop');

  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'NEXT')) do
  begin
    WriteLn('DEBUG: ForStatement body - token: ', FCurrentToken.Lexeme);

    // Check for EOF
    if FCurrentToken.TokenType = tkEndOfFile then
    begin
      Error('Unexpected end of file in FOR loop');
      Break;
    end;

    // Skip blank lines
    if Check(tkEndOfLine) then
    begin
      Advance;
      Continue;
    end;

    // Skip comments
    if Check(tkComment) then
    begin
      Advance;
      Continue;
    end;

    // Process statement
    case AnsiUpperCase(FCurrentToken.Lexeme) of
      'PRINT': PrintStatement;
      'LET': AssignmentStatement;
      'DIM': DeclarationStatement;
      'IF': IfStatement;
      'WHILE': WhileStatement;
      'CALL': CallStatement;
      'GOTO': GoToStatement;
      'GOSUB': GoSubStatement;
      'RETURN': ReturnStatement;
    else
      if FCurrentToken.TokenType = tkIdentifier then
      begin
        if PeekToken.TokenType = tkOperator then
          AssignmentStatement
        else
        begin
          FAssembler.DefineLabel(FCurrentToken.Lexeme);
          Advance;
        end;
      end
      else
        Error('Unexpected token in FOR body: ' + FCurrentToken.Lexeme);
    end;

    while Check(tkEndOfLine) do
      Advance;
  end;

  WriteLn('DEBUG: ForStatement - Found NEXT');
  Match(tkKeyword); // NEXT
  if Check(tkIdentifier) then
    Advance; // Optional loop variable after NEXT
  WriteLn('DEBUG: ForStatement - Complete');
end;

procedure TParser.WithStatement;
var
  ObjectName: string;
begin
  WriteLn('DEBUG: WithStatement - Current token: ', FCurrentToken.Lexeme);

  Match(tkKeyword); // Consume WITH

  if not Check(tkIdentifier) then
    Error('Expected object name after WITH');

  ObjectName := FCurrentToken.Lexeme;
  WriteLn('DEBUG: WITH object: ', ObjectName);
  Advance; // Consume object name

  Match(tkEndOfLine);

  WriteLn('DEBUG: WithStatement - Entering body loop');

  // Parse the body of the WITH block
  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'END')) do
  begin
    WriteLn('DEBUG: WithStatement body - token: ', FCurrentToken.Lexeme);

    // Check for EOF to prevent infinite loop
    if FCurrentToken.TokenType = tkEndOfFile then
    begin
      Error('Unexpected end of file in WITH block');
      Break;
    end;

    // Skip blank lines
    if Check(tkEndOfLine) then
    begin
      Advance;
      Continue;
    end;

    // Skip comments
    if Check(tkComment) then
    begin
      Advance;
      Continue;
    end;

    // Process statement based on keyword
    case AnsiUpperCase(FCurrentToken.Lexeme) of
      'PRINT': PrintStatement;
      'LET': AssignmentStatement;
      'SET':
        begin
          Advance; // Consume SET
          AssignmentStatement;
        end;
      'DIM': DeclarationStatement;
      'IF': IfStatement;
      'WHILE': WhileStatement;
      'FOR': ForStatement;
      'CALL': CallStatement;
    else
      if FCurrentToken.TokenType = tkIdentifier then
      begin
        // Could be assignment or method call
        if PeekToken.TokenType = tkOperator then
          AssignmentStatement
        else if PeekToken.TokenType = tkParenthesisOpen then
          CallStatement
        else
        begin
          // It's a label or property access
          FAssembler.DefineLabel(FCurrentToken.Lexeme);
          Advance;
        end;
      end
      else
        Error('Unexpected token in WITH block: ' + FCurrentToken.Lexeme);
    end;

    // Skip trailing end-of-line
    while Check(tkEndOfLine) do
      Advance;
  end;

  WriteLn('DEBUG: WithStatement - Found END');
  Match(tkKeyword); // END
  Match(tkKeyword); // WITH
  WriteLn('DEBUG: WithStatement - Complete');
end;

procedure TParser.SubDefinition;
begin
  WriteLn('DEBUG: SubDefinition - Current token: ', FCurrentToken.Lexeme);

  Match(tkKeyword); // Consume SUB
  WriteLn('DEBUG: After SUB, current token: ', FCurrentToken.Lexeme);

  Match(tkIdentifier); // Sub name
  WriteLn('DEBUG: After sub name, current token: ', FCurrentToken.Lexeme);

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

  WriteLn('DEBUG: SubDefinition - Entering body loop');

  // Parse the body of the subroutine
  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'END')) do
  begin
    WriteLn('DEBUG: SubDefinition body - token: ', FCurrentToken.Lexeme);

    // Check for EOF to prevent infinite loop
    if FCurrentToken.TokenType = tkEndOfFile then
    begin
      Error('Unexpected end of file in SUB definition');
      Break;
    end;

    // Skip blank lines
    if Check(tkEndOfLine) then
    begin
      Advance;
      Continue;
    end;

    // Skip comments
    if Check(tkComment) then
    begin
      Advance;
      Continue;
    end;

    // Process statement based on keyword
    case AnsiUpperCase(FCurrentToken.Lexeme) of
      'PRINT': PrintStatement;
      'LET': AssignmentStatement;
      'SET':
        begin
          Advance; // Consume SET
          AssignmentStatement;
        end;
      'DIM': DeclarationStatement;
      'IF': IfStatement;
      'WHILE': WhileStatement;
      'FOR': ForStatement;
      'WITH': WithStatement;
      'RETURN': ReturnStatement;
      'CALL': CallStatement;
    else
      if FCurrentToken.TokenType = tkIdentifier then
      begin
        // Assignment without LET or label
        if PeekToken.TokenType = tkOperator then
          AssignmentStatement
        else
        begin
          // It's a label
          FAssembler.DefineLabel(FCurrentToken.Lexeme);
          Advance;
        end;
      end
      else
        Error('Unexpected token in SUB body: ' + FCurrentToken.Lexeme);
    end;

    // Skip trailing end-of-line
    while Check(tkEndOfLine) do
      Advance;
  end;

  WriteLn('DEBUG: SubDefinition - Found END');
  Match(tkKeyword); // END
  Match(tkKeyword); // SUB
  WriteLn('DEBUG: SubDefinition - Complete');
end;

procedure TParser.FunctionDefinition;
begin
  WriteLn('DEBUG: FunctionDefinition - Current token: ', FCurrentToken.Lexeme);

  Match(tkKeyword); // Consume FUNCTION
  WriteLn('DEBUG: After FUNCTION, current token: ', FCurrentToken.Lexeme);

  Match(tkIdentifier); // Function name
  WriteLn('DEBUG: After function name, current token: ', FCurrentToken.Lexeme);

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

  // Handle optional AS return type
  if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'AS') then
  begin
    Advance; // Consume AS
    Match(tkIdentifier); // Type name
  end;

  Match(tkEndOfLine);

  WriteLn('DEBUG: FunctionDefinition - Entering body loop');

  // Parse the body of the function
  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'END')) do
  begin
    WriteLn('DEBUG: FunctionDefinition body - token: ', FCurrentToken.Lexeme);

    // Check for EOF to prevent infinite loop
    if FCurrentToken.TokenType = tkEndOfFile then
    begin
      Error('Unexpected end of file in FUNCTION definition');
      Break;
    end;

    // Skip blank lines
    if Check(tkEndOfLine) then
    begin
      Advance;
      Continue;
    end;

    // Skip comments
    if Check(tkComment) then
    begin
      Advance;
      Continue;
    end;

    // Process statement based on keyword
    case AnsiUpperCase(FCurrentToken.Lexeme) of
      'PRINT': PrintStatement;
      'LET': AssignmentStatement;
      'SET':
        begin
          Advance; // Consume SET
          AssignmentStatement;
        end;
      'DIM': DeclarationStatement;
      'IF': IfStatement;
      'WHILE': WhileStatement;
      'FOR': ForStatement;
      'WITH': WithStatement;
      'RETURN': ReturnStatement;
      'CALL': CallStatement;
    else
      if FCurrentToken.TokenType = tkIdentifier then
      begin
        // Assignment without LET or label
        if PeekToken.TokenType = tkOperator then
          AssignmentStatement
        else
        begin
          // It's a label
          FAssembler.DefineLabel(FCurrentToken.Lexeme);
          Advance;
        end;
      end
      else
        Error('Unexpected token in FUNCTION body: ' + FCurrentToken.Lexeme);
    end;

    // Skip trailing end-of-line
    while Check(tkEndOfLine) do
      Advance;
  end;

  WriteLn('DEBUG: FunctionDefinition - Found END');
  Match(tkKeyword); // END
  Match(tkKeyword); // FUNCTION
  WriteLn('DEBUG: FunctionDefinition - Complete');
end;

procedure TParser.FormDefinition;
begin
  WriteLn('DEBUG: FormDefinition - Current token: ', FCurrentToken.Lexeme);

  Match(tkKeyword); // Consume FORM
  WriteLn('DEBUG: After FORM, current token: ', FCurrentToken.Lexeme);

  Match(tkIdentifier); // Form name
  WriteLn('DEBUG: After form name, current token: ', FCurrentToken.Lexeme);

  Match(tkEndOfLine);

  WriteLn('DEBUG: FormDefinition - Entering body loop');

  // Parse the body of the form
  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'END')) do
  begin
    WriteLn('DEBUG: FormDefinition body - token: ', FCurrentToken.Lexeme);

    // Check for EOF to prevent infinite loop
    if FCurrentToken.TokenType = tkEndOfFile then
    begin
      Error('Unexpected end of file in FORM definition');
      Break;
    end;

    // Skip blank lines
    if Check(tkEndOfLine) then
    begin
      Advance;
      Continue;
    end;

    // Skip comments
    if Check(tkComment) then
    begin
      Advance;
      Continue;
    end;

    // Process statement based on keyword
    case AnsiUpperCase(FCurrentToken.Lexeme) of
      'PRINT': PrintStatement;
      'LET': AssignmentStatement;
      'DIM': DeclarationStatement;
      'IF': IfStatement;
      'WHILE': WhileStatement;
      'FOR': ForStatement;
      'SUB': SubDefinition;
      'FUNCTION': FunctionDefinition;
      'SHOW': ShowStatement;
      'HIDE': HideStatement;
      'CALL': CallStatement;
    else
      if FCurrentToken.TokenType = tkIdentifier then
      begin
        // Assignment without LET or label
        if PeekToken.TokenType = tkOperator then
          AssignmentStatement
        else
        begin
          // It's a label or control definition
          FAssembler.DefineLabel(FCurrentToken.Lexeme);
          Advance;
        end;
      end
      else
        Error('Unexpected token in FORM body: ' + FCurrentToken.Lexeme);
    end;

    // Skip trailing end-of-line
    while Check(tkEndOfLine) do
      Advance;
  end;

  WriteLn('DEBUG: FormDefinition - Found END');
  Match(tkKeyword); // END
  Match(tkKeyword); // FORM
  WriteLn('DEBUG: FormDefinition - Complete');
end;

procedure TParser.ClassDefinition;
var
  ClassName: string;
  BaseClass: string;
begin
  WriteLn('DEBUG: ClassDefinition - Current token: ', FCurrentToken.Lexeme);

  Match(tkKeyword); // Consume CLASS
  WriteLn('DEBUG: After CLASS, current token: ', FCurrentToken.Lexeme);

  // Get class name
  if not Check(tkIdentifier) then
    Error('Expected class name after CLASS keyword');

  ClassName := FCurrentToken.Lexeme;
  WriteLn('DEBUG: Class name: ', ClassName);
  Advance; // Consume class name

  // Check for inheritance: CLASS Derived INHERITS Base
  if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'INHERITS') then
  begin
    WriteLn('DEBUG: Found INHERITS keyword');
    Advance; // Consume INHERITS

    if not Check(tkIdentifier) then
      Error('Expected base class name after INHERITS');

    BaseClass := FCurrentToken.Lexeme;
    WriteLn('DEBUG: Base class: ', BaseClass);
    Advance; // Consume base class name
  end;

  Match(tkEndOfLine);

  WriteLn('DEBUG: ClassDefinition - Entering body loop');

  // Parse the body of the class
  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'END')) do
  begin
    WriteLn('DEBUG: ClassDefinition body - token: ', FCurrentToken.Lexeme);

    // Check for EOF to prevent infinite loop
    if FCurrentToken.TokenType = tkEndOfFile then
    begin
      Error('Unexpected end of file in CLASS definition');
      Break;
    end;

    // Skip blank lines
    if Check(tkEndOfLine) then
    begin
      Advance;
      Continue;
    end;

    // Skip comments
    if Check(tkComment) then
    begin
      Advance;
      Continue;
    end;

    // Process statement based on keyword
    case AnsiUpperCase(FCurrentToken.Lexeme) of
      'DIM', 'PUBLIC', 'PRIVATE':
        begin
          WriteLn('DEBUG: Class property declaration');
          DeclarationStatement;
        end;
      'SUB':
        begin
          WriteLn('DEBUG: Class method (Sub)');
          SubDefinition;
        end;
      'FUNCTION':
        begin
          WriteLn('DEBUG: Class method (Function)');
          FunctionDefinition;
        end;
      'PROPERTY':
        begin
          WriteLn('DEBUG: Property accessor');
          // Handle Property Get/Set
          Advance; // Consume PROPERTY

          // Check for Get or Set
          if Check(tkKeyword) and ((AnsiUpperCase(FCurrentToken.Lexeme) = 'GET') or
                                   (AnsiUpperCase(FCurrentToken.Lexeme) = 'SET')) then
          begin
            Advance; // Consume GET or SET
            Match(tkIdentifier); // Property name

            // Handle parameters for SET
            if Check(tkParenthesisOpen) then
            begin
              Advance;
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
            end;

            Match(tkEndOfLine);

            // Parse property body
            while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'END')) do
            begin
              if FCurrentToken.TokenType = tkEndOfFile then
              begin
                Error('Unexpected end of file in PROPERTY definition');
                Break;
              end;

              if Check(tkEndOfLine) then
              begin
                Advance;
                Continue;
              end;

              if Check(tkComment) then
              begin
                Advance;
                Continue;
              end;

              // Parse property statements
              case AnsiUpperCase(FCurrentToken.Lexeme) of
                'LET': AssignmentStatement;
                'RETURN': ReturnStatement;
              else
                if FCurrentToken.TokenType = tkIdentifier then
                  AssignmentStatement
                else
                  Error('Unexpected token in PROPERTY body');
              end;

              while Check(tkEndOfLine) do
                Advance;
            end;

            Match(tkKeyword); // END
            Match(tkKeyword); // PROPERTY
          end;
        end;
    else
      if FCurrentToken.TokenType = tkIdentifier then
      begin
        // Could be a property without Dim keyword (legacy VB style)
        WriteLn('DEBUG: Implicit property or label: ', FCurrentToken.Lexeme);
        FAssembler.DefineLabel(ClassName + '.' + FCurrentToken.Lexeme);
        Advance;
      end
      else
        Error('Unexpected token in CLASS body: ' + FCurrentToken.Lexeme);
    end;

    // Skip trailing end-of-line
    while Check(tkEndOfLine) do
      Advance;
  end;

  WriteLn('DEBUG: ClassDefinition - Found END');
  Match(tkKeyword); // END
  Match(tkKeyword); // CLASS
  WriteLn('DEBUG: ClassDefinition - Complete for class: ', ClassName);
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
  ClassName: string;
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
    tkKeyword:
      begin
        // Handle NEW keyword for object instantiation
        if AnsiUpperCase(Token.Lexeme) = 'NEW' then
        begin
          WriteLn('DEBUG: NEW keyword detected for object instantiation');
          Advance; // Consume NEW

          if not Check(tkIdentifier) then
            Error('Expected class name after NEW');

          ClassName := FCurrentToken.Lexeme;
          WriteLn('DEBUG: Instantiating class: ', ClassName);
          Advance; // Consume class name

          // Handle optional constructor parameters
          if Check(tkParenthesisOpen) then
          begin
            Advance; // Consume '('

            // Parse constructor arguments
            if not Check(tkParenthesisClose) then
            begin
              Expression;
              while Check(tkComma) do
              begin
                Advance; // Consume comma
                Expression;
              end;
            end;

            Match(tkParenthesisClose); // Consume ')'
          end;

          Result := TLiteralNode.Create('NEW ' + ClassName, tkKeyword);
        end
        else
          Error('Expected expression, found keyword: ' + FCurrentToken.Lexeme);
      end;
    else
      Error('Expected expression, found ' + FCurrentToken.Lexeme);
  end;
end;

end.
