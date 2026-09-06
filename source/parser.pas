unit Parser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fgl, TokenDefs, Lexer, AST, BytecodeTypes, Assembler;

type
  // Struct name -> ordered list of its field names, recorded at parse
  // time by StructDefinition. There's no runtime struct/object value:
  // a struct instance is just sugar for one flat variable slot per
  // field, named "InstanceName.FieldName" (see DeclarationStatement and
  // the dotted-identifier handling in AssignmentStatement/Primary).
  TStructFieldMap = specialize TFPGMap<string, TStringList>;

  // A single-statement parsing method, used to plug the right statement
  // vocabulary into ParseBlockBody (see its comment).
  TStatementProc = procedure of object;

  TParser = class
  private
    FLexer: TLexer;
    FCurrentToken: TToken;
    FPreviousToken: TToken;
    FAssembler: TAssembler;
    FStructs: TStructFieldMap;
    // While True, the expression-parsing methods below still parse (and
    // consume tokens for) an expression but emit no bytecode for it.
    // Used for expressions that are parsed only for their side effect on
    // the token stream (e.g. constructor arguments to a not-yet-modeled
    // NEW ClassName(...)) so no value is left dangling on the VM stack.
    FSuppressCodeGen: Boolean;
    // Name of the CLASS currently being parsed; only meaningful while
    // ClassBodyStatement is on the call stack (see ClassDefinition).
    FCurrentClassName: string;

    procedure Advance;
    procedure Match(ExpectedType: TTokenType);
    function Check(TokenType: TTokenType): Boolean;
    function MatchAny(const Types: array of TTokenType): Boolean;

    // Parsing rules (non-terminals)
    function Statement: TStatementNode;
    procedure DispatchStatement;
    procedure ParseBlockBody(const Context: string; StatementHandler: TStatementProc);
    procedure StructDefinition;
    procedure DeclarationStatement;
    procedure AssignmentStatement;
    procedure PrintStatement;
    procedure InputStatement;
    procedure MsgBoxStatement;
    procedure CallStatement;
    procedure ProcessStatement;
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
    procedure ClassBodyStatement;
    procedure PropertyDefinition;
    procedure PropertyBodyStatement;
    procedure FormDefinition;
    procedure ShowStatement;
    procedure HideStatement;
    procedure OptionStatement;  // NEW: Handle OPTION directives

    // Recursive-descent expression parsing methods.
    // Each one emits the bytecode for what it parses directly (postfix /
    // stack-machine order), leaving exactly one value on the VM's
    // evaluation stack - unless FSuppressCodeGen is set, in which case
    // they only consume tokens and emit nothing.
    procedure Expression;
    procedure SkipExpression; // Parse and discard (see FSuppressCodeGen)
    procedure SkipGenericParams; // Parse and discard "<T, U, ...>" (type erasure)
    procedure Equality;
    procedure Comparison;
    procedure Term;
    procedure Factor;
    procedure Unary;
    procedure Primary;

    // Helper functions
    procedure Error(const Message: String);
    procedure NextToken;
    function PeekToken: TToken;

    // Bytecode helper functions
    function Op_Variable(const VarName: string): Integer;
    function Op_StringLiteral(const Literal: string): Integer;
    function Op_IntegerLiteral(const Value: Int64): Integer;

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
  FSuppressCodeGen := False;
  FStructs := TStructFieldMap.Create;
  FCurrentToken := FLexer.GetNextToken;
  FPreviousToken := FCurrentToken;
  FAssembler := TAssembler.Create;
end;

destructor TParser.Destroy;
var
  I: Integer;
begin
  for I := 0 to FStructs.Count - 1 do
    FStructs.Data[I].Free;
  FStructs.Free;
  FAssembler.Free;
  inherited Destroy;
end;

function TParser.Parse: TByteCodeProgram;
begin
  FAssembler.SetProgramTitle('Kayte Program');

  while FCurrentToken.TokenType = tkComment do
    Advance;

  while FCurrentToken.TokenType <> tkEndOfFile do
  begin
    try
      while (FCurrentToken.TokenType = tkEndOfLine) or
            (FCurrentToken.TokenType = tkComment) do
        Advance;

      if FCurrentToken.TokenType = tkEndOfFile then
        Break;

      // OPTION EXPLICIT ON/OFF is recognized by the lexer as its own
      // token type (not routed through the OPTION keyword statement),
      // so it needs handling here before falling through to DispatchStatement.
      if (FCurrentToken.TokenType = tkOptionExplicitOn) or
         (FCurrentToken.TokenType = tkOptionExplicitOff) then
      begin
        Advance;
        Continue;
      end;

      DispatchStatement;

      while Check(tkEndOfLine) do
        Advance;

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
  // Uses CurrentProgram, not GetProgram: GetProgram transfers ownership and
  // nils out the assembler's program on first use, so it must only be
  // called once, after parsing has fully completed.
  Result := FAssembler.CurrentProgram.AddVariable(VarName);
end;

function TParser.Op_StringLiteral(const Literal: string): Integer;
begin
  Result := FAssembler.CurrentProgram.AddStringConstant(Literal);
end;

function TParser.Op_IntegerLiteral(const Value: Int64): Integer;
begin
  Result := FAssembler.CurrentProgram.AddIntegerLiteral(Value);
end;

//----------------------------------------------------------------------
// Parsing Rules
//----------------------------------------------------------------------

function TParser.Statement: TStatementNode;
begin
  Result := nil;
end;

// Dispatches a single keyword-based statement starting at FCurrentToken.
// Shared by the top-level parse loop and by any construct that embeds a
// single statement (single-line IF-THEN, WHILE/FOR loop bodies, and -
// via ParseBlockBody - WITH/SUB/FUNCTION/FORM bodies).
procedure TParser.DispatchStatement;
begin
  case AnsiUpperCase(FCurrentToken.Lexeme) of
    'END':
      Advance;
    'OPTION':
      OptionStatement;
    'PRINT':
      PrintStatement;
    'PROCESS':
      ProcessStatement;
    'INPUT':
      InputStatement;
    'MSGBOX':
      MsgBoxStatement;
    'LET':
      AssignmentStatement;
    'SET':
      begin
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
    'STRUCT':
      StructDefinition;
  else
    if FCurrentToken.TokenType = tkIdentifier then
    begin
      // Could be a label, or an assignment (with or without LET, and
      // optionally to a struct field: "P.X = 10").
      if (PeekToken.TokenType = tkOperator) or (PeekToken.TokenType = tkDot) then
        AssignmentStatement
      else
      begin
        FAssembler.DefineLabel(FCurrentToken.Lexeme);
        Advance;
      end;
    end
    else if FCurrentToken.TokenType = tkEndOfLine then
      Advance
    else
      Error('Unexpected token: ' + FCurrentToken.Lexeme);
  end;
end;

// Parses statements via StatementHandler until the next token is the
// keyword "END" that closes the block, skipping blank lines and comments
// in between. Shared by every block construct (WITH/SUB/FUNCTION/FORM/
// CLASS) so each one only has to supply what "a statement" means inside
// it and what to do with the block's name - the surrounding "loop until
// END, bail on EOF, skip blank lines" plumbing lives here once.
// Does not consume the "END" token itself, nor the keyword that follows
// it (e.g. "SUB") - the caller matches those since only it knows what
// they should be.
procedure TParser.ParseBlockBody(const Context: string; StatementHandler: TStatementProc);
begin
  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'END')) do
  begin
    if FCurrentToken.TokenType = tkEndOfFile then
    begin
      Error('Unexpected end of file in ' + Context);
      Break;
    end;

    if Check(tkEndOfLine) or Check(tkComment) then
    begin
      Advance;
      Continue;
    end;

    StatementHandler;

    while Check(tkEndOfLine) do
      Advance;
  end;
end;

// STRUCT Name
//   Field1 [AS Type]
//   Field2 [AS Type]
// END STRUCT
//
// Purely a compile-time declaration: it records the struct's field names
// so DIM can expand "DIM P AS Point" into one flat variable slot per
// field ("P.X", "P.Y", ...). There's no runtime struct value or type
// checking - fields are dynamically typed slots like any other variable.
procedure TParser.StructDefinition;
var
  StructName, FieldName: string;
  Fields: TStringList;
begin
  Match(tkKeyword); // Consume STRUCT

  if not Check(tkIdentifier) then
    Error('Expected struct name after STRUCT');
  StructName := FCurrentToken.Lexeme;
  Advance; // Consume struct name
  SkipGenericParams; // Optional "<T, U, ...>" - erased, see SkipGenericParams

  if FStructs.IndexOf(StructName) >= 0 then
    Error('Struct "' + StructName + '" is already defined');

  Match(tkEndOfLine);

  Fields := TStringList.Create;

  while not (Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'END')) do
  begin
    if FCurrentToken.TokenType = tkEndOfFile then
    begin
      Fields.Free;
      Error('Unexpected end of file in STRUCT definition');
      Exit;
    end;

    if Check(tkEndOfLine) or Check(tkComment) then
    begin
      Advance;
      Continue;
    end;

    if not Check(tkIdentifier) then
    begin
      Fields.Free;
      Error('Expected field name in STRUCT body, found: ' + FCurrentToken.Lexeme);
      Exit;
    end;

    FieldName := FCurrentToken.Lexeme;
    Advance; // Consume field name

    // Optional "AS Type" - parsed but not enforced (see unit comment).
    if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'AS') then
    begin
      Advance; // Consume AS
      if not Check(tkIdentifier) then
      begin
        Fields.Free;
        Error('Expected type name after AS');
        Exit;
      end;
      Advance; // Consume type name
      SkipGenericParams; // Optional "<T, U, ...>" - erased, see SkipGenericParams
    end;

    Fields.Add(FieldName);

    while Check(tkEndOfLine) do
      Advance;
  end;

  Match(tkKeyword); // END
  Match(tkKeyword); // STRUCT

  FStructs.Add(StructName, Fields);
end;

procedure TParser.OptionStatement;
begin
  Advance; // Consume OPTION

  // OPTION directives (e.g. "Explicit On", "Base 0") are compile-time only
  // and generate no bytecode, so the rest of the line is just discarded.
  while not Check(tkEndOfLine) and not Check(tkEndOfFile) do
    Advance;
end;

procedure TParser.IfStatement;
var
  JumpOverThen: Integer;
begin
  Match(tkKeyword); // Consume IF
  Expression; // Leaves the condition's truth value on the stack

  // Expect THEN
  if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'THEN') then
    Advance // Consume THEN
  else
    Error('Expected THEN after IF condition');

  // Single-line IF: IF condition THEN statement
  // Emit a placeholder conditional jump that skips the THEN-statement when
  // the condition is false; the real target is back-patched once we know
  // where that statement's bytecode ends.
  // NOTE: only the single-line form is supported - multi-line
  // IF...ELSE...END IF blocks are not yet implemented.
  JumpOverThen := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);

  if not (Check(tkEndOfLine) or Check(tkEndOfFile)) then
    DispatchStatement;

  FAssembler.PatchJumpTarget(JumpOverThen, FAssembler.CurrentInstructionIndex);
end;

procedure TParser.DeclarationStatement;
var
  VarNames: TStringList;
  TypeName: string;
  StructIdx, I, J: Integer;
  Fields: TStringList;
begin
  // Consume DIM/PUBLIC/PRIVATE keyword
  Advance;

  VarNames := TStringList.Create;
  try
    // Handle variable list: DIM a, b, c AS Type
    repeat
      if not Check(tkIdentifier) then
        Error('Expected variable name in declaration');

      VarNames.Add(FCurrentToken.Lexeme);
      Advance; // Consume variable name

      // Check for comma (more variables)
      if Check(tkComma) then
      begin
        Advance; // Consume comma
        Continue; // Get next variable
      end
      else
        Break; // No more variables

    until False;

    TypeName := '';

    // Handle optional AS type
    if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'AS') then
    begin
      Advance; // Consume AS
      if not Check(tkIdentifier) then
        Error('Expected type name after AS');
      TypeName := FCurrentToken.Lexeme;
      Advance; // Consume type name
      SkipGenericParams; // Optional "<T, U, ...>" - erased, see SkipGenericParams
    end;

    StructIdx := -1;
    if TypeName <> '' then
      StructIdx := FStructs.IndexOf(TypeName);

    for I := 0 to VarNames.Count - 1 do
    begin
      if StructIdx >= 0 then
      begin
        // Struct instance: one flat variable slot per field, named
        // "VarName.FieldName" (see the unit comment on StructDefinition).
        Fields := FStructs.Data[StructIdx];
        for J := 0 to Fields.Count - 1 do
          Op_Variable(VarNames[I] + '.' + Fields[J]);
      end
      else
        Op_Variable(VarNames[I]);
    end;
  finally
    VarNames.Free;
  end;
end;

procedure TParser.AssignmentStatement;
var
  VarName: string;
begin
  VarName := FCurrentToken.Lexeme;
  Advance; // Consume identifier

  // Struct field target: P.X = ...
  if Check(tkDot) then
  begin
    Advance; // Consume '.'
    if not Check(tkIdentifier) then
      Error('Expected field name after "."');
    VarName := VarName + '.' + FCurrentToken.Lexeme;
    Advance; // Consume field name
  end;

  if not Check(tkOperator) then
    Error('Expected assignment operator');
  Advance; // Consume operator
  // Emit bytecode for expression evaluation
  Expression;
  // Emit bytecode for assignment
  FAssembler.Emit(BC_ASSIGN, [Op_Variable(VarName)]);
end;

procedure TParser.PrintStatement;
var
  ArgCount: Integer;
begin
  Advance; // Consume PRINT keyword

  // Handle empty PRINT (just prints a newline)
  if Check(tkEndOfLine) or Check(tkEndOfFile) then
  begin
    FAssembler.Emit(BC_PRINT, [0]);
    Exit;
  end;

  ArgCount := 0;
  Expression; // Any expression: literal, variable, "a" & b, 1 + 2, ...
  Inc(ArgCount);

  // Handle additional arguments separated by commas
  while Check(tkComma) do
  begin
    Advance; // Consume comma
    Expression;
    Inc(ArgCount);
  end;

  FAssembler.Emit(BC_PRINT, [ArgCount]);
end;

procedure TParser.InputStatement;
begin
  Match(tkKeyword);
  if Check(tkStringLiteral) then
    Advance;
  Match(tkIdentifier);
end;

procedure TParser.MsgBoxStatement;
var
  ArgCount: Integer;
begin
  Match(tkKeyword);
  // No real GUI available, so MSGBOX prints its arguments to the console
  // (like PRINT), tagged so it's clear where the output came from.
  FAssembler.Emit(BC_LOAD_STRING, [Op_StringLiteral('"[MsgBox]"')]);
  ArgCount := 1;

  Expression;
  Inc(ArgCount);
  while Check(tkComma) do
  begin
    Advance;
    Expression;
    Inc(ArgCount);
  end;

  FAssembler.Emit(BC_PRINT, [ArgCount]);
end;

procedure TParser.CallStatement;
begin
  Match(tkKeyword);
  Match(tkIdentifier);
  SkipGenericParams; // Optional "<T, U, ...>" - erased, see SkipGenericParams
  // User-defined SUB/FUNCTION calls aren't executable yet (no call stack
  // or parameter binding), so arguments are parsed for correct
  // tokenization only, without leaving values on the VM stack.
  if Check(tkParenthesisOpen) then
  begin
    Advance;
    if not Check(tkParenthesisClose) then
    begin
      SkipExpression;
      while Check(tkComma) do
      begin
        Advance;
        SkipExpression;
      end;
    end;
    Match(tkParenthesisClose);
  end;
end;

// PROCESS <command-expr> [, <arg-expr>]* [TO <identifier>]
//
// Spawns an OS process: <command-expr> is the executable, each further
// comma-separated expression is passed as a separate argument (not
// shell-concatenated, so arguments containing spaces/quotes need no
// escaping). If a "TO <identifier>" clause is given, the process's
// captured output is stored in that variable; otherwise it's printed to
// the console directly, like MSGBOX/PRINT.
procedure TParser.ProcessStatement;
var
  ArgCount: Integer;
  DestVarIndex: Integer;
begin
  Advance; // Consume PROCESS keyword

  ArgCount := 0;
  Expression; // Command
  Inc(ArgCount);

  while Check(tkComma) do
  begin
    Advance; // Consume ','
    Expression; // Argument
    Inc(ArgCount);
  end;

  DestVarIndex := -1;
  if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'TO') then
  begin
    Advance; // Consume TO
    if not Check(tkIdentifier) then
      Error('Expected variable name after TO');
    DestVarIndex := Op_Variable(FCurrentToken.Lexeme);
    Advance; // Consume variable name
  end;

  FAssembler.Emit(BC_PROCESS, [ArgCount, DestVarIndex]);
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
var
  LoopStart, JumpToExit: Integer;
begin
  Match(tkKeyword); // Consume WHILE

  // The condition is re-evaluated every iteration, so its bytecode lives
  // at LoopStart and the back-edge below jumps to it, not to the body.
  LoopStart := FAssembler.CurrentInstructionIndex;
  Expression;
  Match(tkEndOfLine);

  JumpToExit := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);

  ParseBlockBody('WHILE loop', @DispatchStatement);

  FAssembler.Emit(BC_JUMP, [LoopStart]);
  FAssembler.PatchJumpTarget(JumpToExit, FAssembler.CurrentInstructionIndex);

  Match(tkKeyword); // WEND
end;

procedure TParser.ForStatement;
var
  LoopVarName, EndVarName, StepVarName: string;
  DescendingStep: Boolean;
  CmpOp: TByteCodeOp;
  LoopStart, JumpToExit: Integer;
begin
  Match(tkKeyword); // Consume FOR
  if not Check(tkIdentifier) then
    Error('Expected loop variable after FOR');
  LoopVarName := FCurrentToken.Lexeme;
  Advance; // Loop variable
  Match(tkOperator); // =
  Expression; // Start value
  FAssembler.Emit(BC_ASSIGN, [Op_Variable(LoopVarName)]); // loopvar := start
  Match(tkKeyword); // TO

  // End and step are evaluated once, at loop entry (not re-evaluated each
  // iteration), matching classic BASIC FOR semantics. They're stashed in
  // hidden per-loop-variable slots since the bytecode has no scratch
  // registers of its own.
  EndVarName := '__for_end$' + LoopVarName;
  StepVarName := '__for_step$' + LoopVarName;

  Expression; // End value
  FAssembler.Emit(BC_ASSIGN, [Op_Variable(EndVarName)]);

  // The step's compile-time sign (when known) picks the loop-continuation
  // comparison (<= for ascending, >= for descending). A non-literal step
  // (e.g. a variable or a parenthesized expression) defaults to ascending
  // - this is a deliberate simplification, not full VB6 semantics.
  DescendingStep := False;
  if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'STEP') then
  begin
    Advance;
    if (FCurrentToken.TokenType = tkOperator) and (FCurrentToken.Lexeme = '-') and
       (PeekToken.TokenType = tkIntegerLiteral) then
      DescendingStep := True;
    Expression; // Step value
  end
  else
    FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(1)]); // Default step: 1
  FAssembler.Emit(BC_ASSIGN, [Op_Variable(StepVarName)]);

  if DescendingStep then
    CmpOp := BC_CMP_GE
  else
    CmpOp := BC_CMP_LE;

  Match(tkEndOfLine);

  LoopStart := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_LOAD_VAR, [Op_Variable(LoopVarName)]);
  FAssembler.Emit(BC_LOAD_VAR, [Op_Variable(EndVarName)]);
  FAssembler.Emit(CmpOp, []);
  JumpToExit := FAssembler.CurrentInstructionIndex;
  FAssembler.Emit(BC_JUMP_IF_FALSE, [-1]);

  ParseBlockBody('FOR loop', @DispatchStatement);

  // loopvar := loopvar + step
  FAssembler.Emit(BC_LOAD_VAR, [Op_Variable(LoopVarName)]);
  FAssembler.Emit(BC_LOAD_VAR, [Op_Variable(StepVarName)]);
  FAssembler.Emit(BC_ADD, []);
  FAssembler.Emit(BC_ASSIGN, [Op_Variable(LoopVarName)]);
  FAssembler.Emit(BC_JUMP, [LoopStart]);
  FAssembler.PatchJumpTarget(JumpToExit, FAssembler.CurrentInstructionIndex);

  Match(tkKeyword); // NEXT
  if Check(tkIdentifier) then
    Advance; // Optional loop variable after NEXT
end;

procedure TParser.WithStatement;
begin
  Match(tkKeyword); // Consume WITH

  if not Check(tkIdentifier) then
    Error('Expected object name after WITH');
  Advance; // Consume object name

  Match(tkEndOfLine);

  ParseBlockBody('WITH block', @DispatchStatement);

  Match(tkKeyword); // END
  Match(tkKeyword); // WITH
end;

procedure TParser.SubDefinition;
begin
  Match(tkKeyword); // Consume SUB
  Match(tkIdentifier); // Sub name
  SkipGenericParams; // Optional "<T, U, ...>" - erased, see SkipGenericParams

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

  ParseBlockBody('SUB definition', @DispatchStatement);

  Match(tkKeyword); // END
  Match(tkKeyword); // SUB
end;

procedure TParser.FunctionDefinition;
begin
  Match(tkKeyword); // Consume FUNCTION
  Match(tkIdentifier); // Function name
  SkipGenericParams; // Optional "<T, U, ...>" - erased, see SkipGenericParams

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
    SkipGenericParams; // Optional "<T, U, ...>" - erased, see SkipGenericParams
  end;

  Match(tkEndOfLine);

  ParseBlockBody('FUNCTION definition', @DispatchStatement);

  Match(tkKeyword); // END
  Match(tkKeyword); // FUNCTION
end;

procedure TParser.FormDefinition;
begin
  Match(tkKeyword); // Consume FORM
  Match(tkIdentifier); // Form name

  Match(tkEndOfLine);

  ParseBlockBody('FORM definition', @DispatchStatement);

  Match(tkKeyword); // END
  Match(tkKeyword); // FORM
end;

// PROPERTY GET/SET Name [(params)]
//   ...
// END PROPERTY
//
// Modeled the same way as SUB/FUNCTION bodies (a plain statement list),
// since properties have no distinct runtime representation.
procedure TParser.PropertyDefinition;
begin
  Advance; // Consume PROPERTY

  if not (Check(tkKeyword) and ((AnsiUpperCase(FCurrentToken.Lexeme) = 'GET') or
                                (AnsiUpperCase(FCurrentToken.Lexeme) = 'SET'))) then
    Exit;

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

  ParseBlockBody('PROPERTY definition', @PropertyBodyStatement);

  Match(tkKeyword); // END
  Match(tkKeyword); // PROPERTY
end;

// One statement inside a PROPERTY GET/SET body.
procedure TParser.PropertyBodyStatement;
begin
  case AnsiUpperCase(FCurrentToken.Lexeme) of
    'LET': AssignmentStatement;
    'RETURN': ReturnStatement;
  else
    if FCurrentToken.TokenType = tkIdentifier then
      AssignmentStatement
    else
      Error('Unexpected token in PROPERTY body');
  end;
end;

// One statement inside a CLASS body: property declarations, methods, and
// property accessors get their own handling (see PropertyDefinition);
// anything else falling through to a bare identifier is treated as an
// implicit (Dim-less) property/label, qualified with the class's name.
procedure TParser.ClassBodyStatement;
begin
  case AnsiUpperCase(FCurrentToken.Lexeme) of
    'DIM', 'PUBLIC', 'PRIVATE':
      DeclarationStatement;
    'SUB':
      SubDefinition;
    'FUNCTION':
      FunctionDefinition;
    'PROPERTY':
      PropertyDefinition;
  else
    if FCurrentToken.TokenType = tkIdentifier then
    begin
      FAssembler.DefineLabel(FCurrentClassName + '.' + FCurrentToken.Lexeme);
      Advance;
    end
    else
      Error('Unexpected token in CLASS body: ' + FCurrentToken.Lexeme);
  end;
end;

procedure TParser.ClassDefinition;
var
  ClsName: string;
begin
  Match(tkKeyword); // Consume CLASS

  if not Check(tkIdentifier) then
    Error('Expected class name after CLASS keyword');

  ClsName := FCurrentToken.Lexeme;
  Advance; // Consume class name
  SkipGenericParams; // Optional "<T, U, ...>" - erased, see SkipGenericParams

  // Check for inheritance: CLASS Derived INHERITS Base
  if Check(tkKeyword) and (AnsiUpperCase(FCurrentToken.Lexeme) = 'INHERITS') then
  begin
    Advance; // Consume INHERITS

    if not Check(tkIdentifier) then
      Error('Expected base class name after INHERITS');

    Advance; // Consume base class name (inheritance not yet modeled in codegen)
  end;

  Match(tkEndOfLine);

  FCurrentClassName := ClsName;
  ParseBlockBody('CLASS definition', @ClassBodyStatement);
  FCurrentClassName := '';

  Match(tkKeyword); // END
  Match(tkKeyword); // CLASS
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
// Expression Parsing Methods
//
// These emit bytecode directly in postfix (stack-machine) order as they
// parse: each level leaves exactly one value on the VM's evaluation
// stack, built from whatever its operands pushed plus one opcode per
// operator - unless FSuppressCodeGen is set (see SkipExpression), in
// which case tokens are still consumed (so the parse stays in sync) but
// no bytecode is emitted and nothing is left on the stack.
//----------------------------------------------------------------------
procedure TParser.SkipExpression;
begin
  FSuppressCodeGen := True;
  try
    Expression;
  finally
    FSuppressCodeGen := False;
  end;
end;

// Kayte has no static type system (see TValueKind in VirtualMachine.pas),
// so type parameters can't be checked or specialized at compile time.
// Rather than reject generic-looking syntax outright, an optional
// "<T, U, ...>" list right after a name (STRUCT/CLASS/SUB/FUNCTION
// definitions, CALL and NEW instantiation sites, and "AS Type" type
// names) is parsed here and discarded: generic code runs exactly like
// its non-generic equivalent. Does not support nested type arguments
// (e.g. "List<List<T>>").
procedure TParser.SkipGenericParams;
begin
  if not (Check(tkOperator) and (FCurrentToken.Lexeme = '<')) then
    Exit;

  Advance; // Consume '<'

  if not Check(tkIdentifier) then
    Error('Expected type parameter name after "<"');
  Advance;

  while Check(tkComma) do
  begin
    Advance; // Consume ','
    if not Check(tkIdentifier) then
      Error('Expected type parameter name after ","');
    Advance;
  end;

  if not (Check(tkOperator) and (FCurrentToken.Lexeme = '>')) then
    Error('Expected ">" to close type parameter list');
  Advance; // Consume '>'
end;

procedure TParser.Expression;
begin
  Equality;
end;

procedure TParser.Equality;
var
  OperatorToken: TToken;
begin
  Comparison;
  while (FCurrentToken.TokenType = tkOperator) and ((FCurrentToken.Lexeme = '=') or (FCurrentToken.Lexeme = '<>')) do
  begin
    OperatorToken := FCurrentToken;
    Advance;
    Comparison;
    if not FSuppressCodeGen then
    begin
      if OperatorToken.Lexeme = '=' then
        FAssembler.Emit(BC_CMP_EQ, [])
      else
        FAssembler.Emit(BC_CMP_NEQ, []);
    end;
  end;
end;

procedure TParser.Comparison;
var
  OperatorToken: TToken;
  Op: string;
begin
  Term;
  while (FCurrentToken.TokenType = tkOperator) and ((FCurrentToken.Lexeme = '>') or (FCurrentToken.Lexeme = '<') or (FCurrentToken.Lexeme = '>=') or (FCurrentToken.Lexeme = '<=') or (FCurrentToken.Lexeme.ToUpper = 'IS')) do
  begin
    OperatorToken := FCurrentToken;
    Advance;
    Term;
    if not FSuppressCodeGen then
    begin
      Op := OperatorToken.Lexeme;
      if Op = '>' then
        FAssembler.Emit(BC_CMP_GT, [])
      else if Op = '<' then
        FAssembler.Emit(BC_CMP_LT, [])
      else if Op = '>=' then
        FAssembler.Emit(BC_CMP_GE, [])
      else if Op = '<=' then
        FAssembler.Emit(BC_CMP_LE, [])
      else
        // 'IS': no distinct object-identity model yet, treat as equality.
        FAssembler.Emit(BC_CMP_EQ, []);
    end;
  end;
end;

procedure TParser.Term;
var
  OperatorToken: TToken;
begin
  Factor;
  while (FCurrentToken.TokenType = tkOperator) and ((FCurrentToken.Lexeme = '+') or (FCurrentToken.Lexeme = '-') or (FCurrentToken.Lexeme = '&')) do
  begin
    OperatorToken := FCurrentToken;
    Advance;
    Factor;
    if not FSuppressCodeGen then
    begin
      if OperatorToken.Lexeme = '+' then
        FAssembler.Emit(BC_ADD, [])
      else if OperatorToken.Lexeme = '-' then
        FAssembler.Emit(BC_SUB, [])
      else
        FAssembler.Emit(BC_CONCAT, []);
    end;
  end;
end;

procedure TParser.Factor;
var
  OperatorToken: TToken;
begin
  Unary;
  while (FCurrentToken.TokenType = tkOperator) and ((FCurrentToken.Lexeme = '*') or (FCurrentToken.Lexeme = '/')) do
  begin
    OperatorToken := FCurrentToken;
    Advance;
    Unary;
    if not FSuppressCodeGen then
    begin
      if OperatorToken.Lexeme = '*' then
        FAssembler.Emit(BC_MUL, [])
      else
        FAssembler.Emit(BC_DIV, []);
    end;
  end;
end;

procedure TParser.Unary;
var
  OperatorToken: TToken;
begin
  if (FCurrentToken.TokenType = tkOperator) and ((FCurrentToken.Lexeme = '-') or (FCurrentToken.Lexeme.ToUpper = 'NOT')) then
  begin
    OperatorToken := FCurrentToken;
    Advance;
    Unary;
    if not FSuppressCodeGen then
    begin
      if OperatorToken.Lexeme = '-' then
        FAssembler.Emit(BC_NEG, [])
      else
        FAssembler.Emit(BC_NOT, []);
    end;
  end
  else
    Primary;
end;

procedure TParser.Primary;
var
  Token: TToken;
  VarName: string;
begin
  Token := FCurrentToken;
  case Token.TokenType of
    tkIntegerLiteral:
      begin
        Advance;
        if not FSuppressCodeGen then
          FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(StrToInt64Def(Token.Lexeme, 0))]);
      end;
    tkStringLiteral:
      begin
        Advance;
        if not FSuppressCodeGen then
          FAssembler.Emit(BC_LOAD_STRING, [Op_StringLiteral(Token.Lexeme)]);
      end;
    tkBooleanLiteral:
      begin
        Advance;
        if not FSuppressCodeGen then
        begin
          if AnsiUpperCase(Token.Lexeme) = 'TRUE' then
            FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(1)])
          else
            FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(0)]);
        end;
      end;
    tkIdentifier:
      begin
        VarName := Token.Lexeme;
        Advance;

        // Struct field read: P.X
        if Check(tkDot) then
        begin
          Advance; // Consume '.'
          if not Check(tkIdentifier) then
            Error('Expected field name after "."');
          VarName := VarName + '.' + FCurrentToken.Lexeme;
          Advance; // Consume field name
        end;

        if not FSuppressCodeGen then
          FAssembler.Emit(BC_LOAD_VAR, [Op_Variable(VarName)]);
      end;
    tkParenthesisOpen:
      begin
        Advance; // Consume '('
        Expression;
        Match(tkParenthesisClose); // Consume ')'
      end;
    tkKeyword:
      begin
        // Handle NEW keyword for object instantiation
        if AnsiUpperCase(Token.Lexeme) = 'NEW' then
        begin
          Advance; // Consume NEW

          if not Check(tkIdentifier) then
            Error('Expected class name after NEW');

          Advance; // Consume class name (no object model in the VM yet, so it's discarded)
          SkipGenericParams; // Optional "<T, U, ...>" - erased, see SkipGenericParams

          // Handle optional constructor parameters. There's no object
          // model in the VM yet, so arguments are parsed for correct
          // tokenization only; they don't leave values on the stack.
          if Check(tkParenthesisOpen) then
          begin
            Advance; // Consume '('

            if not Check(tkParenthesisClose) then
            begin
              SkipExpression;
              while Check(tkComma) do
              begin
                Advance; // Consume comma
                SkipExpression;
              end;
            end;

            Match(tkParenthesisClose); // Consume ')'
          end;

          // Placeholder result for the (not yet modeled) new object.
          if not FSuppressCodeGen then
            FAssembler.Emit(BC_LOAD_INT, [Op_IntegerLiteral(0)]);
        end
        else
          Error('Expected expression, found keyword: ' + FCurrentToken.Lexeme);
      end;
    else
      Error('Expected expression, found ' + FCurrentToken.Lexeme);
  end;
end;

end.
