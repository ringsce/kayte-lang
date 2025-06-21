// UParserTypes.pas
// Defines token types, token records, and Abstract Syntax Tree (AST) node types for Kayte.

unit UParserTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs; // Contnrs for TObjectList

type
  // --- Lexer-related Types ---

  // TTokenType: Defines all possible types of tokens in Kayte.
  TTokenType = (
    ttEOF,             // End of File
    ttError,           // General error token
    ttIdentifier,      // e.g., 'myVar', 'HandleLoginButton'
    ttStringLiteral,   // e.g., "Hello World"
    ttIntegerLiteral,  // e.g., 123, 0
    ttKeywordFunction, // 'Function'
    ttKeywordEnd,      // 'End'
    ttKeywordIf,       // 'If'
    ttKeywordThen,     // 'Then'
    ttKeywordElse,     // 'Else'
    ttKeywordPrint,    // 'Print'
    ttKeywordShow,     // 'Show'
    ttKeywordForm,     // 'Form'
    ttKeywordClose,    // 'Close'
    ttKeywordGet,      // 'GET'
    ttKeywordSet,      // 'SET'
    ttKeywordTo,       // 'TO'
    ttKeywordAnd,      // 'AND'
    ttKeywordOr,       // 'OR'
    ttKeywordNot,      // 'NOT'
    ttKeywordDim,      // 'Dim'
    ttKeywordAs,       // 'As'
    ttKeywordString,   // 'String' (Type specifier)
    ttLParen,          // '('
    ttRParen,          // ')'
    ttDot,             // '.'
    ttEqual,           // '='
    ttPlus,            // '+'
    ttMinus,           // '-'
    ttMultiply,        // '*'
    ttDivide,          // '/'
    ttColon,           // ':' (for future use, e.g., labels)
    ttComment          // ' (single quote for comment)
  );

  // TToken: Represents a single lexical unit found by the lexer.
  TToken = record
    TokenType: TTokenType;
    Value: String;    // The actual text of the token
    Line: Integer;    // Line number where token starts
    Column: Integer;  // Column number where token starts
  end;

  // --- Abstract Syntax Tree (AST) Node Types ---

  // Forward declarations for AST nodes that might reference each other
  TKayteExpression = class;
  TKayteStatement = class;

  // TKayteASTNode: Base class for all AST nodes.
  // Provides common properties like source position for error reporting.
  TKayteASTNode = class
  private
    FLine: Integer;
    FColumn: Integer;
  public
    constructor Create(ALine, AColumn: Integer); overload;
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
  end;

  // TKayteProgram: Represents the root of the AST, containing all functions.
  TKayteProgram = class(TKayteASTNode)
  private
    FFunctions: TObjectList; // List of TKayteFunctionDef
  public
    constructor Create(ALine, AColumn: Integer); overload;
    destructor Destroy; override;
    property Functions: TObjectList read FFunctions; // Items are TKayteFunctionDef
  end;

  // TKayteFunctionDef: Represents a Kayte function/subroutine definition.
  TKayteFunctionDef = class(TKayteASTNode)
  private
    FName: String;
    FStatements: TObjectList; // List of TKayteStatement
  public
    constructor Create(ALine, AColumn: Integer; const AName: String); overload;
    destructor Destroy; override;
    property Name: String read FName;
    property Statements: TObjectList read FStatements; // Items are TKayteStatement
  end;

  // TKayteStatement: Base class for all executable statements.
  TKayteStatement = class(TKayteASTNode)
  public
    constructor Create(ALine, AColumn: Integer); overload;
  end;

  // TPrintStatement: Represents a 'Print' statement.
  TPrintStatement = class(TKayteStatement)
  private
    FExpression: TKayteExpression;
  public
    constructor Create(ALine, AColumn: Integer; AExpression: TKayteExpression); overload;
    destructor Destroy; override;
    property Expression: TKayteExpression read FExpression;
  end;

  // TShowFormStatement: Represents a 'SHOW FORM' statement.
  TShowFormStatement = class(TKayteStatement)
  private
    FFormName: String; // The name of the .kfrm file (e.g., "LoginForm")
  public
    constructor Create(ALine, AColumn: Integer; const AFormName: String); overload;
    property FormName: String read FFormName;
  end;

  // TCloseFormStatement: Represents a 'CLOSE FORM' statement.
  TCloseFormStatement = class(TKayteStatement)
  private
    FFormName: String; // The name of the form to close
  public
    constructor Create(ALine, AColumn: Integer; const AFormName: String); overload;
    property FormName: String read FFormName;
  end;

  // TGetSetTarget: Represents a Form.Control.Property access (e.g., LoginForm.EditUsername.Text).
  TGetSetTarget = class(TKayteASTNode)
  private
    FFormName: String;
    FControlName: String;
    FPropertyName: String;
  public
    constructor Create(ALine, AColumn: Integer; const AFormName, AControlName, APropertyName: String); overload;
    property FormName: String read FFormName;
    property ControlName: String read FControlName;
    property PropertyName: String read FPropertyName;
  end;

  // TGetStatement: Represents a 'GET Form.Control.Property TO Variable' statement.
  TGetStatement = class(TKayteStatement)
  private
    FTarget: TGetSetTarget;
    FVariableName: String;
  public
    constructor Create(ALine, AColumn: Integer; ATarget: TGetSetTarget; const AVariableName: String); overload;
    destructor Destroy; override;
    property Target: TGetSetTarget read FTarget;
    property VariableName: String read FVariableName;
  end;

  // TSetStatement: Represents a 'SET Form.Control.Property = Value' statement.
  TSetStatement = class(TKayteStatement)
  private
    FTarget: TGetSetTarget;
    FValueExpression: TKayteExpression;
  public
    constructor Create(ALine, AColumn: Integer; ATarget: TGetSetTarget; AValueExpression: TKayteExpression); overload;
    destructor Destroy; override;
    property Target: TGetSetTarget read FTarget;
    property ValueExpression: TKayteExpression read FValueExpression;
  end;

  // TIfStatement: Represents an 'IF Condition THEN ... ELSE ... END IF' statement.
  TIfStatement = class(TKayteStatement)
  private
    FCondition: TKayteExpression;
    FThenBlock: TObjectList; // List of TKayteStatement
    FElseBlock: TObjectList; // Optional list of TKayteStatement
  public
    constructor Create(ALine, AColumn: Integer; ACondition: TKayteExpression;
                       AThenBlock, AElseBlock: TObjectList); overload;
    destructor Destroy; override;
    property Condition: TKayteExpression read FCondition;
    property ThenBlock: TObjectList read FThenBlock; // Items are TKayteStatement
    property ElseBlock: TObjectList read FElseBlock; // Items are TKayteStatement
  end;

  // TKayteExpression: Base class for all expressions.
  TKayteExpression = class(TKayteASTNode)
  public
    constructor Create(ALine, AColumn: Integer); overload;
  end;

  // TLiteralExpression: Represents a literal value (string, integer).
  TLiteralExpression = class(TKayteExpression)
  private
    FValue: Variant; // Can hold String, Integer
  public
    constructor Create(ALine, AColumn: Integer; const AValue: Variant); overload;
    property Value: Variant read FValue;
  end;

  // TVariableExpression: Represents a variable reference.
  TVariableExpression = class(TKayteExpression)
  private
    FVariableName: String;
  public
    constructor Create(ALine, AColumn: Integer; const AVariableName: String); overload;
    property VariableName: String read FVariableName;
  end;

  // TBinaryExpression: Represents an operation with two operands (e.g., 1 + 2, "a" = "b").
  TBinaryExpression = class(TKayteExpression)
  private
    FLeft: TKayteExpression;
    FOperator: TTokenType; // e.g., ttEqual, ttPlus, ttAnd
    FRight: TKayteExpression;
  public
    constructor Create(ALine, AColumn: Integer; ALeft: TKayteExpression;
                       AOperator: TTokenType; ARight: TKayteExpression); overload;
    destructor Destroy; override;
    property Left: TKayteExpression read FLeft;
    property Operator: TTokenType read FOperator;
    property Right: TKayteExpression read FRight;
  end;


implementation

{ TKayteASTNode }

constructor TKayteASTNode.Create(ALine, AColumn: Integer);
begin
  FLine := ALine;
  FColumn := AColumn;
end;

{ TKayteProgram }

constructor TKayteProgram.Create(ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FFunctions := TObjectList.Create(True); // Owns objects
end;

destructor TKayteProgram.Destroy;
begin
  FreeAndNil(FFunctions);
  inherited Destroy;
end;

{ TKayteFunctionDef }

constructor TKayteFunctionDef.Create(ALine, AColumn: Integer; const AName: String);
begin
  inherited Create(ALine, AColumn);
  FName := AName;
  FStatements := TObjectList.Create(True); // Owns objects
end;

destructor TKayteFunctionDef.Destroy;
begin
  FreeAndNil(FStatements);
  inherited Destroy;
end;

{ TKayteStatement }

constructor TKayteStatement.Create(ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
end;

{ TPrintStatement }

constructor TPrintStatement.Create(ALine, AColumn: Integer; AExpression: TKayteExpression);
begin
  inherited Create(ALine, AColumn);
  FExpression := AExpression;
end;

destructor TPrintStatement.Destroy;
begin
  FreeAndNil(FExpression);
  inherited Destroy;
end;

{ TShowFormStatement }

constructor TShowFormStatement.Create(ALine, AColumn: Integer; const AFormName: String);
begin
  inherited Create(ALine, AColumn);
  FFormName := AFormName;
end;

{ TCloseFormStatement }

constructor TCloseFormStatement.Create(ALine, AColumn: Integer; const AFormName: String);
begin
  inherited Create(ALine, AColumn);
  FFormName := AFormName;
end;

{ TGetSetTarget }

constructor TGetSetTarget.Create(ALine, AColumn: Integer; const AFormName, AControlName, APropertyName: String);
begin
  inherited Create(ALine, AColumn);
  FFormName := AFormName;
  FControlName := AControlName;
  FPropertyName := APropertyName;
end;

{ TGetStatement }

constructor TGetStatement.Create(ALine, AColumn: Integer; ATarget: TGetSetTarget; const AVariableName: String);
begin
  inherited Create(ALine, AColumn);
  FTarget := ATarget;
  FVariableName := AVariableName;
end;

destructor TGetStatement.Destroy;
begin
  FreeAndNil(FTarget);
  inherited Destroy;
end;

{ TSetStatement }

constructor TSetStatement.Create(ALine, AColumn: Integer; ATarget: TGetSetTarget; AValueExpression: TKayteExpression);
begin
  inherited Create(ALine, AColumn);
  FTarget := ATarget;
  FValueExpression := AValueExpression;
end;

destructor TSetStatement.Destroy;
begin
  FreeAndNil(FTarget);
  FreeAndNil(FValueExpression);
  inherited Destroy;
end;

{ TIfStatement }

constructor TIfStatement.Create(ALine, AColumn: Integer; ACondition: TKayteExpression;
                               AThenBlock, AElseBlock: TObjectList);
begin
  inherited Create(ALine, AColumn);
  FCondition := ACondition;
  FThenBlock := AThenBlock; // Parser will create and pass owned lists
  FElseBlock := AElseBlock;
end;

destructor TIfStatement.Destroy;
begin
  FreeAndNil(FCondition);
  FreeAndNil(FThenBlock); // Lists own their contents
  FreeAndNil(FElseBlock);
  inherited Destroy;
end;

{ TKayteExpression }

constructor TKayteExpression.Create(ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
end;

{ TLiteralExpression }

constructor TLiteralExpression.Create(ALine, AColumn: Integer; const AValue: Variant);
begin
  inherited Create(ALine, AColumn);
  FValue := AValue;
end;

{ TVariableExpression }

constructor TVariableExpression.Create(ALine, AColumn: Integer; const AVariableName: String);
begin
  inherited Create(ALine, AColumn);
  FVariableName := AVariableName;
end;

{ TBinaryExpression }

constructor TBinaryExpression.Create(ALine, AColumn: Integer; ALeft: TKayteExpression;
                                   AOperator: TTokenType; ARight: TKayteExpression);
begin
  inherited Create(ALine, AColumn);
  FLeft := ALeft;
  FOperator := AOperator;
  FRight := ARight;
end;

destructor TBinaryExpression.Destroy;
begin
  FreeAndNil(FLeft);
  FreeAndNil(FRight);
  inherited Destroy;
end;

end.
