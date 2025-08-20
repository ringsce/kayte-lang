unit AST;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TokenDefs; // Make sure TokenDefs is also included here

// Define the core AST node types as classes
type
  // Define a base class for all expression nodes
  TExpressionNode = class(TObject);

  // Define a base class for all statement nodes
  TStatementNode = class(TObject);

  // A list to hold multiple statement nodes
  TStatementNodeList = class(TList)
    function Add(AStatement: TStatementNode): Integer;
    function GetItem(Index: Integer): TStatementNode;
    property Items[Index: Integer]: TStatementNode read GetItem; default;
  end;

  // Now, declare the concrete expression node classes
  TLiteralNode = class(TExpressionNode)
  public
    Lexeme: String;
    TokenType: TTokenType;
    constructor Create(ALexeme: String; ATokenType: TTokenType);
  end;

  TBinaryOpNode = class(TExpressionNode)
  public
    Op: String;
    Left: TExpressionNode;
    Right: TExpressionNode;
    constructor Create(AOp: String; ALeft, ARight: TExpressionNode);
  end;

  TUnaryOpNode = class(TExpressionNode)
  public
    Op: String;
    Right: TExpressionNode;
    constructor Create(AOp: String; ARight: TExpressionNode);
  end;

implementation

{ TStatementNodeList }

function TStatementNodeList.Add(AStatement: TStatementNode): Integer;
begin
  Result := inherited Add(AStatement);
end;

function TStatementNodeList.GetItem(Index: Integer): TStatementNode;
begin
  Result := TStatementNode(inherited Get(Index));
end;

// Now, implement the constructors for the new expression node classes

{ TLiteralNode }
constructor TLiteralNode.Create(ALexeme: String; ATokenType: TTokenType);
begin
  inherited Create;
  Lexeme := ALexeme;
  TokenType := ATokenType;
end;

{ TBinaryOpNode }
constructor TBinaryOpNode.Create(AOp: String; ALeft, ARight: TExpressionNode);
begin
  inherited Create;
  Op := AOp;
  Left := ALeft;
  Right := ARight;
end;

{ TUnaryOpNode }
constructor TUnaryOpNode.Create(AOp: String; ARight: TExpressionNode);
begin
  inherited Create;
  Op := AOp;
  Right := ARight;
end;

end.

