unit AST;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TokenDefs;

type
  TExpressionNode = class(TObject);
  TStatementNode = class(TObject);

  TStatementNodeList = class(TList)
    function Add(AStatement: TStatementNode): Integer;
    function GetItem(Index: Integer): TStatementNode;
    property Items[Index: Integer]: TStatementNode read GetItem; default;
  end;

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

