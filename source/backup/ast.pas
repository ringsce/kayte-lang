unit AST;

interface

uses
  SysUtils, Classes; // Added 'Classes' to resolve TList

// Define the core AST node types as classes
type
  // Define TExpressionNode as a class
  TExpressionNode = class
    // Fields to hold expression data, e.g.,
    // FType: TNodeType;
    // FValue: TValue;
  end;

  // Define TStatementNode as a class
  TStatementNode = class
    // Fields to hold statement data, e.g.,
    // FKind: TStatementKind;
  end;

  // A list to hold multiple statement nodes
  TStatementNodeList = class(TList)
    function Add(AStatement: TStatementNode): Integer;
    function GetItem(Index: Integer): TStatementNode;
    property Items[Index: Integer]: TStatementNode read GetItem; default;
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

end.
