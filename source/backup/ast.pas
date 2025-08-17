unit AST;

interface

uses
  SysUtils;

// Forward declarations to handle circular references if needed
type
  TExpressionNode = class;
  TStatementNode = class;
  TStatementNodeList = class;

// Define the core AST node types as classes or records
type
  TExpressionNode = record
    // Fields to hold expression data, e.g.,
    // FType: TNodeType;
    // FValue: TValue;
  end;

  TStatementNode = record
    // Fields to hold statement data, e.g.,
    // FKind: TStatementKind;
  end;

  TStatementNodeList = class(TList)
    // A list to hold multiple statement nodes
  end;

implementation

end.
