unit KayteParser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type
  EKayteParserError = class(Exception);

  TKayteASTNode = class
    // Base class for all nodes in the Abstract Syntax Tree (AST)
  end;




  TKayteFunctionNode = class(TKayteASTNode)
    FunctionName: string;
    Parameters: TStringList;
    Body: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

  TKayteParser = class
  private
    FSource: TStringList;
    FCurrentLine: Integer;
    function ParseFunction: TKayteFunctionNode;
    function ParseLine(const Line: string): TKayteASTNode;
    public
    function Parse(const Content: string): string;

    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromString(const SourceCode: string);
    function Parse: TList;
  end;

implementation

{ TKayteFunctionNode }

constructor TKayteFunctionNode.Create;
begin
  Parameters := TStringList.Create;
  Body := TStringList.Create;
end;

destructor TKayteFunctionNode.Destroy;
begin
  Parameters.Free;
  Body.Free;
  inherited Destroy;
end;

{ TKayteParser }

constructor TKayteParser.Create;
begin
  FSource := TStringList.Create;
  FCurrentLine := 0;
end;

destructor TKayteParser.Destroy;
begin
  FSource.Free;
  inherited Destroy;
end;

procedure TKayteParser.LoadFromFile(const FileName: string);
begin
  FSource.LoadFromFile(FileName);
  FCurrentLine := 0;
end;

procedure TKayteParser.LoadFromString(const SourceCode: string);
begin
  FSource.Text := SourceCode;
  FCurrentLine := 0;
end;


function TKayteParser.Parse(const Content: string): string;
begin
  // Perform parsing logic here
  Result := '<parsed>' + Content + '</parsed>';  // Example parsing logic
end;

function TKayteParser.ParseFunction: TKayteFunctionNode;
var
  Line: string;
  FuncNode: TKayteFunctionNode;
begin
  FuncNode := TKayteFunctionNode.Create;
  try
    Line := Trim(FSource[FCurrentLine]);

    // Example of parsing a function definition, like: "function foo(a, b)"
    if Pos('function', Line) = 1 then
    begin
      Delete(Line, 1, Length('function'));
      Line := Trim(Line);
      FuncNode.FunctionName := Copy(Line, 1, Pos('(', Line) - 1);
      Line := Trim(Copy(Line, Pos('(', Line) + 1, Length(Line)));
      Line := Copy(Line, 1, Pos(')', Line) - 1);
      FuncNode.Parameters.CommaText := Line;

      Inc(FCurrentLine);

      // Parse function body
      while (FCurrentLine < FSource.Count) and (Trim(FSource[FCurrentLine]) <> 'end') do
      begin
        FuncNode.Body.Add(Trim(FSource[FCurrentLine]));
        Inc(FCurrentLine);
      end;

      // Make sure we are at 'end'
      if (FCurrentLine >= FSource.Count) or (Trim(FSource[FCurrentLine]) <> 'end') then
        raise EKayteParserError.Create('Function definition not properly closed with "end".');

      Inc(FCurrentLine);  // Skip the 'end'
    end
    else
      raise EKayteParserError.Create('Expected function declaration.');
  except
    FuncNode.Free;
    raise;
  end;

  Result := FuncNode;
end;

function TKayteParser.ParseLine(const Line: string): TKayteASTNode;
begin
  Result := nil;

  if Pos('function', Line) = 1 then
    Result := ParseFunction
  else
    raise EKayteParserError.Create('Unknown syntax: ' + Line);
end;

function TKayteParser.Parse: TList;
var
  AST: TList;
  Line: string;
  Node: TKayteASTNode;
begin
  AST := TList.Create;
  try
    while FCurrentLine < FSource.Count do
    begin
      Line := Trim(FSource[FCurrentLine]);

      // Skip empty lines or comments
      if (Line = '') or (Line[1] = '#') then
      begin
        Inc(FCurrentLine);
        Continue;
      end;

      // Parse line by line and build the AST
      Node := ParseLine(Line);
      AST.Add(Node);

      Inc(FCurrentLine);
    end;
  except
    AST.Free;
    raise;
  end;

  Result := AST;
end;

end.

