unit c99;


interface

uses
  SysUtils, Classes;

type
  TC99Parser = class
  private
    FFunctionList: TStringList;
    function ExtractFunctions(const Code: string): TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function ParseKayteFile(const FileName: string): Boolean;
    function GetFunctionList: TStringList;
  end;

implementation

constructor TC99Parser.Create;
begin
  inherited Create;
  FFunctionList := TStringList.Create;
end;

destructor TC99Parser.Destroy;
begin
  FFunctionList.Free;
  inherited Destroy;
end;

function TC99Parser.ExtractFunctions(const Code: string): TStringList;
var
  Lines: TStringList;
  Line, TrimmedLine: string;
  InFunction: Boolean;
begin
  Result := TStringList.Create;
  Lines := TStringList.Create;
  try
    Lines.Text := Code;
    InFunction := False;
    for Line in Lines do
    begin
      TrimmedLine := Trim(Line); // Use a separate variable for the trimmed line
      if TrimmedLine.Contains('(') and TrimmedLine.Contains(')') and TrimmedLine.Contains('{') then
      begin
        InFunction := True;
        Result.Add(TrimmedLine);
      end
      else if InFunction and TrimmedLine.Contains('}') then
      begin
        InFunction := False;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TC99Parser.ParseKayteFile(const FileName: string): Boolean;
var
  KayteFile: TStringList;
begin
  Result := False;
  if not FileExists(FileName) then
    Exit;

  KayteFile := TStringList.Create;
  try
    KayteFile.LoadFromFile(FileName);
    FFunctionList := ExtractFunctions(KayteFile.Text);
    Result := True;
  finally
    KayteFile.Free;
  end;
end;

function TC99Parser.GetFunctionList: TStringList;
begin
  Result := FFunctionList;
end;

end.

