unit c99;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes;

type
  TC99Parser = class
  private
    FFunctionList: TStringList;
    FKeywordList: TStringList;
    function ExtractFunctions(const Code: string): TStringList;
    function ExtractStructuresAndKeywords(const Code: string): TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function ParseKayteFile(const FileName: string): Boolean;
    function GetFunctionList: TStringList;
    function GetKeywordList: TStringList;
  end;

implementation

constructor TC99Parser.Create;
begin
  inherited Create;
  FFunctionList := TStringList.Create;
  FKeywordList := TStringList.Create;
end;

destructor TC99Parser.Destroy;
begin
  FFunctionList.Free;
  FKeywordList.Free;
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
      TrimmedLine := Trim(Line);
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

function TC99Parser.ExtractStructuresAndKeywords(const Code: string): TStringList;
const
  Keywords: array[0..9] of string = (
    'struct', 'enum', 'for', 'while', 'do', 'if', 'else', '#if', '#ifndef', '#endif'
  );
var
  Lines: TStringList;
  Line, TrimmedLine, Keyword: string;
begin
  Result := TStringList.Create;
  Lines := TStringList.Create;
  try
    Lines.Text := Code;
    for Line in Lines do
    begin
      TrimmedLine := Trim(Line);
      for Keyword in Keywords do
      begin
        if TrimmedLine.StartsWith(Keyword) or TrimmedLine.Contains(' ' + Keyword + ' ') then
        begin
          Result.Add(TrimmedLine);
          Break;
        end;
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
    FFunctionList.Free;
    FFunctionList := ExtractFunctions(KayteFile.Text);
    FKeywordList.Free;
    FKeywordList := ExtractStructuresAndKeywords(KayteFile.Text);
    Result := True;
  finally
    KayteFile.Free;
  end;
end;

function TC99Parser.GetFunctionList: TStringList;
begin
  Result := FFunctionList;
end;

function TC99Parser.GetKeywordList: TStringList;
begin
  Result := FKeywordList;
end;

end.

