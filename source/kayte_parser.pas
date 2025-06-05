unit kayte_parser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, StrUtils, fgl;  // for TFPGMap

type
TKayteParser = class
  private
    FVars: TVarMap;
    FOnMessageBox: TKayteFunctionHandler;
    FOnInputBox: TKayteFunctionHandler;
    function ExtractArgs(const Line: string): string;
    function EvalExpr(const Expr: string): string;
    function EvalCondition(const Cond: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseLine(const Line: string; var i: Integer; const Lines: TStringList);
    procedure ParseScript(const Script: TStringList);
    property OnMessageBox: TKayteFunctionHandler read FOnMessageBox write FOnMessageBox;
    property OnInputBox: TKayteFunctionHandler read FOnInputBox write FOnInputBox;
  end;

TKayteFunctionHandler = procedure(const Args: string);
TVarMap = specialize TFPGMap<String, TStringObject>;


  TStringObject = class
    Value: String;
    constructor Create(const AValue: String);
  end;

  constructor TStringObject.Create(const AValue: String);
  begin
    Value := AValue;
  end;



implementation

var
  Vars: TVarMap;
begin
  Vars := TVarMap.Create;
  Vars.Add('username', TStringObject.Create('pedro'));
  Writeln('Hello, ', Vars['username'].Value);
  Vars.Free;
end;

constructor TKayteParser.Create;
begin
  inherited Create;
  FVars := TVarMap.Create;
  FVars.Sorted := False;
end;

destructor TKayteParser.Destroy;
begin
  FVars.Free;
  inherited Destroy;
end;

function TKayteParser.ExtractArgs(const Line: string): string;
var
  StartPos, EndPos: Integer;
begin
  StartPos := Pos('(', Line);
  EndPos := RPos(')', Line);
  if (StartPos > 0) and (EndPos > StartPos) then
    Result := Trim(Copy(Line, StartPos + 1, EndPos - StartPos - 1))
  else
    Result := '';
end;

function TKayteParser.EvalExpr(const Expr: string): string;
var
  VarName: string;
  i: Integer;
begin
  VarName := Trim(Expr);
  i := FVars.IndexOf(VarName);
  if i <> -1 then
    Result := FVars.Objects[i].Value
  else if (VarName.StartsWith('"') and VarName.EndsWith('"')) then
    Result := Copy(VarName, 2, Length(VarName) - 2)
  else
    Result := VarName;
end;

function TKayteParser.EvalCondition(const Cond: string): Boolean;
var
  Parts: TStringArray;
  LeftVal, RightVal: string;
begin
  Parts := Cond.Split(['==']);
  if Length(Parts) = 2 then
  begin
    LeftVal := EvalExpr(Parts[0]);
    RightVal := EvalExpr(Parts[1]);
    Exit(LeftVal = RightVal);
  end;
  Result := False;
end;

procedure TKayteParser.ParseLine(const Line: string; var i: Integer; const Lines: TStringList);
var
  Clean, Args, VarName, Val: string;
  BlockStart, BlockEnd, j: Integer;
begin
  Clean := Trim(Line);

  if Clean = '' then Exit;

  // let name = "Kayte"
  if Clean.StartsWith('let ') then
  begin
    Delete(Clean, 1, 4);
    VarName := Trim(Copy(Clean, 1, Pos('=', Clean) - 1));
    Val := Trim(Copy(Clean, Pos('=', Clean) + 1, MaxInt));
    Val := EvalExpr(Val);

    if FVars.IndexOf(VarName) = -1 then
      FVars.AddObject(VarName, TStringObject.Create(Val))
    else
      FVars.Objects[FVars.IndexOf(VarName)].Value := Val;
  end

  // MessageBox / InputBox
  else if Clean.StartsWith('MessageBox(') then
  begin
    Args := EvalExpr(ExtractArgs(Clean));
    if Assigned(FOnMessageBox) then FOnMessageBox(Args);
  end
  else if Clean.StartsWith('InputBox(') then
  begin
    Args := EvalExpr(ExtractArgs(Clean));
    if Assigned(FOnInputBox) then FOnInputBox(Args);
  end

  // if condition {
  else if Clean.StartsWith('if ') and Clean.EndsWith('{') then
  begin
    Args := Trim(Copy(Clean, 4, Length(Clean) - 4));
    SetLength(Args, Length(Args) - 1); // remove trailing '{'
    if EvalCondition(Args) then
    begin
      Inc(i);
      while (i < Lines.Count) and (Trim(Lines[i]) <> '}') do
      begin
        ParseLine(Lines[i], i, Lines);
        Inc(i);
      end;
    end
    else
    begin
      // skip block
      Inc(i);
      while (i < Lines.Count) and (Trim(Lines[i]) <> '}') do Inc(i);
    end;
  end

  // while condition {
  else if Clean.StartsWith('while ') and Clean.EndsWith('{') then
  begin
    Args := Trim(Copy(Clean, 7, Length(Clean) - 7));
    SetLength(Args, Length(Args) - 1); // remove trailing '{'
    BlockStart := i + 1;

    // find matching end
    j := BlockStart;
    while (j < Lines.Count) and (Trim(Lines[j]) <> '}') do Inc(j);
    BlockEnd := j;

    while EvalCondition(Args) do
    begin
      for j := BlockStart to BlockEnd - 1 do
        ParseLine(Lines[j], j, Lines);
    end;
    i := BlockEnd;
  end;
end;

procedure TKayteParser.ParseScript(const Script: TStringList);
var
  i: Integer;
begin
  i := 0;
  while i < Script.Count do
  begin
    ParseLine(Script[i], i, Script);
    Inc(i);
  end;
end;

end.

