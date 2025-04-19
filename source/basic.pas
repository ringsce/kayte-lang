unit basic;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, StrUtils, fpexprpars;

type
  TBASICInterpreter = class
  private
    FProgram: TStringList;
    FVars: TStringList;
    FStack: TList;
    FCurrentLine: Integer;
    FLoopStack: TList;

    function GetVarValue(const Name: string): Integer;
    procedure SetVarValue(const Name: string; Value: Integer);
    function EvaluateExpression(const Expr: string): Integer;
    procedure ExecuteLine(const Line: string);
    procedure GotoLine(LineNum: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadProgram(Source: TStrings);
    procedure SaveProgram(Dest: TStrings);
    procedure Run;
  end;

implementation

type
  PLoopContext = ^TLoopContext;
  TLoopContext = record
    VarName: string;
    Limit: Integer;
    StartLine: Integer;
  end;

constructor TBASICInterpreter.Create;
begin
  FProgram := TStringList.Create;
  FVars := TStringList.Create;
  FStack := TList.Create;
  FLoopStack := TList.Create;
end;

destructor TBASICInterpreter.Destroy;
var
  i: Integer;
begin
  for i := 0 to FLoopStack.Count - 1 do
    Dispose(PLoopContext(FLoopStack[i]));
  FLoopStack.Free;
  FProgram.Free;
  FVars.Free;
  FStack.Free;
  inherited Destroy;
end;

function TBASICInterpreter.GetVarValue(const Name: string): Integer;
var
  idx: Integer;
begin
  idx := FVars.IndexOfName(Name);
  if idx = -1 then
    Result := 0
  else
    Result := StrToInt(FVars.ValueFromIndex[idx]);
end;

procedure TBASICInterpreter.SetVarValue(const Name: string; Value: Integer);
begin
  FVars.Values[Name] := IntToStr(Value);
end;

function TBASICInterpreter.EvaluateExpression(const Expr: string): Integer;
var
  Parser: TFPExpressionParser;
  i: Integer;
begin
  Parser := TFPExpressionParser.Create(nil);
  try
    for i := 0 to FVars.Count - 1 do
        Parser.Variables[FVars.Names[i]] := StrToFloat(FVars.ValueFromIndex[i]);

    Parser.Expression := Expr;
    Result := Round(Parser.Evaluate.ResFloat);
  except
    on E: Exception do
    begin
      WriteLn('Error evaluating expression: ', Expr, ' (', E.Message, ')');
      Result := 0;
    end;
  end;
  Parser.Free;
end;

procedure TBASICInterpreter.ExecuteLine(const Line: string);
var
  Cmd, Arg, VarName, S: string;
  Val, i: Integer;
  LoopCtx: PLoopContext;
begin
  S := Trim(Line);
  if S = '' then Exit;
  Cmd := UpperCase(ExtractWord(1, S, [' ']));
  Arg := Trim(Copy(S, Length(Cmd) + 2, MaxInt));

  if Cmd = 'LET' then
  begin
    VarName := ExtractWord(1, Arg, ['=']);
    Val := EvaluateExpression(Trim(Copy(Arg, Length(VarName) + 2, MaxInt)));
    SetVarValue(Trim(VarName), Val);
  end
  else if Cmd = 'PRINT' then
    WriteLn(EvaluateExpression(Arg))
  else if Cmd = 'GOTO' then
    GotoLine(StrToIntDef(Arg, FCurrentLine + 1))
  else if Cmd = 'GOSUB' then
  begin
    FStack.Add(Pointer(NativeInt(FCurrentLine)));
    GotoLine(StrToIntDef(Arg, FCurrentLine + 1));
  end
  else if Cmd = 'RETURN' then
  begin
    if FStack.Count > 0 then
    begin
      FCurrentLine := NativeInt(FStack.Last) + 1;
      FStack.Delete(FStack.Count - 1);
      Exit;
    end;
  end
  else if Cmd = 'IF' then
  begin
    i := Pos('THEN', UpperCase(Arg));
    if i > 0 then
    begin
      if EvaluateExpression(Copy(Arg, 1, i - 1)) <> 0 then
        ExecuteLine(Copy(Arg, i + 4, MaxInt));
    end;
  end
  else if Cmd = 'FOR' then
  begin
    VarName := ExtractWord(1, Arg, ['=']);
    Arg := Trim(Copy(Arg, Length(VarName) + 2, MaxInt));
    i := Pos('TO', UpperCase(Arg));
    if i > 0 then
    begin
      Val := EvaluateExpression(Copy(Arg, 1, i - 1));
      SetVarValue(VarName, Val);
      New(LoopCtx);
      LoopCtx^.VarName := VarName;
      LoopCtx^.Limit := EvaluateExpression(Copy(Arg, i + 2, MaxInt));
      LoopCtx^.StartLine := FCurrentLine;
      FLoopStack.Add(LoopCtx);
    end;
  end
  else if Cmd = 'NEXT' then
  begin
    if FLoopStack.Count > 0 then
    begin
      LoopCtx := FLoopStack.Last;
      i := GetVarValue(LoopCtx^.VarName) + 1;
      SetVarValue(LoopCtx^.VarName, i);
      if i <= LoopCtx^.Limit then
      begin
        FCurrentLine := LoopCtx^.StartLine;
        Exit;
      end
      else
      begin
        Dispose(LoopCtx);
        FLoopStack.Delete(FLoopStack.Count - 1);
      end;
    end;
  end;
end;

procedure TBASICInterpreter.GotoLine(LineNum: Integer);
var
  i: Integer;
  LinePrefix: string;
begin
  LinePrefix := IntToStr(LineNum) + ' ';
  for i := 0 to FProgram.Count - 1 do
    if StartsStr(LinePrefix, FProgram[i]) then
    begin
      FCurrentLine := i - 1;
      Exit;
    end;
end;

procedure TBASICInterpreter.LoadProgram(Source: TStrings);
begin
  FProgram.Assign(Source);
end;

procedure TBASICInterpreter.SaveProgram(Dest: TStrings);
begin
  Dest.Assign(FProgram);
end;

procedure TBASICInterpreter.Run;
begin
  FCurrentLine := 0;
  while FCurrentLine < FProgram.Count do
  begin
    ExecuteLine(FProgram[FCurrentLine]);
    Inc(FCurrentLine);
  end;
end;

end.

