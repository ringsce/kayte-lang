program vb6interpreter;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, StrUtils, Contnrs, InterpreterUtils; // <- Contnrs gives you TStack

type
  TSubroutineMap = class(TStringList)
  public
    procedure Add(SubName: String; LineNumber: Integer);
  end;
  TVarType = (vtInteger, vtString);

  TVariable = record
    vtype: TVarType;
    intValue: Integer;
    strValue: String;
  end;
  PVariable = ^TVariable;

  TLoop = record
    startLine: Integer;
    varName: String;
    conditionVal: Integer;
  end;

  TLoopStack = array of TLoop;

var
  Vars: TStringList;
  Labels: TStringList;
  Code: TStringList;
  Subs: TStringList;
  Stack: TStack;          // Only this one
  LoopStack: TLoopStack;


  procedure TSubroutineMap.Add(SubName: String; LineNumber: Integer);
begin
  Self.AddObject(SubName, TObject(PtrInt(LineNumber)));
end;


procedure SetVar(const name: String; val: TVariable);
var
  idx: Integer;
  varPtr: PVariable;
begin
  idx := Vars.IndexOf(name);
  if idx = -1 then
  begin
    New(varPtr);
    varPtr^ := val;
    Vars.AddObject(name, TObject(varPtr));
  end
  else
  begin
    varPtr := PVariable(Vars.Objects[idx]);
    varPtr^ := val;
  end;
end;

function GetVar(const name: String): TVariable;
var
  idx: Integer;
  varPtr: PVariable;
begin
  idx := Vars.IndexOf(name);
  if idx = -1 then
  begin
    WriteLn('Error: Variable ', name, ' not defined');
    Halt(1);
  end;
  varPtr := PVariable(Vars.Objects[idx]);
  Result := varPtr^;
end;

(* BuildLabels *)
procedure BuildLabels;
  var
    i: Integer;
    line, labelName: String;
  begin
    for i := 0 to Code.Count - 1 do
    begin
      line := Trim(Code[i]);
      if (line <> '') and (line[Length(line)] = ':') then
      begin
        labelName := Copy(line, 1, Length(line) - 1);
        Labels.AddObject(labelName, TObject(PtrInt(i)));
      end;
    end;
  end;


(* Testing *)
procedure ExecuteLine(line: String; var pc: Integer);
var
  parts: TStringArray;
  cmd, arg1, arg2: String;
  varVal: TVariable;
  idx: Integer;
begin
  line := Trim(line);
  if line = '' then Exit;
  if line[Length(line)] = ':' then Exit;

  parts := SplitString(line, ' ');
  if Length(parts) = 0 then Exit;
  cmd := LowerCase(parts[0]);

  if cmd = 'dim' then
  begin
    arg1 := parts[1];
    if Pos('string', LowerCase(parts[3])) > 0 then
    begin
      varVal.vtype := vtString;
      varVal.strValue := '';
    end
    else
    begin
      varVal.vtype := vtInteger;
      varVal.intValue := 0;
    end;
    SetVar(arg1, varVal);
  end
  else if cmd = 'let' then
  begin
    arg1 := parts[1];
    arg2 := parts[3];
    varVal := GetVar(arg1);
    if varVal.vtype = vtInteger then
      varVal.intValue := StrToIntDef(arg2, 0)
    else
      varVal.strValue := StringReplace(arg2, '"', '', [rfReplaceAll]);
    SetVar(arg1, varVal);
  end
  else if cmd = 'print' then
  begin
    arg1 := parts[1];
    varVal := GetVar(arg1);
    if varVal.vtype = vtInteger then
      WriteLn(varVal.intValue)
    else
      WriteLn(varVal.strValue);
  end
  else if cmd = 'input' then
  begin
    arg1 := parts[1];
    varVal := GetVar(arg1);
    if varVal.vtype = vtInteger then
      ReadLn(varVal.intValue)
    else
      ReadLn(varVal.strValue);
    SetVar(arg1, varVal);
  end
  else if cmd = 'while' then
  begin
    arg1 := parts[1];
    arg2 := parts[3];

    varVal := GetVar(arg1);
    if IntToStr(varVal.intValue) <> arg2 then
    begin
      while (pc < Code.Count) and (LowerCase(Trim(Code[pc])) <> 'wend') do
        Inc(pc);
    end
    else
    begin
      SetLength(LoopStack, Length(LoopStack) + 1);
      LoopStack[High(LoopStack)].startLine := pc;
      LoopStack[High(LoopStack)].varName := arg1;
      LoopStack[High(LoopStack)].conditionVal := StrToIntDef(arg2, 0);
    end;
  end
  else if cmd = 'wend' then
  begin
    if Length(LoopStack) = 0 then
    begin
      WriteLn('Error: WEND without WHILE');
      Halt(1);
    end;

    varVal := GetVar(LoopStack[High(LoopStack)].varName);
    if varVal.intValue = LoopStack[High(LoopStack)].conditionVal then
      pc := LoopStack[High(LoopStack)].startLine - 1
    else
      SetLength(LoopStack, Length(LoopStack) - 1);
  end
  else if cmd = 'if' then
  begin
    arg1 := parts[1];
    arg2 := parts[3];
    varVal := GetVar(arg1);
    if IntToStr(varVal.intValue) = arg2 then
    begin
      if (LowerCase(parts[4]) = 'then') and (LowerCase(parts[5]) = 'goto') then
      begin
        arg1 := parts[6];
        idx := Labels.IndexOf(arg1);
        if idx <> -1 then
          pc := Integer(PtrInt(Labels.Objects[idx])) - 1;
      end;
    end;
  end
  else if cmd = 'goto' then
  begin
    arg1 := parts[1];
    idx := Labels.IndexOf(arg1);
    if idx <> -1 then
      pc := Integer(PtrInt(Labels.Objects[idx])) - 1;
  end
  else if cmd = 'call' then
  begin
    arg1 := parts[1];
    idx := Subs.IndexOf(arg1);
    if idx <> -1 then
    begin
      Stack.Push(Pointer(pc));
      pc := Integer(PtrInt(Subs.Objects[idx]));
    end
    else
    begin
      WriteLn('Error: Sub not found: ', arg1);
      Halt(1);
    end;
  end
  else if cmd = 'let' then
  begin
    arg1 := parts[1];
    arg2 := parts[3];
    varVal := GetVar(arg1);
    if varVal.vtype = vtInteger then
      varVal.intValue := StrToIntDef(arg2, 0)
    else
      varVal.strValue := StringReplace(arg2, '"', '', [rfReplaceAll]);
    SetVar(arg1, varVal);
  end
  else if cmd = 'print' then
  begin
    arg1 := parts[1];
    varVal := GetVar(arg1);
    if varVal.vtype = vtInteger then
      WriteLn(varVal.intValue)
    else
      WriteLn(varVal.strValue);
  end
  else if cmd = 'if' then
  begin
    arg1 := parts[1];
    arg2 := parts[3];
    varVal := GetVar(arg1);
    if IntToStr(varVal.intValue) = arg2 then
    begin
      if (LowerCase(parts[4]) = 'then') and (LowerCase(parts[5]) = 'goto') then
      begin
        arg1 := parts[6];
        if Labels.IndexOf(arg1) <> -1 then
          pc := Integer(PtrInt(Labels.Objects[Labels.IndexOf(arg1)])) - 1;
      end;
    end;
  end
  else if cmd = 'call' then
begin
  arg1 := parts[1];
  if Subs.IndexOf(arg1) <> -1 then
  begin
    Stack.Push(Pointer(pc)); // properly push
    pc := Integer(PtrInt(Subs.Objects[Subs.IndexOf(arg1)])); // proper call
  end
  else
  begin
    WriteLn('Error: Sub not found: ', arg1);
    Halt(1);
  end;
end
else if cmd = 'goto' then
  begin
    arg1 := parts[1];
    if Labels.IndexOf(arg1) <> -1 then
      pc := PtrInt(Labels.Objects[Labels.IndexOf(arg1)]) - 1;
  end
  else if cmd = 'endsub' then
  begin
    if Stack.Count = 0 then
    begin
      WriteLn('Error: EndSub without Call');
      Halt(1);
    end;
    pc := PtrInt(Stack.Pop);
  end;
(* End *)


var
  Code, Vars, Labels: TStringList;
  Subs: TSubroutineMap;
  pc: Integer;

begin
  Code := TStringList.Create;
  Vars := TStringList.Create;
  Vars.Sorted := False;
  Labels := TStringList.Create;
  Labels.Sorted := False;

  LoadCode;

  BuildLabels(Code, Labels);
  BuildSubs(Code, Subs);

  pc := 0;
  while pc < Code.Count do
  begin
    ExecuteLine(Code[pc], pc);
    Inc(pc);
  end;

  Vars.Free;
  Labels.Free;
  Code.Free;
  Subs.Free;
end.
