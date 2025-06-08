unit InterpreterUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSubroutineMap = class(TStringList)
  public
    procedure Add(SubName: String; LineNumber: Integer);
  end;

procedure BuildLabels(Code, Labels: TStringList);
procedure BuildSubs(Code: TStringList; var Subs: TSubroutineMap);

implementation

procedure TSubroutineMap.Add(SubName: String; LineNumber: Integer);
begin
  Self.AddObject(SubName, TObject(PtrInt(LineNumber)));
end;

procedure BuildLabels(Code, Labels: TStringList);
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

procedure BuildSubs(Code: TStringList; var Subs: TSubroutineMap);
var
  i: Integer;
  line: String;
begin
  Subs := TSubroutineMap.Create;
  for i := 0 to Code.Count - 1 do
  begin
    line := Trim(Code[i]);
    if LowerCase(Copy(line, 1, 3)) = 'sub' then
    begin
      Subs.Add(Copy(line, 5, Length(line) - 8), i); // "Sub xyz:" → "xyz"
    end;
  end;
end;

end.

