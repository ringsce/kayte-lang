unit c_backend;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

procedure TranslateC99ToKayte(const InputFile, OutputFile: string);

implementation

function MapCTypeToKayte(const CType: string): string;
begin
  if CType = 'int' then Exit('int');
  if CType = 'float' then Exit('float');
  if CType = 'double' then Exit('double');
  if CType = 'char' then Exit('char');
  if CType = 'void' then Exit('void');
  if CType = 'bool' then Exit('bool');
  Result := CType; // fallback (e.g., struct names)
end;

function StripComments(const Src: string): string;
var
  i: Integer;
  InComment, InBlock: Boolean;
begin
  Result := '';
  InComment := False;
  InBlock := False;
  i := 1;
  while i <= Length(Src) do
  begin
    if (not InComment) and (i < Length(Src)) and (Src[i] = '/') and (Src[i+1] = '/') then
    begin
      InComment := True;
      Inc(i, 2);
      Continue;
    end;
    if (not InBlock) and (i < Length(Src)) and (Src[i] = '/') and (Src[i+1] = '*') then
    begin
      InBlock := True;
      Inc(i, 2);
      Continue;
    end;
    if InComment and (Src[i] = #10) then
    begin
      InComment := False;
      Result += #10;
      Inc(i);
      Continue;
    end;
    if InBlock and (i < Length(Src)) and (Src[i] = '*') and (Src[i+1] = '/') then
    begin
      InBlock := False;
      Inc(i, 2);
      Continue;
    end;
    if not InComment and not InBlock then
      Result += Src[i];
    Inc(i);
  end;
end;

procedure TranslateC99ToKayte(const InputFile, OutputFile: string);
var
  SrcLines: TStringList;
  OutLines: TStringList;
  Line, CleanLine: string;
  i, p: Integer;
  CType, Ident, Params: string;
begin
  SrcLines := TStringList.Create;
  OutLines := TStringList.Create;
  try
    SrcLines.LoadFromFile(InputFile);

    for i := 0 to SrcLines.Count - 1 do
    begin
      Line := Trim(SrcLines[i]);
      if Line = '' then Continue;
      CleanLine := Trim(StripComments(Line));
      if CleanLine = '' then Continue;

      // Handle function declarations: e.g., int sum(int a, int b) {
      if (Pos('(', CleanLine) > 0) and (Pos(')', CleanLine) > 0) and (Pos('{', CleanLine) > 0) then
      begin
        p := Pos('(', CleanLine);
        CType := Trim(Copy(CleanLine, 1, RPos(' ', Copy(CleanLine, 1, p - 1))));
        Ident := Trim(Copy(CleanLine, RPos(' ', Copy(CleanLine, 1, p - 1)) + 1, p - RPos(' ', Copy(CleanLine, 1, p - 1)) - 1));
        Params := Copy(CleanLine, p + 1, Pos(')', CleanLine) - p - 1);

        OutLines.Add('func ' + Ident + '(' + Params + '): ' + MapCTypeToKayte(CType) + ' {');
        Continue;
      end;

      // Handle variable declarations: e.g., int x = 5;
      if (Pos(';', CleanLine) > 0) and (Pos('=', CleanLine) > 0) then
      begin
        p := Pos(' ', CleanLine);
        CType := Copy(CleanLine, 1, p - 1);
        CleanLine := Trim(Copy(CleanLine, p + 1, Length(CleanLine)));
        OutLines.Add('let ' + CleanLine + ' : ' + MapCTypeToKayte(CType));
        Continue;
      end;

      // Fallback: Copy line as-is
      OutLines.Add(CleanLine);
    end;

    OutLines.SaveToFile(OutputFile);
  finally
    SrcLines.Free;
    OutLines.Free;
  end;
end;

end.

