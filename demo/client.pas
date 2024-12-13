unit client;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, c99;

procedure RunClient;

implementation

procedure RunClient;
var
  Parser: TC99Parser;
  FunctionList: TStringList;
  I: Integer;
begin
  Parser := TC99Parser.Create;
  try
    if Parser.ParseKayteFile('sample.kayte') then
    begin
      WriteLn('C Functions Found:');
      FunctionList := Parser.GetFunctionList;
      for I := 0 to FunctionList.Count - 1 do
      begin
        WriteLn(FunctionList[I]);
      end;
    end
    else
      WriteLn('Failed to parse the .kayte file or file does not exist.');
  finally
    Parser.Free;
  end;
end;

end.

