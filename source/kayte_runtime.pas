unit kayte_runtime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PByte = ^Byte;

  TKayteModule = record
    Name: string;
    Data: PByte;
    Size: Integer;
  end;

var
  KayteModules: array of TKayteModule;

procedure RegisterModule(const Name: string; DataStart, DataEnd: PByte);

implementation

procedure RegisterModule(const Name: string; DataStart, DataEnd: PByte);
var
  ModEntry: TKayteModule;
begin
  ModEntry.Name := Name;
  ModEntry.Data := DataStart;
  ModEntry.Size := PtrUInt(DataEnd) - PtrUInt(DataStart);
  SetLength(KayteModules, Length(KayteModules) + 1);
  KayteModules[High(KayteModules)] := ModEntry;
end;

end.

