unit kayte_vm;

{$mode objfpc}{$H+}

interface

uses
  SysUtils; // ✅ Needed for ChangeFileExt

procedure KayteVM_LoadModule(const Name: string; const Code: string);
procedure KayteVM_RunAll;

implementation

var
  Modules: array of record
    Name: string;
    Code: string;
  end;

procedure KayteVM_LoadModule(const Name: string; const Code: string);
begin
  SetLength(Modules, Length(Modules) + 1);
  Modules[High(Modules)].Name := ChangeFileExt(Name, '');
  Modules[High(Modules)].Code := Code;
end;

procedure KayteVM_RunAll;
var
  i: Integer;
begin
  for i := 0 to High(Modules) do
  begin
    WriteLn('▶️ Running module: ', Modules[i].Name);
    // TODO: Interpret bytecode here
    WriteLn('[BYTECODE] ', Length(Modules[i].Code), ' bytes loaded');
  end;
end;

end.

