unit n64;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure N64_Init;
procedure N64_RunKayteScript(const Script: string);

implementation

// External C functions from libdragon (Assumed to be linked)
{$linklib dragon}
{$linklib m}
{$linklib c}

// Load necessary C functions for printing/debugging
procedure console_init; cdecl; external;
procedure printf(const fmt: PChar); cdecl; varargs; external;

// Simulated Kayte execution (Placeholder)
procedure ExecuteKayteCode(const Script: string);
begin
  printf('Executing Kayte Script: %s'#10, PChar(Script));
  // TODO: Integrate real Kayte interpreter for N64 execution
end;

procedure N64_Init;
begin
  console_init;
  printf('N64 Kayte Runtime Initialized'#10);
end;

procedure N64_RunKayteScript(const Script: string);
begin
  ExecuteKayteCode(Script);
end;

end.

