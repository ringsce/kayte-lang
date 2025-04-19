unit n64;

{$MODE OBJFPC}
{$LINK n64.a}  // Link against your custom static library
{$LINKLIB c}   // Ensure C runtime is linked

interface

procedure InitializeKayte;
procedure RunKayteScript(const Script: PChar);

implementation

uses
  ctypes;

// These functions must be implemented in your `n64.a` static library
procedure n64_init_display; cdecl; external;
procedure n64_init_controller; cdecl; external;
procedure n64_enable_interrupts; cdecl; external;
procedure n64_console_print(msg: PChar); cdecl; external;
function n64_controller_pressed(button: cint): cint; cdecl; external;

const
  BUTTON_A = $8000;

procedure InitializeKayte;
begin
  n64_console_print('Initializing Kayte on custom N64 setup...');
  n64_init_display;
  n64_init_controller;
  n64_enable_interrupts;
  n64_console_print('Kayte environment setup complete.');
end;

procedure RunKayteScript(const Script: PChar);
begin
  n64_console_print(Script);  // Replace with actual Kayte interpreter call
end;

procedure MainLoop;
begin
  while True do
  begin
    if (n64_controller_pressed(BUTTON_A) <> 0) then
    begin
      RunKayteScript('print("Hello from custom Kayte N64!")');
    end;
  end;
end;

begin
  InitializeKayte;
  MainLoop;
end.

