unit n64;

{$MODE OBJFPC}
{$LINKLIB n64} // Link against your custom static library n64.a

interface

procedure InitializeKayte;
procedure RunKayteScript(const Script: PChar);

implementation

uses
  ctypes;

const
  BUTTON_A = $8000;

procedure n64_init_display; cdecl; external;
procedure n64_init_controller; cdecl; external;
procedure n64_enable_interrupts; cdecl; external;
procedure n64_console_print(msg: PChar); cdecl; external;
function n64_controller_pressed(button: cint): cint; cdecl; external;

procedure InitializeKayte;
begin
  n64_console_print('Initializing Kayte for N64...');
  n64_init_display;
  n64_console_print('Display initialized.');
  n64_init_controller;
  n64_console_print('Controller initialized.');
  n64_enable_interrupts;
  n64_console_print('Interrupts enabled.');
  n64_console_print('Kayte environment setup complete.');
end;

procedure RunKayteScript(const Script: PChar);
var
  Msg: String;
begin
  Msg := 'Running Kayte script: ' + Script;
  n64_console_print(PChar(Msg));
end;

procedure MainLoop;
begin
  while True do
  begin
    if n64_controller_pressed(BUTTON_A) <> 0 then
    begin
      RunKayteScript('print("Hello from Kayte on N64!")');
    end;
  end;
end;

begin
  InitializeKayte;
  MainLoop;
end.

