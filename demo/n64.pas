unit n64;

{$MODE OBJFPC}
{$LINKLIB libdragon}

interface

procedure InitializeKayte;
procedure RunKayteScript(const Script: PChar);

implementation

uses
  ctypes;


// Import libdragon functions from C
procedure display_init(resolution: cint; depth: cint; num_buffers: cint); cdecl; external 'libdragon';
procedure controller_init; cdecl; external 'libdragon';
procedure irq_init; cdecl; external 'libdragon';
procedure irq_enable; cdecl; external 'libdragon';
procedure console_init; cdecl; external 'libdragon';
procedure printf(format: PChar); cdecl; varargs; external 'libdragon';
function controller_scan: cint; cdecl; external 'libdragon';
function controller_pressed(port: cint): cint; cdecl; external 'libdragon';

const
  RESOLUTION_320x240 = 0;
  DEPTH_16_BPP = 2;
  CONTROLLER_1 = 0;
  BUTTON_A = $8000;

procedure InitializeKayte;
begin
  Writeln('Initializing Kayte for Nintendo 64...');

  display_init(RESOLUTION_320x240, DEPTH_16_BPP, 2);
  Writeln('Display initialized.');

  controller_init;
  Writeln('Controller initialized.');

  irq_init;
  irq_enable;
  Writeln('Interrupts enabled.');

  console_init;
  Writeln('Console initialized.');

  Writeln('Kayte environment setup complete.');
end;

// stub — just echoes the script text instead of interpreting it
procedure RunKayteScript(const Script: PChar);
begin
  printf('Running Kayte script: %s'#10, Script);
end;

procedure MainLoop;
begin
  while True do
  begin
    controller_scan;

    if (controller_pressed(CONTROLLER_1) and BUTTON_A) <> 0 then
    begin
      RunKayteScript('print("Hello from Kayte on N64!")');
    end;
  end;
end;

begin
  InitializeKayte;
  MainLoop;
end.

