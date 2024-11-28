unit kaytetosnes;

uses
  sdk;

var
  SnesRom: TSuperNesRom;
  TestBank: TRomBank;
begin
  SnesRom := TSuperNesRom.Create;
  try
    // Initialize basic SNES components
    InitializeGraphics;
    InitializeSound;

    // Load an existing ROM or create a new one
    SnesRom.LoadRom('example.smc');

    // Example of writing data to the ROM
    FillChar(TestBank, SizeOf(TestBank), $FF);  // Fill with dummy data
    SnesRom.WriteBank(0, TestBank);  // Write dummy data to the first bank

    // Save modified ROM
    SnesRom.SaveRom('example_modified.smc');
  finally
    SnesRom.Free;
  end;
end.

