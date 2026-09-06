unit kaytetosnes;

uses
  sdk;

var
  SnesRom: TSuperNesRom;
  TestBank: TRomBank;
begin
  SnesRom := TSuperNesRom.Create;
  try
    InitializeGraphics;
    InitializeSound;

    SnesRom.LoadRom('example.smc');

    FillChar(TestBank, SizeOf(TestBank), $FF); // placeholder bank contents
    SnesRom.WriteBank(0, TestBank);

    SnesRom.SaveRom('example_modified.smc');
  finally
    SnesRom.Free;
  end;
end.

