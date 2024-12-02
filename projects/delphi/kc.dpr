program kc;



uses
  System.StartUpCopy,
  FMX.Forms,
  kc in 'kc.pas' {Form1},
  bytecode in 'Z:\kayte\source\bytecode.pas',
  cli in 'Z:\kayte\source\cli.pas',
  sdk in 'Z:\kayte\source\sdk.pas',
  simplehttpserver in 'Z:\kayte\source\simplehttpserver.pas',
  testbytecode in 'Z:\kayte\source\testbytecode.pas',
  testrunner in 'Z:\kayte\source\testrunner.pas',
  virtualmachine in 'Z:\kayte\source\virtualmachine.pas',
  vulkan;

uses
  System.StartUpCopy,
  FMX.Forms,
  kc in 'kc.pas' {Form1},
  bytecode in 'Z:\kayte\source\bytecode.pas',
  cli in 'Z:\kayte\source\cli.pas',
  sdk in 'Z:\kayte\source\sdk.pas',
  simplehttpserver in 'Z:\kayte\source\simplehttpserver.pas',
  testbytecode in 'Z:\kayte\source\testbytecode.pas',
  testrunner in 'Z:\kayte\source\testrunner.pas',
  virtualmachine in 'Z:\kayte\source\virtualmachine.pas',
  vulkan 2 in 'Z:\kayte\source\vulkan 2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
