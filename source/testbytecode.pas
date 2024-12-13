unit TestBytecode;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Bytecode, SysUtils, Classes;

type
  TBytecodeGeneratorTest = class(TTestCase)
  private
    BytecodeGen: TBytecodeGenerator;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBytecodeGeneration;
    procedure TestParseInstruction;
  end;

implementation

procedure TBytecodeGeneratorTest.SetUp;
begin
  BytecodeGen := TBytecodeGenerator.Create;
end;

procedure TBytecodeGeneratorTest.TearDown;
begin
  BytecodeGen.Free;
end;

procedure TBytecodeGeneratorTest.TestBytecodeGeneration;
var
  OutputFile: string;
begin
  // Test bytecode generation for a sample .kyte file
  OutputFile := 'test.bytecode';
  BytecodeGen.GenerateBytecode('example.kyte', OutputFile);

  // Check if bytecode was generated successfully
  CheckTrue(FileExists(OutputFile), 'Bytecode file was not generated.');
  DeleteFile(OutputFile); // Clean up
end;

procedure TBytecodeGeneratorTest.TestParseInstruction;
begin
  // Test individual instructions
  CheckEquals(Byte($00), BytecodeGen.ParseInstruction('NOP'), 'NOP failed');
  CheckEquals(Byte($01), BytecodeGen.ParseInstruction('LOAD'), 'LOAD failed');
  CheckEquals(Byte($02), BytecodeGen.ParseInstruction('ADD'), 'ADD failed');
  CheckEquals(Byte($FF), BytecodeGen.ParseInstruction('HALT'), 'HALT failed');
end;

initialization
  RegisterTest(TBytecodeGeneratorTest);
end.

