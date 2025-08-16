unit VirtualMachine;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,   BytecodeTypes; // Add the unit that defines TByteCodeProgram

type
  TVirtualMachine = class(TObject)
   private
     FProgram: TByteCodeProgram;
     FInstructionPointer: LongInt;
   public
     // New constructor to load a file
     //constructor Create(const BytecodeFilePath: string);
     procedure LoadBytecode(const FileName: string);

     procedure Run;
   end;
implementation


{ TVirtualMachine }

procedure TVirtualMachine.LoadBytecode(const FileName: string);
begin
  if not FileExists(FileName) then
    raise Exception.Create('Bytecode file not found.');

  // Add the logic to load the bytecode from the file
  Writeln('Bytecode loaded from: ', FileName);
end;

procedure TVirtualMachine.Run;
begin
  // Add the execution logic for the bytecode
  Writeln('Executing bytecode...');
  // For now, just simulate running the bytecode
end;

end.

