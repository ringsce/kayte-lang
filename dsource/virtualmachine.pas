unit VirtualMachine;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TVirtualMachine = class
  private
    // TODO: bytecode/register storage fields
  public
    procedure LoadBytecode(const FileName: string);
    procedure Run;
  end;

implementation

{ TVirtualMachine }

procedure TVirtualMachine.LoadBytecode(const FileName: string);
begin
  if not FileExists(FileName) then
    raise Exception.Create('Bytecode file not found.');

  // TODO: actually read the bytecode instead of just logging the path
  Writeln('Bytecode loaded from: ', FileName);
end;

procedure TVirtualMachine.Run;
begin
  // stub — doesn't execute anything yet
  Writeln('Executing bytecode...');
end;

end.

