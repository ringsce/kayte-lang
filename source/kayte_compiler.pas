unit kayte_compiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

type
  EKayteCompileError = class(Exception);

function CompileKayteToObject(const KayteFile, OutputObject: string): Boolean;

implementation

function RunCommandAndWait(const Executable: string; const Args: array of string): Boolean;
var
  Output: string;
begin
  Result := RunCommand(Executable, Args, Output);
end;

function CompileKayteToObject(const KayteFile, OutputObject: string): Boolean;
var
  TempBin: string;
begin
  if not FileExists(KayteFile) then
    raise EKayteCompileError.Create('Input file not found: ' + KayteFile);

  TempBin := ChangeFileExt(KayteFile, '.bin');

  if not RunCommandAndWait('kaytec', [KayteFile, '-o', TempBin]) then
    raise EKayteCompileError.Create('Kayte compiler failed.');

  if not RunCommandAndWait('ld', ['-r', '-b', 'binary', '-o', OutputObject, TempBin]) then
    raise EKayteCompileError.Create('Failed to convert .bin to .o');

  DeleteFile(TempBin);

  Result := True;
end;

end.

