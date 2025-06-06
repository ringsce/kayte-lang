unit kayte_compiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

type
  EKayteCompileError = class(Exception);

function CompileKayteToObject(const KayteFile, OutputObject: string): Boolean;

implementation

function RunCommandAndWait(const CmdLine: string): Boolean;
var
  AProcess: TProcess;
begin
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := '/bin/sh';
    AProcess.Parameters.Add('-c');
    AProcess.Parameters.Add(CmdLine);
    AProcess.Options := [poWaitOnExit, poUsePipes];
    AProcess.Execute;
    Result := AProcess.ExitStatus = 0;
  finally
    AProcess.Free;
  end;
end;

function CompileKayteToObject(const KayteFile, OutputObject: string): Boolean;
var
  TempBin: string;
  Cmd: string;
begin
  if not FileExists(KayteFile) then
    raise EKayteCompileError.Create('Input file not found: ' + KayteFile);

  TempBin := ChangeFileExt(KayteFile, '.bin');

  // Step 1: Compile .kayte file to .bin (raw bytecode)
  Cmd := Format('kaytec "%s" -o "%s"', [KayteFile, TempBin]);
  if not RunCommandAndWait(Cmd) then
    raise EKayteCompileError.Create('Kayte compiler failed.');

  // Step 2: Convert .bin to .o (object file)
  Cmd := Format('ld -r -b binary -o "%s" "%s"', [OutputObject, TempBin]);
  if not RunCommandAndWait(Cmd) then
    raise EKayteCompileError.Create('Failed to convert .bin to .o');

  // Optional: Remove temp .bin
  DeleteFile(TempBin);

  Result := True;
end;

end.

