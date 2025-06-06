unit kayte_compiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure CompileKayteSources(const KayteFiles: array of string; const OutDir: string);

implementation

function RunCommand(const CmdLine: string): Boolean;
var
  AProcess: TProcess;
begin
  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := '/bin/sh';
    AProcess.Parameters.Add('-c');
    AProcess.Parameters.Add(CmdLine);
    AProcess.Options := [poWaitOnExit];
    AProcess.Execute;
    Result := AProcess.ExitStatus = 0;
  finally
    AProcess.Free;
  end;
end;

procedure CompileKayteSources(const KayteFiles: array of string; const OutDir: string);
var
  i: Integer;
  KayteFile, BaseName, BinFile, ObjFile, Cmd: string;
begin
  for i := 0 to High(KayteFiles) do
  begin
    KayteFile := KayteFiles[i];
    BaseName := ExtractFileNameOnly(KayteFile);
    BinFile := Format('%s/bytecode_%s.bin', [OutDir, BaseName]);
    ObjFile := Format('%s/bytecode_%s.o', [OutDir, BaseName]);

    WriteLn('→ Compiling ', KayteFile);

    Cmd := Format('kaytec "%s" -o "%s"', [KayteFile, BinFile]);
    if not RunCommand(Cmd) then
      raise Exception.Create('Failed to compile: ' + KayteFile);

    Cmd := Format('ld -r -b binary -o "%s" "%s"', [ObjFile, BinFile]);
    if not RunCommand(Cmd) then
      raise Exception.Create('Failed to convert .bin to .o for ' + BinFile);

    DeleteFile(BinFile);
  end;
end;

end.

