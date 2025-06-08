program BuildKayte;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Process;

procedure CompileKayte(const KayteFile: string);
var
  Output, BinFile, BaseName: string;
  SrcTime, BinTime: TDateTime;
begin
  BaseName := ChangeFileExt(KayteFile, '');
  BinFile := BaseName + '.bin';

  if not FileExists(BinFile) then
  begin
    WriteLn('[build_kayte] Compiling ', KayteFile, ' → ', BinFile);
    RunCommand('kaytec', [KayteFile, '-o', BinFile], Output);
    Exit;
  end;

  SrcTime := FileAge(KayteFile);
  BinTime := FileAge(BinFile);

  if SrcTime > BinTime then
  begin
    WriteLn('[build_kayte] Recompiling ', KayteFile, ' (source is newer)');
    RunCommand('kaytec', [KayteFile, '-o', BinFile], Output);
  end
  else
    WriteLn('[build_kayte] Up to date: ', KayteFile);
end;

procedure CompileAllInDir(const Dir: string);
var
  SR: TSearchRec;
  KayteFile: string;
begin
  if FindFirst(IncludeTrailingPathDelimiter(Dir) + '*.kayte', faAnyFile, SR) = 0 then
  begin
    repeat
      KayteFile := IncludeTrailingPathDelimiter(Dir) + SR.Name;
      CompileKayte(KayteFile);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end
  else
    WriteLn('[build_kayte] No .kayte files found in ', Dir);
end;

var
  Path: string;
begin
  if ParamCount > 0 then
    Path := ParamStr(1)
  else
    Path := 'kayte';

  Path := ExpandFileName(Path);

  if not DirectoryExists(Path) then
  begin
    WriteLn('[build_kayte] Directory not found: ', Path);
    Halt(1);
  end;

  CompileAllInDir(Path);
end.

