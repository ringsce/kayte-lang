unit kayte_loader;

{$mode objfpc}{$H+}

interface

procedure Kayte_LoadAllFromDir(const Dir: string);
procedure Kayte_RunAll;

implementation

uses
  SysUtils, Process, kayte_vm;

procedure CompileIfNeeded(const KayteFile, BinFile: string);
var
  SrcTime, BinTime: TDateTime;
begin
  if not FileExists(BinFile) then
  begin
    WriteLn('[kayte_loader] Compiling ', KayteFile, ' → ', BinFile);
    RunCommand('kaytec', [KayteFile, '-o', BinFile], []);
    Exit;
  end;

  SrcTime := FileAge(KayteFile);
  BinTime := FileAge(BinFile);

  if SrcTime > BinTime then
  begin
    WriteLn('[kayte_loader] Recompiling ', KayteFile, ' (source is newer)');
    RunCommand('kaytec', [KayteFile, '-o', BinFile], []);
  end;
end;

procedure LoadBinaryToVM(const Name, BinFile: string);
var
  Stream: TFileStream;
  Buffer: string;
begin
  Stream := TFileStream.Create(BinFile, fmOpenRead);
  try
    SetLength(Buffer, Stream.Size);
    Stream.ReadBuffer(Pointer(Buffer)^, Stream.Size);
    KayteVM_LoadModule(Name, Buffer);
  finally
    Stream.Free;
  end;
end;

procedure Kayte_LoadAllFromDir(const Dir: string);
var
  SR: TSearchRec;
  KayteFile, BinFile, ModName: string;
begin
  if FindFirst(IncludeTrailingPathDelimiter(Dir) + '*.kayte', faAnyFile, SR) = 0 then
  repeat
    KayteFile := IncludeTrailingPathDelimiter(Dir) + SR.Name;
    ModName := ChangeFileExt(SR.Name, '');
    BinFile := IncludeTrailingPathDelimiter(Dir) + ModName + '.bin';

    CompileIfNeeded(KayteFile, BinFile);
    LoadBinaryToVM(ModName, BinFile);
  until FindNext(SR) <> 0;
  FindClose(SR);
end;

procedure Kayte_RunAll;
begin
  KayteVM_RunAll;
end;

end.

