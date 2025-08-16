unit kayte_loader;

{$mode objfpc}{$H+}

interface

procedure Kayte_LoadAllFromResources;
procedure Kayte_RunAll;

implementation

uses
  SysUtils, Classes, Process, kayte_vm;

type
  { TCompileTask: Compiles a .kayte file to .bin asynchronously }
  TCompileTask = class(TThread)
  private
    FSrcFile, FOutFile: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const Src, OutFile: string);
  end;

constructor TCompileTask.Create(const Src, OutFile: string);
begin
  inherited Create(False); // auto-start
  FSrcFile := Src;
  FOutFile := OutFile;
  FreeOnTerminate := True;
end;

// Generator
procedure TBytecodeGenerator.SaveProgramToFile(AProgram: TByteCodeProgram; const OutputFilePath: String);
var
  FileStream: TFileStream;
  Len: LongInt;
  I: Integer;
  Key: AnsiString;
  Value: LongInt;
  LObject: Pointer;
begin
  FileStream := TFileStream.Create(OutputFilePath, fmCreate);
  try
    // --- Full Serialization of TByteCodeProgram ---

    // 1. Write ProgramTitle
    Len := Length(AProgram.ProgramTitle);
    FileStream.Write(Len, SizeOf(Len));
    if Len > 0 then
      FileStream.Write(AProgram.ProgramTitle[1], Len);

    // 2. Write Instructions
    Len := Length(AProgram.Instructions);
    FileStream.Write(Len, SizeOf(Len));
    if Len > 0 then
      FileStream.Write(AProgram.Instructions[0], Len * SizeOf(TBCInstruction));

    // 3. Write StringLiterals
    Len := AProgram.StringLiterals.Count;
    FileStream.Write(Len, SizeOf(Len));
    for I := 0 to Len - 1 do
    begin
      Key := AProgram.StringLiterals[I];
      Value := Length(Key);
      FileStream.Write(Value, SizeOf(Value));
      if Value > 0 then
        FileStream.Write(Key[1], Value);
    end;

    // 4. Write IntegerLiterals
    Len := Length(AProgram.IntegerLiterals);
    FileStream.Write(Len, SizeOf(Len));
    if Len > 0 then
      FileStream.Write(AProgram.IntegerLiterals[0], Len * SizeOf(Int64));

    // 5. Write VariableMap (TStringList with Objects)
    Len := AProgram.VariableMap.Count;
    FileStream.Write(Len, SizeOf(Len));
    for I := 0 to Len - 1 do
    begin
      Key := AProgram.VariableMap.Names[I];
      LObject := AProgram.VariableMap.Objects[I];
      Value := LongInt(LObject);
      Len := Length(Key);
      FileStream.Write(Len, SizeOf(Len));
      if Len > 0 then
        FileStream.Write(Key[1], Len);
      FileStream.Write(Value, SizeOf(Value));
    end;

    // 6. Write SubroutineMap
    Len := AProgram.SubroutineMap.Count;
    FileStream.Write(Len, SizeOf(Len));
    for I := 0 to Len - 1 do
    begin
      Key := AProgram.SubroutineMap.Names[I];
      LObject := AProgram.SubroutineMap.Objects[I];
      Value := LongInt(LObject);
      Len := Length(Key);
      FileStream.Write(Len, SizeOf(Len));
      if Len > 0 then
        FileStream.Write(Key[1], Len);
      FileStream.Write(Value, SizeOf(Value));
    end;

    // 7. Write FormMap
    Len := AProgram.FormMap.Count;
    FileStream.Write(Len, SizeOf(Len));
    for I := 0 to Len - 1 do
    begin
      Key := AProgram.FormMap.Names[I];
      LObject := AProgram.FormMap.Objects[I];
      Value := LongInt(LObject);
      Len := Length(Key);
      FileStream.Write(Len, SizeOf(Len));
      if Len > 0 then
        FileStream.Write(Key[1], Len);
      FileStream.Write(Value, SizeOf(Value));
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TCompileTask.Execute;
var
  Output: string;
begin
  WriteLn('[kayte_loader] Compiling: ', FSrcFile);
  RunCommand('kaytec', [FSrcFile, '-o', FOutFile], Output);
end;

procedure CompileIfNeededAsync(const KayteFile, BinFile: string);
begin
  if (not FileExists(BinFile)) or (FileAge(KayteFile) > FileAge(BinFile)) then
    TCompileTask.Create(KayteFile, BinFile);
end;

procedure LoadBinaryToVM(const Name, BinFile: string);
var
  FS: TFileStream;
  Buffer: AnsiString;
begin
  FS := TFileStream.Create(BinFile, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Buffer, FS.Size);
    if FS.Size > 0 then
      FS.ReadBuffer(Buffer[1], FS.Size); // AnsiString is 1-based
    KayteVM_LoadModule(Name, Buffer);
  finally
    FS.Free;
  end;
end;

function AppResourcePath: string;
begin
  {$ifdef darwin}
  // macOS .app bundle structure
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) + '../../Resources/kayte/');
  {$else}
  Result := ExpandFileName('resources/kayte/');
  {$endif}
end;

procedure Kayte_LoadAllFromResources;
var
  SR: TSearchRec;
  KayteFile, BinFile, ModName, BasePath: string;
begin
  BasePath := AppResourcePath;
  if not DirectoryExists(BasePath) then
  begin
    WriteLn('[kayte_loader] ❌ Resource directory not found: ', BasePath);
    Exit;
  end;

  if FindFirst(BasePath + '*.kayte', faAnyFile, SR) = 0 then
  repeat
    KayteFile := BasePath + SR.Name;
    ModName := ChangeFileExt(SR.Name, '');
    BinFile := BasePath + ModName + '.bin';

    CompileIfNeededAsync(KayteFile, BinFile);
    LoadBinaryToVM(ModName, BinFile);
  until FindNext(SR) <> 0;
  FindClose(SR);
end;

procedure Kayte_RunAll;
begin
  KayteVM_RunAll;
end;

end.

