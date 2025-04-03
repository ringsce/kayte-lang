unit KayteLibLoader;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, dynlibs;

type
  TLoadMode = (lmSafe, lmUnsafe);

  { Function to load a library safely or unsafely }
  function LoadLibraryFile(const FilePath: string; LoadMode: TLoadMode): Boolean;

implementation

{ Function to validate the file extension }
function IsValidLibraryFile(const FilePath: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FilePath));
  Result := (Ext = '.dll') or (Ext = '.so') or (Ext = '.a');
end;

{ Function to load a library file based on mode }
function LoadLibraryFile(const FilePath: string; LoadMode: TLoadMode): Boolean;
var
  LibraryHandle: TLibHandle;
begin
  Result := False;

  if not FileExists(FilePath) then
  begin
    WriteLn('Error: File not found - ', FilePath);
    Exit;
  end;

  if (LoadMode = lmSafe) and not IsValidLibraryFile(FilePath) then
  begin
    WriteLn('Error: Invalid file extension for - ', FilePath);
    Exit;
  end;

  try
    LibraryHandle := LoadLibrary(FilePath);
    if LibraryHandle <> NilHandle then
    begin
      WriteLn('Library loaded successfully: ', FilePath);
      Result := True;
      // Remember to free the library after use
      FreeLibrary(LibraryHandle);
    end
    else
      WriteLn('Error: Failed to load library - ', FilePath);
  except
    on E: Exception do
      WriteLn('Error: Exception occurred while loading library - ', E.Message);
  end;
end;

end.

