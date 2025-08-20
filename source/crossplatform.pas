unit CrossPlatform;

{$mode objfpc} // Recommended for cross-platform compatibility with FPC
{$H+} // Enable {$H+} for FPC to use ansistring/string compatibility

interface

uses
  SysUtils, Classes;

// A function to handle platform-specific file paths.
function GetUserHomePath: string;

// A function to show compiler-specific integer and pointer sizes.
procedure PrintArchitectureInfo;

// A function to handle platform-specific string conversions.
procedure ProcessStrings;

procedure PrintCompilerInfo;

procedure LogDebugInfo(const S: string);

implementation

// The implementation section contains the code for the procedures and functions declared in the interface.

function GetUserHomePath: string;
begin
  // On Windows, the home directory is often stored in the HOMEPATH env var.
  {$IFDEF MSWINDOWS}
  Result := GetEnvironmentVariable('HOMEPATH');
  {$ELSE}
  // On Linux and macOS, it's typically in the HOME env var.
  Result := GetEnvironmentVariable('HOME');
  {$ENDIF}
end;

procedure PrintArchitectureInfo;
begin
  WriteLn('--- Architecture Information ---');
  // Check for 64-bit platforms
  {$IFDEF CPUX64}
  WriteLn('This is a 64-bit architecture.');
  // PtrInt and PtrUInt are cross-platform types for storing pointers.
  WriteLn('Size of PtrInt: ' + IntToStr(SizeOf(PtrInt)) + ' bytes.');
  {$ELSE}
  // Assume 32-bit otherwise
  WriteLn('This is a 32-bit architecture.');
  WriteLn('Size of Pointer: ' + IntToStr(SizeOf(Pointer)) + ' bytes.');
  {$ENDIF}
  WriteLn('');
end;

procedure ProcessStrings;
var
  S: string;
begin
  WriteLn('--- String Processing ---');
  S := 'Hello, world!';
  // Delphi's standard string is Unicode (AnsiString on older versions),
  // while FPC defaults to AnsiString.
  {$IFDEF DELPHI}
  // We can use WideString for explicit UTF-16 on Delphi
  WriteLn('Delphi-specific string processing (Unicode assumed): ' + S);
  {$ENDIF}
  {$IFDEF FPC}
  // FPC has different functions for string handling depending on the version and mode.
  WriteLn('FPC-specific string processing (AnsiString assumed): ' + S);
  {$ENDIF}
  WriteLn('');
end;

procedure PrintCompilerInfo;
begin
  WriteLn('--- Compiler Information ---');
  // Check if we are running the Free Pascal Compiler
  {$IFDEF FPC}
  WriteLn('This code is being compiled by Free Pascal.');
  {$ELSEIF DEFINED(DELPHI)} // Use DEFINED(DELPHI) for modern Delphi
  WriteLn('This code is being compiled by Delphi.');
  {$ELSE}
  WriteLn('This code is being compiled by an unknown compiler.');
  {$ENDIF}
  WriteLn('');
end;

procedure LogDebugInfo(const S: string);
begin
  WriteLn('--- Debug Logging ---');
  // This shows a more advanced use case where you might use a different
  // logging mechanism or procedure depending on the compiler and platform.
  {$IFDEF FPC}
    {$IFDEF MSWINDOWS}
    // FPC on Windows-specific code
    WriteLn('FPC Windows Log: ' + S);
    {$ELSE}
    // FPC on other OSes
    WriteLn('FPC Log: ' + S);
    {$ENDIF}
  {$ELSEIF DEFINED(DELPHI)}
    {$IFDEF MSWINDOWS}
    // Delphi on Windows-specific code
    OutputDebugString(PChar(S)); // Uses a Windows API function
    {$ELSE}
    // Delphi on other OSes (e.g., macOS, Linux)
    WriteLn('Delphi Log: ' + S);
    {$ENDIF}
  {$ENDIF}
  WriteLn('');
end;

end.

