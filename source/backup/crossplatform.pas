// Program that uses conditional compilation for different compilers
program CrossPlatform;

// In some cases, FPC needs a specific unit for console I/O,
// while Delphi often doesn't need an explicit 'uses' clause for this.
{$IFDEF FPC}
uses
  SysUtils;
{$ENDIF}

procedure PrintCompilerInfo;
begin
  // Check if we are running the Free Pascal Compiler
  {$IFDEF FPC}
  WriteLn('This code is being compiled by Free Pascal.');
  {$ELSEIF DEFINED(DELPHI)} // Use DEFINED(DELPHI) for modern Delphi
  WriteLn('This code is being compiled by Delphi.');
  {$ELSE}
  WriteLn('This code is being compiled by an unknown compiler.');
  {$ENDIF}
end;

procedure LogDebugInfo(const S: string);
begin
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
end;

begin
  PrintCompilerInfo;
  LogDebugInfo('This is a debug message.');
  ReadLn;
end.

