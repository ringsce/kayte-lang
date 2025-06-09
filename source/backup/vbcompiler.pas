unit VBCompiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process; // Process unit for executing external commands

type
  TVBCompilerResult = record
    Success: Boolean;
    Output: TStringList; // Capture compiler's stdout and stderr
    ErrorMessage: String; // Summary of errors if compilation fails
  end;

  // Function to compile the interpreter project into a shared library (DLL/SO)
  function CompileToSharedLibrary(
    const ProjectSourceFile: String; // e.g., 'vb6interpreter.lpr' or 'your_main_unit.pas'
    const OutputDir: String;        // Directory for the compiled output
    const TargetName: String        // Desired name of the output file (e.g., 'vb6interpreter')
  ): TVBCompilerResult;

  // Function to compile a unit into a static library (.a)
  function CompileToStaticLibrary(
    const UnitSourceFile: String;   // e.g., 'interpretercore.pas'
    const OutputDir: String;        // Directory for the compiled output
    const TargetName: String        // Desired name of the output file (e.g., 'interpretercore')
  ): TVBCompilerResult;

implementation

function ExecuteCommand(const Command: String; const Args: TStringArray): TVBCompilerResult;
var
  Proc: TProcess;
  OutputList: TStringList;
  I: Integer;
begin
  Result.Success := False;
  Result.Output := TStringList.Create;
  Result.ErrorMessage := '';

  Proc := TProcess.Create(nil);
  OutputList := TStringList.Create;
  try
    Proc.CommandLine := Command;
    for I := Low(Args) to High(Args) do
      Proc.Parameters.Add(Args[I]);

    Proc.Options := [poWaitOnExit, poUsePipes];
    Proc.Execute;

    Proc.Output.SetLength(Proc.Output.Size);
    OutputList.LoadFromStream(Proc.Output);

    // Capture stderr as well, as compiler errors often go there
    Proc.Stderr.SetLength(Proc.Stderr.Size);
    OutputList.LoadFromStream(Proc.Stderr);

    Result.Output.AddStrings(OutputList);

    if Proc.ExitStatus = 0 then
      Result.Success := True
    else
    begin
      Result.Success := False;
      Result.ErrorMessage := 'Compiler exited with error code ' + IntToStr(Proc.ExitStatus);
      // Try to find first few error lines
      for I := 0 to min(OutputList.Count - 1, 5) do // Look at first 5 lines
      begin
        if (Pos('Error:', OutputList[I]) > 0) or (Pos('Fatal:', OutputList[I]) > 0) then
        begin
          Result.ErrorMessage := Result.ErrorMessage + #13#10 + 'Details: ' + OutputList[I];
          Break;
        end;
      end;
    end;
  finally
    OutputList.Free;
    Proc.Free;
  end;
end;

function CompileToSharedLibrary(
  const ProjectSourceFile: String;
  const OutputDir: String;
  const TargetName: String
): TVBCompilerResult;
var
  CompilerPath: String;
  OutputFile: String;
  OutputSwitch: String;
  LibrarySwitch: String;
  TargetExt: String;
  CompilerArgs: TStringArray;
begin
  CompilerPath := 'fpc'; // Assumes fpc is in PATH

  {$IFDEF WINDOWS}
    TargetExt := '.dll';
    LibrarySwitch := '-WG'; // Generate DLL (Windows-specific for shared libraries)
  {$ENDIF}
  {$IFDEF LINUX}
    TargetExt := '.so';
    LibrarySwitch := '-WG'; // Generate Shared Object (Linux/Unix)
  {$ENDIF}
  {$IFDEF DARWIN} // macOS (formerly OSX)
    TargetExt := '.dylib'; // Standard extension for shared libraries on macOS
    LibrarySwitch := '-WG'; // Generate Shared Object (macOS)
  {$ENDIF}
  {$IFDEF UNIX} // Generic Unix-like, includes Linux and Darwin if not specifically handled
    // Fallback if not caught by LINUX/DARWIN specific defs
    TargetExt := '.so';
    LibrarySwitch := '-WG';
  {$ENDIF}

  // Ensure TargetExt is set, fallback if no IFDEF matched (unlikely for common OS)
  if TargetExt = '' then
  begin
    Result.Success := False;
    Result.ErrorMessage := 'Unsupported operating system for shared library compilation.';
    Exit;
  end;

  OutputFile := IncludeTrailingPathDelimiter(OutputDir) + TargetName + TargetExt;
  OutputSwitch := '-o' + OutputFile;

  // Common compiler arguments for shared libraries
  CompilerArgs := [
    LibrarySwitch,        // Generate shared library/DLL
    '-Fu' + IncludeTrailingPathDelimiter(OutputDir), // Add output directory to unit path
    '-FE' + IncludeTrailingPathDelimiter(OutputDir), // Set executable/library output directory
    OutputSwitch,
    ProjectSourceFile     // The main project file (e.g., .lpr or .pas)
    // Add other relevant options like -CX for cross-platform, -Cp for CPU etc.
    // '-CX', // Creates linkable units (useful for shared libraries)
    // '-XX', // Generate smart-linking .so/.dylib (reduces size)
    // '-WM1', // MacOS specific: use system libraries directly
    // '-gw', // Generate DWARF debug info
    // '-O1', // Optimization level
    // '-vboh', // Verbose output
    // '-Sd', // Delphi compatibility mode (if helpful)
    // '-Twin32', // Target windows 32 (for cross-compiling)
    // '-Tlinux', // Target linux
    // '-Tdarwin' // Target macOS
  ];

  Result := ExecuteCommand(CompilerPath, CompilerArgs);
end;

function CompileToStaticLibrary(
  const UnitSourceFile: String;
  const OutputDir: String;
  const TargetName: String
): TVBCompilerResult;
var
  CompilerPath: String;
  OutputFile: String;
  OutputSwitch: String;
  LibrarySwitch: String;
  TargetExt: String;
  CompilerArgs: TStringArray;
begin
  CompilerPath := 'fpc'; // Assumes fpc is in PATH

  {$IFDEF WINDOWS}
    TargetExt := '.lib'; // .lib for MSVC-compatible import libraries or static libraries on Windows
    LibrarySwitch := '-WM'; // Compile as module (for creating .obj/.o later for static lib)
    // For a true .a static library on Windows, you'd usually use a linker after .obj files
    // But FPC can generate .obj files for other linkers.
    // If you specifically want a .a, you'd target MinGW (e.g., -Twin32 -WG -XPmingw)
    // or manually run ar.exe
  {$ENDIF}
  {$IFDEF LINUX}
    TargetExt := '.a';
    LibrarySwitch := '-WM'; // Compile as module (produces .o which ar uses)
  {$ENDIF}
  {$IFDEF DARWIN} // macOS
    TargetExt := '.a';
    LibrarySwitch := '-WM'; // Compile as module
  {$ENDIF}
  {$IFDEF UNIX} // Generic Unix-like
    TargetExt := '.a';
    LibrarySwitch := '-WM';
  {$ENDIF}

  // Ensure TargetExt is set
  if TargetExt = '' then
  begin
    Result.Success := False;
    Result.ErrorMessage := 'Unsupported operating system for static library compilation.';
    Exit;
  end;

  OutputFile := IncludeTrailingPathDelimiter(OutputDir) + TargetName + TargetExt;
  OutputSwitch := '-o' + OutputFile;

  // For static libraries, FPC typically compiles to .o (object) files, then ar combines them.
  // Using -WM will produce .o files. To get a .a directly, you might need a different approach
  // or a post-build step with `ar`.
  // For simplicity, this will just compile the unit. If you need a combined .a,
  // you'd typically compile all units to .o then use 'ar rcs libname.a obj1.o obj2.o'.
  // FPC's -WL-a usually is for the linker, not the compiler producing .a.
  // Let's assume -WM is sufficient to get object files for later static linking.
  // If you want a combined .a, you would need a loop over all units and then `ar` call.

  CompilerArgs := [
    LibrarySwitch,        // Compile as module (produces .o)
    '-Fu' + IncludeTrailingPathDelimiter(OutputDir), // Add output directory to unit path
    '-FE' + IncludeTrailingPathDelimiter(OutputDir), // Set output directory for .ppu files etc.
    // For static libraries, the -o switch might just specify the .o file, not the .a
    // You'd usually link multiple .o files into a .a using 'ar'
    // This function focuses on compiling a single unit to an object file (.o or .obj)
    OutputSwitch, // This will make FPC name the .o file according to TargetName.o
    UnitSourceFile
  ];

  Result := ExecuteCommand(CompilerPath, CompilerArgs);

  // For static libraries, you typically compile multiple .o files and then use 'ar'
  // to combine them into a single .a archive. FPC itself doesn't directly output .a
  // from a single unit compilation usually without extra linker flags.
  // This part would need refinement if you want a complete .a automation.
  if Result.Success then
  begin
    {$IFDEF LINUX}
    {$IFDEF DARWIN}
    {$IFDEF UNIX}
      // If we compiled successfully to an .o file, try to archive it into a .a
      // This is a simplified example; a real static library might involve many .o files.
      // Assumes 'ar' is in PATH.
      Result := ExecuteCommand('ar', ['rcs', OutputFile, IncludeTrailingPathDelimiter(OutputDir) + TargetName + '.o']);
      if not Result.Success then
        Result.ErrorMessage := 'Failed to create static archive (.a) using ar.';
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}
  end;
end;

end.
