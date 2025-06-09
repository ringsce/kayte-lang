unit VBCompiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Math; // Added Math unit for min function

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
    Proc.Executable := Command; // Use Executable for the command itself
    for I := Low(Args) to High(Args) do
      Proc.Parameters.Add(Args[I]); // Add arguments individually

    Proc.Options := [poWaitOnExit, poUsePipes];
    Proc.Execute;

    // Load from streams directly, no need for SetLength
    OutputList.LoadFromStream(Proc.Output);
    OutputList.LoadFromStream(Proc.Stderr); // Capture stderr as well

    Result.Output.AddStrings(OutputList);

    if Proc.ExitStatus = 0 then
      Result.Success := True
    else
    begin
      Result.Success := False;
      Result.ErrorMessage := 'Compiler exited with error code ' + IntToStr(Proc.ExitStatus);
      // Try to find first few error lines
      for I := 0 to Math.Min(OutputList.Count - 1, 5) do // Use Math.Min
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
  OutputObjFile: String; // To store the .o or .obj file path
  OutputSwitch: String;
  LibrarySwitch: String;
  TargetExt: String;
  CompilerArgs: TStringArray;
begin
  Result.Success := False; // Initialize Result
  Result.Output := TStringList.Create;
  Result.ErrorMessage := '';

  CompilerPath := 'fpc'; // Assumes fpc is in PATH

  {$IFDEF WINDOWS}
    TargetExt := '.lib'; // .lib for MSVC-compatible import libraries or static libraries on Windows
    LibrarySwitch := '-WM'; // Compile as module (for creating .obj/.o later for static lib)
    OutputObjFile := IncludeTrailingPathDelimiter(OutputDir) + TargetName + '.obj';
  {$ENDIF}
  {$IFDEF LINUX}
    TargetExt := '.a';
    LibrarySwitch := '-WM'; // Compile as module (produces .o which ar uses)
    OutputObjFile := IncludeTrailingPathDelimiter(OutputDir) + TargetName + '.o';
  {$ENDIF}
  {$IFDEF DARWIN} // macOS
    TargetExt := '.a';
    LibrarySwitch := '-WM'; // Compile as module
    OutputObjFile := IncludeTrailingPathDelimiter(OutputDir) + TargetName + '.o';
  {$ENDIF}
  {$IFDEF UNIX} // Generic Unix-like
    TargetExt := '.a';
    LibrarySwitch := '-WM';
    OutputObjFile := IncludeTrailingPathDelimiter(OutputDir) + TargetName + '.o';
  {$ENDIF}

  // Ensure TargetExt is set
  if TargetExt = '' then
  begin
    Result.Success := False;
    Result.ErrorMessage := 'Unsupported operating system for static library compilation.';
    Exit;
  end;

  // This will make FPC produce the object file (e.g., .o or .obj)
  OutputSwitch := '-o' + OutputObjFile;

  CompilerArgs := [
    LibrarySwitch,        // Compile as module (produces .o/.obj)
    '-Fu' + IncludeTrailingPathDelimiter(OutputDir), // Add output directory to unit path
    '-FE' + IncludeTrailingPathDelimiter(OutputDir), // Set output directory for .ppu files etc.
    OutputSwitch, // Direct FPC to output the object file here
    UnitSourceFile
  ];

  Result := ExecuteCommand(CompilerPath, CompilerArgs);

  // If compilation to object file was successful, try to archive it into a .a
  if Result.Success then
  begin
    {$IFDEF LINUX}
    {$IFDEF DARWIN}
    {$IFDEF UNIX}
      // Assuming 'ar' is in PATH for Unix-like systems
      // This step combines the .o file into a .a archive.
      // OutputFile will be the actual .a file path now.
      OutputFile := IncludeTrailingPathDelimiter(OutputDir) + TargetName + TargetExt;
      Result := ExecuteCommand('ar', ['rcs', OutputFile, OutputObjFile]);
      if not Result.Success then
        Result.ErrorMessage := 'Failed to create static archive (.a) using ar.';
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}
    {$IFDEF WINDOWS}
            // On Windows, if you want a .a, you typically use a MinGW-compatible ar.exe
            // If targeting MSVC, you usually use a .lib.
            // This section is left for you to implement if you need a specific Windows static archive tool.
            // For now, it just produces the .obj file.

      case StaticLibFormat of
        slfMSVC:
        begin
          // For MSVC, use 'lib.exe' (part of Visual Studio's Build Tools)
          // You need to ensure 'lib.exe' is in the system's PATH, or provide its full path.
          if FileExists(OutputObjFile) then
          begin
            LinkerArgs := ['/OUT:' + FinalLibFile, OutputObjFile];
            Result := ExecuteCommand('lib.exe', LinkerArgs); // Assuming lib.exe is in PATH
            if not Result.Success then
              Result.ErrorMessage := 'Failed to create static library (.lib) using lib.exe.';
          end
          else
          begin
            Result.Success := False;
            Result.ErrorMessage := 'FPC did not produce the expected object file for MSVC: ' + OutputObjFile;
          end;
        end;
        slfMinGW, slfAuto:
        begin
          // For MinGW, use 'ar.exe' (part of MinGW/MSYS2 distribution)
          // You need to ensure 'ar.exe' is in the system's PATH, or provide its full path.
          if FileExists(OutputObjFile) then
          begin
            LinkerArgs := ['rcs', FinalLibFile, OutputObjFile];
            Result := ExecuteCommand('ar.exe', LinkerArgs); // Assuming ar.exe is in PATH
            if not Result.Success then
              Result.ErrorMessage := 'Failed to create static archive (.a) using ar.exe.';
          end
          else
          begin
            Result.Success := False;
            Result.ErrorMessage := 'FPC did not produce the expected object file for MinGW: ' + OutputObjFile;
          end;
        end;
      end;
      {$ENDIF}
  end;
end;

end.
