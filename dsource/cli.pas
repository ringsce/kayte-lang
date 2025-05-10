unit CLI;

interface

uses
  SysUtils,
  Bytecode  in '..\..\dsource\Bytecode.pas',
  KayteParser  in '..\..\dsource\KayteParser.pas',
  VirtualMachine in '..\dsource\VirtualMachine.pas';

type
  TCLIOptions = record
    ShowHelp: Boolean;
    ShowVersion: Boolean;
    Verbose: Boolean;
    CompileKayte: Boolean;
    RunBytecode: Boolean;
    InputFile: string;
    OutputFile: string;
  end;

  TCLIHandler = class
  private
    FOptions: TCLIOptions;
    FAppName: string;
    FAppVersion: string;
    procedure ShowHelp;
    procedure ShowVersion;
    procedure CompileKayteFile(const InputFile, OutputFile: string);
    procedure RunBytecodeFile(const BytecodeFile: string);
  public
    constructor Create(const AppName, AppVersion: string);
    procedure ParseArgs;
    procedure Execute;
  end;

implementation

{ TCLIHandler }

constructor TCLIHandler.Create(const AppName, AppVersion: string);
begin
  FAppName := AppName;
  FAppVersion := AppVersion;
  FOptions.ShowHelp := False;
  FOptions.ShowVersion := False;
  FOptions.Verbose := False;
  FOptions.CompileKayte := False;
  FOptions.RunBytecode := False;
  FOptions.InputFile := '';
  FOptions.OutputFile := '';
end;

procedure TCLIHandler.ShowHelp;
begin
  Writeln('Usage: ', FAppName, ' [OPTIONS] [FILE]');
  Writeln;
  Writeln('Options:');
  Writeln('  --help             Show this help message and exit');
  Writeln('  -v, --version      Show the version information and exit');
  Writeln('  --verbose          Run in verbose mode');
  Writeln('  --compile <file>   Compile a .kayte source file to bytecode');
  Writeln('  --run <file>       Run a bytecode (.bytecode) file');
  Writeln('  -o <file>          Specify the output bytecode file when compiling');
  Writeln;
end;

procedure TCLIHandler.ShowVersion;
begin
  Writeln(FAppName, ' version ', FAppVersion);
end;

procedure TCLIHandler.CompileKayteFile(const InputFile, OutputFile: string);
var
  BytecodeGen: TBytecodeGenerator;
begin
  if InputFile = '' then
  begin
    Writeln('Error: No input file specified for compilation.');
    Exit;
  end;

  if OutputFile = '' then
  begin
    Writeln('Error: No output file specified for bytecode.');
    Exit;
  end;

  BytecodeGen := TBytecodeGenerator.Create;
  try
    if FOptions.Verbose then
      Writeln('Compiling ', InputFile, ' to ', OutputFile);

    BytecodeGen.GenerateBytecode(InputFile, OutputFile);

    if FOptions.Verbose then
      Writeln('Compilation successful.');
  finally
    BytecodeGen.Free;
  end;
end;

procedure TCLIHandler.RunBytecodeFile(const BytecodeFile: string);
var
  VM: TVirtualMachine;
begin
  // Check if the bytecode file is provided and exists
  if BytecodeFile = '' then
  begin
    Writeln('Error: No bytecode file specified to run.');
    Exit;
  end
  else if not FileExists(BytecodeFile) then
  begin
    Writeln('Error: Bytecode file not found: ', BytecodeFile);
    Exit;
  end;

  VM := TVirtualMachine.Create;
  try
    if FOptions.Verbose then
      Writeln('Loading bytecode file: ', BytecodeFile);

    // Attempt to load the bytecode file into the VM
    try
      VM.LoadBytecode(BytecodeFile);
    except
      on E: Exception do
      begin
        Writeln('Error: Failed to load bytecode: ', E.Message);
        Exit;
      end;
    end;

    if FOptions.Verbose then
      Writeln('Executing bytecode...');

    // Run the virtual machine with the loaded bytecode
    try
      VM.Run;
    except
      on E: Exception do
      begin
        Writeln('Error: Execution failed: ', E.Message);
        Exit;
      end;
    end;

    if FOptions.Verbose then
      Writeln('Execution completed successfully.');
  finally
    VM.Free;
  end;
end;

procedure TCLIHandler.ParseArgs;
var
  I: Integer;
begin
  I := 1;
  while I <= ParamCount do
  begin
    if ParamStr(I) = '--help' then
      FOptions.ShowHelp := True
    else if (ParamStr(I) = '-v') or (ParamStr(I) = '--version') then
      FOptions.ShowVersion := True
    else if ParamStr(I) = '--verbose' then
      FOptions.Verbose := True
    else if ParamStr(I) = '--compile' then
    begin
      FOptions.CompileKayte := True;
      if I + 1 <= ParamCount then
      begin
        Inc(I);
        FOptions.InputFile := ParamStr(I);
      end
      else
        Writeln('Error: No input file specified for --compile.');
    end
    else if ParamStr(I) = '--run' then
    begin
      FOptions.RunBytecode := True;
      if I + 1 <= ParamCount then
      begin
        Inc(I);
        FOptions.InputFile := ParamStr(I);
      end
      else
        Writeln('Error: No bytecode file specified for --run.');
    end
    else if ParamStr(I) = '-o' then
    begin
      if I + 1 <= ParamCount then
      begin
        Inc(I);
        FOptions.OutputFile := ParamStr(I);
      end
      else
        Writeln('Error: No output file specified for -o.');
    end
    else
      Writeln('Unknown option: ', ParamStr(I));

    Inc(I);
  end;
end;

procedure TCLIHandler.Execute;
begin
  if FOptions.ShowHelp then
  begin
    ShowHelp;
    Exit;
  end;

  if FOptions.ShowVersion then
  begin
    ShowVersion;
    Exit;
  end;

  if FOptions.CompileKayte then
  begin
    CompileKayteFile(FOptions.InputFile, FOptions.OutputFile);
    Exit;
  end;

  if FOptions.RunBytecode then
  begin
    RunBytecodeFile(FOptions.InputFile);
    Exit;
  end;

  if FOptions.Verbose then
  begin
    Writeln('No action specified. Use --help to see available options.');
  end;
end;

end.

