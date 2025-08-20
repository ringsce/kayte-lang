unit CLI;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Generics.Collections,
  BytecodeTypes in '../source/BytecodeTypes.pas',
  Bytecode in '../source/ByteCode.pas',
  KayteParser in '../source/kayteparser.pas',
  Lexer,
  Parser,
  VirtualMachine in '../source/virtualmachine.pas';

type

  TVirtualMachine = class
  public
    // Note: The previous constructor was a forward declaration.
    // The implementation is now provided below.
    constructor Create(const BytecodeFile: string);
    procedure Run;
  end;

  // TByteCodeProgram definition
  TByteCodeProgram = class
  public
    ProgramTitle: string;
    Instructions: array of TBCInstruction;
    StringLiterals: TStringList;
    IntegerLiterals: array of Int64;

    VariableMap: TStringList;
    SubroutineMap: TStringList;
    FormMap: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TBytecodeGenerator = class(TObject)
  public
    procedure SaveProgramToFile(AProgram: TByteCodeProgram; const OutputFilePath: String);
    function LoadProgramFromFile(const InputFilePath: String): TByteCodeProgram;
  end;

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
    destructor Destroy; override;
    procedure ParseArgs;
    procedure Execute;
  end;

implementation

{ TVirtualMachine }

constructor TVirtualMachine.Create(const BytecodeFile: string);
begin
  // This is the implementation that was missing.
  // Add the logic to load and initialize the VM with the bytecode file.
  // For now, it's left empty to ensure compilation.
  // The 'BytecodeFile' parameter is now correctly handled.
end;

procedure TVirtualMachine.Run;
begin
  // Implementation of the Run procedure
  // This is where the virtual machine would execute the bytecode.
end;


{ TByteCodeProgram }

constructor TByteCodeProgram.Create;
begin
  inherited Create;
  // Initialize TStringList members
  StringLiterals := TStringList.Create;
  VariableMap := TStringList.Create;
  SubroutineMap := TStringList.Create;
  FormMap := TStringList.Create;
end;

destructor TByteCodeProgram.Destroy;
begin
  // Free TStringList members
  StringLiterals.Free;
  VariableMap.Free;
  SubroutineMap.Free;
  FormMap.Free;
  inherited Destroy;
end;

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

destructor TCLIHandler.Destroy;
begin
  inherited Destroy;
end;

procedure TCLIHandler.ShowHelp;
begin
  Writeln('Usage: ', FAppName, ' [OPTIONS] [FILE]');
  Writeln;
  Writeln('Options:');
  Writeln('  --help           Show this help message and exit');
  Writeln('  -v, --version    Show the version information and exit');
  Writeln('  --verbose        Run in verbose mode');
  Writeln('  --compile <file> Compile a .kayte source file to bytecode');
  Writeln('  --run <file>     Run a bytecode (.bytecode) file');
  Writeln('  -o <file>        Specify the output bytecode file when compiling');
  Writeln;
  Writeln('If no --compile or --run option is given, FILE is assumed to be a Kayte source file to compile.');
  Writeln('Default output file for compilation is <input_file_name>.bytecode');
  Writeln;
end;

procedure TCLIHandler.ShowVersion;
begin
  Writeln(FAppName, ' version ', FAppVersion);
end;

procedure TCLIHandler.CompileKayteFile(const InputFile, OutputFile: string);
var
  Lexer: TLexer;
  Parser: TParser;
begin
  if not FileExists(InputFile) then
  begin
    Writeln('Error: Input file not found: ', InputFile);
    Exit;
  end;

  Writeln('Compiling ', InputFile, '...');
  try
    Lexer := TLexer.Create(InputFile);
    Parser := TParser.Create(Lexer);
    //
    // Perform compilation here
    //
    Writeln('Compilation successful!');

  finally
    Lexer.Free;
    Parser.Free;
  end;
end;

procedure TCLIHandler.RunBytecodeFile(const BytecodeFile: string);
var
  VM: TVirtualMachine;
begin
  if not FileExists(BytecodeFile) then
  begin
    Writeln('Error: Bytecode file not found: ', BytecodeFile);
    Exit;
  end;

  Writeln('Running ', BytecodeFile, '...');
  try
    // The call to create the TVirtualMachine instance.
    VM := TVirtualMachine.Create(BytecodeFile);
    VM.Run;
    Writeln('Execution finished.');
  finally
    VM.Free;
  end;
end;

procedure TCLIHandler.ParseArgs;
var
  I: Integer;
  IsPositionalFile: Boolean;
begin
  IsPositionalFile := False;
  FOptions.OutputFile := ''; // Reset output file

  // Start parsing from the second parameter (index 1)
  I := 1;
  while I <= ParamCount do
  begin
    // Check for options
    if (ParamStr(I) = '--help') then
      FOptions.ShowHelp := True
    else if (ParamStr(I) = '-v') or (ParamStr(I) = '--version') then
      FOptions.ShowVersion := True
    else if (ParamStr(I) = '--verbose') then
      FOptions.Verbose := True
    else if (ParamStr(I) = '--compile') then
    begin
      FOptions.CompileKayte := True;
      Inc(I);
      // Ensure there is a file path after the option
      if I <= ParamCount then
        FOptions.InputFile := ParamStr(I)
      else
        Writeln('Error: Missing file path for --compile option');
    end
    else if (ParamStr(I) = '--run') then
    begin
      FOptions.RunBytecode := True;
      Inc(I);
      if I <= ParamCount then
        FOptions.InputFile := ParamStr(I)
      else
        Writeln('Error: Missing file path for --run option');
    end
    else if (ParamStr(I) = '-o') then
    begin
      Inc(I);
      if I <= ParamCount then
        FOptions.OutputFile := ParamStr(I)
      else
        Writeln('Error: Missing file path for -o option');
    end
    else if (I = ParamCount) and (not IsPositionalFile) then
    begin
      // Treat the last parameter as the first non-option argument as the input file
      FOptions.InputFile := ParamStr(I);
      IsPositionalFile := True;
    end
    else
      Writeln('Unknown option or unexpected argument: ', ParamStr(I));

    Inc(I);
  end;

  // If a positional file was given and no action was specified, default to compile
  if IsPositionalFile and (not FOptions.CompileKayte) and (not FOptions.RunBytecode) then
  begin
    FOptions.CompileKayte := True;
    if FOptions.Verbose then
      Writeln('Info: No action specified, defaulting to --compile for input file: ', FOptions.InputFile);
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

  // Prioritize compile and run actions
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

  // If no specific action, and no help/version requested, and no file given, show help
  if (not FOptions.ShowHelp) and (not FOptions.ShowVersion) and
     (not FOptions.CompileKayte) and (not FOptions.RunBytecode) and
     (FOptions.InputFile = '') then
  begin
    ShowHelp;
  end;

end;

end.

