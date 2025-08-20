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
  SimpleHTTPServer in '../source/SimpleHTTPServer.pas',
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
    StartHttpServer: Boolean; // <-- Add this line

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

{ TBytecodeGenerator }

function TBytecodeGenerator.LoadProgramFromFile(const InputFilePath: String): TByteCodeProgram;
var
  FileStream: TFileStream;
  Len: LongInt;
  I, KeyLength: Integer;
  Key: AnsiString;
  Value: Pointer;
  ProgramTitleBuffer: string;
begin
  Result := TByteCodeProgram.Create;

  FileStream := TFileStream.Create(InputFilePath, fmOpenRead or fmShareDenyWrite);
  try
    // --- Full Deserialization of TByteCodeProgram ---

    // 1. Read ProgramTitle
    FileStream.Read(Len, SizeOf(Len));
    SetLength(ProgramTitleBuffer, Len);
    if Len > 0 then
      FileStream.Read(ProgramTitleBuffer[1], Len);
    Result.ProgramTitle := ProgramTitleBuffer;

    // 2. Read Instructions
    FileStream.Read(Len, SizeOf(Len));
    SetLength(Result.Instructions, Len);
    if Len > 0 then
      FileStream.Read(Result.Instructions[0], Len * SizeOf(TBCInstruction));

    // 3. Read StringLiterals
    FileStream.Read(Len, SizeOf(Len));
    for I := 0 to Len - 1 do
    begin
      FileStream.Read(KeyLength, SizeOf(KeyLength));
      SetLength(Key, KeyLength);
      if KeyLength > 0 then
        FileStream.Read(Key[1], KeyLength);
      Result.StringLiterals.Add(Key);
    end;

    // 4. Read IntegerLiterals
    FileStream.Read(Len, SizeOf(Len));
    SetLength(Result.IntegerLiterals, Len);
    if Len > 0 then
      FileStream.Read(Result.IntegerLiterals[0], Len * SizeOf(Int64));

    // 5. Read VariableMap
    FileStream.Read(Len, SizeOf(Len));
    for I := 0 to Len - 1 do
    begin
      FileStream.Read(KeyLength, SizeOf(KeyLength));
      SetLength(Key, KeyLength);
      if KeyLength > 0 then
        FileStream.Read(Key[1], KeyLength);
      FileStream.Read(Value, SizeOf(Value));
      Result.VariableMap.AddObject(Key, TObject(Value));
    end;

    // 6. Read SubroutineMap
    FileStream.Read(Len, SizeOf(Len));
    for I := 0 to Len - 1 do
    begin
      FileStream.Read(KeyLength, SizeOf(KeyLength));
      SetLength(Key, KeyLength);
      if KeyLength > 0 then
        FileStream.Read(Key[1], KeyLength);
      FileStream.Read(Value, SizeOf(Value));
      Result.SubroutineMap.AddObject(Key, TObject(Value));
    end;

    // 7. Read FormMap
    FileStream.Read(Len, SizeOf(Len));
    for I := 0 to Len - 1 do
    begin
      FileStream.Read(KeyLength, SizeOf(KeyLength));
      SetLength(Key, KeyLength);
      if KeyLength > 0 then
        FileStream.Read(Key[1], KeyLength);
      FileStream.Read(Value, SizeOf(Value));
      Result.FormMap.AddObject(Key, TObject(Value));
    end;

  finally
    FileStream.Free;
  end;
end;

procedure TBytecodeGenerator.SaveProgramToFile(AProgram: TByteCodeProgram; const OutputFilePath: String);
var
  FileStream: TFileStream;
  Len: LongInt;
  I, KeyLength: Integer;
  Key: AnsiString;
  PtrVal: NativeInt; // CORRECTED: Moved the declaration here
begin
  FileStream := TFileStream.Create(OutputFilePath, fmCreate);
  try
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
    for I := 0 to AProgram.StringLiterals.Count - 1 do
    begin
      Key := AProgram.StringLiterals[I];
      KeyLength := Length(Key);
      FileStream.Write(KeyLength, SizeOf(KeyLength));
      if KeyLength > 0 then
        FileStream.Write(Key[1], KeyLength);
    end;

    // 4. Write IntegerLiterals
    Len := Length(AProgram.IntegerLiterals);
    FileStream.Write(Len, SizeOf(Len));
    if Len > 0 then
      FileStream.Write(AProgram.IntegerLiterals[0], Len * SizeOf(Int64));

    // 5. Write VariableMap
    Len := AProgram.VariableMap.Count;
    FileStream.Write(Len, SizeOf(Len));
    for I := 0 to AProgram.VariableMap.Count - 1 do
    begin
      Key := AProgram.VariableMap.Strings[I];
      KeyLength := Length(Key);
      FileStream.Write(KeyLength, SizeOf(KeyLength));
      if KeyLength > 0 then
        FileStream.Write(Key[1], KeyLength);

      PtrVal := NativeInt(AProgram.VariableMap.Objects[I]);
      FileStream.Write(PtrVal, SizeOf(PtrVal));
    end;

    // 6. Write SubroutineMap
    Len := AProgram.SubroutineMap.Count;
    FileStream.Write(Len, SizeOf(Len));
    for I := 0 to AProgram.SubroutineMap.Count - 1 do
    begin
      Key := AProgram.SubroutineMap.Strings[I];
      KeyLength := Length(Key);
      FileStream.Write(KeyLength, SizeOf(KeyLength));
      if KeyLength > 0 then
        FileStream.Write(Key[1], KeyLength);

      PtrVal := NativeInt(AProgram.SubroutineMap.Objects[I]);
      FileStream.Write(PtrVal, SizeOf(PtrVal));
    end;

    // 7. Write FormMap
    Len := AProgram.FormMap.Count;
    FileStream.Write(Len, SizeOf(Len));
    for I := 0 to AProgram.FormMap.Count - 1 do
    begin
      Key := AProgram.FormMap.Strings[I];
      KeyLength := Length(Key);
      FileStream.Write(KeyLength, SizeOf(KeyLength));
      if KeyLength > 0 then
        FileStream.Write(Key[1], KeyLength);

      PtrVal := NativeInt(AProgram.FormMap.Objects[I]);
      FileStream.Write(PtrVal, SizeOf(PtrVal));
    end;
  finally
    FileStream.Free;
  end;
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

(*
  Procedure: InitializeCLIHandler
  Description:
    This is the main entry point for the command-line interface. It creates an
    instance of TCLIHandler, parses the command-line arguments, and executes
    the appropriate action (e.g., compile, run, show help).

    It includes a robust try...finally block to ensure that the TCLIHandler
    object is always freed from memory, preventing resource leaks. The outer
    try...except block handles any exceptions that occur during execution,
    providing a clean, user-friendly error message.
*)


(*
  Procedure: TCLIHandler.ShowHelp
  Description:
    Displays the command-line tool's help message, including all available
    options and their descriptions. This procedure is a method of the
    TCLIHandler class.

  Changes:
    - Added the '--http' option to the help message.
*)
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
  Writeln('  --http           Starts a simple HTTP server'); // <-- New line added

  Writeln;
  Writeln('If no --compile or --run option is given, FILE is assumed to be a Kayte source file to compile.');
  Writeln('Default output file for compilation is <input_file_name>.bytecode');
  Writeln;
end;

(*
  Procedure: InitializeCLIHandler
  Description:
    This is the main entry point for the command-line interface. It creates an
    instance of TCLIHandler, parses the command-line arguments, and executes
    the appropriate action (e.g., compile, run, show help).

    It includes a robust try...finally block to ensure that the TCLIHandler
    object is always freed from memory, preventing resource leaks. The outer
    try...except block handles any exceptions that occur during execution,
    providing a clean, user-friendly error message.
*)
procedure InitializeCLIHandler;
var
  CLIHandler: TCLIHandler;
begin
  // Create an instance of the CLI handler class with application details.
  // 'kreatyveC' is the application name and '1.10.3' is the version number.
  CLIHandler := TCLIHandler.Create('kreatyveC', '1.10.3');
  try
    try
      Writeln('Parsing command-line arguments...');
      // Call the method to parse the arguments.
      CLIHandler.ParseArgs;

      Writeln('Executing command...');
      // Call the method to execute the command.
      CLIHandler.Execute;

      Writeln('CLI command executed successfully.');
    except
      on E: Exception do
      begin
        // Handle any exceptions that occur during parsing or execution.
        Writeln('Error while handling CLI: ', E.Message);
        Writeln('Usage: kc [options]');
        Writeln('Try "kc --help" for more information.');
        Exit; // Exit the procedure after displaying the error.
      end;
    end;
  finally
    // Ensure the CLIHandler object is properly destroyed, regardless of
    // whether an error occurred.
    CLIHandler.Free;
  end;
end;


procedure TCLIHandler.ShowVersion;
begin
  Writeln(FAppName, ' version ', FAppVersion);
end;

(* StartHTTPServer *)
procedure StartHTTPServer;
var
  Port: Integer;
  Server: TSimpleHTTPServer;
  StopSignal: Boolean; // Flag to handle stopping the server
begin
  Port := 9090; // Default port
  StopSignal := False; // Initialize StopSignal

  Writeln('Starting Kings server on port ', Port, '...');

  if ParamCount > 0 then
  begin
    try
      Port := StrToInt(ParamStr(1)); // Allow user to specify the port via command-line
    except
      on E: EConvertError do // Catch specific conversion errors
      begin
        Writeln('Invalid port specified. Using default port ', Port);
      end;
    end;
  end;

  Server := nil;
  try
    Server := TSimpleHTTPServer.Create(Port);
    try
      Server.StartServer;
      Writeln('Server is running. Press [Ctrl+C] to stop...');

      // Simulate stopping the server with a condition
      while not StopSignal do
        Sleep(1000); // Keep the main thread alive
    except
      on E: Exception do
        Writeln('An error occurred while starting the server: ', E.Message);
    end;
  finally
    if Assigned(Server) then
    begin
      Server.StopServer;
      FreeAndNil(Server);
    end;
    Writeln('Server stopped.');
  end;
end;


(*
  Procedure: TCLIHandler.ParseArgs
  Description:
    This procedure parses the command-line arguments and sets the
    corresponding options in the FOptions record.

  Changes:
    - The --http argument is now properly parsed and handled.
*)
procedure TCLIHandler.ParseArgs;
var
  I: Integer;
  IsPositionalFile: Boolean;
begin
  IsPositionalFile := False;
  FOptions.InputFile := '';
  FOptions.OutputFile := '';

  I := 1;
  while I <= ParamCount do
  begin
    if (ParamStr(I) = '--help') then
    begin
      FOptions.ShowHelp := True;
    end
    else if (ParamStr(I) = '-v') or (ParamStr(I) = '--version') then
    begin
      FOptions.ShowVersion := True;
    end
    else if (ParamStr(I) = '--verbose') then
    begin
      FOptions.Verbose := True;
    end
    else if (ParamStr(I) = '--compile') then
    begin
      FOptions.CompileKayte := True;
      if I + 1 <= ParamCount then
      begin
        Inc(I);
        FOptions.InputFile := ParamStr(I);
      end
      else
      begin
        Error('Missing file path for --compile option');
      end;
    end
    else if (ParamStr(I) = '--run') then
    begin
      FOptions.RunBytecode := True;
      if I + 1 <= ParamCount then
      begin
        Inc(I);
        FOptions.InputFile := ParamStr(I);
      end
      else
      begin
        Error('Missing file path for --run option');
      end;
    end
    else if (ParamStr(I) = '-o') then
    begin
      if I + 1 <= ParamCount then
      begin
        Inc(I);
        FOptions.OutputFile := ParamStr(I);
      end
      else
      begin
        Error('Missing file path for -o option');
      end;
    end
    else if (ParamStr(I) = '--http') then
    begin
      FOptions.StartHttpServer := True;
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



procedure TCLIHandler.CompileKayteFile(const InputFile, OutputFile: string);
var
  SourceCode: TStringList;
  Lexer: TLexer;
  Parser: TParser;
begin
  if not FileExists(InputFile) then
  begin
    Writeln('Error: Input file not found: ', InputFile);
    Exit;
  end;

  Writeln('Compiling ', InputFile, '...');

  SourceCode := TStringList.Create;
  try
    // 1. Load the entire file content into a TStringList
    SourceCode.LoadFromFile(InputFile);

    // 2. Create the Lexer, passing the source code
    Lexer := TLexer.Create(SourceCode);
    try
      // 3. Create the Parser, passing the Lexer
      Parser := TParser.Create(Lexer);
      try
        //
        // Perform compilation here using the Parser object
        //
        Writeln('Compilation successful!');
      finally
        Parser.Free;
      end;
    finally
      Lexer.Free;
    end;
  finally
    SourceCode.Free;
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

