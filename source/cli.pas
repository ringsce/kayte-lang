unit CLI;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  BytecodeTypes,
  TokenDefs,
  Lexer,
  Parser,
  SimpleHTTPServer,
  VirtualMachine;

type
  TBytecodeGenerator = class(TObject)
  private
    procedure WriteLenString(Stream: TFileStream; const S: AnsiString);
    function ReadLenString(Stream: TFileStream): AnsiString;
    procedure WriteStringIntMap(Stream: TFileStream; Map: TStringIntMap);
    procedure ReadStringIntMap(Stream: TFileStream; Map: TStringIntMap);
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
    StartHttpServer: Boolean;
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
    procedure StartHTTPServer;
  public
    constructor Create(const AppName, AppVersion: string);
    destructor Destroy; override;
    procedure ParseArgs;
    procedure Execute;
  end;

procedure InitializeCLIHandler;

implementation

{ TBytecodeGenerator }

procedure TBytecodeGenerator.WriteLenString(Stream: TFileStream; const S: AnsiString);
var
  Len: LongInt;
begin
  Len := Length(S);
  Stream.Write(Len, SizeOf(Len));
  if Len > 0 then
    Stream.Write(S[1], Len);
end;

function TBytecodeGenerator.ReadLenString(Stream: TFileStream): AnsiString;
var
  Len: LongInt;
begin
  Stream.Read(Len, SizeOf(Len));
  SetLength(Result, Len);
  if Len > 0 then
    Stream.Read(Result[1], Len);
end;

procedure TBytecodeGenerator.WriteStringIntMap(Stream: TFileStream; Map: TStringIntMap);
var
  Len: LongInt;
  I: Integer;
begin
  Len := Map.Count;
  Stream.Write(Len, SizeOf(Len));
  for I := 0 to Map.Count - 1 do
  begin
    WriteLenString(Stream, Map.Keys[I]);
    Stream.Write(Map.Data[I], SizeOf(LongInt));
  end;
end;

procedure TBytecodeGenerator.ReadStringIntMap(Stream: TFileStream; Map: TStringIntMap);
var
  Len: LongInt;
  I: Integer;
  Key: AnsiString;
  Value: LongInt;
begin
  Stream.Read(Len, SizeOf(Len));
  for I := 0 to Len - 1 do
  begin
    Key := ReadLenString(Stream);
    Stream.Read(Value, SizeOf(Value));
    Map.Add(Key, Value);
  end;
end;

function TBytecodeGenerator.LoadProgramFromFile(const InputFilePath: String): TByteCodeProgram;
var
  FileStream: TFileStream;
  Len: LongInt;
  I: Integer;
  TempInstructions: TBCInstructionArray;
  TempIntLiterals: TIntegerLiteralArray;
begin
  Result := TByteCodeProgram.Create;

  FileStream := TFileStream.Create(InputFilePath, fmOpenRead or fmShareDenyWrite);
  try
    // 1. Read ProgramTitle
    Result.ProgramTitle := ReadLenString(FileStream);

    // 2. Read Instructions
    FileStream.Read(Len, SizeOf(Len));
    SetLength(TempInstructions, Len);
    if Len > 0 then
      FileStream.Read(TempInstructions[0], Len * SizeOf(TBCInstruction));
    Result.Instructions := TempInstructions;

    // 3. Read StringLiterals
    FileStream.Read(Len, SizeOf(Len));
    for I := 0 to Len - 1 do
      Result.StringLiterals.Add(ReadLenString(FileStream));

    // 4. Read IntegerLiterals
    FileStream.Read(Len, SizeOf(Len));
    SetLength(TempIntLiterals, Len);
    if Len > 0 then
      FileStream.Read(TempIntLiterals[0], Len * SizeOf(Int64));
    Result.IntegerLiterals := TempIntLiterals;

    // 5-7. Read VariableMap, SubroutineMap, FormMap
    ReadStringIntMap(FileStream, Result.VariableMap);
    ReadStringIntMap(FileStream, Result.SubroutineMap);
    ReadStringIntMap(FileStream, Result.FormMap);
  finally
    FileStream.Free;
  end;
end;

procedure TBytecodeGenerator.SaveProgramToFile(AProgram: TByteCodeProgram; const OutputFilePath: String);
var
  FileStream: TFileStream;
  Len: LongInt;
  I: Integer;
  TempInstructions: TBCInstructionArray;
  TempIntLiterals: TIntegerLiteralArray;
begin
  FileStream := TFileStream.Create(OutputFilePath, fmCreate);
  try
    // 1. Write ProgramTitle
    WriteLenString(FileStream, AProgram.ProgramTitle);

    // 2. Write Instructions
    TempInstructions := AProgram.Instructions;
    Len := Length(TempInstructions);
    FileStream.Write(Len, SizeOf(Len));
    if Len > 0 then
      FileStream.Write(TempInstructions[0], Len * SizeOf(TBCInstruction));

    // 3. Write StringLiterals
    Len := AProgram.StringLiterals.Count;
    FileStream.Write(Len, SizeOf(Len));
    for I := 0 to AProgram.StringLiterals.Count - 1 do
      WriteLenString(FileStream, AProgram.StringLiterals[I]);

    // 4. Write IntegerLiterals
    TempIntLiterals := AProgram.IntegerLiterals;
    Len := Length(TempIntLiterals);
    FileStream.Write(Len, SizeOf(Len));
    if Len > 0 then
      FileStream.Write(TempIntLiterals[0], Len * SizeOf(Int64));

    // 5-7. Write VariableMap, SubroutineMap, FormMap
    WriteStringIntMap(FileStream, AProgram.VariableMap);
    WriteStringIntMap(FileStream, AProgram.SubroutineMap);
    WriteStringIntMap(FileStream, AProgram.FormMap);
  finally
    FileStream.Free;
  end;
end;

{ TCLIHandler }

constructor TCLIHandler.Create(const AppName, AppVersion: string);
begin
  inherited Create;
  FAppName := AppName;
  FAppVersion := AppVersion;
  FOptions.ShowHelp := False;
  FOptions.ShowVersion := False;
  FOptions.Verbose := False;
  FOptions.CompileKayte := False;
  FOptions.RunBytecode := False;
  FOptions.InputFile := '';
  FOptions.OutputFile := '';
  FOptions.StartHttpServer := False;
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
  Writeln('  --http           Starts a simple HTTP server');
  Writeln;
  Writeln('If no --compile or --run option is given, FILE is assumed to be a Kayte source file to compile.');
  Writeln('Default output file for compilation is <input_file_name>.bytecode');
  Writeln;
end;

procedure TCLIHandler.ShowVersion;
begin
  Writeln(FAppName, ' version ', FAppVersion);
end;

procedure TCLIHandler.StartHTTPServer;
var
  Port: Integer;
  Server: TSimpleHTTPServer;
  StopSignal: Boolean;
begin
  Port := 9090; // Default port
  StopSignal := False;

  Writeln('Starting Kings server on port ', Port, '...');

  Server := nil;
  try
    Server := TSimpleHTTPServer.Create(Port);
    try
      Server.StartServer;
      Writeln('Server is running. Press [Ctrl+C] to stop...');

      // Keep the main thread alive
      while not StopSignal do
        Sleep(1000);
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

procedure TCLIHandler.CompileKayteFile(const InputFile, OutputFile: string);
var
  SourceCode: TStringList;
  Lexer: TLexer;
  Parser: TParser;
  Generator: TBytecodeGenerator;
  BytecodeProgram: TByteCodeProgram;
  OutputFilePath: string;
begin
  if not FileExists(InputFile) then
  begin
    Writeln('Error: Input file not found: ', InputFile);
    Exit;
  end;

  if FOptions.Verbose then
    Writeln('Compiling ', InputFile, '...');

  SourceCode := TStringList.Create;
  try
    SourceCode.LoadFromFile(InputFile);

    if FOptions.Verbose then
      Writeln('Creating lexer...');

    // FIXED: Pass SourceCode (TStringList) not SourceCode.Text (string)
    Lexer := TLexer.Create(SourceCode);
    try
      if FOptions.Verbose then
        Writeln('Creating parser...');

      Parser := TParser.Create(Lexer);
      try
        if FOptions.Verbose then
          Writeln('Parsing source code...');

        // Parse and generate the bytecode program
        BytecodeProgram := Parser.Parse;

        // Determine output file path
        if OutputFile = '' then
          OutputFilePath := ChangeFileExt(InputFile, '.bytecode')
        else
          OutputFilePath := OutputFile;

        if FOptions.Verbose then
          Writeln('Saving bytecode to ', OutputFilePath, '...');

        // Generate and save the bytecode
        Generator := TBytecodeGenerator.Create;
        try
          Generator.SaveProgramToFile(BytecodeProgram, OutputFilePath);
          Writeln('Compilation successful! Bytecode saved to ', OutputFilePath);
        finally
          Generator.Free;
        end;

        // Free the generated program
        BytecodeProgram.Free;
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
  Generator: TBytecodeGenerator;
  BytecodeProgram: TByteCodeProgram;
begin
  if not FileExists(BytecodeFile) then
  begin
    Writeln('Error: Bytecode file not found: ', BytecodeFile);
    Exit;
  end;

  Writeln('Running ', BytecodeFile, '...');
  try
    // Load the bytecode program from file
    Generator := TBytecodeGenerator.Create;
    try
      BytecodeProgram := Generator.LoadProgramFromFile(BytecodeFile);
      try
        // Create VM with the loaded program and execute
        VM := TVirtualMachine.Create(BytecodeProgram);
        try
          VM.Run;
          Writeln('Execution finished.');
        finally
          VM.Free;
        end;
      finally
        BytecodeProgram.Free;
      end;
    finally
      Generator.Free;
    end;
  except
    on E: Exception do
      Writeln('Error during execution: ', E.Message);
  end;
end;

procedure TCLIHandler.ParseArgs;
var
  I: Integer;
  IsPositionalFile: Boolean;
begin
  IsPositionalFile := False;
  FOptions.OutputFile := '';
  FOptions.StartHttpServer := False;

  I := 1;
  while I <= ParamCount do
  begin
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
    else if (ParamStr(I) = '--http') then
    begin
      FOptions.StartHttpServer := True;
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
      FOptions.InputFile := ParamStr(I);
      IsPositionalFile := True;
    end
    else
      Writeln('Unknown option or unexpected argument: ', ParamStr(I));

    Inc(I);
  end;

  // If a positional file was given and no action specified, default to compile
  if IsPositionalFile and (not FOptions.CompileKayte) and (not FOptions.RunBytecode) and (not FOptions.StartHttpServer) then
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

  if FOptions.StartHttpServer then
  begin
    StartHTTPServer;
    Exit;
  end;

  // All actions above Exit when handled; reaching here means none applied.
  if FOptions.InputFile = '' then
    ShowHelp;
end;

procedure InitializeCLIHandler;
var
  CLIHandler: TCLIHandler;
begin
  CLIHandler := TCLIHandler.Create('kaytecc', '0.9.0');
  try
    try
      if ParamCount > 0 then
      begin
        CLIHandler.ParseArgs;
        CLIHandler.Execute;
      end
      else
      begin
        CLIHandler.ShowHelp;
      end;
    except
      on E: Exception do
      begin
        Writeln('Error while handling CLI: ', E.Message);
        Writeln('Usage: kc [options]');
        Writeln('Try "kc --help" for more information.');
      end;
    end;
  finally
    CLIHandler.Free;
  end;
end;

end.
