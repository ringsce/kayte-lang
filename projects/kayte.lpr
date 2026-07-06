Program kayte;
(*
* Programming language interpreter for Kreatyve Designs
* usage, with this tool you can make custom scripts
* to run on our own games, delivered by ringsce store
*)

{$mode objfpc}{$H+}
{$NOTE 6058 OFF}  // Disable inline notes
{$WARN 4046 OFF}
{$HINTS OFF}

uses
  SysUtils, Classes,
  // Core compiler units
  Lexer in '../source/Lexer.pas',
  Parser in '../source/Parser.pas',
  TokenDefs in '../source/TokenDefs.pas',
  AST in '../source/AST.pas',
  Compiler in '../source/compiler.pas',
  Assembler in '../source/Assembler.pas',
  BytecodeTypes in '../source/BytecodeTypes.pas',
  // VM and runtime
  VirtualMachine in '../source/VirtualMachine.pas',
  // Other units (cli.pas removed and integrated below)
  Bytecode in '../source/bytecode.pas',
  TestBytecode in '../source/TestBytecode.pas',
  XMLParser in '../source/XMLParser.pas',
  {$IFDEF KAYTE_HTTP}
  // Optional: pulls in fcl-web/fcl-net (fphttpserver, fpWeb, netdb...).
  // Build with -dKAYTE_HTTP to enable `kayte --http`. Left out of the
  // default build so the core language has no third-party dependencies
  // beyond the standard FPC RTL.
  SimpleHTTPServer in '../components/http/SimpleHTTPServer.pas',
  {$ENDIF}
  sdk in '../source/sdk.pas',
  c99 in '../source/c99.pas',
  kayte2pce in '../source/kayte2pce.pas',
  KayteLibLoader in '../source/KayteLibLoader.pas',
  c_backend in '../source/c_backend.pas',
  kayte_compiler in '../kayte_compiler.pas',
  kayte_runtime in '../source/kayte_runtime.pas',
  kayte_loader in '../source/kayte_loader.pas',
  kayte_vm in '../source/kayte_vm.pas',
  mathlib in '../source/mathlib.pas'
  {$IFDEF DARWIN}
  ,KayteArm64 in '../source/KayteArm64.pas'
  {$ENDIF}
  ;
type
  { TBytecodeGenerator - Handles loading and saving bytecode files }
  TBytecodeGenerator = class(TObject)
  public
    procedure SaveProgramToFile(AProgram: TByteCodeProgram; const OutputFilePath: string);
    function LoadProgramFromFile(const InputFilePath: string): TByteCodeProgram;
  end;

  { TCLIOptions - Command line options structure }
  TCLIOptions = record
    ShowHelp: Boolean;
    ShowVersion: Boolean;
    Verbose: Boolean;
    CompileKayte: Boolean;
    RunBytecode: Boolean;
    CompileNative: Boolean;
    InputFile: string;
    OutputFile: string;
    StartHttpServer: Boolean;
    StartRepl: Boolean;

  end;

var
  Options: TCLIOptions;

{ TBytecodeGenerator Implementation }

function TBytecodeGenerator.LoadProgramFromFile(const InputFilePath: string): TByteCodeProgram;
var
  FileStream: TFileStream;
  Len: LongInt;
  I, KeyLength: Integer;
  Key: AnsiString;
  Value: LongInt;
  ProgramTitleBuffer: string;
  TempInstructions: TBCInstructionArray;
  TempIntLiterals: TIntegerLiteralArray;
begin
  Result := TByteCodeProgram.Create;

  FileStream := TFileStream.Create(InputFilePath, fmOpenRead or fmShareDenyWrite);
  try
    // 1. Read ProgramTitle
    Len := 0;
    FileStream.Read(Len, SizeOf(Len));
    ProgramTitleBuffer := '';
    SetLength(ProgramTitleBuffer, Len);
    if Len > 0 then
      FileStream.Read(ProgramTitleBuffer[1], Len);
    Result.ProgramTitle := ProgramTitleBuffer;

    // 2. Read Instructions
    FileStream.Read(Len, SizeOf(Len));
    TempInstructions := nil;
    SetLength(TempInstructions, Len);
    if Len > 0 then
      FileStream.Read(TempInstructions[0], Len * SizeOf(TBCInstruction));
    Result.Instructions := TempInstructions;

    // 3. Read StringLiterals
    FileStream.Read(Len, SizeOf(Len));
    for I := 0 to Len - 1 do
    begin
      KeyLength := 0;
      FileStream.Read(KeyLength, SizeOf(KeyLength));
      Key := '';
      SetLength(Key, KeyLength);
      if KeyLength > 0 then
        FileStream.Read(Key[1], KeyLength);
      Result.StringLiterals.Add(Key);
    end;

    // 4. Read IntegerLiterals
    FileStream.Read(Len, SizeOf(Len));
    TempIntLiterals := nil;
    SetLength(TempIntLiterals, Len);
    if Len > 0 then
      FileStream.Read(TempIntLiterals[0], Len * SizeOf(Int64));
    Result.IntegerLiterals := TempIntLiterals;

    // 5. Read VariableMap
    FileStream.Read(Len, SizeOf(Len));
    for I := 0 to Len - 1 do
    begin
      KeyLength := 0;
      FileStream.Read(KeyLength, SizeOf(KeyLength));
      Key := '';
      SetLength(Key, KeyLength);
      if KeyLength > 0 then
        FileStream.Read(Key[1], KeyLength);
      Value := 0;
      FileStream.Read(Value, SizeOf(Value));
      Result.VariableMap.Add(Key, Value);
    end;

    // 6. Read SubroutineMap
    FileStream.Read(Len, SizeOf(Len));
    for I := 0 to Len - 1 do
    begin
      KeyLength := 0;
      FileStream.Read(KeyLength, SizeOf(KeyLength));
      Key := '';
      SetLength(Key, KeyLength);
      if KeyLength > 0 then
        FileStream.Read(Key[1], KeyLength);
      Value := 0;
      FileStream.Read(Value, SizeOf(Value));
      Result.SubroutineMap.Add(Key, Value);
    end;

    // 7. Read FormMap
    FileStream.Read(Len, SizeOf(Len));
    for I := 0 to Len - 1 do
    begin
      KeyLength := 0;
      FileStream.Read(KeyLength, SizeOf(KeyLength));
      Key := '';
      SetLength(Key, KeyLength);
      if KeyLength > 0 then
        FileStream.Read(Key[1], KeyLength);
      Value := 0;
      FileStream.Read(Value, SizeOf(Value));
      Result.FormMap.Add(Key, Value);
    end;

  finally
    FileStream.Free;
  end;
end;

procedure TBytecodeGenerator.SaveProgramToFile(AProgram: TByteCodeProgram; const OutputFilePath: string);
var
  FileStream: TFileStream;
  Len: LongInt;
  I, KeyLength: Integer;
  Key: AnsiString;
  Value: LongInt;
  TempInstructions: TBCInstructionArray;
  TempIntLiterals: TIntegerLiteralArray;
begin
  FileStream := TFileStream.Create(OutputFilePath, fmCreate);
  try
    // 1. Write ProgramTitle
    Len := Length(AProgram.ProgramTitle);
    FileStream.Write(Len, SizeOf(Len));
    if Len > 0 then
      FileStream.Write(AProgram.ProgramTitle[1], Len);

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
    begin
      Key := AProgram.StringLiterals[I];
      KeyLength := Length(Key);
      FileStream.Write(KeyLength, SizeOf(KeyLength));
      if KeyLength > 0 then
        FileStream.Write(Key[1], KeyLength);
    end;

    // 4. Write IntegerLiterals
    TempIntLiterals := AProgram.IntegerLiterals;
    Len := Length(TempIntLiterals);
    FileStream.Write(Len, SizeOf(Len));
    if Len > 0 then
      FileStream.Write(TempIntLiterals[0], Len * SizeOf(Int64));

    // 5. Write VariableMap
    Len := AProgram.VariableMap.Count;
    FileStream.Write(Len, SizeOf(Len));
    for I := 0 to AProgram.VariableMap.Count - 1 do
    begin
      Key := AProgram.VariableMap.Keys[I];
      KeyLength := Length(Key);
      FileStream.Write(KeyLength, SizeOf(KeyLength));
      if KeyLength > 0 then
        FileStream.Write(Key[1], KeyLength);

      Value := AProgram.VariableMap.Data[I];
      FileStream.Write(Value, SizeOf(Value));
    end;

    // 6. Write SubroutineMap
    Len := AProgram.SubroutineMap.Count;
    FileStream.Write(Len, SizeOf(Len));
    for I := 0 to AProgram.SubroutineMap.Count - 1 do
    begin
      Key := AProgram.SubroutineMap.Keys[I];
      KeyLength := Length(Key);
      FileStream.Write(KeyLength, SizeOf(KeyLength));
      if KeyLength > 0 then
        FileStream.Write(Key[1], KeyLength);

      Value := AProgram.SubroutineMap.Data[I];
      FileStream.Write(Value, SizeOf(Value));
    end;

    // 7. Write FormMap
    Len := AProgram.FormMap.Count;
    FileStream.Write(Len, SizeOf(Len));
    for I := 0 to AProgram.FormMap.Count - 1 do
    begin
      Key := AProgram.FormMap.Keys[I];
      KeyLength := Length(Key);
      FileStream.Write(KeyLength, SizeOf(KeyLength));
      if KeyLength > 0 then
        FileStream.Write(Key[1], KeyLength);

      Value := AProgram.FormMap.Data[I];
      FileStream.Write(Value, SizeOf(Value));
    end;
  finally
    FileStream.Free;
  end;
end;

{ CLI Procedures }

procedure ShowHelp;
begin
  Writeln('Kayte Language Compiler and Runtime');
  Writeln('Usage: kayte [OPTIONS] [FILE]');
  Writeln;
  Writeln('Options:');
  Writeln('  --help           Show this help message and exit');
  Writeln('  -v, --version    Show the version information and exit');
  Writeln('  --verbose        Run in verbose mode');
  Writeln('  --compile <file> Compile a .kayte source file to bytecode');
  Writeln('  --run <file>     Run a bytecode (.bytecode) file');
  Writeln('  --native <file>  Compile a .kayte source file to native ARM64 executable (macOS only)');
  Writeln('  -o <file>        Specify the output file when compiling');
  Writeln('  --http           Starts a simple HTTP server');
  Writeln('  --repl           Start interactive REPL (Read-Eval-Print Loop)');  // Add this line
  Writeln;
  Writeln('Examples:');
  Writeln('  kayte --compile hello.kayte');
  Writeln('  kayte --native hello.kayte -o hello');
  Writeln('  kayte hello.kayte --native -o hello');
  Writeln('  kayte --run hello.bytecode');
  Writeln('  kayte --repl');
  Writeln;
end;

procedure ShowVersion;
begin
  Writeln('Kayte Language v0.9.0');
  Writeln('Copyright (c) Pedro Dias Vicente 2024-2026');
  {$IFDEF CPUAARCH64}
    {$IFDEF DARWIN}
    Writeln('Platform: macOS ARM64 (Apple Silicon) - Native compilation available');
    {$ENDIF}
    {$IFDEF LINUX}
    Writeln('Platform: Linux ARM64 (AArch64) - Native compilation available');
    {$ENDIF}
    {$IFDEF WINDOWS}
    Writeln('Platform: Windows ARM64 - Native compilation available');
    {$ENDIF}
  {$ELSE}
    Writeln('Platform: ', {$I %FPCTARGETOS%}, '/', {$I %FPCTARGETCPU%});
    Writeln('Native compilation not available on this architecture');
  {$ENDIF}
end;

procedure StartHTTPServer;
{$IFDEF KAYTE_HTTP}
var
  Port: Integer;
  Server: TSimpleHTTPServer;
  StopSignal: Boolean;
begin
  Port := 9090; // Default port
  StopSignal := False;

  Writeln('Starting Kayte HTTP server on port ', Port, '...');

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
{$ELSE}
begin
  Writeln('This build of Kayte was compiled without HTTP server support.');
  Writeln('Rebuild with -dKAYTE_HTTP to enable the --http option.');
end;
{$ENDIF}

procedure StartREPL;
var
  Input: string;
  SourceCode: TStringList;
  LexerInstance: TLexer;
  ParserInstance: TParser;
  BytecodeProgram: TByteCodeProgram;
  VM: TVirtualMachine;
  LineNumber: Integer;
begin
  Writeln('Kayte REPL v0.9.0');
  Writeln('Type "exit" or "quit" to leave, "help" for help');
  Writeln('===============================================');
  Writeln;

  LineNumber := 1;

  while True do
  begin
    Write('kayte[', LineNumber, ']> ');
    ReadLn(Input);

    // Trim whitespace
    Input := Trim(Input);

    // Check for exit commands
    if (Input = 'exit') or (Input = 'quit') then
    begin
      Writeln('Goodbye!');
      Break;
    end;

    // Check for help
    if Input = 'help' then
    begin
      Writeln('REPL Commands:');
      Writeln('  exit, quit - Exit the REPL');
      Writeln('  help       - Show this help message');
      Writeln('  clear      - Clear the screen');
      Writeln;
      Writeln('Enter Kayte code to execute it immediately.');
      Continue;
    end;

    // Check for clear command
    if Input = 'clear' then
    begin
      {$IFDEF WINDOWS}
      Writeln; // Simple approach for Windows
      {$ELSE}
      // For Unix-like systems
      Write(#27'[2J'#27'[H');
      {$ENDIF}
      Continue;
    end;

    // Skip empty lines
    if Input = '' then
    begin
      Inc(LineNumber);
      Continue;
    end;

    // Try to compile and execute the input
    SourceCode := TStringList.Create;
    try
      SourceCode.Add(Input);

      try
        // Create lexer and parser
        LexerInstance := TLexer.Create(SourceCode);
        try
          ParserInstance := TParser.Create(LexerInstance);
          try
            // Parse the input
            BytecodeProgram := ParserInstance.Parse;
            try
              // Execute the bytecode
              VM := TVirtualMachine.Create(BytecodeProgram);
              try
                VM.Run;
              finally
                VM.Free;
              end;
            finally
              BytecodeProgram.Free;
            end;
          finally
            ParserInstance.Free;
          end;
        finally
          LexerInstance.Free;
        end;

      except
        on E: Exception do
        begin
          Writeln('Error: ', E.Message);
        end;
      end;

    finally
      SourceCode.Free;
    end;

    Inc(LineNumber);
  end;
end;


(*procedure CompileToBytecode(const InputFile, OutputFile: string);
var
  SourceCode: TStringList;
  Lexer: TLexer;
  Parser: TParser;
  BytecodeProgram: TByteCodeProgram;
begin
  WriteLn('Compiling ', InputFile, '...');

  // Check if input file exists
  if not FileExists(InputFile) then
  begin
    WriteLn('ERROR: Input file not found: ', InputFile);
    Exit;
  end;

  // Create and load source code
  SourceCode := TStringList.Create;
  try
    WriteLn('Loading source file...');
    SourceCode.LoadFromFile(InputFile);

    if SourceCode.Count = 0 then
    begin
      WriteLn('WARNING: Source file is empty');
    end
    else
      WriteLn('Loaded ', SourceCode.Count, ' lines');

    // Create lexer
    WriteLn('Creating lexer...');
    Lexer := TLexer.Create(SourceCode);
    try
      // Create parser
      WriteLn('Creating parser...');
      Parser := TParser.Create(Lexer);
      try
        // Parse the source code
        WriteLn('Parsing...');
        BytecodeProgram := Parser.Parse;

        // Save the bytecode
        WriteLn('Saving bytecode to ', OutputFile, '...');
        BytecodeProgram.SaveToFile(OutputFile);

        WriteLn('Compilation successful!');

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
*)

procedure CompileKayteFile(const InputFile, OutputFile: string);
var
  SourceCode: TStringList;
  LexerInstance: TLexer;
  ParserInstance: TParser;
  Generator: TBytecodeGenerator;
  BytecodeProgram: TByteCodeProgram;
  OutputFilePath: string;
begin
  if not FileExists(InputFile) then
  begin
    Writeln('Error: Input file not found: ', InputFile);
    Exit;
  end;

  if Options.Verbose then
    Writeln('Compiling ', InputFile, '...');

  SourceCode := TStringList.Create;
  try
    SourceCode.LoadFromFile(InputFile);

    if Options.Verbose then
      Writeln('Creating lexer...');

    LexerInstance := TLexer.Create(SourceCode);
    try
      if Options.Verbose then
        Writeln('Creating parser...');

      ParserInstance := TParser.Create(LexerInstance);
      try
        if Options.Verbose then
          Writeln('Parsing source code...');

        // Parse and generate the bytecode program
        BytecodeProgram := ParserInstance.Parse;

        // Determine output file path
        if OutputFile = '' then
          OutputFilePath := ChangeFileExt(InputFile, '.bytecode')
        else
          OutputFilePath := OutputFile;

        if Options.Verbose then
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
        ParserInstance.Free;
      end;
    finally
      LexerInstance.Free;
    end;
  finally
    SourceCode.Free;
  end;
end;

procedure CompileNativeFile(const InputFile, OutputFile: string);
var
  SourceCode: TStringList;
  LexerInstance: TLexer;
  ParserInstance: TParser;
  BytecodeProgram: TByteCodeProgram;
  OutputFilePath: string;
  NativeInstructions: array of TKayteInsn;
  I: Integer;
  Insn: TBCInstruction;
  CompileResult: Integer;
  PlatformName: string;
begin
  {$IFDEF CPUAARCH64}

  // Determine platform
  {$IFDEF DARWIN}
  PlatformName := 'macOS ARM64';
  {$ENDIF}
  {$IFDEF LINUX}
  PlatformName := 'Linux ARM64';
  {$ENDIF}
  {$IFDEF WINDOWS}
  PlatformName := 'Windows ARM64';
  {$ENDIF}

  if not FileExists(InputFile) then
  begin
    Writeln('Error: Input file not found: ', InputFile);
    Exit;
  end;

  Writeln('Compiling ', InputFile, ' to native ARM64 executable (', PlatformName, ')...');

  SourceCode := TStringList.Create;
  try
    try
      if Options.Verbose then
        Writeln('Loading source file...');

      SourceCode.LoadFromFile(InputFile);

      if Options.Verbose then
        Writeln('  Source loaded: ', SourceCode.Count, ' lines');

      if Options.Verbose then
        Writeln('Step 1/3: Parsing source code...');

      if Options.Verbose then
        Writeln('  Creating lexer...');

      LexerInstance := TLexer.Create(SourceCode);

      if Options.Verbose then
        Writeln('  Lexer created successfully');

      try
        if Options.Verbose then
          Writeln('  Creating parser...');

        ParserInstance := TParser.Create(LexerInstance);

        if Options.Verbose then
          Writeln('  Parser created successfully');

        try
          if Options.Verbose then
            Writeln('  Calling Parse()...');

          BytecodeProgram := ParserInstance.Parse;

          if Options.Verbose then
            Writeln('  Parse() completed');

          if BytecodeProgram = nil then
          begin
            Writeln('Error: Parser returned nil bytecode program');
            Exit;
          end;

          try
            if Options.Verbose then
            begin
              Writeln('Step 2/3: Converting bytecode to native instructions...');
              Writeln('  Instruction count: ', Length(BytecodeProgram.Instructions));
            end;

            // Convert bytecode to native instructions
            NativeInstructions := nil;
            SetLength(NativeInstructions, Length(BytecodeProgram.Instructions));

            for I := 0 to High(BytecodeProgram.Instructions) do
            begin
              Insn := BytecodeProgram.Instructions[I];

              if Options.Verbose then
                Writeln('  Converting instruction ', I, ': OpCode=', Ord(Insn.OpCode),
                        ', Operand1=', Insn.Operand1);

              NativeInstructions[I] := MakeInsn(TKayteOpcode(Ord(Insn.OpCode)), Insn.Operand1);
            end;

            // Determine output file path
            if OutputFile = '' then
            begin
              {$IFDEF WINDOWS}
              OutputFilePath := ChangeFileExt(InputFile, '.exe');
              {$ELSE}
              OutputFilePath := ChangeFileExt(InputFile, '');
              {$ENDIF}
            end
            else
              OutputFilePath := OutputFile;

            if Options.Verbose then
            begin
              Writeln('Step 3/3: Generating native executable...');
              Writeln('  Platform: ', PlatformName);
              Writeln('  Output: ', OutputFilePath);
              Writeln('  Instructions: ', Length(NativeInstructions));
            end;

            // Compile to native format based on platform
            {$IFDEF DARWIN}
            // macOS - Mach-O format
            CompileResult := CompileToMachO(NativeInstructions, OutputFilePath);
            {$ENDIF}

            {$IFDEF LINUX}
            // Linux - ELF format
            CompileResult := CompileToELF(NativeInstructions, OutputFilePath);
            {$ENDIF}

            {$IFDEF WINDOWS}
            // Windows - PE format
            CompileResult := CompileToPE(NativeInstructions, OutputFilePath);
            {$ENDIF}

            if CompileResult = 0 then
            begin
              Writeln;
              Writeln('✓ Native compilation successful!');
              Writeln('Platform: ', PlatformName);
              Writeln('Executable created: ', OutputFilePath);
              Writeln;
              {$IFDEF WINDOWS}
              Writeln('Run with: ', ExtractFileName(OutputFilePath));
              {$ELSE}
              Writeln('Run with: ./', ExtractFileName(OutputFilePath));
              {$ENDIF}
            end
            else
            begin
              Writeln;
              Writeln('Error: Native compilation failed with code: ', CompileResult);
              Writeln('The native compiler for ', PlatformName, ' may not be fully implemented yet.');
              {$IFDEF DARWIN}
              Writeln('Please check kayte_arm64_emit.c for implementation details.');
              {$ENDIF}
              {$IFDEF LINUX}
              Writeln('Please check kayte_arm64_elf.c for implementation details.');
              {$ENDIF}
              {$IFDEF WINDOWS}
              Writeln('Please check kayte_arm64_pe.c for implementation details.');
              {$ENDIF}
            end;

          finally
            if Options.Verbose then
              Writeln('  Freeing BytecodeProgram...');
            BytecodeProgram.Free;
          end;

        finally
          if Options.Verbose then
            Writeln('  Freeing Parser...');
          ParserInstance.Free;
        end;
      finally
        if Options.Verbose then
          Writeln('  Freeing Lexer...');
        LexerInstance.Free;
      end;

    except
      on E: Exception do
      begin
        Writeln('Error during compilation: ', E.ClassName, ': ', E.Message);
        Writeln('This error occurred while processing: ', InputFile);
        Exit;
      end;
    end;
  finally
    if Options.Verbose then
      Writeln('  Freeing SourceCode...');
    SourceCode.Free;
  end;

  {$ELSE}
  // Not ARM64 architecture
  Writeln('Error: Native compilation is only supported on ARM64 architecture.');
  Writeln('Current architecture: ', {$I %FPCTARGETCPU%});
  Writeln('Please use --compile to generate bytecode instead.');
  {$ENDIF}
end;

procedure RunBytecodeFile(const BytecodeFile: string);
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

procedure ParseArgs;
var
  I: Integer;
  Param: string;
begin
  Options.OutputFile := '';
  Options.StartHttpServer := False;

  I := 1;
  while I <= ParamCount do
  begin
    Param := ParamStr(I);

    if (Param = '--help') then
    begin
      Options.ShowHelp := True;
      Inc(I);
    end
    else if (Param = '-v') or (Param = '--version') then
    begin
      Options.ShowVersion := True;
      Inc(I);
    end
    else if (Param = '--verbose') then
    begin
      Options.Verbose := True;
      Inc(I);
    end
    else if (Param = '--compile') then
    begin
      Options.CompileKayte := True;
      Inc(I);
      if I <= ParamCount then
      begin
        Options.InputFile := ParamStr(I);
        Inc(I);
      end
      else
        Writeln('Error: Missing file path for --compile option');
    end
    else if (Param = '--native') then
    begin
      Options.CompileNative := True;
      Inc(I);
      if I <= ParamCount then
      begin
        Options.InputFile := ParamStr(I);
        Inc(I);
      end
      else
        Writeln('Error: Missing file path for --native option');
    end
    else if (Param = '--run') then
    begin
      Options.RunBytecode := True;
      Inc(I);
      if I <= ParamCount then
      begin
        Options.InputFile := ParamStr(I);
        Inc(I);
      end
      else
        Writeln('Error: Missing file path for --run option');
    end
    else if (Param = '--http') then
    begin
      Options.StartHttpServer := True;
      Inc(I);
    end
    else if (Param = '--repl') then
    begin
      Options.StartRepl := True;
      Inc(I);
    end
    else if (Param = '-o') then
    begin
      Inc(I);
      if I <= ParamCount then
      begin
        Options.OutputFile := ParamStr(I);
        Inc(I);
      end
      else
        Writeln('Error: Missing file path for -o option');
    end
    else if (Param[1] <> '-') then
    begin
      // Positional argument - could be input file
      if Options.InputFile = '' then
        Options.InputFile := Param;
      Inc(I);
    end
    else
    begin
      Writeln('Unknown option: ', Param);
      Inc(I);
    end;
  end;  // This closes the while loop

  // If an input file was given and no action specified, default to compile
  if (Options.InputFile <> '') and
     (not Options.CompileKayte) and
     (not Options.RunBytecode) and
     (not Options.StartHttpServer) and
     (not Options.StartRepl) and
     (not Options.CompileNative) then
  begin
    Options.CompileKayte := True;
    if Options.Verbose then
      Writeln('Info: No action specified, defaulting to --compile for input file: ', Options.InputFile);
  end;
end;

{ Main Program }

{$R *.res}

begin
  // Initialize options
  Options.ShowHelp := False;
  Options.ShowVersion := False;
  Options.Verbose := False;
  Options.CompileKayte := False;
  Options.RunBytecode := False;
  Options.CompileNative := False;
  Options.InputFile := '';
  Options.OutputFile := '';
  Options.StartHttpServer := False;
  Options.StartRepl := False;


  // Display banner
  Writeln('Kayte Language Runtime Environment');
  Writeln('===================================');
  Writeln;

  // Parse command line arguments
  if ParamCount = 0 then
  begin
    ShowHelp;
    Exit;
  end;

  try
    ParseArgs;

    // Debug: Show what was parsed
    if Options.Verbose then
    begin
      Writeln('Parsed arguments:');
      Writeln('  Input file: ', Options.InputFile);
      Writeln('  Output file: ', Options.OutputFile);
      Writeln('  Compile native: ', Options.CompileNative);
      Writeln('  Compile bytecode: ', Options.CompileKayte);
      Writeln('  Run bytecode: ', Options.RunBytecode);
      Writeln;
    end;

    // Execute based on parsed options
    if Options.ShowHelp then
    begin
      ShowHelp;
      Exit;
    end;

    if Options.ShowVersion then
    begin
      ShowVersion;
      Exit;
    end;

    if Options.CompileNative then
    begin
      CompileNativeFile(Options.InputFile, Options.OutputFile);
      Exit;
    end;

    if Options.CompileKayte then
    begin
      CompileKayteFile(Options.InputFile, Options.OutputFile);
      Exit;
    end;

    if Options.RunBytecode then
    begin
      RunBytecodeFile(Options.InputFile);
      Exit;
    end;

    if Options.StartHttpServer then
    begin
      StartHTTPServer;
      Exit;
    end;

    if Options.StartRepl then  // Add this block
    begin
      StartREPL;
      Exit;
    end;

    // If no specific action, show help
    if (Options.InputFile = '') then
    begin
      ShowHelp;
    end;

  except
    on E: EAccessViolation do
    begin
      Writeln('FATAL: Access violation detected');
      Writeln('This usually indicates:');
      Writeln('  1. Nil pointer dereference');
      Writeln('  2. Invalid memory access');
      Writeln('  3. Array bounds violation');
      Writeln;
      Writeln('Error: ', E.Message);
      Halt(2);
    end;
    on E: Exception do
    begin
      Writeln('Error: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;

  Writeln;
  Writeln('Program execution completed.');
end.
