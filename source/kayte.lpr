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
  Lexer in 'Lexer.pas',
  Parser in 'Parser.pas',
  TokenDefs in 'TokenDefs.pas',
  AST in 'AST.pas',
  Compiler in 'compiler.pas',
  Assembler in 'Assembler.pas',
  BytecodeTypes in 'BytecodeTypes.pas',
  // VM and runtime
  VirtualMachine in 'VirtualMachine.pas',
  // Other units (cli.pas removed and integrated below)
  Bytecode in 'bytecode.pas',
  TestBytecode in 'TestBytecode.pas',
  XMLParser in 'XMLParser.pas',
  {$IFDEF KAYTE_HTTP}
  // Optional: pulls in fcl-web/fcl-net (fphttpserver, fpWeb, netdb...).
  // Build with -dKAYTE_HTTP to enable `kayte --http`. Left out of the
  // default build so the core language has no third-party dependencies
  // beyond the standard FPC RTL.
  SimpleHTTPServer in 'simplehttpserver.pas',
  {$ENDIF}
  sdk in 'sdk.pas',
  c99 in 'c99.pas',
  kayte2pce in 'kayte2pce.pas',
  KayteLibLoader in 'KayteLibLoader.pas',
  c_backend in 'c_backend.pas',
  kayte_compiler in 'kayte_compiler.pas',
  kayte_runtime in 'kayte_runtime.pas',
  kayte_loader in 'kayte_loader.pas',
  kayte_vm in 'kayte_vm.pas',
  mathlib in 'mathlib.pas',
  kayte_sdl3 in 'kayte_sdl3.pas',
  kayte_sdl2 in 'kayte_sdl2.pas',
  kayte_qt6 in 'kayte_qt6.pas',
  kayte_native in 'kayte_native.pas',
  kayte_llvm in 'kayte_llvm.pas',
  jsfrontend in 'jsfrontend.pas'
  {$IFDEF DARWIN}
  ,KayteArm64 in 'KayteArm64.pas'
  {$ENDIF}
  {$IFDEF LINUX}
  ,KayteArm64ELF in 'kaytearm64elf.pas'
  {$ENDIF}
  {$IFDEF WINDOWS}
  ,KayteArm64PE in 'kaytearm64pe.pas'
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
    NativeArm64: Boolean;  // --native-arm64: the direct Mach-O emitter
    KeepC: Boolean;        // --keep-c: keep --native's generated C (or --llvm's IR)
    UseLLVM: Boolean;      // --llvm: the LLVM backend (source/kayte_llvm.pas)
    Target: string;        // --target <triple> for --llvm; '' = this machine
    InputFile: string;
    OutputFile: string;
    StartHttpServer: Boolean;
    StartRepl: Boolean;
    QuickBasic: Boolean;   // --qbs: QuickBASIC compatibility (source/parser.pas)

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

  // Reads a length/count field and checks that that many items of
  // ItemSize bytes can still follow. The format has no header to check,
  // so this is what rejects a file that isn't Kayte bytecode, instead of
  // reading garbage lengths and allocating/looping without end.
  function ReadCount(ItemSize: Integer): LongInt;
  begin
    Result := -1;
    if (FileStream.Read(Result, SizeOf(Result)) <> SizeOf(Result)) or (Result < 0)
      or (Int64(Result) * ItemSize > FileStream.Size - FileStream.Position) then
      raise Exception.Create(InputFilePath + ' is not a Kayte bytecode file (make one with kayte --compile)');
  end;

begin
  Result := TByteCodeProgram.Create;

  FileStream := TFileStream.Create(InputFilePath, fmOpenRead or fmShareDenyWrite);
  try
    // 1. Read ProgramTitle
    Len := 0;
    Len := ReadCount(1);
    ProgramTitleBuffer := '';
    SetLength(ProgramTitleBuffer, Len);
    if Len > 0 then
      FileStream.Read(ProgramTitleBuffer[1], Len);
    Result.ProgramTitle := ProgramTitleBuffer;

    // 2. Read Instructions
    Len := ReadCount(SizeOf(TBCInstruction));
    TempInstructions := nil;
    SetLength(TempInstructions, Len);
    if Len > 0 then
      FileStream.Read(TempInstructions[0], Len * SizeOf(TBCInstruction));
    Result.Instructions := TempInstructions;

    // 3. Read StringLiterals
    Len := ReadCount(SizeOf(LongInt));
    for I := 0 to Len - 1 do
    begin
      KeyLength := 0;
      KeyLength := ReadCount(1);
      Key := '';
      SetLength(Key, KeyLength);
      if KeyLength > 0 then
        FileStream.Read(Key[1], KeyLength);
      Result.StringLiterals.Add(Key);
    end;

    // 4. Read IntegerLiterals
    Len := ReadCount(SizeOf(Int64));
    TempIntLiterals := nil;
    SetLength(TempIntLiterals, Len);
    if Len > 0 then
      FileStream.Read(TempIntLiterals[0], Len * SizeOf(Int64));
    Result.IntegerLiterals := TempIntLiterals;

    // 5. Read VariableMap
    Len := ReadCount(2 * SizeOf(LongInt));
    for I := 0 to Len - 1 do
    begin
      KeyLength := 0;
      KeyLength := ReadCount(1);
      Key := '';
      SetLength(Key, KeyLength);
      if KeyLength > 0 then
        FileStream.Read(Key[1], KeyLength);
      Value := 0;
      FileStream.Read(Value, SizeOf(Value));
      Result.VariableMap.Add(Key, Value);
    end;

    // 6. Read SubroutineMap
    Len := ReadCount(2 * SizeOf(LongInt));
    for I := 0 to Len - 1 do
    begin
      KeyLength := 0;
      KeyLength := ReadCount(1);
      Key := '';
      SetLength(Key, KeyLength);
      if KeyLength > 0 then
        FileStream.Read(Key[1], KeyLength);
      Value := 0;
      FileStream.Read(Value, SizeOf(Value));
      Result.SubroutineMap.Add(Key, Value);
    end;

    // 7. Read FormMap
    Len := ReadCount(2 * SizeOf(LongInt));
    for I := 0 to Len - 1 do
    begin
      KeyLength := 0;
      KeyLength := ReadCount(1);
      Key := '';
      SetLength(Key, KeyLength);
      if KeyLength > 0 then
        FileStream.Read(Key[1], KeyLength);
      Value := 0;
      FileStream.Read(Value, SizeOf(Value));
      Result.FormMap.Add(Key, Value);
    end;

  except
    Result.Free;
    FileStream.Free;
    raise;
  end;
  FileStream.Free;
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
  Writeln('  --native <file>  Compile a .kayte source file to a native executable (via C; needs cc)');
  Writeln('  --qbs            Compile QuickBASIC programs (line numbers, PRINT ;, DATA / READ, TYPE ...)');
  Writeln('  --keep-c         With --native, keep the generated C next to the output');
  Writeln('                   (-o <name>.c writes only the C, e.g. for an iOS app build)');
  Writeln('  --llvm <file>    Compile a .kayte source file to a native executable via LLVM IR (needs clang)');
  Writeln('  --target <triple>  With --llvm, the platform to build for, e.g. x86_64-linux-gnu,');
  Writeln('                   x86_64-w64-mingw32, arm64-apple-ios17.0, wasm32-wasi (default: this machine)');
  Writeln('                   (-o <name>.ll writes only the IR; --keep-c keeps it next to the output)');
  Writeln('  --native-arm64 <file>  Experimental direct ARM64 Mach-O emitter (macOS)');
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
  Writeln('Kayte Language v0.9.10'); // Changing version, upgrade, 0.9.10
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
    ExitCode := 1;
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

// Compiles source with the front end its file name picks: the
// JavaScript-like one (source/jsfrontend.pas) for .kjs / .js, else the
// BASIC one. Both produce the same bytecode. After printing the errors,
// raises EKayteParseError if there were any.
function ParseSource(SourceCode: TStringList; const FileName: string): TByteCodeProgram;
var
  Lexer: TLexer;
  Parser: TParser;
begin
  if IsJSSourceFile(FileName) then
    Exit(CompileJS(SourceCode.Text, FileName));
  Lexer := TLexer.Create(SourceCode);
  try
    Lexer.QBMode := Options.QuickBasic;
    Parser := TParser.Create(Lexer);
    try
      Parser.QBMode := Options.QuickBasic;
      Result := Parser.Parse;
    finally
      Parser.Free;
    end;
  finally
    Lexer.Free;
  end;
end;

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
    ExitCode := 1;
    Writeln('Error: Input file not found: ', InputFile);
    Exit;
  end;

  if Options.Verbose then
    Writeln('Compiling ', InputFile, '...');

  SourceCode := TStringList.Create;
  try
    SourceCode.LoadFromFile(InputFile);

    if Options.Verbose then
      Writeln('Parsing source code...');

    // Parse and generate the bytecode program
    BytecodeProgram := ParseSource(SourceCode, InputFile);
    try
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
    finally
      BytecodeProgram.Free;
    end;
  finally
    SourceCode.Free;
  end;
end;

// --native: compiles through C (see source/kayte_native.pas) into a
// standalone executable that behaves like `kayte --run`, QT included.
procedure CompileNativeFile(const InputFile, OutputFile: string);
var
  SourceCode: TStringList;
  LexerInstance: TLexer;
  ParserInstance: TParser;
  BytecodeProgram: TByteCodeProgram;
  OutputFilePath, ErrorMsg: string;
  Success: Boolean;
begin
  if not FileExists(InputFile) then
  begin
    ExitCode := 1;
    Writeln('Error: Input file not found: ', InputFile);
    Exit;
  end;

  if OutputFile <> '' then
    OutputFilePath := OutputFile
  else if Options.UseLLVM and (Options.Target <> '') then
  begin
    // Name the output for the target platform, not this machine.
    if (Pos('windows', LowerCase(Options.Target)) > 0) or (Pos('mingw', LowerCase(Options.Target)) > 0) then
      OutputFilePath := ChangeFileExt(InputFile, '.exe')
    else if Pos('wasm', LowerCase(Options.Target)) > 0 then
      OutputFilePath := ChangeFileExt(InputFile, '.wasm')
    else
      OutputFilePath := ChangeFileExt(InputFile, '');
  end
  else
  {$IFDEF WINDOWS}
    OutputFilePath := ChangeFileExt(InputFile, '.exe');
  {$ELSE}
    OutputFilePath := ChangeFileExt(InputFile, '');
  {$ENDIF}

  if Options.UseLLVM then
  begin
    if Options.Target <> '' then
      Writeln('Compiling ', InputFile, ' with LLVM for ', Options.Target, '...')
    else
      Writeln('Compiling ', InputFile, ' with LLVM...');
  end
  else
    Writeln('Compiling ', InputFile, ' to a native executable...');

  BytecodeProgram := nil;
  SourceCode := TStringList.Create;
  try
    SourceCode.LoadFromFile(InputFile);
    BytecodeProgram := ParseSource(SourceCode, InputFile);

    if Options.UseLLVM then
      Success := CompileWithLLVM(BytecodeProgram, OutputFilePath, Options.Target, Options.KeepC,
        Options.Verbose, ErrorMsg)
    else
      Success := CompileToNative(BytecodeProgram, OutputFilePath, Options.KeepC, Options.Verbose, ErrorMsg);
    if not Success then
    begin
      Writeln('Error: ', ErrorMsg);
      ExitCode := 1;
      Exit;
    end;

    Writeln('Native compilation successful: ', OutputFilePath);
  finally
    BytecodeProgram.Free;
    SourceCode.Free;
  end;
end;

// --native-arm64: the experimental direct Mach-O emitter
// (source/kayte_arm64_emit.c), kept for development of that backend.
procedure CompileNativeArm64File(const InputFile, OutputFile: string);
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
    ExitCode := 1;
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
            ExitCode := 1;
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
              ExitCode := 1;
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
        ExitCode := 1;
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
  ExitCode := 1;
  Writeln('Error: Native compilation is only supported on ARM64 architecture.');
  Writeln('Current architecture: ', {$I %FPCTARGETCPU%});
  Writeln('Please use --compile to generate bytecode instead.');
  {$ENDIF}
end;

// True for a Mach-O or ELF file - what --native produces.
function IsNativeExecutable(const FileName: string): Boolean;
var
  F: TFileStream;
  Magic: LongWord;
begin
  Result := False;
  Magic := 0;
  try
    F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      if F.Read(Magic, SizeOf(Magic)) <> SizeOf(Magic) then
        Exit;
    finally
      F.Free;
    end;
  except
    Exit;
  end;
  Result := (Magic = $FEEDFACF) or (Magic = $CFFAEDFE)   // Mach-O 64-bit
         or (Magic = $CAFEBABE) or (Magic = $BEBAFECA)   // universal (fat) Mach-O
         or (Magic = $464C457F);                          // ELF: 7F 'E' 'L' 'F'
end;

procedure RunBytecodeFile(const BytecodeFile: string);
var
  VM: TVirtualMachine;
  Generator: TBytecodeGenerator;
  BytecodeProgram: TByteCodeProgram;
begin
  if not FileExists(BytecodeFile) then
  begin
    ExitCode := 1;
    Writeln('Error: Bytecode file not found: ', BytecodeFile);
    Exit;
  end;

  if IsNativeExecutable(BytecodeFile) then
  begin
    ExitCode := 1;
    if ExtractFilePath(BytecodeFile) = '' then
      Writeln('Error: ', BytecodeFile, ' is a native executable (from --native), not bytecode - run it directly: ./', BytecodeFile)
    else
      Writeln('Error: ', BytecodeFile, ' is a native executable (from --native), not bytecode - run it directly: ', BytecodeFile);
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
    begin
      Writeln('Error during execution: ', E.Message);
      ExitCode := 1;
    end;
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
      begin
        Writeln('Error: Missing file path for --compile option');
        ExitCode := 1;
      end;
    end
    else if (Param = '--native') or (Param = '--native-arm64') or (Param = '--llvm') then
    begin
      Options.CompileNative := True;
      Options.NativeArm64 := Param = '--native-arm64';
      Options.UseLLVM := Param = '--llvm';
      Inc(I);
      if I <= ParamCount then
      begin
        Options.InputFile := ParamStr(I);
        Inc(I);
      end
      else
      begin
        Writeln('Error: Missing file path for --native option');
        ExitCode := 1;
      end;
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
      begin
        Writeln('Error: Missing file path for --run option');
        ExitCode := 1;
      end;
    end
    else if (Param = '--qbs') then
    begin
      Options.QuickBasic := True;
      Inc(I);
    end
    else if (Param = '--keep-c') then
    begin
      Options.KeepC := True;
      Inc(I);
    end
    else if (Param = '--target') then
    begin
      Inc(I);
      if I <= ParamCount then
      begin
        Options.Target := ParamStr(I);
        Inc(I);
      end
      else
      begin
        Writeln('Error: Missing triple for --target option (e.g. x86_64-linux-gnu)');
        ExitCode := 1;
      end;
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
      begin
        Writeln('Error: Missing file path for -o option');
        ExitCode := 1;
      end;
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
  Options.NativeArm64 := False;
  Options.KeepC := False;
  Options.UseLLVM := False;
  Options.Target := '';
  Options.InputFile := '';
  Options.OutputFile := '';
  Options.StartHttpServer := False;
  Options.StartRepl := False;
  Options.QuickBasic := False;


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
      if Options.NativeArm64 then
        CompileNativeArm64File(Options.InputFile, Options.OutputFile)
      else
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
    on E: EKayteParseError do
    begin
      // The errors themselves were printed as they were found.
      Writeln('Error: ', E.Message);
      Halt(1);
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
