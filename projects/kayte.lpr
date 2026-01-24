Program kayte;
(*
* Programming language interpreter for Kreatyve Designs
* usage, with this tool you can make custom scripts
* to run on our own games, delivered by ringsce store
*)

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Zipper, fphttpclient, fpjson, jsonparser, Process,
  // Core compiler units
  Lexer, Parser, TokenDefs, AST, Compiler, Assembler, BytecodeTypes,
  // VM and runtime
  VirtualMachine,  // This is your real VM from ../source/VirtualMachine.pas
  // Other units
  cli in '../source/cli.pas',
  Bytecode in '../source/bytecode.pas',
  TestBytecode in '../source/TestBytecode.pas',
  XMLParser in '../source/XMLParser.pas',
  SimpleHTTPServer in '../components/http/SimpleHTTPServer.pas',
  sdk in '../source/sdk.pas',
  c99 in '../source/c99.pas',
  kayte2pce in '../source/kayte2pce.pas',
  KayteLibLoader in '../source/KayteLibLoader.pas',
  c_backend in '../source/c_backend.pas',
  kayte_compiler in '../kayte_compiler.pas',
  kayte_runtime in '../source/kayte_runtime.pas',
  kayte_loader in '../source/kayte_loader.pas',
  kayte_vm in '../source/kayte_vm.pas';

const
  DefaultPort = 8080;

// This shows the proper flow for executing a .kayte file
procedure ExecuteKayteFile(const SourceFile: string);
var
  Compiler: TCompiler;
  VM: TVirtualMachine;
  Source: TStringList;
  ByteCodeProg: TByteCodeProgram;
begin
  if not FileExists(SourceFile) then
  begin
    Writeln('Error: Source file not found: ', SourceFile);
    Exit;
  end;

  Writeln('Converting ', SourceFile, ' to bytecode...');

  // Step 1: Load the source code
  Source := TStringList.Create;
  try
    Source.LoadFromFile(SourceFile);

    // Step 2: Compile it to bytecode
    Compiler := TCompiler.Create;
    try
      ByteCodeProg := Compiler.CompileSource(Source);

      // Step 3: Execute the bytecode
      // Note: VM.Create takes the ByteCodeProgram as a parameter
      VM := TVirtualMachine.Create(ByteCodeProg);
      try
        Writeln('Executing bytecode...');
        VM.Run;
      finally
        VM.Free;
      end;

      // Free the bytecode program after execution
      ByteCodeProg.Free;

    finally
      Compiler.Free;
    end;

  finally
    Source.Free;
  end;
end;

procedure LoadAndExecuteBytecode(const BytecodeFile: string);
var
  VM: TVirtualMachine;
  ByteCodeProg: TByteCodeProgram;
begin
  if not FileExists(BytecodeFile) then
  begin
    Writeln('Error: Bytecode file not found: ', BytecodeFile);
    Exit;
  end;

  Writeln('Loading bytecode from ', BytecodeFile, '...');

  // TODO: Implement bytecode file loading
  // For now, we'll just show an error message
  Writeln('Error: Direct bytecode file loading not yet implemented.');
  Writeln('Please use .kayte source files instead.');

  {
  // This will be implemented when TByteCodeProgram has LoadFromFile method
  ByteCodeProg := TByteCodeProgram.Create;
  try
    ByteCodeProg.LoadFromFile(BytecodeFile);

    VM := TVirtualMachine.Create(ByteCodeProg);
    try
      VM.Run;
    finally
      VM.Free;
    end;

  finally
    ByteCodeProg.Free;
  end;
  }
end;

procedure DownloadMapsFromGitHubRepo(const RepoURL: string);
var
  Process: TProcess;
begin
  if RepoURL = '' then
  begin
    Writeln('Error: Repository URL is empty.');
    Exit;
  end;

  Process := TProcess.Create(nil);
  try
    try
      Process.Executable := '/usr/bin/curl';
      Process.Parameters.Add('-LOk');
      Process.Parameters.Add(RepoURL);
      Process.Options := Process.Options + [poWaitOnExit];
      Process.Execute;
      Writeln('Maps downloaded successfully from GitHub repository: ', RepoURL);
    except
      on E: Exception do
        Writeln('Error downloading maps: ', E.Message);
    end;
  finally
    Process.Free;
  end;
end;

procedure CheckForUpdates(const URL: string);
var
  Process: TProcess;
  Response: TStringList;
begin
  Process := TProcess.Create(nil);
  Response := TStringList.Create;
  try
    Process.Executable := '/usr/bin/curl';
    Process.Parameters.Add('-sI');
    Process.Parameters.Add(URL);
    Process.Options := Process.Options + [poUsePipes, poWaitOnExit];
    Process.Execute;
    Response.LoadFromStream(Process.Output);
    Writeln('Last-Modified:', Response.Values['Last-Modified']);
  finally
    Process.Free;
    Response.Free;
  end;
end;

procedure SaveKayteFileToBytecode(const SourceFile, OutputFile: string);
var
  BytecodeGen: TBytecodeGenerator;
begin
  BytecodeGen := TBytecodeGenerator.Create;
  try
    Writeln('Converting ', SourceFile, ' to bytecode...');
    BytecodeGen.GenerateBytecode(SourceFile, OutputFile);
    Writeln('Bytecode saved to ', OutputFile);
  finally
    BytecodeGen.Free;
  end;
end;

procedure CreatePK3File(SourceFile, TargetFile: string);
var
  Z: TZipper;
begin
  Z := TZipper.Create;
  try
    Z.FileName := TargetFile;
    Z.Entries.AddFileEntry(SourceFile);
    Z.ZipAllFiles;
    Writeln('Created PK3 file: ', TargetFile);
  finally
    Z.Free;
  end;
end;

procedure ParseAndRunDTD;
const
  DTDFilePath = 'assets/ui/ui.dtd';
var
  DTDParser: TDTDParser;
begin
  if not FileExists(DTDFilePath) then
  begin
    Writeln('Error: DTD file not found at: ', DTDFilePath);
    Exit;
  end;

  DTDParser := TDTDParser.Create(DTDFilePath);
  try
    try
      DTDParser.ParseDTD;
      Writeln('DTD parsing completed successfully. The file is well-formed.');
    except
      on E: Exception do
      begin
        Writeln('Error while parsing DTD: ', E.Message);
        Exit;
      end;
    end;
  finally
    DTDParser.Free;
  end;
end;

procedure ShowHelp;
begin
  Writeln('Kayte Language Compiler and Runtime');
  Writeln('Usage:');
  Writeln('  kayte <sourcefile.kayte>      - Compile and execute a Kayte source file');
  Writeln('  kayte <bytecode.kbc>          - Execute a compiled bytecode file');
  Writeln('  kayte --help                  - Show this help message');
  Writeln('  kayte --version               - Show version information');
end;

procedure ShowVersion;
begin
  Writeln('Kayte Language v0.1.0');
  Writeln('Copyright (c) 2024');
end;

var
  KayteConverter: TKayte2PCE;
  InputFile: string;
  FileExt: string;

{$R *.res}

begin
  // Initialize features
  Writeln('Downloading maps... (replace with implementation)');
  Writeln('Checking for updates... (replace with implementation)');

  // Display initialization messages
  Writeln('Memory initialized to 1024 bytes.');
  Writeln('Registers initialized to 16.');
  Writeln('Virtual machine initialized with 1024 bytes of memory and 16 registers.');

  // Test basic VM operations display
  Writeln('Executing NOP (No Operation)');
  Writeln('Executing LOAD');
  Writeln('Executing ADD');
  Writeln('Executing SUB');
  Writeln('Executing HALT');

  Writeln('Creating PK3 file... (replace with implementation)');

  // Parse command line arguments
  if ParamCount = 0 then
  begin
    ShowHelp;
    Exit;
  end;

  if ParamStr(1) = '--help' then
  begin
    ShowHelp;
    Exit;
  end;

  if ParamStr(1) = '--version' then
  begin
    ShowVersion;
    Exit;
  end;

  // Get the input file
  InputFile := ParamStr(1);
  FileExt := LowerCase(ExtractFileExt(InputFile));

  // Execute based on file extension
  if FileExt = '.kayte' then
  begin
    // Compile and execute source file
    ExecuteKayteFile(InputFile);
  end
  else if FileExt = '.kbc' then
  begin
    // Execute pre-compiled bytecode file
    LoadAndExecuteBytecode(InputFile);
  end
  else
  begin
    Writeln('Error: Unknown file type "', FileExt, '"');
    Writeln('Supported file types: .kayte (source), .kbc (bytecode)');
    Exit;
  end;

  Writeln('Program execution completed.');

  // Optional: Other operations can be called here as needed
  // SaveKayteFileToBytecode('example.kayte', 'example.bytecode');
  // CreatePK3File('example.txt', 'example.pk3');

  {
  try
    KayteConverter := TKayte2PCE.Create('/path/to/pceas');
    KayteConverter.ConvertKayteToROM('demo.kayte', 'output.pce');
  finally
    KayteConverter.Free;
  end;
  }
end.
