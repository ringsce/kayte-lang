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
    //constructor Create;override; // <-- no parameters
    procedure Run;
  end;

  // Define TByteCodeProgram before it is used
TByteCodeProgram = class
public
  ProgramTitle: string;
  Instructions: array of TBCInstruction;
  StringLiterals: TStringList;
  IntegerLiterals: array of Int64;

  // Maps are now TStringList with key-value pairs stored in the object list
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
    function CompileSource(const SourceFilePath: String): TByteCodeProgram;
    procedure SaveProgramToFile(AProgram: TByteCodeProgram; const OutputFilePath: String);
    function LoadProgramFromFile(const InputFilePath: String): TByteCodeProgram;
    procedure ParseArgs;
    procedure Execute;
  end;

implementation


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

procedure TBytecodeGenerator.SaveProgramToFile(AProgram: TByteCodeProgram; const OutputFilePath: String);
var
  FileStream: TFileStream;
  Len: LongInt;
  I: Integer;
  Key: AnsiString;
  Value: LongInt;
  LObject: Pointer; // Correctly use Pointer for non-object data
begin
  FileStream := TFileStream.Create(OutputFilePath, fmCreate);
  try
    // --- Full Serialization of TByteCodeProgram ---

    // 1. Write ProgramTitle
    Len := Length(AProgram.ProgramTitle);
    FileStream.Write(Len, SizeOf(Len)); // Write length of string
    if Len > 0 then
      FileStream.Write(AProgram.ProgramTitle[1], Len); // Write string data

    // 2. Write Instructions
    Len := Length(AProgram.Instructions);
    FileStream.Write(Len, SizeOf(Len)); // Write number of instructions
    if Len > 0 then
      FileStream.Write(AProgram.Instructions[0], Len * SizeOf(TBCInstruction)); // Write all instructions at once

    // 3. Write StringLiterals (TStringList)
    Len := AProgram.StringLiterals.Count;
    FileStream.Write(Len, SizeOf(Len)); // Write number of string literals
    for I := 0 to Len - 1 do
    begin
      Key := AProgram.StringLiterals[I]; // Use Key variable for string to write
      Value := Length(Key); // Reuse Value for string length
      FileStream.Write(Value, SizeOf(Value)); // Write length of current string
      if Value > 0 then
        FileStream.Write(Key[1], Value); // Write string data
    end;

    // 4. Write IntegerLiterals (dynamic array of Int64)
    Len := Length(AProgram.IntegerLiterals);
    FileStream.Write(Len, SizeOf(Len)); // Write number of integer literals
    if Len > 0 then
      FileStream.Write(AProgram.IntegerLiterals[0], Len * SizeOf(Int64)); // Write all integers at once

    // 5. Write VariableMap (TStringList with Objects)
    Len := AProgram.VariableMap.Count;
    FileStream.Write(Len, SizeOf(Len)); // Write number of entries in the list
    for I := 0 to Len - 1 do
    begin
      Key := AProgram.VariableMap.Names[I];
      LObject := AProgram.VariableMap.Objects[I];
      Value := LongInt(LObject); // Correct type cast from Pointer to LongInt

      // Write key (string)
      Len := Length(Key);
      FileStream.Write(Len, SizeOf(Len));
      if Len > 0 then
        FileStream.Write(Key[1], Len);

      // Write value (LongInt)
      FileStream.Write(Value, SizeOf(Value));
    end;

    // 6. Write SubroutineMap (TStringList with Objects)
    Len := AProgram.SubroutineMap.Count;
    FileStream.Write(Len, SizeOf(Len)); // Write number of entries
    for I := 0 to Len - 1 do
    begin
      Key := AProgram.SubroutineMap.Names[I];
      LObject := AProgram.SubroutineMap.Objects[I];
      Value := LongInt(LObject); // Correct type cast from Pointer to LongInt

      // Write key (string)
      Len := Length(Key);
      FileStream.Write(Len, SizeOf(Len));
      if Len > 0 then
        FileStream.Write(Key[1], Len);

      // Write value (LongInt)
      FileStream.Write(Value, SizeOf(Value));
    end;

    // 7. Write FormMap (TStringList with Objects)
    Len := AProgram.FormMap.Count;
    FileStream.Write(Len, SizeOf(Len)); // Write number of entries
    for I := 0 to Len - 1 do
    begin
      Key := AProgram.FormMap.Names[I];
      LObject := AProgram.FormMap.Objects[I];
      Value := LongInt(LObject); // Correct type cast from Pointer to LongInt

      // Write key (string)
      Len := Length(Key);
      FileStream.Write(Len, SizeOf(Len));
      if Len > 0 then
        FileStream.Write(Key[1], Len);

      // Write value (LongInt)
      FileStream.Write(Value, SizeOf(Value));
    end;

  finally
    FileStream.Free;
  end;
end;

// --- LoadProgramFromFile (needs to be updated to match the new save format) ---
function TBytecodeGenerator.LoadProgramFromFile(const InputFilePath: String): TByteCodeProgram;
var
  FileStream: TFileStream;
  Len: LongInt;
  I: Integer;
  Key: AnsiString;
  Value: LongInt;
  ProgramTitleBuffer: String;
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
    FileStream.Read(Len, SizeOf(Len)); // Read number of instructions
    SetLength(Result.Instructions, Len);
    if Len > 0 then
      FileStream.Read(Result.Instructions[0], Len * SizeOf(TBCInstruction)); // Read all instructions at once

    // 3. Read StringLiterals (TStringList)
    FileStream.Read(Len, SizeOf(Len)); // Read number of string literals
    for I := 0 to Len - 1 do
    begin
      FileStream.Read(Value, SizeOf(Value)); // Reuse Value for string length
      SetLength(Key, Value); // Reuse Key for string buffer
      if Value > 0 then
        FileStream.Read(Key[1], Value);
      Result.StringLiterals.Add(Key);
    end;

    // 4. Read IntegerLiterals (dynamic array of Int64)
    FileStream.Read(Len, SizeOf(Len)); // Read number of integer literals
    SetLength(Result.IntegerLiterals, Len);
    if Len > 0 then
      FileStream.Read(Result.IntegerLiterals[0], Len * SizeOf(Int64)); // Read all integers at once

    // 5. Read VariableMap (TStringList with Objects)
    FileStream.Read(Len, SizeOf(Len)); // Read number of entries in the list
    for I := 0 to Len - 1 do
    begin
      // Read key (string)
      FileStream.Read(Value, SizeOf(Value)); // Read key string length
      SetLength(Key, Value);
      if Value > 0 then
        FileStream.Read(Key[1], Value);

      // Read value (LongInt)
      FileStream.Read(Value, SizeOf(Value));
      Result.VariableMap.AddObject(Key, TObject(Pointer(Value))); // Corrected cast: LongInt -> Pointer -> TObject
    end;

    // 6. Read SubroutineMap (TStringList with Objects)
    FileStream.Read(Len, SizeOf(Len)); // Read number of entries
    for I := 0 to Len - 1 do
    begin
      // Read key (string)
      FileStream.Read(Value, SizeOf(Value));
      SetLength(Key, Value);
      if Value > 0 then
        FileStream.Read(Key[1], Value);

      // Read value (LongInt)
      FileStream.Read(Value, SizeOf(Value));
      Result.SubroutineMap.AddObject(Key, TObject(Pointer(Value))); // Corrected cast
    end;

    // 7. Read FormMap (TStringList with Objects)
    FileStream.Read(Len, SizeOf(Len)); // Read number of entries
    for I := 0 to Len - 1 do
    begin
      // Read key (string)
      FileStream.Read(Value, SizeOf(Value));
      SetLength(Key, Value);
      if Value > 0 then
        FileStream.Read(Key[1], Value);

      // Read value (LongInt)
      FileStream.Read(Value, SizeOf(Value));
      Result.FormMap.AddObject(Key, TObject(Pointer(Value))); // Corrected cast
    end;

  finally
    FileStream.Free;
  end;
end;


procedure TCLIHandler.RunBytecodeFile(const BytecodeFile: string);
var
  BytecodeGen: TBytecodeGenerator; // Used for loading the bytecode program
  KayteProgram: TByteCodeProgram;
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

  BytecodeGen := TBytecodeGenerator.Create;
  KayteProgram := nil; // Initialize to nil to handle cleanup if loading fails
  VM := nil; // Initialize to nil to handle cleanup if creation fails

  try
    if FOptions.Verbose then
      Writeln('Loading bytecode file: ', BytecodeFile);

    // Step 1: Load the TByteCodeProgram object from the bytecode file
    try
      KayteProgram := BytecodeGen.LoadProgramFromFile(BytecodeFile);
    except
      on E: Exception do
      begin
        Writeln('Error: Failed to load bytecode: ', E.Message);
        Exit;
      end;
    end;

    if FOptions.Verbose then
      Writeln('Executing bytecode...');

    // Step 2: Create the VM with the loaded TByteCodeProgram
    VM := TVirtualMachine.Create(KayteProgram); // Corrected: Use TVirtualMachine
    try
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
      // Free the VM. It will free the KayteProgram object.
      VM.Free;
    end;
  finally
    // Free the BytecodeGenerator.
    BytecodeGen.Free;
  end;
end;

procedure TCLIHandler.ParseArgs;
var
  I: Integer;
  IsPositionalFile: Boolean;
begin
  IsPositionalFile := False; // Flag to track if a positional file argument is found

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
    else if (not FOptions.CompileKayte) and (not FOptions.RunBytecode) and (FOptions.InputFile = '') then
    begin
      // If no explicit --compile or --run, treat the first non-option argument as the input file
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
    Writeln('No action or input file specified.');
    ShowHelp;
  end
  else if FOptions.Verbose then
  begin
    Writeln('No action specified. Use --help to see available options.');
  end;
end;

end.

