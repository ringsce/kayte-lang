Program kayte;
(*
* Programming language interpreter for Kreatyve Designs
* usage, with this tool you can make custom scripts
* to run on our own games, delivered by ringsce store
*)

(*{$linklib n64}
{$linklibc}
{$ifdef darwin}
  {$linkframework CoreFoundation}  // if you use macOS native features
  {$linkframework IOKit}
  {$linkframework Cocoa}
  {$linkframework AppKit}
{$endif} *)

uses
  SysUtils, Classes, Zipper, fphttpclient, fpjson, jsonparser, Process,
  cli in '../source/cli.pas',
  Bytecode in '../source/bytecode.pas',
  TestBytecode in '../source/TestBytecode.pas',
  VirtualMachine in '../source/VirtualMachine.pas',
  XMLParser in '../source/XMLParser.pas',
  SimpleHTTPServer in '../source/SimpleHTTPServer.pas',
  sdk in '../source/sdk.pas',
  c99 in '../source/c99.pas',
  //sys_ios in '../source/sys_ios.pas',
  //sys_mac in '../source/sys_mac.pas',
  //basic in '../source/basic.pas',
  kayte2pce in '../source/kayte2pce.pas',
  KayteLibLoader in '../source/KayteLibLoader.pas',
  c_backend in '../source/c_backend.pas',
  kayte_compiler in '../kayte_compiler.pas',
  kayte_runtime in '../source/kayte_runtime.pas',
  kayte_loader in '../source/kayte_loader.pas',
  kayte_vm in '../source/kayte_vm.pas',
  jvm in '../jvm/jvm.pas', AST, CrossPlatform
  //kayte_syntax in '../source/KayteSyntax.pas'
  //bytecode_embed in '../source/bytecode_embed.pas'
  //kayte_parser in '../source/kayte_parser.pas',
  //n64 in '../source/n64.pas'
  ;
  (*KayteToSNES*)

type
  TInstruction = (NOP, LOAD, ADD, SUB, HALT, IRC_HELP, IRC_WHOIS, IRC_SERVER, IRC_CONNECT, IF_COND, ELSE_COND, ENDIF, CASE_COND, ENDCASE);



TVirtualMachine = class
  private
    FMemory: array of Byte;
    FRegisters: array of Integer;
    FPC: Integer; // Program Counter
    FRunning: Boolean;
    procedure InitializeMemory(Size: Integer);
    procedure InitializeRegisters(Count: Integer);
    procedure ExecuteInstruction(Instruction: TInstruction);
  public
    procedure Init(MemorySize: Integer; RegisterCount: Integer);
    procedure Run;
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
      Process.Executable := '/usr/bin/curl'; // Path to the curl executable
      Process.Parameters.Add('-LOk');        // -LOk flags to download files and follow redirects
      Process.Parameters.Add(RepoURL);      // GitHub repository URL
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

(* Check for Updates *)
procedure CheckForUpdates(const URL: string);
var
  Process: TProcess;
  Response: TStringList;
begin
  Process := TProcess.Create(nil);
  Response := TStringList.Create;
  try
    Process.Executable := '/usr/bin/curl'; // Path to the curl executable
    Process.Parameters.Add('-sI'); // -s silent mode to suppress progress meter and error messages, -I fetches headers only
    Process.Parameters.Add(URL); // Resource URL
    Process.Options := Process.Options + [poUsePipes, poWaitOnExit];
    Process.Execute;
    Response.LoadFromStream(Process.Output);
    Writeln('Last-Modified:', Response.Values['Last-Modified']);
  finally
    Process.Free;
    Response.Free;
  end;
end;

  (* VM init *)

procedure TVirtualMachine.InitializeMemory(Size: Integer);
begin
  if Size > 0 then
  begin
    SetLength(FMemory, Size);
    FillChar(FMemory[0], Length(FMemory) * SizeOf(FMemory[0]), 0);
    Writeln('Memory initialized to ', Size, ' bytes.');
  end
  else
  begin
    Writeln('Error: Memory size must be greater than 0.');
  end;
end;

procedure TVirtualMachine.InitializeRegisters(Count: Integer);
begin
  if Count > 0 then
  begin
    SetLength(FRegisters, Count);
    FillChar(FRegisters[0], Length(FRegisters) * SizeOf(FRegisters[0]), 0);
    Writeln('Registers initialized to ', Count, '.');
  end
  else
  begin
    Writeln('Error: Register count must be greater than 0.');
  end;
end;


procedure TVirtualMachine.ExecuteInstruction(Instruction: TInstruction);
begin
  case Instruction of
    NOP: Writeln('Executing NOP (No Operation)');
    LOAD: Writeln('Executing LOAD');
    ADD: Writeln('Executing ADD');
    SUB: Writeln('Executing SUB');
    HALT:
    begin
      Writeln('Executing HALT');
      FRunning := False;
    end;
  else
    Writeln('Unknown instruction');
  end;
end;

(* Procedure to save .kayte files *)
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


procedure TVirtualMachine.Init(MemorySize: Integer; RegisterCount: Integer);
begin
  if (MemorySize > 0) and (RegisterCount > 0) then
  begin
    InitializeMemory(MemorySize);
    InitializeRegisters(RegisterCount);
    FPC := 0;
    FRunning := True;
    Writeln('Virtual machine initialized with ', MemorySize, ' bytes of memory and ', RegisterCount, ' registers.');
  end
  else
  begin
    Writeln('Error: Both memory size and register count must be greater than 0.');
  end;
end;

procedure TVirtualMachine.Run;
begin
  if (FPC >= 0) and (FPC < Length(FMemory)) then
  begin
    // Main execution loop using repeat-until (do-while equivalent)
    repeat
      // Simulating fetching an instruction and executing it
      // Here we just cycle through some sample instructions for demonstration
      case FPC of
        0: ExecuteInstruction(NOP);
        1: ExecuteInstruction(LOAD);
        2: ExecuteInstruction(ADD);
        3: ExecuteInstruction(SUB);
        4: ExecuteInstruction(HALT);
      else
        ExecuteInstruction(HALT); // Default to HALT if out of range
      end;

      Inc(FPC);
    until not FRunning;
  end
  else
  begin
    Writeln('Error: Program counter out of range.');
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

procedure InitializeCLIHandler;
var
  CLIHandler: TCLIHandler; // Declare CLIHandler as a local variable
begin
  CLIHandler := TCLIHandler.Create('kc', '1.10.0'); // Instantiate the object with command-line tool info
  try
    try
      Writeln('Parsing command-line arguments...');
      CLIHandler.ParseArgs;  // Parse command-line arguments
      Writeln('Executing command...');
      CLIHandler.Execute;    // Execute the appropriate command
      Writeln('CLI command executed successfully.');
    except
      on E: Exception do
      begin
        // Display error message and usage hints to the user
        Writeln('Error while handling CLI: ', E.Message);
        Writeln('Usage: kc [options]');
        Writeln('Try "kc --help" for more information.');
        Exit; // Exit the procedure if an error occurs
      end;
    end;
  finally
    CLIHandler.Free; // Ensure the object is freed after use
  end;
end;

procedure ParseAndRunDTD;
const
  DTDFilePath = 'assets/ui/ui.dtd'; // Define the DTD file path
var
  DTDParser: TDTDParser; // Declare DTDParser as a local variable
begin
  if not FileExists(DTDFilePath) then
  begin
    Writeln('Error: DTD file not found at: ', DTDFilePath);
    Exit; // Exit if the file does not exist
  end;

  DTDParser := TDTDParser.Create(DTDFilePath); // Create the DTDParser instance
  try
    try
      DTDParser.ParseDTD; // Parse the DTD file
      Writeln('DTD parsing completed successfully. The file is well-formed.');
    except
      on E: Exception do
      begin
        Writeln('Error while parsing DTD: ', E.Message); // Handle and log exceptions
        Exit; // Exit the procedure if an error occurs
      end;
    end;
  finally
    DTDParser.Free; // Ensure the object is freed after use
  end;
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

procedure InitializeAndRunVM;
var
  VM: TVirtualMachine;  // Assuming a custom TVirtualMachine class
begin
  VM := TVirtualMachine.Create;  // Instantiate the virtual machine object
  try
    // Download Maps (replace with actual download logic)
    Writeln('Downloading maps... (replace with implementation)');

    // Check for Updates (replace with actual update checking)
    Writeln('Checking for updates... (replace with implementation)');

    // Initialize the virtual machine
    VM.Init(1024, 16);  // Assuming Init takes memory size and register count

    // Run the virtual machine (replace with actual execution logic)
    VM.Run;

    // Create a PK3 file (replace with actual PK3 creation logic)
    Writeln('Creating PK3 file... (replace with implementation)');
  finally
    VM.Free;  // Release resources associated with the virtual machine
  end;
end;

var
  KayteConverter: TKayte2PCE;
  LibraryPath: string;

{$R *.res}

begin
    (*WriteLn('Enter the library file path to load:');
  ReadLn(LibraryPath);*)

  // Safe load
  if not LoadLibraryFile(LibraryPath, lmSafe) then
    WriteLn('Safe load failed for ', LibraryPath);

  // Unsafe load
  if not LoadLibraryFile(LibraryPath, lmUnsafe) then
    WriteLn('Unsafe load failed for ', LibraryPath);
  // Initialize the CLI handler and process arguments
  InitializeCLIHandler;

  // Parse and process DTD
  ParseAndRunDTD;

  // Save Kayte source file to bytecode (commented out, can be enabled as needed)
  // SourceFile := 'example.kyte';
  // OutputFile := 'example.bytecode';
  // SaveKayteFileToBytecode(SourceFile, OutputFile);

  // Start the HTTP server
  StartHTTPServer;

  // Initialize and run the virtual machine
  InitializeAndRunVM;

  // Call the procedure with a GitHub repository URL
  DownloadMapsFromGitHubRepo('https://github.com/username/repo-name/archive/main.zip');

  // Call the procedure with the update URL
  CheckForUpdates('https://example.com/updates/info');

  // Example call to SaveKayteFileToBytecode
  SaveKayteFileToBytecode('example.kayte', 'example.bytecode');

  // Example call to CreatePK3File
  CreatePK3File('example.txt', 'example.pk3');

  try
    KayteConverter := TKayte2PCE.Create('/path/to/pceas'); // Specify assembler path
    KayteConverter.ConvertKayteToROM('demo.kayte', 'output.pce'); // Source and output files
  finally
    KayteConverter.Free;
  end;

end.


procedure TVirtualMachine.InitializeRegisters(Count: Integer);
  begin
    if Count > 0 then
    begin
      SetLength(FRegisters, Count);
      FillChar(FRegisters[0], Count * SizeOf(Integer), 0);
      Writeln('Registers initialized to ', Count, '.');
    end
    else
    begin
      Writeln('Error: Register count must be greater than 0.');
    end;
  end;

procedure TVirtualMachine.Init(MemorySize: Integer; RegisterCount: Integer);
  begin
    if (MemorySize > 0) and (RegisterCount > 0) then
    begin
      InitializeMemory(MemorySize);
      InitializeRegisters(RegisterCount);
      FPC := 0;
      Writeln('Virtual machine initialized with ', MemorySize, ' bytes of memory and ', RegisterCount, ' registers.');
    end
    else
    begin
      Writeln('Error: Both memory size and register count must be greater than 0.');
    end;
  end;

procedure TVirtualMachine.Run;
  begin
    if (FPC >= 0) and (FPC < Length(FMemory)) then
    begin
      // Placeholder for the main execution loop
      Writeln('Running the virtual machine from PC = ', FPC, '...');
      // Add your execution logic here
    end
    else
    begin
      Writeln('Error: Program counter out of range.');
    end;
  end;

  var
    VM: TVirtualMachine;

begin
    VM := TVirtualMachine.Create;
    try
      VM.Init(1024, 16); // Initialize with 1024 bytes of memory and 16 registers
      VM.Run;
    finally
      VM.Free;
    end;
end.

// Function for addition
function Add(a, b: Integer): Integer;
begin
  Add := a + b;
end;

// Function for subtraction
function Sub(a, b: Integer): Integer;
begin
  Sub := a - b;
end;

// Function for multiplication
function Mult(a, b: Integer): Integer;
begin
  Mult := a * b;
end;

// Function for integer division
function Divi(a, b: Integer): Integer;
begin
  if b = 0 then
    raise Exception.Create('Division by zero');
  Divi := a div b;
end;

(* Function for modulus *)
function Modu(a, b: Integer): Integer;
begin
  if b = 0 then
    raise Exception.Create('Modulus by zero');
  Modu := a mod b;
end;

(* Procedure to implement while loop
//procedure PascalWhile(Condition: Boolean; Body: TProc);
//begin
//  while Condition do
//    Body();
//end;
 to be fixed

// Procedure to implement if-else statement
//procedure PascalIf(Condition: Boolean; ThenBlock, ElseBlock: TProc);
//begin
//  if Condition then
//    ThenBlock()
//  else if Assigned(ElseBlock) then
//    ElseBlock();
//end;      *)


(* Commands on the fly *)

(* if else procedure*)
procedure TVirtualMachine.ExecuteInstruction(Instruction: TInstruction);
var
  Condition: Boolean;
begin
  case Instruction of
    NOP: Writeln('Executing NOP (No Operation)');
    LOAD: Writeln('Executing LOAD');
    ADD: Writeln('Executing ADD');
    SUB: Writeln('Executing SUB');
    HALT:
    begin
      Writeln('Executing HALT');
      FRunning := False;
    end;
    IF_COND:
    begin
      // Example: Check if Register 0 > 0 (This is just a placeholder condition)
      Condition := FRegisters[0] > 0;
      if not Condition then
      begin
        // Skip to the next ELSE or ENDIF
        repeat
          Inc(FPC);
        until (FMemory[FPC] = Ord(ELSE_COND)) or (FMemory[FPC] = Ord(ENDIF));
      end;
    end;
    ELSE_COND:
    begin
      // Skip to the ENDIF
      repeat
        Inc(FPC);
      until FMemory[FPC] = Ord(ENDIF);
    end;
    ENDIF:
      ; // No operation, just a marker for end of IF
  else
    Writeln('Unknown instruction');
  end;
end;

procedure TVirtualMachine.ExecuteInstruction(Instruction: TBCInstruction);
var
  CaseValue: Int64;
  Matched: Boolean;
  TargetAddress: Integer;
  LoopVarIndex: Integer;
  CollectionIndex: Integer;
  CurrentCollection: TStringList;
  CurrentIterator: Integer;
  ItemValue: TBCValue;
  FormID: Integer; // For FORM_START/END
begin
  case Instruction.OpCode of
    OP_PUSH_INT: FRegisters[0] := CreateBCValueInteger(Instruction.Operand1);
    OP_PUSH_STRING: FRegisters[0] := CreateBCValueString(FProgram.StringLiterals[Instruction.Operand1]);
    OP_PUSH_VAR: FRegisters[0] := GetVariableValue(Instruction.Operand1);
    OP_POP_VAR: SetVariableValue(Instruction.Operand1, FRegisters[0]);
    OP_POP: ;

    OP_ADD_INT: FRegisters[0].IntValue := FRegisters[0].IntValue + FRegisters[1].IntValue;
    OP_SUB_INT: FRegisters[0].IntValue := FRegisters[0].IntValue - FRegisters[1].IntValue;
    OP_MUL_INT: FRegisters[0].IntValue := FRegisters[0].IntValue * FRegisters[1].IntValue;
    OP_DIV_INT:
      if FRegisters[1].IntValue = 0 then
        raise Exception.Create('VM Error: Division by zero');
      FRegisters[0].IntValue := FRegisters[0].IntValue div FRegisters[1].IntValue;
    OP_ADD_STRING: FRegisters[0].StringValue := FRegisters[0].AsString + FRegisters[1].AsString;

    OP_JUMP: FPC := Instruction.Operand1 - 1;
    OP_JUMP_IF_FALSE:
      if not FRegisters[0].BoolValue then
        FPC := Instruction.Operand1 - 1;

    OP_PRINT: WriteLn(FRegisters[0].AsString);
    OP_SHOW_FORM: WriteLn(Format('VM: Showing form: %s', [FRegisters[0].AsString]));

    OP_CASE_COND:
    begin
      CaseValue := FRegisters[0].IntValue;
      Matched := False;
      if CaseValue = 0 then Matched := True; // Placeholder condition

      if not Matched then
      begin
        repeat
          Inc(FPC);
          if FPC >= Length(FProgram.Instructions) then
            raise Exception.Create('VM Error: ENDCASE not found for CASE_COND.');
        until (FProgram.Instructions[FPC].OpCode = OP_ENDCASE);
      end;
    end;
    OP_ENDCASE:
      ;

    OP_FOREACH_INIT:
    begin
      LoopVarIndex := Instruction.Operand1;
      CollectionIndex := Instruction.Operand2;
      CurrentCollection := GetCollectionByIndex(CollectionIndex);
      FLoopIterators.Add(0);
      FLoopCollections.Add(CurrentCollection);

      if CurrentCollection.Count = 0 then
        FPC := Instruction.Operand2 - 1; // Jump past loop if empty
      else
      begin
        ItemValue := CreateBCValueString(CurrentCollection[0]);
        SetVariableValue(LoopVarIndex, ItemValue);
      end;
    end;

    OP_FOREACH_ITER:
    begin
      LoopVarIndex := Instruction.Operand1;
      if FLoopIterators.Count = 0 then
        raise Exception.Create('VM Error: FOREACH_ITER without active loop iterator.');

      CurrentIterator := FLoopIterators[FLoopIterators.Count - 1];
      CurrentCollection := FLoopCollections[FLoopCollections.Count - 1];

      Inc(CurrentIterator);

      if CurrentIterator < CurrentCollection.Count then
      begin
        FLoopIterators[FLoopIterators.Count - 1] := CurrentIterator;
        ItemValue := CreateBCValueString(CurrentCollection[CurrentIterator]);
        SetVariableValue(LoopVarIndex, ItemValue);
        FPC := Instruction.Operand2 - 1;
      end
      else
      begin
        FLoopIterators.Delete(FLoopIterators.Count - 1);
        FLoopCollections.Delete(FLoopCollections.Count - 1);
      end;
    end;

    OP_FOREACH_END:
      ;

    OP_CALL_PROC:
    begin
      TargetAddress := Instruction.Operand1;
      FCallStack.Push(FPC);
      FPC := TargetAddress - 1;
      WriteLn(Format('VM: Calling procedure at address %d. Return address: %d', [TargetAddress, FCallStack.FItems[FCallStack.Count-1]]));
    end;

    OP_RETURN_PROC:
    begin
      if FCallStack.IsEmpty then
        raise Exception.Create('VM Error: RETURN_PROC with empty call stack (no active call).');
      TargetAddress := FCallStack.Pop;
      FPC := TargetAddress;
      WriteLn(Format('VM: Returning from procedure to address %d', [TargetAddress]));
    end;

    OP_FORM_START:
    begin
      FormID := Instruction.Operand1;
      FActiveFormStack.Push(FormID);
      WriteLn(Format('VM: Entering FORM definition block for Form ID: %d', [FormID]));
    end;

    OP_FORM_END:
    begin
      if FActiveFormStack.IsEmpty then
        raise Exception.Create('VM Error: END FORM without matching FORM START.');
      FormID := FActiveFormStack.Pop;
      WriteLn(Format('VM: Exiting FORM definition block for Form ID: %d', [FormID]));
    end;

    OP_HALT:
    begin
      WriteLn('VM: HALT instruction encountered. Stopping execution.');
      // <<< NEW: Use the ProgramTitle here
      if FProgram <> nil then
        WriteLn(Format('VM: Application "%s" has finished.', [FProgram.ProgramTitle]));
      FPC := MaxInt;
    end;

    else
      WriteLn(Format('VM Error: Unknown instruction opcode %d at PC %d', [Ord(Instruction.OpCode), FPC]));
      FPC := MaxInt;
  end;
end;

