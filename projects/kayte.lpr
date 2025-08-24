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
  SimpleHTTPServer in '../components/http/SimpleHTTPServer.pas',
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
  //jvm in '../jvm/jvm.pas',
  AST, Compiler, Assembler
  //kayte_syntax in '../source/KayteSyntax.pas'
  //bytecode_embed in '../source/bytecode_embed.pas'
  //kayte_parser in '../source/kayte_parser.pas',
  //n64 in '../source/n64.pas'
  ;
  (*KayteToSNES*)
const
  DefaultPort = 8080;

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

(* HTTPServer and node *)

{ Utility function to load a file into a string }
function LoadFileAsString(const FileName: string): string;
var
  FileStream: TFileStream;
begin
  FileStream := nil;
  try
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    SetLength(Result, FileStream.Size);
    if FileStream.Size > 0 then
    begin
      FileStream.Read(Result[1], FileStream.Size);
    end;
  finally
    FileStream.Free;
  end;
end;

{ Helper function to check if a command-line tool exists }
function CheckForTool(const ToolName: string): Boolean;
var
  P: TProcess;
begin
  Result := False;
  P := nil;
  try
    P := TProcess.Create(nil);
    P.Executable := ToolName;
    P.Parameters.Add('-v'); // Use -v for version check, a common practice
    P.Options := [poUsePipes, poNoConsole]; // Hide console output
    P.Execute;
    if P.ExitStatus = 0 then
      Result := True;
  finally
    FreeAndNil(P);
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
  Server: TSimpleHTTPServer; // The missing variable declaration


{$R *.res}




begin
   // Initialize the CLI handler and process arguments
  //InitializeCLIHandler;

  // Parse and process DTD
  //ParseAndRunDTD;

  // Save Kayte source file to bytecode (commented out, can be enabled as needed)
  // SourceFile := 'example.kyte';
  // OutputFile := 'example.bytecode';
  // SaveKayteFileToBytecode(SourceFile, OutputFile);

  (*try
    // Create an instance of the HTTP server on the default port
    Server := TSimpleHTTPServer.Create(DefaultPort);

    // Assign our request handling procedure to the server's OnRequest event
    Server.OnRequest := @MyRequestHandler;

    // Start the server and wait for the user to press Enter to stop
    Server.StartServer;
    Writeln('Server is running on http://localhost:', DefaultPort);
    Writeln('Press Enter to stop the server.');
    Readln;
  finally
    // Always free the server object to prevent memory leaks
    Server.StopServer;
    Server.Free;
  end;   *)

  // Initialize and run the virtual machine
  InitializeAndRunVM;

  // Call the procedure with a GitHub repository URL
  //DownloadMapsFromGitHubRepo('https://github.com/username/repo-name/archive/main.zip');

  // Call the procedure with the update URL
  //CheckForUpdates('https://example.com/updates/info');

  // Example call to SaveKayteFileToBytecode
  SaveKayteFileToBytecode('example.kayte', 'example.bytecode');

  // Example call to CreatePK3File
  //CreatePK3File('example.txt', 'example.pk3');

  try
    KayteConverter := TKayte2PCE.Create('/path/to/pceas'); // Specify assembler path
    KayteConverter.ConvertKayteToROM('demo.kayte', 'output.pce'); // Source and output files
  finally
    KayteConverter.Free;
  end;

end.


(*
  Procedure to parse a `try-catch` block.

  Syntax:
  try { <body> } catch (error_var) { <body> }

  This procedure performs the following steps:
  1. Matches the `try` keyword and the opening curly brace '{'.
  2. Emits a placeholder instruction that the VM can use to find the exception handler.
     This placeholder will be patched later with the address of the `catch` block.
  3. Parses the statements within the `try` block.
  4. Emits a `JUMP` instruction to skip the `catch` block if the `try` block completes successfully.
     This jump's target is also a placeholder to be patched.
  5. Matches the closing curly brace '}' for the `try` block.
  6. Records the bytecode address of the `catch` block.
  7. Patches the placeholder from step 2 with the address of the `catch` block.
  8. Matches the `catch` keyword and the opening parenthesis '('.
  9. Parses the error variable identifier (e.g., `error`).
  10. Matches the closing parenthesis ')' and opening curly brace '{' for the `catch` block.
  11. Parses the statements within the `catch` block.
  12. Matches the closing curly brace '}'.
  13. Patches the placeholder from step 4 with the address of the instruction immediately following the `catch` block.
*)
procedure TParser.TryStatement;
var
  TryBlockAddress: Integer;
  JumpToCatchAddress: Integer;
  JumpOverCatchAddress: Integer;
  CatchBlockAddress: Integer;
  ErrorVarIndex: Integer;
begin
  // 1. Match the 'try' keyword.
  Match(tkTry);
  Match(tkBraceOpen);

  // 2. Emit an instruction to mark the beginning of the try block.
  // We need to tell the VM where to jump if an exception is thrown.
  TryBlockAddress := FBytecodeGenerator.NextInstructionAddress;
  FBytecodeGenerator.EmitInstruction(BC_PUSH_TRY_HANDLER);
  FBytecodeGenerator.EmitOperand(0); // Placeholder for the catch block address

  // 3. Parse the statements inside the 'try' block.
  while not (Check(tkBraceClose) or Check(tkEndOfFile)) do
  begin
    Statement;
  end;

  // 4. Emit a jump to skip the catch block if the try block is successful.
  JumpOverCatchAddress := FBytecodeGenerator.NextInstructionAddress;
  FBytecodeGenerator.EmitInstruction(BC_JUMP);
  FBytecodeGenerator.EmitOperand(0); // Placeholder for the jump target

  // 5. Match the closing '}'.
  Match(tkBraceClose);

  // 6. Record the address of the catch block.
  CatchBlockAddress := FBytecodeGenerator.NextInstructionAddress;

  // 7. Patch the handler address in the PUSH_TRY_HANDLER instruction.
  FBytecodeGenerator.PatchOperand(TryBlockAddress + 1, CatchBlockAddress);

  // 8. Match the 'catch' keyword and the error variable.
  Match(tkCatch);
  Match(tkParenthesisOpen);
  if FCurrentToken.TokenType <> tkIdentifier then
  begin
    Error('Expected an identifier for the catch variable.');
  end;
  // Here, you would get the variable index for the error variable and store it.
  ErrorVarIndex := FBytecodeGenerator.GetVariableIndex(FCurrentToken.Lexeme);
  Advance; // Consume the identifier
  Match(tkParenthesisClose);
  Match(tkBraceOpen);

  // 9. Parse the statements inside the 'catch' block.
  while not (Check(tkBraceClose) or Check(tkEndOfFile)) do
  begin
    Statement;
  end;

  // 10. Match the closing '}'.
  Match(tkBraceClose);

  // 11. Patch the JUMP instruction to skip the catch block.
  FBytecodeGenerator.PatchOperand(JumpOverCatchAddress + 1, FBytecodeGenerator.NextInstructionAddress);
end;



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


(*
  Procedure to parse and handle the 'uses' statement.

  This procedure performs the following steps:
  1. Matches the 'uses' keyword.
  2. Enters a loop to parse a comma-separated list of unit names.
  3. For each unit name, it attempts to find and load the corresponding source file.
  4. It recursively calls the parser on the new unit's source code, effectively
     importing its declarations into the current compilation scope.
  5. It handles errors for missing files or incorrect syntax.

  Assumes the existence of a file resolution mechanism (e.g., a function that
  maps a unit name like 'SysUtils' to a file path like 'SysUtils.pas' in a
  known library directory).
*)
procedure TParser.UsesStatement;
var
  UnitNameToken: TToken;
  UnitFileName: string;
begin
  // 1. Match the 'uses' keyword.
  Match(tkUses);

  // 2. Loop through a comma-separated list of unit names.
  repeat
    // Check if the current token is a valid identifier for a unit name.
    if FCurrentToken.TokenType <> tkIdentifier then
    begin
      Error('Expected a unit name (identifier) after uses statement');
      Break; // Exit the loop on error
    end;

    UnitNameToken := FCurrentToken;
    Advance; // Consume the unit name identifier

    // We can add a check here to prevent circular dependencies.
    // Example: if FImportedUnits.Contains(UnitNameToken.Lexeme) then continue;

    // 3. Resolve the unit name to a file path.
    // This is a placeholder for a real file resolution function.
    // For a simple case, you might just append '.pas' and look in the current directory.
    UnitFileName := UnitNameToken.Lexeme + '.pas';

    // 4. Load the unit and recursively call the parser on it.
    try
      if FileExists(UnitFileName) then
      begin
        // Create a new lexer for the unit file.
        // It's crucial to save and restore the state of the current parser.
        var OldLexer := FLexer;
        var OldCurrentToken := FCurrentToken;
        var OldPreviousToken := FPreviousToken;

        // Initialize a new lexer for the imported file.
        FLexer := TLexer.Create(UnitFileName);
        Advance; // Get the first token of the new file

        // Recursively call the main parsing procedure for the imported unit.
        // This is where the actual importing happens.
        LProgram; // Assuming LProgram is the entry point for parsing a source file.

        // Restore the parser's state after the unit is done.
        FLexer.Free;
        FLexer := OldLexer;
        FCurrentToken := OldCurrentToken;
        FPreviousToken := OldPreviousToken;

        // A better approach would be to pass the new lexer instance to the recursive call,
        // and have LProgram manage its own state, but this demonstrates the concept.
      end
      else
      begin
        Error('Unit file not found: ' + UnitFileName);
      end;
    except
      on E: Exception do
      begin
        Error('Error processing unit ' + UnitNameToken.Lexeme + ': ' + E.Message);
      end;
    end;

  // 5. Check for a comma to continue the list.
  until not MatchAny([tkComma]);

  // Match the final semicolon.
  Match(tkSemicolon);
end;


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

(*
    This procedure has been improved to use explicit jump instructions
    for conditional logic, which is more efficient and scalable.

    Assumptions:
    - The TInstruction type now includes an Operand field, e.g.,
      type
        TInstruction = record
          Opcode: TBytecodeOpcode;
          Operand: Integer;
        end;
    - The compiler is responsible for calculating and placing the correct
      jump target addresses in the Operand field during compilation.
*)
procedure TVirtualMachine.ExecuteInstruction(Instruction: TInstruction);
var
  Condition: Boolean;
begin
  case Instruction.Opcode of
    NOP: Writeln('Executing NOP (No Operation)');
    LOAD: Writeln('Executing LOAD');
    ADD: Writeln('Executing ADD');
    SUB: Writeln('Executing SUB');
    HALT:
    begin
      Writeln('Executing HALT');
      FRunning := False;
    end;

    // IF-ELSE logic handled with explicit jumps
    JUMP_IF_FALSE:
    begin
      // Pop the condition from the stack. Let's assume 0 is false, 1 is true.
      // We will need to have a pop function to get the value from the stack.
      // For this example, we'll use a placeholder variable.
      Condition := FStack.Pop() <> 0;

      // If the condition is false, jump to the address specified in the operand.
      // This address points to the start of the 'else' block or the 'endif'.
      if not Condition then
      begin
        FPC := Instruction.Operand;
      end;
      // If true, we simply continue to the next instruction (the 'if' block)
    end;

    JUMP:
    begin
      // This is a simple, unconditional jump.
      // It's used after the 'if' block to skip over the 'else' block.
      FPC := Instruction.Operand;
    end;

    // We can use a simple NOP for ENDIF. It's just a marker for the compiler.
    ENDIF:
      ; // No operation, the jumps handle the control flow

    else
      Writeln('Unknown instruction');
  end;
end;

(*
  Procedure to parse a C-style `for` loop and generate bytecode.

  Syntax:
  for ( <initializer>; <condition>; <increment> ) { <body> }

  This procedure performs the following steps to handle the loop:
  1. Matches the `for` keyword and the opening parenthesis `(`.
  2. Parses the `<initializer>` expression (e.g., `var i = 0;`).
  3. Matches the first semicolon `;`.
  4. Records the start of the loop body for bytecode jump instructions.
     This address is the target for the unconditional jump at the end of the loop body.
  5. Parses the `<condition>` expression (e.g., `i < 10;`).
  6. Matches the second semicolon `;`.
  7. Generates a `JUMP_IF_FALSE` instruction and reserves space for the jump target.
  8. Records the start of the increment expression's bytecode.
  9. Parses the `<increment>` expression (e.g., `i++`).
  10. Generates an unconditional `JUMP` instruction back to the loop body's start.
  11. Matches the closing parenthesis `)`.
  12. Matches the opening curly brace `{`.
  13. Parses the loop body statement.
  14. Matches the closing curly brace `}`.
  15. Generates an unconditional `JUMP` back to the start of the increment expression.
  16. Patches the `JUMP_IF_FALSE` instruction's target address to the end of the loop.
*)
function TParser.ForStatement: TStatementNode;
var
  Initializer: TStatementNode;
  Condition: TExpressionNode;
  Increment: TStatementNode;
  Body: TStatementNode;
  LoopStart: Integer;
  JumpIfFalseAddr: Integer;
begin
  // 1. Match the `for` keyword and the opening parenthesis.
  Match(tkFor);
  Match(tkParenthesisOpen);

  // 2. Parse the initializer expression (e.g., `var i = 0;`).
  Initializer := Statement;

  // Store the initializer node to be part of the ForStatementNode AST.
  Result := TForStatementNode.Create(Initializer);

  // 3. Get the bytecode address for the loop start (before the condition).
  LoopStart := FBytecodeGenerator.NextInstructionAddress;

  // 4. Parse the condition expression (e.g., `i < 10`).
  Condition := Expression;
  // Match the second semicolon.
  Match(tkSemicolon);

  // Store the condition node.
  TForStatementNode(Result).SetCondition(Condition);

  // 5. Generate a JUMP_IF_FALSE instruction. The target address is unknown for now.
  // We'll patch this later after parsing the body.
  JumpIfFalseAddr := FBytecodeGenerator.EmitInstruction(BC_JUMP_IF_FALSE);
  FBytecodeGenerator.EmitOperand(0); // Placeholder for jump address

  // 6. Record the start of the increment part for a later jump.
  var IncrementStart := FBytecodeGenerator.NextInstructionAddress;

  // 7. Parse the increment expression (e.g., `i++`).
  Increment := Statement;

  // Store the increment node.
  TForStatementNode(Result).SetIncrement(Increment);

  // 8. Generate an unconditional jump back to the condition check.
  FBytecodeGenerator.EmitInstruction(BC_JUMP);
  FBytecodeGenerator.EmitOperand(LoopStart);

  // 9. Match the closing parenthesis.
  Match(tkParenthesisClose);

  // 10. Record the address of the loop body.
  var LoopBodyStart := FBytecodeGenerator.NextInstructionAddress;

  // 11. Parse the loop body.
  Body := Statement;

  // Store the body node.
  TForStatementNode(Result).SetBody(Body);

  // 12. Generate an unconditional jump back to the increment.
  FBytecodeGenerator.EmitInstruction(BC_JUMP);
  FBytecodeGenerator.EmitOperand(IncrementStart);

  // 13. Patch the JUMP_IF_FALSE instruction.
  // The jump target is the address right after the loop's body.
  FBytecodeGenerator.PatchOperand(JumpIfFalseAddr, FBytecodeGenerator.NextInstructionAddress);
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

