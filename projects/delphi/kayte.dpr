program Kayte;

(*
 * Programming language interpreter for Kreatyve Designs
 * usage, with this tool you can make custom scripts
 * to run on our own games, delivered by ringsce store
 *)

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  IdHTTP,
  IdZLib,
  XMLDoc,
  XMLIntf,
  System.Types,
  kaytemath in 'kaytemath.pas',
  sdk in 'sdk.pas';

type
  TInstruction = (NOP, LOAD, ADD, SUB, HALT, IRC_HELP, IRC_WHOIS, IRC_SERVER,
    IRC_CONNECT, IF_COND, ELSE_COND, ENDIF, CASE_COND, ENDCASE);

  TVirtualMachine = class
  private
    FMemory: TBytes;
    FRegisters: TArray<Integer>;
    FPC: Integer; // Program Counter
    FRunning: Boolean;
    procedure InitializeMemory(Size: Integer);
    procedure InitializeRegisters(Count: Integer);
    procedure ExecuteInstruction(Instruction: TInstruction);
  public
    procedure Init(MemorySize: Integer; RegisterCount: Integer);
    procedure Run;
  end;

procedure TVirtualMachine.InitializeMemory(Size: Integer);
begin
  if Size > 0 then
  begin
    SetLength(FMemory, Size);
    FillChar(FMemory[0], Size, 0);
    Writeln('Memory initialized to ', Size, ' bytes.');
  end
  else
    Writeln('Error: Memory size must be greater than 0.');
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
    Writeln('Error: Register count must be greater than 0.');
end;

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
        Condition := FRegisters[0] > 0;
        if not Condition then
        begin
          repeat
            Inc(FPC);
          until (FMemory[FPC] = Ord(ELSE_COND)) or (FMemory[FPC] = Ord(ENDIF));
        end;
      end;
    ELSE_COND:
      begin
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
    Writeln('Error: Both memory size and register count must be greater than 0.');
end;

procedure TVirtualMachine.Run;
begin
  if (FPC >= 0) and (FPC < Length(FMemory)) then
  begin
    repeat
      case FPC of
        0: ExecuteInstruction(NOP);
        1: ExecuteInstruction(LOAD);
        2: ExecuteInstruction(ADD);
        3: ExecuteInstruction(SUB);
        4: ExecuteInstruction(HALT);
      else
        ExecuteInstruction(HALT);
      end;
      Inc(FPC);
    until not FRunning;
  end
  else
    Writeln('Error: Program counter out of range.');
end;

var
  VM: TVirtualMachine;

begin
  try
    VM := TVirtualMachine.Create;
    try
      VM.Init(1024, 16); // Initialize with 1024 bytes of memory and 16 registers
      VM.Run;
    finally
      VM.Free;
    end;
  except
    on E: Exception do
      Writeln('An error occurred: ', E.Message);
  end;
end.

