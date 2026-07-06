unit Assembler;
{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Generics.Collections, BytecodeTypes;

type
  { TAssembler }
  // TAssembler is responsible for generating the bytecode for the virtual machine.
  // It receives instructions and data from the parser and appends them
  // to an internal TByteCodeProgram object.
  TLabelMap = specialize TDictionary<string, Integer>;

  TAssembler = class
  private
    FProgram: TByteCodeProgram;
    FLabelMap: TLabelMap;
    FCurrentInstructionIndex: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetProgramTitle(const ATitle: string);
    // Defines a label at the current instruction index.
    procedure DefineLabel(const ALabel: string);
    // Emits a single instruction with its operands.
    procedure Emit(AOpCode: TByteCodeOp; const AOperands: array of Integer);
    // Index the *next* Emit call will occupy - i.e. how many instructions
    // have been emitted so far. Used as a jump target.
    function CurrentInstructionIndex: Integer;
    // Overwrites the jump target (Operand1) of an already-emitted
    // BC_JUMP/BC_JUMP_IF_FALSE instruction. Used to back-patch forward
    // jumps once the target address is known (e.g. the end of an IF's
    // THEN-branch, or a loop's exit point).
    procedure PatchJumpTarget(InstrIndex: Integer; Target: Integer);
    // Returns the program being built, without transferring ownership.
    // Safe to call repeatedly while parsing is still in progress.
    function CurrentProgram: TByteCodeProgram;
    // Returns the finalized bytecode program (transfers ownership to caller).
    // Must only be called once, after parsing has fully completed.
    function GetProgram: TByteCodeProgram;
    // Validates the bytecode before emitting it
    function ValidateBytecode: Boolean;
  end;

implementation

{ TAssembler }

constructor TAssembler.Create;
begin
  inherited Create;
  FProgram := TByteCodeProgram.Create;
  FLabelMap := TLabelMap.Create;
  FCurrentInstructionIndex := 0;
end;

destructor TAssembler.Destroy;
begin
  // Only free FProgram if ownership wasn't transferred via GetProgram
  if Assigned(FProgram) then
    FProgram.Free;
  FLabelMap.Free;
  inherited Destroy;
end;

procedure TAssembler.SetProgramTitle(const ATitle: string);
begin
  FProgram.ProgramTitle := ATitle;
end;

procedure TAssembler.DefineLabel(const ALabel: string);
begin
  if FLabelMap.ContainsKey(ALabel) then
    raise Exception.CreateFmt('Assembler Error: Label "%s" already defined.', [ALabel]);
  FLabelMap.Add(ALabel, FCurrentInstructionIndex);
end;

procedure TAssembler.Emit(AOpCode: TByteCodeOp; const AOperands: array of Integer);
var
  NewInstruction: TBCInstruction;
  OperandCount: Integer;
  CurrentLength: Integer;
  TempInstructions: TBCInstructionArray;
begin
  NewInstruction.OpCode := AOpCode;
  NewInstruction.Operand1 := 0;
  NewInstruction.Operand2 := 0;
  NewInstruction.Operand3 := 0;
  OperandCount := Length(AOperands);
  if OperandCount > 0 then
    NewInstruction.Operand1 := AOperands[0];
  if OperandCount > 1 then
    NewInstruction.Operand2 := AOperands[1];
  if OperandCount > 2 then
    NewInstruction.Operand3 := AOperands[2];
  // Get current instructions array
  TempInstructions := FProgram.Instructions;
  CurrentLength := Length(TempInstructions);
  // Expand the instructions array
  SetLength(TempInstructions, CurrentLength + 1);
  TempInstructions[CurrentLength] := NewInstruction;
  // Write back to the program
  FProgram.Instructions := TempInstructions;
  FCurrentInstructionIndex := CurrentLength + 1;
end;

function TAssembler.CurrentInstructionIndex: Integer;
begin
  Result := FCurrentInstructionIndex;
end;

procedure TAssembler.PatchJumpTarget(InstrIndex: Integer; Target: Integer);
var
  Instructions: TBCInstructionArray;
begin
  Instructions := FProgram.Instructions;
  if (InstrIndex < 0) or (InstrIndex >= Length(Instructions)) then
    raise Exception.CreateFmt('Assembler Error: Cannot patch out-of-range instruction %d.', [InstrIndex]);
  Instructions[InstrIndex].Operand1 := Target;
  FProgram.Instructions := Instructions;
end;

function TAssembler.CurrentProgram: TByteCodeProgram;
begin
  Result := FProgram;
end;

function TAssembler.GetProgram: TByteCodeProgram;
begin
  Result := FProgram;
  FProgram := nil;  // Transfer ownership to caller
end;

function TAssembler.ValidateBytecode: Boolean;
var
  I: Integer;
  Instructions: TBCInstructionArray;
begin
  Result := True;
  Instructions := FProgram.Instructions;
  for I := 0 to Length(Instructions) - 1 do
  begin
    // Add your validation logic here
    // Example: check if operands are within valid range
    if Instructions[I].Operand1 < 0 then
    begin
      raise Exception.CreateFmt('Assembler Error: Invalid operand at instruction %d.', [I]);
      Result := False;
      Exit;
    end;
  end;
end;

end.
