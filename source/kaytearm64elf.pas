unit KayteArm64ELF;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type
  { TKayteOpcode - Instruction opcodes for Kayte VM }
  TKayteOpcode = (
    OP_NOP = 0,
    OP_PUSH,
    OP_POP,
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_LOAD,
    OP_STORE,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_CALL,
    OP_RETURN,
    OP_PRINT,
    OP_HALT,
    OP_CMP,
    OP_JE,
    OP_JNE,
    OP_JL,
    OP_JLE,
    OP_JG,
    OP_JGE
  );

  { TKayteInsn - Single instruction }
  TKayteInsn = record
    OpCode: TKayteOpcode;
    Operand: Int64;
  end;

{ Helper function to create instruction }
function MakeInsn(OpCode: TKayteOpcode; Operand: Int64): TKayteInsn;

{ Main compilation function - generates ELF executable for Linux ARM64 }
function CompileToELF(const Instructions: array of TKayteInsn; const OutputPath: string): Integer;

implementation

function MakeInsn(OpCode: TKayteOpcode; Operand: Int64): TKayteInsn;
begin
  Result.OpCode := OpCode;
  Result.Operand := Operand;
end;

{ External C function that generates the ELF binary }
function kayte_arm64_compile_elf(
  instructions: Pointer;
  instruction_count: Integer;
  output_path: PChar
): Integer; cdecl; external 'kaytearm64elf' name 'kayte_arm64_compile_elf';

function CompileToELF(const Instructions: array of TKayteInsn; const OutputPath: string): Integer;
var
  InstructionCount: Integer;
  OutputPathCStr: PChar;
  InstructionsPtr: Pointer;
begin
  Result := -1;

  if Length(Instructions) = 0 then
  begin
    WriteLn('Error: No instructions to compile');
    Exit;
  end;

  InstructionCount := Length(Instructions);
  InstructionsPtr := @Instructions[0];
  OutputPathCStr := PChar(OutputPath);

  try
    // Call the C backend to generate ELF executable
    Result := kayte_arm64_compile_elf(InstructionsPtr, InstructionCount, OutputPathCStr);

    if Result = 0 then
    begin
      // Make the file executable (chmod +x)
      {$IFDEF LINUX}
      fpChmod(OutputPath, &755); // rwxr-xr-x
      {$ENDIF}
    end;

  except
    on E: Exception do
    begin
      WriteLn('Exception in CompileToELF: ', E.Message);
      Result := -1;
    end;
  end;
end;

end.
