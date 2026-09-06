unit BytecodeTypes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fgl;

type
  // Bytecode operation codes
  TByteCodeOp = (
    BC_NOP,           // No operation
    BC_HALT,          // Stop execution
    BC_LOAD_INT,      // Load integer constant
    BC_LOAD_STRING,   // Load string constant
    BC_LOAD_VAR,      // Load variable value
    BC_STORE_VAR,     // Store variable value
    BC_ASSIGN,        // Assignment operation
    BC_ADD,           // Addition
    BC_SUB,           // Subtraction
    BC_MUL,           // Multiplication
    BC_DIV,           // Division
    BC_PRINT,         // Print to console
    BC_INPUT,         // Read input
    BC_JUMP,          // Unconditional jump
    BC_JUMP_IF_FALSE, // Conditional jump
    BC_CALL,          // Call subroutine
    BC_RETURN,        // Return from subroutine
    BC_CONCAT,        // String concatenation (&)
    BC_NEG,           // Unary negation
    BC_NOT,           // Logical NOT
    BC_CMP_EQ,        // =
    BC_CMP_NEQ,       // <>
    BC_CMP_LT,        // <
    BC_CMP_GT,        // >
    BC_CMP_LE,        // <=
    BC_CMP_GE,        // >=
    BC_POP,           // Discard the top of the evaluation stack
    BC_PROCESS        // Spawn an OS process (PROCESS statement)
  );

  // Bytecode instruction structure
  TBCInstruction = record
    OpCode: TByteCodeOp;
    Operand1: Integer;
    Operand2: Integer;
    Operand3: Integer;
  end;

  // Instruction array type
  TBCInstructionArray = array of TBCInstruction;

  // Runtime value type used by BytecodeVM (the tree-walking VB6 VM, distinct
  // from the TByteCodeOp instruction stream above)
  TBCValueType = (bcvtNull, bcvtInteger, bcvtString, bcvtBoolean);

  TBCValue = record
    ValueType: TBCValueType;
    IntValue: Int64;
    StringValue: String;
    BoolValue: Boolean;
  end;

  TVMVariable = TBCValue;

  // Integer literal array type
  TIntegerLiteralArray = array of Int64;

  /// String map for variables and constants
  TStringIntMap = specialize TFPGMap<string, Integer>;

  /// Bytecode program structure
  TByteCodeProgram = class
  private
    FProgramTitle: string;
    FInstructions: TBCInstructionArray;
    FVariables: TStringList;
    FStringConstants: TStringList;
    FVariableMap: TStringIntMap;
    FStringMap: TStringIntMap;
    FStringLiterals: TStringList;      /// For CLI compatibility
    FIntegerLiterals: TIntegerLiteralArray;  /// For CLI compatibility
    FSubroutineMap: TStringIntMap;     /// For CLI compatibility
    FFormMap: TStringIntMap;           /// For CLI compatibility
  public
    constructor Create;
    destructor Destroy; override;

    // Add a variable and return its index
    function AddVariable(const VarName: string): Integer;

    // Add a string constant and return its index
    function AddStringConstant(const Literal: string): Integer;

    // Add an integer literal and return its index
    function AddIntegerLiteral(const Value: Int64): Integer;

    // File I/O
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);

    // Properties
    property ProgramTitle: string read FProgramTitle write FProgramTitle;
    property Instructions: TBCInstructionArray read FInstructions write FInstructions;
    property Variables: TStringList read FVariables;
    property StringConstants: TStringList read FStringConstants;

    // Additional properties for CLI compatibility
    property StringLiterals: TStringList read FStringLiterals;
    property IntegerLiterals: TIntegerLiteralArray read FIntegerLiterals write FIntegerLiterals;
    property VariableMap: TStringIntMap read FVariableMap;
    property SubroutineMap: TStringIntMap read FSubroutineMap;
    property FormMap: TStringIntMap read FFormMap;
  end;

function CreateBCValueNull: TBCValue;
function CreateBCValueInteger(A: Int64): TBCValue;
function CreateBCValueString(const S: String): TBCValue;
function CreateBCValueBoolean(B: Boolean): TBCValue;
function BCValueToString(const AValue: TBCValue): String;

implementation

function CreateBCValueNull: TBCValue;
begin
  Result.ValueType := bcvtNull;
  Result.IntValue := 0;
  Result.StringValue := '';
  Result.BoolValue := False;
end;

function CreateBCValueInteger(A: Int64): TBCValue;
begin
  Result := CreateBCValueNull;
  Result.ValueType := bcvtInteger;
  Result.IntValue := A;
end;

function CreateBCValueString(const S: String): TBCValue;
begin
  Result := CreateBCValueNull;
  Result.ValueType := bcvtString;
  Result.StringValue := S;
end;

function CreateBCValueBoolean(B: Boolean): TBCValue;
begin
  Result := CreateBCValueNull;
  Result.ValueType := bcvtBoolean;
  Result.BoolValue := B;
end;

function BCValueToString(const AValue: TBCValue): String;
begin
  case AValue.ValueType of
    bcvtNull: Result := 'NULL';
    bcvtInteger: Result := IntToStr(AValue.IntValue);
    bcvtString: Result := AValue.StringValue;
    bcvtBoolean: Result := BoolToStr(AValue.BoolValue, True);
  end;
end;

{ TByteCodeProgram }

constructor TByteCodeProgram.Create;
begin
  inherited Create;
  FProgramTitle := '';
  SetLength(FInstructions, 0);
  SetLength(FIntegerLiterals, 0);
  FVariables := TStringList.Create;
  FStringConstants := TStringList.Create;
  FStringLiterals := TStringList.Create;
  FVariableMap := TStringIntMap.Create;
  FStringMap := TStringIntMap.Create;
  FSubroutineMap := TStringIntMap.Create;
  FFormMap := TStringIntMap.Create;
end;

destructor TByteCodeProgram.Destroy;
begin
  FVariables.Free;
  FStringConstants.Free;
  FStringLiterals.Free;
  FVariableMap.Free;
  FStringMap.Free;
  FSubroutineMap.Free;
  FFormMap.Free;
  inherited Destroy;
end;

function TByteCodeProgram.AddVariable(const VarName: string): Integer;
begin
  if FVariableMap.IndexOf(VarName) >= 0 then
  begin
    Result := FVariableMap.KeyData[VarName];
    Exit;
  end;

  Result := FVariables.Add(VarName);
  FVariableMap.Add(VarName, Result);
end;

function TByteCodeProgram.AddStringConstant(const Literal: string): Integer;
begin
  Result := FStringLiterals.IndexOf(Literal);
  if Result >= 0 then
    Exit;

  Result := FStringLiterals.Add(Literal);

  // Kept in sync with StringLiterals for older code paths that read
  // StringConstants directly.
  if FStringConstants.IndexOf(Literal) < 0 then
    FStringConstants.Add(Literal);
end;

function TByteCodeProgram.AddIntegerLiteral(const Value: Int64): Integer;
begin
  Result := Length(FIntegerLiterals);
  SetLength(FIntegerLiterals, Result + 1);
  FIntegerLiterals[Result] := Value;
end;

procedure TByteCodeProgram.SaveToFile(const FileName: string);
const
  MagicNumber: array[0..3] of Char = ('K', 'B', 'C', 'F');
var
  F: File;
  I: Integer;
  StrLen: Integer;
  InstrCount: Integer;
  VarCount: Integer;
  ConstCount: Integer;
  TempStr: AnsiString;
begin
  AssignFile(F, FileName);
  try
    Rewrite(F, 1);

    // Write magic number for file validation
    BlockWrite(F, MagicNumber, 4);

    // Write program title
    TempStr := AnsiString(FProgramTitle);
    StrLen := Length(TempStr);
    BlockWrite(F, StrLen, SizeOf(Integer));
    if StrLen > 0 then
      BlockWrite(F, TempStr[1], StrLen);

    // Write instructions
    InstrCount := Length(FInstructions);
    BlockWrite(F, InstrCount, SizeOf(Integer));
    if InstrCount > 0 then
    begin
      for I := 0 to InstrCount - 1 do
      begin
        BlockWrite(F, FInstructions[I].OpCode, SizeOf(TByteCodeOp));
        BlockWrite(F, FInstructions[I].Operand1, SizeOf(Integer));
        BlockWrite(F, FInstructions[I].Operand2, SizeOf(Integer));
        BlockWrite(F, FInstructions[I].Operand3, SizeOf(Integer));
      end;
    end;

    // Write variables
    VarCount := FVariables.Count;
    BlockWrite(F, VarCount, SizeOf(Integer));
    for I := 0 to VarCount - 1 do
    begin
      TempStr := AnsiString(FVariables[I]);
      StrLen := Length(TempStr);
      BlockWrite(F, StrLen, SizeOf(Integer));
      if StrLen > 0 then
        BlockWrite(F, TempStr[1], StrLen);
    end;

    // Write string constants
    ConstCount := FStringLiterals.Count;
    BlockWrite(F, ConstCount, SizeOf(Integer));
    for I := 0 to ConstCount - 1 do
    begin
      TempStr := AnsiString(FStringLiterals[I]);
      StrLen := Length(TempStr);
      BlockWrite(F, StrLen, SizeOf(Integer));
      if StrLen > 0 then
        BlockWrite(F, TempStr[1], StrLen);
    end;

    WriteLn('DEBUG: Saved ', InstrCount, ' instructions, ', VarCount, ' variables, ', ConstCount, ' string constants');

  finally
    CloseFile(F);
  end;
end;

procedure TByteCodeProgram.LoadFromFile(const FileName: string);
var
  F: File;
  I: Integer;
  StrLen: Integer;
  InstrCount: Integer;
  VarCount: Integer;
  ConstCount: Integer;
  Magic: array[0..3] of Char;
  TempStr: AnsiString;
  TempInstr: TBCInstruction;
  TempInstructions: TBCInstructionArray;
begin
  AssignFile(F, FileName);
  try
    Reset(F, 1);

    // Read and validate magic number
    BlockRead(F, Magic, 4);
    if (Magic[0] <> 'K') or (Magic[1] <> 'B') or (Magic[2] <> 'C') or (Magic[3] <> 'F') then
      raise Exception.Create('Invalid bytecode file format');

    // Read program title
    BlockRead(F, StrLen, SizeOf(Integer));
    if StrLen > 0 then
    begin
      SetLength(TempStr, StrLen);
      BlockRead(F, TempStr[1], StrLen);
      FProgramTitle := String(TempStr);
    end;

    // Read instructions
    BlockRead(F, InstrCount, SizeOf(Integer));
    SetLength(TempInstructions, InstrCount);
    for I := 0 to InstrCount - 1 do
    begin
      BlockRead(F, TempInstr.OpCode, SizeOf(TByteCodeOp));
      BlockRead(F, TempInstr.Operand1, SizeOf(Integer));
      BlockRead(F, TempInstr.Operand2, SizeOf(Integer));
      BlockRead(F, TempInstr.Operand3, SizeOf(Integer));
      TempInstructions[I] := TempInstr;
    end;
    FInstructions := TempInstructions;

    // Read variables
    BlockRead(F, VarCount, SizeOf(Integer));
    FVariables.Clear;
    FVariableMap.Clear;
    for I := 0 to VarCount - 1 do
    begin
      BlockRead(F, StrLen, SizeOf(Integer));
      if StrLen > 0 then
      begin
        SetLength(TempStr, StrLen);
        BlockRead(F, TempStr[1], StrLen);
        FVariables.Add(String(TempStr));
        FVariableMap.Add(String(TempStr), I);
      end;
    end;

    // Read string constants
    BlockRead(F, ConstCount, SizeOf(Integer));
    FStringLiterals.Clear;
    FStringConstants.Clear;
    for I := 0 to ConstCount - 1 do
    begin
      BlockRead(F, StrLen, SizeOf(Integer));
      if StrLen > 0 then
      begin
        SetLength(TempStr, StrLen);
        BlockRead(F, TempStr[1], StrLen);
        FStringLiterals.Add(String(TempStr));
        FStringConstants.Add(String(TempStr));
      end;
    end;

  finally
    CloseFile(F);
  end;
end;

end.
