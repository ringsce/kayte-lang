unit BytecodeTypes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fgl;

const
  // Built-in functions (BC_BUILTIN's Operand1). The numbers are shared with
  // the native runtime (source/native/kayte_native_rt.c) - keep them in sync.
  BI_LEN = 1; BI_LEFT = 2; BI_RIGHT = 3; BI_MID = 4; BI_UCASE = 5; BI_LCASE = 6;
  BI_TRIM = 7; BI_LTRIM = 8; BI_RTRIM = 9; BI_INSTR = 10; BI_REPLACE = 11; BI_STR = 12;
  BI_VAL = 13; BI_CHR = 14; BI_ASC = 15; BI_SPACE = 16; BI_ABS = 17; BI_SGN = 18;
  BI_MIN = 19; BI_MAX = 20; BI_UBOUND = 21; BI_LBOUND = 22; BI_ARRAY = 23; BI_JOIN = 24;
  BI_SPLIT = 25; BI_TYPENAME = 26; BI_ISARRAY = 27; BI_NEWARRAY = 28; BI_RESIZE = 29;
  BI_NEWOBJECT = 30; BI_ISNUMERIC = 31; BI_CINT = 32; BI_RND = 33;
  BI_POW = 34; BI_SQR = 35; BI_STRING = 36; BI_TIMER = 37; BI_DATE = 38; BI_TIME = 39;
  BI_HEX = 40; BI_OCT = 41; BI_SLEEP = 42; BI_INKEY = 43; BI_QBSTR = 44;
  BI_INT = 45; BI_FIX = 46; BI_CDBL = 47; BI_ROUND = 48; BI_SIN = 49; BI_COS = 50; BI_TAN = 51;
  BI_ATN = 52; BI_EXP = 53; BI_LOG = 54;
  BI_USING = 55; BI_FOPEN = 56; BI_FCLOSE = 57; BI_FPRINT = 58; BI_FREADLINE = 59; BI_FREADFIELD = 60;
  BI_EOF = 61; BI_FREEFILE = 62; BI_LOF = 63; BI_KILL = 64; BI_NAME = 65;
  BI_FGET = 66; BI_FPUT = 67; BI_FSEEK = 68; BI_FSEEKPOS = 69; BI_FLOC = 70; BI_FINPUTS = 71;
  BI_MKI = 72; BI_MKL = 73; BI_MKS = 74; BI_MKD = 75; BI_CVI = 76; BI_CVL = 77; BI_CVS = 78; BI_CVD = 79;
  // QuickBASIC: bitwise AND / OR / XOR / NOT, error numbers (ERR) and
  // messages (ERROR n), MID$ = , LSET / RSET
  BI_BAND = 80; BI_BOR = 81; BI_BXOR = 82; BI_BNOT = 83; BI_ERRCODE = 84; BI_ERRMSG = 85;
  BI_MIDSET = 86; BI_LSET = 87; BI_RSET = 88;

  // BC_QPRINT's Operand1: QuickBASIC-style PRINT pieces (no newline unless
  // QP_NEWLINE). The runtime tracks the output column for QP_COMMA / QP_TAB.
  QP_VALUE = 0;   // pops a value: numbers as " 5 " / "-5 ", strings as they are
  QP_COMMA = 1;   // to the next 14-column print zone
  QP_NEWLINE = 2;
  QP_TAB = 3;     // pops n: to column n (1-based), on a new line if past it
  QP_SPC = 4;     // pops n: n spaces
  QP_TEXT = 5;    // pops a value: its text, as it is

  // BC_QT's Operand3: which statement emitted it. QML statements share the
  // QT machinery but have their own command names (see kayte_qt6.cpp).
  QT_STATEMENT_QT = 0;
  QT_STATEMENT_QML = 1;

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
    BC_CALL,          // Call SUB: Operand1 = entry address, Operand2 = argument count
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
    BC_PROCESS,       // Spawn an OS process (PROCESS statement)
    BC_QT,            // Qt6 GUI command (QT or QML statement, see kayte_qt6.pas);
                      // Operand3 = QT_STATEMENT_QT / QT_STATEMENT_QML
    BC_ENTER,         // First instruction of a SUB: Operand1 = its parameter count
    // Arrays (and class objects, which are arrays tagged with their class):
    BC_INDEX_GET,     // pops index, array; pushes array(index)
    BC_INDEX_SET,     // pops value, index, array; array(index) := value
    // Built-in functions: Operand1 = BI_* id, Operand2 = argument count;
    // pops the arguments, pushes the result.
    BC_BUILTIN,
    // TRY / CATCH: BC_TRY's Operand1 is the CATCH address. A runtime error
    // (or THROW) inside restores the stack and call depth to the TRY's,
    // pushes the error message and jumps there.
    BC_TRY,
    BC_TRY_END,       // the TRY block finished normally
    BC_THROW,         // pops a message and raises it as a runtime error
    BC_QPRINT,        // a piece of a QuickBASIC PRINT: Operand1 = QP_* (BC_INPUT pushes a line read from stdin)
    BC_LOAD_FLOAT,    // a double: Operand1 = low 32 bits, Operand2 = high 32 bits of its IEEE 754 form
    BC_IDIV,          // \ : whole-number division (operands rounded, result truncated)
    BC_MOD            // MOD: the remainder, with the sign of the left side
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
  // "Hello" and "hello" are different strings.
  FStringConstants.CaseSensitive := True;
  FStringLiterals.CaseSensitive := True;
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
