unit BytecodeTypes;

{$mode objfpc}{$H+}
{$modeswitch TypeHelpers-}    // disable implicit helpers just for this unit

interface

uses
  SysUtils,                // IntToStr, BoolToStr …
  Classes,                 // TStringList
  fgl;                     // TFPGMap (generics)

{ ---------------------------------------------------------------------------
  ▸  TYPE DECLARATIONS
  --------------------------------------------------------------------------- }
type
  {--- 1. Value-type enum ---}
  TBCValueType = (bcvtNull, bcvtInteger, bcvtString, bcvtBoolean);

  {--- 2. Runtime value record ---}
  TBCValue = record
    ValueType   : TBCValueType;
    IntValue    : Int64;
    StringValue : String;
    BoolValue   : Boolean;
  end;

  {--- 3. VM variable alias ---}
  TVMVariable = TBCValue;

  {--- 4. Variable wrapper object ---}
  TVMVariableObject = class(TObject)
    Variable: TBCValue;
    constructor Create;
    destructor Destroy; override;
  end;

  {--- 5. String → Int maps ---}
  TStringIntMap = specialize TFPGMap<AnsiString, LongInt>;

  {--- 6. Opcodes ---}
  TOpCode = (
    OP_PUSH_INT, OP_PUSH_STRING, OP_PUSH_VAR, OP_POP_VAR, OP_POP,
    OP_ADD_INT, OP_SUB_INT, OP_MUL_INT, OP_DIV_INT, OP_ADD_STRING,
    OP_EQUAL, OP_NOT_EQUAL, OP_GREATER, OP_LESS,
    OP_GREATER_EQUAL, OP_LESS_EQUAL,
    OP_AND, OP_OR, OP_NOT,
    OP_JUMP, OP_JUMP_IF_FALSE, OP_CALL, OP_RETURN, OP_HALT,
    OP_DECL_VAR, OP_PRINT, OP_INPUT, OP_SHOW_FORM,
    OP_CASE_COND,      // For SELECT CASE condition evaluation
    OP_ENDCASE,        // Marker for END SELECT
    OP_FOREACH_INIT,   // Initialize FOREACH loop (Operand1: loop var index, Operand2: collection index)
    OP_FOREACH_ITER,   // Iterate FOREACH loop (Operand1: loop var index, Operand2: jump if end)
    OP_FOREACH_END,    // Marker for END FOREACH
    OP_CALL_PROC,      // Call a procedure/function (Operand1: target address)
    OP_RETURN_PROC,    // Return from a procedure/function
    OP_FORM_START,     // Start of a FORM definition block (Operand1: Form ID/Index)
    OP_FORM_END ,       // End of a FORM definition block
    OP_KEY_PRESSED,   // <<< NEW: Check if a key is pressed (non-blocking)
    OP_READ_KEY       // <<< NEW: Read a key (blocking)

  );

  {--- 7. Instruction ---}
  TBCInstruction = record
    OpCode   : TOpCode;
    Operand1 : LongInt;
    Operand2 : LongInt;
  end;

  {--- 8. Byte-code program container ---}
  TByteCodeProgram = class(TObject)
  public
    Instructions    : array of TBCInstruction;
    StringLiterals  : TStringList;
    IntegerLiterals : array of Int64;
    VariableMap     : TStringIntMap;
    SubroutineMap   : TStringIntMap;
    FormMap         : TStringIntMap;
    ProgramTitle    : String;

    constructor Create;
    destructor  Destroy; override;
  end;

{ ---------------------------------------------------------------------------
  ▸  HELPER CREATORS
  --------------------------------------------------------------------------- }
function CreateBCValueNull         : TBCValue;
function CreateBCValueInteger(A: Int64)  : TBCValue;
function CreateBCValueString (const S: String): TBCValue;
function CreateBCValueBoolean(B: Boolean): TBCValue;

{ ---------------------------------------------------------------------------
  ▸  GLOBAL HELPER FUNCTION FOR CONVERSION
  --------------------------------------------------------------------------- }
function BCValueToString(const AValue: TBCValue): String;
function GetBCValueTypeName(AType: TBCValueType): String; // <<< NEW FUNCTION DECLARATION

implementation
{ --------------------------------------------------------------------------- }

{ TVMVariableObject }
constructor TVMVariableObject.Create;
begin
  inherited Create;
  Variable := CreateBCValueNull;
end;

destructor TVMVariableObject.Destroy;
begin
  inherited Destroy;
end;


{ global creators }
function CreateBCValueNull: TBCValue;
begin
  Result.ValueType    := bcvtNull;
  Result.IntValue     := 0;
  Result.StringValue  := '';
  Result.BoolValue    := False;
end;

function CreateBCValueInteger(A: Int64): TBCValue;
begin
  Result := CreateBCValueNull;
  Result.ValueType := bcvtInteger;
  Result.IntValue  := A;
end;

function CreateBCValueString(const S: String): TBCValue;
begin
  Result := CreateBCValueNull;
  Result.ValueType    := bcvtString;
  Result.StringValue := S;
end;

function CreateBCValueBoolean(B: Boolean): TBCValue;
begin
  Result := CreateBCValueNull;
  Result.ValueType := bcvtBoolean;
  Result.BoolValue := B;
end;

{ TByteCodeProgram }
constructor TByteCodeProgram.Create;
begin
  inherited Create;
  StringLiterals  := TStringList.Create;
  VariableMap     := TStringIntMap.Create;
  SubroutineMap   := TStringIntMap.Create;
  FormMap         := TStringIntMap.Create;
  SetLength(Instructions, 0);
  SetLength(IntegerLiterals, 0);
  ProgramTitle := 'Kayte Application';
end;

destructor TByteCodeProgram.Destroy;
begin
  StringLiterals.Free;
  VariableMap   .Free;
  SubroutineMap .Free;
  FormMap       .Free;
  inherited Destroy;
end;

{ GLOBAL HELPER FUNCTION FOR CONVERSION }
function BCValueToString(const AValue: TBCValue): String;
begin
  case AValue.ValueType of
    bcvtNull    : Result := 'NULL';
    bcvtInteger : Result := IntToStr(AValue.IntValue);
    bcvtString  : Result := AValue.StringValue;
    bcvtBoolean : Result := BoolToStr(AValue.BoolValue, True);
  end;
end;

// <<< NEW FUNCTION IMPLEMENTATION
function GetBCValueTypeName(AType: TBCValueType): String;
begin
  case AType of
    bcvtNull: Result := 'Null';
    bcvtInteger: Result := 'Integer';
    bcvtString: Result := 'String';
    bcvtBoolean: Result := 'Boolean';
  end;
end;
// End NEW FUNCTION IMPLEMENTATION

end.

