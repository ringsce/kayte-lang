unit BytecodeTypes;

{$mode objfpc}{$H+}
{$modeswitch TypeHelpers-}   // disable implicit helpers just for this unit

interface

uses
  SysUtils,            // IntToStr, BoolToStr …
  Classes,             // TStringList
  fgl;                 // TFPGMap (generics)

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
    function AsString: String;
  end; // <<< NO SEMICOLON HERE. This is crucial for correct syntax when other types follow.

  {--- 3. VM variable alias ---}
  TVMVariable = TBCValue;

  {--- 4. Variable wrapper object ---}
  TVMVariableObject = class(TObject) // Explicitly inherit from TObject
    Variable: TBCValue;
    constructor Create;
    destructor Destroy; override; // Must be override if inheriting from TObject
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
    OP_DECL_VAR, OP_PRINT, OP_INPUT, OP_SHOW_FORM
  );

  {--- 7. Instruction ---}
  TBCInstruction = record
    OpCode   : TOpCode;
    Operand1 : LongInt;
    Operand2 : LongInt;
  end;

  {--- 8. Byte-code program container ---}
  TByteCodeProgram = class(TObject) // Explicitly inherit from TObject
  public
    Instructions    : array of TBCInstruction;
    StringLiterals  : TStringList;
    IntegerLiterals : array of Int64;
    VariableMap     : TStringIntMap;
    SubroutineMap   : TStringIntMap;
    FormMap         : TStringIntMap;
    constructor Create;
    destructor  Destroy; override;
  end; // <<< ONLY THIS LAST 'end;' IN THE ENTIRE TYPE BLOCK GETS THE SEMICOLON

{ ---------------------------------------------------------------------------
  ▸  HELPER CREATORS
  --------------------------------------------------------------------------- }
function CreateBCValueNull        : TBCValue;
function CreateBCValueInteger(A: Int64)  : TBCValue;
function CreateBCValueString (const S: String): TBCValue;
function CreateBCValueBoolean(B: Boolean): TBCValue;

implementation
{ --------------------------------------------------------------------------- }

{ TBCValue helpers }
function TBCValue.AsString: String;
begin
  case ValueType of
    bcvtNull    : Result := 'NULL';
    bcvtInteger : Result := IntToStr(IntValue);
    bcvtString  : Result := StringValue;
    bcvtBoolean : Result := BoolToStr(BoolValue, True);
  end;
end;


{ TVMVariableObject }
constructor TVMVariableObject.Create;
begin
  inherited Create; // Call inherited constructor for TObject
  Variable := CreateBCValueNull; // Initialize the record
end;

destructor TVMVariableObject.Destroy;
begin
  // No explicit Dispose needed for Variable.StringValue as String is managed by FPC.
  // CreateBCValueNull initializes the record, so no pointer to free.
  inherited Destroy; // Call inherited destructor for TObject
end;


{ global creators }
function CreateBCValueNull: TBCValue;
begin
  Result.ValueType   := bcvtNull;
  Result.IntValue    := 0;
  Result.StringValue := '';
  Result.BoolValue   := False;
end;

function CreateBCValueInteger(A: Int64): TBCValue;
begin
  Result := CreateBCValueNull; // Initialize all fields
  Result.ValueType := bcvtInteger;
  Result.IntValue  := A;
end;

function CreateBCValueString(const S: String): TBCValue;
begin
  Result := CreateBCValueNull; // Initialize all fields
  Result.ValueType   := bcvtString;
  Result.StringValue := S;
end;

function CreateBCValueBoolean(B: Boolean): TBCValue;
begin
  Result := CreateBCValueNull; // Initialize all fields
  Result.ValueType := bcvtBoolean;
  Result.BoolValue := B;
end;

{ TByteCodeProgram }
constructor TByteCodeProgram.Create;
begin
  inherited Create; // Call inherited constructor for TObject
  StringLiterals  := TStringList.Create;
  VariableMap     := TStringIntMap.Create;
  SubroutineMap   := TStringIntMap.Create;
  FormMap         := TStringIntMap.Create;
  SetLength(Instructions, 0); // Initialize dynamic array
  SetLength(IntegerLiterals, 0); // Initialize dynamic array
end;

destructor TByteCodeProgram.Destroy;
begin
  // Free owned objects (TStringList and TFPGMap instances)
  StringLiterals.Free;
  VariableMap   .Free;
  SubroutineMap .Free;
  FormMap       .Free;
  // Dynamic arrays (Instructions, IntegerLiterals) are automatically managed by FPC.
  inherited Destroy; // Call inherited destructor last
end;

end.

