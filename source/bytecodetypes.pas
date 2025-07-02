unit BytecodeTypes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, // For IntToStr, BoolToStr, etc.
  Classes,  // For TStringList, TObject
  fgl;      // For TFPGMap (generics)

type // --- ALL TYPE DECLARATIONS START HERE ---
  // 1. Unified value-type enum
  TBCValueType = (
    bcvtNull,
    bcvtInteger,
    bcvtString,
    bcvtBoolean
  );

  // 2. Runtime value structure (record) - NO LONGER A VARIANT RECORD
  //    Uses direct StringValue for simpler memory management.
  TBCValue = record
    ValueType: TBCValueType;
    IntValue: Int64;
    StringValue: String; // <<< DIRECT STRING STORAGE, NO POINTER
    BoolValue: Boolean;

    // Helper method to get the string representation of the value
    function AsString: String;
  end;

  // 3. Alias for VM variable type
  TVMVariable = TBCValue;

  // 4. Variable object wrapper (class) - If you need to wrap TBCValue in an object
  //    Adjusted constructor/destructor for direct StringValue in TBCValue
  TVMVariableObject = class(TObject) // Inherit from TObject
    Variable: TBCValue;
    constructor Create;
    destructor Destroy; override;
  end;

  // 5. Subroutine map – String → Integer (specialized map from FGL)
  TStringIntMap = specialize TFPGMap<AnsiString, LongInt>;

  // 6. The enum for opcodes
  TOpCode = (
    OP_PUSH_INT, OP_PUSH_STRING, OP_PUSH_VAR, OP_POP_VAR, OP_POP,
    OP_ADD_INT, OP_SUB_INT, OP_MUL_INT, OP_DIV_INT, OP_ADD_STRING,
    OP_EQUAL, OP_NOT_EQUAL, OP_GREATER, OP_LESS,
    OP_GREATER_EQUAL, OP_LESS_EQUAL,
    OP_AND, OP_OR, OP_NOT,
    OP_JUMP, OP_JUMP_IF_FALSE, OP_CALL, OP_RETURN, OP_HALT,
    OP_DECL_VAR, OP_PRINT, OP_INPUT, OP_SHOW_FORM
  );

  // 7. Instruction format (record)
  TBCInstruction = record
    OpCode: TOpCode;
    Operand1: Integer;
    Operand2: Integer;
  end;

  // 8. Byte-code program class
  TByteCodeProgram = class(TObject) // Inherit from TObject
  public
    Instructions: array of TBCInstruction;
    StringLiterals: TStringList;
    IntegerLiterals: array of Int64;
    VariableMap: TStringIntMap;
    SubroutineMap: TStringIntMap;
    FormMap: TStringIntMap;

    constructor Create;
    destructor Destroy; override;
  end;
// --- ALL TYPE DECLARATIONS END HERE ---


// --- GLOBAL FUNCTION DECLARATIONS ---
// These are helper functions to create TBCValue instances (like constructors for records)
function CreateBCValueNull: TBCValue;
function CreateBCValueInteger(AValue: Int64): TBCValue; // Changed to Int64 to match TBCValue.IntValue
function CreateBCValueString(const AValue: String): TBCValue;
function CreateBCValueBoolean(AValue: Boolean): TBCValue;


implementation

{ TBCValue Record Methods }

function TBCValue.AsString: String;
begin
  case ValueType of
    bcvtNull: Result := 'NULL';
    bcvtInteger: Result := IntToStr(IntValue);
    bcvtString: Result := StringValue;
    bcvtBoolean: Result := BoolToStr(BoolValue, True); // True for 'True'/'False' strings
    else Result := '';
  end;
end;

{ TVMVariableObject }

constructor TVMVariableObject.Create;
begin
  inherited Create;
  // Initialize the TBCValue record directly
  Variable.ValueType := bcvtNull;
  Variable.IntValue := 0;
  Variable.StringValue := ''; // Initialize string
  Variable.BoolValue := False;
end;

destructor TVMVariableObject.Destroy;
begin
  // No explicit Dispose needed for Variable.StringValue as String is managed by FPC
  inherited Destroy;
end;


{ Global TBCValue Creation Functions }

function CreateBCValueNull: TBCValue;
begin
  Result.ValueType := bcvtNull;
  Result.IntValue := 0;
  Result.StringValue := '';
  Result.BoolValue := False;
end;

function CreateBCValueInteger(AValue: Int64): TBCValue;
begin
  Result.ValueType := bcvtInteger;
  Result.IntValue := AValue;
  Result.StringValue := '';
  Result.BoolValue := False;
end;

function CreateBCValueString(const AValue: String): TBCValue;
begin
  Result.ValueType := bcvtString;
  Result.IntValue := 0;
  Result.StringValue := AValue;
  Result.BoolValue := False;
end;

function CreateBCValueBoolean(AValue: Boolean): TBCValue;
begin
  Result.ValueType := bcvtBoolean;
  Result.IntValue := 0;
  Result.StringValue := '';
  Result.BoolValue := AValue;
end;


{ TByteCodeProgram methods }

constructor TByteCodeProgram.Create;
begin
  inherited Create; // Call inherited constructor for TObject
  StringLiterals := TStringList.Create;
  SetLength(IntegerLiterals, 0); // Initialize dynamic array
  VariableMap := TStringIntMap.Create;
  SubroutineMap := TStringIntMap.Create;
  FormMap := TStringIntMap.Create;
  SetLength(Instructions, 0); // Initialize dynamic array
end;

destructor TByteCodeProgram.Destroy;
begin
  // Free owned objects
  FreeAndNil(StringLiterals);
  FreeAndNil(VariableMap);
  FreeAndNil(SubroutineMap);
  FreeAndNil(FormMap);

  // Dynamic arrays of simple types (like TBCInstruction, Int64) are managed by FPC.
  // No explicit FreeAndNil or SetLength(..., 0) is strictly needed here for them
  // if they are class fields, as the class destructor handles their memory.

  inherited Destroy; // Call inherited destructor last
end;

end.

