unit BytecodeTypes;

interface
uses
  SysUtils, // For PString, FreeAndNil, Dispose, SetLength
  Classes, fgl, tokenDefs;  // For TStringList, TObject (Note: Generics.Collections is no longer needed if TList<T> is removed)


type // <--- ALL TYPE DECLARATIONS START HERE AND END WITH THE LAST TYPE
  // 1. Unified value-type enum
  TBCValueType = (
    bcvtNull,
    bcvtInteger,
    bcvtString,
    bcvtBoolean
  );

  // 2. String pointer used inside VM values
  PBCString = ^AnsiString;

  // 3. Runtime value structure (record)
  TBCValue = record
    case ValueType: TBCValueType of
      bcvtNull: ();
      bcvtInteger: (IntValue: Int64);
      bcvtString: (StrValuePtr: PBCString);
      bcvtBoolean: (BoolValue: Boolean);
  end; // <--- SEMICOLON HERE, as more types follow

  // 4. Alias for VM variable type
  TVMVariable = TBCValue; // This is an alias, no 'end;' needed

  // 5. Variable object wrapper (class)
  TVMVariableObject = class
    Variable: TBCValue;
    constructor Create;
    destructor Destroy; override;
  end; // <--- SEMICOLON HERE, as more types follow

  // 6. Subroutine map – String → Integer (specialized map)
  TStringIntMap = specialize TFPGMap<AnsiString, LongInt>; // <--- SEMICOLON HERE, as more types follow

  // 7. The enum for opcodes
  TOpCode = (
    OP_PUSH_INT, OP_PUSH_STRING, OP_PUSH_VAR, OP_POP_VAR, OP_POP,
    OP_ADD_INT, OP_SUB_INT, OP_MUL_INT, OP_DIV_INT, OP_ADD_STRING,
    OP_EQUAL, OP_NOT_EQUAL, OP_GREATER, OP_LESS,
    OP_GREATER_EQUAL, OP_LESS_EQUAL,
    OP_AND, OP_OR, OP_NOT,
    OP_JUMP, OP_JUMP_IF_FALSE, OP_CALL, OP_RETURN, OP_HALT,
    OP_DECL_VAR, OP_PRINT, OP_INPUT, OP_SHOW_FORM
  ); // <--- SEMICOLON HERE, as more types follow

  // 8. Instruction format (record)
  TBCInstruction = record
    OpCode: TOpCode;
    Operand1: Integer;
    Operand2: Integer;
  end;


  // 9. Byte-code program class
  TByteCodeProgram = class
  public
    Instructions: array of TBCInstruction;
    StringLiterals: TStringList;
    IntegerLiterals: array of Int64;
    VariableMap: TStringIntMap;
    SubroutineMap: TStringIntMap;
    FormMap: TStringIntMap;

    constructor Create;
    destructor Destroy; override;
  end; // <--- NO SEMICOLON HERE, as this is the last type declaration in the 'type' block

// 3. GLOBAL FUNCTION DECLARATIONS GO THIRD (AFTER the 'type' block)
(*function CreateBCValueInteger(AValue: Int64): TBCValue;
function CreateBCValueString(const AValue: String): TBCValue;
function CreateBCValueBoolean(AValue: Boolean): TBCValue;
function CreateBCValueNull: TBCValue;   *)


// --- Implementation for Token Types ---
// If GetTokenType is truly a standalone function, it belongs here.
implementation

{ TVMVariableObject }

constructor TVMVariableObject.Create;
begin
  inherited Create;
  Variable.ValueType := bcvtNull;
end;

destructor TVMVariableObject.Destroy;
begin
  if Variable.ValueType = bcvtString then
    Dispose(Variable.StrValuePtr);
  inherited Destroy;
end;


// However, this kind of logic is *usually* a method of the TLexer class.
// If it's a TLexer method, its implementation should be in Lexer.pas.
function GetTokenType(const S: String): TTokenType;
begin
  Result := tkIdentifier; // Default to identifier

  // Convert to uppercase for case-insensitive comparison (VB6 style)
  case AnsiUpperCase(S) of
    // Keywords
    'REM', 'END', 'SUB', 'FUNCTION', 'IF', 'THEN', 'ELSE', 'ELSEIF', 'ENDIF',
    'SELECT', 'CASE', 'END SELECT', 'WHILE', 'WEND', 'FOR', 'NEXT', 'TO', 'STEP',
    'DIM', 'AS', 'REDIM', 'PRESERVE', 'CALL', 'GOTO', 'GOSUB', 'RETURN',
    'PRINT', 'INPUT', 'MSGBOX', 'FORM', 'END FORM', 'SHOW', 'HIDE':
      Result := tkKeyword;
    // Boolean Literals
    'TRUE', 'FALSE':
      Result := tkBooleanLiteral;
    // Operators (basic ones; full list would be larger)
    '+', '-', '*', '/', '=', '<', '>', '<=', '>=', '<>', '&': // '&' for string concat
      Result := tkOperator;
    'AND', 'OR', 'NOT', 'IS': // Logical/comparison operators as keywords
      Result := tkOperator; // Or keep as tkKeyword if you want to distinguish
    '(': Result := tkParenthesisOpen;
    ')': Result := tkParenthesisClose;
    ',': Result := tkComma;
    '.': Result := tkDot;
    ':': Result := tkColon;
  end;
end;

// --- Implementation for TByteCodeProgram methods ---
constructor TByteCodeProgram.Create;
begin
  inherited;
  StringLiterals := TStringList.Create;
  SetLength(IntegerLiterals, 0);
  VariableMap := TStringIntMap.Create;
  SubroutineMap := TStringIntMap.Create;
  FormMap := TStringIntMap.Create;
  SetLength(Instructions, 0);
end;

destructor TByteCodeProgram.Destroy;
begin
  // For dynamic arrays, FPC automatically manages memory if they are fields of a class or local vars.
  // No explicit FreeAndNil(IntegerLiterals) is needed for dynamic arrays of simple types.
  // If IntegerLiterals was 'array of PInteger', you'd need to iterate and Dispose each PInteger.

  FreeAndNil(StringLiterals);
  // FreeAndNil(IntegerLiterals); // Not needed for dynamic array of Integer
  FreeAndNil(VariableMap);
  FreeAndNil(SubroutineMap);
  FreeAndNil(FormMap);

  inherited; // Call inherited destructor last
end;

end.
