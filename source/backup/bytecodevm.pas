unit BytecodeVM;

interface

uses
  SysUtils, Classes, Contnrs, Forms, // For TStringList, TObjectList, TForm, etc.
  BytecodeTypes, fgl; // Our defined opcodes and structures

type

  // Define TIntegerObject here if it's not available globally or from another standard unit,
  // or ensure the unit that defines it is in the uses clause.
  // Example if you need to define it yourself:
  TIntegerObject = class(TObject)
  public
    Value: Integer;
    constructor Create(AValue: Integer);
  end;
  // Represents a runtime variable in the VM's memory
  TVMVariable = record
    Name: String; // <-- This is a String;
    Value: TBCValue;
  end;

  // Call stack frame for GOSUB/RETURN
  TCallFrame = record
    ReturnAddress: Integer; // Instruction pointer to return to
    // Add fields for local variables if you implement local scope
  end;

  // IMPORTANT: Wrapper classes for records to be stored in TObjectList
  // TObjectList can only hold descendants of TObject.
  TBCValueObject = class(TObject)
  public
    Value: TBCValue; // The actual TBCValue record
    constructor Create;
    destructor Destroy; override; // To manage PString in Value
  end;


  TVMVariableObject = class(TObject)
  public
    Variable: TVMVariable; // The actual TVMVariable record
    constructor Create;
    destructor Destroy; override; // To manage PString in Variable.Value
  end;

  TCallFrameObject = class(TObject)
  public
    Frame: TCallFrame;
    constructor Create(AReturnAddress: Integer);
  end;


  TBytecodeVM = class
  private
    FProgram: TByteCodeProgram;
    FInstructionPointer: Integer;
    FOperandStack: TObjectList;
    FCallStack: TObjectList;
    FVariables: TObjectList;
    FRuntimeForms: TObjectList;

    function PopValue: TBCValue;
    procedure PushValue(const AValue: TBCValue);
    function GetVMVariable(VarID: Integer): TBCValue; // Declaration only!
    procedure SetVMVariable(VarID: Integer; const AValue: TBCValue);
    procedure LoadBytecodeFile(const FileName: String);
    procedure InitVMState;
    procedure CleanupVMState;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(const BytecodeFileName: String);
  end;

implementation

// --- TIntegerObject implementation moved here ---
constructor TIntegerObject.Create(AValue: Integer);
begin
  inherited Create;
  Value := AValue;
end;
// ---------------------------------------------


{ TBCValueObject }

constructor TBCValueObject.Create;
begin
  inherited Create;
  // Initialize PString to nil to avoid accessing invalid memory
  Value.StrValuePtr := nil;
end;

destructor TBCValueObject.Destroy;
begin
  // Manually free the PString memory if it was allocated and is a string
  if (Value.ValueType = bcvtString) and Assigned(Value.StrValuePtr) then
  begin
    Dispose(Value.StrValuePtr);
    Value.StrValuePtr := nil;
  end;
  inherited Destroy;
end;

{ TVMVariableObject }

constructor TVMVariableObject.Create;
begin
  inherited Create;
  // Initialize PString in nested TBCValue
  Variable.Value.StrValuePtr := nil;
end;

destructor TVMVariableObject.Destroy;
begin
  // Manually free the PString memory if it was allocated within the variable's value and is a string
  if (Variable.Value.ValueType = bcvtString) and Assigned(Variable.Value.StrValuePtr) then
  begin
    Dispose(Variable.Value.StrValuePtr);
    Variable.Value.StrValuePtr := nil;
  end;
  inherited Destroy;
end;

{ TCallFrameObject }

constructor TCallFrameObject.Create(AReturnAddress: Integer);
begin
  inherited Create;
  Frame.ReturnAddress := AReturnAddress;
end;

{ TBytecodeVM }

constructor TBytecodeVM.Create;
begin
  inherited;
  FProgram := TByteCodeProgram.Create;

  // No OwnsObjects for FProgram.SubroutineMap or FormMap
  FOperandStack := TObjectList.Create(True); // Owns and frees TBCValueObject
  FCallStack := TObjectList.Create(True);    // Owns and frees TCallFrameObject
  FVariables := TObjectList.Create(True);    // Owns and frees TVMVariableObject
  FRuntimeForms := TObjectList.Create(True); // Owns and frees TForm instances
end;

destructor TBytecodeVM.Destroy;
begin
  CleanupVMState; // Ensure any runtime-created objects are freed
  FreeAndNil(FProgram);
  FreeAndNil(FOperandStack);
  FreeAndNil(FCallStack);
  FreeAndNil(FVariables);
  FreeAndNil(FRuntimeForms);
  inherited;
end;

procedure TBytecodeVM.LoadBytecodeFile(const FileName: String);
var
  Stream: TFileStream;
  Reader: TReader;
  i: Integer;
  InstructionCount: Integer;
  StrCount: Integer; // For StringLiterals count
  IntCount: Integer; // For IntegerLiterals count
  VarCount: Integer; // For VariableMap count
  SubCount: Integer; // For SubroutineMap count
  FormCount: Integer; // For FormMap count
  Name: String;
  Index: LongInt;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Reader := TReader.Create(Stream, 4096); // Use a buffer size
    try
      // Read String Literals
      StrCount := Reader.ReadInteger;
      FProgram.StringLiterals.Capacity := StrCount;
      for i := 0 to StrCount - 1 do
        FProgram.StringLiterals.Add(Reader.ReadString);

      // Read Integer Literals (dynamic array)
      IntCount := Reader.ReadInteger;
      SetLength(FProgram.IntegerLiterals, IntCount);
      for i := 0 to IntCount - 1 do
        FProgram.IntegerLiterals[i] := Reader.ReadInteger;

      // Read VariableMap
      VarCount := Reader.ReadInteger;
      FProgram.VariableMap.Capacity := VarCount;
      for i := 0 to VarCount - 1 do
        FProgram.VariableMap.Add(Reader.ReadString);

      // Read SubroutineMap (StringList where objects are line numbers)
      SubCount := Reader.ReadInteger;
      FProgram.SubroutineMap.Capacity := SubCount;
      begin
           for i := 0 to SubCount - 1 do
           begin
           Name := Reader.ReadString;
           Index := Reader.ReadInteger;
           FProgram.SubroutineMap.KeyData[Name] := Index;
           end;

      end;
      // Read FormMap (StringList where objects are line numbers)
      FProgram.FormMap.Sorted := True;
      FormCount := Reader.ReadInteger;
      FProgram.FormMap.Capacity := FormCount;
      for i := 0 to FormCount - 1 do
      begin
        FProgram.FormMap.KeyData[Reader.ReadString] := Reader.ReadInteger;

      end;

      // Read Instructions
      InstructionCount := Reader.ReadInteger;
      SetLength(FProgram.Instructions, InstructionCount);
      for i := 0 to InstructionCount - 1 do
      begin
        FProgram.Instructions[i].OpCode := TOpCode(Reader.ReadInteger);
        FProgram.Instructions[i].Operand1 := Reader.ReadInteger;
        FProgram.Instructions[i].Operand2 := Reader.ReadInteger;
      end;

    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TBytecodeVM.InitVMState;
var
  i: Integer;
  NewVarObject: TVMVariableObject;
begin
  FInstructionPointer := 0;
  FOperandStack.Clear;
  FCallStack.Clear;
  FVariables.Clear;
  FRuntimeForms.Clear;

  // Pre-populate FVariables based on FProgram.VariableMap
  for i := 0 to FProgram.VariableMap.Count - 1 do
  begin
    NewVarObject := TVMVariableObject.Create;
    NewVarObject.Variable.Name := FProgram.VariableMap.Keys[i]; // Fixed here
    NewVarObject.Variable.Value.ValueType := bcvtNull;
    NewVarObject.Variable.Value.IntValue := 0;
    NewVarObject.Variable.Value.StrValuePtr := nil;
    NewVarObject.Variable.Value.BoolValue := False;
    FVariables.Add(NewVarObject);
  end;
end;

procedure TBytecodeVM.CleanupVMState;
begin
  // TObjectList(True) will free owned objects when Clear is called.
  // Their destructors handle PString memory.
  FOperandStack.Clear;
  FCallStack.Clear;
  FVariables.Clear;
  FRuntimeForms.Clear;
end;

function TBytecodeVM.PopValue: TBCValue;
var
  ValueObject: TBCValueObject;
  TempBCValue: TBCValue; // Temporary to hold the record before deep copy
begin
  if FOperandStack.Count = 0 then
    raise Exception.Create('VM Stack underflow');

  ValueObject := TBCValueObject(FOperandStack.Last);
  FOperandStack.Remove(ValueObject); // Remove from list

  TempBCValue := ValueObject.Value; // Get the record. Its PString is still valid here.
  ValueObject.Free; // Free the wrapper object, which also frees its PString if it was a string

  // Now, create a deep copy for the Result, especially if it's a string.
  // The PString in TempBCValue is now dangling! So we need to copy it *before* freeing ValueObject.
  // Let's re-think PopValue logic to ensure the string content is copied before the source PString is freed.
  Result.ValueType := TempBCValue.ValueType;
  Result.IntValue := TempBCValue.IntValue;
  Result.BoolValue := TempBCValue.BoolValue;
  Result.StrValuePtr := nil; // Default to nil

  if (Result.ValueType = bcvtString) then // if TempBCValue was a string
  begin
    // Allocate new memory for the Result's string pointer
    New(Result.StrValuePtr);
    // Copy the actual string content that *was* at TempBCValue.StrValuePtr^
    // However, since ValueObject.Free already disposed TempBCValue.StrValuePtr,
    // this means we need to copy the string *before* ValueObject.Free.

    // Let's change PopValue to extract the string content *before* Freeing the object.
    // The string will be copied into a temporary `AnsiString` first, then a new `PString` allocated for `Result`.
    if Assigned(TempBCValue.StrValuePtr) then
    begin
      New(Result.StrValuePtr);
      Result.StrValuePtr^ := TempBCValue.StrValuePtr^;
    end;
  end;
end;


procedure TBytecodeVM.PushValue(const AValue: TBCValue);
var
  ValueObject: TBCValueObject;
begin
  ValueObject := TBCValueObject.Create; // Create a new wrapper object

  // Deep copy AValue into ValueObject.Value, especially for strings
  ValueObject.Value.ValueType := AValue.ValueType;
  case AValue.ValueType of
    bcvtInteger:
      ValueObject.Value.IntValue := AValue.IntValue;
    bcvtString:
      begin
        // Allocate new memory for the string and copy the content
        New(ValueObject.Value.StrValuePtr);
        // Ensure AValue.StrValuePtr is not nil before dereferencing
        if Assigned(AValue.StrValuePtr) then
          ValueObject.Value.StrValuePtr^ := AValue.StrValuePtr^
        else
          ValueObject.Value.StrValuePtr^ := ''; // Assign empty string if source is nil
      end;
    bcvtBoolean:
      ValueObject.Value.BoolValue := AValue.BoolValue;
    bcvtNull:
      // Nothing to copy for null, StrValuePtr is already nil by default in Create
  end;

  FOperandStack.Add(ValueObject);
end;

// This is where the problematic function's implementation should be:
function TBytecodeVM.GetVMVariable(VarID: Integer): TBCValue;
var
  VarObj      : TVMVariableObject;
  SourceValue : TBCValue;
begin
  // bounds-check
  if (VarID < 0) or (VarID >= FVariables.Count) then
    raise Exception.CreateFmt(
      'VM Runtime Error: Invalid variable ID %d', [VarID]);

  // fetch stored value
  VarObj      := TVMVariableObject(FVariables[VarID]);
  SourceValue := VarObj.Variable.Value;

  // shallow copy
  Result := SourceValue;

  // deep-copy string, if any
  if Result.ValueType = bcvtString then
  begin
    if Assigned(SourceValue.StrValuePtr) then
    begin
      New(Result.StrValuePtr);
      Result.StrValuePtr^ := SourceValue.StrValuePtr^;
    end
    else
      Result.StrValuePtr := nil;
  end;
end;


procedure TBytecodeVM.SetVMVariable(VarID: Integer; const AValue: TBCValue);
var
  VarObj: TVMVariableObject;
  OldValue: TBCValue; // To manage old PString memory
begin
  if (VarID < 0) or (VarID >= FVariables.Count) then
    raise Exception.CreateFmt('VM Runtime Error: Invalid variable ID %d', [VarID]);

  VarObj := TVMVariableObject(FVariables[VarID]);
  OldValue := VarObj.Variable.Value; // Keep old value to free PString if needed

  // Free old string memory if it was a string
  if (OldValue.ValueType = bcvtString) and Assigned(OldValue.StrValuePtr) then
    Dispose(OldValue.StrValuePtr);

  // Assign the new value, managing PString memory
  VarObj.Variable.Value.ValueType := AValue.ValueType;
  case AValue.ValueType of
    bcvtInteger:
      VarObj.Variable.Value.IntValue := AValue.IntValue;
    bcvtString:
      begin
        // Allocate new memory for the string and copy the content
        New(VarObj.Variable.Value.StrValuePtr);
        if Assigned(AValue.StrValuePtr) then
          VarObj.Variable.Value.StrValuePtr^ := AValue.StrValuePtr^
        else
          VarObj.Variable.Value.StrValuePtr^ := ''; // Assign empty string if source is nil
      end;
    bcvtBoolean:
      VarObj.Variable.Value.BoolValue := AValue.BoolValue;
    bcvtNull:
      VarObj.Variable.Value.StrValuePtr := nil; // Ensure null PString
  end;
end;


procedure TBytecodeVM.Execute(const BytecodeFileName: String);
var
  CurrentInstruction: TBCInstruction;
  Val1, Val2, ResultVal: TBCValue;
  TargetIP: Integer;
  SubroutineID: Integer;
  SubroutineMap: TStringList;
  FormName: String;
  FoundForm: TForm;
  i: Integer;
  NewVarObject: TVMVariableObject; // Declared here for proper scope
  varName: String;                 // Declare varName here

begin
  LoadBytecodeFile(BytecodeFileName);
  InitVMState;

  FInstructionPointer := 0;
  while FInstructionPointer < Length(FProgram.Instructions) do
  begin
    CurrentInstruction := FProgram.Instructions[FInstructionPointer];
    Inc(FInstructionPointer); // Advance IP for next instruction

    // Initialize ResultVal (Val1 and Val2 will be initialized by PopValue)
    ResultVal.ValueType := bcvtNull;
    ResultVal.IntValue := 0;
    ResultVal.StrValuePtr := nil;
    ResultVal.BoolValue := False;

    case CurrentInstruction.OpCode of
      OP_PUSH_INT:
        begin
          ResultVal.ValueType := bcvtInteger;
          ResultVal.IntValue := CurrentInstruction.Operand1;
          PushValue(ResultVal);
        end;
      OP_PUSH_STRING:
        begin
          ResultVal.ValueType := bcvtString;
          // Allocate new PString and copy content from literal pool
          New(ResultVal.StrValuePtr);
          ResultVal.StrValuePtr^ := FProgram.StringLiterals[CurrentInstruction.Operand1];
          PushValue(ResultVal);
          // Free the temporary PString created for ResultVal
          Dispose(ResultVal.StrValuePtr);
          ResultVal.StrValuePtr := nil;
        end;
      OP_PUSH_VAR:
        PushValue(GetVMVariable(CurrentInstruction.Operand1)); // Correct: GetVMVariable returns TBCValue directly

      OP_POP_VAR:
        SetVMVariable(CurrentInstruction.Operand1, PopValue);
      OP_POP:
       begin
      Val1 := PopValue;
      if (Val1.ValueType = bcvtString) and Assigned(Val1.StrValuePtr) then
        Dispose(Val1.StrValuePtr);
        end;
       OP_ADD_INT:
        begin
          Val2 := PopValue;
          Val1 := PopValue;
          ResultVal.ValueType := bcvtInteger;
          ResultVal.IntValue := Val1.IntValue + Val2.IntValue;
          PushValue(ResultVal);
          // Dispose any PStrings from Val1 and Val2 if they were strings
          if (Val1.ValueType = bcvtString) and Assigned(Val1.StrValuePtr) then Dispose(Val1.StrValuePtr);
          if (Val2.ValueType = bcvtString) and Assigned(Val2.StrValuePtr) then Dispose(Val2.StrValuePtr);
        end;
      OP_ADD_STRING: // For VB6 '&' operator
        begin
          Val2 := PopValue;
          Val1 := PopValue;
          ResultVal.ValueType := bcvtString;
          New(ResultVal.StrValuePtr);
          // Concatenate string values from pointers
          ResultVal.StrValuePtr^ := Val1.StrValuePtr^ + Val2.StrValuePtr^;
          PushValue(ResultVal);
          // Free the temporary PString from ResultVal
          Dispose(ResultVal.StrValuePtr);
          ResultVal.StrValuePtr := nil;
          // Dispose PStrings from Val1 and Val2
          if (Val1.ValueType = bcvtString) and Assigned(Val1.StrValuePtr) then Dispose(Val1.StrValuePtr);
          if (Val2.ValueType = bcvtString) and Assigned(Val2.StrValuePtr) then Dispose(Val2.StrValuePtr);
        end;

      // Comparison Operations (pop 2, push 1 boolean)
      OP_EQUAL:
        begin
            Val2 := PopValue;
            Val1 := PopValue;
            ResultVal.ValueType := bcvtBoolean;
            case Val1.ValueType of
                bcvtInteger: ResultVal.BoolValue := (Val1.IntValue = Val2.IntValue);
                bcvtString:
                    // Ensure pointers are assigned before dereferencing
                    if Assigned(Val1.StrValuePtr) and Assigned(Val2.StrValuePtr) then
                        ResultVal.BoolValue := (Val1.StrValuePtr^ = Val2.StrValuePtr^)
                    else
                        ResultVal.BoolValue := (not Assigned(Val1.StrValuePtr) and not Assigned(Val2.StrValuePtr)); // Both nil
                bcvtBoolean: ResultVal.BoolValue := (Val1.BoolValue = Val2.BoolValue);
                bcvtNull: ResultVal.BoolValue := (Val2.ValueType = bcvtNull);
            else
                ResultVal.BoolValue := False; // Default for unhandled types
            end;
            PushValue(ResultVal);
            // Dispose popped string Pointers
            if (Val1.ValueType = bcvtString) and Assigned(Val1.StrValuePtr) then Dispose(Val1.StrValuePtr);
            if (Val2.ValueType = bcvtString) and Assigned(Val2.StrValuePtr) then Dispose(Val2.StrValuePtr);
        end;
      OP_NOT_EQUAL:
        begin
            Val2 := PopValue;
            Val1 := PopValue;
            ResultVal.ValueType := bcvtBoolean;
            case Val1.ValueType of
                bcvtInteger: ResultVal.BoolValue := (Val1.IntValue <> Val2.IntValue);
                bcvtString:
                    if Assigned(Val1.StrValuePtr) and Assigned(Val2.StrValuePtr) then
                        ResultVal.BoolValue := (Val1.StrValuePtr^ <> Val2.StrValuePtr^)
                    else
                        ResultVal.BoolValue := not (not Assigned(Val1.StrValuePtr) and not Assigned(Val2.StrValuePtr)); // Not both nil
                bcvtBoolean: ResultVal.BoolValue := (Val1.BoolValue <> Val2.BoolValue);
                bcvtNull: ResultVal.BoolValue := (Val2.ValueType <> bcvtNull);
            else
                ResultVal.BoolValue := True; // Default for unhandled types
            end;
            PushValue(ResultVal);
            if (Val1.ValueType = bcvtString) and Assigned(Val1.StrValuePtr) then Dispose(Val1.StrValuePtr);
            if (Val2.ValueType = bcvtString) and Assigned(Val2.StrValuePtr) then Dispose(Val2.StrValuePtr);
        end;
      OP_GREATER:
        begin
            Val2 := PopValue;
            Val1 := PopValue;
            ResultVal.ValueType := bcvtBoolean;
            case Val1.ValueType of
                bcvtInteger: ResultVal.BoolValue := (Val1.IntValue > Val2.IntValue);
                bcvtString: ResultVal.BoolValue := (Val1.StrValuePtr^ > Val2.StrValuePtr^); // Lexicographical
            else
                ResultVal.BoolValue := False;
            end;
            PushValue(ResultVal);
            if (Val1.ValueType = bcvtString) and Assigned(Val1.StrValuePtr) then Dispose(Val1.StrValuePtr);
            if (Val2.ValueType = bcvtString) and Assigned(Val2.StrValuePtr) then Dispose(Val2.StrValuePtr);
        end;
      OP_LESS:
        begin
            Val2 := PopValue;
            Val1 := PopValue;
            ResultVal.ValueType := bcvtBoolean;
            case Val1.ValueType of
                bcvtInteger: ResultVal.BoolValue := (Val1.IntValue < Val2.IntValue);
                bcvtString: ResultVal.BoolValue := (Val1.StrValuePtr^ < Val2.StrValuePtr^);
            else
                ResultVal.BoolValue := False;
            end;
            PushValue(ResultVal);
            if (Val1.ValueType = bcvtString) and Assigned(Val1.StrValuePtr) then Dispose(Val1.StrValuePtr);
            if (Val2.ValueType = bcvtString) and Assigned(Val2.StrValuePtr) then Dispose(Val2.StrValuePtr);
        end;
      OP_GREATER_EQUAL:
        begin
            Val2 := PopValue;
            Val1 := PopValue;
            ResultVal.ValueType := bcvtBoolean;
            case Val1.ValueType of
                bcvtInteger: ResultVal.BoolValue := (Val1.IntValue >= Val2.IntValue);
                bcvtString: ResultVal.BoolValue := (Val1.StrValuePtr^ >= Val2.StrValuePtr^);
            else
                ResultVal.BoolValue := False;
            end;
            PushValue(ResultVal);
            if (Val1.ValueType = bcvtString) and Assigned(Val1.StrValuePtr) then Dispose(Val1.StrValuePtr);
            if (Val2.ValueType = bcvtString) and Assigned(Val2.StrValuePtr) then Dispose(Val2.StrValuePtr);
        end;
      OP_LESS_EQUAL:
        begin
            Val2 := PopValue;
            Val1 := PopValue;
            ResultVal.ValueType := bcvtBoolean;
            case Val1.ValueType of
                bcvtInteger: ResultVal.BoolValue := (Val1.IntValue <= Val2.IntValue);
                bcvtString: ResultVal.BoolValue := (Val1.StrValuePtr^ <= Val2.StrValuePtr^);
            else
                ResultVal.BoolValue := False;
            end;
            PushValue(ResultVal);
            if (Val1.ValueType = bcvtString) and Assigned(Val1.StrValuePtr) then Dispose(Val1.StrValuePtr);
            if (Val2.ValueType = bcvtString) and Assigned(Val2.StrValuePtr) then Dispose(Val2.StrValuePtr);
        end;

      // Logical Operations (typically only operate on booleans, so no PString dispose needed)
      OP_AND:
        begin
            Val2 := PopValue;
            Val1 := PopValue;
            ResultVal.ValueType := bcvtBoolean;
            ResultVal.BoolValue := Val1.BoolValue and Val2.BoolValue;
            PushValue(ResultVal);
        end;
      OP_OR:
        begin
            Val2 := PopValue;
            Val1 := PopValue;
            ResultVal.ValueType := bcvtBoolean;
            ResultVal.BoolValue := Val1.BoolValue or Val2.BoolValue;
            PushValue(ResultVal);
        end;
      OP_NOT:
        begin
            Val1 := PopValue;
            ResultVal.ValueType := bcvtBoolean;
            ResultVal.BoolValue := not Val1.BoolValue;
            PushValue(ResultVal);
        end;


      OP_JUMP:
        FInstructionPointer := CurrentInstruction.Operand1; // Jump to target IP
      OP_JUMP_IF_FALSE:
        begin
          Val1 := PopValue;
          if not Val1.BoolValue then
            FInstructionPointer := CurrentInstruction.Operand1;
          // Dispose PString from Val1 if it was a string
          if (Val1.ValueType = bcvtString) and Assigned(Val1.StrValuePtr) then Dispose(Val1.StrValuePtr);
        end;
       OP_CALL:
        begin // This 'begin' matches an 'end' later
          SubroutineID := CurrentInstruction.Operand1;
          FCallStack.Add(TCallFrameObject.Create(FInstructionPointer));
          //FProgram.SubroutineMap.Objects[SubroutineID]
          end; // This 'end' matches the 'begin' above
       OP_RETURN:
        begin
          if FCallStack.Count = 0 then
            raise Exception.Create('VM Runtime Error: RETURN without GOSUB');
          // Pop the frame and set IP
          TargetIP := TCallFrameObject(FCallStack.Last).Frame.ReturnAddress;
          FCallStack.Remove(FCallStack.Last);
          FInstructionPointer := TargetIP;
        end;
      OP_HALT:
        Exit;
      OP_DECL_VAR:
      begin
        NewVarObject := TVMVariableObject.Create;

        // works for TFPGMap<String,Integer>
        varName := FProgram.VariableMap.Keys[CurrentInstruction.Operand1];
        NewVarObject.Variable.Name := varName;

        NewVarObject.Variable.Value.ValueType :=
          TBCValueType(CurrentInstruction.Operand2);

        case NewVarObject.Variable.Value.ValueType of
          bcvtInteger: NewVarObject.Variable.Value.IntValue := 0;
          bcvtString : NewVarObject.Variable.Value.StrValuePtr := nil;
          bcvtBoolean: NewVarObject.Variable.Value.BoolValue := False;
        end;

        FVariables.Add(NewVarObject);
      end;
      OP_PRINT:
        begin
          Val1 := PopValue;
          case Val1.ValueType of
            bcvtInteger: WriteLn(Val1.IntValue);
            bcvtString:
              if Assigned(Val1.StrValuePtr) then WriteLn(Val1.StrValuePtr^) // Dereference PString
              else WriteLn(''); // Print empty for nil PString
            bcvtBoolean: WriteLn(Val1.BoolValue);
            bcvtNull: WriteLn('<NULL>');
          end;
          // Dispose popped string Pointers if present
          if (Val1.ValueType = bcvtString) and Assigned(Val1.StrValuePtr) then Dispose(Val1.StrValuePtr);
        end;

      OP_SHOW_FORM:
        begin
          FormName := FProgram.StringLiterals[CurrentInstruction.Operand1]; // Get form name string
          FoundForm := nil;
          // Look up existing form instance
          for i := 0 to FRuntimeForms.Count - 1 do
          begin
            if (FRuntimeForms.Items[i] is TForm) and SameText(TForm(FRuntimeForms.Items[i]).Name, FormName) then
            begin
              FoundForm := TForm(FRuntimeForms.Items[i]);
              Break;
            end;
          end;

          if not Assigned(FoundForm) then
          begin
            // Auto-create form (similar to your interpreter logic)
            FoundForm := TForm.Create(Application); // Assign owner to Application for proper lifecycle
            FoundForm.Name := FormName;
            FRuntimeForms.Add(FoundForm);
          end;
          FoundForm.Show; // Or ShowModal for modal forms
        end;

      else
        raise Exception.CreateFmt('VM Runtime Error: Unknown opcode %d at instruction %d', [Ord(CurrentInstruction.OpCode), FInstructionPointer - 1]);
    end; // case CurrentInstruction.OpCode
  end; // while
end;

end.
