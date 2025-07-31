unit BytecodeVM;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Contnrs, Forms, // For TStringList, TObjectList, TForm, etc.
  BytecodeTypes, fgl; // Our defined opcodes and structures

type
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
    destructor Destroy; override;
  end;

  TVMVariableObject = class(TObject)
  public
    Variable: TVMVariable; // The actual TVMVariable record (which is TBCValue)
    constructor Create;
    destructor Destroy; override;
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
    FOperandStack: TObjectList; // Stores TBCValueObject
    FCallStack: TObjectList;    // Stores TCallFrameObject
    FVariables: TObjectList;    // Stores TVMVariableObject
    FRuntimeForms: TObjectList; // Stores TForm instances

    function PopValue: TBCValue;
    procedure PushValue(const AValue: TBCValue);
    function GetVMVariable(VarID: Integer): TBCValue;
    procedure SetVMVariable(VarID: Integer; const AValue: TBCValue);
    procedure LoadBytecodeFile(const FileName: String); // Now uses TBytecodeGenerator
    procedure InitVMState;
    procedure CleanupVMState;
  public
    constructor Create(AProgram: TByteCodeProgram); // Accepts a pre-loaded program
    destructor Destroy; override;
    procedure Run; // Renamed from Execute to avoid confusion with file loading
  end;

implementation

uses
  Bytecode, // To use TBytecodeGenerator for loading
  InputBox; // To use InputBox function

{ TBCValueObject }

constructor TBCValueObject.Create;
begin
  inherited Create;
  Value.ValueType := bcvtNull;
  Value.IntValue := 0;
  Value.StringValue := '';
  Value.BoolValue := False;
end;

destructor TBCValueObject.Destroy;
begin
  inherited Destroy;
end;

{ TVMVariableObject }

constructor TVMVariableObject.Create;
begin
  inherited Create;
  Variable.ValueType := bcvtNull;
  Variable.IntValue := 0;
  Variable.StringValue := '';
  Variable.BoolValue := False;
end;

destructor TVMVariableObject.Destroy;
begin
  inherited Destroy;
end;

{ TCallFrameObject }

constructor TCallFrameObject.Create(AReturnAddress: Integer);
begin
  inherited Create;
  Frame.ReturnAddress := AReturnAddress;
end;

{ TBytecodeVM }

constructor TBytecodeVM.Create(AProgram: TByteCodeProgram);
begin
  inherited Create; // <<< FIXED: Explicitly calling inherited Create
  FProgram := AProgram; // VM takes ownership of the program object
  FOperandStack := TObjectList.Create(True); // Owns and frees TBCValueObject
  FCallStack := TObjectList.Create(True);    // Owns and frees TCallFrameObject
  FVariables := TObjectList.Create(True);    // Owns and frees TVMVariableObject
  FRuntimeForms := TObjectList.Create(True); // Owns and frees TForm instances
  InitVMState;
end;

destructor TBytecodeVM.Destroy;
begin
  CleanupVMState;
  FreeAndNil(FProgram);
  FreeAndNil(FOperandStack);
  FreeAndNil(FCallStack);
  FreeAndNil(FVariables);
  FreeAndNil(FRuntimeForms);
  inherited;
end;

procedure TBytecodeVM.LoadBytecodeFile(const FileName: String);
var
  BytecodeGen: TBytecodeGenerator;
begin
  BytecodeGen := TBytecodeGenerator.Create;
  try
    FProgram := BytecodeGen.LoadProgramFromFile(FileName);
  finally
    BytecodeGen.Free;
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

  for i := 0 to FProgram.VariableMap.Count - 1 do
  begin
    NewVarObject := TVMVariableObject.Create;
    NewVarObject.Variable.Name := FProgram.VariableMap.Keys[i];
    NewVarObject.Variable.Value := CreateBCValueNull;
    FVariables.Add(NewVarObject);
  end;
end;

procedure TBytecodeVM.CleanupVMState;
begin
  FOperandStack.Clear;
  FCallStack.Clear;
  FVariables.Clear;
  FRuntimeForms.Clear;
end;

function TBytecodeVM.PopValue: TBCValue;
var
  ValueObject: TBCValueObject;
begin
  if FOperandStack.Count = 0 then
    raise Exception.Create('VM Stack underflow');

  ValueObject := TBCValueObject(FOperandStack.Last);
  FOperandStack.Remove(ValueObject);

  Result := ValueObject.Value;
  ValueObject.Free;
end;

procedure TBytecodeVM.PushValue(const AValue: TBCValue);
var
  ValueObject: TBCValueObject;
begin
  ValueObject := TBCValueObject.Create;
  ValueObject.Value := AValue;
  FOperandStack.Add(ValueObject);
end;

function TBytecodeVM.GetVMVariable(VarID: Integer): TBCValue;
var
  VarObj: TVMVariableObject;
begin
  if (VarID < 0) or (VarID >= FVariables.Count) then
    raise Exception.CreateFmt('VM Runtime Error: Invalid variable ID %d', [VarID]);

  VarObj := TVMVariableObject(FVariables[VarID]);
  Result := VarObj.Variable.Value;
end;

procedure TBytecodeVM.SetVMVariable(VarID: Integer; const AValue: TBCValue);
var
  VarObj: TVMVariableObject;
begin
  if (VarID < 0) or (VarID >= FVariables.Count) then
    raise Exception.CreateFmt('VM Runtime Error: Invalid variable ID %d', [VarID]);

  VarObj := TVMVariableObject(FVariables[VarID]);
  VarObj.Variable.Value := AValue;
end;

procedure TBytecodeVM.Run;
var
  CurrentInstruction: TBCInstruction;
  Val1, Val2, ResultVal: TBCValue;
  TargetIP: Integer;
  SubroutineTargetAddress: LongInt;
  FormName: String;
  FoundForm: TForm;
  i: Integer;
  VarObj: TVMVariableObject;
  varName: String;

begin
  FInstructionPointer := 0;

  while FInstructionPointer < Length(FProgram.Instructions) do
  begin
    CurrentInstruction := FProgram.Instructions[FInstructionPointer];
    Inc(FInstructionPointer);

    ResultVal := CreateBCValueNull;

    case CurrentInstruction.OpCode of
      OP_PUSH_INT:
        begin
          ResultVal := CreateBCValueInteger(FProgram.IntegerLiterals[CurrentInstruction.Operand1]);
          PushValue(ResultVal);
        end;
      OP_PUSH_STRING:
        begin
          ResultVal := CreateBCValueString(FProgram.StringLiterals[CurrentInstruction.Operand1]);
          PushValue(ResultVal);
        end;
      OP_PUSH_VAR:
        PushValue(GetVMVariable(CurrentInstruction.Operand1));

      OP_POP_VAR:
        SetVMVariable(CurrentInstruction.Operand1, PopValue);
      OP_POP:
        Val1 := PopValue;

      OP_ADD_INT:
        begin
          Val2 := PopValue;
          Val1 := PopValue;
          ResultVal := CreateBCValueInteger(Val1.IntValue + Val2.IntValue);
          PushValue(ResultVal);
        end;
      OP_ADD_STRING:
        begin
          Val2 := PopValue;
          Val1 := PopValue;
          ResultVal := CreateBCValueString(Val1.StringValue + Val2.StringValue);
          PushValue(ResultVal);
        end;

      OP_EQUAL:
        begin
          Val2 := PopValue;
          Val1 := PopValue;
          ResultVal := CreateBCValueBoolean(False);
          case Val1.ValueType of
            bcvtInteger: ResultVal.BoolValue := (Val1.IntValue = Val2.IntValue);
            bcvtString:  ResultVal.BoolValue := (Val1.StringValue = Val2.StringValue);
            bcvtBoolean: ResultVal.BoolValue := (Val1.BoolValue = Val2.BoolValue);
            bcvtNull:    ResultVal.BoolValue := (Val2.ValueType = bcvtNull);
          else
            ResultVal.BoolValue := False;
          end;
          PushValue(ResultVal);
        end;
      OP_NOT_EQUAL:
        begin
          Val2 := PopValue;
          Val1 := PopValue;
          ResultVal := CreateBCValueBoolean(False);
          case Val1.ValueType of
            bcvtInteger: ResultVal.BoolValue := (Val1.IntValue <> Val2.IntValue);
            bcvtString:  ResultVal.BoolValue := (Val1.StringValue <> Val2.StringValue);
            bcvtBoolean: ResultVal.BoolValue := (Val1.BoolValue <> Val2.BoolValue);
            bcvtNull:    ResultVal.BoolValue := (Val2.ValueType <> bcvtNull);
          else
            ResultVal.BoolValue := True;
          end;
          PushValue(ResultVal);
        end;
      OP_GREATER:
        begin
          Val2 := PopValue;
          Val1 := PopValue;
          ResultVal := CreateBCValueBoolean(False);
          case Val1.ValueType of
            bcvtInteger: ResultVal.BoolValue := (Val1.IntValue > Val2.IntValue);
            bcvtString:  ResultVal.BoolValue := (Val1.StringValue > Val2.StringValue);
          else
            ResultVal.BoolValue := False;
          end;
          PushValue(ResultVal);
        end;
      OP_LESS:
        begin
          Val2 := PopValue;
          Val1 := PopValue;
          ResultVal := CreateBCValueBoolean(False);
          case Val1.ValueType of
            bcvtInteger: ResultVal.BoolValue := (Val1.IntValue < Val2.IntValue);
            bcvtString:  ResultVal.BoolValue := (Val1.StringValue < Val2.StringValue);
          else
            ResultVal.BoolValue := False;
          end;
          PushValue(ResultVal);
        end;
      OP_GREATER_EQUAL:
        begin
          Val2 := PopValue;
          Val1 := PopValue;
          ResultVal := CreateBCValueBoolean(False);
          case Val1.ValueType of
            bcvtInteger: ResultVal.BoolValue := (Val1.IntValue >= Val2.IntValue);
            bcvtString:  ResultVal.BoolValue := (Val1.StringValue >= Val2.StringValue);
          else
            ResultVal.BoolValue := False;
          end;
          PushValue(ResultVal);
        end;
      OP_LESS_EQUAL:
        begin
          Val2 := PopValue;
          Val1 := PopValue;
          ResultVal := CreateBCValueBoolean(False);
          case Val1.ValueType of
            bcvtInteger: ResultVal.BoolValue := (Val1.IntValue <= Val2.IntValue);
            bcvtString:  ResultVal.BoolValue := (Val1.StringValue <= Val2.StringValue);
          else
            ResultVal.BoolValue := False;
          end;
          PushValue(ResultVal);
        end;

      OP_AND:
        begin
          Val2 := PopValue;
          Val1 := PopValue;
          ResultVal := CreateBCValueBoolean(Val1.BoolValue and Val2.BoolValue);
          PushValue(ResultVal);
        end;
      OP_OR:
        begin
          Val2 := PopValue;
          Val1 := PopValue;
          ResultVal := CreateBCValueBoolean(Val1.BoolValue or Val2.BoolValue);
          PushValue(ResultVal);
        end;
      OP_NOT:
        begin
          Val1 := PopValue;
          ResultVal := CreateBCValueBoolean(not Val1.BoolValue);
          PushValue(ResultVal);
        end;

      OP_JUMP:
        FInstructionPointer := CurrentInstruction.Operand1;
      OP_JUMP_IF_FALSE:
        begin
          Val1 := PopValue;
          if not Val1.BoolValue then
            FInstructionPointer := CurrentInstruction.Operand1;
        end;
      OP_CALL:
        begin
          SubroutineTargetAddress := FProgram.SubroutineMap.Items[FProgram.StringLiterals[CurrentInstruction.Operand1]];
          FCallStack.Add(TCallFrameObject.Create(FInstructionPointer));
          FInstructionPointer := SubroutineTargetAddress;
        end;
      OP_RETURN:
        begin
          if FCallStack.Count = 0 then
            raise Exception.Create('VM Runtime Error: RETURN without GOSUB');
          TargetIP := TCallFrameObject(FCallStack.Last).Frame.ReturnAddress;
          FCallStack.Remove(FCallStack.Last);
          FInstructionPointer := TargetIP;
        end;
      OP_HALT:
        Exit;

      OP_DECL_VAR:
        begin
          varName := FProgram.StringLiterals[CurrentInstruction.Operand1];
          VarObj := nil;
          for i := 0 to FVariables.Count - 1 do
          begin
            if SameText(TVMVariableObject(FVariables[i]).Variable.Name, varName) then
            begin
              VarObj := TVMVariableObject(FVariables[i]);
              Break;
            end;
          end;

          if not Assigned(VarObj) then
          begin
            VarObj := TVMVariableObject.Create;
            VarObj.Variable.Name := varName;
            FVariables.Add(VarObj);
          end;

          VarObj.Variable.Value.ValueType := TBCValueType(CurrentInstruction.Operand2);
          case VarObj.Variable.Value.ValueType of
            bcvtInteger: VarObj.Variable.Value.IntValue := 0;
            bcvtString:  VarObj.Variable.Value.StringValue := '';
            bcvtBoolean: VarObj.Variable.Value.BoolValue := False;
          end;
        end;

      OP_PRINT:
        begin
          Val1 := PopValue;
          Writeln(BCValueToString(Val1));
        end;

      OP_INPUT:
        begin
          Val1 := PopValue;
          varName := Val1.StringValue;

          ResultVal := CreateBCValueString(InputBox('Input', varName, ''));
          SetVMVariable(CurrentInstruction.Operand2, ResultVal);
        end;

      OP_SHOW_FORM:
        begin
          FormName := FProgram.StringLiterals[CurrentInstruction.Operand1];
          FoundForm := nil;
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
            FoundForm := TForm.Create(FormName);
            FRuntimeForms.Add(FoundForm);
          end;
          FoundForm.Show;
        end;

      else
        raise Exception.CreateFmt('VM Runtime Error: Unknown opcode %d at instruction %d', [Ord(CurrentInstruction.OpCode), FInstructionPointer - 1]);
    end;
  end;
end;

end.

