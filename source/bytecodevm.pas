unit BytecodeVM;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Contnrs, Forms, // For TStringList, TObjectList, TForm, etc.
  BytecodeTypes, Dialogs, fgl; // Our defined opcodes and structures

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
    FName: String;       // <<< CONFIRMED: Field to store the variable's name
    Variable: TVMVariable; // The actual TVMVariable record (which is TBCValue)
    constructor Create;
    destructor Destroy; override;
  end;

  TCallFrameObject = class(TObject)
  public
    Frame: TCallFrame;
    constructor Create(AReturnAddress: Integer);
  end;

  { Legacy stack-VM opcode/instruction format, private to this unit. Predates
    the BC_* refactor of BytecodeTypes.TByteCodeOp/TBCInstruction that the
    kayte.lpi compiler pipeline now uses; kept distinct rather than merged
    since nothing in this codebase currently produces or consumes it. }
  TVMOpCode = (
    OP_PUSH_INT, OP_PUSH_STRING, OP_PUSH_VAR, OP_POP_VAR, OP_POP,
    OP_ADD_INT, OP_SUB_INT, OP_MUL_INT, OP_DIV_INT, OP_ADD_STRING,
    OP_EQUAL, OP_NOT_EQUAL, OP_GREATER, OP_LESS,
    OP_GREATER_EQUAL, OP_LESS_EQUAL,
    OP_AND, OP_OR, OP_NOT,
    OP_JUMP, OP_JUMP_IF_FALSE, OP_CALL, OP_RETURN, OP_HALT,
    OP_DECL_VAR, OP_PRINT, OP_INPUT, OP_SHOW_FORM,
    OP_CASE_COND, OP_ENDCASE,
    OP_FOREACH_INIT, OP_FOREACH_ITER, OP_FOREACH_END,
    OP_CALL_PROC, OP_RETURN_PROC,
    OP_FORM_START, OP_FORM_END,
    OP_KEY_PRESSED, OP_READ_KEY
  );

  TVMInstruction = record
    OpCode: TVMOpCode;
    Operand1: LongInt;
    Operand2: LongInt;
  end;

  TVMInstructionArray = array of TVMInstruction;

  TVMByteCodeProgram = class(TObject)
  public
    Instructions: TVMInstructionArray;
    StringLiterals: TStringList;
    IntegerLiterals: array of Int64;
    VariableMap: TStringIntMap;
    SubroutineMap: TStringIntMap;
    FormMap: TStringIntMap;
    ProgramTitle: String;

    constructor Create;
    destructor Destroy; override;
  end;

  TBytecodeVM = class
  private
    FProgram: TVMByteCodeProgram;
    FInstructionPointer: Integer;
    FOperandStack: TObjectList; // Stores TBCValueObject
    FCallStack: TObjectList;    // Stores TCallFrameObject
    FVariables: TObjectList;    // Stores TVMVariableObject
    FRuntimeForms: TObjectList; // Stores TForm instances

    function PopValue: TBCValue;
    procedure PushValue(const AValue: TBCValue);
    function GetVMVariable(VarID: Integer): TBCValue;
    procedure SetVMVariable(VarID: Integer; const AValue: TBCValue);
    (*procedure LoadBytecodeFile(const FileName: String); // Now uses TBytecodeGenerator*)
    procedure InitVMState;
    procedure CleanupVMState;
  public
    constructor Create(AProgram: TVMByteCodeProgram); // Accepts a pre-loaded program
    destructor Destroy; override;
    procedure Run; // Renamed from Execute to avoid confusion with file loading
  end;


  TBytecodeGenerator = class
  private
    // Helper procedures for writing data to a stream
    procedure WriteString(FileStream: TFileStream; const S: String);
    procedure WriteLongInt(FileStream: TFileStream; const L: LongInt);
    procedure WriteInteger(FileStream: TFileStream; const I: Integer);

    // Helper functions for reading data from a stream
    function ReadString(AStream: TStream): String;
    function ReadLongInt(AStream: TStream): LongInt;
    function ReadInteger(AStream: TStream): Integer;

  public
    (*procedure SaveProgramToFile(AProgram: TVMByteCodeProgram; const OutputFilePath: String);*)

    // Reads back everything SaveProgramToFile wrote, in the same order:
    // title, instructions, then the literal and symbol maps.
    function LoadProgramFromStream(AStream: TStream): TVMByteCodeProgram;

    // Thin wrapper: opens InputFilePath and hands it to LoadProgramFromStream.
    function LoadProgramFromFile(const InputFilePath: String): TVMByteCodeProgram;
  end;


implementation



uses
  Bytecode, FileUtil, // To use TBytecodeGenerator for loading
  Crt;      // For KeyPressed and ReadKey


{ TBytecodeGenerator Helper Procedures/Functions }

procedure TBytecodeGenerator.WriteString(FileStream: TFileStream; const S: String);
var
  Len: LongInt;
begin
  Len := Length(S);
  FileStream.Write(Len, SizeOf(Len));
  if Len > 0 then
    FileStream.Write(S[1], Len); // Write string content
end;

function TBytecodeGenerator.ReadString(AStream: TStream): String;
var
  Len: LongInt;
begin
  AStream.Read(Len, SizeOf(Len));
  SetLength(Result, Len);
  if Len > 0 then
    AStream.Read(Result[1], Len); // Read string content
end;

procedure TBytecodeGenerator.WriteLongInt(FileStream: TFileStream; const L: LongInt);
begin
  FileStream.Write(L, SizeOf(L));
end;

function TBytecodeGenerator.ReadLongInt(AStream: TStream): LongInt;
begin
  AStream.Read(Result, SizeOf(Result));
end;

procedure TBytecodeGenerator.WriteInteger(FileStream: TFileStream; const I: Integer);
begin
  FileStream.Write(I, SizeOf(I));
end;

function TBytecodeGenerator.ReadInteger(AStream: TStream): Integer;
begin
  AStream.Read(Result, SizeOf(Result));
end;


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

{ TVMByteCodeProgram }

constructor TVMByteCodeProgram.Create;
begin
  inherited Create;
  StringLiterals := TStringList.Create;
  VariableMap := TStringIntMap.Create;
  SubroutineMap := TStringIntMap.Create;
  FormMap := TStringIntMap.Create;
  SetLength(Instructions, 0);
  SetLength(IntegerLiterals, 0);
  ProgramTitle := 'Kayte Application';
end;

destructor TVMByteCodeProgram.Destroy;
begin
  StringLiterals.Free;
  VariableMap.Free;
  SubroutineMap.Free;
  FormMap.Free;
  inherited Destroy;
end;

{ TBytecodeVM }

constructor TBytecodeVM.Create(AProgram: TVMByteCodeProgram);
begin
  inherited Create;
  FProgram := AProgram;
  FOperandStack := TObjectList.Create(True);
  FCallStack := TObjectList.Create(True);
  FVariables := TObjectList.Create(True);
  FRuntimeForms := TObjectList.Create(True);
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

{ TBytecodeGenerator }

function TBytecodeGenerator.LoadProgramFromStream(AStream: TStream): TVMByteCodeProgram;
var
  LProgram: TVMByteCodeProgram; // Renamed from 'Program' to 'LProgram'
  Count: LongInt;
  i: Integer;
  Instruction: TVMInstruction;
  KeyString: String;
  ValueLongInt: LongInt;
  Int64Value: Int64;
begin
  LProgram := nil; // Initialize to nil for safe cleanup in except block
  try
    LProgram := TVMByteCodeProgram.Create;

    // 1. Read ProgramTitle
    LProgram.ProgramTitle := ReadString(AStream);

    // 2. Read Instructions
    Count := ReadLongInt(AStream);
    SetLength(LProgram.Instructions, Count);
    for i := Low(LProgram.Instructions) to High(LProgram.Instructions) do
    begin
      AStream.Read(Instruction, SizeOf(Instruction));
      LProgram.Instructions[i] := Instruction;
    end;

    // 3. Read IntegerLiterals (array of Int64)
    Count := ReadLongInt(AStream);
    SetLength(LProgram.IntegerLiterals, Count);
    for i := Low(LProgram.IntegerLiterals) to High(LProgram.IntegerLiterals) do
    begin
      AStream.Read(Int64Value, SizeOf(Int64));
      LProgram.IntegerLiterals[i] := Int64Value;
    end;

    // 4. Read StringLiterals (TStringList)
    Count := ReadLongInt(AStream);
    LProgram.StringLiterals := TStringList.Create; // Create instance
    for i := 0 to Count - 1 do
    begin
      LProgram.StringLiterals.Add(ReadString(AStream));
    end;

    // 5. Read SubroutineMap (TFPGMap<AnsiString, LongInt>)
    Count := ReadLongInt(AStream);
    LProgram.SubroutineMap := TStringIntMap.Create; // Create instance
    for i := 0 to Count - 1 do
    begin
      KeyString := ReadString(AStream);
      ValueLongInt := ReadLongInt(AStream);
      LProgram.SubroutineMap.Add(AnsiString(KeyString), ValueLongInt); // Add to TFPGMap
    end;

    // 6. Read VariableMap (TFPGMap<AnsiString, LongInt>)
    Count := ReadLongInt(AStream);
    LProgram.VariableMap := TStringIntMap.Create; // Create instance
    for i := 0 to Count - 1 do
    begin
      KeyString := ReadString(AStream);
      ValueLongInt := ReadLongInt(AStream); // Read as LongInt, as TFPGMap stores LongInt
      LProgram.VariableMap.Add(AnsiString(KeyString), ValueLongInt); // Add to TFPGMap
    end;

    // 7. Read FormMap (TFPGMap<AnsiString, LongInt>)
    Count := ReadLongInt(AStream);
    LProgram.FormMap := TStringIntMap.Create; // Create instance
    for i := 0 to Count - 1 do
    begin
      KeyString := ReadString(AStream);
      ValueLongInt := ReadLongInt(AStream);
      LProgram.FormMap.Add(AnsiString(KeyString), ValueLongInt); // Add to TFPGMap
    end;

    Result := LProgram;
  except
    // Ensure LProgram is freed if an error occurs during loading
    if Assigned(LProgram) then
      LProgram.Free;
    raise;
  end;
end;

function TBytecodeGenerator.LoadProgramFromFile(const InputFilePath: String): TVMByteCodeProgram;
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(InputFilePath, fmOpenRead);
  try
    Result := LoadProgramFromStream(FileStream);
  finally
    FileStream.Free;
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
    NewVarObject.FName := FProgram.VariableMap.Keys[i];
    NewVarObject.Variable := CreateBCValueNull; // <<< FIXED: Assign the TBCValue record directly
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
  Result := VarObj.Variable; // <<< FIXED: Assign the TBCValue record directly
end;

procedure TBytecodeVM.SetVMVariable(VarID: Integer; const AValue: TBCValue);
var
  VarObj: TVMVariableObject;
begin
  if (VarID < 0) or (VarID >= FVariables.Count) then
    raise Exception.CreateFmt('VM Runtime Error: Invalid variable ID %d', [VarID]);

  VarObj := TVMVariableObject(FVariables[VarID]);
  VarObj.Variable := AValue; // <<< FIXED: Assign the TBCValue record directly
end;

procedure TBytecodeVM.Run;
var
  CurrentInstruction: TVMInstruction;
  Val1, Val2, ResultVal: TBCValue;
  TargetIP: Integer;
  SubroutineTargetAddress: LongInt;
  FormName: String;
  FoundForm: TForm;
  i: Integer;
  VarObj: TVMVariableObject;
  varName: String;
  KeyChar: Char;

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
        // SubroutineMap.Items returns a generic Pointer; this assumes the address
        // was stored as Pointer(LongIntAddress), so casting straight back is safe.
        SubroutineTargetAddress := LongInt(FProgram.SubroutineMap.Items[CurrentInstruction.Operand1]);
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
            // When finding an existing variable, compare its FName
            if SameText(TVMVariableObject(FVariables[i]).FName, varName) then
            begin
              VarObj := TVMVariableObject(FVariables[i]);
              Break;
            end;
          end;

          if not Assigned(VarObj) then
          begin
            VarObj := TVMVariableObject.Create;
            VarObj.FName := varName;
            FVariables.Add(VarObj);
          end;

          // Assign the type and default value to the Variable record itself
          VarObj.Variable.ValueType := TBCValueType(CurrentInstruction.Operand2);
          case VarObj.Variable.ValueType of
            bcvtInteger: VarObj.Variable.IntValue := 0;
            bcvtString:  VarObj.Variable.StringValue := '';
            bcvtBoolean: VarObj.Variable.BoolValue := False;
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
            // Corrected line: TForm.Create expects a TComponent (like nil or Application) as owner.
            // We pass nil because the VM will manage the form's lifecycle.
            FoundForm := TForm.Create(nil);
            // Assign the name AFTER creation.
            FoundForm.Name := FormName;
            FRuntimeForms.Add(FoundForm);
          end;
          FoundForm.Show;
        end;

      OP_KEY_PRESSED:
        begin
          ResultVal := CreateBCValueBoolean(KeyPressed);
          PushValue(ResultVal);
        end;
      OP_READ_KEY:
        begin
          KeyChar := ReadKey;
          ResultVal := CreateBCValueString(KeyChar);
          PushValue(ResultVal);
        end;

      else
        raise Exception.CreateFmt('VM Runtime Error: Unknown opcode %d at instruction %d', [Ord(CurrentInstruction.OpCode), FInstructionPointer - 1]);
    end;
  end;
end;

end.

