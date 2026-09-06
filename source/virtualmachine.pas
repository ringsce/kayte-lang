unit VirtualMachine;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TypInfo, Process, BytecodeTypes; // BytecodeTypes unit is essential for TByteCodeProgram

type
  // Runtime values are dynamically typed (like a BASIC Variant), holding
  // either an integer or a string. Operators decide at runtime how to
  // combine them (e.g. + adds two integers but concatenates otherwise).
  TValueKind = (vkInt, vkStr);

  TKayteValue = record
    Kind: TValueKind;
    IntVal: Int64;
    StrVal: string;
  end;

  TVirtualMachine = class(TObject)
   private
     FProgram: TByteCodeProgram;
     FInstructionPointer: LongInt; // To track the current instruction
     FStack: array of TKayteValue;
     FStackTop: Integer;
     FVariables: array of TKayteValue;

     procedure Push(const Value: TKayteValue);
     function Pop: TKayteValue;
     function MakeInt(Value: Int64): TKayteValue;
     function MakeStr(const Value: string): TKayteValue;
     function ToDisplayString(const Value: TKayteValue): string;
     function ToInt(const Value: TKayteValue): Int64;
     function IsTruthy(const Value: TKayteValue): Boolean;
     function CompareValues(const A, B: TKayteValue; Op: TByteCodeOp): Boolean;
     function GetIntegerLiteral(Index: Integer): Int64;
     function GetStringLiteral(Index: Integer): string;
     procedure CheckVarIndex(Index: Integer);
   public
     // Constructor to accept the pre-loaded TByteCodeProgram object
     constructor Create(AProgram: TByteCodeProgram);
     destructor Destroy; override;
     procedure Run;
   end;

implementation

{ TVirtualMachine }

constructor TVirtualMachine.Create(AProgram: TByteCodeProgram);
begin
  inherited Create;
  FProgram := AProgram;
  FInstructionPointer := 0;
  FStackTop := 0;

  // AProgram's memory isn't owned here - the caller
  // (TCLIHandler.RunBytecodeFile) frees it after the VM is done.
end;

destructor TVirtualMachine.Destroy;
begin
  // No need to free FProgram here, as the CLIHandler manages its lifetime.
  inherited Destroy;
end;

procedure TVirtualMachine.Push(const Value: TKayteValue);
begin
  if FStackTop >= Length(FStack) then
    SetLength(FStack, Length(FStack) * 2 + 16);
  FStack[FStackTop] := Value;
  Inc(FStackTop);
end;

function TVirtualMachine.Pop: TKayteValue;
begin
  if FStackTop <= 0 then
    raise Exception.Create('Runtime Error: evaluation stack underflow (malformed bytecode)');
  Dec(FStackTop);
  Result := FStack[FStackTop];
end;

function TVirtualMachine.MakeInt(Value: Int64): TKayteValue;
begin
  Result.Kind := vkInt;
  Result.IntVal := Value;
  Result.StrVal := '';
end;

function TVirtualMachine.MakeStr(const Value: string): TKayteValue;
begin
  Result.Kind := vkStr;
  Result.IntVal := 0;
  Result.StrVal := Value;
end;

function TVirtualMachine.ToDisplayString(const Value: TKayteValue): string;
begin
  if Value.Kind = vkInt then
    Result := IntToStr(Value.IntVal)
  else
    Result := Value.StrVal;
end;

function TVirtualMachine.ToInt(const Value: TKayteValue): Int64;
begin
  if Value.Kind = vkInt then
    Result := Value.IntVal
  else if not TryStrToInt64(Trim(Value.StrVal), Result) then
    raise Exception.CreateFmt('Runtime Error: cannot convert "%s" to a number', [Value.StrVal]);
end;

function TVirtualMachine.IsTruthy(const Value: TKayteValue): Boolean;
begin
  if Value.Kind = vkInt then
    Result := Value.IntVal <> 0
  else
    Result := Value.StrVal <> '';
end;

function TVirtualMachine.CompareValues(const A, B: TKayteValue; Op: TByteCodeOp): Boolean;
var
  Rel: Integer; // <0, 0, >0
begin
  if (A.Kind = vkInt) and (B.Kind = vkInt) then
  begin
    if A.IntVal < B.IntVal then Rel := -1
    else if A.IntVal > B.IntVal then Rel := 1
    else Rel := 0;
  end
  else
    Rel := CompareStr(ToDisplayString(A), ToDisplayString(B));

  case Op of
    BC_CMP_EQ:  Result := Rel = 0;
    BC_CMP_NEQ: Result := Rel <> 0;
    BC_CMP_LT:  Result := Rel < 0;
    BC_CMP_GT:  Result := Rel > 0;
    BC_CMP_LE:  Result := Rel <= 0;
    BC_CMP_GE:  Result := Rel >= 0;
  else
    Result := False;
  end;
end;

function TVirtualMachine.GetIntegerLiteral(Index: Integer): Int64;
begin
  if (Index < 0) or (Index >= Length(FProgram.IntegerLiterals)) then
    raise Exception.CreateFmt('Runtime Error: integer literal index %d out of range', [Index]);
  Result := FProgram.IntegerLiterals[Index];
end;

function TVirtualMachine.GetStringLiteral(Index: Integer): string;
var
  Raw: string;
begin
  if (Index < 0) or (Index >= FProgram.StringLiterals.Count) then
    raise Exception.CreateFmt('Runtime Error: string literal index %d out of range', [Index]);
  Raw := FProgram.StringLiterals[Index];
  // String literals are stored with their surrounding quotes intact, as
  // captured verbatim from source by the lexer - strip them here.
  if (Length(Raw) >= 2) and (Raw[1] = '"') and (Raw[Length(Raw)] = '"') then
    Result := Copy(Raw, 2, Length(Raw) - 2)
  else
    Result := Raw;
end;

procedure TVirtualMachine.CheckVarIndex(Index: Integer);
begin
  if (Index < 0) or (Index >= Length(FVariables)) then
    raise Exception.CreateFmt('Runtime Error: variable index %d out of range', [Index]);
end;

procedure TVirtualMachine.Run;
var
  InstructionCount: LongInt;
  Instr: TBCInstruction;
  A, B: TKayteValue;
  MaxVarIndex, I, J: Integer;
  PrintArgs: array of TKayteValue;
  OutLine: string;
  ProcArgs: array of TKayteValue;
  ProcArgStrs: array of string;
  ProcOutput: string;
begin
  InstructionCount := Length(FProgram.Instructions);

  Writeln('Executing bytecode for program: ', FProgram.ProgramTitle);
  Writeln('Total instructions to execute: ', InstructionCount);

  if InstructionCount = 0 then
  begin
    Writeln('Program is empty. Execution finished.');
    Exit;
  end;

  // Size the runtime variable table from the highest variable index seen
  // at compile time (VariableMap). Variables (the name list) isn't
  // repopulated when loading a saved .bytecode file, so it can't be used
  // for this - VariableMap always is.
  MaxVarIndex := -1;
  for I := 0 to FProgram.VariableMap.Count - 1 do
    if FProgram.VariableMap.Data[I] > MaxVarIndex then
      MaxVarIndex := FProgram.VariableMap.Data[I];
  SetLength(FVariables, MaxVarIndex + 1);
  for I := 0 to High(FVariables) do
    FVariables[I] := MakeInt(0);

  FStackTop := 0;
  FInstructionPointer := 0;

  while FInstructionPointer < InstructionCount do
  begin
    Instr := FProgram.Instructions[FInstructionPointer];

    case Instr.OpCode of
      BC_NOP: ;
      BC_HALT:
        begin
          FInstructionPointer := InstructionCount;
          Continue;
        end;
      BC_LOAD_INT:
        Push(MakeInt(GetIntegerLiteral(Instr.Operand1)));
      BC_LOAD_STRING:
        Push(MakeStr(GetStringLiteral(Instr.Operand1)));
      BC_LOAD_VAR:
        begin
          CheckVarIndex(Instr.Operand1);
          Push(FVariables[Instr.Operand1]);
        end;
      BC_STORE_VAR, BC_ASSIGN:
        begin
          CheckVarIndex(Instr.Operand1);
          FVariables[Instr.Operand1] := Pop;
        end;
      BC_ADD:
        begin
          B := Pop;
          A := Pop;
          if (A.Kind = vkInt) and (B.Kind = vkInt) then
            Push(MakeInt(A.IntVal + B.IntVal))
          else
            // Loosely-typed '+': falls back to concatenation, like VB6.
            Push(MakeStr(ToDisplayString(A) + ToDisplayString(B)));
        end;
      BC_SUB:
        begin
          B := Pop;
          A := Pop;
          Push(MakeInt(ToInt(A) - ToInt(B)));
        end;
      BC_MUL:
        begin
          B := Pop;
          A := Pop;
          Push(MakeInt(ToInt(A) * ToInt(B)));
        end;
      BC_DIV:
        begin
          B := Pop;
          A := Pop;
          if ToInt(B) = 0 then
            raise Exception.Create('Runtime Error: division by zero');
          Push(MakeInt(ToInt(A) div ToInt(B)));
        end;
      BC_CONCAT:
        begin
          B := Pop;
          A := Pop;
          Push(MakeStr(ToDisplayString(A) + ToDisplayString(B)));
        end;
      BC_NEG:
        begin
          A := Pop;
          Push(MakeInt(-ToInt(A)));
        end;
      BC_NOT:
        begin
          A := Pop;
          Push(MakeInt(Ord(not IsTruthy(A))));
        end;
      BC_CMP_EQ, BC_CMP_NEQ, BC_CMP_LT, BC_CMP_GT, BC_CMP_LE, BC_CMP_GE:
        begin
          B := Pop;
          A := Pop;
          Push(MakeInt(Ord(CompareValues(A, B, Instr.OpCode))));
        end;
      BC_PRINT:
        begin
          SetLength(PrintArgs, Instr.Operand1);
          for J := Instr.Operand1 - 1 downto 0 do
            PrintArgs[J] := Pop;
          OutLine := '';
          for J := 0 to Instr.Operand1 - 1 do
          begin
            if J > 0 then
              OutLine := OutLine + ' ';
            OutLine := OutLine + ToDisplayString(PrintArgs[J]);
          end;
          Writeln(OutLine);
        end;
      BC_POP:
        Pop;
      BC_JUMP:
        begin
          FInstructionPointer := Instr.Operand1;
          Continue;
        end;
      BC_JUMP_IF_FALSE:
        begin
          A := Pop;
          if not IsTruthy(A) then
          begin
            FInstructionPointer := Instr.Operand1;
            Continue;
          end;
        end;
      BC_PROCESS:
        begin
          // Operand1 = pushed argument count (command + args), in order;
          // Operand2 = destination variable index, or -1 to print output.
          SetLength(ProcArgs, Instr.Operand1);
          for J := Instr.Operand1 - 1 downto 0 do
            ProcArgs[J] := Pop;

          SetLength(ProcArgStrs, Instr.Operand1 - 1);
          for J := 1 to Instr.Operand1 - 1 do
            ProcArgStrs[J - 1] := ToDisplayString(ProcArgs[J]);

          ProcOutput := '';
          if not RunCommand(ToDisplayString(ProcArgs[0]), ProcArgStrs, ProcOutput) then
            raise Exception.CreateFmt('Runtime Error: failed to run process "%s"', [ToDisplayString(ProcArgs[0])]);

          if Instr.Operand2 >= 0 then
          begin
            CheckVarIndex(Instr.Operand2);
            FVariables[Instr.Operand2] := MakeStr(ProcOutput);
          end
          else
            Writeln(ProcOutput);
        end;
      BC_INPUT, BC_CALL, BC_RETURN:
        raise Exception.CreateFmt('Runtime Error: %s is not supported yet', [GetEnumName(TypeInfo(TByteCodeOp), Ord(Instr.OpCode))]);
    end;

    Inc(FInstructionPointer);
  end;

  Writeln('Execution finished successfully.');
end;

end.
