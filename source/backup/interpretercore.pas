unit InterpreterCore;

{$mode objfpc}{$H+} // Use Object Pascal mode and extended syntax

interface

uses
  Classes,
  SysUtils,           // For string functions like Trim, LowerCase, SameText, IntToStr, StrToIntDef, FileExists, Exceptions
  InterpreterUtils,   // Your utility unit for Code, Vars, Labels, Subs, SplitString, FormLocations
  Forms, Contnrs; // <--- CRUCIAL FIX: Added this unit to make TForm and FormsList visible

// Define TVarType and TVariable if not already defined elsewhere (e.g., in InterpreterUtils
// or a dedicated Types unit). For this example, I'll include basic definitions.
type
  TVarType = (vtInteger, vtString, vtBoolean, vtUndefined); // Added vtBoolean, vtUndefined for completeness
  PVariable = ^TVariable;
  TVariable = record
    vtype: TVarType;
    intValue: Integer;
    strValue: String;
    boolValue: Boolean; // Added for boolean variables
  end;

// Dummy record types for loop/select case stacks if they're not fully implemented yet
// (These are conceptual and depend on your full interpreter's state management)
type
  TLoopFrame = record
    startLine: Integer;
    // Add other relevant loop state, e.g., loop variables, conditions
  end;
  PInteger = ^Integer; // For the call stack's return addresses
  TSelectCaseFrame = record
    startLine: Integer;
    selectVarValue: TVariable;
    matchedCase: Boolean;
    endSelectLine: Integer;
    caseElseLine: Integer;
  end;


 var
  Code: TStringList;
  Vars: TStringList;
  Labels: TStringList;
  Subs: TStringList;
  Stack: TObjectList;
  FormLocations: TStringList;
  RuntimeForms: TObjectList; // This will now be recognized
  PVar: PVariable;



// Helper to get a variable's value pointer (you'll need to implement this fully)
function GetVar(const VarName: String): PVariable;
// Helper to set a variable's value (you'll need to implement this fully)
procedure SetVar(const VarName: String; const AValue: TVariable);


procedure ExecuteLine(const Line: String; var pc: Integer);

implementation

// --- Minimal stub implementations for GetVar/SetVar for compilation ---
// YOU MUST REPLACE THESE WITH YOUR ACTUAL, ROBUST VARIABLE MANAGEMENT LOGIC
// If these are defined in InterpreterUtils, you don't need them here.
function GetVar(const VarName: String): PVariable;
var
  idx: Integer;
begin
  idx := Vars.IndexOf(LowerCase(VarName));
  if idx <> -1 then
    Result := PVariable(Vars.Objects[idx])
  else
    Result := nil; // Variable not found
end;

procedure SetVar(const VarName: String; const AValue: TVariable);
var
  idx: Integer;
  PVar: PVariable;
begin
  idx := Vars.IndexOf(LowerCase(VarName));
  if idx <> -1 then
  begin
    // Variable already exists, update it
    PVar := PVariable(Vars.Objects[idx]);
    PVar^ := AValue;
  end
  else
  begin
    // Variable doesn't exist, create it (DIM should do this, but this is a fallback)
    New(PVar);
    PVar^ := AValue;
    Vars.AddObject(LowerCase(VarName), TObject(PVar));
  end;
end;


procedure ExecuteLine(const Line: String; var pc: Integer);
var
  (*trimmedLine: String;
  parts: TStringArray; // Defined in InterpreterUtils
  cmd: String;
  val: String;
  // --- All local variables declared at the top, as Pascal requires ---
  targetLine: Integer;
  formName: String;
  foundForm: TForm;     // TForm is now recognized because of 'uses Forms;'
  i: Integer;
  varName: String;
  variableValue: TVariable; // For holding parsed variable values
  conditionVarName: String;
  conditionOp: String;
  conditionValue: String;
  conditionResult: Boolean;
  subName: String;
  ReturnAddr: PInteger;
  labelName: String;
  currentScanLine: Integer;
  scanTrimmedLine: String;
  scanParts: TStringArray;
  nestedWhiles: Integer;
  selectCaseVarName: String;
  caseValue: String;
  nestedSelects: Integer;
  endSelectLine: Integer;
  caseElseLine: Integer;
  foundEndSelect: Boolean;
  selectVarValue: TVariable;
  match: Boolean;
  valInt: Integer; // <--- DECLARE IT HERE WITHOUT INITIALIZATION
  //conditionValue: String; // Assuming conditionValue is also a local variable
  valBool : Boolean;
  currentForm: TForm; // <--- ADD THIS DECLARATION HERE    *)

  trimmedLine: String;
  parts: TStringArray; // Defined in InterpreterUtils
  cmd: String;
  val: String;
  // --- All local variables declared at the top, as Pascal requires ---
  targetLine: Integer;
  formName: String;
  foundForm: TForm;     // TForm is now recognized because of 'uses Forms;'
  i: Integer;
  varName: String;
  variableValue: TVariable; // For holding parsed variable values
  conditionVarName: String;
  conditionOp: String;
  conditionValue: String;
  conditionResult: Boolean;
  subName: String;
  ReturnAddr: PInteger;
  labelName: String;
  currentScanLine: Integer;
  scanTrimmedLine: String;
  scanParts: TStringArray;
  nestedWhiles: Integer;
  selectCaseVarName: String;
  caseValue: String;
  nestedSelects: Integer;
  endSelectLine: Integer;
  caseElseLine: Integer;
  foundEndSelect: Boolean;
  selectVarValue: TVariable;
  match: Boolean;
  valInt: Integer; // <--- DECLARE IT HERE WITHOUT INITIALIZATION
  //conditionValue: String; // Assuming conditionValue is also a local variable
  valBool : Boolean;
  currentForm: TForm; // <--- ADD THIS DECLARATION HERE  trimmedLine: String;
  parts: TStringArray; // Assuming TStringArray is defined elsewhere
  cmd: String;

  // Variables for 'SHOW' command (from previous fixes)
  formName: String;
  foundForm: TForm;
  currentForm: TForm;
  targetLine: Integer; // used in SHOW command (and potentially elsewhere)

  // Variables for 'IF' command (from previous fix for valInt/conditionValue)
  PVar: PVariable;
  conditionValue: String;
  valInt: Integer;

  // Variables for 'SELECT CASE' command (NEWLY ADDED/CONFIRMED)
  currentScanLine: Integer; // For scanning lines within SELECT CASE
  endSelectLine: Integer;   // To mark the end of the SELECT CASE block
  caseFound: Boolean;       // To track if a case has already matched
  nestedSelects: Integer;   // To handle nested SELECT CASE statements
  scanTrimmedLine: String;  // For parsing lines within SELECT CASE
  scanParts: TStringArray;  // For parsing lines within SELECT CASE
  caseValue: String;        // The value from the CASE statement
  match: Boolean;           // Result of comparison between selectVarValue and caseValue
  valBool: Boolean;         // For boolean comparisons in CASE statement
  // You also need 'selectVarValue' to be declared, it's a PVariable
  // (or whatever type holds your interpreter's variable values)
  //selectVarValue: TVariableValue; // Assuming this is the type of selectVarValue
  //i: Integer; // Loop counter, typically used in several places




begin // Start of ExecuteLine's implementation
  trimmedLine := Trim(Line);

  // Skip empty lines or lines that are just labels
  if (Length(trimmedLine) = 0) or (Pos(':', trimmedLine) = Length(trimmedLine)) then
  begin
    Exit; // Do nothing for empty lines or pure labels
  end;

  parts := SplitString(trimmedLine, ' '); // SplitString is from InterpreterUtils
  cmd := LowerCase(parts[0]);

  // valint
  valInt := StrToIntDef(conditionValue, 0); // <--- INITIALIZE IT HERE IN THE EXECUTABLE CODE

  // --- Command Dispatcher (large if-else if block) ---
  if cmd = 'form' then
  begin
    // 'FORM <Name>' is a declaration, not an executable instruction at runtime.
    // Its location is already stored during the BuildForms pass.
    Exit;
  end
  else if (cmd = 'end') and (Length(parts) >= 2) and (LowerCase(parts[1]) = 'form') then
  begin
    // 'END FORM' is also a declaration boundary, not an executable instruction.
    Exit;
  end
  else if cmd = 'show' then
  begin
    if Length(parts) < 2 then
    begin
      raise Exception.CreateFmt('Syntax error: SHOW requires a form name at line %d', [pc + 1]);
    end;
    formName := LowerCase(parts[1]);

    foundForm := nil;
    // --- Command Dispatcher (large if-else if block) ---
  if cmd = 'form' then
  begin
    // 'FORM <Name>' is a declaration, not an executable instruction at runtime.
    // Its location is already stored during the BuildForms pass.
    Exit;
  end
  else if (cmd = 'end') and (Length(parts) >= 2) and (LowerCase(parts[1]) = 'form') then
  begin
    // 'END FORM' is also a declaration boundary, not an executable instruction.
    Exit;
  end
  else if cmd = 'show' then
  begin
    if Length(parts) < 2 then
    begin
      raise Exception.CreateFmt('Syntax error: SHOW requires a form name at line %d', [pc + 1]);
    end;
    formName := LowerCase(parts[1]);

    // Find the 'show' command block:
    // ... (previous error checks) ...

    foundForm := nil;


    // --- FIX START ---
    for i := 0 to RuntimeForms.Count - 1 do
    begin
      if (RuntimeForms.Items[i] is TForm) then
      begin
        currentForm := TForm(RuntimeForms.Items[i]); // This line will now correctly use the declared variable

        if SameText(currentForm.Name, formName) then
        begin
          foundForm := currentForm;
          Break;
        end;
      end;
    end;
    // --- FIX END ---
    if Assigned(foundForm) then
    begin
      foundForm.Show; // Call the Show method of the existing TForm instance
    end
    else
    begin
      // If the form instance wasn't found, auto-create it (VB6 style)
      // Check if it's a known form from the BuildForms pass (FormLocations)
      targetLine := FormLocations.IndexOf(formName); // targetLine should be declared at the top of ExecuteLine
      if targetLine <> -1 then
      begin
        // Get the name (original casing from FormLocations.Strings, not .Keys)
        formName := FormLocations.Strings[targetLine];

        // --- FIX START ---
        // Correct TForm.Create call: it only takes one parameter (AOwner).
        // Set the Name property separately after creation.
        foundForm := TForm.Create(nil); // Create the form with nil owner
        foundForm.Name := formName;     // Set its Name property
        // Add the newly created form to your RuntimeForms list for future reference
        RuntimeForms.Add(foundForm);
        // --- FIX END ---

        foundForm.Show; // Then show it
      end
      else
      begin
        raise Exception.CreateFmt('Runtime error: Form "%s" not found at line %d', [formName, pc + 1]);
      end;
    end;
  end; // End of 'show' cmd block
  end
else if cmd = 'dim' then
  begin
    if Length(parts) < 4 then
      raise Exception.CreateFmt('Syntax error: DIM requires variable name and type at line %d', [pc + 1]);
    varName := LowerCase(parts[1]);

    if LowerCase(parts[2]) <> 'as' then
      raise Exception.CreateFmt('Syntax error: DIM expects "AS" at line %d', [pc + 1]);

    variableValue.vtype := vtUndefined; // Default to undefined
    variableValue.intValue := 0;
    variableValue.strValue := '';
    variableValue.boolValue := False;

    if LowerCase(parts[3]) = 'integer' then
      variableValue.vtype := vtInteger
    else if LowerCase(parts[3]) = 'string' then
      variableValue.vtype := vtString
    else if LowerCase(parts[3]) = 'boolean' then
      variableValue.vtype := vtBoolean
    else
      raise Exception.CreateFmt('Syntax error: DIM unknown type "%s" at line %d', [parts[3], pc + 1]);

    SetVar(varName, variableValue); // Initialize variable with default type and value
  end
  else if cmd = 'let' then
  begin
    if Length(parts) < 3 then
      raise Exception.CreateFmt('Syntax error: LET requires variable and value at line %d', [pc + 1]);
    varName := LowerCase(parts[1]);
    if parts[2] <> '=' then
      raise Exception.CreateFmt('Syntax error: LET expects "=" at line %d', [pc + 1]);

    val := Copy(trimmedLine, Pos('=', trimmedLine) + 1, MaxInt); // Get everything after '='
    val := Trim(val);

    PVar := GetVar(varName); // This line will now correctly use the declared PVar
      if PVar = nil then
    raise Exception.CreateFmt('Runtime error: Variable "%s" not declared at line %d', [varName, pc + 1]);

    // Assign value based on variable type
    case PVar^.vtype of
      vtInteger: begin
        PVar^.intValue := StrToIntDef(val, 0); // Convert string to integer
        PVar^.strValue := '';
        PVar^.boolValue := False;
      end;
      vtString: begin
        // Remove quotes if present
        if (Length(val) >= 2) and (val[1] = '"') and (val[Length(val)] = '"') then
          val := Copy(val, 2, Length(val) - 2);
        PVar^.strValue := val;
        PVar^.intValue := 0;
        PVar^.boolValue := False;
      end;
      vtBoolean: begin
        if SameText(val, 'true') then
          PVar^.boolValue := True
        else if SameText(val, 'false') then
          PVar^.boolValue := False
        else
          PVar^.boolValue := (StrToIntDef(val, 0) <> 0); // Allow numeric conversion for boolean
        PVar^.strValue := '';
        PVar^.intValue := 0;
      end;
      vtUndefined:
        raise Exception.CreateFmt('Runtime error: Cannot assign value to uninitialized variable "%s" at line %d', [varName, pc + 1]);
    end; // case PVar^.vtype
  end
  else if cmd = 'print' then
  begin
    if Length(parts) < 2 then
      raise Exception.CreateFmt('Syntax error: PRINT requires an argument at line %d', [pc + 1]);

    val := Copy(trimmedLine, Pos(' ', trimmedLine) + 1, MaxInt);
    val := Trim(val);

    if (Length(val) >= 2) and (val[1] = '"') and (val[Length(val)] = '"') then
    begin
      // It's a string literal
      Writeln(Copy(val, 2, Length(val) - 2));
    end
    else
    begin
      // It's a variable name
      PVar := GetVar(LowerCase(val));
      if PVar = nil then
        raise Exception.CreateFmt('Runtime error: Variable "%s" not declared for PRINT at line %d', [val, pc + 1]);

      case PVar^.vtype of
        vtInteger: Writeln(PVar^.intValue);
        vtString: Writeln(PVar^.strValue);
        vtBoolean: Writeln(BoolToStr(PVar^.boolValue)); // Assuming BoolToStr is defined or used
        vtUndefined: Writeln('Undefined variable value.');
      end;
    end;
  end
  else if cmd = 'input' then
  begin
    if Length(parts) < 2 then
      raise Exception.CreateFmt('Syntax error: INPUT requires a variable name at line %d', [pc + 1]);

    varName := LowerCase(parts[1]);
    Write('? '); // Prompt for input

    PVar := GetVar(varName);
    if PVar = nil then
      raise Exception.CreateFmt('Runtime error: Variable "%s" not declared for INPUT at line %d', [varName, pc + 1]);

    Readln(val); // Read input as string

    case PVar^.vtype of
      vtInteger: begin
        PVar^.intValue := StrToIntDef(val, 0);
        PVar^.strValue := ''; PVar^.boolValue := False;
      end;
      vtString: begin
        PVar^.strValue := val;
        PVar^.intValue := 0; PVar^.boolValue := False;
      end;
      vtBoolean: begin
        if SameText(val, 'true') then
          PVar^.boolValue := True
        else if SameText(val, 'false') then
          PVar^.boolValue := False
        else
          PVar^.boolValue := (StrToIntDef(val, 0) <> 0);
        PVar^.strValue := ''; PVar^.intValue := 0;
      end;
      vtUndefined:
        raise Exception.CreateFmt('Runtime error: Cannot input value to uninitialized variable "%s" at line %d', [varName, pc + 1]);
    end; // case PVar^.vtype
  end
  else if cmd = 'if' then
  begin
    // Simplified IF: IF <var> <op> <value> THEN GOTO <label>
    if Length(parts) < 6 then
      raise Exception.CreateFmt('Syntax error: IF requires variable, operator, value, THEN, GOTO, label at line %d', [pc + 1]);

    conditionVarName := LowerCase(parts[1]);
    conditionOp := LowerCase(parts[2]); // e.g., "=", "<", ">", "<>", "<=", ">="
    conditionValue := LowerCase(parts[3]);
    labelName := LowerCase(parts[6]); // Assuming 'THEN' and 'GOTO' are parts[4] and parts[5]

    if not (SameText(parts[4], 'then') and SameText(parts[5], 'goto')) then
      raise Exception.CreateFmt('Syntax error: IF statement requires "THEN GOTO" at line %d', [pc + 1]);

    PVar := GetVar(conditionVarName);
    if PVar = nil then
      raise Exception.CreateFmt('Runtime error: Variable "%s" not declared in IF statement at line %d', [conditionVarName, pc + 1]);

    // Evaluate the condition
    conditionResult := False;
    if PVar^.vtype = vtInteger then
    begin
      case conditionOp of
        '=': conditionResult := (PVar^.intValue = valInt);
        '<': conditionResult := (PVar^.intValue < valInt);
        '>': conditionResult := (PVar^.intValue > valInt);
        '<>': conditionResult := (PVar^.intValue <> valInt);
        '<=': conditionResult := (PVar^.intValue <= valInt);
        '>=': conditionResult := (PVar^.intValue >= valInt);
        else raise Exception.CreateFmt('Runtime error: Unsupported operator "%s" for integer comparison at line %d', [conditionOp, pc + 1]);
      end;
    end
    else if PVar^.vtype = vtString then
    begin
      // Remove quotes from conditionValue if present
      if (Length(conditionValue) >= 2) and (conditionValue[1] = '"') and (conditionValue[Length(conditionValue)] = '"') then
        conditionValue := Copy(conditionValue, 2, Length(conditionValue) - 2);
      case conditionOp of
        '=': conditionResult := SameText(PVar^.strValue, conditionValue);
        '<>': conditionResult := not SameText(PVar^.strValue, conditionValue);
        else raise Exception.CreateFmt('Runtime error: Unsupported operator "%s" for string comparison at line %d', [conditionOp, pc + 1]);
      end;
    end
    else if PVar^.vtype = vtBoolean then
    begin
      //var valBool: Boolean;
      if SameText(conditionValue, 'true') then valBool := True
      else if SameText(conditionValue, 'false') then valBool := False
      else valBool := (StrToIntDef(conditionValue, 0) <> 0);

      case conditionOp of
        '=': conditionResult := (PVar^.boolValue = valBool);
        '<>': conditionResult := (PVar^.boolValue <> valBool);
        else raise Exception.CreateFmt('Runtime error: Unsupported operator "%s" for boolean comparison at line %d', [conditionOp, pc + 1]);
      end;
    end;


    if conditionResult then
    begin
      targetLine := Labels.IndexOf(labelName); // Labels from InterpreterUtils
      if targetLine <> -1 then
      begin
        pc := PLineNumberData(Labels.Objects[targetLine])^.Line -1; // -1 because Inc(pc) at loop end adds 1
      end
      else
      begin
        raise Exception.CreateFmt('Runtime error: Label "%s" not found for IF GOTO at line %d', [labelName, pc + 1]);
      end;
    end;
  end
  else if cmd = 'goto' then
  begin
    if Length(parts) < 2 then
      raise Exception.CreateFmt('Syntax error: GOTO requires a label at line %d', [pc + 1]);
    labelName := LowerCase(parts[1]);
    targetLine := Labels.IndexOf(labelName);
    if targetLine <> -1 then
    begin
      pc := PLineNumberData(Labels.Objects[targetLine])^.Line -1; // -1 because Inc(pc) at loop end adds 1
    end
    else
    begin
      raise Exception.CreateFmt('Runtime error: Label "%s" not found for GOTO at line %d', [labelName, pc + 1]);
    end;
  end
  else if cmd = 'call' then
  begin
    if Length(parts) < 2 then
      raise Exception.CreateFmt('Syntax error: CALL requires a subroutine name at line %d', [pc + 1]);
    subName := LowerCase(parts[1]);
    targetLine := Subs.IndexOf(subName); // Subs from InterpreterUtils
    if targetLine <> -1 then
    begin
      // Push return address onto the stack (current pc + 1 for next line)
      New(ReturnAddr);
      ReturnAddr^ := pc + 1;
      Stack.Add(TObject(ReturnAddr));

      pc := PLineNumberData(Subs.Objects[targetLine])^.Line -1; // Jump to subroutine start (pc will be incremented)
    end
    else
    begin
      raise Exception.CreateFmt('Runtime error: Subroutine "%s" not found for CALL at line %d', [subName, pc + 1]);
    end;
  end
  else if cmd = 'endsub' then
  begin
    if Stack.Count > 0 then
    begin
      // Pop return address from stack
      ReturnAddr := PInteger(Stack.Last);
      Stack.Remove(Stack.Last);
      pc := ReturnAddr^ -1; // Jump back to stored return address (pc will be incremented)
      Dispose(ReturnAddr); // Free the allocated PInteger
    end
    else
    begin
      raise Exception.Create('Runtime error: END SUB without matching CALL at line ' + IntToStr(pc + 1));
    end;
  end
  else if cmd = 'while' then
  begin
    // Simplified WHILE: WHILE <var> <op> <value>
    if Length(parts) < 4 then
      raise Exception.CreateFmt('Syntax error: WHILE requires variable, operator, value at line %d', [pc + 1]);

    conditionVarName := LowerCase(parts[1]);
    conditionOp := LowerCase(parts[2]); // e.g., "=", "<", ">", "<>", "<=", ">="
    conditionValue := LowerCase(parts[3]);

    PVar := GetVar(conditionVarName);
    if PVar = nil then
      raise Exception.CreateFmt('Runtime error: Variable "%s" not declared in WHILE statement at line %d', [conditionVarName, pc + 1]);

    // Evaluate the condition (same logic as IF for now)
    conditionResult := False;
    if PVar^.vtype = vtInteger then
    begin
      //var valInt: Integer := StrToIntDef(conditionValue, 0);
      case conditionOp of
        '=': conditionResult := (PVar^.intValue = valInt);
        '<': conditionResult := (PVar^.intValue < valInt);
        '>': conditionResult := (PVar^.intValue > valInt);
        '<>': conditionResult := (PVar^.intValue <> valInt);
        '<=': conditionResult := (PVar^.intValue <= valInt);
        '>=': conditionResult := (PVar^.intValue >= valInt);
        else raise Exception.CreateFmt('Runtime error: Unsupported operator "%s" for integer comparison at line %d', [conditionOp, pc + 1]);
      end;
    end
    else if PVar^.vtype = vtString then
    begin
      if (Length(conditionValue) >= 2) and (conditionValue[1] = '"') and (conditionValue[Length(conditionValue)] = '"') then
        conditionValue := Copy(conditionValue, 2, Length(conditionValue) - 2);
      case conditionOp of
        '=': conditionResult := SameText(PVar^.strValue, conditionValue);
        '<>': conditionResult := not SameText(PVar^.strValue, conditionValue);
        else raise Exception.CreateFmt('Runtime error: Unsupported operator "%s" for string comparison at line %d', [conditionOp, pc + 1]);
      end;
    end
    else if PVar^.vtype = vtBoolean then
    begin
      //var valBool: Boolean;
      if SameText(conditionValue, 'true') then valBool := True
      else if SameText(conditionValue, 'false') then valBool := False
      else valBool := (StrToIntDef(conditionValue, 0) <> 0);

      case conditionOp of
        '=': conditionResult := (PVar^.boolValue = valBool);
        '<>': conditionResult := (PVar^.boolValue <> valBool);
        else raise Exception.CreateFmt('Runtime error: Unsupported operator "%s" for boolean comparison at line %d', [conditionOp, pc + 1]);
      end;
    end;

    if not conditionResult then
    begin
      // Condition is false, skip to WEND
      nestedWhiles := 0;
      currentScanLine := pc + 1; // Start scanning from next line

      while currentScanLine < Code.Count do
      begin
        scanTrimmedLine := LowerCase(Trim(Code[currentScanLine]));
        scanParts := SplitString(scanTrimmedLine, ' ');

        if (Length(scanParts) > 0) then
        begin
          if scanParts[0] = 'while' then
            Inc(nestedWhiles)
          else if scanParts[0] = 'wend' then
          begin
            if nestedWhiles = 0 then
            begin
              pc := currentScanLine; // Jump to WEND
              Exit; // Exit ExecuteLine; main loop will increment pc
            end
            else
              Dec(nestedWhiles);
          end;
        end;
        Inc(currentScanLine);
      end;
      raise Exception.CreateFmt('Runtime error: WEND not found for WHILE at line %d', [pc + 1]);
    end
    else
    begin
      // Condition is true, proceed. For proper nesting, you'd push loop frame here.
      // E.g., Push(TLoopFrame.Create(pc, ...))
    end;
  end
  else if cmd = 'wend' then
  begin
    // For proper nesting, you'd pop from a loop stack to find the corresponding WHILE.
    // E.g., If LoopStack.Count > 0 Then Pop and set pc back to the WHILE line - 1
    Exit; // For now, just exit as the WHILE block handles jumping if condition false
  end
  else if (cmd = 'select') and (Length(parts) >= 2) and (LowerCase(parts[1]) = 'case') then
  begin
    if Length(parts) < 3 then
      raise Exception.CreateFmt('Syntax error: SELECT CASE requires an expression at line %d', [pc + 1]);

    selectCaseVarName := LowerCase(parts[2]);
    PVar := GetVar(selectCaseVarName);
    if PVar = nil then
      raise Exception.CreateFmt('Runtime error: Variable "%s" not declared in SELECT CASE at line %d', [selectCaseVarName, pc + 1]);

    selectVarValue := PVar^; // Get the value of the variable being selected

    // --- Find End Select and Case Else (simplified scan at runtime) ---
    endSelectLine := -1;
    caseElseLine := -1;
    nestedSelects := 0;
    foundEndSelect := False;
    currentScanLine := pc + 1; // Start scanning from the line after SELECT CASE

    while currentScanLine < Code.Count do
    begin
      scanTrimmedLine := LowerCase(Trim(Code[currentScanLine]));
      scanParts := SplitString(scanTrimmedLine, ' ');

      if (Length(scanParts) > 0) then
      begin
        if (scanParts[0] = 'select') and (Length(scanParts) >= 2) and (scanParts[1] = 'case') then
          Inc(nestedSelects)
        else if (scanParts[0] = 'end') and (Length(scanParts) >= 2) and (scanParts[1] = 'select') then
        begin
          if nestedSelects = 0 then
          begin
            endSelectLine := currentScanLine;
            foundEndSelect := True;
            Break; // Found the matching END SELECT
          end
          else
            Dec(nestedSelects);
        end
        else if (scanParts[0] = 'case') and (Length(scanParts) >= 2) and (LowerCase(scanParts[1]) = 'else') and (nestedSelects = 0) then
        begin
          if caseElseLine = -1 then // Only set the first one found
            caseElseLine := currentScanLine;
        end;
      end;
      Inc(currentScanLine);
    end;
    if not foundEndSelect then
      raise Exception.CreateFmt('Runtime error: END SELECT not found for SELECT CASE at line %d', [pc + 1]);
    // --- End simplified Find End Select and Case Else ---

    // Now, go through the CASE statements to find a match
    currentScanLine := pc + 1; // Start from line after SELECT CASE
    caseFound := False;
    nestedSelects := 0; // Reset for actual case jumping

    while currentScanLine < endSelectLine do // Only scan up to END SELECT
    begin
      scanTrimmedLine := LowerCase(Trim(Code[currentScanLine]));
      scanParts := SplitString(scanTrimmedLine, ' ');

      if (Length(scanParts) > 0) then
      begin
        if (scanParts[0] = 'select') and (Length(scanParts) >= 2) and (scanParts[1] = 'case') then
          Inc(nestedSelects) // Handle nested selects, skip their cases
        else if (scanParts[0] = 'end') and (Length(scanParts) >= 2) and (scanParts[1] = 'select') then
          Dec(nestedSelects) // Exiting a nested select
        else if (scanParts[0] = 'case') and (nestedSelects = 0) then // Only process top-level cases
        begin
          if (Length(scanParts) >= 2) and (LowerCase(scanParts[1]) = 'else') then
          begin
            // This is CASE ELSE. If no other case matched yet, this is the one.
            if not caseFound then
            begin
              pc := currentScanLine - 1; // Jump to CASE ELSE (pc will be incremented)
              Exit;
            end;
          end
          else if Length(scanParts) >= 2 then
          begin
            // Standard CASE value (can be multiple values, or "To" ranges; simplified to single value here)
            caseValue := scanParts[1];
            // Remove quotes if string literal
            if (Length(caseValue) >= 2) and (caseValue[1] = '"') and (caseValue[Length(caseValue)] = '"') then
              caseValue := Copy(caseValue, 2, Length(caseValue) - 2);

            // Compare selectVarValue with caseValue
            match := False;
            case selectVarValue.vtype of
              vtInteger: match := (selectVarValue.intValue = StrToIntDef(caseValue, 0));
              vtString:  match := SameText(selectVarValue.strValue, caseValue);
              vtBoolean:
                begin
                  if SameText(caseValue, 'true') then valBool := True
                  else if SameText(caseValue, 'false') then valBool := False
                  else valBool := (StrToIntDef(caseValue, 0) <> 0);
                  match := (selectVarValue.boolValue = valBool);
                end;
            end; // case selectVarValue.vtype

            if match then
            begin
              pc := currentScanLine - 1; // Jump to matching CASE (pc will be incremented)
              caseFound := True; // Mark that a case has been found
              Exit; // Exit ExecuteLine; main loop will continue from this point
            end;
          end;
        end;
      end;
      Inc(currentScanLine); // Move to next line to scan
    end;

    // If no case matched and no CASE ELSE was executed, jump to END SELECT
    pc := endSelectLine - 1;

  end
  else if cmd = 'case' then
  begin
    // If we've executed a CASE (which jumped here), we must skip subsequent CASE
    // statements until we hit the END SELECT or the next CASE ELSE.
    // This requires proper `SelectCaseFrame` on a stack to track if a match occurred.
    // For now, if the `SELECT CASE` block didn't jump, it means no previous CASE matched.
    // If the interpreter logic jumped *into* a CASE, we should now jump *out* of subsequent cases.
    // This requires a more complex state management with a `SelectCaseStack`.
    Exit; // Proceed line by line for now. If a jump occurred, the pc is already set.
  end
  else if (cmd = 'end') and (Length(parts) >= 2) and (LowerCase(parts[1]) = 'select') then
  begin
    // If reached, simply exit this block
    Exit;
  end
  else
  begin
    raise Exception.CreateFmt('Syntax error: Unknown command "%s" at line %d', [cmd, pc + 1]);
  end; // <--- This 'end;' correctly closes the entire main IF-ELSE IF-ELSE block for commands.
end; // End of ExecuteLine procedure

initialization
  // --- ADD THESE INITIALIZATIONS ---
  Code := TStringList.Create;
  Vars := TStringList.Create;
  Labels := TStringList.Create;
  Subs := TStringList.Create;
  Stack := TObjectList.Create;
  FormLocations := TStringList.Create;

  // Initialize RuntimeForms here
  RuntimeForms := TObjectList.Create;
  RuntimeForms.OwnsObjects := True; // Crucial: Makes TObjectList responsible for freeing the TForm instances it contains

finalization
  // --- ADD THESE FINALIZATIONS ---
  FreeAndNil(Code);
  FreeAndNil(Vars);
  FreeAndNil(Labels);
  FreeAndNil(Subs);
  FreeAndNil(Stack);
  FreeAndNil(FormLocations);

  // Free RuntimeForms here
  FreeAndNil(RuntimeForms);
end.// End of unit InterpreterCore
