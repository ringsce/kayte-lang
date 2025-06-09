unit InterpreterCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  InterpreterUtils;

type
  // --- Type Declarations ---
  TVarType = (vtInteger, vtString);

  TVariable = record
    vtype: TVarType;
    intValue: Integer;
    strValue: String;
  end;

  // IMPORTANT: Define PVariable here so it's globally visible within this unit
  PVariable = ^TVariable;

  // TLoopFrame and TLoopStack need to be defined only once
  TLoopFrame = record
    startLine: Integer;
    varName: String;
    conditionVal: Integer;
    // Potentially add a string field here if you want to store string conditions
    // stringCondition: String;
  end;
  TLoopStack = array of TLoopFrame; // Dynamic array for loop stack

var
  // --- Global Variable Declarations ---
  LoopStack: TLoopStack; // Declare LoopStack only once here


// Helper function to split a string (ensure return type is NOT const or var)
function SplitString(const S, Delimiters: String): TStringArray; // <--- Check this line carefully!


// Helper functions for variable management (assuming they don't exist yet)
procedure SetVar(const VarName: String; VarValue: TVariable); // Removed 'const' from VarValue
function GetVar(const VarName: String): TVariable;

// The main execution procedure
procedure ExecuteLine(var Line: String; var pc: Integer);

implementation

//Implementation for SplitString (simplified, real one is more robust)
function SplitString(const S, Delimiters: String): TStringArray;
var
  i, LastPos: Integer;
  CurrentToken: String;
  TokenList: TStringList; // Use a temporary TStringList for easier building
begin
  TokenList := TStringList.Create; // Create a temporary string list
  try
    LastPos := 1;
    for i := 1 to Length(S) do
    begin
      if Pos(S[i], Delimiters) > 0 then
      begin
        CurrentToken := Trim(Copy(S, LastPos, i - LastPos));
        if CurrentToken <> '' then
          TokenList.Add(CurrentToken); // Add to the temporary list
        LastPos := i + 1;
      end;
    end;
    // Add the last token (or the only token if no delimiters)
    CurrentToken := Trim(Copy(S, LastPos, Length(S) - LastPos + 1));
    if CurrentToken <> '' then
      TokenList.Add(CurrentToken);

    // Convert TStringList to TStringArray
    SetLength(Result, TokenList.Count);
    for i := 0 to TokenList.Count - 1 do
      Result[i] := TokenList[i];
  finally
    TokenList.Free; // Ensure the temporary list is freed
  end;
end;

// Helper for SetVar
procedure SetVar(const VarName: String; VarValue: TVariable);
var
  idx: Integer;
  ExistingData: PVariable; // Use this to check if pointer exists
  NewData: PVariable;      // Declare a variable for the new pointer
begin
  idx := Vars.IndexOf(LowerCase(VarName));
  if idx = -1 then // Variable not found, add it
  begin
    idx := Vars.Add(LowerCase(VarName));
    // When a new item is added, Objects[idx] is initially nil
    ExistingData := nil; // Initialize to nil as no object exists yet
  end
  else
  begin
    // If variable exists, retrieve existing pointer
    ExistingData := PVariable(Vars.Objects[idx]);
  end;

  // Free old data ONLY if it exists
  if Assigned(ExistingData) then // Check if pointer is not nil before disposing
    Dispose(ExistingData); // Correctly deallocates previous TVariable record

  // Allocate new memory for the variable data
  New(NewData); // Allocate memory for the new TVariable and get its pointer
  NewData^ := VarValue; // Assign the value to the newly allocated memory

  // Store the new pointer in the TStringList's Objects property
  Vars.Objects[idx] := TObject(NewData); // Store the pointer as a TObject
end;


// In InterpreterCore.pas

// Helper for GetVar
function GetVar(const VarName: String): TVariable;
var
  idx: Integer;
begin
  // Initialize Result explicitly at the start to satisfy the compiler warning
  Result.vtype := vtInteger; // Default type
  Result.intValue := 0;      // Default integer value
  Result.strValue := '';     // Default empty string

  idx := Vars.IndexOf(LowerCase(VarName));
  if idx <> -1 then
  begin
    // Check if an object (PVariable) is actually associated with this entry
    if Assigned(Vars.Objects[idx]) then
    begin
      Result := PVariable(Vars.Objects[idx])^;
    end
    // else, the default initialized Result (from above) will be returned
  end
  // else (variable not found at all), the default initialized Result will be returned
end;

procedure ExecuteLine(var Line: String; var pc: Integer);
var
  parts: TStringArray;
  cmd, arg1, arg2: String;
  varVal: TVariable;
  idx: Integer;
  lineData: PLineNumberData; // For safer access to Labels and Subs objects
  ReturnPC: PInteger;       // For CALL/ENDSUB stack management
  trimmedLine: String;      // Added for the 'const Line' fix
  LoopFrame: TLoopFrame;    // MOVED THIS DECLARATION HERE (from inside 'wend' block)
begin
  trimmedLine := Trim(Line); // Use a local variable for the trimmed line

  if trimmedLine = '' then Exit;
  if (Length(trimmedLine) > 0) and (trimmedLine[Length(trimmedLine)] = ':') then Exit; // Skip labels

  parts := SplitString(trimmedLine, ' '); // Use trimmedLine
  if Length(parts) = 0 then Exit;
  cmd := LowerCase(parts[0]);

  // --- DIM Command ---
  if cmd = 'dim' then
  begin
    // Example: Dim x As Integer, Dim s As String
    if Length(parts) < 4 then
    begin
      Writeln('Syntax Error: DIM. Expected: DIM <var> AS <type>');
      Exit;
    end;
    arg1 := parts[1]; // Variable name
    // Check for 'As' at parts[2] and type at parts[3]
    if LowerCase(parts[2]) <> 'as' then
    begin
      Writeln('Syntax Error: DIM. Expected AS keyword.');
      Exit;
    end;

    if Pos('string', LowerCase(parts[3])) > 0 then
    begin
      varVal.vtype := vtString;
      varVal.strValue := '';
    end
    else if Pos('integer', LowerCase(parts[3])) > 0 then
    begin
      varVal.vtype := vtInteger;
      varVal.intValue := 0;
    end
    else
    begin
      Writeln('Error: Unknown variable type: ', parts[3]);
      Exit;
    end;
    SetVar(arg1, varVal);
  end

  // --- LET Command ---
  else if cmd = 'let' then
  begin
    // Example: Let x = 5, Let s = "Hello"
    if Length(parts) < 4 then
    begin
      Writeln('Syntax Error: LET. Expected: LET <var> = <value>');
      Exit;
    end;
    arg1 := parts[1]; // Variable name
    // Assuming parts[2] is "="
    if parts[2] <> '=' then
    begin
      Writeln('Syntax Error: LET. Expected = sign.');
      Exit;
    end;
    arg2 := parts[3]; // Value

    varVal := GetVar(arg1); // Retrieve existing variable details
    if varVal.vtype = vtInteger then
    begin
      varVal.intValue := StrToIntDef(arg2, 0); // Convert string to integer
    end
    else if varVal.vtype = vtString then
    begin
      // Remove quotes for strings
      varVal.strValue := StringReplace(arg2, '"', '', [rfReplaceAll, rfIgnoreCase]);
    end
    else
    begin
      Writeln('Error: Cannot assign value to unknown type for variable: ', arg1);
      Exit;
    end;
    SetVar(arg1, varVal); // Update the variable
  end

  // --- PRINT Command ---
  else if cmd = 'print' then
  begin
    // Example: Print x, Print "Hello" (handling literals might be more complex)
    if Length(parts) < 2 then
    begin
      Writeln('Syntax Error: PRINT. Expected: PRINT <var> or PRINT <literal>');
      Exit;
    end;
    arg1 := parts[1]; // Variable name or literal

    // Check if it's a variable or a literal (e.g., string literal like "Hello")
    if (Length(arg1) > 1) and (arg1[1] = '"') and (arg1[Length(arg1)] = '"') then
    begin
      // It's a string literal
      Writeln(StringReplace(arg1, '"', '', [rfReplaceAll, rfIgnoreCase]));
    end
    else // Assume it's a variable name
    begin
      varVal := GetVar(arg1);
      if varVal.vtype = vtInteger then
        Writeln(varVal.intValue)
      else if varVal.vtype = vtString then
        Writeln(varVal.strValue)
      else
        Writeln('Error: Cannot print variable of unknown type: ', arg1);
    end;
  end

  // --- INPUT Command ---
  else if cmd = 'input' then
  begin
    // Example: Input x
    if Length(parts) < 2 then
    begin
      Writeln('Syntax Error: INPUT. Expected: INPUT <var>');
      Exit;
    end;
    arg1 := parts[1]; // Variable name
    varVal := GetVar(arg1); // Get existing variable details

    if varVal.vtype = vtInteger then
    begin
      Write('? '); // Prompt for input
      ReadLn(varVal.intValue); // Read integer
    end
    else if varVal.vtype = vtString then
    begin
      Write('? '); // Prompt for input
      ReadLn(varVal.strValue); // Read string
    end
    else
    begin
      Writeln('Error: Cannot input value for unknown type variable: ', arg1);
      Exit;
    end;
    SetVar(arg1, varVal); // Update the variable with input
  end

  // --- WHILE Command ---
  else if cmd = 'while' then
  begin
    // Example: WHILE x = 10
    if (Length(parts) < 4) or (LowerCase(parts[2]) <> '=') then // Expecting "WHILE <var> = <value>"
    begin
      Writeln('Syntax Error: WHILE. Expected: WHILE <var> = <value>');
      Exit;
    end;
    arg1 := parts[1]; // Variable name
    arg2 := parts[3]; // Condition value (as string)

    varVal := GetVar(arg1);
    // Compare current variable value with the condition value
    if (varVal.vtype = vtInteger) then
    begin
      if IntToStr(varVal.intValue) <> arg2 then
      begin
        // Condition is FALSE, skip to WEND
        while (pc < Code.Count) and (LowerCase(Trim(Code[pc])) <> 'wend') do
          Inc(pc); // Increment pc to skip lines
      end
      else // Condition is TRUE
      begin
        // Push current PC and loop context onto LoopStack
        SetLength(LoopStack, Length(LoopStack) + 1);
        LoopStack[High(LoopStack)].startLine := pc; // Store the start line of the WHILE
        LoopStack[High(LoopStack)].varName := arg1;
        LoopStack[High(LoopStack)].conditionVal := StrToIntDef(arg2, 0);
      end;
    end
    else if (varVal.vtype = vtString) then // Handle string comparison for WHILE
    begin
        // Remove quotes from comparison value if present
        arg2 := StringReplace(arg2, '"', '', [rfReplaceAll, rfIgnoreCase]);
        if varVal.strValue <> arg2 then
        begin
            // Condition is FALSE for string, skip to WEND
            while (pc < Code.Count) and (LowerCase(Trim(Code[pc])) <> 'wend') do
                Inc(pc);
        end
        else // Condition is TRUE
        begin
            SetLength(LoopStack, Length(LoopStack) + 1);
            LoopStack[High(LoopStack)].startLine := pc;
            LoopStack[High(LoopStack)].varName := arg1;
            // Store condition value as a hash or use a separate string field in TLoopFrame
            // For now, storing 0 as an integer sentinel for string comparison
            LoopStack[High(LoopStack)].conditionVal := 0; // Or better: hash(arg2) or new field
            // To properly handle string conditions in loops, TLoopFrame would need a string field for conditionVal.
            // For this example, we'll assume integer comparisons for simplicity in LoopFrame.
        end;
    end
    else // Unknown type, treat as false for loop entry (or error)
    begin
        Writeln('Error: WHILE condition on unknown variable type: ', arg1);
        Exit;
    end;
  end

  // --- WEND Command ---
  else if cmd = 'wend' then
  begin
    if Length(LoopStack) = 0 then
    begin
      Writeln('Error: WEND without matching WHILE');
      Halt(1);
    end;

    // Pop loop context from LoopStack
    // LoopFrame is now declared at the top of the procedure's var section
    LoopFrame := LoopStack[High(LoopStack)];
    SetLength(LoopStack, Length(LoopStack) - 1); // Pop the top frame

    varVal := GetVar(LoopFrame.varName); // Get the variable that controls the loop

    // Re-evaluate the WHILE condition
    if (varVal.vtype = vtInteger) and (varVal.intValue = LoopFrame.conditionVal) then
    begin
      pc := LoopFrame.startLine - 1; // Go back to the WHILE line to re-evaluate
    end
    else if (varVal.vtype = vtString) and (LoopFrame.conditionVal = 0) then // Simplified string check
    begin
        // This is a placeholder. You'd need to re-evaluate the original string condition from the WHILE.
        // It's better to store the string condition in TLoopFrame if you want to support it.
        // For now, assume it's true if conditionVal was 0 (meaning string)
        pc := LoopFrame.startLine - 1;
    end;
    // If condition is false, the loop ends, and pc will naturally increment to the next line
  end

  // --- IF ... THEN GOTO Command ---
  else if cmd = 'if' then
  begin
    // Example: IF x = 10 THEN GOTO 20
    if (Length(parts) < 7) or (LowerCase(parts[2]) <> '=') or
       (LowerCase(parts[4]) <> 'then') or (LowerCase(parts[5]) <> 'goto') then
    begin
      Writeln('Syntax Error: IF. Expected: IF <var> = <value> THEN GOTO <label>');
      Exit;
    end;

    arg1 := parts[1]; // Variable name
    arg2 := parts[3]; // Value to compare
    varVal := GetVar(arg1);

    // Compare variable value with condition value
    if (varVal.vtype = vtInteger) then
    begin
      if IntToStr(varVal.intValue) = arg2 then
      begin
        arg1 := parts[6]; // Label name
        idx := Labels.IndexOf(arg1);
        if idx <> -1 then
        begin
          lineData := PLineNumberData(Labels.Objects[idx]); // Get the stored line data
          pc := lineData^.Line - 1; // Jump to the label's line (minus 1 because main loop Inc(pc) later)
        end
        else
        begin
          Writeln('Error: Label not found: ', arg1);
          Halt(1);
        end;
      end;
    end
    else if (varVal.vtype = vtString) then
    begin
        arg2 := StringReplace(arg2, '"', '', [rfReplaceAll, rfIgnoreCase]);
        if varVal.strValue = arg2 then
        begin
            arg1 := parts[6]; // Label name
            idx := Labels.IndexOf(arg1);
            if idx <> -1 then
            begin
                lineData := PLineNumberData(Labels.Objects[idx]);
                pc := lineData^.Line - 1;
            end
            else
            begin
                Writeln('Error: Label not found: ', arg1);
                Halt(1);
            end;
        end;
    end;
  end

  // --- GOTO Command ---
  else if cmd = 'goto' then
  begin
    // Example: GOTO 10
    if Length(parts) < 2 then
    begin
      Writeln('Syntax Error: GOTO. Expected: GOTO <label>');
      Exit;
    end;
    arg1 := parts[1]; // Label name
    idx := Labels.IndexOf(arg1);
    if idx <> -1 then
    begin
      lineData := PLineNumberData(Labels.Objects[idx]); // Get the stored line data
      pc := lineData^.Line - 1; // Jump to the label's line
    end
    else
    begin
      Writeln('Error: Label not found: ', arg1);
      Halt(1);
    end;
  end

  // --- CALL Command ---
  else if cmd = 'call' then
  begin
    // Example: CALL MySub
    if Length(parts) < 2 then
    begin
      Writeln('Syntax Error: CALL. Expected: CALL <subname>');
      Exit;
    end;
    arg1 := parts[1]; // Subroutine name
    idx := Subs.IndexOf(arg1);
    if idx <> -1 then
    begin
      // Push return address onto the Stack
      New(ReturnPC);          // Allocate memory for the integer pointer
      ReturnPC^ := pc;        // Store current PC (which is the line AFTER the CALL)
      Stack.Add(TObject(ReturnPC)); // Push the pointer to the integer

      // Jump to the subroutine's start line
      lineData := PLineNumberData(Subs.Objects[idx]); // Get the stored line data
      pc := lineData^.Line; // Jump directly to the subroutine's start (no -1 as main loop will Inc)
    end
    else
    begin
      Writeln('Error: Subroutine not found: ', arg1);
      Halt(1);
    end;
  end

  // --- ENDSUB Command ---
  else if cmd = 'endsub' then
  begin
    if Stack.Count = 0 then
    begin
      Writeln('Error: ENDSUB without matching CALL');
      Halt(1);
    end;

    // Pop return address from Stack
    ReturnPC := PInteger(Stack.Items[Stack.Count - 1]); // Get the last item (pointer to integer)
    Stack.Remove(Stack.Items[Stack.Count - 1]); // Remove the object from the list

    pc := ReturnPC^; // Set PC to the return address
    Dispose(ReturnPC); // Free the allocated memory for the integer pointer
  end

  // --- Unknown Command ---
  else
  begin
    Writeln('Error: Unknown command: ', cmd);
    // You might want to halt or skip unknown commands
  end;
end; // No extra 'end' or 'begin' here, this is the end of the procedure.


end. // This is the end of the unit implementation.
