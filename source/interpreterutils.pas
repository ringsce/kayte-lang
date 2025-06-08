unit InterpreterUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Define a record or a small class to hold the line number,
  // making the TStringList.AddObject usage safer.
  PLineNumberData = ^TLineNumberData;
  TLineNumberData = record
    Line: Integer;
  end;

  TSubroutineMap = class(TStringList)
  public
    // Add method now uses the safe way to store line numbers
    procedure Add(SubName: String; LineNumber: Integer);
    // You might want a function to retrieve the line number safely too
    function GetLineNumber(SubName: String): Integer;
  end;

// Global variables that will be managed by the interpreter.
// Declaring them here makes them accessible throughout the unit and to the main program.
// Using 'var' in the interface section declares global variables.
var
  Code: TStringList;
  Vars: TStringList;      // This was declared in the main program, but seems like a global interpreter state
  Labels: TStringList;
  Subs: TSubroutineMap;   // Changed to TSubroutineMap type
  Stack: TObjectList;     // Assuming TStack is TObjectList or similar (need its definition if custom)
  Variables: TStringList; // From InitInterpreter

// Procedures that need to be called from the main program
procedure InitInterpreter;
procedure FreeInterpreter;
procedure LoadCode; // Declared in interface so main program can call it
procedure BuildLabels(Code, Labels: TStringList);
procedure BuildSubs(Code: TStringList; var Subs: TSubroutineMap); // var is correct here

implementation

{ TSubroutineMap }

procedure TSubroutineMap.Add(SubName: String; LineNumber: Integer);
var
  Data: PLineNumberData;
begin
  New(Data); // Allocate memory for the record
  Data^.Line := LineNumber;
  Self.AddObject(SubName, TObject(Data)); // Store the pointer to the record
end;

function TSubroutineMap.GetLineNumber(SubName: String): Integer;
var
  Index: Integer;
  Data: PLineNumberData;
begin
  Index := Self.IndexOf(SubName);
  if Index <> -1 then
  begin
    Data := PLineNumberData(Self.Objects[Index]);
    Result := Data^.Line;
  end
  else
  begin
    Result := -1; // Or raise an error, or some other indicator of not found
  end;
end;


procedure BuildLabels(Code, Labels: TStringList);
var
  i: Integer;
  line, labelName: String;
  Data: PLineNumberData;
begin
  for i := 0 to Code.Count - 1 do
  begin
    line := Trim(Code[i]);
    if (line <> '') and (line[Length(line)] = ':') then
    begin
      labelName := Copy(line, 1, Length(line) - 1);
      New(Data);
      Data^.Line := i;
      Labels.AddObject(labelName, TObject(Data));
    end;
  end;
end;

procedure BuildSubs(Code: TStringList; var Subs: TSubroutineMap);
var
  i: Integer;
  line: String;
begin
  // Subs is already created in InitInterpreter, no need to create it again here.
  // This procedure just populates the existing Subs map.
  for i := 0 to Code.Count - 1 do
  begin
    line := Trim(Code[i]);
    // Assuming subroutine declaration is "Sub SubName:"
    if (LowerCase(Copy(line, 1, 3)) = 'sub') and (Length(line) > 4) and (line[Length(line)] = ':') then // Added check for ":"
    begin
      // Extract "SubName" from "Sub SubName:"
      Subs.Add(Copy(line, 5, Length(line) - 5 -1), i); // Adjusting substring to remove "Sub " and ":"
    end;
  end;
end;

{ LoadCode, InitInterpreter, FreeInterpreter - now in interface section }

procedure LoadCode;
begin
  Code.Text :=
    'Dim x As Integer' + LineEnding +
    'Let x = 5' + LineEnding +
    'Print x' + LineEnding +
    '10:' + LineEnding +
    'Let x = 10' + LineEnding +
    'If x = 10 Then Goto 20' + LineEnding +
    'Print x' + LineEnding +
    'Goto 30' + LineEnding +
    '20:' + LineEnding +
    'Print x' + LineEnding +
    '30:' + LineEnding +
    'Print x' + LineEnding +
    'Sub MySub:' + LineEnding + // Added a sample subroutine for BuildSubs to find
    '  Print "Inside MySub"' + LineEnding +
    'End Sub';
end;

procedure InitInterpreter;
begin
  // Initialize all global lists and maps
  Code := TStringList.Create;
  Vars := TStringList.Create;
  Vars.Sorted := False; // Ensure it's not sorted if you intend to use IndexOf
  Labels := TStringList.Create;
  Labels.Sorted := False; // Ensure it's not sorted
  Subs := TSubroutineMap.Create; // Correctly create TSubroutineMap
  Stack := TObjectList.Create; // Assuming TStack is TObjectList or similar
  Variables := TStringList.Create;
end;

procedure FreeInterpreter;
var
  i: Integer;
begin
  // Free TLineNumberData records stored in Labels and Subs
  for i := 0 to Labels.Count - 1 do
    Dispose(PLineNumberData(Labels.Objects[i]));
  for i := 0 to Subs.Count - 1 do
    Dispose(PLineNumberData(Subs.Objects[i]));

  // Free the TStringList/TObjectList instances themselves
  Stack.Free;
  Code.Free;
  Labels.Free;
  Subs.Free;
  Variables.Free;
end;

end.
