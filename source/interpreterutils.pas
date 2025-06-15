unit InterpreterUtils;

interface

uses
  Classes, Contnrs, SysUtils; // Added SysUtils for TFileStream and file operations,
                             // and for Pos, Length, Copy functions used in SplitString.

type
  TStringArray = array of String; // Common definition for string arrays

  PLineNumberData = ^TLineNumberData;
  TLineNumberData = record
    Line: Integer;
  end;

var
  i: Integer;
  Vars: TStringList;        // Stores variable names and their PVariable pointers
  Code: TStringList;        // Stores the lines of code from the script
  Labels: TStringList;      // Stores labels and their PLineNumberData
  Subs: TStringList;        // Stores subroutine names and their PLineNumberData
  Stack: TObjectList;       // Stores return addresses for CALL/ENDSUB (PInteger)
  FormLocations: TStringList; // Stores form names and their start line (like Labels/Subs)

// --- Procedures for parsing/building script metadata ---
procedure BuildLabels;
procedure BuildSubs;
procedure BuildForms; // New procedure to find form definitions

// --- Procedure to load code from a file ---
procedure LoadCodeFromFile(const FileName: String);

// --- Declaration for SplitString function ---
function SplitString(const S, Delimiters: String): TStringArray;

implementation

// --- Implementation of SplitString function ---
function SplitString(const S, Delimiters: String): TStringArray;
var
  CurrentPos, LastPos: Integer;
  LenS: Integer;
  TempList: TStringList;
  // --- MOVED THESE DECLARATIONS TO THE TOP OF THE FUNCTION ---
  FoundDelimiter: Boolean;
  DelimiterPos: Integer;
  P: Integer; // For Pos function result
  i: Integer; // For the loop variable
begin
  Result := nil; // Initialize result to empty array
  LenS := Length(S);
  if LenS = 0 then
    Exit; // Nothing to split

  TempList := TStringList.Create;
  try
    LastPos := 1;
    CurrentPos := 1;

    while CurrentPos <= LenS do
    begin
      // Find the next occurrence of any delimiter
      FoundDelimiter := False; // Now just assignment, not declaration
      DelimiterPos := MaxInt; // Initialize with a large value

      for i := 1 to Length(Delimiters) do // Now just assignment, not declaration
      begin
        P := Pos(Delimiters[i], S, CurrentPos); // Now just assignment, not declaration
        if (P > 0) and (P < DelimiterPos) then
        begin
          DelimiterPos := P;
          FoundDelimiter := True;
        end;
      end;

      if FoundDelimiter then
      begin
        TempList.Add(Trim(Copy(S, LastPos, DelimiterPos - LastPos)));
        CurrentPos := DelimiterPos + 1; // Move past the delimiter
        LastPos := CurrentPos;
      end
      else
      begin // No more delimiters found
        TempList.Add(Trim(Copy(S, LastPos, LenS - LastPos + 1)));
        CurrentPos := LenS + 1; // Exit loop
      end;
    end;

    SetLength(Result, TempList.Count);
    for CurrentPos := 0 to TempList.Count - 1 do
      Result[CurrentPos] := TempList[CurrentPos];
  finally
    TempList.Free;
  end;
end;


procedure BuildLabels;
var
  i: Integer;
  trimmedLine: String;
  parts: TStringArray;
  labelName: String;
  LineData: PLineNumberData;
begin
  Labels.Clear; // Clear existing labels

  for i := 0 to Code.Count - 1 do
  begin
    trimmedLine := Trim(Code[i]);
    // Check if the line ends with a colon, indicating a label
    if (Length(trimmedLine) > 0) and (trimmedLine[Length(trimmedLine)] = ':') then
    begin
      labelName := Copy(trimmedLine, 1, Length(trimmedLine) - 1); // Remove the colon
      labelName := LowerCase(Trim(labelName));

      if Labels.IndexOf(labelName) = -1 then // Only add if it's a new label
      begin
        New(LineData);
        LineData^.Line := i; // Store the 0-based line index
        Labels.AddObject(labelName, TObject(LineData));
      end
      else
      begin
        Writeln('Warning: Duplicate label found: ', labelName, ' at line ', i + 1);
      end;
    end;
  end;
end;

procedure BuildSubs;
var
  i: Integer;
  trimmedLine: String;
  parts: TStringArray;
  subName: String;
  LineData: PLineNumberData;
begin
  Subs.Clear; // Clear existing subroutines

  for i := 0 to Code.Count - 1 do
  begin
    trimmedLine := LowerCase(Trim(Code[i]));
    parts := SplitString(trimmedLine, ' '); // Now SplitString is defined here!

    if (Length(parts) >= 2) and (parts[0] = 'sub') then
    begin
      subName := parts[1];
      if Pos(':', subName) > 0 then // Remove trailing colon if present (e.g., "MySub:")
        subName := Copy(subName, 1, Length(subName) - 1);
      subName := Trim(subName); // Trim again just in case

      if Subs.IndexOf(subName) = -1 then // Only add if it's a new subroutine
      begin
        New(LineData);
        LineData^.Line := i; // Store the 0-based line index of the 'Sub' declaration
        Subs.AddObject(subName, TObject(LineData));
      end
      else
      begin
        Writeln('Warning: Duplicate subroutine found: ', subName, ' at line ', i + 1);
      end;
    end;
  end;
end;

procedure BuildForms;
var
  i: Integer;
  trimmedLine: String;
  parts: TStringArray;
  formName: String;
  LineData: PLineNumberData;
begin
  FormLocations.Clear;

  for i := 0 to Code.Count - 1 do
  begin
    trimmedLine := LowerCase(Trim(Code[i]));
    parts := SplitString(trimmedLine, ' '); // Now SplitString is defined here!

    if (Length(parts) >= 2) and (parts[0] = 'form') then
    begin
      formName := parts[1];
      formName := Trim(formName);

      if FormLocations.IndexOf(formName) = -1 then
      begin
        New(LineData);
        LineData^.Line := i; // Store the 0-based line index of the 'Form' declaration
        FormLocations.AddObject(formName, TObject(LineData));
      end
      else
      begin
        Writeln('Warning: Duplicate form found: ', formName, ' at line ', i + 1);
      end;
    end;
  end;
end;

procedure LoadCodeFromFile(const FileName: String);
begin
  if not FileExists(FileName) then
  begin
    raise Exception.CreateFmt('Error: File "%s" not found.', [FileName]);
  end;

  Code.Clear; // Clear any previously loaded code
  Code.LoadFromFile(FileName); // Load all lines from the specified file

  // After loading, rebuild labels and subroutines
  BuildLabels;
  BuildSubs;
  BuildForms; // Also build form locations
end;

initialization
  Vars := TStringList.Create;
  Code := TStringList.Create;
  Labels := TStringList.Create;
  Subs := TStringList.Create;
  Stack := TObjectList.Create;
  Stack.OwnsObjects := True; // Make TObjectList responsible for freeing objects it contains
  FormLocations := TStringList.Create; // Initialize FormLocations
finalization
  begin
    // Free any dynamically allocated LineNumberData objects in Labels and Subs
    for i := 0 to Labels.Count - 1 do
      Dispose(PLineNumberData(Labels.Objects[i]));
    Labels.Clear;

    for i := 0 to Subs.Count - 1 do
      Dispose(PLineNumberData(Subs.Objects[i]));
    Subs.Clear;

    for i := 0 to FormLocations.Count - 1 do // Free for FormLocations as well
      Dispose(PLineNumberData(FormLocations.Objects[i]));
    FormLocations.Clear;

    FreeAndNil(Vars);
    FreeAndNil(Code);
    FreeAndNil(Labels);
    FreeAndNil(Subs);
    FreeAndNil(Stack);
    FreeAndNil(FormLocations); // Free the TStringList itself
  end;
end.
