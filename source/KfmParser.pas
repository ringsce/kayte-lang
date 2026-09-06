unit KfmParser;

{$mode objfpc}{$H+}

// Reader/writer for the .kfm form-definition format (formerly .kfrm):
//
//   [FORM:Form1]
//   [CONTROL:Form1:vctForm]
//     Caption="My First Form"
//     Width=400
//   [CONTROL:Button1:vctButton]
//     Caption="Click Me!"
//     Left=50
//
// A "[FORM:Name]" line names the form. Each "[CONTROL:Name:Type]" line
// starts a control (Type is a TVMControlType enum name, e.g. "vctButton");
// indented "Key=Value" lines that follow set that control's properties,
// verbatim (quotes, if any, are kept as part of the stored value - see
// TVMControl.AddProperty). Blank lines and lines starting with "//" or
// ";" are ignored.
//
// This is the format KfmLibGen.pas compiles into a shared/static library.

interface

uses
  SysUtils, Classes, VMUI;

// Parses .kfm source text (already loaded into memory) into a form
// definition. Caller owns the returned TVMFormDefinition.
function ParseKfmText(const Source: TStrings): TVMFormDefinition;

// Convenience wrapper: reads FileName from disk and parses it.
function LoadKfmFile(const FileName: String): TVMFormDefinition;

// Serializes a form definition back to .kfm text, in the same format
// ParseKfmText/LoadKfmFile read.
procedure SaveKfmFile(FormDef: TVMFormDefinition; const FileName: String);

implementation

// Position of Needle in S, starting the search at index StartPos (1-based).
function PosFrom(const Needle, S: String; StartPos: Integer): Integer;
var
  Found: Integer;
begin
  Found := Pos(Needle, Copy(S, StartPos, Length(S) - StartPos + 1));
  if Found = 0 then
    Result := 0
  else
    Result := Found + StartPos - 1;
end;

function StrToControlType(const S: String): TVMControlType;
begin
  if SameText(S, 'vctForm') then Result := vctForm
  else if SameText(S, 'vctButton') then Result := vctButton
  else if SameText(S, 'vctLabel') then Result := vctLabel
  else if SameText(S, 'vctTextBox') then Result := vctTextBox
  else Result := vctUnknown;
end;

function ControlTypeToStr(T: TVMControlType): String;
begin
  case T of
    vctForm: Result := 'vctForm';
    vctButton: Result := 'vctButton';
    vctLabel: Result := 'vctLabel';
    vctTextBox: Result := 'vctTextBox';
  else
    Result := 'vctUnknown';
  end;
end;

function ParseKfmText(const Source: TStrings): TVMFormDefinition;
var
  I, EqPos, ColonPos1, ColonPos2: Integer;
  Line, Trimmed, Header, ControlName, ControlTypeStr, Key, Value: String;
  CurrentControl: TVMControl;
  FormName: String;
begin
  FormName := '';
  CurrentControl := nil;
  Result := nil;

  for I := 0 to Source.Count - 1 do
  begin
    Line := Source[I];
    Trimmed := Trim(Line);

    if (Trimmed = '') or (Copy(Trimmed, 1, 2) = '//') or (Copy(Trimmed, 1, 1) = ';') then
      Continue;

    if (Trimmed[1] = '[') and (Trimmed[Length(Trimmed)] = ']') then
    begin
      Header := Copy(Trimmed, 2, Length(Trimmed) - 2);
      ColonPos1 := Pos(':', Header);

      if SameText(Copy(Header, 1, ColonPos1 - 1), 'FORM') then
      begin
        FormName := Copy(Header, ColonPos1 + 1, Length(Header));
        if Result = nil then
          Result := TVMFormDefinition.Create(FormName)
        else
          Result.Name := FormName;
      end
      else if SameText(Copy(Header, 1, ColonPos1 - 1), 'CONTROL') then
      begin
        ColonPos2 := PosFrom(':', Header, ColonPos1 + 1);
        if ColonPos2 = 0 then
          raise Exception.CreateFmt('Malformed .kfm CONTROL header (expected "[CONTROL:Name:Type]"): %s', [Line]);

        ControlName := Copy(Header, ColonPos1 + 1, ColonPos2 - ColonPos1 - 1);
        ControlTypeStr := Copy(Header, ColonPos2 + 1, Length(Header));

        if Result = nil then
          Result := TVMFormDefinition.Create(''); // CONTROL seen before FORM - tolerate it

        CurrentControl := TVMControl.Create;
        CurrentControl.Name := ControlName;
        CurrentControl.ControlType := StrToControlType(ControlTypeStr);
        Result.Controls.Add(CurrentControl);
      end
      else
        raise Exception.CreateFmt('Unknown .kfm section header: %s', [Line]);

      Continue;
    end;

    // Otherwise: an indented "Key=Value" property line for the current control.
    EqPos := Pos('=', Trimmed);
    if EqPos = 0 then
      raise Exception.CreateFmt('Expected "Key=Value" in .kfm file, found: %s', [Line]);
    if CurrentControl = nil then
      raise Exception.CreateFmt('Property line before any [CONTROL:...] section: %s', [Line]);

    Key := Trim(Copy(Trimmed, 1, EqPos - 1));
    Value := Copy(Trimmed, EqPos + 1, Length(Trimmed));
    CurrentControl.AddProperty(Key, Value);
  end;

  if Result = nil then
    raise Exception.Create('.kfm file contained no [FORM:...] or [CONTROL:...] sections');
end;

function LoadKfmFile(const FileName: String): TVMFormDefinition;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName);
    Result := ParseKfmText(SL);
  finally
    SL.Free;
  end;
end;

procedure SaveKfmFile(FormDef: TVMFormDefinition; const FileName: String);
var
  SL: TStringList;
  I, J: Integer;
  Control: TVMControl;
begin
  SL := TStringList.Create;
  try
    SL.Add(Format('[FORM:%s]', [FormDef.Name]));

    for I := 0 to FormDef.Controls.Count - 1 do
    begin
      Control := TVMControl(FormDef.Controls[I]);
      SL.Add(Format('[CONTROL:%s:%s]', [Control.Name, ControlTypeToStr(Control.ControlType)]));
      for J := 0 to High(Control.Properties) do
        SL.Add(Format('  %s=%s', [Control.Properties[J].PropName, Control.Properties[J].PropValue]));
    end;

    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;

end.
