unit UKfrmParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, // IniFiles for TMemIniFile
  UKfrmTypes; // For TKfrmFormDef, TKfrmControlDef

type
  TKfrmParser = class
  public
    function ParseKfrmFile(const AFilePath: String): TKfrmFormDef;
  end;

implementation

{ TKfrmParser }

function TKfrmParser.ParseKfrmFile(const AFilePath: String): TKfrmFormDef;
var
  IniFile: TMemIniFile;
  FormDef: TKfrmFormDef;
  SectionNames: TStringList;
  I: Integer;
  ControlDef: TKfrmControlDef;
  PropName: String;
  Value: String;
begin
  Result := nil; // Default to nil if parsing fails

  if not FileExists(AFilePath) then
  begin
    // You might have a global logging mechanism here instead of WriteLn
    WriteLn(SysUtils.Format('Error: Kfrm file not found: %s', [AFilePath]));
    Raise EFileNotFound.CreateFmt('Kfrm file not found: %s', [AFilePath]);
  end;

  IniFile := TMemIniFile.Create(AFilePath);
  try
    FormDef := TKfrmFormDef.Create;
    try
      // --- Parse [Form] section ---
      FormDef.Name := IniFile.ReadString('Form', 'Name', '');
      FormDef.Caption := IniFile.ReadString('Form', 'Caption', '');
      FormDef.Width := IniFile.ReadInteger('Form', 'Width', 0);
      FormDef.Height := IniFile.ReadInteger('Form', 'Height', 0);
      // ReadString for enum, then convert. This is safer than ReadInteger directly
      // as INI files typically store strings.
      FormDef.Position := TFormPosition(GetEnumValue(TypeInfo(TFormPosition), IniFile.ReadString('Form', 'Position', 'poDesigned')));

      // --- Parse [Controls.Name] sections ---
      SectionNames := TStringList.Create;
      try
        IniFile.ReadSections(SectionNames);
        for I := 0 to SectionNames.Count - 1 do
        begin
          if StartsText('Controls.', SectionNames[I]) then
          begin
            ControlDef := TKfrmControlDef.Create;
            try
              ControlDef.Name := Copy(SectionNames[I], Length('Controls.') + 1, MaxInt); // Extract control name
              ControlDef.ControlClassType := IniFile.ReadString(SectionNames[I], 'Type', '');
              ControlDef.Caption := IniFile.ReadString(SectionNames[I], 'Caption', '');
              ControlDef.Text := IniFile.ReadString(SectionNames[I], 'Text', '');
              ControlDef.Left := IniFile.ReadInteger(SectionNames[I], 'Left', 0);
              ControlDef.Top := IniFile.ReadInteger(SectionNames[I], 'Top', 0);
              ControlDef.Width := IniFile.ReadInteger(SectionNames[I], 'Width', 0);
              ControlDef.Height := IniFile.ReadInteger(SectionNames[I], 'Height', 0);
              ControlDef.Visible := IniFile.ReadBool(SectionNames[I], 'Visible', True);
              ControlDef.OnClickHandlerName := IniFile.ReadString(SectionNames[I], 'OnClick', '');

              // Specific property for TEdit
              Value := IniFile.ReadString(SectionNames[I], 'PasswordChar', '');
              if Length(Value) > 0 then
                ControlDef.PasswordChar := Value[1];

              // Read any other properties in the section as generic properties
              // Iterate through keys in the section and add them
              var PropList: TStringList;
              PropList := TStringList.Create;
              try
                IniFile.ReadSection(SectionNames[I], PropList);
                for var J := 0 to PropList.Count - 1 do
                begin
                  PropName := PropList.Names[J];
                  Value := PropList.Values[PropName];
                  // Avoid re-adding properties already handled specifically
                  if not (SameText(PropName, 'Type') or
                          SameText(PropName, 'Caption') or
                          SameText(PropName, 'Text') or
                          SameText(PropName, 'Left') or
                          SameText(PropName, 'Top') or
                          SameText(PropName, 'Width') or
                          SameText(PropName, 'Height') or
                          SameText(PropName, 'Visible') or
                          SameText(PropName, 'OnClick') or
                          SameText(PropName, 'PasswordChar')) then
                  begin
                    ControlDef.AddProperty(PropName, Value);
                  end;
                end;
              finally
                FreeAndNil(PropList);
              end;

              FormDef.AddControl(ControlDef);
            except
              on E: Exception do
              begin
                FreeAndNil(ControlDef);
                WriteLn(SysUtils.Format('Error parsing control %s in %s: %s', [SectionNames[I], AFilePath, E.Message]));
                // You might choose to re-raise or log and continue
              end;
            end;
          end;
        end;
      finally
        FreeAndNil(SectionNames);
      end;

      Result := FormDef; // Return the successfully parsed form definition
    except
      on E: Exception do
      begin
        FreeAndNil(FormDef); // Free partial object on error
        WriteLn(SysUtils.Format('Error parsing form file %s: %s', [AFilePath, E.Message]));
        Raise; // Re-raise the exception
      end;
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

end.
