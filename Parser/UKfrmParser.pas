unit UKfrmParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  UKfrmTypes;

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
  Result := nil;

  if not FileExists(AFilePath) then
  begin
    WriteLn(SysUtils.Format('Error: Kfrm file not found: %s', [AFilePath]));
    Raise EFileNotFound.CreateFmt('Kfrm file not found: %s', [AFilePath]);
  end;

  IniFile := TMemIniFile.Create(AFilePath);
  try
    FormDef := TKfrmFormDef.Create;
    try
      FormDef.Name := IniFile.ReadString('Form', 'Name', '');
      FormDef.Caption := IniFile.ReadString('Form', 'Caption', '');
      FormDef.Width := IniFile.ReadInteger('Form', 'Width', 0);
      FormDef.Height := IniFile.ReadInteger('Form', 'Height', 0);
      // stored as a string in the ini, so read as string and convert to the enum
      FormDef.Position := TFormPosition(GetEnumValue(TypeInfo(TFormPosition), IniFile.ReadString('Form', 'Position', 'poDesigned')));

      SectionNames := TStringList.Create;
      try
        IniFile.ReadSections(SectionNames);
        for I := 0 to SectionNames.Count - 1 do
        begin
          if StartsText('Controls.', SectionNames[I]) then
          begin
            ControlDef := TKfrmControlDef.Create;
            try
              ControlDef.Name := Copy(SectionNames[I], Length('Controls.') + 1, MaxInt);
              ControlDef.ControlClassType := IniFile.ReadString(SectionNames[I], 'Type', '');
              ControlDef.Caption := IniFile.ReadString(SectionNames[I], 'Caption', '');
              ControlDef.Text := IniFile.ReadString(SectionNames[I], 'Text', '');
              ControlDef.Left := IniFile.ReadInteger(SectionNames[I], 'Left', 0);
              ControlDef.Top := IniFile.ReadInteger(SectionNames[I], 'Top', 0);
              ControlDef.Width := IniFile.ReadInteger(SectionNames[I], 'Width', 0);
              ControlDef.Height := IniFile.ReadInteger(SectionNames[I], 'Height', 0);
              ControlDef.Visible := IniFile.ReadBool(SectionNames[I], 'Visible', True);
              ControlDef.OnClickHandlerName := IniFile.ReadString(SectionNames[I], 'OnClick', '');

              Value := IniFile.ReadString(SectionNames[I], 'PasswordChar', '');
              if Length(Value) > 0 then
                ControlDef.PasswordChar := Value[1];

              // anything else in the section becomes a generic property
              var PropList: TStringList;
              PropList := TStringList.Create;
              try
                IniFile.ReadSection(SectionNames[I], PropList);
                for var J := 0 to PropList.Count - 1 do
                begin
                  PropName := PropList.Names[J];
                  Value := PropList.Values[PropName];
                  // skip properties already handled above
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
              end;
            end;
          end;
        end;
      finally
        FreeAndNil(SectionNames);
      end;

      Result := FormDef;
    except
      on E: Exception do
      begin
        FreeAndNil(FormDef);
        WriteLn(SysUtils.Format('Error parsing form file %s: %s', [AFilePath, E.Message]));
        Raise;
      end;
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

end.
