unit UKfrmTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls;

type
  { A parsed control definition from a .kfrm file }
  TKfrmControlDef = class
  private
    FProperties: TStringList;
  public
    Name: String;
    ControlClassType: String; // LCL class name, e.g. 'TButton', 'TLabel', 'TEdit'

    Caption: String;      // TButton, TLabel
    Text: String;         // TEdit, TMemo
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    Visible: Boolean;

    PasswordChar: Char;   // TEdit only

    OnClickHandlerName: String; // Kayte function name to call on click

    constructor Create;
    destructor Destroy; override;

    procedure AddProperty(const AName, AValue: String);
    function GetProperty(const AName: String): String;
  end;

  { A parsed form definition from a .kfrm file }
  TKfrmFormDef = class
  private
    FControls: TObjectList;
  public
    Name: String;
    Caption: String;
    Width: Integer;
    Height: Integer;
    Position: TFormPosition;

    constructor Create;
    destructor Destroy; override;

    procedure AddControl(AControlDef: TKfrmControlDef);
    function GetControlByName(const AName: String): TKfrmControlDef;
    function Controls: TObjectList;
  end;

implementation

{ TKfrmControlDef }

constructor TKfrmControlDef.Create;
begin
  inherited Create;
  FProperties := TStringList.Create;
  Visible := True;
  PasswordChar := #0;
end;

destructor TKfrmControlDef.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

procedure TKfrmControlDef.AddProperty(const AName, AValue: String);
begin
  FProperties.Values[AName] := AValue;
end;

function TKfrmControlDef.GetProperty(const AName: String): String;
begin
  Result := FProperties.Values[AName]; // empty string if not found
end;

{ TKfrmFormDef }

constructor TKfrmFormDef.Create;
begin
  inherited Create;
  FControls := TObjectList.Create(True); // owns the control defs
end;

destructor TKfrmFormDef.Destroy;
begin
  FreeAndNil(FControls);
  inherited Destroy;
end;

procedure TKfrmFormDef.AddControl(AControlDef: TKfrmControlDef);
begin
  FControls.Add(AControlDef);
end;

function TKfrmFormDef.GetControlByName(const AName: String): TKfrmControlDef;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FControls.Count - 1 do
  begin
    if TKfrmControlDef(FControls[I]).Name = AName then
    begin
      Result := TKfrmControlDef(FControls[I]);
      Exit;
    end;
  end;
end;

function TKfrmFormDef.Controls: TObjectList;
begin
  Result := FControls;
end;

end.
