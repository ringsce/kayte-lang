unit UKfrmTypes;

{$mode objfpc}{$H+} // Enable Object Pascal syntax extensions and wider string types

interface

uses
  Classes, SysUtils, Forms, Controls; // Forms and Controls for TFormPosition, TControl

type
  { TKfrmControlDef: Represents a parsed control definition from a .kfrm file }
  TKfrmControlDef = class
  private
    FProperties: TStringList; // To store arbitrary key-value properties
  public
    Name: String;          // The unique name of the control (e.g., "ButtonLogin")
    ControlClassType: String; // The LCL class name (e.g., 'TButton', 'TLabel', 'TEdit')

    // Common control properties
    Caption: String;      // For TButton, TLabel
    Text: String;         // For TEdit, TMemo
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    Visible: Boolean;

    // Control-specific properties (add more as you support them)
    PasswordChar: Char;   // Specific to TEdit

    // Event handlers mapped to interpreted Kayte functions
    OnClickHandlerName: String; // Name of the Kayte function to call on click event

    constructor Create;
    destructor Destroy; override;

    procedure AddProperty(const AName, AValue: String);
    function GetProperty(const AName: String): String;
  end;

  { TKfrmFormDef: Represents a parsed form definition from a .kfrm file }
  TKfrmFormDef = class
  private
    FControls: TObjectList; // TObjectList<TKfrmControlDef> to manage control definitions
  public
    Name: String;
    Caption: String;
    Width: Integer;
    Height: Integer;
    Position: TFormPosition; // LCL Form position enum (e.g., poScreenCenter)

    constructor Create;
    destructor Destroy; override;

    procedure AddControl(AControlDef: TKfrmControlDef);
    function GetControlByName(const AName: String): TKfrmControlDef;
    function Controls: TObjectList; // Expose the collection of control definitions
  end;

implementation

{ TKfrmControlDef }

constructor TKfrmControlDef.Create;
begin
  inherited Create;
  FProperties := TStringList.Create;
  Visible := True; // Default to visible
  PasswordChar := #0; // Default for no password char
end;

destructor TKfrmControlDef.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

procedure TKfrmControlDef.AddProperty(const AName, AValue: String);
begin
  // TStringList.Values[] allows adding or updating properties by name
  FProperties.Values[AName] := AValue;
end;

function TKfrmControlDef.GetProperty(const AName: String): String;
begin
  Result := FProperties.Values[AName]; // Returns empty string if not found
end;

{ TKfrmFormDef }

constructor TKfrmFormDef.Create;
begin
  inherited Create;
  // TObjectList with ownership (True) will automatically free TKfrmControlDef objects
  FControls := TObjectList.Create(True);
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
