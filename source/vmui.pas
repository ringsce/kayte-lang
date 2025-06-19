// In VMUI.pas (or extend BytecodeTypes.pas)
unit VMUI;

interface

uses
  SysUtils, Classes, Forms, Controls, StdCtrls; // For TForm, TButton, TLabel etc.

type
  TVMControlType = (
    vctForm,
    vctButton,
    vctLabel,
    vctTextBox,
    // Add more as needed
    vctUnknown
  );

  TVMControlProperty = record
    PropName: String;
    PropValue: String; // Store as string, convert at runtime
  end;

  TVMControlProperties = array of TVMControlProperty;

  TVMControl = class
  public
    Name: String;
    ControlType: TVMControlType;
    Properties: TVMControlProperties; // Array of properties

    // Add methods for adding properties
    procedure AddProperty(const AName, AValue: String);
    // Add helper to find property
    function GetPropertyValue(const AName: String): String;
    constructor Create;
    destructor Destroy; override;
  end;

  // Represents a complete form definition
  TVMFormDefinition = class
  public
    Name: String; // Form name (e.g., "Form1")
    Controls: TObjectList; // List of TVMControl instances

    constructor Create(const AName: String);
    destructor Destroy; override;
  end;

implementation

{ TVMControl }

constructor TVMControl.Create;
begin
  inherited;
  SetLength(Properties, 0);
end;

destructor TVMControl.Destroy;
begin
  SetLength(Properties, 0); // Clear dynamic array
  inherited;
end;

procedure TVMControl.AddProperty(const AName, AValue: String);
var
  i: Integer;
begin
  i := Length(Properties);
  SetLength(Properties, i + 1);
  Properties[i].PropName := AName;
  Properties[i].PropValue := AValue;
end;

function TVMControl.GetPropertyValue(const AName: String): String;
var
  i: Integer;
begin
  for i := 0 to High(Properties) do
  begin
    if SameText(Properties[i].PropName, AName) then
    begin
      Result := Properties[i].PropValue;
      Exit;
    end;
  end;
  Result := ''; // Property not found
end;


{ TVMFormDefinition }

constructor TVMFormDefinition.Create(const AName: String);
begin
  inherited Create;
  Name := AName;
  Controls := TObjectList.Create(True); // Owns the TVMControl instances
end;

destructor TVMFormDefinition.Destroy;
begin
  FreeAndNil(Controls);
  inherited Destroy;
end;

end.
