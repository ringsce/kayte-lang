unit UKfrmRenderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Contnrs,  StdCtrls, ExtCtrls, Buttons,
  UKfrmTypes, UEventRouter;

type
  TKfrmRenderer = class
  public
    function CreateAndPopulateForm(AFormDef: TKfrmFormDef;
                                   AEventRouter: TEventRouter): TForm;
  end;
  { -- one control on the form ------------------------------------------------ }
  TKfrmControlDef = class
  public
    Name               : String;
    ControlClassType   : String;   // “TButton”, “TEdit”, …
    Left, Top          : Integer;
    Width, Height      : Integer;
    Visible            : Boolean;

    // type‑specific extras
    Caption            : String;   // for TLabel / TButton
    Text               : String;   // for TEdit, TMemo …
    PasswordChar       : Char;
    OnClickHandlerName : String;

    constructor Create;
  end;

  { -- whole form description ------------------------------------------------ }
  TKfrmFormDef = class
  public
    Name      : String;
    Caption   : String;
    Width     : Integer;
    Height    : Integer;
    Position  : TPosition;

    Controls  : TObjectList;      // list of TKfrmControlDef

    constructor Create;
    destructor  Destroy; override;
  end;

implementation

{ TKfrmControlDef }

constructor TKfrmControlDef.Create;
begin
  inherited Create;
  Visible       := True;
  PasswordChar  := #0;
end;

{ TKfrmFormDef }

constructor TKfrmFormDef.Create;
begin
  inherited Create;
  Controls := TObjectList.Create(True);  // own the control objects
end;

destructor TKfrmFormDef.Destroy;
begin
  Controls.Free;
  inherited Destroy;
end;

end.

