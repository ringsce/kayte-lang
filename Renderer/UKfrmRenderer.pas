// UKfrmRenderer.pas
// Responsible for rendering KFrm definitions into actual LCL forms and controls.

unit UKfrmRenderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Buttons, // StdCtrls for TButton, TLabel, TEdit; ExtCtrls for TPanel, Buttons for TButton
  Contnrs, // For TObjectList
  UKfrmTypes in '../source/UKfrmTypes.pas', // For TKfrmFormDef, TKfrmControlDef, TKfrmPropertyDef
  UEventRouter; // For TEventRouter

type
  TKfrmRenderer = class(TObject)
  private
    // Helper function to apply properties to an LCL control based on TKfrmComponentDef data
    // Changed AControlDef type from TKfrmControlDef to TKfrmComponentDef as per UKfrmTypes hierarchy
    procedure ApplyControlProperties(AControl: TControl; AControlDef: TKfrmComponentDef);
    // Helper function to register events for an LCL control
    // Changed AControlDef type from TKfrmControlDef to TKfrmComponentDef
    procedure RegisterControlEvents(AControl: TControl; AControlDef: TKfrmComponentDef; AEventRouter: TEventRouter);
    // Recursive helper to create controls and their children
    // Changed AControlDef type from TKfrmControlDef to TKfrmComponentDef
    function CreateLCLControl(AControlDef: TKfrmComponentDef; AParent: TWinControl; AEventRouter: TEventRouter): TControl;
  public
    constructor Create;
    destructor Destroy; override;

    // Main function to create and populate an LCL form from a TKfrmFormDef
    // TKfrmFormDef is also defined in UKfrmTypes
    function CreateAndPopulateForm(AFormDef: TKfrmFormDef; AEventRouter: TEventRouter): TForm;
  end;

implementation

{ TKfrmRenderer }

constructor TKfrmRenderer.Create;
begin
  inherited Create;
end;

destructor TKfrmRenderer.Destroy;
begin
  inherited Destroy;
end;

// !!! FIX APPLIED HERE: Changed AControlDef type to TKfrmComponentDef !!!
procedure TKfrmRenderer.ApplyControlProperties(AControl: TControl; AControlDef: TKfrmComponentDef);
var
  IntValue: Integer;
begin
  if not Assigned(AControl) or not Assigned(AControlDef) then Exit;

  // Apply common properties from TKfrmComponentDef (base class)
  AControl.Name := AControlDef.Name; // Use Name from TKfrmComponentDef (inherited)
  AControl.Left := AControlDef.Left;
  AControl.Top := AControlDef.Top;
  AControl.Width := AControlDef.Width;
  AControl.Height := AControlDef.Height;
  AControl.Visible := AControlDef.Visible;
  AControl.Enabled := AControlDef.Enabled; // Apply Enabled property

  // Apply Caption if applicable (from TKfrmComponentDef)
  if (AControl is TButton) or (AControl is TLabel) or (AControl is TForm) then // Forms also have Caption
    AControl.Caption := AControlDef.Caption;

  // Apply type-specific properties based on the actual *runtime type* of AControlDef
  if AControlDef is TKfrmFormDef then
  begin
    if AControl is TForm then
    begin
      TForm(AControl).Position := TKfrmFormDef(AControlDef).Position;
    end;
  end
  else if AControlDef is TKfrmEditDef then
  begin
    if AControl is TEdit then
    begin
      TEdit(AControl).Text := TKfrmEditDef(AControlDef).Text;
      TEdit(AControl).ReadOnly := TKfrmEditDef(AControlDef).ReadOnly;
      TEdit(AControl).MaxLength := TKfrmEditDef(AControlDef).MaxLength;
      TEdit(AControl).PasswordChar := TKfrmEditDef(AControlDef).PasswordChar;
    end;
  end
  else if AControlDef is TKfrmLabelDef then
  begin
    if AControl is TLabel then
    begin
      // TAlignment is an enum, convert string to enum value
      // This is a simplified conversion. For production, use a robust string-to-enum function.
      case TKfrmLabelDef(AControlDef).Alignment.ToLower of
        'left': TLabel(AControl).Alignment := taLeftJustify;
        'right': TLabel(AControl).Alignment := taRightJustify;
        'center': TLabel(AControl).Alignment := taCenter;
        else
          WriteLn(SysUtils.Format('UKfrmRenderer: Warning: Unknown Alignment "%s" for label "%s".', [TKfrmLabelDef(AControlDef).Alignment, AControl.Name]));
      end;
      TLabel(AControl).AutoSize := TKfrmLabelDef(AControlDef).AutoSize;
      TLabel(AControl).WordWrap := TKfrmLabelDef(AControlDef).WordWrap;
    end;
  end
  // Add more control types and their specific properties here
end;

// !!! FIX APPLIED HERE: Changed AControlDef type to TKfrmComponentDef !!!
procedure TKfrmRenderer.RegisterControlEvents(AControl: TControl; AControlDef: TKfrmComponentDef; AEventRouter: TEventRouter);
var
  EventBinding: TEventBinding;
  I: Integer;
begin
  if not Assigned(AControl) or not Assigned(AControlDef) or not Assigned(AEventRouter) then Exit;

  // Iterate through all event bindings defined for this component
  for I := 0 to AControlDef.EventBindings.Count - 1 do
  begin
    EventBinding := TEventBinding(AControlDef.EventBindings.Items[I]);
    case EventBinding.EventName.ToLower of
      'onclick':
        // Only assign OnClick if the control actually has an OnClick event
        if AControl is TButton then
          // !!! FIX APPLIED HERE: Call RegisterOnClickHandler as a procedure, not as an assignment source !!!
          AEventRouter.RegisterOnClickHandler(AControl, EventBinding.FunctionName)
        else if (AControl is TWinControl) and (TWinControl(AControl).IsControl) then
          WriteLn(SysUtils.Format('UKfrmRenderer: Warning: OnClick specified for non-button TWinControl "%s" (Name: "%s"). Implement custom click handling for this type.', [AControl.ClassName, AControl.Name]))
        else
          WriteLn(SysUtils.Format('UKfrmRenderer: Warning: OnClick specified for unsupported control type "%s" (Name: "%s").', [AControl.ClassName, AControl.Name]));
      // Add more event types (e.g., 'onchange', 'onkeydown') here if your TEventBinding includes them
      else
        WriteLn(SysUtils.Format('UKfrmRenderer: Warning: Unknown event "%s" specified for control "%s".', [EventBinding.EventName, AControl.Name]));
    end;
  end;
end;


// !!! CHANGED AControlDef TYPE TO TKfrmComponentDef !!!
function TKfrmRenderer.CreateLCLControl(AControlDef: TKfrmComponentDef; AParent: TWinControl; AEventRouter: TEventRouter): TControl;
var
  LCLControl: TControl;
  ChildComponentDef: TKfrmComponentDef; // Use base type for children
  I: Integer;
begin
  LCLControl := nil;

  // Create the LCL control based on the specific type of AControlDef
  if AControlDef is TKfrmFormDef then
    LCLControl := TForm.Create(AParent)
  else if AControlDef is TKfrmButtonDef then
    LCLControl := TButton.Create(AParent)
  else if AControlDef is TKfrmLabelDef then
    LCLControl := TLabel.Create(AParent)
  else if AControlDef is TKfrmEditDef then
    LCLControl := TEdit.Create(AParent)
  // Add more control types here
  else
  begin
    WriteLn(SysUtils.Format('UKfrmRenderer: Error: Unknown or unsupported TKfrmComponentDef type "%s". Cannot create control.', [AControlDef.ClassName]));
    Exit(nil);
  end;

  // Set parent
  if Assigned(AParent) then
    LCLControl.Parent := AParent;

  // Apply properties
  ApplyControlProperties(LCLControl, AControlDef);

  // Register events
  RegisterControlEvents(LCLControl, AControlDef, AEventRouter);

  // Recursively create and populate child controls (if any)
  // Only TKfrmFormDef has a 'Controls' list for children.
  if AControlDef is TKfrmFormDef then
  begin
    for I := 0 to TKfrmFormDef(AControlDef).Controls.Count - 1 do
    begin
      ChildComponentDef := TKfrmComponentDef(TKfrmFormDef(AControlDef).Controls.Items[I]);
      CreateLCLControl(ChildComponentDef, TWinControl(LCLControl), AEventRouter);
    end;
  end;

  Result := LCLControl;
end;



function TKfrmRenderer.CreateAndPopulateForm(AFormDef: TKfrmFormDef; AEventRouter: TEventRouter): TForm;
var
  NewForm: TForm;
  ControlDef: TKfrmComponentDef; // Corrected type to TKfrmComponentDef
  I: Integer;
begin
  Result := nil;
  if not Assigned(AFormDef) then Exit;

  // Create the form instance
  NewForm := TForm.Create(Application); // Application is the owner for top-level forms
  NewForm.Name := AFormDef.Name; // Set the form's name early for debugging/lookup (used Name from TKfrmComponentDef)

  // Apply form properties (TKfrmFormDef inherits from TKfrmComponentDef, so this works)
  ApplyControlProperties(NewForm, AFormDef);

  // Register form events (if any)
  RegisterControlEvents(NewForm, AFormDef, AEventRouter);

  // Create and populate controls on the form
  if Assigned(AFormDef.Controls) then
  begin
    for I := 0 to AFormDef.Controls.Count - 1 do
    begin
      ControlDef := TKfrmComponentDef(AFormDef.Controls.Items[I]);
      CreateLCLControl(ControlDef, NewForm, AEventRouter); // Pass NewForm as parent
      // !!! REMOVED THE EXTRANEOUS "fungicide." HERE !!!
    end;
  end;

  Result := NewForm;
end;


end.

