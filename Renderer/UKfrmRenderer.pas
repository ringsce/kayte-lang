// Renders KFrm definitions into actual LCL forms and controls.

unit UKfrmRenderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Buttons,
  Contnrs,
  UKfrmTypes in '../source/UKfrmTypes.pas',
  UEventRouter;

type
  TKfrmRenderer = class(TObject)
  private
    procedure ApplyControlProperties(AControl: TControl; AControlDef: TKfrmComponentDef);
    procedure RegisterControlEvents(AControl: TControl; AControlDef: TKfrmComponentDef; AEventRouter: TEventRouter);
    // Recursively creates a control and its children
    function CreateLCLControl(AControlDef: TKfrmComponentDef; AParent: TWinControl; AEventRouter: TEventRouter): TControl;
  public
    constructor Create;
    destructor Destroy; override;

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

procedure TKfrmRenderer.ApplyControlProperties(AControl: TControl; AControlDef: TKfrmComponentDef);
var
  IntValue: Integer;
begin
  if not Assigned(AControl) or not Assigned(AControlDef) then Exit;

  AControl.Name := AControlDef.Name;
  AControl.Left := AControlDef.Left;
  AControl.Top := AControlDef.Top;
  AControl.Width := AControlDef.Width;
  AControl.Height := AControlDef.Height;
  AControl.Visible := AControlDef.Visible;
  AControl.Enabled := AControlDef.Enabled;

  if (AControl is TButton) or (AControl is TLabel) or (AControl is TForm) then
    AControl.Caption := AControlDef.Caption;

  // dispatch on the runtime type of AControlDef for type-specific properties
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
      // simplified string-to-enum conversion; expand if more alignments are ever needed
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
end;

procedure TKfrmRenderer.RegisterControlEvents(AControl: TControl; AControlDef: TKfrmComponentDef; AEventRouter: TEventRouter);
var
  EventBinding: TEventBinding;
  I: Integer;
begin
  if not Assigned(AControl) or not Assigned(AControlDef) or not Assigned(AEventRouter) then Exit;

  for I := 0 to AControlDef.EventBindings.Count - 1 do
  begin
    EventBinding := TEventBinding(AControlDef.EventBindings.Items[I]);
    case EventBinding.EventName.ToLower of
      'onclick':
        if AControl is TButton then
          AEventRouter.RegisterOnClickHandler(AControl, EventBinding.FunctionName)
        else if (AControl is TWinControl) and (TWinControl(AControl).IsControl) then
          WriteLn(SysUtils.Format('UKfrmRenderer: Warning: OnClick specified for non-button TWinControl "%s" (Name: "%s"). Implement custom click handling for this type.', [AControl.ClassName, AControl.Name]))
        else
          WriteLn(SysUtils.Format('UKfrmRenderer: Warning: OnClick specified for unsupported control type "%s" (Name: "%s").', [AControl.ClassName, AControl.Name]));
      else
        WriteLn(SysUtils.Format('UKfrmRenderer: Warning: Unknown event "%s" specified for control "%s".', [EventBinding.EventName, AControl.Name]));
    end;
  end;
end;

function TKfrmRenderer.CreateLCLControl(AControlDef: TKfrmComponentDef; AParent: TWinControl; AEventRouter: TEventRouter): TControl;
var
  LCLControl: TControl;
  ChildComponentDef: TKfrmComponentDef;
  I: Integer;
begin
  LCLControl := nil;

  if AControlDef is TKfrmFormDef then
    LCLControl := TForm.Create(AParent)
  else if AControlDef is TKfrmButtonDef then
    LCLControl := TButton.Create(AParent)
  else if AControlDef is TKfrmLabelDef then
    LCLControl := TLabel.Create(AParent)
  else if AControlDef is TKfrmEditDef then
    LCLControl := TEdit.Create(AParent)
  else
  begin
    WriteLn(SysUtils.Format('UKfrmRenderer: Error: Unknown or unsupported TKfrmComponentDef type "%s". Cannot create control.', [AControlDef.ClassName]));
    Exit(nil);
  end;

  if Assigned(AParent) then
    LCLControl.Parent := AParent;

  ApplyControlProperties(LCLControl, AControlDef);
  RegisterControlEvents(LCLControl, AControlDef, AEventRouter);

  // only forms carry a Controls list of children
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
  ControlDef: TKfrmComponentDef;
  I: Integer;
begin
  Result := nil;
  if not Assigned(AFormDef) then Exit;

  NewForm := TForm.Create(Application);
  NewForm.Name := AFormDef.Name;

  ApplyControlProperties(NewForm, AFormDef);
  RegisterControlEvents(NewForm, AFormDef, AEventRouter);

  if Assigned(AFormDef.Controls) then
  begin
    for I := 0 to AFormDef.Controls.Count - 1 do
    begin
      ControlDef := TKfrmComponentDef(AFormDef.Controls.Items[I]);
      CreateLCLControl(ControlDef, NewForm, AEventRouter);
    end;
  end;

  Result := NewForm;
end;

end.

