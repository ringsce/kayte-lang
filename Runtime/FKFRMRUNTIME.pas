// UKfrmRuntime.pas -- NOTE: This unit's *filename* is typically UKfrmRuntime.pas.
// The unit name 'FKFRMRUNTIME' below is used to satisfy a specific compiler error
// in your current project setup, indicating the compiler expects this name.
unit FKFRMRUNTIME; // <--- CHANGED TO FKFRMRUNTIME TO MATCH COMPILER'S EXPECTATION
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, // Add other LCL units as needed
  UKfrmTypes, // This unit name remains UKfrmTypes as it's a different file
  UKfrmParser in '../Parser/UKFrmParser.pas',
  UKfrmRenderer in '../Renderer/UKfrmRenderer.pas',
  UEventRouter in '../VM/UEventRouter.pas'; // Corrected path to UEventRouter based on typical structure

type
  // The class name can remain TKfrmRuntime, as it's separate from the unit name.
  TKfrmRuntime = class
  private
    FKfrmParser: TKfrmParser;
    FKfrmRenderer: TKfrmRenderer;
    FEventRouter: TEventRouter; // Reference to your centralized event router
    FLoadedForms: TObjectList; // Stores instances of dynamically created TForm (interpreted forms)

  public
    constructor Create(AEventRouter: TEventRouter);
    destructor Destroy; override;

    function ShowKfrmForm(const AFilePath: String): TForm;
    procedure HideKfrmForm(const AFormName: String);
    procedure CloseKfrmForm(const AFormName: String);
    function GetLoadedForm(const AFormName: String): TForm; // Get a loaded form by its original name
    function GetControlFromForm(AForm: TForm; const AControlName: String): TControl;
  end;

implementation

{ TKfrmRuntime }

constructor TKfrmRuntime.Create(AEventRouter: TEventRouter);
begin
  inherited Create;
  FKfrmParser := TKfrmParser.Create;
  FKfrmRenderer := TKfrmRenderer.Create;
  FEventRouter := AEventRouter; // Assign the passed event router
  FLoadedForms := TObjectList.Create(True); // TObjectList will own and free the TForm objects
end;

destructor TKfrmRuntime.Destroy;
begin
  FreeAndNil(FKfrmParser);
  FreeAndNil(FKfrmRenderer);
  FreeAndNil(FLoadedForms);
  FEventRouter := nil; // Just clear the reference
  inherited Destroy;
end;

function TKfrmRuntime.ShowKfrmForm(const AFilePath: String): TForm;
var
  FormDef: TKfrmFormDef;
  NewForm: TForm;
begin
  Result := nil;
  try
    WriteLn(SysUtils.Format('KfrmRuntime: Parsing and showing .kfrm: %s', [AFilePath]));
    FormDef := FKfrmParser.ParseKfrmFile(AFilePath);
    try
      NewForm := FKfrmRenderer.CreateAndPopulateForm(FormDef, FEventRouter);
      FLoadedForms.Add(NewForm); // Add to our tracking list
      NewForm.Show; // Display the actual LCL form
      Result := NewForm;
    finally
      FreeAndNil(FormDef); // Free the definition object once rendered
    end;
  except
    on E: Exception do
    begin
      WriteLn(SysUtils.Format('Error loading/running .kfrm %s: %s', [AFilePath, E.Message]));
      // You might want to re-raise the exception or handle it more gracefully
    end;
  end;
end;

procedure TKfrmRuntime.HideKfrmForm(const AFormName: String);
var
  I: Integer;
  FormInstance: TForm;
begin
  for I := 0 to FLoadedForms.Count - 1 do
  begin
    FormInstance := TForm(FLoadedForms[I]);
    // Remember that NewForm.Name was set to AFormDef.Name + 'Instance' in renderer
    if SameText(FormInstance.Name, AFormName + 'Instance') then
    begin
      FormInstance.Hide;
      Exit;
    end;
  end;
end;

procedure TKfrmRuntime.CloseKfrmForm(const AFormName: String);
var
  I: Integer;
  FormInstance: TForm;
begin
  for I := 0 to FLoadedForms.Count - 1 do
  begin
    FormInstance := TForm(FLoadedForms[I]);
    if SameText(FormInstance.Name, AFormName + 'Instance') then
    begin
      FLoadedForms.Delete(I); // TObjectList will free FormInstance because it owns objects
      Exit;
    end;
  end;
end;

function TKfrmRuntime.GetLoadedForm(const AFormName: String): TForm;
var
  I: Integer;
  FormInstance: TForm;
begin
  Result := nil;
  for I := 0 to FLoadedForms.Count - 1 do
  begin
    FormInstance := TForm(FLoadedForms[I]);
    if SameText(FormInstance.Name, AFormName + 'Instance') then
    begin
      Result := FormInstance;
      Exit;
    end;
  end;
end;

function TKfrmRuntime.GetControlFromForm(AForm: TForm; const AControlName: String): TControl;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(AForm) then
  begin
    for I := 0 to AForm.ControlCount - 1 do
    begin
      if SameText(AForm.Controls[I].Name, AControlName) then
      begin
        Result := AForm.Controls[I];
        Exit;
      end;
    end;
  end;
end;

end.
