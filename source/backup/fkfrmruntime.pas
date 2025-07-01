unit FKfrmRuntime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls,
  Contnrs,             // <-- for TObjectList
  UKfrmTypes,
  UKfrmParser,         // <-- real (or stub) parser
  UKfrmRenderer in '../Renderer/UKfrmRenderer.pas',       // <-- renderer unit already in project path (KEEP HERE)
  UEventRouter;

type
  TKfrmRuntime = class
  private
    FKfrmParser   : TKfrmParser;
    FKfrmRenderer : TKfrmRenderer;
    FEventRouter  : TEventRouter;
    FLoadedForms  : TObjectList;   // owns the TForm instances
  public
    constructor Create(AEventRouter: TEventRouter);
    destructor  Destroy; override;

    function  ShowKfrmForm(const AFilePath: String): TForm;
    procedure HideKfrmForm(const AFormName: String);
    procedure CloseKfrmForm(const AFormName: String);

    function  GetLoadedForm(const AFormName: String): TForm;
    function  GetControlFromForm(AForm: TForm; const AControlName: String): TControl;
  end;

implementation

uses
  UKfrmRenderer; // <<< ADDED UKfrmRenderer TO IMPLEMENTATION USES CLAUSE <<<

{ TKfrmRuntime }

constructor TKfrmRuntime.Create(AEventRouter: TEventRouter);
begin
  inherited Create;
  FKfrmParser   := TKfrmParser.Create;
  FKfrmRenderer := TKfrmRenderer.Create; // This line needs TKfrmRenderer to be known in implementation
  FEventRouter  := AEventRouter;
  FLoadedForms  := TObjectList.Create(True);  // owns objects
end;

destructor TKfrmRuntime.Destroy;
begin
  FLoadedForms.Free;
  FKfrmRenderer.Free;
  FKfrmParser.Free;
  inherited Destroy;
end;

function TKfrmRuntime.ShowKfrmForm(const AFilePath: String): TForm;
var
  FormDef : TKfrmFormDef;
  NewForm : TForm;
begin
  Result := nil;
  FormDef := FKfrmParser.ParseKfrmFile(AFilePath);
  try
    NewForm := FKfrmRenderer.CreateAndPopulateForm(FormDef, FEventRouter);
    FLoadedForms.Add(NewForm);
    NewForm.Show;
    Result := NewForm;
  finally
    FormDef.Free;
  end;
end;

procedure TKfrmRuntime.HideKfrmForm(const AFormName: String);
var
  I : Integer;
  F : TForm;
begin
  for I := 0 to FLoadedForms.Count-1 do
  begin
    F := TForm(FLoadedForms[I]);
    if SameText(F.Name, AFormName + 'Instance') then
    begin
      F.Hide;
      Exit;
    end;
  end;
end;

procedure TKfrmRuntime.CloseKfrmForm(const AFormName: String);
var
  I : Integer;
  F : TForm;
begin
  for I := 0 to FLoadedForms.Count-1 do
  begin
    F := TForm(FLoadedForms[I]);
    if SameText(F.Name, AFormName + 'Instance') then
    begin
      FLoadedForms.Delete(I);   // deletes & frees the form
      Exit;
    end;
  end;
end;

function TKfrmRuntime.GetLoadedForm(const AFormName: String): TForm;
var
  I : Integer;
  F : TForm;
begin
  Result := nil;
  for I := 0 to FLoadedForms.Count-1 do
  begin
    F := TForm(FLoadedForms[I]);
    if SameText(F.Name, AFormName + 'Instance') then
      Exit(F);
  end;
end;

function TKfrmRuntime.GetControlFromForm(AForm: TForm;
  const AControlName: String): TControl;
var
  I : Integer;
begin
  Result := nil;
  if AForm<>nil then
    for I := 0 to AForm.ControlCount-1 do
      if SameText(AForm.Controls[I].Name, AControlName) then
        Exit(AForm.Controls[I]);
end;

end.
