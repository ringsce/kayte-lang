unit UControlEventHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Forms;

type
  TEventRouter = class forward; // breaks the circular reference below

  { Holds a control's interpreted function name and routes generic LCL
    events to it. One instance per control/event mapping. }
  TControlEventHandler = class
  private
    FControl: TControl;
    FInterpretedFunctionName: String;
    FEventRouter: TEventRouter;
  public
    constructor Create(AControl: TControl; const AInterpretedFunctionName: String; AEventRouter: TEventRouter);
    destructor Destroy; override;

    // Assigned directly to LCL events (OnClick, OnChange, etc.)
    procedure HandleEvent(Sender: TObject);
  end;

implementation

{ TControlEventHandler }

constructor TControlEventHandler.Create(AControl: TControl; const AInterpretedFunctionName: String; AEventRouter: TEventRouter);
begin
  inherited Create;
  FControl := AControl;
  FInterpretedFunctionName := AInterpretedFunctionName;
  FEventRouter := AEventRouter;
end;

destructor TControlEventHandler.Destroy;
begin
  // FControl is owned by the form, not us — don't free it
  FControl := nil;
  FEventRouter := nil;
  inherited Destroy;
end;

procedure TControlEventHandler.HandleEvent(Sender: TObject);
begin
  // Hand off to the router, which dispatches into the VM
  if Assigned(FEventRouter) then
    FEventRouter.GenericControlEventHandler(Sender);
end;

end.
