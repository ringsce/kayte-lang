unit UControlEventHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Forms; // Needed for TControl, TNotifyEvent

type
  TEventRouter = class forward; // Forward declaration for circular reference

  { TControlEventHandler: A helper class to hold control and function name,
    used to route generic LCL events. Each instance will manage one control's event mapping. }
  TControlEventHandler = class
  private
    FControl: TControl;
    FInterpretedFunctionName: String;
    FEventRouter: TEventRouter; // Reference back to the main router instance
  public
    constructor Create(AControl: TControl; const AInterpretedFunctionName: String; AEventRouter: TEventRouter);
    destructor Destroy; override;

    // This is the actual method pointer that will be assigned to LCL events (e.g., OnClick, OnChange)
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
  // Do NOT free FControl here, it's owned by the form
  FControl := nil;
  FEventRouter := nil;
  inherited Destroy;
end;

procedure TControlEventHandler.HandleEvent(Sender: TObject);
begin
  // When an LCL event fires, this method is called.
  // It then asks the main EventRouter to dispatch it to the VM.
  if Assigned(FEventRouter) then
    FEventRouter.GenericControlEventHandler(Sender);
end;

end.
