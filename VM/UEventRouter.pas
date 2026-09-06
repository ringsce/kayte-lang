unit UEventRouter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms,
  UVM,
  UControlEventHandler;

type
  { Centralized event dispatcher for interpreted forms }
  TEventRouter = class
  private
    FVM: TVM;
    FControlHandlers: TObjectList; // owns the TControlEventHandler instances

  public
    constructor Create(AVM: TVM);
    destructor Destroy; override;

    // Called by TControlEventHandler.HandleEvent; finds the matching handler and dispatches to the VM
    procedure GenericControlEventHandler(Sender: TObject);

    // Bridges an LCL event property to an interpreted function via a TControlEventHandler
    function CreateEventHandler(AControl: TControl; const AInterpretedFunctionName: String): TNotifyEvent;
  end;

implementation

{ TEventRouter }

constructor TEventRouter.Create(AVM: TVM);
begin
  inherited Create;
  FVM := AVM;
  FControlHandlers := TObjectList.Create(True); // owns its items, so Destroy frees them too
end;

destructor TEventRouter.Destroy;
begin
  FreeAndNil(FControlHandlers);
  FVM := nil; // VM is owned by the main form, not us
  inherited Destroy;
end;

procedure TEventRouter.GenericControlEventHandler(Sender: TObject);
var
  Handler: TControlEventHandler;
  Found: Boolean;
  I: Integer;
begin
  Found := False;
  for I := 0 to FControlHandlers.Count - 1 do
  begin
    Handler := TControlEventHandler(FControlHandlers[I]);
    if Handler.FControl = Sender then
    begin
      Found := True;
      if Assigned(FVM) then
      begin
        WriteLn(SysUtils.Format('EventRouter: Event from "%s". Calling VM function: "%s"', [Sender.Name, Handler.FInterpretedFunctionName]));
        FVM.ExecuteInterpretedFunction(Handler.FInterpretedFunctionName);
      end
      else
      begin
        WriteLn('Error: VM not assigned to EventRouter or is nil!');
      end;
      Break;
    end;
  end;

  if not Found then
  begin
    WriteLn(SysUtils.Format('Warning: No interpreted handler found for control: %s', [Sender.Name]));
  end;
end;

function TEventRouter.CreateEventHandler(AControl: TControl; const AInterpretedFunctionName: String): TNotifyEvent;
var
  Handler: TControlEventHandler;
begin
  Handler := TControlEventHandler.Create(AControl, AInterpretedFunctionName, Self);
  FControlHandlers.Add(Handler); // keep it alive for as long as the router lives

  Result := Handler.HandleEvent;
end;

end.
