unit UEventRouter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, // For TControl, TNotifyEvent etc.
  UVM, // Reference to your VM unit
  UControlEventHandler; // The helper class

type
  { TEventRouter: Centralized event dispatcher for interpreted forms }
  TEventRouter = class
  private
    FVM: TVM; // Reference to your VM instance
    // FControlEventMap: TObjectList; // Already declared in UControlEventHandler.pas if forward declaration is used there.
    // TObjectList to hold TControlEventHandler instances. It will own these objects.
    FControlHandlers: TObjectList;

  public
    constructor Create(AVM: TVM);
    destructor Destroy; override;

    // This method is called by TControlEventHandler.HandleEvent.
    // It identifies the control and dispatches to the VM.
    procedure GenericControlEventHandler(Sender: TObject);

    // Creates a method pointer (TNotifyEvent) that can be assigned to an LCL event property.
    // Internally, it creates a TControlEventHandler instance to bridge the event.
    function CreateEventHandler(AControl: TControl; const AInterpretedFunctionName: String): TNotifyEvent;
  end;

implementation

{ TEventRouter }

constructor TEventRouter.Create(AVM: TVM);
begin
  inherited Create;
  FVM := AVM;
  // TObjectList(True) ensures the TControlEventHandler objects are freed when FControlHandlers is freed.
  FControlHandlers := TObjectList.Create(True);
end;

destructor TEventRouter.Destroy;
begin
  FreeAndNil(FControlHandlers);
  FVM := nil; // Just clear reference, VM is owned by main form
  inherited Destroy;
end;

procedure TEventRouter.GenericControlEventHandler(Sender: TObject);
var
  Handler: TControlEventHandler;
  Found: Boolean;
  I: Integer;
begin
  // When an LCL event fires (via TControlEventHandler.HandleEvent), this method is called.
  // It iterates through its registered handlers to find which one corresponds to the Sender.
  Found := False;
  for I := 0 to FControlHandlers.Count - 1 do
  begin
    Handler := TControlEventHandler(FControlHandlers[I]);
    if Handler.FControl = Sender then
    begin
      Found := True;
      // Now, call your VM to execute the interpreted function
      if Assigned(FVM) then
      begin
        // In a real interpreter, this would queue an event for the VM's event loop
        // or directly call a VM method to execute the target Kayte function.
        WriteLn(SysUtils.Format('EventRouter: Event from "%s". Calling VM function: "%s"', [Sender.Name, Handler.FInterpretedFunctionName]));
        FVM.ExecuteInterpretedFunction(Handler.FInterpretedFunctionName);
      end
      else
      begin
        WriteLn('Error: VM not assigned to EventRouter or is nil!');
      end;
      Break; // Found the handler, no need to continue
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
  // Creates a new TControlEventHandler instance for the specific control and its Kayte function name.
  // This instance's HandleEvent method will be the one assigned to the LCL control's event.
  Handler := TControlEventHandler.Create(AControl, AInterpretedFunctionName, Self);
  FControlHandlers.Add(Handler); // Add to our list, so Lazarus doesn't free it prematurely

  // Return the method pointer to the TControlEventHandler's HandleEvent method
  Result := Handler.HandleEvent;
end;

end.
