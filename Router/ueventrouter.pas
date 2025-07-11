// UEventRouter.pas
// Responsible for routing UI events from LCL controls to Kayte script functions.

unit UEventRouter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Contnrs, Forms; // StdCtrls for TButton, TLabel, TEdit; Contnrs for TObjectList

// IMPORTANT: This TKayteVM definition is a DUMMY for compilation purposes only.
// In your actual project, you MUST REMOVE this dummy definition
// and instead include your real Kayte VM unit in the 'uses' clause of your main project file
// or any unit that needs to access TKayteVM (and consequently UEventRouter).
type
  // Dummy TKayteVM definition - REMOVE THIS WHEN YOU HAVE THE REAL VM UNIT
  TKayteVM = class
  public
    procedure ExecuteFunction(const FunctionName: string);
  end;
  // End of Dummy TKayteVM definition

type
  TEventRouter = class; // Forward declaration for TEventRouter

  // TUIEventHandler: This class will act as the actual LCL event handler.
  // We'll create instances of this dynamically and connect them to LCL control events.
  // Each instance will know which Kayte function it needs to call.
  TUIEventHandler = class(TObject)
  private
    FEventRouter: TEventRouter; // Reference back to the main router
    FKayteFunctionName: String; // The Kayte function this handler should call

  public
    constructor Create(ARouter: TEventRouter; const AFunctionName: String);
    procedure HandleClick(Sender: TObject); // The actual method assigned to OnClick

    property KayteFunctionName: String read FKayteFunctionName;
  end;


  TEventRouter = class
  private
    FVM: TKayteVM; // Reference to the Kayte Virtual Machine
    // This list will hold the TUIEventHandler instances, so they are not freed prematurely.
    FEventHandlers: TObjectList; // Owns the TUIEventHandler instances

  public
    constructor Create(AVM: TKayteVM);
    destructor Destroy; override;

    // Registers an OnClick event for a given LCL control to a Kayte function.
    procedure RegisterOnClickHandler(AControl: TControl; const AKayteFunctionName: String);
  end;

implementation

// { TKayteVM - Dummy Implementation for methods }
// The class definition is now in the interface, but its methods are still here.
// When you use the real TKayteVM unit, these implementations also need to be removed.
procedure TKayteVM.ExecuteFunction(const FunctionName: string);
begin
  WriteLn(SysUtils.Format('KayteVM: Executing Kayte function: %s (This is a DUMMY VM call)', [FunctionName]));
  // In a real VM, you would look up and execute the bytecode/AST for FunctionName here.
  // For example: YourVMInstance.CallFunction(FunctionName);
end;


{ TUIEventHandler }

constructor TUIEventHandler.Create(ARouter: TEventRouter; const AFunctionName: String);
begin
  inherited Create;
  FEventRouter := ARouter;
  FKayteFunctionName := AFunctionName;
end;

procedure TUIEventHandler.HandleClick(Sender: TObject);
begin
  // When the LCL control is clicked, this method is called.
  // It then tells the EventRouter to execute the corresponding Kayte function.
  if Assigned(FEventRouter) and Assigned(FEventRouter.FVM) then
  begin
    WriteLn(SysUtils.Format('UEventRouter: Control "%s" Clicked. Calling Kayte function "%s".',
      [TControl(Sender).Name, FKayteFunctionName]));
    FEventRouter.FVM.ExecuteFunction(FKayteFunctionName);
  end;
end;

{ TEventRouter }

constructor TEventRouter.Create(AVM: TKayteVM);
begin
  inherited Create;
  FVM := AVM;
  FEventHandlers := TObjectList.Create(True); // Owns the TUIEventHandler instances
end;

destructor TEventRouter.Destroy;
begin
  FreeAndNil(FEventHandlers); // Frees all TUIEventHandler instances
  FVM := nil; // Just nil the reference as it's owned externally
  inherited Destroy;
end;


procedure TEventRouter.RegisterOnClickHandler(AControl: TControl;
  const AKayteFunctionName: String);
var
  Handler: TUIEventHandler;
begin
  if not Assigned(AControl) then
    Exit;

  Handler := TUIEventHandler.Create(Self, AKayteFunctionName);
  FEventHandlers.Add(Handler);          // keep it alive

  {--- assign the event handler ---}
  if AControl is TButton then
    TButton(AControl).OnClick := @Handler.HandleClick          //  ←  @ here!
  else if AControl is TLabel then
    WriteLn(Format(
      'UEventRouter: Warning: Cannot directly register OnClick for TLabel "%s".',
      [AControl.Name]))
  else if AControl is TEdit then
    WriteLn(Format(
      'UEventRouter: Warning: OnClick for TEdit "%s" is unusual; consider OnChange.',
      [AControl.Name]));
  { add more control types as needed }
end;

end.
