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
    // !!! UNCOMMENTED HandleClick DECLARATION !!!
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

// !!! UNCOMMENTED AND CORRECTED CONSTRUCTOR IMPLEMENTATION !!!
constructor TUIEventHandler.Create(ARouter: TEventRouter; const AFunctionName: String);
begin
  inherited Create;
  FEventRouter := ARouter; // Corrected field name to FEventRouter
  FKayteFunctionName := AFunctionName; // Corrected field name to FKayteFunctionName
end;

// !!! UNCOMMENTED HandleClick IMPLEMENTATION !!!
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


procedure TEventRouter.RegisterOnClickHandler(AControl: TControl; const AKayteFunctionName: String);
var
  Handler: TUIEventHandler;
begin
  if not Assigned(AControl) then Exit;

  // Create a new event handler instance for this specific Kayte function
  Handler := TUIEventHandler.Create(Self, AKayteFunctionName);
  FEventHandlers.Add(Handler); // Add to our list so it's managed and not freed prematurely

  // Assign the handler's method to the LCL control's OnClick event
  if AControl is TButton then
    TButton(AControl).OnClick := TNotifyEvent(Handler.HandleClick) // Explicit cast added here
  else if AControl is TLabel then
    begin
      WriteLn(SysUtils.Format('UEventRouter: Warning: Cannot directly register OnClick for TLabel "%s". Consider making it interactive or using a clickable component.',
        [AControl.Name]));
    end
  else if AControl is TEdit then
    begin
      WriteLn(SysUtils.Format('UEventRouter: Warning: OnClick registration for TEdit "%s" might not be intended. Consider other events like OnChange or OnEnter.',
        [AControl.Name]));
    end
  // Add more control types as needed (e.g., TBitBtn, TSpeedButton, TPanel if clickable)
  // else if AControl is TSomeOtherClickableControl then
  //    TSomeOtherClickableControl(AControl).OnClick := Handler.HandleClick;
  ;
end;

end.
