// Routes UI events from LCL controls to Kayte script functions.

unit UEventRouter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Contnrs, Forms;

// Stand-in for the real Kayte VM so this unit compiles standalone.
// Swap in the actual VM unit and delete this once it's wired up.
type
  TKayteVM = class
  public
    procedure ExecuteFunction(const FunctionName: string);
  end;

type
  TEventRouter = class;

  // The actual LCL event handler: dynamically created and hooked to a control's
  // event, each instance knows which Kayte function to call when it fires.
  TUIEventHandler = class(TObject)
  private
    FEventRouter: TEventRouter;
    FKayteFunctionName: String;

  public
    constructor Create(ARouter: TEventRouter; const AFunctionName: String);
    procedure HandleClick(Sender: TObject);

    property KayteFunctionName: String read FKayteFunctionName;
  end;


  TEventRouter = class
  private
    FVM: TKayteVM;
    FEventHandlers: TObjectList; // owns the TUIEventHandler instances

  public
    constructor Create(AVM: TKayteVM);
    destructor Destroy; override;

    procedure RegisterOnClickHandler(AControl: TControl; const AKayteFunctionName: String);
  end;

implementation

{ TKayteVM (dummy) }
procedure TKayteVM.ExecuteFunction(const FunctionName: string);
begin
  WriteLn(SysUtils.Format('KayteVM: Executing Kayte function: %s (This is a DUMMY VM call)', [FunctionName]));
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
  FEventHandlers := TObjectList.Create(True);
end;

destructor TEventRouter.Destroy;
begin
  FreeAndNil(FEventHandlers);
  FVM := nil; // owned externally
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
  FEventHandlers.Add(Handler); // keep it alive

  if AControl is TButton then
    TButton(AControl).OnClick := @Handler.HandleClick
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
