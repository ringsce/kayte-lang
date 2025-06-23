// UKfrmTypes.pas
// Defines the data structures for representing Kayte UI (.kfrm) forms and controls.

unit UKfrmTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs; // Contnrs for TObjectList

type
  // Enum for different types of controls supported in .kfrm files
  TControlKfrmType = (
    cktForm,
    cktLabel,
    cktEdit,
    cktButton
    // Add more control types as needed (e.g., cktMemo, cktPanel, cktCheckbox)
  );

  // Forward declarations for classes that might reference each other
  // TKfrmControlList = class; // <-- REMOVE THIS LINE
  TKfrmComponentDef = class;
  TKfrmFormDef = class;

  // TEventBinding: Represents a binding between a UI event (e.g., OnClick)
  // and a Kayte script function name.
  // NOTE: If using TObjectList, it's better for this to be a class,
  // or use a TList<PEventBinding> and manage memory manually.
  // For now, let's keep it as a record but acknowledge TObjectList's expectation of objects.
  PEventBinding = ^TEventBinding; // Pointer to the record for TObjectList
  TEventBinding = record
    EventName: String;    // e.g., 'OnClick'
    FunctionName: String; // The name of the Kayte function to call
  end;


  // TKfrmComponentDef: Base class for all UI components (forms and controls).
  // Contains common properties like name, position, and size.
  TKfrmComponentDef = class
  private
    FName: String;
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FCaption: String; // Common for many visual controls
    FVisible: Boolean;
    FEnabled: Boolean;
    FEventBindings: TObjectList; // List of PEventBinding (pointers to TEventBinding records)

  public
    constructor Create; overload;
    constructor Create(const AName: String; ALeft, ATop, AWidth, AHeight: Integer); overload;
    destructor Destroy; override;

    procedure AddEventBinding(const AEventName, AFunctionName: String);

    property Name: String read FName write FName;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Caption: String read FCaption write FCaption;
    property Visible: Boolean read FVisible write FVisible;
    property Enabled: Boolean read FEnabled write FEnabled;
    property EventBindings: TObjectList read FEventBindings; // Contains PEventBinding
  end;

  // TKfrmFormDef: Represents a Kayte Form definition.
  // This is the root container for other controls.
  TKfrmFormDef = class(TKfrmComponentDef)
  private
    FControls: TObjectList; // List of TKfrmComponentDef (the controls on the form)
    FFileName: String;      // The original .kfrm filename
    FFormType: TControlKfrmType; // Stores the type, mainly cktForm for this class

  public
    constructor Create; overload;
    constructor Create(const AName: String; ALeft, ATop, AWidth, AHeight: Integer; const AFileName: String); overload;
    destructor Destroy; override;

    property Controls: TObjectList read FControls; // Items are TKfrmComponentDef
    property FileName: String read FFileName write FFileName;
    property FormType: TControlKfrmType read FFormType;
  end;

  // TKfrmLabelDef: Represents a Label control.
  TKfrmLabelDef = class(TKfrmComponentDef)
  private
    FControlType: TControlKfrmType; // Stores cktLabel
    // Specific properties for TLabel
    FAlignment: String; // e.g., 'Left', 'Center', 'Right'
    FAutoSize: Boolean;
    FWordWrap: Boolean;

  public
    constructor Create; overload;
    constructor Create(const AName: String; ALeft, ATop, AWidth, AHeight: Integer; const ACaption: String); overload;

    property ControlType: TControlKfrmType read FControlType;
    property Alignment: String read FAlignment write FAlignment;
    property AutoSize: Boolean read FAutoSize write FAutoSize;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
  end;

  // TKfrmEditDef: Represents an Edit control (single-line text input).
  TKfrmEditDef = class(TKfrmComponentDef)
  private
    FControlType: TControlKfrmType; // Stores cktEdit
    // Specific properties for TEdit
    FText: String;
    FReadOnly: Boolean;
    FMaxLength: Integer;
    FPasswordChar: Char;

  public
    constructor Create; overload;
    constructor Create(const AName: String; ALeft, ATop, AWidth, AHeight: Integer; const AText: String); overload;

    property ControlType: TControlKfrmType read FControlType;
    property Text: String read FText write FText;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property MaxLength: Integer read FMaxLength write FMaxLength;
    property PasswordChar: Char read FPasswordChar write FPasswordChar;
  end;

  // TKfrmButtonDef: Represents a Button control.
  TKfrmButtonDef = class(TKfrmComponentDef)
  private
    FControlType: TControlKfrmType; // Stores cktButton
    // Specific properties for TButton (none beyond common for now, 'Caption' is key)
    // TButton has 'ModalResult', 'Default', 'Cancel', etc., but we'll keep it simple for now.

  public
    constructor Create; overload;
    constructor Create(const AName: String; ALeft, ATop, AWidth, AHeight: Integer; const ACaption: String); overload;

    property ControlType: TControlKfrmType read FControlType;
  end;

implementation

{ TKfrmComponentDef }

constructor TKfrmComponentDef.Create;
begin
  inherited Create;
  FName := '';
  FLeft := 0;
  FTop := 0;
  FWidth := 0;
  FHeight := 0;
  FCaption := '';
  FVisible := True;
  FEnabled := True;
  FEventBindings := TObjectList.Create(True); // Owns records (pointers to records)
end;

constructor TKfrmComponentDef.Create(const AName: String; ALeft, ATop, AWidth, AHeight: Integer);
begin
  Create; // Call default constructor
  FName := AName;
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
  FHeight := AHeight;
end;

destructor TKfrmComponentDef.Destroy;
begin
  // For TObjectList to properly free the PEventBinding, they must be allocated with New.
  // The AddEventBinding procedure uses New(Binding), so TObjectList(True) will call FreeMem for us.
  FreeAndNil(FEventBindings);
  inherited Destroy;
end;

procedure TKfrmComponentDef.AddEventBinding(const AEventName, AFunctionName: String);
var
  Binding: PEventBinding; // Pointer to a new TEventBinding record
begin
  New(Binding); // Allocate memory for the record
  Binding^.EventName := AEventName;
  Binding^.FunctionName := AFunctionName;
  FEventBindings.Add(Binding); // Add the pointer to the list
end;


{ TKfrmFormDef }

constructor TKfrmFormDef.Create;
begin
  inherited Create; // Call base class constructor
  FControls := TObjectList.Create(True); // Owns the control objects
  FFileName := '';
  FFormType := cktForm;
end;

constructor TKfrmFormDef.Create(const AName: String; ALeft, ATop, AWidth, AHeight: Integer; const AFileName: String);
begin
  inherited Create(AName, ALeft, ATop, AWidth, AHeight);
  FControls := TObjectList.Create(True);
  FFileName := AFileName;
  FFormType := cktForm;
end;

destructor TKfrmFormDef.Destroy;
begin
  FreeAndNil(FControls); // Frees all control objects added to it
  inherited Destroy;
end;

{ TKfrmLabelDef }

constructor TKfrmLabelDef.Create;
begin
  inherited Create;
  FControlType := cktLabel;
  FAlignment := 'Left'; // Default
  FAutoSize := True;   // Default
  FWordWrap := False;  // Default
end;

constructor TKfrmLabelDef.Create(const AName: String; ALeft, ATop, AWidth, AHeight: Integer; const ACaption: String);
begin
  inherited Create(AName, ALeft, ATop, AWidth, AHeight);
  FControlType := cktLabel;
  FCaption := ACaption;
  FAlignment := 'Left';
  FAutoSize := True;
  FWordWrap := False;
end;

{ TKfrmEditDef }

constructor TKfrmEditDef.Create;
begin
  inherited Create;
  FControlType := cktEdit;
  FText := '';
  FReadOnly := False;
  FMaxLength := 0; // 0 means unlimited
  FPasswordChar := #0; // Null char means no password char
end;

constructor TKfrmEditDef.Create(const AName: String; ALeft, ATop, AWidth, AHeight: Integer; const AText: String);
begin
  inherited Create(AName, ALeft, ATop, AWidth, AHeight);
  FControlType := cktEdit;
  FText := AText;
  FReadOnly := False;
  FMaxLength := 0;
  FPasswordChar := #0;
end;

{ TKfrmButtonDef }

constructor TKfrmButtonDef.Create;
begin
  inherited Create;
  FControlType := cktButton;
  // Specific TButton properties can be added here if needed,
  // but for now, Caption is handled by TKfrmComponentDef
end;

constructor TKfrmButtonDef.Create(const AName: String; ALeft, ATop, AWidth, AHeight: Integer; const ACaption: String);
begin
  inherited Create(AName, ALeft, ATop, AWidth, AHeight);
  FControlType := cktButton;
  FCaption := ACaption;
end;

implementation

{ TKfrmComponentDef }

constructor TKfrmComponentDef.Create;
begin
  inherited Create;
  FName := '';
  FLeft := 0;
  FTop := 0;
  FWidth := 0;
  FHeight := 0;
  FCaption := '';
  FVisible := True;
  FEnabled := True;
  FEventBindings := TObjectList.Create(True); // Owns records (pointers to records)
end;

constructor TKfrmComponentDef.Create(const AName: String; ALeft, ATop, AWidth, AHeight: Integer);
begin
  Create; // Call default constructor
  FName := AName;
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
  FHeight := AHeight;
end;

destructor TKfrmComponentDef.Destroy;
begin
  // FEventBindings stores records, which are value types.
  // TObjectList needs to own objects, not records.
  // If TEventBinding were a class, FEventBindings would free them.
  // Since it's a record, we just free the list itself.
  // If we decided to store PEventBinding and allocated them, we'd need to free PEventBinding.
  // For simplicity, let's assume we'll store TEventBinding directly if we had a TList<TEventBinding> (FPC 3.2
