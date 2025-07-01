unit UKfrmTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, // Contnrs for TObjectList
  Forms, Controls; // Forms for TPosition, Controls for TControl (indirectly via LCL types)

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
  // (though with the current structure, direct forward declarations might not be strictly needed
  // if definitions are ordered correctly, it's good practice for clarity).
  TKfrmComponentDef = class;
  TKfrmFormDef = class; // TKfrmFormDef inherits from TKfrmComponentDef

  // TEventBinding: Represents a binding between a UI event (e.g., OnClick)
  // and a Kayte script function name.
  TEventBinding = class(TObject)
  public
    EventName: String;    // e.g., 'OnClick'
    FunctionName: String; // The name of the Kayte function to call
    constructor Create(const AEventName, AFunctionName: String);
    destructor Destroy; override; // Added destructor for completeness, though not strictly needed for simple strings
  end;


  // TKfrmComponentDef: Base class for all UI components (forms and controls).
  // Contains common properties like name, position, and size.
  TKfrmComponentDef = class(TObject) // Inherit from TObject
  private
    FName: String;
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FCaption: String; // Common for many visual controls
    FVisible: Boolean;
    FEnabled: Boolean;
    FEventBindings: TObjectList; // List of TEventBinding (now actual TObject instances)

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
    property EventBindings: TObjectList read FEventBindings; // Contains TEventBinding objects
  end;

  // TKfrmFormDef: Represents a Kayte Form definition.
  // This is the root container for other controls.
  TKfrmFormDef = class(TKfrmComponentDef) // Inherit from TKfrmComponentDef
  private
    FControls: TObjectList; // List of TKfrmComponentDef (the controls on the form)
    FFileName: String;      // The original .kfrm filename
    FFormType: TControlKfrmType; // Stores the type, mainly cktForm for this class
    FPosition: TPosition; // Position property for forms

  public
    constructor Create; overload;
    constructor Create(const AName: String; ALeft, ATop, AWidth, AHeight: Integer; const AFileName: String); overload;
    destructor Destroy; override;

    property Controls: TObjectList read FControls; // Items are TKfrmComponentDef
    property FileName: String read FFileName write FFileName;
    property FormType: TControlKfrmType read FFormType;
    property Position: TPosition read FPosition write FPosition;
  end;

  // TKfrmLabelDef: Represents a Label control.
  TKfrmLabelDef = class(TKfrmComponentDef) // Inherit from TKfrmComponentDef
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
  TKfrmEditDef = class(TKfrmComponentDef) // Inherit from TKfrmComponentDef
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
  TKfrmButtonDef = class(TKfrmComponentDef) // Inherit from TKfrmComponentDef
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

{ TEventBinding }

constructor TEventBinding.Create(const AEventName, AFunctionName: String);
begin
  inherited Create;
  EventName := AEventName;
  FunctionName := AFunctionName;
end;

destructor TEventBinding.Destroy;
begin
  // Strings are managed by the compiler, no explicit freeing needed here.
  inherited Destroy;
end;


{ TKfrmComponentDef }

constructor TKfrmComponentDef.Create;
begin
  inherited Create; // Calls TObject.Create
  FName := '';
  FLeft := 0;
  FTop := 0;
  FWidth := 0;
  FHeight := 0;
  FCaption := '';
  FVisible := True;
  FEnabled := True;
  FEventBindings := TObjectList.Create(True); // Owns the TEventBinding objects
end;

constructor TKfrmComponentDef.Create(const AName: String; ALeft, ATop, AWidth, AHeight: Integer);
begin
  Create; // Call default constructor to initialize common fields and FEventBindings
  FName := AName;
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
  FHeight := AHeight;
end;

destructor TKfrmComponentDef.Destroy;
begin
  FreeAndNil(FEventBindings); // Frees all TEventBinding objects
  inherited Destroy;
end;

procedure TKfrmComponentDef.AddEventBinding(const AEventName, AFunctionName: String);
var
  Binding: TEventBinding;
begin
  Binding := TEventBinding.Create(AEventName, AFunctionName);
  FEventBindings.Add(Binding);
end;


{ TKfrmFormDef }

constructor TKfrmFormDef.Create;
begin
  inherited Create; // Calls TKfrmComponentDef.Create
  FControls := TObjectList.Create(True); // Owns the control objects (TKfrmComponentDef instances)
  FFileName := '';
  FFormType := cktForm;
  FPosition := poDesigned; // Default position for forms
end;

constructor TKfrmFormDef.Create(const AName: String; ALeft, ATop, AWidth, AHeight: Integer; const AFileName: String);
begin
  inherited Create(AName, ALeft, ATop, AWidth, AHeight); // Calls TKfrmComponentDef.Create(AName, ...)
  FControls := TObjectList.Create(True); // Owns the control objects
  FFileName := AFileName;
  FFormType := cktForm;
  FPosition := poDesigned; // Default position for forms
end;

destructor TKfrmFormDef.Destroy;
begin
  FreeAndNil(FControls); // Frees all control objects added to it
  inherited Destroy; // Calls TKfrmComponentDef.Destroy
end;

{ TKfrmLabelDef }

constructor TKfrmLabelDef.Create;
begin
  inherited Create; // Calls TKfrmComponentDef.Create
  FControlType := cktLabel;
  FAlignment := 'Left'; // Default
  FAutoSize := True;    // Default
  FWordWrap := False;  // Default
end;

constructor TKfrmLabelDef.Create(const AName: String; ALeft, ATop, AWidth, AHeight: Integer; const ACaption: String);
begin
  inherited Create(AName, ALeft, ATop, AWidth, AHeight); // Calls TKfrmComponentDef.Create(AName, ...)
  FControlType := cktLabel;
  FCaption := ACaption; // Set specific caption
  FAlignment := 'Left';
  FAutoSize := True;
  FWordWrap := False;
end;

{ TKfrmEditDef }

constructor TKfrmEditDef.Create;
begin
  inherited Create; // Calls TKfrmComponentDef.Create
  FControlType := cktEdit;
  FText := '';
  FReadOnly := False;
  FMaxLength := 0; // 0 means unlimited
  FPasswordChar := #0; // Null char means no password char
end;

constructor TKfrmEditDef.Create(const AName: String; ALeft, ATop, AWidth, AHeight: Integer; const AText: String);
begin
  inherited Create(AName, ALeft, ATop, AWidth, AHeight); // Calls TKfrmComponentDef.Create(AName, ...)
  FControlType := cktEdit;
  FText := AText; // Set specific text
  FReadOnly := False;
  FMaxLength := 0;
  FPasswordChar := #0;
end;

{ TKfrmButtonDef }

constructor TKfrmButtonDef.Create;
begin
  inherited Create; // Calls TKfrmComponentDef.Create
  FControlType := cktButton;
  // Specific TButton properties can be added here if needed,
  // but for now, Caption is handled by TKfrmComponentDef
end;

constructor TKfrmButtonDef.Create(const AName: String; ALeft, ATop, AWidth, AHeight: Integer; const ACaption: String);
begin
  inherited Create(AName, ALeft, ATop, AWidth, AHeight); // Calls TKfrmComponentDef.Create(AName, ...)
  FControlType := cktButton;
  FCaption := ACaption; // Set specific caption
end;

end.
