unit WinComInterop;

{$mode objfpc}{$H+}

interface

uses
  Windows,       // Core Windows API types, constants, and functions
  SysUtils,      // For basic utilities like GUIDToString
  Classes,       // For TObject, TList etc.
  ComObj,        // For IUnknown, IDispatch, and basic COM helper functions (CoCreateInstance, CreateOleObject)
  ActiveXs;      // For common ActiveX related interfaces (IOleObject, IOleControl, etc.)

// --- SECTION 1: COMMON WINDOWS API DECLARATIONS (Examples) ---
// These are often already in the 'Windows' unit, but shown for illustration.
// You might add specific API calls here if not already in 'Windows'.

function ShowMessage(const Msg: string): Integer; stdcall; // Wrapper for MessageBox
  external user32 name 'MessageBoxA'; // Or MessageBoxW for Unicode

procedure OutputDebugString(const lpOutputString: PChar); stdcall;
  external kernel32 name 'OutputDebugStringA'; // Or OutputDebugStringW for Unicode

// Example: Function to get a Windows handle
function GetForegroundWindow: HWND; stdcall;
  external user32 name 'GetForegroundWindow';


// --- SECTION 2: COM INTERFACE DECLARATIONS (Examples) ---
// These are the blueprints for COM objects you might interact with.
// Always inherit from IUnknown (or IDispatch for Automation compatible objects).

const
  // Example GUIDs (Globally Unique Identifiers)
  // You would generate these for your own custom interfaces/classes.
  // Tools like Lazarus's "Create GUID" tool (Tools -> Create GUID) or PowerShell's [guid]::NewGuid()
  // are used for this.
  IID_IMyCustomComInterface: TGUID = '{YOUR-GUID-FOR-IMYCUSTOMCOMINTERFACE-HERE}'; // Replace with a real GUID
  CLSID_MyComAutomationClass: TGUID = '{YOUR-GUID-FOR-MYCOMAUTOMATIONCLASS-HERE}'; // Replace with a real GUID

type
  // IUnknown is the base interface for all COM objects.
  // It provides reference counting (AddRef, Release) and interface querying (QueryInterface).
  // Lazarus's ComObj unit already defines this.

  // IDispatch is the base interface for Automation (OLE Automation) compatible COM objects.
  // It allows late-bound method invocation (e.g., VBScript, JavaScript).
  // Lazarus's ComObj unit already defines this.

  // Example: A custom simple COM interface (inherits from IUnknown)
  IMyCustomComInterface = interface(IUnknown)
  ['{YOUR-GUID-FOR-IMYCUSTOMCOMINTERFACE-HERE}'] // Interface GUID must match constant above
    function GetMessage: WideString; stdcall;
    procedure SetValue(const AValue: Integer); stdcall;
    function GetValue: Integer; stdcall;
  end;

  // Example: A custom Automation-compatible COM interface (inherits from IDispatch)
  // Methods for IDispatch interfaces are usually late-bound and don't need stdcall.
  // However, if you explicitly define them, they can be early-bound too.
  IMyAutomationInterface = interface(IDispatch)
  ['{YOUR-GUID-FOR-IMYAUTOMATIONINTERFACE-HERE}'] // Interface GUID
    function GetAutomatedMessage: OleVariant; safecall; // safecall for error handling
    procedure SetAutomatedValue(const AValue: Integer); safecall;
  end;


// --- SECTION 3: HELPER FUNCTIONS FOR COM OBJECT INTERACTION ---

// Function to create an instance of a COM object given its Class ID (CLSID) and Interface ID (IID).
// This is low-level COM, often wrapped by higher-level functions like CreateOleObject.
function CreateComObject(const ClassID, InterfaceID: TGUID): IUnknown;
var
  hr: HRESULT;
  pUnk: IUnknown;
begin
  Result := nil;
  // Initialize COM library for the current thread if not already initialized
  hr := CoInitialize(nil); // CoInitializeEx is often preferred
  if SUCCEEDED(hr) or (hr = S_FALSE) then // S_FALSE means already initialized
  begin
    hr := CoCreateInstance(ClassID, nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, InterfaceID, Pointer(pUnk));
    if SUCCEEDED(hr) then
    begin
      Result := pUnk;
    end
    else
    begin
      Writeln('Error creating COM object (HRESULT: ', IntToHex(hr, 8), ')');
    end;
  end
  else
  begin
    Writeln('Error initializing COM library (HRESULT: ', IntToHex(hr, 8), ')');
  end;
end;


// --- SECTION 4: ACTIVEX-RELATED DECLARATIONS (for consumption, not building) ---
// ActiveX controls are specific types of COM objects with UI and embedding capabilities.
// The ActiveXs unit provides many standard interfaces like IOleObject, IOleControl, etc.

// Example: Function to embed an ActiveX control into a simple form (conceptual)
// This would typically be part of a Lazarus Form, not a standalone unit.
(*
procedure EmbedActiveXControl(const AClassName: string; AParentHandle: HWND);
var
  OleControl: IOleControl;
  OleObject: IOleObject;
  ClassID: TGUID;
  hr: HRESULT;
begin
  // Convert progID to CLSID (e.g., "MediaPlayer.MediaPlayer.1")
  hr := CLSIDFromProgID(PWideChar(StringToOleStr(AClassName)), ClassID);
  if FAILED(hr) then
  begin
    Writeln('Error: Could not get CLSID for ProgID: ', AClassName);
    Exit;
  end;

  // Create the ActiveX control
  OleControl := CreateComObject(ClassID, IOleControl) as IOleControl;
  if Assigned(OleControl) then
  begin
    OleObject := OleControl as IOleObject;
    if Assigned(OleObject) then
    begin
      // Set the client site (container) for the control
      // This is a complex part involving implementing IOleClientSite interface
      // and passing it to OleObject.SetClientSite.
      // For a simple demo, you'd typically use TActiveXContainer in Lazarus.
      Writeln('ActiveX control created, but embedding requires more code.');
    end;
  end;
end;
*)

// --- SECTION 5: INITIALIZATION / FINALIZATION ---
// CoInitialize and CoUninitialize manage the COM library lifecycle.
// Call CoInitialize once per thread that uses COM.
// CoUninitialize should be called when the thread no longer needs COM.

initialization
  // It's good practice to initialize COM in your main application's initialization
  // block if you know you'll be using COM throughout.
  // CoInitializeEx(nil, COINIT_APARTMENTTHREADED); // Preferred for UI threads
finalization
  // CoUninitialize; // Uncomment if CoInitialize was called without CoUninitialize per thread.
end.
