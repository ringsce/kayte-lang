unit JSBindings;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes; // Classes for TObject, if you plan to link Pascal objects to JS

const
{$IFDEF DARWIN} // Covers both Intel and ARM macOS
  {$IFDEF CPUARM64} // Specific to Apple Silicon (ARM64)
    QuickJSLib = 'libquickjs.a'; // Or 'libquickjs.dylib' if you build it dynamically
  {$ELSE} // Intel macOS
    QuickJSLib = 'libquickjs.a'; // Or 'libquickjs.dylib'
  {$ENDIF}
  {$ELSEIF DEF LINUX} // Uncommented for other platforms
  QuickJSLib = 'libquickjs.so'; // Linux dynamic library
{$ELSEIF DEF WINDOWS}
  QuickJSLib = 'quickjs.dll'; // Windows dynamic library
{$ELSE}
  QuickJSLib = 'libquickjs.so'; // Default fallback for other Unix-like OS
{$ENDIF}

  // This constant needs to be in the 'const' block, not 'type'
  JS_TAG_EXCEPTION = $FFFFFFFFFFFFFFF0; // QuickJS JS_TAG_EXCEPTION (Verify this value with QuickJS source)

type
  // QuickJS basic types (Pointers to internal C structs)
  PJSRuntime = Pointer;
  PJSContext = Pointer;
  JSValue = UInt64; // QuickJS uses a 64-bit value type. Be careful with conversions!
  PJSValue = ^JSValue; // FIX: Define PJSValue

  // QuickJS's JS_NewCFunction func type (for exposing Pascal funcs to JS)
  // This is a simplified signature. Real one involves argc and argv.
  // JS_CFunction (JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv)
  TJSCCallbackProc = function(ctx: PJSContext; this_val: JSValue; argc: Integer; argv: PJSValue): JSValue; cdecl;


// --- QuickJS C API Declarations (external functions) ---
// These are the direct calls to the QuickJS C library.

// Runtime and Context Management
function JS_NewRuntime: PJSRuntime; cdecl; external QuickJSLib;
procedure JS_FreeRuntime(rt: PJSRuntime); cdecl; external QuickJSLib;
function JS_NewContext(rt: PJSRuntime): PJSContext; cdecl; external QuickJSLib;
procedure JS_FreeContext(ctx: PJSContext); cdecl; external QuickJSLib;

// Value Creation
function JS_NewUndefined: JSValue; cdecl; external QuickJSLib;
function JS_NewNull: JSValue; cdecl; external QuickJSLib;
function JS_NewBool(b: Integer): JSValue; cdecl; external QuickJSLib;
function JS_NewInt32(v: Int32): JSValue; cdecl; external QuickJSLib;
function JS_NewFloat64(v: Double): JSValue; cdecl; external QuickJSLib;
function JS_NewStringLen(ctx: PJSContext; s: PAnsiChar; len: SizeInt): JSValue; cdecl; external QuickJSLib;
function JS_NewObject(ctx: PJSContext): JSValue; cdecl; external QuickJSLib;
function JS_NewArray(ctx: PJSContext): JSValue; cdecl; external QuickJSLib;

// Value Inspection and Type Checking
function JS_IsNumber(v: JSValue): Boolean; cdecl; external QuickJSLib;
function JS_IsString(v: JSValue): Boolean; cdecl; external QuickJSLib;
function JS_IsObject(v: JSValue): Boolean; cdecl; external QuickJSLib;
function JS_IsFunction(ctx: PJSContext; v: JSValue): Boolean; cdecl; external QuickJSLib;
function JS_IsUndefined(v: JSValue): Boolean; cdecl; external QuickJSLib;
function JS_IsNull(v: JSValue): Boolean; cdecl; external QuickJSLib;
function JS_IsException(v: JSValue): Boolean; cdecl; external QuickJSLib;

// Value Conversion
function JS_ToInt32(ctx: PJSContext; var pres: Int32; val: JSValue): Integer; cdecl; external QuickJSLib;
function JS_ToFloat64(ctx: PJSContext; var pres: Double; val: JSValue): Integer; cdecl; external QuickJSLib;
function JS_ToCString(ctx: PJSContext; val: JSValue): PAnsiChar; cdecl; external QuickJSLib;
procedure JS_FreeCString(ctx: PJSContext; ptr: PAnsiChar); cdecl; external QuickJSLib;

// Value Release (CRITICAL for memory management)
procedure JS_FreeValue(ctx: PJSContext; v: JSValue); cdecl; external QuickJSLib; // Decrements ref count

// Global Object
function JS_GetGlobalObject(ctx: PJSContext): JSValue; cdecl; external QuickJSLib;

// Object Property Access
function JS_SetPropertyStr(ctx: PJSContext; obj: JSValue; prop: PAnsiChar; val: JSValue): Integer; cdecl; external QuickJSLib;
function JS_GetPropertyStr(ctx: PJSContext; obj: JSValue; prop: PAnsiChar): JSValue; cdecl; external QuickJSLib;

// Function Creation and Execution
function JS_Eval(ctx: PJSContext; input: PAnsiChar; input_len: SizeInt; filename: PAnsiChar; eval_flags: Integer): JSValue; cdecl; external QuickJSLib;
function JS_NewCFunction(ctx: PJSContext; func: TJSCCallbackProc; name: PAnsiChar; length: Integer): JSValue; cdecl; external QuickJSLib;

// Exception/Error Handling
function JS_GetException(ctx: PJSContext): JSValue; cdecl; external QuickJSLib;

// --- Pascal Helper Functions (Wrappers for easier use) ---

var
  GlobalJSRuntime: PJSRuntime = nil; // Singleton runtime for the application


implementation

procedure InitJSRuntime;
begin
  if GlobalJSRuntime = nil then
    GlobalJSRuntime := JS_NewRuntime;
end;

procedure DoneJSRuntime;
begin
  if GlobalJSRuntime <> nil then
  begin
    JS_FreeRuntime(GlobalJSRuntime);
    GlobalJSRuntime := nil;
  end;
end;

function CreateJSContext: PJSContext;
begin
  InitJSRuntime; // Ensure runtime is initialized
  Result := JS_NewContext(GlobalJSRuntime);
  if Result = nil then
    raise Exception.Create('Failed to create QuickJS context.');
end;

procedure ReleaseJSContext(ctx: PJSContext);
begin
  JS_FreeContext(ctx);
end;

function CreateJSString(ctx: PJSContext; const S: String): JSValue;
var
  AnsiS: AnsiString;
begin
  AnsiS := AnsiString(S); // QuickJS expects UTF-8 or standard AnsiChar
  Result := JS_NewStringLen(ctx, PAnsiChar(AnsiS), Length(AnsiS));
end;

function JSValueToString(ctx: PJSContext; val: JSValue): String;
var
  CStr: PAnsiChar;
begin
  CStr := JS_ToCString(ctx, val);
  if CStr <> nil then
  begin
    Result := String(AnsiString(CStr)); // Convert AnsiChar to FPC String
    JS_FreeCString(ctx, CStr);
  end
  else
    Result := '';
end;

function CreateJSUndefined(ctx: PJSContext): JSValue;
begin
  Result := JS_NewUndefined;
end;

function GetGlobalJSObject(ctx: PJSContext): JSValue;
begin
  Result := JS_GetGlobalObject(ctx);
end;

procedure DefineJSFunction(ctx: PJSContext; obj: JSValue; const Name: String; Func: TJSCCallbackProc);
var
  FuncNameJS: JSValue;
  JSFunc: JSValue;
begin
  FuncNameJS := CreateJSString(ctx, Name);
  JSFunc := JS_NewCFunction(ctx, Func, PAnsiChar(AnsiString(Name)), 0); // Length is usually 0 for custom funcs
  JS_SetPropertyStr(ctx, obj, PAnsiChar(AnsiString(Name)), JSFunc);
  // JSFunc is now owned by the object, so we free the name value
  JS_FreeValue(ctx, FuncNameJS);
  // Do NOT free JSFunc here; it's owned by the JS object.
end;

// Helper to evaluate script and handle basic errors
procedure EvaluateJSCode(ctx: PJSContext; const Script: String; const Filename: String = '<eval>');
var
  ScriptJS: JSValue;
  ResultVal: JSValue;
  ExceptionVal: JSValue;
begin
  ScriptJS := CreateJSString(ctx, Script);
  ResultVal := JS_Eval(ctx, PAnsiChar(AnsiString(Script)), Length(Script), PAnsiChar(AnsiString(Filename)), 0); // 0 = JS_EVAL_TYPE_GLOBAL

  if JS_IsException(ResultVal) then
  begin
    ExceptionVal := JS_GetException(ctx);
    raise Exception.Create('JavaScript execution error: ' + JSValueToString(ctx, ExceptionVal));
    JS_FreeValue(ctx, ExceptionVal);
  end;
  JS_FreeValue(ctx, ScriptJS);
  JS_FreeValue(ctx, ResultVal); // Free the result of evaluation
end;

procedure SetJSObjectProperty(ctx: PJSContext; obj: JSValue; const PropName: String; val: JSValue);
begin
  JS_SetPropertyStr(ctx, obj, PAnsiChar(AnsiString(PropName)), val);
  // Note: The value 'val' is consumed by JS_SetPropertyStr if successful.
  // Do NOT free 'val' after this call if it's a newly created JSValue.
end;

function GetJSObjectProperty(ctx: PJSContext; obj: JSValue; const PropName: String): JSValue;
begin
  Result := JS_GetPropertyStr(ctx, obj, PAnsiChar(AnsiString(PropName)));
end;

// --- Advanced: Linking Pascal Objects to JS Objects ---
// QuickJS allows storing a native pointer on a JS object.

// Key for storing Pascal object reference in JS object
const
  PascalObjKey = 'PascalObjectPtr'; // Choose a unique property name

procedure ExposeNativePascalObject(ctx: PJSContext; jsObj: JSValue; PascalObj: TObject);
begin
  // This is a simplified way to link a Pascal object.
  // In QuickJS, you'd typically use 'private_data' or a specific type of object.
  // For demonstration, we'll store its pointer as a property.
  // THIS IS NOT ROBUST FOR PRODUCTION! Use JS_SetOpaque for proper linking in QuickJS.
  // It's better to create a custom JS class or use JS_SetOpaque for robust linking.
  // But for quick example:
  JS_SetPropertyStr(ctx, jsObj, PAnsiChar(AnsiString(PascalObjKey)), JS_NewFloat64(NativeUInt(PascalObj) as Double)); // FIX: Corrected call
end;

function GetNativePascalObject(ctx: PJSContext; jsObj: JSValue): TObject;
var
  PtrVal: Double;
  Res: Integer;
begin
  Result := nil;
  PtrVal := 0.0; // FIX: Initialize PtrVal to silence hint
  // This is also NOT ROBUST for production. See note in ExposeNativePascalObject.
  Res := JS_ToFloat64(ctx, PtrVal, JS_GetPropertyStr(ctx, jsObj, PAnsiChar(AnsiString(PascalObjKey))));
  if Res = 0 then
    Result := TObject(NativeUInt(PtrVal));
end;


initialization
  // Optional: Initialize the runtime when the unit loads
  // InitJSRuntime; // You might want to control this manually in your main program
finalization
  // Optional: Free the runtime when the unit unloads
  // DoneJSRuntime; // You might want to control this manually in your main program
end.
