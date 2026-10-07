unit kayte_qt6;

// Qt6 (QtWidgets) binding for the Kayte VM - backs the QT statement.
//
// Unlike kayte_sdl2/kayte_sdl3, Qt has no C API to bind with
// "cdecl; external", so this unit talks to libkayte_qt6 - the extern "C"
// shim in source/qt6/kayte_qt6.cpp - instead. The shim is loaded with
// dynlibs on the first QT statement rather than linked, so kayte still
// starts (and runs non-Qt scripts) on machines without Qt installed.
//
// The command set itself (names, argument rules, error messages) lives in
// the shim's kqt_call(), shared with natively compiled programs; this unit
// only marshals values across. The VM-level commands that call SUBs (on,
// run, event) are handled in VirtualMachine.pas.
//
// Library lookup order:
//   1. $KAYTE_QT6_LIB (full path)
//   2. next to the kayte executable
//   3. the platform's normal library search path

{$mode objfpc}{$H+}

interface

uses SysUtils, Math, dynlibs;

const
{$IFDEF WINDOWS}
  KayteQt6_Lib = 'kayte_qt6.dll';
{$ELSE}
  {$IFDEF DARWIN}
  KayteQt6_Lib = 'libkayte_qt6.dylib';
  {$ELSE}
  KayteQt6_Lib = 'libkayte_qt6.so';
  {$ENDIF}
{$ENDIF}

type
  // Mirrors the VM's dynamically-typed value (int or string) without
  // depending on VirtualMachine, so this unit stays usable on its own.
  TQtValue = record
    IsStr: Boolean;
    IntVal: Int64;
    StrVal: string;
  end;

// Runs one QT command, e.g. QtCall('button', [Win, 'OK']). Raises an
// exception (message from the shim) for an unknown command, bad
// arguments, or an invalid handle.
function QtCall(const Cmd: string; const Args: array of TQtValue): TQtValue;

function QtInt(Value: Int64): TQtValue;
function QtStr(const Value: string): TQtValue;

// Full path of the shim library this process would load, or '' if none
// is found. The native compiler bakes it into compiled programs.
function FindQtLibrary: string;

implementation

type
  // Matches struct kqt_value in kayte_qt6.cpp.
  TKqtValue = record
    IsStr: LongInt;
    Reserved: LongInt;
    I: Int64;
    S: PChar;
  end;
  PKqtValue = ^TKqtValue;

  TKqtCall = function(Cmd: PChar; Argc: LongInt; Args: PKqtValue; Res: PKqtValue): LongInt; cdecl;
  TKqtLastError = function: PChar; cdecl;

var
  LibHandle: TLibHandle = NilHandle;
  kqt_call: TKqtCall;
  kqt_last_error: TKqtLastError;

function QtInt(Value: Int64): TQtValue;
begin
  Result.IsStr := False;
  Result.IntVal := Value;
  Result.StrVal := '';
end;

function QtStr(const Value: string): TQtValue;
begin
  Result.IsStr := True;
  Result.IntVal := 0;
  Result.StrVal := Value;
end;

function LibraryCandidates: TStringArray;
begin
  Result := [GetEnvironmentVariable('KAYTE_QT6_LIB'),
             ExtractFilePath(ParamStr(0)) + KayteQt6_Lib,
             KayteQt6_Lib];
end;

function FindQtLibrary: string;
var
  Path: string;
begin
  for Path in LibraryCandidates do
    if (Path <> '') and FileExists(Path) then
      Exit(ExpandFileName(Path));
  Result := '';
end;

procedure EnsureLoaded;

  function Bind(const Name: string): Pointer;
  begin
    Result := GetProcedureAddress(LibHandle, Name);
    if Result = nil then
      raise Exception.CreateFmt('Runtime Error: %s is missing symbol %s - rebuild it from source/qt6',
        [KayteQt6_Lib, Name]);
  end;

var
  Path: string;
begin
  if LibHandle <> NilHandle then
    Exit;

  for Path in LibraryCandidates do
    if Path <> '' then
    begin
      LibHandle := LoadLibrary(Path);
      if LibHandle <> NilHandle then
        Break;
    end;

  if LibHandle = NilHandle then
    raise Exception.CreateFmt('Runtime Error: cannot load %s - build it from source/qt6 '
      + '(see source/qt6/README.md) and put it next to kayte or set KAYTE_QT6_LIB', [KayteQt6_Lib]);

  // FPC unmasks FPU exceptions by default, but Qt (like most C++ code)
  // relies on them being masked and would otherwise trap with "Invalid
  // floating point operation" during layout/font handling.
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);

  Pointer(kqt_call) := Bind('kqt_call');
  Pointer(kqt_last_error) := Bind('kqt_last_error');
end;

function QtCall(const Cmd: string; const Args: array of TQtValue): TQtValue;
var
  CArgs: array of TKqtValue;
  CRes: TKqtValue;
  I: Integer;
begin
  EnsureLoaded;

  // CArgs[I].S points into Args[I].StrVal, which outlives the call.
  CArgs := nil;
  SetLength(CArgs, Length(Args));
  for I := 0 to High(Args) do
  begin
    CArgs[I].IsStr := Ord(Args[I].IsStr);
    CArgs[I].Reserved := 0;
    CArgs[I].I := Args[I].IntVal;
    CArgs[I].S := PChar(Args[I].StrVal);
  end;

  if kqt_call(PChar(Cmd), Length(CArgs), PKqtValue(CArgs), @CRes) = 0 then
    raise Exception.Create('Runtime Error: ' + string(kqt_last_error()));

  if CRes.IsStr <> 0 then
    Result := QtStr(UTF8String(CRes.S))
  else
    Result := QtInt(CRes.I);
end;

end.
