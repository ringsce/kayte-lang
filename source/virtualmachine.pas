unit VirtualMachine;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TypInfo, Math, Process, fgl, BytecodeTypes, kayte_qt6; // BytecodeTypes unit is essential for TByteCodeProgram

type
  // Runtime values are dynamically typed (like a BASIC Variant), holding
  // an integer, a string, or an array. Operators decide at runtime how to
  // combine them (e.g. + adds two integers but concatenates otherwise).
  // Arrays are shared by reference (like VB.NET arrays); a class object is
  // an array of its fields, tagged with the class name.
  TValueKind = (vkInt, vkStr, vkArr, vkFloat);

  IKayteArray = interface;

  TKayteValue = record
    Kind: TValueKind;
    IntVal: Int64;
    StrVal: string;
    Arr: IKayteArray; // vkArr (reference counted)
    FltVal: Double;   // vkFloat (always finite)
  end;

  IKayteArray = interface
    ['{6E4D1D9A-7C35-4B2E-9C8E-2C1A5E0B7F31}']
    function Obj: TObject;
  end;

  TKayteArray = class(TInterfacedObject, IKayteArray)
  public
    Items: array of TKayteValue;
    Tag: string; // the class name of an object; '' for a plain array
    function Obj: TObject;
  end;

  // An active TRY: where its CATCH is, and the evaluation stack / call
  // depth to go back to when an error is caught.
  TTryHandler = record
    Catch: LongInt;
    StackTop: Integer;
    CallDepth: Integer;
  end;

  // One active CALL: where to resume, and how many arguments it pushed
  // (checked against the SUB's BC_ENTER).
  TCallFrame = record
    ReturnIP: LongInt;
    ArgCount: Integer;
  end;

  // Qt widget/menu/timer handle -> entry address of its handler SUB.
  TQtHandlerMap = specialize TFPGMap<Int64, Integer>;

  TVirtualMachine = class(TObject)
   private
     FProgram: TByteCodeProgram;
     FInstructionPointer: LongInt; // To track the current instruction
     FStack: array of TKayteValue;
     FStackTop: Integer;
     FVariables: array of TKayteValue;
     FCallStack: array of TCallFrame;
     FCallDepth: Integer;
     FQtHandlers: TQtHandlerMap;
     FQtEvent: Int64; // handle whose event QT "run" is currently handling
     FTries: array of TTryHandler;
     FTryCount: Integer;
     FColumn: Integer; // output column, for QuickBASIC PRINT's "," and TAB
     // QuickBASIC files #1 .. #255: C FILE handles, mode (0 INPUT, 1
     // OUTPUT, 2 APPEND) and output column.
     FFiles: array[1..255] of Pointer;
     FFileMode: array[1..255] of Integer;
     FFileCol: array[1..255] of Int64;
     FFileLen: array[1..255] of Int64; // RANDOM: the record length
     FFileRec: array[1..255] of Int64; // RANDOM: the last record read or written
     procedure NeedOpen(Num: Int64);
     function BinSeek(Num, Pos: Int64; out RecLen: Int64): Pointer;
     procedure PutValue(var B: string; const L: string; var P: Integer; const V: TKayteValue);
     function LaySize(const L: string; var P: Integer; const Cur: TKayteValue): Int64;
     function GetValue(const D: string; var Off: Integer; const L: string; var P: Integer;
       const Cur: TKayteValue): TKayteValue;
     function MkCv(Id: Integer; const V: TKayteValue): TKayteValue;
     function MkSingle(F: Single): TKayteValue;
     function FormatUsing(const Args: array of TKayteValue): TKayteValue;
     function NeedFile(Num: Int64; Input: Boolean): Pointer;
     procedure FileOut(Num: Int64; const S: string);
     procedure FilePrint(Num: Int64; Kind: Integer; const V: TKayteValue);
     function FileLine(Num: Int64): string;
     function FileField(Num: Int64): string;
     function FileBuiltin(Id: Integer; const Args: array of TKayteValue): TKayteValue;
     procedure CloseFiles;

     procedure Output(const S: string);
     procedure Push(const Value: TKayteValue);
     function Pop: TKayteValue;
     function MakeInt(Value: Int64): TKayteValue;
     function MakeStr(const Value: string): TKayteValue;
     function MakeFloat(Value: Double): TKayteValue;
     function NumOf(const Value: TKayteValue): TKayteValue;
     function ToFloat(const Value: TKayteValue): Double;
     function Arith(Op: TByteCodeOp; const A, B: TKayteValue): TKayteValue;
     function MakeArr(Count: Integer; const Tag: string = ''): TKayteValue;
     function ArrayOf(const Value: TKayteValue; const What: string): TKayteArray;
     function DisplayDepth(const Value: TKayteValue; Depth: Integer): string;
     function CallBuiltin(Id: Integer; const Args: array of TKayteValue): TKayteValue;
     function NewArray(const Dims: array of Int64; Level: Integer): TKayteValue;
     function ToDisplayString(const Value: TKayteValue): string;
     function ToInt(const Value: TKayteValue): Int64;
     function IsTruthy(const Value: TKayteValue): Boolean;
     function CompareValues(const A, B: TKayteValue; Op: TByteCodeOp): Boolean;
     function GetIntegerLiteral(Index: Integer): Int64;
     function GetStringLiteral(Index: Integer): string;
     procedure CheckVarIndex(Index: Integer);
     function SubNameAt(Address: Integer): string;
     procedure EnterCall(Target: Integer; ArgCount: Integer; ReturnIP: LongInt);
     function QtVMCommand(const Keyword: string; const Args: array of TKayteValue;
       out Res: TKayteValue; out Jumped: Boolean): Boolean;
     procedure AttachFormHandlers(Window: Int64; const Keyword, Prefix: string);
   public
     // Constructor to accept the pre-loaded TByteCodeProgram object
     constructor Create(AProgram: TByteCodeProgram);
     destructor Destroy; override;
     procedure Run;
   end;

implementation

{ TVirtualMachine }

constructor TVirtualMachine.Create(AProgram: TByteCodeProgram);
begin
  inherited Create;
  FProgram := AProgram;
  FInstructionPointer := 0;
  FStackTop := 0;
  FQtHandlers := TQtHandlerMap.Create;

  // AProgram's memory isn't owned here - the caller
  // (TCLIHandler.RunBytecodeFile) frees it after the VM is done.
end;

destructor TVirtualMachine.Destroy;
begin
  // No need to free FProgram here, as the CLIHandler manages its lifetime.
  FQtHandlers.Free;
  inherited Destroy;
end;

procedure TVirtualMachine.Push(const Value: TKayteValue);
begin
  if FStackTop >= Length(FStack) then
    SetLength(FStack, Length(FStack) * 2 + 16);
  FStack[FStackTop] := Value;
  Inc(FStackTop);
end;

function TVirtualMachine.Pop: TKayteValue;
begin
  if FStackTop <= 0 then
    raise Exception.Create('Runtime Error: evaluation stack underflow (malformed bytecode)');
  Dec(FStackTop);
  Result := FStack[FStackTop];
end;

function TVirtualMachine.MakeInt(Value: Int64): TKayteValue;
begin
  Result.Kind := vkInt;
  Result.FltVal := 0;
  Result.IntVal := Value;
  Result.StrVal := '';
  Result.Arr := nil;
end;

function TVirtualMachine.MakeFloat(Value: Double): TKayteValue;
begin
  if IsNan(Value) or IsInfinite(Value) then
    raise Exception.Create('Runtime Error: overflow (the result is too large for a number)');
  Result.Kind := vkFloat;
  Result.IntVal := 0;
  Result.StrVal := '';
  Result.Arr := nil;
  if Value = 0 then
    Result.FltVal := 0 // no -0
  else
    Result.FltVal := Value;
end;

// The C library's snprintf: its correctly rounded "%.14e" is what the
// native runtime formats doubles from (FPC's own conversion rounds
// differently in the last digit).
{$IFDEF WINDOWS}
function c_snprintf(Buf: PChar; Size: PtrUInt; Fmt: PChar): LongInt; cdecl; varargs; external 'msvcrt' name '_snprintf';
{$ELSE}
function c_snprintf(Buf: PChar; Size: PtrUInt; Fmt: PChar): LongInt; cdecl; varargs; external 'c' name 'snprintf';
{$ENDIF}

// The C math library, for results identical to the native runtime's to
// the last bit (FPC's Power, Sin ... can differ there).
const
{$IFDEF WINDOWS}
  LibM = 'msvcrt';
{$ELSE}{$IFDEF DARWIN}
  LibM = 'c';
{$ELSE}
  LibM = 'm';
{$ENDIF}{$ENDIF}
function c_pow(X, Y: Double): Double; cdecl; external LibM name 'pow';
function c_fmod(X, Y: Double): Double; cdecl; external LibM name 'fmod';
function c_sqrt(X: Double): Double; cdecl; external LibM name 'sqrt';
function c_sin(X: Double): Double; cdecl; external LibM name 'sin';
function c_cos(X: Double): Double; cdecl; external LibM name 'cos';
function c_tan(X: Double): Double; cdecl; external LibM name 'tan';
function c_atan(X: Double): Double; cdecl; external LibM name 'atan';
function c_exp(X: Double): Double; cdecl; external LibM name 'exp';
function c_log(X: Double): Double; cdecl; external LibM name 'log';
function c_log10(X: Double): Double; cdecl; external LibM name 'log10';
function c_floor(X: Double): Double; cdecl; external LibM name 'floor';

// C stdio, for QuickBASIC files: the native runtime uses the same calls,
// so both behave the same.
const
{$IFDEF WINDOWS}
  LibC = 'msvcrt';
{$ELSE}
  LibC = 'c';
{$ENDIF}
function c_fopen(Name, Mode: PChar): Pointer; cdecl; external LibC name 'fopen';
function c_fclose(F: Pointer): LongInt; cdecl; external LibC name 'fclose';
function c_fwrite(Buf: Pointer; Size, Count: PtrUInt; F: Pointer): PtrUInt; cdecl; external LibC name 'fwrite';
function c_fgetc(F: Pointer): LongInt; cdecl; external LibC name 'fgetc';
function c_fread(Buf: Pointer; Size, Count: PtrUInt; F: Pointer): PtrUInt; cdecl; external LibC name 'fread';
function c_ungetc(C: LongInt; F: Pointer): LongInt; cdecl; external LibC name 'ungetc';
function c_fflush(F: Pointer): LongInt; cdecl; external LibC name 'fflush';
function c_fseek(F: Pointer; Offset: PtrInt; Whence: LongInt): LongInt; cdecl; external LibC name 'fseek';
function c_ftell(F: Pointer): PtrInt; cdecl; external LibC name 'ftell';
function c_remove(Name: PChar): LongInt; cdecl; external LibC name 'remove';
function c_rename(OldName, NewName: PChar): LongInt; cdecl; external LibC name 'rename';
function c_strerror(Err: LongInt): PChar; cdecl; external LibC name 'strerror';
{$IFDEF DARWIN}
function c_errno_ptr: PLongInt; cdecl; external LibC name '__error';
{$ELSE}{$IFDEF WINDOWS}
function c_errno_ptr: PLongInt; cdecl; external LibC name '_errno';
{$ELSE}
function c_errno_ptr: PLongInt; cdecl; external LibC name '__errno_location';
{$ENDIF}{$ENDIF}

function CErrorText: string;
begin
  Result := StrPas(c_strerror(c_errno_ptr^));
end;

const
  KMaxFiles = 255;
// C's rint: to the nearest whole number, to even on .5 (exact: doubles of
// 2^52 and more are whole already).
function c_rint(X: Double): Double;
begin
  if Abs(X) >= 4503599627370496.0 then
    Result := X
  else
    Result := Round(X);
end;

// A double's text (VB style): 15 significant digits without trailing
// zeros, scientific notation (1.5E+20, 1E-07) outside 1E-05 .. 1E+15. QB:
// QuickBASIC's ".5" for "0.5". The native runtime's fmt_flt matches it.
function FormatNumber(V: Double; QB: Boolean): string;
var
  M, Digits: string;
  P, E, I: Integer;
  Buf: array[0..63] of Char;
begin
  if V = 0 then
    Exit('0');
  c_snprintf(@Buf[0], SizeOf(Buf), '%.14e', Abs(V)); // d.dddddddddddddde+XX
  M := StrPas(@Buf[0]);
  P := Pos('E', UpperCase(M));
  Digits := '';
  for I := 1 to P - 1 do
    if M[I] in ['0'..'9'] then
      Digits := Digits + M[I];
  E := StrToInt(Copy(M, P + 1, MaxInt));
  while (Length(Digits) > 1) and (Digits[Length(Digits)] = '0') do
    SetLength(Digits, Length(Digits) - 1);
  if V < 0 then
    Result := '-'
  else
    Result := '';
  if (E >= 15) or (E < -5) then
  begin
    Result := Result + Digits[1];
    if Length(Digits) > 1 then
      Result := Result + '.' + Copy(Digits, 2, MaxInt);
    if E < 0 then
      Result := Result + 'E-' + Format('%.2d', [-E])
    else
      Result := Result + 'E+' + Format('%.2d', [E]);
  end
  else if E >= 0 then
  begin
    for I := 1 to E + 1 do
      if I <= Length(Digits) then
        Result := Result + Digits[I]
      else
        Result := Result + '0';
    if Length(Digits) > E + 1 then
      Result := Result + '.' + Copy(Digits, E + 2, MaxInt);
  end
  else
  begin
    if not QB then
      Result := Result + '0';
    Result := Result + '.' + StringOfChar('0', -E - 1) + Digits;
  end;
end;

// A number written as text: optional spaces and sign, digits with at most
// one ".", an optional exponent - like the native runtime's str_to_num.
function StrToNumber(const S: string; out IsFloat: Boolean; out I: Int64; out D: Double): Boolean;
var
  T: string;
  P, Digits, Dots, Code: Integer;
begin
  Result := False;
  IsFloat := False;
  I := 0;
  D := 0;
  T := Trim(S);
  if T = '' then
    Exit;
  P := 1;
  if T[1] in ['+', '-'] then
    Inc(P);
  Digits := 0;
  Dots := 0;
  while (P <= Length(T)) and (T[P] in ['0'..'9', '.']) do
  begin
    if T[P] = '.' then Inc(Dots) else Inc(Digits);
    Inc(P);
  end;
  if (Digits = 0) or (Dots > 1) then
    Exit;
  if (P <= Length(T)) and (T[P] in ['e', 'E']) then
  begin
    Inc(P);
    if (P <= Length(T)) and (T[P] in ['+', '-']) then
      Inc(P);
    if (P > Length(T)) or not (T[P] in ['0'..'9']) then
      Exit;
    while (P <= Length(T)) and (T[P] in ['0'..'9']) do
      Inc(P);
    Dots := 1;
  end;
  if P <= Length(T) then
    Exit;
  if (Dots = 0) and TryStrToInt64(T, I) then
    Exit(True);
  Val(T, D, Code);
  if (Code <> 0) or IsNan(D) or IsInfinite(D) then
    Exit;
  IsFloat := True;
  Result := True;
end;

// Value as a number (vkInt or vkFloat); strings are converted.
function TVirtualMachine.NumOf(const Value: TKayteValue): TKayteValue;
var
  IsFloat: Boolean;
  I: Int64;
  D: Double;
begin
  case Value.Kind of
    vkInt, vkFloat: Result := Value;
    vkArr: raise Exception.CreateFmt('Runtime Error: cannot use %s as a number', [ToDisplayString(Value)]);
  else
    if not StrToNumber(Value.StrVal, IsFloat, I, D) then
      raise Exception.CreateFmt('Runtime Error: cannot convert "%s" to a number', [Value.StrVal]);
    if IsFloat then
      Result := MakeFloat(D)
    else
      Result := MakeInt(I);
  end;
end;

function TVirtualMachine.ToFloat(const Value: TKayteValue): Double;
var
  N: TKayteValue;
begin
  N := NumOf(Value);
  if N.Kind = vkInt then
    Result := N.IntVal
  else
    Result := N.FltVal;
end;

// A double rounded to a whole number (to even on .5, like VB's CInt).
function FloatToInt(D: Double): Int64;
begin
  if (D >= 9223372036854775807.0) or (D < -9223372036854775808.0) then
    raise Exception.Create('Runtime Error: overflow (the number is too large for a whole number)');
  Result := Round(D);
end;

// - * / \ MOD (see rt_arith in the native runtime: whole numbers stay
// whole, / is exact, \ rounds its operands and truncates).
function TVirtualMachine.Arith(Op: TByteCodeOp; const A, B: TKayteValue): TKayteValue;
var
  X, Y: TKayteValue;
  P, Q: Int64;
  FP, FQ: Double;
begin
  X := NumOf(A);
  Y := NumOf(B);
  if Op = BC_IDIV then
  begin
    P := ToInt(X);
    Q := ToInt(Y);
    if Q = 0 then
      raise Exception.Create('Runtime Error: division by zero');
    if Q = -1 then
      Exit(MakeInt(Int64(0 - QWord(P))));
    Exit(MakeInt(P div Q));
  end;
  if (X.Kind = vkInt) and (Y.Kind = vkInt) then
  begin
    P := X.IntVal;
    Q := Y.IntVal;
    case Op of
      BC_SUB: Exit(MakeInt(Int64(QWord(P) - QWord(Q))));
      BC_MUL: Exit(MakeInt(Int64(QWord(P) * QWord(Q))));
      BC_DIV:
        begin
          if Q = 0 then
            raise Exception.Create('Runtime Error: division by zero');
          if Q = -1 then
            Exit(MakeInt(Int64(0 - QWord(P))));
          if P mod Q = 0 then
            Exit(MakeInt(P div Q));
          Exit(MakeFloat(Double(P) / Double(Q)));
        end;
    else // MOD
      if Q = 0 then
        raise Exception.Create('Runtime Error: division by zero');
      if Q = -1 then
        Exit(MakeInt(0));
      Exit(MakeInt(P mod Q));
    end;
  end;
  FP := ToFloat(X);
  FQ := ToFloat(Y);
  case Op of
    BC_SUB: Result := MakeFloat(FP - FQ);
    BC_MUL: Result := MakeFloat(FP * FQ);
    BC_DIV:
      begin
        if FQ = 0 then
          raise Exception.Create('Runtime Error: division by zero');
        Result := MakeFloat(FP / FQ);
      end;
  else // MOD: the remainder, with the sign of FP (C's fmod)
    if FQ = 0 then
      raise Exception.Create('Runtime Error: division by zero');
    Result := MakeFloat(c_fmod(FP, FQ));
  end;
end;

function TVirtualMachine.MakeStr(const Value: string): TKayteValue;
begin
  Result.Kind := vkStr;
  Result.FltVal := 0;
  Result.IntVal := 0;
  Result.StrVal := Value;
  Result.Arr := nil;
end;

function TKayteArray.Obj: TObject;
begin
  Result := Self;
end;

function TVirtualMachine.MakeArr(Count: Integer; const Tag: string): TKayteValue;
var
  A: TKayteArray;
  I: Integer;
begin
  A := TKayteArray.Create;
  SetLength(A.Items, Count);
  for I := 0 to Count - 1 do
    A.Items[I] := MakeInt(0);
  A.Tag := Tag;
  Result.Kind := vkArr;
  Result.FltVal := 0;
  Result.IntVal := 0;
  Result.StrVal := '';
  Result.Arr := A;
end;

function TVirtualMachine.ArrayOf(const Value: TKayteValue; const What: string): TKayteArray;
begin
  if Value.Kind <> vkArr then
    raise Exception.CreateFmt('Runtime Error: %s needs an array, got "%s"', [What, ToDisplayString(Value)]);
  Result := TKayteArray(Value.Arr.Obj);
end;

// Text of a value: arrays as [a, b, c], objects as <ClassName>. Shared by
// PRINT, & and comparisons - the native runtime's kv text matches it.
function TVirtualMachine.DisplayDepth(const Value: TKayteValue; Depth: Integer): string;
var
  A: TKayteArray;
  I: Integer;
begin
  case Value.Kind of
    vkInt: Result := IntToStr(Value.IntVal);
    vkStr: Result := Value.StrVal;
    vkFloat: Result := FormatNumber(Value.FltVal, False);
  else
    A := TKayteArray(Value.Arr.Obj);
    if A.Tag <> '' then
      Exit('<' + A.Tag + '>');
    if Depth > 16 then
      Exit('[...]');
    Result := '[';
    for I := 0 to High(A.Items) do
    begin
      if I > 0 then
        Result := Result + ', ';
      Result := Result + DisplayDepth(A.Items[I], Depth + 1);
    end;
    Result := Result + ']';
  end;
end;

function TVirtualMachine.ToDisplayString(const Value: TKayteValue): string;
begin
  Result := DisplayDepth(Value, 0);
end;

// A decimal integer, with optional spaces around it and a sign - like the
// native runtime's strtoll (FPC's own conversion also takes hex: "$1F", "x1").
function DecimalToInt(const S: string; out V: Int64): Boolean;
var
  T: string;
  I: Integer;
begin
  T := Trim(S);
  Result := False;
  if T = '' then
    Exit;
  I := 1;
  if T[1] in ['+', '-'] then
    Inc(I);
  if I > Length(T) then
    Exit;
  for I := I to Length(T) do
    if not (T[I] in ['0'..'9']) then
      Exit;
  Result := TryStrToInt64(T, V);
end;

// Writes S to stdout, keeping track of the column.
procedure TVirtualMachine.Output(const S: string);
var
  I: Integer;
begin
  Write(S);
  I := LastDelimiter(#10, S);
  if I > 0 then
    FColumn := Length(S) - I
  else
    Inc(FColumn, Length(S));
end;

function TVirtualMachine.ToInt(const Value: TKayteValue): Int64;
var
  N: TKayteValue;
begin
  N := NumOf(Value);
  if N.Kind = vkInt then
    Result := N.IntVal
  else
    Result := FloatToInt(N.FltVal);
end;

function TVirtualMachine.IsTruthy(const Value: TKayteValue): Boolean;
begin
  case Value.Kind of
    vkInt: Result := Value.IntVal <> 0;
    vkStr: Result := Value.StrVal <> '';
    vkFloat: Result := Value.FltVal <> 0;
  else
    Result := True; // an array or object
  end;
end;

// The array made by DIM a(d1, d2, ...): d1 + 1 elements, each an array for
// the next dimension (or 0 in the last).
function TVirtualMachine.NewArray(const Dims: array of Int64; Level: Integer): TKayteValue;
var
  I: Integer;
  A: TKayteArray;
begin
  if Dims[Level] < -1 then
    raise Exception.CreateFmt('Runtime Error: an array''s upper bound can''t be %d', [Dims[Level]]);
  Result := MakeArr(Dims[Level] + 1);
  if Level < High(Dims) then
  begin
    A := TKayteArray(Result.Arr.Obj);
    for I := 0 to High(A.Items) do
      A.Items[I] := NewArray(Dims, Level + 1);
  end;
end;

//----------------------------------------------------------------------
// QuickBASIC error numbers (ERR) and messages (ERROR n) - the same tables
// as K_qb_errors / K_qb_patterns in the native runtime.
//----------------------------------------------------------------------

type
  TQBError = record
    Code: Integer;
    Text: string;
  end;

const
  QBErrors: array[0..24] of TQBError = (
    (Code: 3; Text: 'RETURN without GOSUB'), (Code: 4; Text: 'Out of DATA'),
    (Code: 5; Text: 'Illegal function call'), (Code: 6; Text: 'Overflow'),
    (Code: 7; Text: 'Out of memory'), (Code: 9; Text: 'Subscript out of range'),
    (Code: 10; Text: 'Duplicate definition'), (Code: 11; Text: 'Division by zero'),
    (Code: 13; Text: 'Type mismatch'), (Code: 14; Text: 'Out of string space'),
    (Code: 20; Text: 'RESUME without error'), (Code: 52; Text: 'Bad file name or number'),
    (Code: 53; Text: 'File not found'), (Code: 54; Text: 'Bad file mode'),
    (Code: 55; Text: 'File already open'), (Code: 57; Text: 'Device I/O error'),
    (Code: 58; Text: 'File already exists'), (Code: 61; Text: 'Disk full'),
    (Code: 62; Text: 'Input past end of file'), (Code: 63; Text: 'Bad record number'),
    (Code: 64; Text: 'Bad file name'), (Code: 67; Text: 'Too many files'),
    (Code: 70; Text: 'Permission denied'), (Code: 75; Text: 'Path/File access error'),
    (Code: 76; Text: 'Path not found'));
  QBPatterns: array[0..19] of TQBError = (
    (Code: 11; Text: 'division by zero'), (Code: 6; Text: 'overflow'), (Code: 9; Text: 'out of range'),
    (Code: 4; Text: 'out of data'), (Code: 53; Text: 'no such file'), (Code: 70; Text: 'permission denied'),
    (Code: 62; Text: 'input past the end'), (Code: 52; Text: 'isn''t open'), (Code: 54; Text: 'is open for'),
    (Code: 55; Text: 'already open'), (Code: 63; Text: 'bad record number'),
    (Code: 67; Text: 'too many open files'), (Code: 3; Text: 'return without'), (Code: 7; Text: 'out of memory'),
    (Code: 13; Text: 'cannot convert'), (Code: 13; Text: 'got a string'), (Code: 13; Text: 'got a number'),
    (Code: 13; Text: 'needs an array'), (Code: 13; Text: 'cannot use'), (Code: 75; Text: 'can''t open'));

function QBErrorCode(const M: string): Integer;
var
  I, Code: Integer;
  L: string;
begin
  for I := 0 to High(QBErrors) do
    if SameText(M, QBErrors[I].Text) then
      Exit(QBErrors[I].Code);
  // ERROR n with a number QuickBASIC has no message for: "Error n"
  if (Copy(M, 1, 6) = 'Error ') and (Length(M) > 6) and (M[7] in ['0'..'9']) then
  begin
    I := 7;
    Code := 0;
    while (I <= Length(M)) and (M[I] in ['0'..'9']) do
    begin
      Code := Code * 10 + Ord(M[I]) - Ord('0');
      Inc(I);
    end;
    Exit(Code);
  end;
  L := LowerCase(M);
  for I := 0 to High(QBPatterns) do
    if Pos(QBPatterns[I].Text, L) > 0 then
      Exit(QBPatterns[I].Code);
  Result := 5;
end;

// Built-in functions (BC_BUILTIN). Strings are byte strings and positions
// 1-based, as in VB. The native runtime's rt_builtin mirrors this exactly.
function TVirtualMachine.CallBuiltin(Id: Integer; const Args: array of TKayteValue): TKayteValue;
var
  Str, F: string;
  St, L, I, K: Int64;
  D, E: Double;
  V: TKayteValue;
  IsFloat: Boolean;
  A: TKayteArray;
  Dims: array of Int64;
  Parts: TStringList;

  function S(Index: Integer): string;
  begin
    Result := ToDisplayString(Args[Index]);
  end;

  function N(Index: Integer): Int64;
  begin
    Result := ToInt(Args[Index]);
  end;

  procedure Fail(const Msg: string);
  begin
    raise Exception.Create('Runtime Error: ' + Msg);
  end;

  function NonNegative(Index: Integer; const Name: string): Int64;
  begin
    Result := N(Index);
    if Result < 0 then
      Fail(Name + ': the length can''t be negative');
  end;

  function Clip(V: Int64): Integer;
  begin
    if V > MaxInt then Result := MaxInt else Result := Integer(V);
  end;

begin
  case Id of
    BI_LEN:
      if Args[0].Kind = vkArr then
        Result := MakeInt(Length(ArrayOf(Args[0], 'LEN').Items))
      else
        Result := MakeInt(Length(S(0)));
    BI_LEFT:
      Result := MakeStr(Copy(S(0), 1, Clip(NonNegative(1, 'LEFT'))));
    BI_RIGHT:
      begin
        Str := S(0);
        L := NonNegative(1, 'RIGHT');
        if L > Length(Str) then
          L := Length(Str);
        Result := MakeStr(Copy(Str, Length(Str) - L + 1, L));
      end;
    BI_MID:
      begin
        Str := S(0);
        St := N(1);
        if St < 1 then
          Fail('MID: the start position must be 1 or more');
        if Length(Args) > 2 then
          L := NonNegative(2, 'MID')
        else
          L := MaxInt;
        Result := MakeStr(Copy(Str, Clip(St), Clip(L)));
      end;
    BI_UCASE: Result := MakeStr(UpperCase(S(0)));
    BI_LCASE: Result := MakeStr(LowerCase(S(0)));
    BI_TRIM, BI_LTRIM, BI_RTRIM:
      begin
        Str := S(0);
        St := 1;
        L := Length(Str);
        if Id <> BI_RTRIM then
          while (St <= L) and (Str[St] = ' ') do
            Inc(St);
        if Id <> BI_LTRIM then
          while (L >= St) and (Str[L] = ' ') do
            Dec(L);
        Result := MakeStr(Copy(Str, St, L - St + 1));
      end;
    BI_INSTR:
      begin
        if Length(Args) = 3 then
        begin
          St := N(0);
          Str := S(1);
          F := S(2);
        end
        else
        begin
          St := 1;
          Str := S(0);
          F := S(1);
        end;
        if St < 1 then
          Fail('INSTR: the start position must be 1 or more');
        if F = '' then
        begin
          if St <= Length(Str) + 1 then Result := MakeInt(St) else Result := MakeInt(0);
        end
        else if St > Length(Str) then
          Result := MakeInt(0)
        else
          Result := MakeInt(Pos(F, Str, Clip(St)));
      end;
    BI_REPLACE:
      begin
        Str := S(0);
        F := S(1);
        if F = '' then
          Result := MakeStr(Str)
        else
          Result := MakeStr(StringReplace(Str, F, S(2), [rfReplaceAll]));
      end;
    BI_STR: Result := MakeStr(S(0));
    BI_VAL:
      if Args[0].Kind in [vkInt, vkFloat] then
        Result := Args[0]
      else
      begin
        // Like VB's Val: leading spaces, an optional sign, then the longest
        // number (digits, a ".", an exponent); nothing numeric gives 0.
        Str := S(0);
        I := 1;
        while (I <= Length(Str)) and (Str[I] = ' ') do
          Inc(I);
        St := I;
        if (I <= Length(Str)) and (Str[I] in ['+', '-']) then
          Inc(I);
        K := 0; // digits
        while (I <= Length(Str)) and (Str[I] in ['0'..'9']) do
        begin
          Inc(I);
          Inc(K);
        end;
        if (I <= Length(Str)) and (Str[I] = '.') then
        begin
          Inc(I);
          while (I <= Length(Str)) and (Str[I] in ['0'..'9']) do
          begin
            Inc(I);
            Inc(K);
          end;
        end;
        L := I; // the end
        if (K > 0) and (I <= Length(Str)) and (Str[I] in ['e', 'E']) then
        begin
          Inc(I);
          if (I <= Length(Str)) and (Str[I] in ['+', '-']) then
            Inc(I);
          if (I <= Length(Str)) and (Str[I] in ['0'..'9']) then
          begin
            while (I <= Length(Str)) and (Str[I] in ['0'..'9']) do
              Inc(I);
            L := I;
          end;
        end;
        Result := MakeInt(0);
        if K > 0 then
        begin
          F := Copy(Str, St, Min(L - St, 63));
          if (F <> '') and (F[Length(F)] = '.') then
            SetLength(F, Length(F) - 1);
          if StrToNumber(F, IsFloat, K, D) then
            if IsFloat then
              Result := MakeFloat(D)
            else
              Result := MakeInt(K);
        end;
      end;
    BI_CHR:
      begin
        L := N(0);
        if (L < 0) or (L > 255) then
          Fail('CHR: the character code must be 0 to 255');
        Result := MakeStr(Chr(L));
      end;
    BI_ASC:
      begin
        Str := S(0);
        if Str = '' then
          Fail('ASC of an empty string');
        Result := MakeInt(Ord(Str[1]));
      end;
    BI_SPACE: Result := MakeStr(StringOfChar(' ', Clip(NonNegative(0, 'SPACE'))));
    BI_ABS:
      begin
        Result := NumOf(Args[0]);
        if Result.Kind = vkInt then
        begin
          if Result.IntVal < 0 then
            Result := MakeInt(Int64(0 - QWord(Result.IntVal)));
        end
        else
          Result := MakeFloat(Abs(Result.FltVal));
      end;
    BI_SGN: Result := MakeInt(Sign(ToFloat(Args[0])));
    BI_MIN, BI_MAX:
      begin
        Result := NumOf(Args[0]);
        for I := 1 to High(Args) do
        begin
          V := NumOf(Args[I]);
          if CompareValues(V, Result, BC_CMP_LT) and (Id = BI_MIN) or
             CompareValues(V, Result, BC_CMP_GT) and (Id = BI_MAX) then
            Result := V;
        end;
      end;
    BI_UBOUND, BI_LBOUND:
      begin
        A := ArrayOf(Args[0], 'UBOUND / LBOUND');
        if Length(Args) > 1 then L := N(1) else L := 1;
        if L < 1 then
          Fail('UBOUND / LBOUND: the dimension must be 1 or more');
        for I := 2 to L do
        begin
          if (Length(A.Items) = 0) or (A.Items[0].Kind <> vkArr) then
            Fail(Format('the array has fewer than %d dimensions', [L]));
          A := TKayteArray(A.Items[0].Arr.Obj);
        end;
        if Id = BI_UBOUND then
          Result := MakeInt(Length(A.Items) - 1)
        else
          Result := MakeInt(0);
      end;
    BI_ARRAY:
      begin
        Result := MakeArr(Length(Args));
        A := TKayteArray(Result.Arr.Obj);
        for I := 0 to High(Args) do
          A.Items[I] := Args[I];
      end;
    BI_JOIN:
      begin
        A := ArrayOf(Args[0], 'JOIN');
        if Length(Args) > 1 then F := S(1) else F := ' ';
        Str := '';
        for I := 0 to High(A.Items) do
        begin
          if I > 0 then
            Str := Str + F;
          Str := Str + ToDisplayString(A.Items[I]);
        end;
        Result := MakeStr(Str);
      end;
    BI_SPLIT:
      begin
        Str := S(0);
        if Length(Args) > 1 then F := S(1) else F := ' ';
        Parts := TStringList.Create;
        try
          if Str <> '' then
            if F = '' then
              Parts.Add(Str)
            else
            begin
              I := Pos(F, Str);
              while I > 0 do
              begin
                Parts.Add(Copy(Str, 1, I - 1));
                Delete(Str, 1, I - 1 + Length(F));
                I := Pos(F, Str);
              end;
              Parts.Add(Str);
            end;
          Result := MakeArr(Parts.Count);
          A := TKayteArray(Result.Arr.Obj);
          for I := 0 to Parts.Count - 1 do
            A.Items[I] := MakeStr(Parts[I]);
        finally
          Parts.Free;
        end;
      end;
    BI_TYPENAME:
      case Args[0].Kind of
        vkInt: Result := MakeStr('Integer');
        vkStr: Result := MakeStr('String');
        vkFloat: Result := MakeStr('Double');
      else
        A := TKayteArray(Args[0].Arr.Obj);
        if A.Tag <> '' then Result := MakeStr(A.Tag) else Result := MakeStr('Array');
      end;
    BI_ISARRAY:
      Result := MakeInt(Ord((Args[0].Kind = vkArr) and (TKayteArray(Args[0].Arr.Obj).Tag = '')));
    BI_NEWARRAY:
      begin
        SetLength(Dims, Length(Args));
        for I := 0 to High(Args) do
          Dims[I] := N(I);
        Result := NewArray(Dims, 0);
      end;
    BI_RESIZE:
      begin
        // REDIM PRESERVE: a new array of the new size, keeping the items.
        A := ArrayOf(Args[0], 'REDIM PRESERVE');
        L := N(1);
        if L < -1 then
          Fail(Format('an array''s upper bound can''t be %d', [L]));
        Result := MakeArr(L + 1);
        for I := 0 to L do
          if I <= High(A.Items) then
            TKayteArray(Result.Arr.Obj).Items[I] := A.Items[I];
      end;
    BI_NEWOBJECT: Result := MakeArr(N(1), S(0));
    BI_ISNUMERIC:
      case Args[0].Kind of
        vkInt, vkFloat: Result := MakeInt(1);
        vkStr: Result := MakeInt(Ord(StrToNumber(Args[0].StrVal, IsFloat, L, D)));
      else
        Result := MakeInt(0);
      end;
    BI_CINT: Result := MakeInt(N(0));
    BI_RND:
      begin
        if Length(Args) = 0 then
          Exit(MakeFloat(Random)); // RND: a fraction 0 <= x < 1
        L := N(0);
        if L < 1 then
          Fail('RND: the range must be 1 or more');
        Result := MakeInt(Random(L));
      end;
    BI_POW:
      begin
        // Whole numbers to a power >= 0 stay whole; anything else is a double.
        V := NumOf(Args[0]);
        Result := NumOf(Args[1]);
        // (also when the whole result wouldn't fit: 10 ^ 20 is 1E+20)
        if (V.Kind <> vkInt) or (Result.Kind <> vkInt) or (Result.IntVal < 0) or
           (Abs(c_pow(V.IntVal, Result.IntVal)) >= 9.2e18) then
        begin
          D := ToFloat(V);
          E := ToFloat(Result);
          if (D = 0) and (E < 0) then
            Fail('division by zero');
          if (D < 0) and (E <> Int(E)) then
            Fail('a negative number to a fractional power');
          Exit(MakeFloat(c_pow(D, E)));
        end;
        L := V.IntVal;
        K := Result.IntVal;
        begin
          St := 1;
          while K > 0 do
          begin
            if K and 1 = 1 then
              St := Int64(QWord(St) * QWord(L));
            L := Int64(QWord(L) * QWord(L));
            K := K shr 1;
          end;
          Result := MakeInt(St);
        end;
      end;
    BI_SQR:
      begin
        D := ToFloat(Args[0]);
        if D < 0 then
          Fail('SQR of a negative number');
        Result := MakeFloat(c_sqrt(D));
      end;
    BI_INT, BI_FIX:
      begin
        // INT: the whole number at or below (INT(-2.5) is -3); FIX: the
        // fraction cut off (FIX(-2.5) is -2).
        Result := NumOf(Args[0]);
        if Result.Kind = vkFloat then
        begin
          D := Int(Result.FltVal);
          if (Id = BI_INT) and (D > Result.FltVal) then
            D := D - 1;
          Result := MakeInt(FloatToInt(D));
        end;
      end;
    BI_CDBL: Result := MakeFloat(ToFloat(Args[0]));
    BI_ROUND:
      begin
        if Length(Args) > 1 then L := N(1) else L := 0;
        if (L < 0) or (L > 15) then
          Fail('ROUND: the number of decimals must be 0 to 15');
        Result := NumOf(Args[0]);
        if Result.Kind = vkFloat then
          if L = 0 then
            Result := MakeInt(FloatToInt(Result.FltVal))
          else
          begin
            D := c_pow(10, L);
            Result := MakeFloat(c_rint(Result.FltVal * D) / D);
          end;
      end;
    BI_SIN, BI_COS, BI_TAN, BI_ATN, BI_EXP, BI_LOG:
      begin
        D := ToFloat(Args[0]);
        if (Id = BI_LOG) and (D <= 0) then
          Fail('LOG of a number <= 0');
        case Id of
          BI_SIN: Result := MakeFloat(c_sin(D));
          BI_COS: Result := MakeFloat(c_cos(D));
          BI_TAN: Result := MakeFloat(c_tan(D));
          BI_ATN: Result := MakeFloat(c_atan(D));
          BI_EXP: Result := MakeFloat(c_exp(D));
        else
          Result := MakeFloat(c_log(D));
        end;
      end;
    BI_STRING:
      begin
        L := NonNegative(0, 'STRING$');
        if Args[1].Kind = vkStr then
        begin
          if Args[1].StrVal = '' then
            Fail('STRING$ needs a character');
          F := Args[1].StrVal[1];
        end
        else
        begin
          K := N(1);
          if (K < 0) or (K > 255) then
            Fail('STRING$: the character code must be 0 to 255');
          F := Chr(K);
        end;
        Result := MakeStr(StringOfChar(F[1], Clip(L)));
      end;
    BI_TIMER: Result := MakeInt(Trunc(Frac(Now) * 86400));
    BI_DATE: Result := MakeStr(FormatDateTime('mm"-"dd"-"yyyy', Now));
    BI_TIME: Result := MakeStr(FormatDateTime('hh":"nn":"ss', Now));
    BI_HEX: Result := MakeStr(IntToHex(QWord(N(0)), 1));
    BI_OCT:
      begin
        Str := '';
        L := N(0);
        repeat
          Str := Chr(Ord('0') + Integer(QWord(L) and 7)) + Str;
          L := Int64(QWord(L) shr 3);
        until L = 0;
        Result := MakeStr(Str);
      end;
    BI_SLEEP:
      begin
        Flush(System.Output);
        L := N(0);
        if L > 0 then
          Sleep(Clip(L) * 1000);
        Result := MakeInt(0);
      end;
    BI_INKEY: Result := MakeStr('');
    BI_USING: Result := FormatUsing(Args);
    BI_BAND: Result := MakeInt(N(0) and N(1));
    BI_BOR: Result := MakeInt(N(0) or N(1));
    BI_BXOR: Result := MakeInt(N(0) xor N(1));
    BI_BNOT: Result := MakeInt(not N(0));
    BI_ERRCODE: Result := MakeInt(QBErrorCode(S(0)));
    BI_ERRMSG:
      begin
        L := N(0);
        if (L < 1) or (L > 255) then
          Fail('Illegal function call'); // ERROR n needs 1 .. 255
        Result := MakeStr('Error ' + IntToStr(L));
        for I := 0 to High(QBErrors) do
          if QBErrors[I].Code = L then
            Result := MakeStr(QBErrors[I].Text);
      end;
    BI_MIDSET:
      begin
        // MID$(s$, start, len) = v$: s$ with part replaced, same length
        St := N(1);
        L := N(2);
        if St < 1 then
          Fail('Illegal function call');
        Str := S(0);
        F := S(3);
        K := Length(F);
        if (L >= 0) and (L < K) then
          K := L;
        if K > Length(Str) - (St - 1) then
          K := Length(Str) - (St - 1);
        for I := 1 to K do
          Str[St - 1 + I] := F[I];
        Result := MakeStr(Str);
      end;
    BI_LSET, BI_RSET:
      begin
        // LSET / RSET a$ = v$: v$ in a$'s length, padded with spaces
        Str := StringOfChar(' ', Length(S(0)));
        F := Copy(S(1), 1, Length(Str));
        if Id = BI_LSET then
          Move(F[1], Str[1], Length(F) * Ord(F <> ''))
        else if F <> '' then
          Move(F[1], Str[Length(Str) - Length(F) + 1], Length(F));
        Result := MakeStr(Str);
      end;
    BI_FOPEN, BI_FCLOSE, BI_FPRINT, BI_FREADLINE, BI_FREADFIELD, BI_EOF, BI_FREEFILE, BI_LOF, BI_KILL, BI_NAME,
    BI_FGET, BI_FPUT, BI_FSEEK, BI_FSEEKPOS, BI_FLOC, BI_FINPUTS, BI_MKI, BI_MKL, BI_MKS, BI_MKD, BI_CVI, BI_CVL,
    BI_CVS, BI_CVD:
      Result := FileBuiltin(Id, Args);
    BI_QBSTR:
      // QuickBASIC's STR$: a non-negative number gets a leading space.
      if (Args[0].Kind = vkInt) and (Args[0].IntVal >= 0) then
        Result := MakeStr(' ' + IntToStr(Args[0].IntVal))
      else if Args[0].Kind = vkFloat then
      begin
        if Args[0].FltVal >= 0 then
          Result := MakeStr(' ' + FormatNumber(Args[0].FltVal, True))
        else
          Result := MakeStr(FormatNumber(Args[0].FltVal, True));
      end
      else
        Result := MakeStr(S(0));
  else
    Fail(Format('unknown built-in function %d', [Id]));
  end;
end;


//----------------------------------------------------------------------
// PRINT USING (QuickBASIC) - mirrors using_format in kayte_native_rt.c
//----------------------------------------------------------------------

type
  TUsingField = record
    Kind: Char; // '!', '&', '\' or 'N'
    Len, Width: Integer;
    LeadPlus, TrailPlus, TrailMinus, Stars, Dollar, Comma, Point: Boolean;
    Before, Decs, Expn: Integer;
  end;

// The field that starts at F[P] (1-based), if one does.
function UsingField(const F: string; P: Integer; out Fd: TUsingField): Boolean;
var
  N, I, Q: Integer;
  C, C1: Char;

  function At(K: Integer): Char;
  begin
    if (K >= 1) and (K <= N) then Result := F[K] else Result := #0;
  end;

begin
  FillChar(Fd, SizeOf(Fd), 0);
  N := Length(F);
  C := At(P);
  C1 := At(P + 1);
  Result := True;
  if (C = '!') or (C = '&') then
  begin
    Fd.Kind := C;
    Fd.Len := 1;
    Exit;
  end;
  if C = '\' then
  begin
    Q := P + 1;
    while At(Q) = ' ' do
      Inc(Q);
    if At(Q) = '\' then
    begin
      Fd.Kind := '\';
      Fd.Len := Q - P + 1;
      Fd.Width := Fd.Len;
      Exit;
    end;
    Exit(False);
  end;
  if not ((C = '#') or ((C = '.') and (C1 = '#')) or ((C = '+') and (C1 in ['#', '.', '$', '*'])) or
          ((C = '*') and (C1 = '*')) or ((C = '$') and (C1 = '$'))) then
    Exit(False);
  Fd.Kind := 'N';
  I := P;
  if At(I) = '+' then
  begin
    Fd.LeadPlus := True;
    Inc(I);
  end;
  if (At(I) = '*') and (At(I + 1) = '*') and (At(I + 2) = '$') then
  begin
    Fd.Stars := True;
    Fd.Dollar := True;
    Inc(Fd.Before, 2);
    Inc(I, 3);
  end
  else if (At(I) = '*') and (At(I + 1) = '*') then
  begin
    Fd.Stars := True;
    Inc(Fd.Before, 2);
    Inc(I, 2);
  end
  else if (At(I) = '$') and (At(I + 1) = '$') then
  begin
    Fd.Dollar := True;
    Inc(Fd.Before);
    Inc(I, 2);
  end;
  while At(I) in ['#', ','] do
  begin
    if At(I) = '#' then Inc(Fd.Before) else Fd.Comma := True;
    Inc(I);
  end;
  if At(I) = '.' then
  begin
    Fd.Point := True;
    Inc(I);
    while At(I) = '#' do
    begin
      Inc(Fd.Decs);
      Inc(I);
    end;
  end;
  if Copy(F, I, 4) = '^^^^' then
  begin
    if At(I + 4) = '^' then Fd.Expn := 5 else Fd.Expn := 4;
    Inc(I, Fd.Expn);
  end;
  if (At(I) = '+') and not Fd.LeadPlus then
  begin
    Fd.TrailPlus := True;
    Inc(I);
  end
  else if At(I) = '-' then
  begin
    Fd.TrailMinus := True;
    Inc(I);
  end;
  Fd.Len := I - P;
end;

// "%.*f" of A >= 0, split into its whole part and its fraction.
procedure FixedParts(A: Double; Decs: Integer; out IP, FP: string);
var
  Buf: array[0..399] of Char;
  S: string;
  D: Integer;
begin
  c_snprintf(@Buf[0], SizeOf(Buf), '%.*f', LongInt(Decs), A);
  S := StrPas(@Buf[0]);
  D := 1;
  while (D <= Length(S)) and (S[D] in ['0'..'9']) do
    Inc(D);
  IP := Copy(S, 1, D - 1);
  if D <= Length(S) then
    FP := Copy(S, D + 1, MaxInt) // after the decimal point, whatever the locale makes it
  else
    FP := '';
end;

function UsingNumber(const Fd: TUsingField; V: Double): string;
var
  Neg: Boolean;
  A: Double;
  IntD, E, Tries, J, EDigits: Integer;
  IP, FP, Body, Grouped, Text, ES: string;
begin
  Neg := V < 0;
  A := Abs(V);
  if Fd.Expn > 0 then
  begin
    if Fd.LeadPlus or Fd.TrailPlus or Fd.TrailMinus then IntD := Fd.Before else IntD := Fd.Before - 1;
    if IntD < 0 then
      IntD := 0;
    E := 0;
    if A <> 0 then
      E := Trunc(c_floor(c_log10(A))) - (IntD - 1);
    for Tries := 0 to 1 do
    begin
      if A = 0 then
        FixedParts(0, Fd.Decs, IP, FP)
      else
        FixedParts(A / c_pow(10, E), Fd.Decs, IP, FP);
      if ((IntD > 0) and (Length(IP) > IntD)) or ((IntD = 0) and ((Length(IP) > 1) or (IP <> '0'))) then
        Inc(E)
      else
        Break;
    end;
    if IntD = 0 then
      IP := '';
    EDigits := Fd.Expn - 2;
    ES := IntToStr(Abs(E));
    while Length(ES) < EDigits do
      ES := '0' + ES;
    Body := '';
    if Fd.Dollar then Body := '$';
    Body := Body + IP;
    if Fd.Point then Body := Body + '.';
    Body := Body + FP + 'E';
    if E < 0 then Body := Body + '-' else Body := Body + '+';
    Body := Body + ES;
  end
  else
  begin
    FixedParts(A, Fd.Decs, IP, FP);
    if (Fd.Before = 0) and (IP = '0') then
      IP := '';
    Grouped := '';
    for J := 1 to Length(IP) do
    begin
      if Fd.Comma and (J > 1) and ((Length(IP) - J + 1) mod 3 = 0) then
        Grouped := Grouped + ',';
      Grouped := Grouped + IP[J];
    end;
    Body := '';
    if Fd.Dollar then Body := '$';
    Body := Body + Grouped;
    if Fd.Point then Body := Body + '.';
    Body := Body + FP;
    // -0.00 shows as 0.00
    if Neg then
    begin
      Neg := False;
      for J := 1 to Length(Body) do
        if not (Body[J] in ['$', '0', '.', ',']) then
          Neg := True;
    end;
  end;
  Text := '';
  if Fd.LeadPlus then
  begin
    if Neg then Text := '-' else Text := '+';
  end
  else if Neg and not Fd.TrailPlus and not Fd.TrailMinus then
    Text := '-';
  Text := Text + Body;
  if Fd.TrailPlus then
  begin
    if Neg then Text := Text + '-' else Text := Text + '+';
  end
  else if Fd.TrailMinus then
  begin
    if Neg then Text := Text + '-' else Text := Text + ' ';
  end;
  if Length(Text) > Fd.Len then
    Result := '%' + Text // too wide for the field
  else if Fd.Stars then
    Result := StringOfChar('*', Fd.Len - Length(Text)) + Text
  else
    Result := StringOfChar(' ', Fd.Len - Length(Text)) + Text;
end;

// PRINT USING fmt; items: the formatted text. The format is used again
// from the start when there are more items than fields.
function TVirtualMachine.FormatUsing(const Args: array of TKayteValue): TKayteValue;
var
  F, OutS, T: string;
  Len, Pos_, Q, J, C: Integer;
  Fd: TUsingField;
  Any: Boolean;
begin
  F := ToDisplayString(Args[0]);
  Len := Length(F);
  Any := False;
  Q := 1;
  while (Q <= Len) and not Any do
  begin
    if F[Q] = '_' then
      Inc(Q)
    else
      Any := UsingField(F, Q, Fd);
    Inc(Q);
  end;
  if not Any then
    raise Exception.Create('Runtime Error: PRINT USING: the format has no field (#, !, &, \ \)');
  OutS := '';
  Pos_ := 1;
  for J := 1 to High(Args) do
  begin
    repeat
      if Pos_ > Len then
        Pos_ := 1;
      if (F[Pos_] = '_') and (Pos_ < Len) then
      begin
        OutS := OutS + F[Pos_ + 1];
        Inc(Pos_, 2);
        Continue;
      end;
      if UsingField(F, Pos_, Fd) then
        Break;
      OutS := OutS + F[Pos_];
      Inc(Pos_);
    until False;
    Inc(Pos_, Fd.Len);
    if Fd.Kind = 'N' then
    begin
      if not (Args[J].Kind in [vkInt, vkFloat]) then
        raise Exception.Create('Runtime Error: PRINT USING: a number field (#) got a string');
      OutS := OutS + UsingNumber(Fd, ToFloat(Args[J]));
    end
    else
    begin
      if Args[J].Kind <> vkStr then
        raise Exception.Create('Runtime Error: PRINT USING: a string field (! & \ \) got a number');
      T := Args[J].StrVal;
      case Fd.Kind of
        '!': if T <> '' then OutS := OutS + T[1] else OutS := OutS + ' ';
        '&': OutS := OutS + T;
      else
        for C := 1 to Fd.Width do
          if C <= Length(T) then OutS := OutS + T[C] else OutS := OutS + ' ';
      end;
    end;
  end;
  // the literal text after the last field
  while (Pos_ <= Len) and not UsingField(F, Pos_, Fd) do
    if (F[Pos_] = '_') and (Pos_ < Len) then
    begin
      OutS := OutS + F[Pos_ + 1];
      Inc(Pos_, 2);
    end
    else
    begin
      OutS := OutS + F[Pos_];
      Inc(Pos_);
    end;
  Result := MakeStr(OutS);
end;

//----------------------------------------------------------------------
// Files (QuickBASIC OPEN / PRINT # / INPUT # ...) - mirrors the native
// runtime, through the same C stdio calls.
//----------------------------------------------------------------------

const
  ModeNames: array[0..4] of string = ('INPUT', 'OUTPUT', 'APPEND', 'BINARY', 'RANDOM');

procedure TVirtualMachine.NeedOpen(Num: Int64);
begin
  if (Num < 1) or (Num > KMaxFiles) or (FFiles[Num] = nil) then
    raise Exception.CreateFmt('Runtime Error: file #%d isn''t open', [Num]);
end;

// A text file, for reading (Input) or writing.
function TVirtualMachine.NeedFile(Num: Int64; Input: Boolean): Pointer;
begin
  NeedOpen(Num);
  if (FFileMode[Num] >= 3) or (Input <> (FFileMode[Num] = 0)) then
    raise Exception.CreateFmt('Runtime Error: file #%d is open for %s', [Num, ModeNames[FFileMode[Num]]]);
  Result := FFiles[Num];
end;

//---- BINARY / RANDOM: GET and PUT (mirrors the native runtime) ----------

// The next code of layout L at P: its letter and size ('' at the end of
// the layout or of a {...}).
function LayCode(const L: string; var P: Integer; out Size: Int64): Char;
begin
  while (P <= Length(L)) and (L[P] = ',') do
    Inc(P);
  if (P > Length(L)) or (L[P] = '}') then
    Exit(#0);
  Result := L[P];
  Inc(P);
  Size := 0;
  while (P <= Length(L)) and (L[P] in ['0'..'9']) do
  begin
    Size := Size * 10 + Ord(L[P]) - Ord('0');
    Inc(P);
  end;
end;

procedure PutLE(var B: string; V: QWord; NBytes: Integer);
var
  K: Integer;
begin
  for K := 0 to NBytes - 1 do
    B := B + Chr((V shr (8 * K)) and $FF);
end;

function GetLE(const D: string; Off, NBytes: Integer): QWord;
var
  K: Integer;
begin
  Result := 0;
  for K := 0 to NBytes - 1 do
    if Off + K <= Length(D) then
      Result := Result or (QWord(Ord(D[Off + K])) shl (8 * K));
end;

procedure PutFloat(var B: string; D: Double; NBytes: Integer);
var
  F: Single;
  U32: LongWord;
  U64: QWord;
begin
  if NBytes = 4 then
  begin
    if Abs(D) > 3.4028234663852886e38 then
      raise Exception.Create('Runtime Error: overflow: too large for a SINGLE');
    F := D;
    Move(F, U32, 4);
    PutLE(B, U32, 4);
  end
  else
  begin
    Move(D, U64, 8);
    PutLE(B, U64, 8);
  end;
end;

procedure TVirtualMachine.PutValue(var B: string; const L: string; var P: Integer; const V: TKayteValue);
var
  Code: Char;
  Size, I, Lim, K: Int64;
  A: TKayteArray;
  T: string;
begin
  Code := LayCode(L, P, Size);
  case Code of
    '{':
      begin
        if V.Kind <> vkArr then
          raise Exception.Create('Runtime Error: GET / PUT: a TYPE variable holds no record');
        A := TKayteArray(V.Arr.Obj);
        K := 0;
        while (P <= Length(L)) and (L[P] <> '}') do
        begin
          if L[P] = ',' then
          begin
            Inc(P);
            Continue;
          end;
          if K > High(A.Items) then
            raise Exception.Create('Runtime Error: internal error: the record has fewer fields than its TYPE');
          PutValue(B, L, P, A.Items[K]);
          Inc(K);
        end;
        Inc(P);
      end;
    'I':
      begin
        I := ToInt(V);
        if Size < 8 then
        begin
          Lim := Int64(1) shl (8 * Size - 1);
          if (I < -Lim) or (I >= Lim) then
            raise Exception.CreateFmt('Runtime Error: overflow: %d doesn''t fit in %d bytes', [I, Size]);
        end;
        PutLE(B, QWord(I), Size);
      end;
    'F': PutFloat(B, ToFloat(V), Size);
    'S', 'V', '?':
      if (Code = '?') and (V.Kind = vkInt) then
        PutLE(B, QWord(V.IntVal), 8)
      else if (Code = '?') and (V.Kind = vkFloat) then
        PutFloat(B, V.FltVal, 8)
      else
      begin
        T := ToDisplayString(V);
        if Code = 'S' then
        begin
          for K := 1 to Size do
            if K <= Length(T) then B := B + T[K] else B := B + ' ';
        end
        else
          B := B + T;
      end;
  else
    raise Exception.Create('Runtime Error: internal error: bad GET / PUT layout');
  end;
end;

function TVirtualMachine.LaySize(const L: string; var P: Integer; const Cur: TKayteValue): Int64;
var
  Code: Char;
  Size, K: Int64;
  Item: TKayteValue;
begin
  Code := LayCode(L, P, Size);
  case Code of
    #0: Result := 0;
    '{':
      begin
        Result := 0;
        K := 0;
        while (P <= Length(L)) and (L[P] <> '}') do
        begin
          if L[P] = ',' then
          begin
            Inc(P);
            Continue;
          end;
          if (Cur.Kind = vkArr) and (K <= High(TKayteArray(Cur.Arr.Obj).Items)) then
            Item := TKayteArray(Cur.Arr.Obj).Items[K]
          else
            Item := MakeInt(0);
          Inc(K);
          Result := Result + LaySize(L, P, Item);
        end;
        Inc(P);
      end;
    'V': if Cur.Kind = vkStr then Result := Length(Cur.StrVal) else Result := 0;
    '?': if Cur.Kind = vkStr then Result := Length(Cur.StrVal) else Result := 8;
  else
    Result := Size;
  end;
end;

// A float read from 4 bytes, rounded to a SINGLE's 7 digits.
function TVirtualMachine.MkSingle(F: Single): TKayteValue;
var
  Buf: array[0..31] of Char;
  IsFloat: Boolean;
  I: Int64;
  D: Double;
begin
  c_snprintf(@Buf[0], SizeOf(Buf), '%.7g', Double(F));
  if StrToNumber(StrPas(@Buf[0]), IsFloat, I, D) then
  begin
    if IsFloat then Result := MakeFloat(D) else Result := MakeInt(I);
  end
  else
    Result := MakeFloat(F);
end;

function TVirtualMachine.GetValue(const D: string; var Off: Integer; const L: string; var P: Integer;
  const Cur: TKayteValue): TKayteValue;
var
  Code: Char;
  Size, K, Len: Int64;
  U: QWord;
  U32: LongWord;
  F: Single;
  X: Double;
  A: TKayteArray;
begin
  Code := LayCode(L, P, Size);
  case Code of
    '{':
      begin
        if Cur.Kind <> vkArr then
          raise Exception.Create('Runtime Error: GET / PUT: a TYPE variable holds no record');
        A := TKayteArray(Cur.Arr.Obj);
        K := 0;
        while (P <= Length(L)) and (L[P] <> '}') do
        begin
          if L[P] = ',' then
          begin
            Inc(P);
            Continue;
          end;
          A.Items[K] := GetValue(D, Off, L, P, A.Items[K]);
          Inc(K);
        end;
        Inc(P);
        Result := Cur;
      end;
    'I':
      begin
        U := GetLE(D, Off, Size);
        Inc(Off, Size);
        if (Size < 8) and (((U shr (8 * Size - 1)) and 1) = 1) then
          U := U or (not QWord(0) shl (8 * Size)); // sign-extend
        Result := MakeInt(Int64(U));
      end;
    'F':
      begin
        U := GetLE(D, Off, Size);
        Inc(Off, Size);
        if Size = 4 then
        begin
          U32 := LongWord(U);
          Move(U32, F, 4);
          Result := MkSingle(F);
        end
        else
        begin
          Move(U, X, 8);
          if IsNan(X) or IsInfinite(X) then
            raise Exception.Create('Runtime Error: GET: not a valid DOUBLE in the file');
          Result := MakeFloat(X);
        end;
      end;
  else // S, V, ?
    if (Code = '?') and (Cur.Kind <> vkStr) then
    begin
      U := GetLE(D, Off, 8);
      Inc(Off, 8);
      if Cur.Kind = vkFloat then
      begin
        Move(U, X, 8);
        if IsNan(X) or IsInfinite(X) then
          raise Exception.Create('Runtime Error: GET: not a valid DOUBLE in the file');
        Exit(MakeFloat(X));
      end;
      Exit(MakeInt(Int64(U)));
    end;
    if Code = 'S' then
      Len := Size
    else if Cur.Kind = vkStr then
      Len := Length(Cur.StrVal)
    else
      Len := 0;
    Result := MakeStr(Copy(D, Off, Len) + StringOfChar(#0, Max(0, Off + Len - 1 - Length(D))));
    Inc(Off, Len);
  end;
end;

// Where a GET / PUT goes: RANDOM - record Pos (or the next one), BINARY -
// byte Pos (or the current position).
function TVirtualMachine.BinSeek(Num, Pos: Int64; out RecLen: Int64): Pointer;
var
  Rec: Int64;
begin
  NeedOpen(Num);
  Result := FFiles[Num];
  if FFileMode[Num] < 3 then
    raise Exception.CreateFmt('Runtime Error: GET / PUT need a BINARY or RANDOM file (file #%d is open for %s)',
      [Num, ModeNames[FFileMode[Num]]]);
  if FFileMode[Num] = 4 then RecLen := FFileLen[Num] else RecLen := 0;
  if FFileMode[Num] = 4 then
  begin
    if Pos < 0 then Rec := FFileRec[Num] + 1 else Rec := Pos;
    if Rec < 1 then
      raise Exception.CreateFmt('Runtime Error: bad record number %d', [Rec]);
    FFileRec[Num] := Rec;
    c_fseek(Result, (Rec - 1) * FFileLen[Num], 0);
  end
  else if Pos >= 0 then
  begin
    if Pos < 1 then
      raise Exception.CreateFmt('Runtime Error: bad file position %d', [Pos]);
    c_fseek(Result, Pos - 1, 0);
  end
  else
    c_fseek(Result, 0, 1); // between a read and a write
end;

// MKI$ / MKL$ / MKS$ / MKD$ and CVI / CVL / CVS / CVD.
function TVirtualMachine.MkCv(Id: Integer; const V: TKayteValue): TKayteValue;
const
  Codes: array[0..3] of string = ('I2', 'I4', 'F4', 'F8');
  Sizes: array[0..3] of Integer = (2, 4, 4, 8);
  Letters = 'ILSD';
var
  Which, P, Off: Integer;
  B, T: string;
begin
  if Id >= BI_CVI then Which := Id - BI_CVI else Which := Id - BI_MKI;
  P := 1;
  if Id < BI_CVI then
  begin
    B := '';
    PutValue(B, Codes[Which], P, V);
    Exit(MakeStr(B));
  end;
  T := ToDisplayString(V);
  if Length(T) < Sizes[Which] then
    raise Exception.CreateFmt('Runtime Error: CV%s needs a string of at least %d bytes', [Letters[Which + 1], Sizes[Which]]);
  Off := 1;
  Result := GetValue(T, Off, Codes[Which], P, MakeInt(0));
end;

procedure TVirtualMachine.FileOut(Num: Int64; const S: string);
var
  FH: Pointer;
  I: Integer;
begin
  FH := NeedFile(Num, False);
  if S <> '' then
    c_fwrite(@S[1], 1, Length(S), FH);
  for I := 1 to Length(S) do
    if S[I] = #10 then FFileCol[Num] := 0 else Inc(FFileCol[Num]);
end;

procedure TVirtualMachine.FilePrint(Num: Int64; Kind: Integer; const V: TKayteValue);
var
  C: Int64;
begin
  case Kind of
    QP_VALUE, QP_TEXT:
      if (Kind = QP_VALUE) and (V.Kind = vkInt) then
      begin
        if V.IntVal >= 0 then FileOut(Num, ' ' + IntToStr(V.IntVal) + ' ')
        else FileOut(Num, IntToStr(V.IntVal) + ' ');
      end
      else if (Kind = QP_VALUE) and (V.Kind = vkFloat) then
      begin
        if V.FltVal >= 0 then FileOut(Num, ' ' + FormatNumber(V.FltVal, True) + ' ')
        else FileOut(Num, FormatNumber(V.FltVal, True) + ' ');
      end
      else
        FileOut(Num, ToDisplayString(V));
    QP_COMMA:
      begin
        NeedFile(Num, False);
        FileOut(Num, StringOfChar(' ', 14 - FFileCol[Num] mod 14));
      end;
    QP_NEWLINE: FileOut(Num, #10);
    QP_TAB:
      begin
        NeedFile(Num, False);
        C := ToInt(V) - 1;
        if C < FFileCol[Num] then
          FileOut(Num, #10);
        if C > FFileCol[Num] then
          FileOut(Num, StringOfChar(' ', C - FFileCol[Num]));
      end;
    QP_SPC:
      begin
        C := ToInt(V);
        if C > 0 then
          FileOut(Num, StringOfChar(' ', C));
      end;
  end;
end;

function FPeek(FH: Pointer): LongInt;
begin
  Result := c_fgetc(FH);
  if Result <> -1 then
    c_ungetc(Result, FH);
end;

function TVirtualMachine.FileLine(Num: Int64): string;
var
  FH: Pointer;
  C: LongInt;
begin
  FH := NeedFile(Num, True);
  if FPeek(FH) = -1 then
    raise Exception.CreateFmt('Runtime Error: input past the end of file #%d', [Num]);
  Result := '';
  repeat
    C := c_fgetc(FH);
    if (C = -1) or (C = 10) then
      Break;
    Result := Result + Chr(C);
  until False;
  if (Result <> '') and (Result[Length(Result)] = #13) then
    SetLength(Result, Length(Result) - 1);
end;

function TVirtualMachine.FileField(Num: Int64): string;
var
  FH: Pointer;
  C: LongInt;
begin
  FH := NeedFile(Num, True);
  repeat
    C := FPeek(FH);
    if C in [32, 9, 13, 10] then
      c_fgetc(FH)
    else
      Break;
  until False;
  if C = -1 then
    raise Exception.CreateFmt('Runtime Error: input past the end of file #%d', [Num]);
  Result := '';
  if C = 34 then
  begin
    c_fgetc(FH);
    repeat
      C := c_fgetc(FH);
      if (C = -1) or (C = 34) then
        Break;
      Result := Result + Chr(C);
    until False;
    repeat
      C := FPeek(FH);
      if C in [32, 9, 13] then
        c_fgetc(FH)
      else
        Break;
    until False;
    if (C = 44) or (C = 10) then
      c_fgetc(FH);
  end
  else
  begin
    repeat
      C := c_fgetc(FH);
      if (C = -1) or (C = 44) or (C = 10) then
        Break;
      Result := Result + Chr(C);
    until False;
    while (Result <> '') and (Result[Length(Result)] in [' ', #9, #13]) do
      SetLength(Result, Length(Result) - 1);
  end;
end;

function TVirtualMachine.FileBuiltin(Id: Integer; const Args: array of TKayteValue): TKayteValue;
var
  Num, Mode, J, RecLen, Pos_: Int64;
  Data: string;
  P, Off: Integer;
  Name, Name2: string;
  FH: Pointer;
  Here, Size: PtrInt;
  Bad: LongInt;
begin
  Result := MakeInt(0);
  case Id of
    BI_FOPEN:
      begin
        Num := ToInt(Args[2]);
        Mode := ToInt(Args[1]);
        if (Num < 1) or (Num > KMaxFiles) then
          raise Exception.CreateFmt('Runtime Error: file numbers are 1 to %d', [KMaxFiles]);
        if FFiles[Num] <> nil then
          raise Exception.CreateFmt('Runtime Error: file #%d is already open', [Num]);
        if Length(Args) > 3 then RecLen := ToInt(Args[3]) else RecLen := 128;
        if (Mode = 4) and ((RecLen < 1) or (RecLen > 32767)) then
          raise Exception.Create('Runtime Error: OPEN ... LEN: the record length must be 1 to 32767');
        Name := ToDisplayString(Args[0]);
        case Mode of
          0: FH := c_fopen(PChar(Name), 'rb');
          1: FH := c_fopen(PChar(Name), 'wb');
          2: FH := c_fopen(PChar(Name), 'ab');
        else
          // BINARY / RANDOM: read and write, created if missing
          FH := c_fopen(PChar(Name), 'r+b');
          if FH = nil then
            FH := c_fopen(PChar(Name), 'w+b');
        end;
        if FH = nil then
          raise Exception.CreateFmt('Runtime Error: can''t open "%s": %s', [Name, CErrorText]);
        FFiles[Num] := FH;
        FFileMode[Num] := Mode;
        FFileCol[Num] := 0;
        FFileLen[Num] := RecLen;
        FFileRec[Num] := 0;
      end;
    BI_FCLOSE:
      begin
        Num := ToInt(Args[0]);
        for J := 1 to KMaxFiles do
          if ((Num = 0) or (J = Num)) and (FFiles[J] <> nil) then
          begin
            c_fclose(FFiles[J]);
            FFiles[J] := nil;
          end;
      end;
    BI_FPRINT:
      if Length(Args) > 2 then
        FilePrint(ToInt(Args[0]), ToInt(Args[1]), Args[2])
      else
        FilePrint(ToInt(Args[0]), ToInt(Args[1]), MakeInt(0));
    BI_FREADLINE: Result := MakeStr(FileLine(ToInt(Args[0])));
    BI_FREADFIELD: Result := MakeStr(FileField(ToInt(Args[0])));
    BI_EOF:
      begin
        Num := ToInt(Args[0]);
        if (Num < 1) or (Num > KMaxFiles) or (FFiles[Num] = nil) then
          raise Exception.CreateFmt('Runtime Error: file #%d isn''t open', [Num]);
        if FFileMode[Num] >= 3 then
        begin
          // past the last byte / the last record read
          FH := FFiles[Num];
          Here := c_ftell(FH);
          c_fseek(FH, 0, 2);
          Size := c_ftell(FH);
          c_fseek(FH, Here, 0);
          if FFileMode[Num] = 4 then
            Result := MakeInt(Ord(FFileRec[Num] * FFileLen[Num] >= Size))
          else
            Result := MakeInt(Ord(Here >= Size));
        end
        else
          Result := MakeInt(Ord((FFileMode[Num] <> 0) or (FPeek(FFiles[Num]) = -1)));
      end;
    BI_FGET, BI_FPUT:
      begin
        Num := ToInt(Args[0]);
        FH := BinSeek(Num, ToInt(Args[1]), RecLen);
        Name := Args[3].StrVal; // the layout
        P := 1;
        if Id = BI_FPUT then
        begin
          Data := '';
          PutValue(Data, Name, P, Args[2]);
          if (RecLen > 0) and (Length(Data) > RecLen) then
            raise Exception.CreateFmt('Runtime Error: PUT: the record (%d bytes) is longer than the file''s LEN = %d',
              [Length(Data), RecLen]);
          if Data <> '' then
            c_fwrite(@Data[1], 1, Length(Data), FH);
        end
        else
        begin
          Size := LaySize(Name, P, Args[2]);
          if (RecLen > 0) and (Size > RecLen) then
            raise Exception.CreateFmt('Runtime Error: GET: the record (%d bytes) is longer than the file''s LEN = %d',
              [Size, RecLen]);
          SetLength(Data, Size);
          if Size > 0 then
          begin
            FillChar(Data[1], Size, 0); // past the end: zeros
            c_fread(@Data[1], 1, Size, FH);
          end;
          P := 1;
          Off := 1;
          Result := GetValue(Data, Off, Name, P, Args[2]);
        end;
      end;
    BI_FSEEK:
      begin
        Num := ToInt(Args[0]);
        Pos_ := ToInt(Args[1]);
        NeedOpen(Num);
        if Pos_ < 1 then
          raise Exception.CreateFmt('Runtime Error: bad file position %d', [Pos_]);
        if FFileMode[Num] = 4 then
        begin
          FFileRec[Num] := Pos_ - 1;
          c_fseek(FFiles[Num], (Pos_ - 1) * FFileLen[Num], 0);
        end
        else
          c_fseek(FFiles[Num], Pos_ - 1, 0);
      end;
    BI_FSEEKPOS, BI_FLOC:
      begin
        Num := ToInt(Args[0]);
        NeedOpen(Num);
        if FFileMode[Num] = 4 then
          Result := MakeInt(FFileRec[Num] + Ord(Id = BI_FSEEKPOS))
        else
          Result := MakeInt(c_ftell(FFiles[Num]) + Ord(Id = BI_FSEEKPOS));
      end;
    BI_FINPUTS:
      begin
        Pos_ := ToInt(Args[0]);
        Num := ToInt(Args[1]);
        NeedOpen(Num);
        if FFileMode[Num] in [1, 2] then
          raise Exception.CreateFmt('Runtime Error: file #%d is open for %s', [Num, ModeNames[FFileMode[Num]]]);
        if Pos_ < 0 then
          raise Exception.Create('Runtime Error: INPUT$: the count can''t be negative');
        SetLength(Data, Pos_);
        if (Pos_ > 0) and (c_fread(@Data[1], 1, Pos_, FFiles[Num]) < PtrUInt(Pos_)) then
          raise Exception.CreateFmt('Runtime Error: input past the end of file #%d', [Num]);
        Result := MakeStr(Data);
      end;
    BI_MKI, BI_MKL, BI_MKS, BI_MKD, BI_CVI, BI_CVL, BI_CVS, BI_CVD:
      Result := MkCv(Id, Args[0]);
    BI_FREEFILE:
      begin
        for J := 1 to KMaxFiles do
          if FFiles[J] = nil then
            Exit(MakeInt(J));
        raise Exception.Create('Runtime Error: too many open files');
      end;
    BI_LOF:
      begin
        Num := ToInt(Args[0]);
        if (Num < 1) or (Num > KMaxFiles) or (FFiles[Num] = nil) then
          raise Exception.CreateFmt('Runtime Error: file #%d isn''t open', [Num]);
        FH := FFiles[Num];
        c_fflush(FH);
        Here := c_ftell(FH);
        c_fseek(FH, 0, 2);
        Size := c_ftell(FH);
        c_fseek(FH, Here, 0);
        Result := MakeInt(Size);
      end;
    BI_KILL, BI_NAME:
      begin
        Name := ToDisplayString(Args[0]);
        if Id = BI_KILL then
          Bad := c_remove(PChar(Name))
        else
        begin
          Name2 := ToDisplayString(Args[1]);
          Bad := c_rename(PChar(Name), PChar(Name2));
        end;
        if Bad <> 0 then
          if Id = BI_KILL then
            raise Exception.CreateFmt('Runtime Error: can''t delete "%s": %s', [Name, CErrorText])
          else
            raise Exception.CreateFmt('Runtime Error: can''t rename "%s": %s', [Name, CErrorText]);
      end;
  end;
end;

procedure TVirtualMachine.CloseFiles;
var
  J: Integer;
begin
  for J := 1 to KMaxFiles do
    if FFiles[J] <> nil then
    begin
      c_fclose(FFiles[J]);
      FFiles[J] := nil;
    end;
end;

function TVirtualMachine.CompareValues(const A, B: TKayteValue; Op: TByteCodeOp): Boolean;
var
  Rel: Integer; // <0, 0, >0
begin
  // Arrays and objects compare by identity: = / <> only.
  if (A.Kind = vkArr) or (B.Kind = vkArr) then
  begin
    if not (Op in [BC_CMP_EQ, BC_CMP_NEQ]) then
      raise Exception.Create('Runtime Error: arrays and objects can only be compared with = and <>');
    Result := (A.Kind = vkArr) and (B.Kind = vkArr) and (A.Arr = B.Arr);
    if Op = BC_CMP_NEQ then
      Result := not Result;
    Exit;
  end;
  if (A.Kind = vkInt) and (B.Kind = vkInt) then
  begin
    if A.IntVal < B.IntVal then Rel := -1
    else if A.IntVal > B.IntVal then Rel := 1
    else Rel := 0;
  end
  else if (A.Kind in [vkInt, vkFloat]) and (B.Kind in [vkInt, vkFloat]) then
  begin
    if ToFloat(A) < ToFloat(B) then Rel := -1
    else if ToFloat(A) > ToFloat(B) then Rel := 1
    else Rel := 0;
  end
  else
    Rel := CompareStr(ToDisplayString(A), ToDisplayString(B));

  case Op of
    BC_CMP_EQ:  Result := Rel = 0;
    BC_CMP_NEQ: Result := Rel <> 0;
    BC_CMP_LT:  Result := Rel < 0;
    BC_CMP_GT:  Result := Rel > 0;
    BC_CMP_LE:  Result := Rel <= 0;
    BC_CMP_GE:  Result := Rel >= 0;
  else
    Result := False;
  end;
end;

function TVirtualMachine.GetIntegerLiteral(Index: Integer): Int64;
begin
  if (Index < 0) or (Index >= Length(FProgram.IntegerLiterals)) then
    raise Exception.CreateFmt('Runtime Error: integer literal index %d out of range', [Index]);
  Result := FProgram.IntegerLiterals[Index];
end;

function TVirtualMachine.GetStringLiteral(Index: Integer): string;
var
  Raw: string;
begin
  if (Index < 0) or (Index >= FProgram.StringLiterals.Count) then
    raise Exception.CreateFmt('Runtime Error: string literal index %d out of range', [Index]);
  Raw := FProgram.StringLiterals[Index];
  // String literals are stored with their surrounding quotes intact, as
  // captured verbatim from source by the lexer - strip them here.
  if (Length(Raw) >= 2) and (Raw[1] = '"') and (Raw[Length(Raw)] = '"') then
    Result := Copy(Raw, 2, Length(Raw) - 2)
  else
    Result := Raw;
end;

procedure TVirtualMachine.CheckVarIndex(Index: Integer);
begin
  if (Index < 0) or (Index >= Length(FVariables)) then
    raise Exception.CreateFmt('Runtime Error: variable index %d out of range', [Index]);
end;

// Name of the SUB starting at Address, for error messages.
function TVirtualMachine.SubNameAt(Address: Integer): string;
var
  I: Integer;
begin
  for I := 0 to FProgram.SubroutineMap.Count - 1 do
    if FProgram.SubroutineMap.Data[I] = Address then
      Exit(FProgram.SubroutineMap.Keys[I]);
  Result := '?';
end;

const
  MaxCallDepth = 10000;

procedure TVirtualMachine.EnterCall(Target: Integer; ArgCount: Integer; ReturnIP: LongInt);
begin
  if Target < 0 then
    raise Exception.Create('Runtime Error: CALL to an undefined SUB');
  if FCallDepth >= MaxCallDepth then
    raise Exception.CreateFmt('Runtime Error: call stack overflow (more than %d nested CALLs)', [MaxCallDepth]);
  if FCallDepth >= Length(FCallStack) then
    SetLength(FCallStack, Length(FCallStack) * 2 + 16);
  FCallStack[FCallDepth].ReturnIP := ReturnIP;
  FCallStack[FCallDepth].ArgCount := ArgCount;
  Inc(FCallDepth);
  FInstructionPointer := Target;
end;

// QT commands that need the VM itself rather than the Qt shim, because
// they involve SUBs:
//   QT "on", handle, "SubName"  - run SubName (no parameters) when handle
//                                 fires an event; "" removes the handler
//   QT "run"                    - event loop: waits for events and CALLs
//                                 their handlers, until every window closes
//   QT "event"                  - inside a handler: the handle that fired
// The same commands exist in QML statements; Keyword ('QT' or 'QML') is
// only for error messages. Args[0] is the command name. Returns False for
// any other command.
//
// "run" calls a handler by pushing its own arguments back and making the
// handler return to this very BC_QT instruction, which then re-executes
// and waits for the next event - so handlers are ordinary SUB calls and
// can themselves CALL other SUBs. Jumped tells Run not to advance the IP.
function TVirtualMachine.QtVMCommand(const Keyword: string; const Args: array of TKayteValue;
  out Res: TKayteValue; out Jumped: Boolean): Boolean;
var
  Cmd, SubKey: string;
  Handle: Int64;
  Idx, Target, J: Integer;
begin
  Result := True;
  Jumped := False;
  Res := MakeInt(0);
  Cmd := LowerCase(ToDisplayString(Args[0]));

  if Cmd = 'on' then
  begin
    if Length(Args) <> 3 then
      raise Exception.CreateFmt('Runtime Error: %s "on" expects 2 argument(s), got %d', [Keyword, Length(Args) - 1]);
    Handle := ToInt(Args[1]);
    SubKey := UpperCase(ToDisplayString(Args[2]));
    if SubKey = '' then
    begin
      Idx := FQtHandlers.IndexOf(Handle);
      if Idx >= 0 then
        FQtHandlers.Delete(Idx);
      Exit;
    end;
    Idx := FProgram.SubroutineMap.IndexOf(SubKey);
    if Idx < 0 then
      raise Exception.CreateFmt('Runtime Error: %s "on" - there is no SUB named "%s"', [Keyword, ToDisplayString(Args[2])]);
    Target := FProgram.SubroutineMap.Data[Idx];
    if FProgram.Instructions[Target].Operand1 <> 0 then
      raise Exception.CreateFmt('Runtime Error: %0:s "on" - handler SUB "%1:s" must take no parameters '
        + '(use %0:s "event" inside it to see which widget fired)', [Keyword, ToDisplayString(Args[2])]);
    FQtHandlers[Handle] := Target;
    Res := MakeInt(1);
  end
  else if Cmd = 'event' then
  begin
    if Length(Args) <> 1 then
      raise Exception.CreateFmt('Runtime Error: %s "event" takes no arguments', [Keyword]);
    Res := MakeInt(FQtEvent);
  end
  else if Cmd = 'run' then
  begin
    if Length(Args) <> 1 then
      raise Exception.CreateFmt('Runtime Error: %s "run" takes no arguments', [Keyword]);
    repeat
      Handle := QtCall('wait', []).IntVal;
      if Handle = 0 then
      begin
        FQtEvent := 0;
        Exit; // every window closed - continue after the QT/QML "run"
      end;
      Idx := FQtHandlers.IndexOf(Handle);
    until Idx >= 0;

    FQtEvent := Handle;
    for J := 0 to High(Args) do
      Push(Args[J]);
    EnterCall(FQtHandlers.Data[Idx], 0, FInstructionPointer);
    Jumped := True;
  end
  else
    Result := False;
end;

// After QT "loadform": attaches the SUBs the form file names (onclick:
// DoLogin(), or VB-style Button1_Click) to the handles it created, as QT
// "on" would. The shim lists them as "handle<TAB>SubName<TAB>optional"
// lines; optional ones are skipped when there's no such SUB.
procedure TVirtualMachine.AttachFormHandlers(Window: Int64; const Keyword, Prefix: string);
var
  Lines: TStringList;
  Line, Rest, SubName: string;
  Handle: Int64;
  Tab, Idx, Target: Integer;
  Optional: Boolean;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := QtCall(Prefix + 'formhandlers', [QtInt(Window)]).StrVal;
    for Line in Lines do
    begin
      Tab := Pos(#9, Line);
      if Tab = 0 then
        Continue;
      Handle := StrToInt64(Copy(Line, 1, Tab - 1));
      Rest := Copy(Line, Tab + 1, MaxInt);
      Tab := Pos(#9, Rest);
      if Tab = 0 then
        Continue;
      SubName := Copy(Rest, 1, Tab - 1);
      Optional := Copy(Rest, Tab + 1, MaxInt) = '1';

      Idx := FProgram.SubroutineMap.IndexOf(UpperCase(SubName));
      if Idx < 0 then
      begin
        if Optional then
          Continue;
        raise Exception.CreateFmt('Runtime Error: %s "loadform" - the form''s handler SUB "%s" doesn''t exist',
          [Keyword, SubName]);
      end;
      Target := FProgram.SubroutineMap.Data[Idx];
      if FProgram.Instructions[Target].Operand1 <> 0 then
        raise Exception.CreateFmt('Runtime Error: %0:s "loadform" - handler SUB "%1:s" must take no parameters '
          + '(use %0:s "event" inside it to see which widget fired)', [Keyword, SubName]);
      FQtHandlers[Handle] := Target;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TVirtualMachine.Run;
var
  InstructionCount: LongInt;
  Instr: TBCInstruction;
  A, B, C: TKayteValue;
  Arr: TKayteArray;
  Idx: Int64;
  Bits: QWord;
  FloatLit: Double;
  Done: Boolean;
  MaxVarIndex, I, J: Integer;
  PrintArgs: array of TKayteValue;
  OutLine: string;
  ProcArgs: array of TKayteValue;
  ProcArgStrs: array of string;
  ProcOutput: string;
  QtArgs: array of TQtValue;
  QtResult: TQtValue;
  QtKeyword, QtPrefix: string;
  VMResult: TKayteValue;
  Jumped: Boolean;
begin
  InstructionCount := Length(FProgram.Instructions);

  Writeln('Executing bytecode for program: ', FProgram.ProgramTitle);
  Writeln('Total instructions to execute: ', InstructionCount);

  if InstructionCount = 0 then
  begin
    Writeln('Program is empty. Execution finished.');
    Exit;
  end;

  // Size the runtime variable table from the highest variable index seen
  // at compile time (VariableMap). Variables (the name list) isn't
  // repopulated when loading a saved .bytecode file, so it can't be used
  // for this - VariableMap always is.
  MaxVarIndex := -1;
  for I := 0 to FProgram.VariableMap.Count - 1 do
    if FProgram.VariableMap.Data[I] > MaxVarIndex then
      MaxVarIndex := FProgram.VariableMap.Data[I];
  SetLength(FVariables, MaxVarIndex + 1);
  for I := 0 to High(FVariables) do
    FVariables[I] := MakeInt(0);

  FStackTop := 0;
  FCallDepth := 0;
  FTryCount := 0;
  FInstructionPointer := 0;
  Randomize;
  // Overflow gives infinity, which MakeFloat reports (as the native runtime does).
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

  // A runtime error inside a TRY goes to its CATCH: the handler restores
  // the stack and call depth, pushes the message (without "Runtime
  // Error: ") and execution resumes there.
  Done := False;
  repeat
  try
  while FInstructionPointer < InstructionCount do
  begin
    Instr := FProgram.Instructions[FInstructionPointer];

    case Instr.OpCode of
      BC_NOP: ;
      BC_HALT:
        begin
          FInstructionPointer := InstructionCount;
          Continue;
        end;
      BC_LOAD_INT:
        Push(MakeInt(GetIntegerLiteral(Instr.Operand1)));
      BC_LOAD_STRING:
        Push(MakeStr(GetStringLiteral(Instr.Operand1)));
      BC_LOAD_VAR:
        begin
          CheckVarIndex(Instr.Operand1);
          Push(FVariables[Instr.Operand1]);
        end;
      BC_STORE_VAR, BC_ASSIGN:
        begin
          CheckVarIndex(Instr.Operand1);
          FVariables[Instr.Operand1] := Pop;
        end;
      BC_ADD:
        begin
          B := Pop;
          A := Pop;
          if (A.Kind = vkInt) and (B.Kind = vkInt) then
            Push(MakeInt(Int64(QWord(A.IntVal) + QWord(B.IntVal))))
          else if (A.Kind in [vkInt, vkFloat]) and (B.Kind in [vkInt, vkFloat]) then
            Push(MakeFloat(ToFloat(A) + ToFloat(B)))
          else
            // Loosely-typed '+': falls back to concatenation, like VB6.
            Push(MakeStr(ToDisplayString(A) + ToDisplayString(B)));
        end;
      BC_SUB, BC_MUL, BC_DIV, BC_IDIV, BC_MOD:
        begin
          B := Pop;
          A := Pop;
          Push(Arith(Instr.OpCode, A, B));
        end;
      BC_LOAD_FLOAT:
        begin
          Bits := QWord(LongWord(Instr.Operand1)) or (QWord(LongWord(Instr.Operand2)) shl 32);
          Move(Bits, FloatLit, SizeOf(FloatLit));
          Push(MakeFloat(FloatLit));
        end;
      BC_CONCAT:
        begin
          B := Pop;
          A := Pop;
          Push(MakeStr(ToDisplayString(A) + ToDisplayString(B)));
        end;
      BC_NEG:
        begin
          A := NumOf(Pop);
          if A.Kind = vkInt then
            Push(MakeInt(Int64(0 - QWord(A.IntVal))))
          else
            Push(MakeFloat(-A.FltVal));
        end;
      BC_NOT:
        begin
          A := Pop;
          Push(MakeInt(Ord(not IsTruthy(A))));
        end;
      BC_CMP_EQ, BC_CMP_NEQ, BC_CMP_LT, BC_CMP_GT, BC_CMP_LE, BC_CMP_GE:
        begin
          B := Pop;
          A := Pop;
          Push(MakeInt(Ord(CompareValues(A, B, Instr.OpCode))));
        end;
      BC_PRINT:
        begin
          SetLength(PrintArgs, Instr.Operand1);
          for J := Instr.Operand1 - 1 downto 0 do
            PrintArgs[J] := Pop;
          OutLine := '';
          for J := 0 to Instr.Operand1 - 1 do
          begin
            if J > 0 then
              OutLine := OutLine + ' ';
            OutLine := OutLine + ToDisplayString(PrintArgs[J]);
          end;
          Writeln(OutLine);
          FColumn := 0;
        end;
      BC_QPRINT:
        case Instr.Operand1 of
          QP_VALUE:
            begin
              A := Pop;
              if A.Kind = vkInt then
              begin
                if A.IntVal >= 0 then
                  Output(' ' + IntToStr(A.IntVal) + ' ')
                else
                  Output(IntToStr(A.IntVal) + ' ');
              end
              else if A.Kind = vkFloat then
              begin
                if A.FltVal >= 0 then
                  Output(' ' + FormatNumber(A.FltVal, True) + ' ')
                else
                  Output(FormatNumber(A.FltVal, True) + ' ');
              end
              else
                Output(ToDisplayString(A));
            end;
          QP_TEXT: Output(ToDisplayString(Pop));
          QP_COMMA: Output(StringOfChar(' ', 14 - FColumn mod 14));
          QP_NEWLINE: Output(#10);
          QP_TAB:
            begin
              J := ToInt(Pop) - 1;
              if J < FColumn then
                Output(#10);
              if J > FColumn then
                Output(StringOfChar(' ', J - FColumn));
            end;
          QP_SPC:
            begin
              J := ToInt(Pop);
              if J > 0 then
                Output(StringOfChar(' ', J));
            end;
        end;
      BC_POP:
        Pop;
      BC_JUMP:
        begin
          FInstructionPointer := Instr.Operand1;
          Continue;
        end;
      BC_JUMP_IF_FALSE:
        begin
          A := Pop;
          if not IsTruthy(A) then
          begin
            FInstructionPointer := Instr.Operand1;
            Continue;
          end;
        end;
      BC_PROCESS:
        begin
          // Operand1 = pushed argument count (command + args), in order;
          // Operand2 = destination variable index, or -1 to print output.
          SetLength(ProcArgs, Instr.Operand1);
          for J := Instr.Operand1 - 1 downto 0 do
            ProcArgs[J] := Pop;

          SetLength(ProcArgStrs, Instr.Operand1 - 1);
          for J := 1 to Instr.Operand1 - 1 do
            ProcArgStrs[J - 1] := ToDisplayString(ProcArgs[J]);

          ProcOutput := '';
          if not RunCommand(ToDisplayString(ProcArgs[0]), ProcArgStrs, ProcOutput) then
            raise Exception.CreateFmt('Runtime Error: failed to run process "%s"', [ToDisplayString(ProcArgs[0])]);

          if Instr.Operand2 >= 0 then
          begin
            CheckVarIndex(Instr.Operand2);
            FVariables[Instr.Operand2] := MakeStr(ProcOutput);
          end
          else
            Writeln(ProcOutput);
        end;
      BC_QT:
        begin
          // Same operand layout as BC_PROCESS: Operand1 = pushed argument
          // count (command name + args), Operand2 = destination variable
          // index for the result, or -1 to discard it. Operand3 says
          // whether a QT or a QML statement emitted it; the shim tells
          // QML commands apart by a "qml." prefix.
          SetLength(ProcArgs, Instr.Operand1);
          for J := Instr.Operand1 - 1 downto 0 do
            ProcArgs[J] := Pop;
          if Instr.Operand3 = QT_STATEMENT_QML then
          begin
            QtKeyword := 'QML';
            QtPrefix := 'qml.';
          end
          else
          begin
            QtKeyword := 'QT';
            QtPrefix := '';
          end;

          if QtVMCommand(QtKeyword, ProcArgs, VMResult, Jumped) then
          begin
            if Jumped then
              Continue; // now at a handler SUB's entry
            if Instr.Operand2 >= 0 then
            begin
              CheckVarIndex(Instr.Operand2);
              FVariables[Instr.Operand2] := VMResult;
            end;
            Inc(FInstructionPointer);
            Continue;
          end;

          SetLength(QtArgs, Instr.Operand1 - 1);
          for J := 1 to Instr.Operand1 - 1 do
            if ProcArgs[J].Kind = vkInt then
              QtArgs[J - 1] := QtInt(ProcArgs[J].IntVal)
            else
              QtArgs[J - 1] := QtStr(ToDisplayString(ProcArgs[J]));

          QtResult := QtCall(QtPrefix + ToDisplayString(ProcArgs[0]), QtArgs);
          if SameText(ToDisplayString(ProcArgs[0]), 'loadform') then
            AttachFormHandlers(QtResult.IntVal, QtKeyword, QtPrefix);

          if Instr.Operand2 >= 0 then
          begin
            CheckVarIndex(Instr.Operand2);
            if QtResult.IsStr then
              FVariables[Instr.Operand2] := MakeStr(QtResult.StrVal)
            else
              FVariables[Instr.Operand2] := MakeInt(QtResult.IntVal);
          end;
        end;
      BC_CALL:
        begin
          // Operand1 = SUB entry address (-1 if it was never defined),
          // Operand2 = number of arguments pushed for it.
          EnterCall(Instr.Operand1, Instr.Operand2, FInstructionPointer + 1);
          Continue;
        end;
      BC_ENTER:
        // Operand1 = the SUB's parameter count. Normally checked at compile
        // time; this catches the rest (e.g. a QT "on" handler).
        if FCallDepth = 0 then
          raise Exception.CreateFmt('Runtime Error: SUB "%s" entered without a CALL', [SubNameAt(FInstructionPointer)])
        else if FCallStack[FCallDepth - 1].ArgCount <> Instr.Operand1 then
          raise Exception.CreateFmt('Runtime Error: SUB "%s" takes %d argument(s), got %d',
            [SubNameAt(FInstructionPointer), Instr.Operand1, FCallStack[FCallDepth - 1].ArgCount]);
      BC_RETURN:
        begin
          if FCallDepth = 0 then
            raise Exception.Create('Runtime Error: RETURN without a GOSUB (or outside a SUB call)');
          Dec(FCallDepth);
          // TRYs started inside the call that's ending no longer apply.
          while (FTryCount > 0) and (FTries[FTryCount - 1].CallDepth > FCallDepth) do
            Dec(FTryCount);
          FInstructionPointer := FCallStack[FCallDepth].ReturnIP;
          Continue;
        end;
      BC_INDEX_GET:
        begin
          B := Pop;
          A := Pop;
          Arr := ArrayOf(A, 'indexing');
          Idx := ToInt(B);
          if (Idx < 0) or (Idx > High(Arr.Items)) then
            raise Exception.CreateFmt('Runtime Error: index %d is out of range (0 to %d)', [Idx, High(Arr.Items)]);
          Push(Arr.Items[Idx]);
        end;
      BC_INDEX_SET:
        begin
          C := Pop;
          B := Pop;
          A := Pop;
          Arr := ArrayOf(A, 'indexing');
          Idx := ToInt(B);
          if (Idx < 0) or (Idx > High(Arr.Items)) then
            raise Exception.CreateFmt('Runtime Error: index %d is out of range (0 to %d)', [Idx, High(Arr.Items)]);
          Arr.Items[Idx] := C;
        end;
      BC_BUILTIN:
        begin
          SetLength(ProcArgs, Instr.Operand2);
          for J := Instr.Operand2 - 1 downto 0 do
            ProcArgs[J] := Pop;
          Push(CallBuiltin(Instr.Operand1, ProcArgs));
        end;
      BC_TRY:
        begin
          if FTryCount >= Length(FTries) then
            SetLength(FTries, Length(FTries) * 2 + 8);
          FTries[FTryCount].Catch := Instr.Operand1;
          FTries[FTryCount].StackTop := FStackTop;
          FTries[FTryCount].CallDepth := FCallDepth;
          Inc(FTryCount);
        end;
      BC_TRY_END:
        if FTryCount > 0 then
          Dec(FTryCount);
      BC_THROW:
        raise Exception.Create('Runtime Error: ' + ToDisplayString(Pop));
      BC_INPUT:
        begin
          // A line from stdin (without its line break); "" at the end of input.
          Flush(System.Output);
          if EOF(System.Input) then
            OutLine := ''
          else
            ReadLn(OutLine);
          if (OutLine <> '') and (OutLine[Length(OutLine)] = #13) then
            SetLength(OutLine, Length(OutLine) - 1);
          FColumn := 0;
          Push(MakeStr(OutLine));
        end;
    end;

    Inc(FInstructionPointer);
  end;
  Done := True;
  except
    on E: Exception do
    begin
      if FTryCount = 0 then
      begin
        CloseFiles; // what was written so far gets to the files
        raise;
      end;
      Dec(FTryCount);
      FStackTop := FTries[FTryCount].StackTop;
      FCallDepth := FTries[FTryCount].CallDepth;
      if Pos('Runtime Error: ', E.Message) = 1 then
        Push(MakeStr(Copy(E.Message, 16, MaxInt)))
      else
        Push(MakeStr(E.Message));
      FInstructionPointer := FTries[FTryCount].Catch;
    end;
  end;
  until Done;
  CloseFiles;

  Writeln('Execution finished successfully.');
end;

end.
