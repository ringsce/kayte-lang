unit MathLib;

{$mode objfpc}{$H+}

interface

uses
  Math, SysUtils;

type
  { TMathLib - Comprehensive math library for Kayte Lang }
  TMathLib = class
  public
    { Basic Arithmetic Operations }
    class function Add(const A, B: Double): Double;
    class function Subtract(const A, B: Double): Double;
    class function Multiply(const A, B: Double): Double;
    class function Divide(const A, B: Double): Double;
    class function Modulo(const A, B: Integer): Integer;
    class function Power(const Base, Exponent: Double): Double;

    { Trigonometric Functions }
    class function Sin(const Angle: Double): Double;
    class function Cos(const Angle: Double): Double;
    class function Tan(const Angle: Double): Double;
    class function ArcSin(const Value: Double): Double;
    class function ArcCos(const Value: Double): Double;
    class function ArcTan(const Value: Double): Double;
    class function ArcTan2(const Y, X: Double): Double;

    { Hyperbolic Functions }
    class function SinH(const Value: Double): Double;
    class function CosH(const Value: Double): Double;
    class function TanH(const Value: Double): Double;

    { Exponential and Logarithmic Functions }
    class function Exp(const Value: Double): Double;
    class function Ln(const Value: Double): Double;
    class function Log10(const Value: Double): Double;
    class function Log2(const Value: Double): Double;
    class function LogN(const Base, Value: Double): Double;

    { Root Functions }
    class function Sqrt(const Value: Double): Double;
    class function CubeRoot(const Value: Double): Double;
    class function NthRoot(const Value: Double; const N: Integer): Double;

    { Rounding Functions }
    class function Round(const Value: Double): Int64;
    class function RoundTo(const Value: Double; const Digits: Integer): Double;
    class function Floor(const Value: Double): Int64;
    class function Ceil(const Value: Double): Int64;
    class function Trunc(const Value: Double): Int64;
    class function Frac(const Value: Double): Double;

    { Absolute and Sign Functions }
    class function Abs(const Value: Double): Double;
    class function Sign(const Value: Double): Integer;

    { Min/Max Functions }
    class function MinValue(const A, B: Double): Double;
    class function MaxValue(const A, B: Double): Double;
    class function Clamp(const Value, MinVal, MaxVal: Double): Double;

    { Constants }
    class function Pi: Double;
    class function E: Double;
    class function GoldenRatio: Double;

    { Statistical Functions }
    class function Average(const Values: array of Double): Double;
    class function Sum(const Values: array of Double): Double;
    class function Variance(const Values: array of Double): Double;
    class function StdDev(const Values: array of Double): Double;
    class function Median(const Values: array of Double): Double;

    { Random Number Functions }
    class function Random: Double; overload;
    class function Random(const Range: Integer): Integer; overload;
    class function RandomRange(const Min, Max: Integer): Integer;

    { Factorial and Combinatorics }
    class function Factorial(const N: Integer): Int64;
    class function Permutation(const N, R: Integer): Int64;
    class function Combination(const N, R: Integer): Int64;

    { Angle Conversion }
    class function DegToRad(const Degrees: Double): Double;
    class function RadToDeg(const Radians: Double): Double;

    { Distance and Geometry }
    class function Distance2D(const X1, Y1, X2, Y2: Double): Double;
    class function Distance3D(const X1, Y1, Z1, X2, Y2, Z2: Double): Double;
    class function PointInCircle(const PX, PY, CX, CY, Radius: Double): Boolean;

    { Number Properties }
    class function IsEven(const N: Integer): Boolean;
    class function IsOdd(const N: Integer): Boolean;
    class function IsPrime(const N: Integer): Boolean;
    class function GCD(A, B: Integer): Integer;
    class function LCM(A, B: Integer): Integer;
  end;

implementation

{ TMathLib }

{ Basic Arithmetic Operations }
class function TMathLib.Add(const A, B: Double): Double;
begin
  Result := A + B;
end;

class function TMathLib.Subtract(const A, B: Double): Double;
begin
  Result := A - B;
end;

class function TMathLib.Multiply(const A, B: Double): Double;
begin
  Result := A * B;
end;

class function TMathLib.Divide(const A, B: Double): Double;
begin
  if B = 0 then
    raise Exception.Create('Division by zero');
  Result := A / B;
end;

class function TMathLib.Modulo(const A, B: Integer): Integer;
begin
  if B = 0 then
    raise Exception.Create('Modulo by zero');
  Result := A mod B;
end;

class function TMathLib.Power(const Base, Exponent: Double): Double;
begin
  Result := Math.Power(Base, Exponent);
end;

{ Trigonometric Functions }
class function TMathLib.Sin(const Angle: Double): Double;
begin
  Result := System.Sin(Angle);
end;

class function TMathLib.Cos(const Angle: Double): Double;
begin
  Result := System.Cos(Angle);
end;

class function TMathLib.Tan(const Angle: Double): Double;
begin
  Result := Math.Tan(Angle);
end;

class function TMathLib.ArcSin(const Value: Double): Double;
begin
  Result := Math.ArcSin(Value);
end;

class function TMathLib.ArcCos(const Value: Double): Double;
begin
  Result := Math.ArcCos(Value);
end;

class function TMathLib.ArcTan(const Value: Double): Double;
begin
  Result := System.ArcTan(Value);
end;

class function TMathLib.ArcTan2(const Y, X: Double): Double;
begin
  Result := Math.ArcTan2(Y, X);
end;

{ Hyperbolic Functions }
class function TMathLib.SinH(const Value: Double): Double;
begin
  Result := Math.SinH(Value);
end;

class function TMathLib.CosH(const Value: Double): Double;
begin
  Result := Math.CosH(Value);
end;

class function TMathLib.TanH(const Value: Double): Double;
begin
  Result := Math.TanH(Value);
end;

{ Exponential and Logarithmic Functions }
class function TMathLib.Exp(const Value: Double): Double;
begin
  Result := System.Exp(Value);
end;

class function TMathLib.Ln(const Value: Double): Double;
begin
  if Value <= 0 then
    raise Exception.Create('Logarithm of non-positive value');
  Result := System.Ln(Value);
end;

class function TMathLib.Log10(const Value: Double): Double;
begin
  if Value <= 0 then
    raise Exception.Create('Logarithm of non-positive value');
  Result := Math.Log10(Value);
end;

class function TMathLib.Log2(const Value: Double): Double;
begin
  if Value <= 0 then
    raise Exception.Create('Logarithm of non-positive value');
  Result := Math.Log2(Value);
end;

class function TMathLib.LogN(const Base, Value: Double): Double;
begin
  if (Value <= 0) or (Base <= 0) or (Base = 1) then
    raise Exception.Create('Invalid logarithm parameters');
  Result := Math.LogN(Base, Value);
end;

{ Root Functions }
class function TMathLib.Sqrt(const Value: Double): Double;
begin
  if Value < 0 then
    raise Exception.Create('Square root of negative number');
  Result := System.Sqrt(Value);
end;

class function TMathLib.CubeRoot(const Value: Double): Double;
begin
  if Value < 0 then
    Result := -Math.Power(-Value, 1/3)
  else
    Result := Math.Power(Value, 1/3);
end;

class function TMathLib.NthRoot(const Value: Double; const N: Integer): Double;
begin
  if N = 0 then
    raise Exception.Create('Cannot compute 0th root');
  if (Value < 0) and (N mod 2 = 0) then
    raise Exception.Create('Even root of negative number');
  if Value < 0 then
    Result := -Math.Power(-Value, 1/N)
  else
    Result := Math.Power(Value, 1/N);
end;

{ Rounding Functions }
class function TMathLib.Round(const Value: Double): Int64;
begin
  Result := System.Round(Value);
end;

class function TMathLib.RoundTo(const Value: Double; const Digits: Integer): Double;
begin
  Result := Math.RoundTo(Value, Digits);
end;

class function TMathLib.Floor(const Value: Double): Int64;
begin
  Result := Math.Floor(Value);
end;

class function TMathLib.Ceil(const Value: Double): Int64;
begin
  Result := Math.Ceil(Value);
end;

class function TMathLib.Trunc(const Value: Double): Int64;
begin
  Result := System.Trunc(Value);
end;

class function TMathLib.Frac(const Value: Double): Double;
begin
  Result := System.Frac(Value);
end;

{ Absolute and Sign Functions }
class function TMathLib.Abs(const Value: Double): Double;
begin
  Result := System.Abs(Value);
end;

class function TMathLib.Sign(const Value: Double): Integer;
begin
  Result := Math.Sign(Value);
end;

{ Min/Max Functions }
class function TMathLib.MinValue(const A, B: Double): Double;
begin
  Result := Math.Min(A, B);
end;

class function TMathLib.MaxValue(const A, B: Double): Double;
begin
  Result := Math.Max(A, B);
end;

class function TMathLib.Clamp(const Value, MinVal, MaxVal: Double): Double;
begin
  Result := Math.EnsureRange(Value, MinVal, MaxVal);
end;

{ Constants }
class function TMathLib.Pi: Double;
begin
  Result := Math.PI;
end;

class function TMathLib.E: Double;
begin
  Result := System.Exp(1);
end;

class function TMathLib.GoldenRatio: Double;
begin
  Result := (1 + Sqrt(5)) / 2;
end;

{ Statistical Functions }
class function TMathLib.Average(const Values: array of Double): Double;
var
  I: Integer;
  TotalSum: Double;
begin
  if Length(Values) = 0 then
    raise Exception.Create('Cannot compute average of empty array');
  TotalSum := 0;
  for I := Low(Values) to High(Values) do
    TotalSum := TotalSum + Values[I];
  Result := TotalSum / Length(Values);
end;

class function TMathLib.Sum(const Values: array of Double): Double;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(Values) to High(Values) do
    Result := Result + Values[I];
end;

class function TMathLib.Variance(const Values: array of Double): Double;
var
  Avg: Double;
  I: Integer;
  SumSq: Double;
begin
  if Length(Values) < 2 then
    raise Exception.Create('Need at least 2 values for variance');
  Avg := Average(Values);
  SumSq := 0;
  for I := Low(Values) to High(Values) do
    SumSq := SumSq + Math.Power(Values[I] - Avg, 2);
  Result := SumSq / (Length(Values) - 1);
end;

class function TMathLib.StdDev(const Values: array of Double): Double;
begin
  Result := Sqrt(Variance(Values));
end;

class function TMathLib.Median(const Values: array of Double): Double;
var
  SortedValues: array of Double;
  I, J, Len: Integer;
  Temp: Double;
begin
  Len := Length(Values);
  if Len = 0 then
    raise Exception.Create('Cannot compute median of empty array');

  SetLength(SortedValues, Len);
  for I := 0 to Len - 1 do
    SortedValues[I] := Values[I];

  // Simple bubble sort
  for I := 0 to Len - 2 do
    for J := 0 to Len - I - 2 do
      if SortedValues[J] > SortedValues[J + 1] then
      begin
        Temp := SortedValues[J];
        SortedValues[J] := SortedValues[J + 1];
        SortedValues[J + 1] := Temp;
      end;

  if Len mod 2 = 1 then
    Result := SortedValues[Len div 2]
  else
    Result := (SortedValues[Len div 2 - 1] + SortedValues[Len div 2]) / 2;
end;

{ Random Number Functions }
class function TMathLib.Random: Double;
begin
  Result := System.Random;
end;

class function TMathLib.Random(const Range: Integer): Integer;
begin
  if Range <= 0 then
    raise Exception.Create('Range must be positive');
  Result := System.Random(Range);
end;

class function TMathLib.RandomRange(const Min, Max: Integer): Integer;
begin
  if Min >= Max then
    raise Exception.Create('Min must be less than Max');
  Result := Min + System.Random(Max - Min + 1);
end;

{ Factorial and Combinatorics }
class function TMathLib.Factorial(const N: Integer): Int64;
var
  I: Integer;
begin
  if N < 0 then
    raise Exception.Create('Factorial of negative number');
  if N > 20 then
    raise Exception.Create('Factorial overflow (use N <= 20)');
  Result := 1;
  for I := 2 to N do
    Result := Result * I;
end;

class function TMathLib.Permutation(const N, R: Integer): Int64;
var
  I: Integer;
begin
  if (N < 0) or (R < 0) or (R > N) then
    raise Exception.Create('Invalid permutation parameters');
  Result := 1;
  for I := N - R + 1 to N do
    Result := Result * I;
end;

class function TMathLib.Combination(const N, R: Integer): Int64;
begin
  if (N < 0) or (R < 0) or (R > N) then
    raise Exception.Create('Invalid combination parameters');
  if R > N - R then
    Result := Permutation(N, R) div Factorial(R)
  else
    Result := Permutation(N, N - R) div Factorial(N - R);
end;

{ Angle Conversion }
class function TMathLib.DegToRad(const Degrees: Double): Double;
begin
  Result := Math.DegToRad(Degrees);
end;

class function TMathLib.RadToDeg(const Radians: Double): Double;
begin
  Result := Math.RadToDeg(Radians);
end;

{ Distance and Geometry }
class function TMathLib.Distance2D(const X1, Y1, X2, Y2: Double): Double;
begin
  Result := Sqrt(Math.Power(X2 - X1, 2) + Math.Power(Y2 - Y1, 2));
end;

class function TMathLib.Distance3D(const X1, Y1, Z1, X2, Y2, Z2: Double): Double;
begin
  Result := Sqrt(Math.Power(X2 - X1, 2) + Math.Power(Y2 - Y1, 2) + Math.Power(Z2 - Z1, 2));
end;

class function TMathLib.PointInCircle(const PX, PY, CX, CY, Radius: Double): Boolean;
begin
  Result := Distance2D(PX, PY, CX, CY) <= Radius;
end;

{ Number Properties }
class function TMathLib.IsEven(const N: Integer): Boolean;
begin
  Result := (N mod 2) = 0;
end;

class function TMathLib.IsOdd(const N: Integer): Boolean;
begin
  Result := (N mod 2) <> 0;
end;

class function TMathLib.IsPrime(const N: Integer): Boolean;
var
  I: Integer;
begin
  if N <= 1 then
    Exit(False);
  if N <= 3 then
    Exit(True);
  if (N mod 2 = 0) or (N mod 3 = 0) then
    Exit(False);
  I := 5;
  while I * I <= N do
  begin
    if (N mod I = 0) or (N mod (I + 2) = 0) then
      Exit(False);
    I := I + 6;
  end;
  Result := True;
end;

class function TMathLib.GCD(A, B: Integer): Integer;
var
  Temp: Integer;
begin
  A := Abs(A);
  B := Abs(B);
  while B <> 0 do
  begin
    Temp := B;
    B := A mod B;
    A := Temp;
  end;
  Result := A;
end;

class function TMathLib.LCM(A, B: Integer): Integer;
begin
  if (A = 0) or (B = 0) then
    Result := 0
  else
    Result := Abs(A * B) div GCD(A, B);
end;

end.
