library mathlibdylib;

{$mode objfpc}{$H+}

uses
  Math, SysUtils, MathLib;

{ Export C-compatible wrapper functions for the math library }

{ Basic Arithmetic }
function ml_add(a, b: Double): Double; cdecl;
begin
  Result := TMathLib.Add(a, b);
end;

function ml_subtract(a, b: Double): Double; cdecl;
begin
  Result := TMathLib.Subtract(a, b);
end;

function ml_multiply(a, b: Double): Double; cdecl;
begin
  Result := TMathLib.Multiply(a, b);
end;

function ml_divide(a, b: Double): Double; cdecl;
begin
  Result := TMathLib.Divide(a, b);
end;

function ml_power(base, exponent: Double): Double; cdecl;
begin
  Result := TMathLib.Power(base, exponent);
end;

function ml_modulo(a, b: LongInt): LongInt; cdecl;
begin
  Result := TMathLib.Modulo(a, b);
end;

{ Trigonometric Functions }
function ml_sin(angle: Double): Double; cdecl;
begin
  Result := TMathLib.Sin(angle);
end;

function ml_cos(angle: Double): Double; cdecl;
begin
  Result := TMathLib.Cos(angle);
end;

function ml_tan(angle: Double): Double; cdecl;
begin
  Result := TMathLib.Tan(angle);
end;

function ml_arcsin(value: Double): Double; cdecl;
begin
  Result := TMathLib.ArcSin(value);
end;

function ml_arccos(value: Double): Double; cdecl;
begin
  Result := TMathLib.ArcCos(value);
end;

function ml_arctan(value: Double): Double; cdecl;
begin
  Result := TMathLib.ArcTan(value);
end;

function ml_arctan2(y, x: Double): Double; cdecl;
begin
  Result := TMathLib.ArcTan2(y, x);
end;

{ Hyperbolic Functions }
function ml_sinh(value: Double): Double; cdecl;
begin
  Result := TMathLib.SinH(value);
end;

function ml_cosh(value: Double): Double; cdecl;
begin
  Result := TMathLib.CosH(value);
end;

function ml_tanh(value: Double): Double; cdecl;
begin
  Result := TMathLib.TanH(value);
end;

{ Exponential and Logarithmic }
function ml_exp(value: Double): Double; cdecl;
begin
  Result := TMathLib.Exp(value);
end;

function ml_ln(value: Double): Double; cdecl;
begin
  Result := TMathLib.Ln(value);
end;

function ml_log10(value: Double): Double; cdecl;
begin
  Result := TMathLib.Log10(value);
end;

function ml_log2(value: Double): Double; cdecl;
begin
  Result := TMathLib.Log2(value);
end;

function ml_logn(base, value: Double): Double; cdecl;
begin
  Result := TMathLib.LogN(base, value);
end;

{ Root Functions }
function ml_sqrt(value: Double): Double; cdecl;
begin
  Result := TMathLib.Sqrt(value);
end;

function ml_cuberoot(value: Double): Double; cdecl;
begin
  Result := TMathLib.CubeRoot(value);
end;

function ml_nthroot(value: Double; n: LongInt): Double; cdecl;
begin
  Result := TMathLib.NthRoot(value, n);
end;

{ Rounding Functions }
function ml_round(value: Double): Int64; cdecl;
begin
  Result := TMathLib.Round(value);
end;

function ml_floor(value: Double): Int64; cdecl;
begin
  Result := TMathLib.Floor(value);
end;

function ml_ceil(value: Double): Int64; cdecl;
begin
  Result := TMathLib.Ceil(value);
end;

function ml_trunc(value: Double): Int64; cdecl;
begin
  Result := TMathLib.Trunc(value);
end;

function ml_frac(value: Double): Double; cdecl;
begin
  Result := TMathLib.Frac(value);
end;

{ Absolute and Sign }
function ml_abs(value: Double): Double; cdecl;
begin
  Result := TMathLib.Abs(value);
end;

function ml_sign(value: Double): LongInt; cdecl;
begin
  Result := TMathLib.Sign(value);
end;

{ Min/Max }
function ml_min(a, b: Double): Double; cdecl;
begin
  Result := TMathLib.MinValue(a, b);
end;

function ml_max(a, b: Double): Double; cdecl;
begin
  Result := TMathLib.MaxValue(a, b);
end;

function ml_clamp(value, minval, maxval: Double): Double; cdecl;
begin
  Result := TMathLib.Clamp(value, minval, maxval);
end;

{ Constants }
function ml_pi: Double; cdecl;
begin
  Result := TMathLib.Pi;
end;

function ml_e: Double; cdecl;
begin
  Result := TMathLib.E;
end;

function ml_goldenratio: Double; cdecl;
begin
  Result := TMathLib.GoldenRatio;
end;

{ Random }
function ml_random: Double; cdecl;
begin
  Result := TMathLib.Random;
end;

function ml_random_range(min, max: LongInt): LongInt; cdecl;
begin
  Result := TMathLib.RandomRange(min, max);
end;

{ Factorial and Combinatorics }
function ml_factorial(n: LongInt): Int64; cdecl;
begin
  Result := TMathLib.Factorial(n);
end;

function ml_permutation(n, r: LongInt): Int64; cdecl;
begin
  Result := TMathLib.Permutation(n, r);
end;

function ml_combination(n, r: LongInt): Int64; cdecl;
begin
  Result := TMathLib.Combination(n, r);
end;

{ Angle Conversion }
function ml_degtorad(degrees: Double): Double; cdecl;
begin
  Result := TMathLib.DegToRad(degrees);
end;

function ml_radtodeg(radians: Double): Double; cdecl;
begin
  Result := TMathLib.RadToDeg(radians);
end;

{ Distance and Geometry }
function ml_distance2d(x1, y1, x2, y2: Double): Double; cdecl;
begin
  Result := TMathLib.Distance2D(x1, y1, x2, y2);
end;

function ml_distance3d(x1, y1, z1, x2, y2, z2: Double): Double; cdecl;
begin
  Result := TMathLib.Distance3D(x1, y1, z1, x2, y2, z2);
end;

function ml_pointincircle(px, py, cx, cy, radius: Double): Boolean; cdecl;
begin
  Result := TMathLib.PointInCircle(px, py, cx, cy, radius);
end;

{ Number Properties }
function ml_iseven(n: LongInt): Boolean; cdecl;
begin
  Result := TMathLib.IsEven(n);
end;

function ml_isodd(n: LongInt): Boolean; cdecl;
begin
  Result := TMathLib.IsOdd(n);
end;

function ml_isprime(n: LongInt): Boolean; cdecl;
begin
  Result := TMathLib.IsPrime(n);
end;

function ml_gcd(a, b: LongInt): LongInt; cdecl;
begin
  Result := TMathLib.GCD(a, b);
end;

function ml_lcm(a, b: LongInt): LongInt; cdecl;
begin
  Result := TMathLib.LCM(a, b);
end;

exports
  { Basic Arithmetic }
  ml_add,
  ml_subtract,
  ml_multiply,
  ml_divide,
  ml_power,
  ml_modulo,

  { Trigonometric }
  ml_sin,
  ml_cos,
  ml_tan,
  ml_arcsin,
  ml_arccos,
  ml_arctan,
  ml_arctan2,

  { Hyperbolic }
  ml_sinh,
  ml_cosh,
  ml_tanh,

  { Exponential and Logarithmic }
  ml_exp,
  ml_ln,
  ml_log10,
  ml_log2,
  ml_logn,

  { Root Functions }
  ml_sqrt,
  ml_cuberoot,
  ml_nthroot,

  { Rounding }
  ml_round,
  ml_floor,
  ml_ceil,
  ml_trunc,
  ml_frac,

  { Absolute and Sign }
  ml_abs,
  ml_sign,

  { Min/Max }
  ml_min,
  ml_max,
  ml_clamp,

  { Constants }
  ml_pi,
  ml_e,
  ml_goldenratio,

  { Random }
  ml_random,
  ml_random_range,

  { Factorial and Combinatorics }
  ml_factorial,
  ml_permutation,
  ml_combination,

  { Angle Conversion }
  ml_degtorad,
  ml_radtodeg,

  { Distance and Geometry }
  ml_distance2d,
  ml_distance3d,
  ml_pointincircle,

  { Number Properties }
  ml_iseven,
  ml_isodd,
  ml_isprime,
  ml_gcd,
  ml_lcm;

begin
  { Library initialization }
end.
