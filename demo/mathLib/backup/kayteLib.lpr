library kayteLib;

{$mode objfpc}{$H+}

uses
  Classes
  { you can add units after this };
const
  zon =-5.0;

type
  name = Array [1..3] of char;

var
timezon, frac, secs : real;
i, i1,i2,i3,id, im, iy : integer;
j1, j2, n, nph : integer;
phase : Array [0..3] of name;

begin
  (*[]*)
timezon:= zon/24.0;
phase [0] := 'new moon';
phase [1] := 'first quarter';
phase [2] := 'full moon';
phase [3] := 'last quarter';



end.

