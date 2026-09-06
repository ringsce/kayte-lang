unit KayteMath;

interface

uses
  SysUtils;

function Add(a, b: Integer): Integer;
function Sub(a, b: Integer): Integer;
function Mult(a, b: Integer): Integer;
function Divi(a, b: Integer): Integer;
function Modu(a, b: Integer): Integer;

implementation

function Add(a, b: Integer): Integer;
begin
  Result := a + b;
end;

function Sub(a, b: Integer): Integer;
begin
  Result := a - b;
end;

function Mult(a, b: Integer): Integer;
begin
  Result := a * b;
end;

function Divi(a, b: Integer): Integer;
begin
  if b = 0 then
    raise Exception.Create('Division by zero');
  Result := a div b;
end;

function Modu(a, b: Integer): Integer;
begin
  if b = 0 then
    raise Exception.Create('Modulus by zero');
  Result := a mod b;
end;

end.

