unit KayteMath;

interface

uses
  SysUtils;

// Function for addition
function Add(a, b: Integer): Integer;

// Function for subtraction
function Sub(a, b: Integer): Integer;

// Function for multiplication
function Mult(a, b: Integer): Integer;

// Function for integer division
function Divi(a, b: Integer): Integer;

// Function for modulus
function Modu(a, b: Integer): Integer;

implementation

// Function for addition
function Add(a, b: Integer): Integer;
begin
  Result := a + b; // Use 'Result' instead of directly assigning to the function name
end;

// Function for subtraction
function Sub(a, b: Integer): Integer;
begin
  Result := a - b;
end;

// Function for multiplication
function Mult(a, b: Integer): Integer;
begin
  Result := a * b;
end;

// Function for integer division
function Divi(a, b: Integer): Integer;
begin
  if b = 0 then
    raise Exception.Create('Division by zero'); // Raise an exception for division by zero
  Result := a div b;
end;

// Function for modulus
function Modu(a, b: Integer): Integer;
begin
  if b = 0 then
    raise Exception.Create('Modulus by zero'); // Raise an exception for modulus by zero
  Result := a mod b;
end;

end.

