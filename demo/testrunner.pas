program TestRunner;

{$mode objfpc}{$H+}

uses
  fpcunit, testregistry, TestBytecode; // Use correct units for fpcunit

begin
  RunRegisteredTests;
end.

