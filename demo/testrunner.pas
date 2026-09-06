program TestRunner;

{$mode objfpc}{$H+}

uses
  fpcunit, testregistry, TestBytecode;

begin
  RunRegisteredTests;
end.

