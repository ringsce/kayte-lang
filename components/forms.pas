unit Forms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TForm = class
  public
    procedure Show; virtual;
  end;

procedure ApplicationInitialize;

implementation

procedure ApplicationInitialize;
begin
  Writeln('Application initialized.');
end;

procedure TForm.Show;
begin
  Writeln('Form shown (stub).');
end;

end.
// This code defines a basic structure for a form in a Pascal application.
// The `TForm` class has a `Show` method that outputs a message to the console.