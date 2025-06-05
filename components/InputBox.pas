unit inputbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TInputBoxComponent = class(TComponent)
  public
    function Prompt(const ATitle, APrompt, ADefault: String): String;
  end;

procedure Register;

implementation

function TInputBoxComponent.Prompt(const ATitle, APrompt, ADefault: String): String;
var
  UserInput: String;
begin
  Writeln('=== ', ATitle, ' ===');
  Write(APrompt, ' [', ADefault, ']: ');
  ReadLn(UserInput);
  if UserInput = '' then
    Result := ADefault
  else
    Result := UserInput;
end;

procedure Register;
begin
  // No Lazarus IDE, so RegisterComponents is not needed
end;

end.
// This code defines a simple input box component in Pascal that prompts the user for input.
// It includes a method to display a prompt and return the user's input, defaulting to a specified value if no input is given.
// The `Register` procedure is included but does not register the component in a Lazarus IDE context, as it is not applicable here.