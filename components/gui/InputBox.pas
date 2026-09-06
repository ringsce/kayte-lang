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