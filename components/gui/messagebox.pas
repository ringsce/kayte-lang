unit messagebox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TMessageBoxComponent = class(TComponent)
  public
    procedure ShowMessage(const ATitle, AMessage: String);
  end;

procedure Register;

implementation

procedure TMessageBoxComponent.ShowMessage(const ATitle, AMessage: String);
begin
  Writeln('=== ', ATitle, ' ===');
  Writeln(AMessage);
  Writeln('[OK]');
end;

procedure Register;
begin
  // No Lazarus IDE, so RegisterComponents is not needed
end;

end.
