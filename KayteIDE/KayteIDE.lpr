program KayteIDE;

uses
  Forms, Interfaces, Main in 'Main.pas' {MainForm};

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.