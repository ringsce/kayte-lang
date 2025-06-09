program KayteIDE;

uses
  Forms, Interfaces, Main in 'Main.pas', {MainForm}
  SynVBHighlighter in '../source/SynVBHighlighter.pas';

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
