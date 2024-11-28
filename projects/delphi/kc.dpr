program kc;

uses
  System.StartUpCopy,
  FMX.Forms,
  kayte in 'kayte.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
