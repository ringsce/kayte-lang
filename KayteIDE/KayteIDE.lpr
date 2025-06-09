program KayteIDE; // This is the name of your executable

{$mode objfpc}{$H+} // Standard Free Pascal project mode

uses
  Interfaces, // Required for GUI applications
  Forms,      // Required for GUI forms
  KayteIDEUnit; // <--- This refers to your form's unit (the one we just modified)

{$R *.res} // Standard resource file for Windows, often automatically generated

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm); // Creates an instance of TMainForm
  Application.Run; // Runs the application main loop
end.
