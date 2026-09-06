unit FormGenerator;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  VMUI,       // TVMFormDefinition/TVMControl - in-memory form definition
  KfmParser;  // .kfm file reader/writer

// Writes FormDef to FileName in .kfm format. Thin wrapper kept for
// backwards compatibility - new code should call KfmParser.SaveKfmFile
// directly.
procedure SaveVMFormDefinition(const FormDef: TVMFormDefinition; const FileName: String);

// Builds and saves a small sample form to Form1.kfm, demonstrating the API.
procedure GenerateExampleForm;

implementation

procedure SaveVMFormDefinition(const FormDef: TVMFormDefinition; const FileName: String);
begin
  KfmParser.SaveKfmFile(FormDef, FileName);
end;

procedure GenerateExampleForm;
var
  Form1Def: TVMFormDefinition;
  FormControl, Button1, Label1: TVMControl;
begin
  Form1Def := TVMFormDefinition.Create('Form1');
  try
    FormControl := TVMControl.Create;
    FormControl.Name := 'Form1';
    FormControl.ControlType := vctForm;
    FormControl.AddProperty('Caption', '"My First VM Form"');
    FormControl.AddProperty('Width', '400');
    FormControl.AddProperty('Height', '300');
    FormControl.AddProperty('Top', '100');
    FormControl.AddProperty('Left', '100');
    Form1Def.Controls.Add(FormControl);

    Button1 := TVMControl.Create;
    Button1.Name := 'Button1';
    Button1.ControlType := vctButton;
    Button1.AddProperty('Caption', '"Click Me!"');
    Button1.AddProperty('Left', '50');
    Button1.AddProperty('Top', '50');
    Button1.AddProperty('Width', '100');
    Button1.AddProperty('Height', '25');
    Form1Def.Controls.Add(Button1);

    Label1 := TVMControl.Create;
    Label1.Name := 'Label1';
    Label1.ControlType := vctLabel;
    Label1.AddProperty('Caption', '"Initial Text"');
    Label1.AddProperty('Left', '50');
    Label1.AddProperty('Top', '100');
    Label1.AddProperty('Width', '200');
    Label1.AddProperty('Height', '20');
    Form1Def.Controls.Add(Label1);

    SaveVMFormDefinition(Form1Def, 'Form1.kfm');
  finally
    Form1Def.Free;
  end;
end;

end.
