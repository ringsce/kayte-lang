unit UMainInterpreterForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UVM, // <--- UMainInterpreterForm needs to 'use' UVM because it creates TVM
  UEventRouter, UKfrmRuntime;

type
  { TMainInterpreterForm }
  TMainInterpreterForm = class(TForm, IInterpreterCallback) // Implement the interface
    MemoOutput: TMemo;
    ButtonLoadKfrm: TButton;
    // Add other UI components as needed, e.g., a TMemo for Kayte code input
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonLoadKfrmClick(Sender: TObject);
  private
    { private declarations }
    FVM: TVM;
    FEventRouter: TEventRouter;
    FKfrmRuntime: TKfrmRuntime;
  public
    { public declarations }
    // Property to expose FKfrmRuntime for your VM to interact with forms
    property KfrmRuntime: TKfrmRuntime read FKfrmRuntime;

    // Helper method to log messages to the MemoOutput
    procedure LogMessage(const Msg: String);

    // IInterpreterCallback implementation
    // This method will be called by the VM for logging
    procedure Log(const Msg: String); // Implement the interface method
  end;

var
  MainInterpreterForm: TMainInterpreterForm; // Global instance for easy access

implementation

{$R *.lfm}

{ TMainInterpreterForm }

// Implementation of the IInterpreterCallback.Log method
procedure TMainInterpreterForm.Log(const Msg: String);
begin
  // Simply delegate to the existing LogMessage procedure
  LogMessage(Msg);
end;

procedure TMainInterpreterForm.FormCreate(Sender: TObject);
begin
  // Initialize your interpreter components
  // Pass 'Self' (this instance, cast as IInterpreterCallback) to the VM
  FVM := TVM.Create(Self as IInterpreterCallback); // <--- Pass the interface
  FEventRouter := TEventRouter.Create(FVM);
  FKfrmRuntime := TKfrmRuntime.Create(FEventRouter);

  LogMessage('Kayte Interpreter initialized. Ready to load .kfrm files.');
end;

procedure TMainInterpreterForm.FormDestroy(Sender: TObject);
begin
  // Free interpreter components in reverse order of creation
  FreeAndNil(FKfrmRuntime);
  FreeAndNil(FEventRouter);
  FreeAndNil(FVM);
end;

procedure TMainInterpreterForm.ButtonLoadKfrmClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(Self);
  try
    OpenDialog.Filter := 'Kayte Form Files (*.kfrm)|*.kfrm|All Files (*.*)|*.*';
    // Suggest the 'Forms' subfolder relative to the executable
    OpenDialog.InitialDir := ExtractFilePath(Application.ExeName) + 'Forms';
    if OpenDialog.Execute then
    begin
      LogMessage(SysUtils.Format('MainForm: Loading .kfrm: %s', [OpenDialog.FileName]));
      FKfrmRuntime.ShowKfrmForm(OpenDialog.FileName);
    end;
  finally
    FreeAndNil(OpenDialog);
  end;
end;

procedure TMainInterpreterForm.LogMessage(const Msg: String);
begin
  MemoOutput.Lines.Add(DateTimeToStr(Now) + ': ' + Msg);
  MemoOutput.SelStart := Length(MemoOutput.Text); // Scroll to end
  MemoOutput.SelLength := 0;
end;

end.
