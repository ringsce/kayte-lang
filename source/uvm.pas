unit UVM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls; // Keep Forms, StdCtrls if your VM needs to
                                    // directly reference types like TForm, TEdit for UI operations.

type
  // Remove the forward declaration for TMainInterpreterForm.
  // It's no longer needed in this unit's interface.
  // TMainInterpreterForm = class;

  IInterpreterCallback = interface
    // IMPORTANT: Generate a new unique GUID for your interface!
    // You can do this in Lazarus: Tools -> Generate GUID
    ['{PUT-YOUR-GENERATED-GUID-HERE}'] // Example: ['{5181D5E9-2C62-427D-A104-511B612C208E}']
    procedure Log(const Msg: String);
  end;

  TVM = class
  private
    // Remove FMainForm: TMainInterpreterForm;
    FCallback: IInterpreterCallback; // The VM now holds a reference to the interface

  public
    // Remove the constructor that took TMainInterpreterForm
    // constructor Create(AMainForm: TMainInterpreterForm);
    destructor Destroy; override;
    // Use this constructor to pass the callback interface
    constructor Create(ACallback: IInterpreterCallback);
    procedure ExecuteInterpretedFunction(const AFunctionName: String);
  end;

  // --- Forward declarations for Parser-related types (if they're in a separate unit) ---
  // If TLexer, TToken, TTokenType, TBCValue are defined in a separate unit (e.g., UParserTypes.pas),
  // then you'd need to 'use' that unit here.
  // For now, let's include dummy definitions to make this unit compile in isolation.
  TLexer = class; // Dummy forward for TLexer
  TTokenType = (ttUnknown, ttIdentifier, ttString, ttInteger, ttKeyword); // Example dummy enum
  TToken = record
    TokenType: TTokenType;
    Value: String;
    Line: Integer;
    Column: Integer;
  end; // Example dummy record for TToken
  TBCValue = class; // Dummy forward for TBCValue

  // TParser class definition (assuming it's in UVM.pas for now, or moved to UParser.pas later)
  TParser = class
  private
    FLexer: TLexer;
    FCurrentToken: TToken;

    procedure ConsumeToken(ExpectedType: TTokenType);
    function PeekToken: TToken;

    procedure ParsePrintStatement;
    procedure ParseShowStatement;
    procedure ParseLetStatement(const VarName: String);
    procedure ParseIfStatement;

    function ParseExpression: TBCValue;
    function ParseTerm: TBCValue;
    function ParseFactor: TBCValue;

  public
    constructor Create(ALexer: TLexer);
    destructor Destroy; override;
    procedure ParseProgram; // This would typically parse the Kayte source code
  end;

implementation

uses
  UKfrmRuntime; // <--- UVM needs to 'use' UKfrmRuntime to interact with forms

// Implementations for TVM methods
{ TVM }

constructor TVM.Create(ACallback: IInterpreterCallback);
begin
  inherited Create;
  FCallback := ACallback; // Store the interface reference
  // Log directly via the callback
  if Assigned(FCallback) then
    FCallback.Log('VM: Virtual Machine created and initialized.');
end;

destructor TVM.Destroy;
begin
  if Assigned(FCallback) then
    FCallback.Log('VM: Virtual Machine destroyed.');
  FCallback := nil; // Release the interface reference
  inherited Destroy;
end;

procedure TVM.ExecuteInterpretedFunction(const AFunctionName: String);
var
  LoadedLoginForm: TForm;
  UsernameEdit: TEdit;
  PasswordEdit: TEdit;
begin
  if Assigned(FCallback) then
    FCallback.Log(SysUtils.Format('VM: Attempting to execute interpreted function: %s', [AFunctionName]));

  // --- Implement your VM's logic here to find and execute the function ---
  // This would involve looking up 'AFunctionName' in your interpreter's symbol table.
  // To interact with the UI (e.g., read/write control values), the VM needs access to FKfrmRuntime.
  // One way to provide this is to make FKfrmRuntime a global variable
  // in UMainInterpreterForm.pas (var FKfrmRuntime: TKfrmRuntime;)
  // and then 'uses UMainInterpreterForm;' in this implementation section.
  // Or, pass a reference to FKfrmRuntime to TVM during its creation.
  // For now, let's assume FKfrmRuntime is globally accessible (e.g., from UMainInterpreterForm)
  // or passed to the VM (which would require updating the TVM constructor).

  // Example (needs FKfrmRuntime accessible):
  if AFunctionName = 'HandleLoginButton' then
  begin
    if Assigned(FCallback) then
      FCallback.Log('VM: Executing interpreted HandleLoginButton logic.');

    // THIS PART NEEDS ACCESS TO FKfrmRuntime.
    // If MainInterpreterForm.FKfrmRuntime is globally exposed:
    // LoadedLoginForm := MainInterpreterForm.FKfrmRuntime.GetLoadedForm('LoginForm');
    // UsernameEdit := TEdit(MainInterpreterForm.FKfrmRuntime.GetControlFromForm(LoadedLoginForm, 'EditUsername'));
    // PasswordEdit := TEdit(MainInterpreterForm.FKfrmRuntime.GetControlFromForm(LoadedLoginForm, 'EditPassword'));
    //
    // A cleaner way: Pass FKfrmRuntime to TVM during its creation, or give TVM an IFormRuntime interface.

    // For now, let's use a dummy message for this example if FKfrmRuntime isn't passed:
    if Assigned(FCallback) then
        FCallback.Log('VM: (Simulating) Login logic for HandleLoginButton.');

    // Replace with actual logic when FKfrmRuntime is accessible
    // if Assigned(LoadedLoginForm) and Assigned(UsernameEdit) and Assigned(PasswordEdit) then
    // begin
    //   FCallback.Log(SysUtils.Format('Interpreted Login - Username: "%s", Password: "%s"', [UsernameEdit.Text, PasswordEdit.Text]));
    //   if (UsernameEdit.Text = 'admin') and (PasswordEdit.Text = 'password') then
    //     FCallback.Log('VM: Login Successful (interpreted)')
    //   else
    //     FCallback.Log('VM: Login Failed (interpreted)');
    // end;
  end
  else if AFunctionName = 'HandleCancelButton' then
  begin
    if Assigned(FCallback) then
      FCallback.Log('VM: Executing interpreted HandleCancelButton logic. Closing form.');

    // THIS PART NEEDS ACCESS TO FKfrmRuntime.
    // For now, use dummy message:
    if Assigned(FCallback) then
        FCallback.Log('VM: (Simulating) Closing form logic for HandleCancelButton.');
    // MainInterpreterForm.FKfrmRuntime.CloseKfrmForm('LoginForm'); // Requires global FKfrmRuntime
  end
  else
  begin
    if Assigned(FCallback) then
      FCallback.Log(SysUtils.Format('VM: Interpreted function "%s" not found or not handled by VM.', [AFunctionName]));
  end;
end;

// Implementations for TParser methods (dummy for now)
{ TParser }

constructor TParser.Create(ALexer: TLexer);
begin
  inherited Create;
  FLexer := ALexer;
end;

destructor TParser.Destroy;
begin
  inherited Destroy;
end;

procedure TParser.ConsumeToken(ExpectedType: TTokenType);
begin
  // Dummy implementation
end;

function TParser.PeekToken: TToken;
begin
  // Dummy implementation
  Result.TokenType := ttUnknown;
  Result.Value := '';
end;

procedure TParser.ParsePrintStatement;
begin
  // Dummy implementation
end;

procedure TParser.ParseShowStatement;
begin
  // Dummy implementation
end;

procedure TParser.ParseLetStatement(const VarName: String);
begin
  // Dummy implementation
end;

procedure TParser.ParseIfStatement;
begin
  // Dummy implementation
end;

function TParser.ParseExpression: TBCValue;
begin
  Result := nil; // Dummy
end;

function TParser.ParseTerm: TBCValue;
begin
  Result := nil; // Dummy
end;

function TParser.ParseFactor: TBCValue;
begin
  Result := nil; // Dummy
end;

procedure TParser.ParseProgram;
begin
  // Dummy implementation
end;

end.
