unit Forms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs; // Added Contnrs for TObjectList

type
  TForm = class
  private
    FName: String; // To store the name of the form
  public
    constructor Create(AOwner: TComponent; const AName: String); // Added constructor
    destructor Destroy; override; // Added destructor for proper cleanup
    procedure Show; virtual;
    property Name: String read FName; // Property to access the form's name
  end;

var
  // Global list to keep track of all instantiated forms
  // This helps your interpreter find forms by name.
  FormsList: TObjectList;

procedure ApplicationInitialize;

implementation

constructor TForm.Create(AOwner: TComponent; const AName: String);
begin
  inherited Create(AOwner);
  FName := AName; // Assign the name
  FormsList.Add(Self); // Add this new form instance to the global list
  Writeln('Form "' + FName + '" created.'); // Debug output
end;

destructor TForm.Destroy;
begin
  FormsList.Remove(Self); // Remove from the global list when destroyed
  Writeln('Form "' + FName + '" destroyed.'); // Debug output
  inherited Destroy;
end;

procedure TForm.Show;
begin
  Writeln('--- Displaying Form: ' + FName + ' ---');
  Writeln('  [This is a console-based form stub.]');
  Writeln('-----------------------------------');
end;

procedure ApplicationInitialize;
begin
  Writeln('Application initialized (Forms unit).');
end;

initialization
  FormsList := TObjectList.Create(True); // Create the list, set OwnsObjects to True
                                        // so it frees TForm objects when removed/cleared.
finalization
  FreeAndNil(FormsList); // Free the list itself
end.