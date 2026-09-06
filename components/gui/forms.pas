unit Forms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TForm = class
  private
    FName: String;
  public
    constructor Create(AOwner: TComponent; const AName: String);
    destructor Destroy; override;
    procedure Show; virtual;
    property Name: String read FName;
  end;

var
  // lets the interpreter look forms up by name
  FormsList: TObjectList;

procedure ApplicationInitialize;

implementation

constructor TForm.Create(AOwner: TComponent; const AName: String);
begin
  inherited Create(AOwner);
  FName := AName;
  FormsList.Add(Self);
  Writeln('Form "' + FName + '" created.');
end;

destructor TForm.Destroy;
begin
  FormsList.Remove(Self);
  Writeln('Form "' + FName + '" destroyed.');
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
  FormsList := TObjectList.Create(True); // OwnsObjects=True: frees TForm instances when removed/cleared
finalization
  FreeAndNil(FormsList);
end.