unit Main;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ExtCtrls, ImgList, StdCtrls, LazIntfImage, FPImage, FPReadPNG, FPImgCanv, IniFiles;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuEdit: TMenuItem;
    MenuView: TMenuItem;
    MenuBuild: TMenuItem;
    MenuHelp: TMenuItem;
    Memo1: TMemo;
    ToolBar1: TToolBar;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure LoadIcons;
  public
    procedure LoadProject(const FileName: string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  LoadIcons;
end;

procedure TMainForm.LoadIcons;
var
  i: Integer;
  names: array[0..4] of string = ('build','clean','run','debug','save');
  reader: TFPReaderPNG;
  img: TLazIntfImage;
  bmp: TBitmap;
begin
  ImageList1.Clear;
  reader := TFPReaderPNG.Create;
  try
    for i := 0 to High(names) do
    begin
      img := TLazIntfImage.Create(32, 32, [riqfRGB]);
      img.LoadFromFile('ide_icons/' + names[i] + '.png', reader);
      bmp := TBitmap.Create;
      bmp.LoadFromIntfImage(img);
      ImageList1.Add(bmp, nil);
      bmp.Free;
      img.Free;
    end;
  finally
    reader.Free;
  end;
  ToolBar1.Images := ImageList1;
end;

procedure TMainForm.LoadProject(const FileName: string);
var
  Ini: TIniFile;
  MainFile, SourceFiles, CompilerOptions: string;
begin
  Ini := TIniFile.Create(FileName);
  try
    MainFile := Ini.ReadString('KayteIDE', 'MainFile', '');
    SourceFiles := Ini.ReadString('KayteIDE', 'SourceFiles', '');
    CompilerOptions := Ini.ReadString('KayteIDE', 'CompilerOptions', '');
    ShowMessage('Project Loaded:\nMain: ' + MainFile + '\nSources: ' + SourceFiles + '\nOptions: ' + CompilerOptions);
  finally
    Ini.Free;
  end;
end;

end.