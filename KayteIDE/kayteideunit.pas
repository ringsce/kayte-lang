unit KayteIDEUnit; // Renamed from Main

{$mode objfpc}{$H+} // Standard Free Pascal project mode

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ExtCtrls, ImgList, StdCtrls,
  FPImage,     // Provides TFPImage, TBitmap drawing functionality
  FPReadJPG,   // <--- NEW: For reading JPEG files
  FPImgCanv,   // Provides drawing helpers like img.Draw()
  IniFiles;

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
  reader: TFPReaderJPG; // <--- Changed to TFPReaderJPG
  img: TFPImage;
  bmp: TBitmap;
begin
  ImageList1.Clear;
  reader := TFPReaderJPG.Create; // <--- Create JPEG reader
  try
    for i := 0 to High(names) do
    begin
      img := TFPImage.Create;
      try
        // Load the JPEG file into the TFPImage
        // IMPORTANT: Ensure your icons are now .jpeg or .jpg files in 'ide_icons/'
        img.LoadFromFile('ide_icons/' + names[i] + '.jpeg', reader); // <--- Changed file extension

        bmp := TBitmap.Create;
        try
          bmp.Width := img.Width;
          bmp.Height := img.Height;
          img.Draw(bmp.Canvas, 0, 0); // This is the correct way to transfer image data
          ImageList1.Add(bmp, nil);
        finally
          bmp.Free;
        end;
      finally
        img.Free;
      end;
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
    ShowMessage('Project Loaded:' + LineEnding + 'Main: ' + MainFile + LineEnding + 'Sources: ' + SourceFiles + LineEnding + 'Options: ' + CompilerOptions);
  finally
    Ini.Free;
  end;
end;

end.
