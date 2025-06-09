unit Main;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ExtCtrls, ImgList, StdCtrls, FPImage, FPReadTIFF, FPImgCanv, IniFiles; // Replaced LazIntfImage and FPReadPNG with FPReadTIFF

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
  reader: TFPReaderTIFF; // Changed to TFPReaderTIFF
  img: TFPImage;         // Changed to TFPImage
  bmp: TBitmap;
begin
  ImageList1.Clear;
  reader := TFPReaderTIFF.Create; // Create TIFF reader
  try
    for i := 0 to High(names) do
    begin
      img := TFPImage.Create; // Create FPImage instance
      try
        // Load the TIFF file into the TFPImage
        // Make sure your icon files are now .tiff (e.g., 'ide_icons/build.tiff')
        img.LoadFromFile('ide_icons/' + names[i] + '.tiff', reader);

        bmp := TBitmap.Create;
        try
          // Convert TFPImage to TBitmap
          // We need to ensure the bitmap has the correct size before drawing
          bmp.SetSize(img.Width, img.Height);
          // Draw the FPImage content onto the bitmap's canvas
          img.Draw(bmp.Canvas, 0, 0);
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
