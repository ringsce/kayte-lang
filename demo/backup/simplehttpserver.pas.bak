unit SimpleHTTPServer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, HTTPDefs, fpWeb, KayteParser;  // Assume KayteParser is a unit that can process .kayte files

type
  TSimpleHTTPServer = class
  private
    FServer: TFPHTTPServer;
    procedure OnRequestHandler(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    function ParseKayteFile(const FilePath: string): string;  // Method to handle .kayte files
  public
    constructor Create(APort: Integer);
    procedure StartServer;
    procedure StopServer;
  end;

implementation

{ TSimpleHTTPServer }

constructor TSimpleHTTPServer.Create(APort: Integer);
begin
  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := APort;
  FServer.OnRequest := @OnRequestHandler;
end;

function LoadFileAsString(const FileName: string): string;
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  StringStream := TStringStream.Create('');
  try
    StringStream.CopyFrom(FileStream, FileStream.Size);
    Result := StringStream.DataString;
  finally
    FileStream.Free;
    StringStream.Free;
  end;
end;

function TSimpleHTTPServer.ParseKayteFile(const FilePath: string): string;
var
  KayteContent, ParsedContent: string;
  KayteParser: TKayteParser;
begin
  KayteContent := LoadFileAsString(FilePath);  // Load the .kayte file content into a string
  KayteParser := TKayteParser.Create;
  try
    ParsedContent := KayteParser.Parse(KayteContent);  // Call the Parse method with the file content
  finally
    KayteParser.Free;
  end;
  Result := ParsedContent;
end;

procedure TSimpleHTTPServer.OnRequestHandler(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  FilePath, ContentType: string;
  FileStream: TFileStream;
  ParsedKayteContent: string;
begin
  FilePath := '.' + ARequest.URI;
  if (ARequest.URI = '/') then
    FilePath := './index.html';  // Serve default index.html

  if FileExists(FilePath) then
  begin
    ContentType := 'text/html';
    if LowerCase(ExtractFileExt(FilePath)) = '.css' then
      ContentType := 'text/css'
    else if LowerCase(ExtractFileExt(FilePath)) = '.kayte' then
    begin
      ParsedKayteContent := ParseKayteFile(FilePath);
      AResponse.ContentType := 'text/html';  // Serve the parsed .kayte as HTML
      AResponse.Content := ParsedKayteContent;
      //AResponse.ResponseCode := 200;  // Use ResponseCode instead of ResultCode
      Exit;  // Skip the rest of the file handling logic for .kayte files
    end;

    FileStream := TFileStream.Create(FilePath, fmOpenRead);
    try
      AResponse.ContentStream := TMemoryStream.Create;
      AResponse.ContentStream.CopyFrom(FileStream, FileStream.Size);
      AResponse.ContentStream.Position := 0;
      AResponse.ContentType := ContentType;
      //AResponse.ResponseCode := 200;  // Use ResponseCode instead of ResultCode
    finally
      FileStream.Free;
    end;
  end
  else
  begin
    AResponse.Content := '404 Not Found';
    //AResponse.ResponseCode := 404;  // Use ResponseCode instead of ResultCode
  end;
end;

procedure TSimpleHTTPServer.StartServer;
begin
  Writeln('Starting HTTP server on port ', FServer.Port);
  FServer.Active := True;
end;

procedure TSimpleHTTPServer.StopServer;
begin
  FServer.Active := False;
  FServer.Free;
  Writeln('HTTP server stopped');
end;

end.

