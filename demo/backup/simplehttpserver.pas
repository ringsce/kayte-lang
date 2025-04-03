unit SimpleHTTPServer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, HTTPDefs, fpWeb, KayteParser;  // KayteParser processes .kayte files

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

{ Utility function to load a file into a string }
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

{ TSimpleHTTPServer }

constructor TSimpleHTTPServer.Create(APort: Integer);
begin
  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := APort;
  FServer.OnRequest := @OnRequestHandler;
end;

{ Parses a .kayte file and returns its HTML representation }
function TSimpleHTTPServer.ParseKayteFile(const FilePath: string): string;
var
  KayteContent, ParsedContent: string;
  KayteParser: TKayteParser;
begin
  KayteContent := LoadFileAsString(FilePath);  // Load .kayte content
  KayteParser := TKayteParser.Create;
  try
    ParsedContent := KayteParser.Parse(KayteContent);  // Parse the content
  finally
    KayteParser.Free;
  end;
  Result := ParsedContent;
end;

{ Handles incoming HTTP requests }
procedure TSimpleHTTPServer.OnRequestHandler(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  FilePath, ContentType: string;
  ParsedKayteContent: string;
begin
  FilePath := '.' + ARequest.URI;
  if (ARequest.URI = '/') then
    FilePath := './index.kayte';  // Default to index.kayte if no specific file is requested

  if FileExists(FilePath) then
  begin
    if LowerCase(ExtractFileExt(FilePath)) = '.kayte' then
    begin
      // Handle .kayte file and convert to HTML
      ParsedKayteContent := ParseKayteFile(FilePath);
      AResponse.ContentType := 'text/html';
      AResponse.Content := ParsedKayteContent;
    end
    else
    begin
      // Handle regular files
      ContentType := 'text/html';
      if LowerCase(ExtractFileExt(FilePath)) = '.css' then
        ContentType := 'text/css'
      else if LowerCase(ExtractFileExt(FilePath)) = '.js' then
        ContentType := 'application/javascript';

      AResponse.ContentStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
      AResponse.ContentType := ContentType;
    end;
  end
  else
  begin
    AResponse.Content := '404 Not Found';
    AResponse.Code := 404;  // Correctly set HTTP status code
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
  FreeAndNil(FServer);
  Writeln('HTTP server stopped');
end;

end.

