unit SimpleHTTPServer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, HTTPDefs, fpWeb, process,
  KayteParser in '../source/KayteParser.pas'; // KayteParser processes .kayte files

type
  TSimpleHTTPServer = class
  private
    FServer: TFPHTTPServer;
    FNodeProcess: TProcess; // To manage the Node.js server process

    procedure OnRequestHandler(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    function ParseKayteFile(const FilePath: string): string;

  public
    constructor Create(APort: Integer);
    destructor Destroy; override;
    procedure StartServer;
    procedure StopServer;
    procedure StartNodeServer; // New: Starts a Node.js server
    function CheckForNode: Boolean; // Checks if Node.js is installed
    function CheckForNpm: Boolean;  // Checks if npm is installed
  end;

implementation

{ Utility function to load a file into a string }
function LoadFileAsString(const FileName: string): string;
var
  FileStream: TFileStream;
begin
  FileStream := nil;
  try
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    SetLength(Result, FileStream.Size);
    if FileStream.Size > 0 then
    begin
      FileStream.Read(Result[1], FileStream.Size);
    end;
  finally
    FileStream.Free;
  end;
end;

{ Helper function to check if a command-line tool exists }
function CheckForTool(const ToolName: string): Boolean;
var
  P: TProcess;
begin
  Result := False;
  P := nil;
  try
    P := TProcess.Create(nil);
    P.Executable := ToolName;
    P.Parameters.Add('-v'); // Use -v for version check, a common practice
    P.Options := [poUsePipes, poNoConsole]; // Hide console output
    P.Execute;
    if P.ExitStatus = 0 then
      Result := True;
  finally
    FreeAndNil(P);
  end;
end;

{ TSimpleHTTPServer }

constructor TSimpleHTTPServer.Create(APort: Integer);
begin
  inherited Create;
  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := APort;
  FServer.OnRequest := @OnRequestHandler;
  FNodeProcess := nil;
end;

destructor TSimpleHTTPServer.Destroy;
begin
  FreeAndNil(FServer);
  if Assigned(FNodeProcess) then
    FNodeProcess.Terminate;
  FreeAndNil(FNodeProcess);
  inherited Destroy;
end;

{ Parses a .kayte file and returns its HTML representation }
function TSimpleHTTPServer.ParseKayteFile(const FilePath: string): string;
var
  KayteContent: string;
  KayteParser: TKayteParser;
  SourceList: TStringList;
begin
  Result := '';
  SourceList := nil;
  KayteParser := nil;
  try
    KayteContent := LoadFileAsString(FilePath);
    SourceList := TStringList.Create;
    SourceList.Text := KayteContent;

    KayteParser := TKayteParser.Create;
    // Assuming TKayteParser.Parse takes a TStringList and returns a string
    Result := KayteParser.Parse(SourceList);
  finally
    FreeAndNil(KayteParser);
    FreeAndNil(SourceList);
  end;
end;

{ Handles incoming HTTP requests }
procedure TSimpleHTTPServer.OnRequestHandler(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  FilePath, ContentType: string;
begin
  FilePath := '.' + ARequest.URI;
  if (ARequest.URI = '/') then
    FilePath := './example.kayte'; // Default to example.kayte

  if FileExists(FilePath) then
  begin
    if LowerCase(ExtractFileExt(FilePath)) = '.kayte' then
    begin
      // Handle .kayte file and convert to HTML
      AResponse.ContentType := 'text/html';
      AResponse.Content := ParseKayteFile(FilePath);
    end
    else
    begin
      // Handle regular files
      ContentType := 'text/html';
      if LowerCase(ExtractFileExt(FilePath)) = '.css' then
        ContentType := 'text/css'
      else if LowerCase(ExtractFileExt(FilePath)) = '.js' then
        ContentType := 'application/javascript';

      AResponse.ContentType := ContentType;
      AResponse.ContentStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
      // FPC automatically frees ContentStream after sending
    end;
    AResponse.Code := 200; // OK
  end
  else
  begin
    AResponse.Code := 404;
    AResponse.Content := '404 Not Found';
  end;
end;

procedure TSimpleHTTPServer.StartServer;
begin
  Writeln('Starting Pascal HTTP server on port ', FServer.Port);
  FServer.Active := True;
end;

procedure TSimpleHTTPServer.StopServer;
begin
  Writeln('Stopping Pascal HTTP server...');
  FServer.Active := False;
end;

function TSimpleHTTPServer.CheckForNode: Boolean;
begin
  Result := CheckForTool('node');
end;

function TSimpleHTTPServer.CheckForNpm: Boolean;
begin
  Result := CheckForTool('npm');
end;

procedure TSimpleHTTPServer.StartNodeServer;
var
  AppJSPath: string;
begin
  if not Assigned(FNodeProcess) then
  begin
    AppJSPath := 'app.js';
    Writeln('Attempting to start Node.js server at: ', AppJSPath);

    FNodeProcess := TProcess.Create(nil);
    FNodeProcess.Executable := 'node';
    FNodeProcess.Parameters.Add(AppJSPath);
    FNodeProcess.Options := [poUsePipes, poNoConsole]; // Hide console output

    try
      FNodeProcess.Execute;
      Writeln('Node.js server process started successfully.');
    except
      on E: Exception do
      begin
        Writeln('Failed to start Node.js server: ', E.Message);
        FreeAndNil(FNodeProcess);
      end;
    end;
  end
  else
  begin
    Writeln('Node.js server is already running.');
  end;
end;

end.

