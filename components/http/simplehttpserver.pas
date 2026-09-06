unit SimpleHTTPServer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, HTTPDefs, fpWeb, process,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
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
    procedure StartNodeServer;
    function CheckForNode: Boolean;
    function CheckForNpm: Boolean;
  end;

implementation

{ Decodes percent-encoded sequences (e.g. "%2e" -> ".") in a request
  path, so an encoded traversal attempt (e.g. "%2e%2e/") is normalized
  before the safety check below, not just a literal ".." sequence. }
function URLDecodePath(const S: string): string;
var
  I, CharCode: Integer;
begin
  Result := '';
  I := 1;
  while I <= Length(S) do
  begin
    if (S[I] = '%') and (I + 2 <= Length(S)) and
       TryStrToInt('$' + Copy(S, I + 1, 2), CharCode) then
    begin
      Result := Result + Chr(CharCode);
      Inc(I, 3);
    end
    else
    begin
      Result := Result + S[I];
      Inc(I);
    end;
  end;
end;

{$IFDEF UNIX}
{ ExpandFileName only normalizes "..'/"." lexically - it does not
  follow symlinks. So a symlink planted inside BaseDir (e.g.
  "public/escape -> /etc") would pass the textual prefix check below
  while actually serving files outside BaseDir. This walks every path
  component between BaseDir and the target and rejects the request if
  any of them is a symlink, rather than trying to resolve where it
  points - fail closed instead of chasing the link. }
function PathHasSymlinkComponent(const BaseDir, FullPath: string): Boolean;
var
  Rel, Check, Part: string;
  Info: Stat;
  P: Integer;
begin
  Result := False;
  Rel := Copy(FullPath, Length(BaseDir) + 1, MaxInt);
  Check := ExcludeTrailingPathDelimiter(BaseDir);
  while Length(Rel) > 0 do
  begin
    P := Pos(PathDelim, Rel);
    if P = 0 then
    begin
      Part := Rel;
      Rel := '';
    end
    else
    begin
      Part := Copy(Rel, 1, P - 1);
      Rel := Copy(Rel, P + 1, MaxInt);
    end;
    if Part = '' then
      Continue;
    Check := Check + PathDelim + Part;
    if (FpLStat(Check, Info) = 0) and FPS_ISLNK(Info.st_mode) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;
{$ENDIF}

{ Resolves a request URI to a file path confined to BaseDir, refusing
  to leave BaseDir via "..", encoded traversal sequences, backslashes,
  NUL bytes, or a symlink planted inside BaseDir that points outside
  it. Returns '' if the resulting path would fall outside BaseDir.
  Fixes a directory-traversal bug where ARequest.URI was concatenated
  directly into a file path with no containment check, letting a
  request like "GET /../../../../etc/passwd" read any file readable
  by the server process. }
function SafeResolvePath(const BaseDir, RequestURI: string): string;
var
  CleanURI, Decoded, Candidate, ExpandedBase, ExpandedCandidate: string;
  QueryPos: Integer;
begin
  Result := '';
  CleanURI := RequestURI;
  QueryPos := Pos('?', CleanURI);
  if QueryPos > 0 then
    CleanURI := Copy(CleanURI, 1, QueryPos - 1);

  Decoded := URLDecodePath(CleanURI);
  if (Pos(#0, Decoded) > 0) or (Pos('\', Decoded) > 0) then
    Exit;

  Candidate := BaseDir + Decoded;
  ExpandedBase := IncludeTrailingPathDelimiter(ExpandFileName(BaseDir));
  ExpandedCandidate := ExpandFileName(Candidate);

  if Copy(ExpandedCandidate, 1, Length(ExpandedBase)) <> ExpandedBase then
    Exit;

  {$IFDEF UNIX}
  if PathHasSymlinkComponent(ExpandedBase, ExpandedCandidate) then
    Exit;
  {$ENDIF}

  Result := ExpandedCandidate;
end;

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

function CheckForTool(const ToolName: string): Boolean;
var
  P: TProcess;
begin
  Result := False;
  P := nil;
  try
    P := TProcess.Create(nil);
    P.Executable := ToolName;
    P.Parameters.Add('-v'); // most CLI tools accept -v for a version check
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
  // TProcess's own destructor already handles terminating the process,
  // so there's no need to call Terminate explicitly here
  FreeAndNil(FNodeProcess);
  inherited Destroy;
end;

function TSimpleHTTPServer.ParseKayteFile(const FilePath: string): string;
var
  KayteContent: string;
  KayteParser: TKayteParser;
begin
  Result := '';
  KayteParser := nil;
  try
    KayteContent := LoadFileAsString(FilePath);

    KayteParser := TKayteParser.Create;
    Result := KayteParser.Parse(KayteContent);
  finally
    FreeAndNil(KayteParser);
  end;
end;

procedure TSimpleHTTPServer.OnRequestHandler(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  FilePath, ContentType, RequestedURI: string;
begin
  RequestedURI := ARequest.URI;
  if RequestedURI = '/' then
    RequestedURI := '/example.kayte';

  FilePath := SafeResolvePath('.', RequestedURI);

  if (FilePath <> '') and FileExists(FilePath) then
  begin
    if LowerCase(ExtractFileExt(FilePath)) = '.kayte' then
    begin
      AResponse.ContentType := 'text/html';
      AResponse.Content := ParseKayteFile(FilePath);
    end
    else
    begin
      ContentType := 'text/html';
      if LowerCase(ExtractFileExt(FilePath)) = '.css' then
        ContentType := 'text/css'
      else if LowerCase(ExtractFileExt(FilePath)) = '.js' then
        ContentType := 'application/javascript';

      AResponse.ContentType := ContentType;
      AResponse.ContentStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
      // FPC automatically frees ContentStream after sending
    end;
    AResponse.Code := 200;
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

