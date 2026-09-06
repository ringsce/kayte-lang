unit SimpleHTTPServer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, HTTPDefs, fpWeb,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  KayteParser;  // KayteParser processes .kayte files

type
  TSimpleHTTPServer = class
  private
    FServer: TFPHTTPServer;
    procedure OnRequestHandler(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    function ParseKayteFile(const FilePath: string): string;
  public
    constructor Create(APort: Integer);
    procedure StartServer;
    procedure StopServer;
  end;

implementation

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
{ ExpandFileName only normalizes ".." lexically - it does not follow
  symlinks. A symlink planted inside BaseDir (e.g. "public/escape ->
  /etc") would pass the textual prefix check below while actually
  serving files outside BaseDir. This walks every path component
  between BaseDir and the target and rejects the request if any of
  them is a symlink, rather than trying to resolve where it points -
  fail closed instead of chasing the link. }
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
  it. Returns '' if the resulting path would fall outside BaseDir. }
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

{ TSimpleHTTPServer }

constructor TSimpleHTTPServer.Create(APort: Integer);
begin
  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := APort;
  FServer.OnRequest := @OnRequestHandler;
end;

function TSimpleHTTPServer.ParseKayteFile(const FilePath: string): string;
var
  KayteContent, ParsedContent: string;
  KayteParser: TKayteParser;
begin
  KayteContent := LoadFileAsString(FilePath);
  KayteParser := TKayteParser.Create;
  try
    ParsedContent := KayteParser.Parse(KayteContent);
  finally
    KayteParser.Free;
  end;
  Result := ParsedContent;
end;

procedure TSimpleHTTPServer.OnRequestHandler(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  FilePath, ContentType, RequestedURI: string;
  ParsedKayteContent: string;
begin
  RequestedURI := ARequest.URI;
  if RequestedURI = '/' then
    RequestedURI := '/index.kayte';  // Default to index.kayte if no specific file is requested

  FilePath := SafeResolvePath('.', RequestedURI);

  if (FilePath <> '') and FileExists(FilePath) then
  begin
    if LowerCase(ExtractFileExt(FilePath)) = '.kayte' then
    begin
      ParsedKayteContent := ParseKayteFile(FilePath);
      AResponse.ContentType := 'text/html';
      AResponse.Content := ParsedKayteContent;
    end
    else
    begin
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
    AResponse.Code := 404;
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

