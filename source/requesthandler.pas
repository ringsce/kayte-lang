unit RequestHandler;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  SimpleHTTPServer;

procedure MyRequestHandler(const ARequest: THTTPServerRequest; var AResponse: THTTPServerResponse);

implementation

procedure MyRequestHandler(const ARequest: THTTPServerRequest; var AResponse: THTTPServerResponse);
begin
  AResponse.StatusCode := 200;
  AResponse.StatusText := 'OK';
  AResponse.ContentType := 'text/html';

  AResponse.ContentStream.WriteString('<html>');
  AResponse.ContentStream.WriteString('<head>');
  AResponse.ContentStream.WriteString('<title>My Pascal Web Server</title>');
  AResponse.ContentStream.WriteString('</head>');
  AResponse.ContentStream.WriteString('<body>');
  AResponse.ContentStream.WriteString('<h1>Hello, World from Free Pascal!</h1>');
  AResponse.ContentStream.WriteString('<p>This page was served by your server.</p>');
  AResponse.ContentStream.WriteString('</body>');
  AResponse.ContentStream.WriteString('</html>');
end;

end.

