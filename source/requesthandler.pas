unit RequestHandler;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  SimpleHTTPServer; // This unit defines the THTTPServerRequest and THTTPServerResponse types

procedure MyRequestHandler(const ARequest: THTTPServerRequest; var AResponse: THTTPServerResponse);

implementation

procedure MyRequestHandler(const ARequest: THTTPServerRequest; var AResponse: THTTPServerResponse);
begin
  // Set the HTTP status code to 200 (OK)
  AResponse.StatusCode := 200;
  AResponse.StatusText := 'OK';

  // Set the content type to HTML
  AResponse.ContentType := 'text/html';

  // Write a simple HTML page to the response stream
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

