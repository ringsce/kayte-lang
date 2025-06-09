unit KayteSyntax;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

function HighlightKayteCode(const Code: string): string;
procedure HighlightFileToConsole(const Filename: string);

implementation

const
  // ANSI terminal colors
  COLOR_RESET   = #27'[0m';
  COLOR_KEYWORD = #27'[1;36m'; // cyan
  COLOR_STRING  = #27'[0;32m'; // green
  COLOR_COMMENT = #27'[1;30m'; // dark gray
  COLOR_NUMBER  = #27'[1;33m'; // yellow
  COLOR_IDENT   = #27'[0;37m'; // white

const
  KayteKeywords: array[0..9] of string = (
    'let', 'fn', 'return', 'if', 'else',
    'while', 'for', 'in', 'match', 'import'
  );

function IsKeyword(const Token: string): Boolean;
var
  i: Integer;
begin
  for i := Low(KayteKeywords) to High(KayteKeywords) do
    if Token = KayteKeywords[i] then
      Exit(True);
  Result := False;
end;

function HighlightKayteCode(const Code: string): string;
var
  i, Start: Integer;
  C: Char;
  Token, ResultStr: string;
  InString, InComment: Boolean;
begin
  ResultStr := '';
  i := 1;
  InString := False;
  InComment := False;

  while i <= Length(Code) do
  begin
    C := Code[i];

    // Line comment
    if not InString and not InComment and (C = '/') and (i < Length(Code)) and (Code[i+1] = '/') then
    begin
      ResultStr += COLOR_COMMENT;
      while (i <= Length(Code)) and (Code[i] <> #10) do
      begin
        ResultStr += Code[i];
        Inc(i);
      end;
      ResultStr += COLOR_RESET;
      Continue;
    end;

    // String literal
    if C = '"' then
    begin
      ResultStr += COLOR_STRING + C;
      Inc(i);
      while (i <= Length(Code)) do
      begin
        C := Code[i];
        ResultStr += C;
        if C = '"' then Break;
        Inc(i);
      end;
      ResultStr += COLOR_RESET;
      Inc(i);
      Continue;
    end;

    // Identifier or keyword
    if C in ['A'..'Z', 'a'..'z', '_'] then
    begin
      Start := i;
      while (i <= Length(Code)) and (Code[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
        Inc(i);
      Token := Copy(Code, Start, i - Start);

      if IsKeyword(Token) then
        ResultStr += COLOR_KEYWORD + Token + COLOR_RESET
      else
        ResultStr += COLOR_IDENT + Token + COLOR_RESET;
      Continue;
    end;

    // Number
    if C in ['0'..'9'] then
    begin
      Start := i;
      while (i <= Length(Code)) and (Code[i] in ['0'..'9', '.', 'x']) do
        Inc(i);
      Token := Copy(Code, Start, i - Start);
      ResultStr += COLOR_NUMBER + Token + COLOR_RESET;
      Continue;
    end;

    // Regular character
    ResultStr += C;
    Inc(i);
  end;

  Result := ResultStr;
end;

procedure HighlightFileToConsole(const Filename: string);
var
  Code: TStringList;
begin
  Code := TStringList.Create;
  try
    Code.LoadFromFile(Filename);
    WriteLn(HighlightKayteCode(Code.Text));
  finally
    Code.Free;
  end;
end;

end.

