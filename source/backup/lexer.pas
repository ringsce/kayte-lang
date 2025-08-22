unit Lexer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TokenDefs; // TokenDefs for TToken, TTokenType

type
  TLexer = class
  private
  FSource: String;
    FPosition: Integer;
    FSourceCode: TStringList;
    FCurrentLineIndex: Integer;
    FCurrentCharIndex: Integer;
    FCurrentLine: String;
    FEOF: Boolean;

    procedure Advance;
    function CurrentChar: Char;
    function PeekChar: Char;
    procedure SkipWhitespace;
    function IsDigit(C: Char): Boolean;
    function IsLetter(C: Char): Boolean;
    function IsIdentifierStart(C: Char): Boolean;
    function IsIdentifierChar(C: Char): Boolean;
    function GetTokenType(const S: String): TTokenType;

  public
    constructor Create(ASourceCode: TStringList);
    destructor Destroy; override;
    procedure Reset;
    function GetNextToken: TToken;
    function PeekNextToken: TToken;


    // --- Added Public Properties for Current Position ---
    property CurrentLine: Integer read FCurrentLineIndex;
    property CurrentColumn: Integer read FCurrentCharIndex;
    // --- End Added Public Properties ---
  end;

implementation

{ TLexer }

constructor TLexer.Create(ASourceCode: TStringList);
begin
  inherited Create;
  FSourceCode := ASourceCode; // Lexer does not own the StringList
  Reset;
end;

destructor TLexer.Destroy;
begin
  inherited Destroy;
end;

procedure TLexer.Reset;
begin
  FCurrentLineIndex := 0;
  FCurrentCharIndex := 0;
  FCurrentLine := '';
  FEOF := False;
  if (FSourceCode.Count > 0) then
    FCurrentLine := FSourceCode[FCurrentLineIndex]
  else
    FEOF := True; // Empty source code
end;

procedure TLexer.Advance;
begin
  if FEOF then Exit;

  Inc(FCurrentCharIndex);
  if FCurrentCharIndex >= Length(FCurrentLine) then
  begin
    Inc(FCurrentLineIndex);
    FCurrentCharIndex := 0;
    if FCurrentLineIndex < FSourceCode.Count then
      FCurrentLine := FSourceCode[FCurrentLineIndex]
    else
      FEOF := True;
  end;
end;

function TLexer.CurrentChar: Char;
begin
  if FEOF then Result := #0 else Result := FCurrentLine[FCurrentCharIndex + 1];
end;

function TLexer.PeekChar: Char;
var
  NextCharIndex: Integer;
  NextLineIndex: Integer;
begin
  if FEOF then Result := #0;

  NextCharIndex := FCurrentCharIndex + 1;
  NextLineIndex := FCurrentLineIndex;

  if NextCharIndex >= Length(FCurrentLine) then
  begin
    NextLineIndex := FCurrentLineIndex + 1;
    NextCharIndex := 0;
    if NextLineIndex < FSourceCode.Count then
      Result := FSourceCode[NextLineIndex][NextCharIndex + 1]
    else
      Result := #0; // End of file
  end
  else
    Result := FCurrentLine[NextCharIndex + 1];
end;

procedure TLexer.SkipWhitespace;
begin
  while (CurrentChar = ' ') or (CurrentChar = #9) do // Space or Tab
    Advance;
end;


procedure TParser.LProgram;
begin
  // Check for the 'option explicit' directive at the beginning of the file.
  if Check(tkOption) then
  begin
    OptionStatement;
  end;

  // The rest of your program parsing logic follows here.
  // For example:
  while not Check(tkEndOfFile) do
  begin
    Statement;
  end;
end;

function TLexer.IsDigit(C: Char): Boolean;
begin
  Result := (C >= '0') and (C <= '9');
end;

function TLexer.IsLetter(C: Char): Boolean;
begin
  Result := ((C >= 'a') and (C <= 'z')) or ((C >= 'A') and (C <= 'Z'));
end;

function TLexer.IsIdentifierStart(C: Char): Boolean;
begin
  Result := IsLetter(C) or (C = '_');
end;

function TLexer.IsIdentifierChar(C: Char): Boolean;
begin
  Result := IsLetter(C) or IsDigit(C) or (C = '_');
end;

function TLexer.GetTokenType(const S: String): TTokenType;
begin
  Result := tkIdentifier; // Default to identifier

  // Convert to uppercase for case-insensitive comparison (VB6 style)
  case AnsiUpperCase(S) of
    // Keywords
    'REM', 'END', 'SUB', 'FUNCTION', 'IF', 'THEN', 'ELSE', 'ELSEIF', 'ENDIF',
    'SELECT', 'CASE', 'END SELECT', 'WHILE', 'WEND', 'FOR', 'NEXT', 'TO', 'STEP',
    'DIM', 'AS', 'REDIM', 'PRESERVE', 'CALL', 'GOTO', 'GOSUB', 'RETURN',
    'PRINT', 'INPUT', 'MSGBOX', 'FORM', 'END FORM', 'SHOW', 'HIDE':
      Result := tkKeyword;
    // Boolean Literals
    'TRUE', 'FALSE':
      Result := tkBooleanLiteral;
    // Operators (basic ones; full list would be larger)
    '+', '-', '*', '/', '=', '<', '>', '<=', '>=', '<>', '&': // '&' for string concat
      Result := tkOperator;
    'AND', 'OR', 'NOT', 'IS': // Logical/comparison operators as keywords
      Result := tkOperator; // Or keep as tkKeyword if you want to distinguish
    '(': Result := tkParenthesisOpen;
    ')': Result := tkParenthesisClose;
    ',': Result := tkComma;
    '.': Result := tkDot;
    ':': Result := tkColon;
    // --- New Keywords for Option Explicit ---
    'OPTION': Result := tkKeywordOption; // Changed from tkOption
    'EXPLICIT': Result := tkKeywordExplicit; // Changed from tkExplicit
    'ON': Result := tkKeywordOn; // Changed from tkOn
    'OFF': Result := tkKeywordOff; // Changed from tkOff
    // --- End New Keywords ---
  end;
end;


function TLexer.GetNextToken: TToken;
var
  StartCol: Integer;
  LexemeBuilder: String;
  CurrentTokType: TTokenType;
  // Store current position to rollback if it's not "Option Explicit On/Off"
  SavedCharIndex : Integer;
  SavedLineIndex : Integer;
  SavedLineContent : String;
begin
  Result.Line := FCurrentLineIndex; // Use .Line and .Column as per TokenDefs
  Result.Column := FCurrentCharIndex;

  // Handle End of File
  if FEOF then
  begin
    Result.TokenType := tkEndOfFile;
    Result.Lexeme := '';
    Exit;
  end;

  SkipWhitespace; // Skip leading whitespace

  StartCol := FCurrentCharIndex;
  Result.Column := StartCol; // Update column number after skipping whitespace

  // Handle End of Line
  if (FCurrentCharIndex >= Length(FCurrentLine)) and (FCurrentLineIndex < FSourceCode.Count) then
  begin
    Advance; // Move to the next line
    Result.TokenType := tkEndOfLine;
    Result.Lexeme := '';
    Exit;
  end;

  // Handle comments starting with ' or REM
  // Check for REM keyword first, as 'REM' is a keyword, but also a comment
  if (AnsiUpperCase(Copy(FCurrentLine, FCurrentCharIndex + 1, 3)) = 'REM') and
     ((FCurrentCharIndex + 3 = Length(FCurrentLine)) or (not IsIdentifierChar(FCurrentLine[FCurrentCharIndex + 4]))) then
  begin
    LexemeBuilder := Copy(FCurrentLine, FCurrentCharIndex + 1, Length(FCurrentLine) - FCurrentCharIndex);
    FCurrentCharIndex := Length(FCurrentLine); // Move to end of line
    Result.TokenType := tkComment;
    Result.Lexeme := LexemeBuilder;
    Exit;
  end
  else if CurrentChar = '''' then // Single quote comment
  begin
    LexemeBuilder := '';
    while (CurrentChar <> #0) and (FCurrentCharIndex < Length(FCurrentLine)) do
    begin
      LexemeBuilder := LexemeBuilder + CurrentChar;
      Advance;
    end;
    Result.TokenType := tkComment;
    Result.Lexeme := LexemeBuilder;
    Exit;
  end;


  // Handle String Literals
  if CurrentChar = '"' then
  begin
    LexemeBuilder := '"';
    Advance; // Consume the opening quote
    while (CurrentChar <> '"') and (CurrentChar <> #0) and (FCurrentCharIndex < Length(FCurrentLine)) do
    begin
      LexemeBuilder := LexemeBuilder + CurrentChar;
      Advance;
    end;
    if CurrentChar = '"' then
    begin
      LexemeBuilder := LexemeBuilder + '"';
      Advance; // Consume the closing quote
      Result.TokenType := tkStringLiteral;
      Result.Lexeme := LexemeBuilder;
      Exit;
    end
    else
      raise Exception.CreateFmt('Lexer Error: Unclosed string literal at %d:%d', [Result.Line + 1, Result.Column + 1]);
  end;

  // Handle Integer Literals
  if IsDigit(CurrentChar) then
  begin
    LexemeBuilder := '';
    while IsDigit(CurrentChar) do
    begin
      LexemeBuilder := LexemeBuilder + CurrentChar;
      Advance;
    end;
    Result.TokenType := tkIntegerLiteral;
    Result.Lexeme := LexemeBuilder;
    Exit;
  end;

  // Handle Identifiers and Keywords
  if IsIdentifierStart(CurrentChar) then
  begin
    LexemeBuilder := '';
    while IsIdentifierChar(CurrentChar) do
    begin
      LexemeBuilder := LexemeBuilder + CurrentChar;
      Advance;
    end;

    CurrentTokType := GetTokenType(LexemeBuilder);

    // --- Special handling for "Option Explicit On/Off" sequence ---
    if (CurrentTokType = tkKeywordOption) then // Changed from tkOption
    begin
      // Store current position to rollback if it's not "Option Explicit On/Off"
      SavedCharIndex := FCurrentCharIndex;
      SavedLineIndex := FCurrentLineIndex;
      SavedLineContent := FCurrentLine;

      SkipWhitespace; // Skip space after "Option"
      LexemeBuilder := '';
      while IsIdentifierChar(CurrentChar) do
      begin
        LexemeBuilder := LexemeBuilder + CurrentChar;
        Advance;
      end;
      if GetTokenType(LexemeBuilder) = tkKeywordExplicit then // Changed from tkExplicit
      begin
        SkipWhitespace; // Skip space after "Explicit"
        LexemeBuilder := '';
        while IsIdentifierChar(CurrentChar) do
        begin
          LexemeBuilder := LexemeBuilder + CurrentChar;
          Advance;
        end;
        if GetTokenType(LexemeBuilder) = tkKeywordOn then // Changed from tkOn
        begin
          Result.TokenType := tkOptionExplicitOn;
          Result.Lexeme := 'Option Explicit On';
          Exit;
        end
        else if GetTokenType(LexemeBuilder) = tkKeywordOff then // Changed from tkOff
        begin
          Result.TokenType := tkOptionExplicitOff;
          Result.Lexeme := 'Option Explicit Off';
          Exit;
        end
        else
        begin
          // Not "On" or "Off", rollback
          FCurrentCharIndex := SavedCharIndex;
          FCurrentLineIndex := SavedLineIndex;
          FCurrentLine := SavedLineContent;
          // Re-process "Option" as a regular keyword/identifier
          Result.TokenType := tkKeyword; // It's just 'Option' keyword
          Result.Lexeme := 'Option';
          // No Advance here, as the token is already formed from the rollback point
          Exit;
        end;
      end
      else
      begin
        // Not "Explicit", rollback
        FCurrentCharIndex := SavedCharIndex;
        FCurrentLineIndex := SavedLineIndex;
        FCurrentLine := SavedLineContent;
        // Re-process "Option" as a regular keyword/identifier
        Result.TokenType := tkKeyword; // It's just 'Option' keyword
        Result.Lexeme := 'Option';
        // No Advance here, as the token is already formed from the rollback point
        Exit;
      end;
    end;
    // --- End Special handling ---

    Result.TokenType := CurrentTokType;
    Result.Lexeme := LexemeBuilder;
    Exit;
  end;

  // Handle Operators and single-character tokens
  // Initialize LexemeBuilder with the current character, assuming it's a single-char token
  LexemeBuilder := String(CurrentChar);
  CurrentTokType := tkUnknown; // Default to unknown, will be updated in case

  case CurrentChar of
    '+', '-', '*', '/': CurrentTokType := tkOperator;
    '=': CurrentTokType := tkOperator; // Assignment and equality
    '<':
      begin
        Advance; // Consume '<'
        if CurrentChar = '=' then
        begin
          CurrentTokType := tkOperator; LexemeBuilder := '<='; Advance;
        end
        else if CurrentChar = '>' then
        begin
          CurrentTokType := tkOperator; LexemeBuilder := '<>'; Advance;
        end
        else // It was just '<'
        begin
          CurrentTokType := tkOperator;
          LexemeBuilder := '<';
        end;
      end;
    '>':
      begin
        Advance; // Consume '>'
        if CurrentChar = '=' then
        begin
          CurrentTokType := tkOperator; LexemeBuilder := '>='; Advance;
        end
        else // It was just '>'
        begin
          CurrentTokType := tkOperator;
          LexemeBuilder := '>';
        end;
      end;
    '&': CurrentTokType := tkOperator; // String concatenation
    '(': CurrentTokType := tkParenthesisOpen;
    ')': CurrentTokType := tkParenthesisClose;
    ',': CurrentTokType := tkComma;
    '.': CurrentTokType := tkDot;
    ':': CurrentTokType := tkColon;
    else
      raise Exception.CreateFmt('Lexer Error: Unexpected character "%s" at %d:%d',
        [CurrentChar, Result.Line + 1, Result.Column + 1]);
  end;

  // If a multi-character operator was handled (e.g., <=, >=, <>), Advance would have already been called.
  // For single-character operators, we need to advance here.
  if Length(LexemeBuilder) = 1 then
    Advance;

  Result.TokenType := CurrentTokType;
  Result.Lexeme := LexemeBuilder;
end;

function TLexer.PeekNextToken: TToken;
var
  SavedPosition: Integer;
begin
  // Save the current position
  SavedPosition := FPosition;

  // Get the next token without updating FPosition
  Result := GetNextToken;

  // Restore the original position
  FPosition := SavedPosition;
end;


end.
