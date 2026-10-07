unit Lexer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TokenDefs; // TokenDefs for TToken, TTokenType

type
  TLexer = class
  private
    FSourceCode: TStringList;
    FCurrentLineIndex: Integer;
    FCurrentCharIndex: Integer;
    FCurrentLine: String;
    FEOF: Boolean;
    FQBMode: Boolean;
    FRawRest: Boolean; // QuickBASIC DATA: the rest of the line is one token

    procedure Advance;
    procedure AdvanceLine;
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
    // QuickBASIC (kayte --qbs): "?" is PRINT, names can end in % ! # & as
    // well as $, &H / &O number literals, DATA takes the rest of its line
    // as written, and Kayte's own keywords (CLASS, TRY, SHOW ...) are
    // ordinary names.
    property QBMode: Boolean read FQBMode write FQBMode;


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
  FRawRest := False;
  if (FSourceCode.Count > 0) then
    FCurrentLine := FSourceCode[FCurrentLineIndex]
  else
    FEOF := True; // Empty source code
end;

procedure TLexer.Advance;
begin
  // Intentionally never crosses a line boundary: a token being scanned
  // character-by-character (identifier, number, operator, ...) must stop
  // dead at the end of the current line, not silently continue reading
  // into the next line's text (which previously merged e.g. "MyIntegerVar"
  // at the end of one line with "DIM" at the start of the next into
  // "MyIntegerVarDIM"). Line transitions are handled explicitly via
  // AdvanceLine, from the End-of-Line handling in GetNextToken.
  if FEOF then Exit;

  if FCurrentCharIndex < Length(FCurrentLine) then
    Inc(FCurrentCharIndex);
end;

procedure TLexer.AdvanceLine;
begin
  if FEOF then Exit;

  Inc(FCurrentLineIndex);
  FCurrentCharIndex := 0;
  if FCurrentLineIndex < FSourceCode.Count then
    FCurrentLine := FSourceCode[FCurrentLineIndex]
  else
  begin
    FCurrentLine := '';
    FEOF := True;
  end;
end;

function TLexer.CurrentChar: Char;
begin
  if FEOF or (FCurrentCharIndex >= Length(FCurrentLine)) then
    Result := #0
  else
    Result := FCurrentLine[FCurrentCharIndex + 1];
end;

function TLexer.PeekChar: Char;
var
  NextCharIndex: Integer;
  NextLineIndex: Integer;
begin
  if FEOF then
  begin
    Result := #0;
    Exit;  // ← IMPORTANT: Was missing!
  end;

  NextCharIndex := FCurrentCharIndex + 1;
  NextLineIndex := FCurrentLineIndex;

  if NextCharIndex >= Length(FCurrentLine) then
  begin
    NextLineIndex := FCurrentLineIndex + 1;
    NextCharIndex := 0;
    if NextLineIndex < FSourceCode.Count then
    begin
      if Length(FSourceCode[NextLineIndex]) > 0 then
        Result := FSourceCode[NextLineIndex][NextCharIndex + 1]
      else
        Result := #0;
    end
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
  // "$" for classic BASIC names like LEFT$ and name$.
  Result := IsLetter(C) or IsDigit(C) or (C = '_') or (C = '$');
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
    'PRINT', 'INPUT', 'MSGBOX', 'FORM', 'END FORM', 'SHOW', 'HIDE', 'STRUCT',
    'PROCESS', 'QT', 'QML', 'EXIT', 'DO', 'LOOP', 'UNTIL', 'CONTINUE',
    'CLASS', 'NEW', 'PROPERTY', 'WITH', 'TRY', 'CATCH', 'FINALLY', 'THROW':
      Result := tkKeyword;
    // Boolean Literals
    'TRUE', 'FALSE':
      Result := tkBooleanLiteral;
    // Operators (basic ones; full list would be larger)
    '+', '-', '*', '/', '=', '<', '>', '<=', '>=', '<>', '&': // '&' for string concat
      Result := tkOperator;
    'AND', 'OR', 'XOR', 'NOT', 'IS', 'MOD': // Logical/comparison/MOD operators as words
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

  // The rest of a QuickBASIC DATA line, as written (see DataStatement).
  if FRawRest then
  begin
    FRawRest := False;
    if FCurrentCharIndex < Length(FCurrentLine) then
    begin
      Result.Column := FCurrentCharIndex;
      Result.TokenType := tkComment;
      Result.Lexeme := Copy(FCurrentLine, FCurrentCharIndex + 1, MaxInt);
      FCurrentCharIndex := Length(FCurrentLine);
      Exit;
    end;
  end;

  StartCol := FCurrentCharIndex;
  Result.Column := StartCol; // Update column number after skipping whitespace

  // Handle End of Line
  if (FCurrentCharIndex >= Length(FCurrentLine)) and (FCurrentLineIndex < FSourceCode.Count) then
  begin
    AdvanceLine; // Move to the next line
    Result.TokenType := tkEndOfLine;
    Result.Lexeme := '';
    Exit;
  end;

  // Handle comments starting with ', REM, //, or ///
  // Check for /// first (doc comment)
  if (CurrentChar = '/') and (PeekChar = '/') then
  begin
    Advance; // Skip first /
    if (CurrentChar = '/') and (PeekChar = '/') then
    begin
      // Triple slash - documentation comment (///)
      Advance; // Skip second /
      Advance; // Skip third /
      LexemeBuilder := '';
      while (CurrentChar <> #0) and (FCurrentCharIndex < Length(FCurrentLine)) do
      begin
        LexemeBuilder := LexemeBuilder + CurrentChar;
        Advance;
      end;
      Result.TokenType := tkDocComment;
      Result.Lexeme := Trim(LexemeBuilder);
      Exit;
    end
    else
    begin
      // Double slash - regular comment (//)
      Advance; // Skip second /
      LexemeBuilder := '';
      while (CurrentChar <> #0) and (FCurrentCharIndex < Length(FCurrentLine)) do
      begin
        LexemeBuilder := LexemeBuilder + CurrentChar;
        Advance;
      end;
      Result.TokenType := tkComment;
      Result.Lexeme := Trim(LexemeBuilder);
      Exit;
    end;
  end;

  // Check for REM keyword comment
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
    Advance; // Skip the opening quote
    while (CurrentChar <> #0) and (FCurrentCharIndex < Length(FCurrentLine)) do
    begin
      LexemeBuilder := LexemeBuilder + CurrentChar;
      Advance;
    end;
    Result.TokenType := tkComment;
    Result.Lexeme := Trim(LexemeBuilder);
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

  // Number literals: 42, 3.14, .5, 1E-3, 2.5E+10
  if IsDigit(CurrentChar) or ((CurrentChar = '.') and IsDigit(PeekChar)) then
  begin
    LexemeBuilder := '';
    CurrentTokType := tkIntegerLiteral;
    while IsDigit(CurrentChar) do
    begin
      LexemeBuilder := LexemeBuilder + CurrentChar;
      Advance;
    end;
    if (CurrentChar = '.') and IsDigit(PeekChar) then
    begin
      CurrentTokType := tkFloatLiteral;
      LexemeBuilder := LexemeBuilder + '.';
      Advance;
      while IsDigit(CurrentChar) do
      begin
        LexemeBuilder := LexemeBuilder + CurrentChar;
        Advance;
      end;
    end;
    // An exponent: E, an optional sign, digits.
    if (UpCase(CurrentChar) = 'E') and
       (IsDigit(PeekChar) or ((PeekChar in ['+', '-']) and (FCurrentCharIndex + 3 <= Length(FCurrentLine)) and
         IsDigit(FCurrentLine[FCurrentCharIndex + 3]))) then
    begin
      CurrentTokType := tkFloatLiteral;
      LexemeBuilder := LexemeBuilder + 'E';
      Advance;
      if CurrentChar in ['+', '-'] then
      begin
        LexemeBuilder := LexemeBuilder + CurrentChar;
        Advance;
      end;
      while IsDigit(CurrentChar) do
      begin
        LexemeBuilder := LexemeBuilder + CurrentChar;
        Advance;
      end;
    end;
    if FQBMode and (CurrentChar in ['%', '&', '!', '#']) then
      Advance; // a type suffix: 10& is 10, 2.5# is 2.5
    if LexemeBuilder[1] = '.' then
      LexemeBuilder := '0' + LexemeBuilder;
    Result.TokenType := CurrentTokType;
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
    if FQBMode and (CurrentChar in ['%', '&', '!', '#']) and
       (LexemeBuilder[Length(LexemeBuilder)] <> '$') then
    begin
      LexemeBuilder := LexemeBuilder + CurrentChar; // a type suffix: count%, total&
      Advance;
    end;

    CurrentTokType := GetTokenType(LexemeBuilder);
    if FQBMode then
    begin
      case AnsiUpperCase(LexemeBuilder) of
        'CLASS', 'NEW', 'PROPERTY', 'WITH', 'TRY', 'CATCH', 'FINALLY', 'THROW', 'PROCESS', 'QT', 'QML',
        'FORM', 'SHOW', 'HIDE', 'STRUCT', 'CONTINUE', 'MSGBOX':
          CurrentTokType := tkIdentifier;
        'DATA':
          FRawRest := True;
        // QuickBASIC has no TRUE / FALSE (programs define CONST TRUE = -1)
        'TRUE', 'FALSE':
          CurrentTokType := tkIdentifier;
        'DEF': // DEF FN ... END DEF / EXIT DEF
          CurrentTokType := tkKeyword;
        'EQV', 'IMP':
          CurrentTokType := tkOperator;
      end;
    end;

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

  // Operators and single-character tokens; assume single-char until the
  // case below says otherwise.
  LexemeBuilder := String(CurrentChar);
  CurrentTokType := tkUnknown;

  case CurrentChar of
    '+', '-', '*', '\': CurrentTokType := tkOperator; // '\' is integer division
    '/':
      begin
        // '//' comments should already be consumed upstream of this point -
        // seeing one here means that logic missed a case.
        if PeekChar = '/' then
        begin
          raise Exception.CreateFmt('Lexer Error: Unexpected comment at %d:%d (should have been handled earlier)',
            [Result.Line + 1, Result.Column + 1]);
        end
        else
          CurrentTokType := tkOperator; // It's division
      end;
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
    '&':
      if FQBMode and (UpCase(PeekChar) in ['H', 'O']) then
      begin
        // &HFF / &O17: a number in hex / octal
        Advance; // &
        CurrentTokType := tkIntegerLiteral;
        if UpCase(CurrentChar) = 'H' then
        begin
          Advance;
          LexemeBuilder := '$';
          while CurrentChar in ['0'..'9', 'a'..'f', 'A'..'F'] do
          begin
            LexemeBuilder := LexemeBuilder + CurrentChar;
            Advance;
          end;
        end
        else
        begin
          Advance;
          LexemeBuilder := '&';
          while CurrentChar in ['0'..'7'] do
          begin
            LexemeBuilder := LexemeBuilder + CurrentChar;
            Advance;
          end;
        end;
        if Length(LexemeBuilder) = 1 then
          raise Exception.CreateFmt('Lexer Error: expected digits after &H / &O at %d:%d',
            [Result.Line + 1, Result.Column + 1]);
        if CurrentChar in ['%', '&'] then
          Advance;
        Result.TokenType := tkIntegerLiteral;
        Result.Lexeme := IntToStr(StrToInt64(LexemeBuilder));
        Exit;
      end
      else
        CurrentTokType := tkOperator; // String concatenation
    '^': CurrentTokType := tkOperator; // power
    '#':
      if FQBMode then
        CurrentTokType := tkOperator // a file number: PRINT #1, ...
      else
        raise Exception.CreateFmt('Lexer Error: Unexpected character "#" at %d:%d',
          [Result.Line + 1, Result.Column + 1]);
    ';': CurrentTokType := tkSemicolon;
    '?':
      if FQBMode then
      begin
        Advance;
        Result.TokenType := tkKeyword; // ? is PRINT
        Result.Lexeme := 'PRINT';
        Exit;
      end
      else
        raise Exception.CreateFmt('Lexer Error: Unexpected character "?" at %d:%d',
          [Result.Line + 1, Result.Column + 1]);
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
  SavedLineIndex: Integer;
  SavedCharIndex: Integer;
  SavedLine: String;
  SavedEOF: Boolean;
  SavedRaw: Boolean;
begin
  // Save current state
  SavedLineIndex := FCurrentLineIndex;
  SavedCharIndex := FCurrentCharIndex;
  SavedLine := FCurrentLine;
  SavedEOF := FEOF;
  SavedRaw := FRawRest;

  // Get next token
  Result := GetNextToken;
  FRawRest := SavedRaw;

  // Restore state
  FCurrentLineIndex := SavedLineIndex;
  FCurrentCharIndex := SavedCharIndex;
  FCurrentLine := SavedLine;
  FEOF := SavedEOF;
end;

end.
