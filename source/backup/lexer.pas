unit Lexer;

interface

uses
  SysUtils, Classes, // For TStringList
  BytecodeTypes, tokenDefs; // For TToken, TTokenType, GetTokenType

// --- Define TToken and related types FIRST ---

// This enum will define the possible types of tokens
type

// --- Now define TLexer ---
TLexer = class
private
  FSourceCode: TStringList; // The lines of code to tokenize
  FCurrentLineIdx: Integer; // Current line number in FSourceCode
  FCurrentCharIdx: Integer; // Current character position in the current line
  FCurrentChar: Char;       // The character at FCurrentCharIdx
  FLineLength: Integer;     // Length of the current line

  // Advances to the next character, handles EOL
  procedure AdvanceChar;
  // Peeks at the next character without advancing
  function PeekChar(Offset: Integer = 1): Char;

  // Helper for numeric literals
  function ScanNumber: TToken;
  // Helper for string literals
  function ScanString: TToken;
  // Helper for identifiers/keywords
  function ScanIdentifierOrKeyword: TToken;
  // Helper for operators
  function ScanOperator: TToken;

public
  constructor Create(ASourceCode: TStringList);
  destructor Destroy; override;

  function GetNextToken: TToken; // Main method to get the next token
  procedure Reset; // Reset lexer to start of code
end;

implementation

{ TLexer }

constructor TLexer.Create(ASourceCode: TStringList);
begin
  inherited Create;
  FSourceCode := ASourceCode; // Assign (not own) the TStringList
  Reset;
end;

destructor TLexer.Destroy;
begin
  // FSourceCode is not owned here, so don't free it
  inherited Destroy;
end;

procedure TLexer.Reset;
begin
  FCurrentLineIdx := 0;
  FCurrentCharIdx := 0;
  FCurrentChar := #0; // Null character
  FLineLength := 0;
  if (FSourceCode <> nil) and (FSourceCode.Count > 0) then
  begin
    FLineLength := Length(FSourceCode[FCurrentLineIdx]);
    if FLineLength > 0 then
      FCurrentChar := FSourceCode[FCurrentLineIdx][1];
  end;
end;

procedure TLexer.AdvanceChar;
begin
  Inc(FCurrentCharIdx);
  if FCurrentCharIdx > FLineLength then
  begin
    // End of current line, move to next
    Inc(FCurrentLineIdx);
    FCurrentCharIdx := 0; // Reset char index for new line
    if FCurrentLineIdx < FSourceCode.Count then
    begin
      FLineLength := Length(FSourceCode[FCurrentLineIdx]);
      if FLineLength > 0 then
        FCurrentChar := FSourceCode[FCurrentLineIdx][1] // Start of new line
      else
        FCurrentChar := #0; // Empty line
    end
    else
    begin
      FCurrentChar := #0; // End of file
      FLineLength := 0;
    end;
  end
  else if FCurrentCharIdx <= FLineLength then
    FCurrentChar := FSourceCode[FCurrentLineIdx][FCurrentCharIdx];
end;

function TLexer.PeekChar(Offset: Integer = 1): Char;
var
  TargetCharIdx: Integer;
  TargetLineIdx: Integer;
begin
  TargetCharIdx := FCurrentCharIdx + Offset;
  TargetLineIdx := FCurrentLineIdx;

  if TargetCharIdx > FLineLength then
  begin
    // Peek into next lines
    TargetLineIdx := FCurrentLineIdx + (TargetCharIdx div (FLineLength + 1)); // Approximation
    TargetCharIdx := TargetCharIdx mod (FLineLength + 1);
    if TargetCharIdx = 0 then TargetCharIdx := 1; // Adjust if it wrapped perfectly

    if TargetLineIdx < FSourceCode.Count then
    begin
      if TargetCharIdx <= Length(FSourceCode[TargetLineIdx]) then
        Result := FSourceCode[TargetLineIdx][TargetCharIdx]
      else
        Result := #0; // Beyond end of that line
    end
    else
      Result := #0; // End of file
  end
  else
    Result := FSourceCode[FCurrentLineIdx][TargetCharIdx];
end;


function TLexer.ScanNumber: TToken;
var
  StartPos: Integer;
  NumStr: String;
begin
  Result.TokenType := tkIntegerLiteral; // Assuming tkIntegerLiteral is in your unified TTokenType
  Result.LineNum := FCurrentLineIdx;
  Result.ColNum := FCurrentCharIdx;
  StartPos := FCurrentCharIdx;
  NumStr := '';

  while (FCurrentChar >= '0') and (FCurrentChar <= '9') do
  begin
    NumStr := NumStr + FCurrentChar;
    AdvanceChar;
  end;
  Result.Lexeme := NumStr; // <-- Changed from Result.Value to Result.Lexeme
end;


function TLexer.ScanString: TToken;
var
  StartPos: Integer;
  StrContent: String;
begin
  Result.TokenType := tkStringLiteral; // Assuming tkStringLiteral is correctly defined
  Result.LineNum := FCurrentLineIdx;
  Result.ColNum := FCurrentCharIdx;
  StartPos := FCurrentCharIdx;
  StrContent := '';

  // Consume the opening quote
  if FCurrentChar = '"' then
    AdvanceChar
  else
    raise Exception.CreateFmt('Lexer Error: Expected " at %d:%d', [FCurrentLineIdx + 1, FCurrentCharIdx]);

  while (FCurrentChar <> '"') and (FCurrentChar <> #0) do // #0 for EOF
  begin
    // Handle escaped quotes if VB6 supports them (e.g., "" for a single ")
    // This simple lexer doesn't, just takes char until next single quote.
    StrContent := StrContent + FCurrentChar;
    AdvanceChar;
  end;

  // Consume the closing quote
  if FCurrentChar = '"' then
    AdvanceChar
  else
    raise Exception.CreateFmt('Lexer Error: Unterminated string literal at %d:%d', [FCurrentLineIdx + 1, StartPos]);

  Result.Lexeme := '"' + StrContent + '"'; // <-- Changed from Result.Value to Result.Lexeme
end;

function TLexer.ScanIdentifierOrKeyword: TToken;
var
  StartPos: Integer;
  IdStr: String;
begin
  Result.LineNum := FCurrentLineIdx;
  Result.ColNum := FCurrentCharIdx;
  StartPos := FCurrentCharIdx;
  IdStr := '';

  while (FCurrentChar >= 'A') and (FCurrentChar <= 'Z') or
        (FCurrentChar >= 'a') and (FCurrentChar <= 'z') or
        (FCurrentChar >= '0') and (FCurrentChar <= '9') or
        (FCurrentChar = '_') do // Identifiers can contain letters, digits, and underscore
  begin
    IdStr := IdStr + FCurrentChar;
    AdvanceChar;
  end;

  Result.Lexeme := IdStr; // <-- Changed from Result.Value to Result.Lexeme
  Result.TokenType := GetTokenType(IdStr); // Check if it's a keyword
end;


function TLexer.ScanOperator: TToken;
var
  OpStr: String;
  NextChar: Char;
begin
  Result.LineNum := FCurrentLineIdx;
  Result.ColNum := FCurrentCharIdx;
  OpStr := FCurrentChar;
  AdvanceChar;

  // Handle multi-character operators (e.g., <=, >=, <>)
  NextChar := FCurrentChar;
  if (OpStr = '<') and (NextChar = '>') then
  begin
    OpStr := OpStr + NextChar;
    AdvanceChar;
  end
  else if (OpStr = '<') and (NextChar = '=') then
  begin
    OpStr := OpStr + NextChar;
    AdvanceChar;
  end
  else if (OpStr = '>') and (NextChar = '=') then
  begin
    OpStr := OpStr + NextChar;
    AdvanceChar;
  end;

  Result.Lexeme := OpStr; // <-- Changed from Result.Value to Result.Lexeme
  Result.TokenType := GetTokenType(OpStr); // Ensure it resolves to tkOperator
end;


function TLexer.GetNextToken: TToken;
var
  CurrentTokenCol: Integer;
begin
  while True do
  begin
    // Skip whitespace
    while (FCurrentChar = ' ') or (FCurrentChar = #9) do // Space or Tab
      AdvanceChar;

    CurrentTokenCol := FCurrentCharIdx; // Store start column for token

    if FCurrentChar = #0 then // End of file
    begin
      Result.TokenType := tkEOF; // Use tokEOF or tkEndOfFile as defined in TokenDefs.pas
      Result.Lexeme := ''; // <-- Changed from .Value to .Lexeme
      Result.LineNum := FCurrentLineIdx;
      Result.ColNum := CurrentTokenCol;
      Exit;
    end
    else if FCurrentChar = #13 then // Carriage Return (Windows EOL)
    begin
      AdvanceChar;
      if FCurrentChar = #10 then // Line Feed (Windows EOL)
        AdvanceChar;
      Result.TokenType := tkNewline; // Use tkNewline or tkEndOfLine as defined
      Result.Lexeme := ''; // <-- Changed from .Value to .Lexeme
      Result.LineNum := FCurrentLineIdx;
      Result.ColNum := CurrentTokenCol;
      Exit;
    end
    else if FCurrentChar = #10 then // Line Feed (Unix EOL)
    begin
      AdvanceChar;
      Result.TokenType := tkNewline; // Use tkNewline or tkEndOfLine
      Result.Lexeme := ''; // <-- Changed from .Value to .Lexeme
      Result.LineNum := FCurrentLineIdx;
      Result.ColNum := CurrentTokenCol;
      Exit;
    end
    else if (FCurrentChar = '''') then // Single quote comment
    begin
      // Consume the rest of the line as a comment
      Result.TokenType := tkComment; // Assuming tkComment is defined
      Result.Lexeme := Copy(FSourceCode[FCurrentLineIdx], FCurrentCharIdx, FLineLength - FCurrentCharIdx + 1); // <-- Changed from .Value to .Lexeme
      FCurrentCharIdx := FLineLength + 1; // Move past end of line
      AdvanceChar; // To trigger EOL handling
      // A comment token is returned, but for an interpreter, you'd usually discard them here
      // and then call GetNextToken again to get the real next statement or EOL.
      // For now, we'll return it and the parser can skip it.
      Exit;
    end
    else if (FCurrentChar >= '0') and (FCurrentChar <= '9') then
      Exit(ScanNumber)
    else if FCurrentChar = '"' then
      Exit(ScanString)
    else if (FCurrentChar >= 'A') and (FCurrentChar <= 'Z') or
            (FCurrentChar >= 'a') and (FCurrentChar <= 'z') or
            (FCurrentChar = '_') then
      Exit(ScanIdentifierOrKeyword)
    else if (FCurrentChar = '+') or (FCurrentChar = '-') or (FCurrentChar = '*') or (FCurrentChar = '/') or
            (FCurrentChar = '=') or (FCurrentChar = '<') or (FCurrentChar = '>') or (FCurrentChar = '&') or
            (FCurrentChar = '(') or (FCurrentChar = ')') or (FCurrentChar = ',') or (FCurrentChar = '.') or
            (FCurrentChar = ':') then
      Exit(ScanOperator) // Single char operators and multi-char prefixes
    else
      raise Exception.CreateFmt('Lexer Error: Unrecognized character "%s" at %d:%d', [FCurrentChar, FCurrentLineIdx + 1, FCurrentCharIdx]);
  end; // while True
end;

end.
