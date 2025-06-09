unit SynVBHighlighter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, SynHighlighter, SynPasSyn;

type
  { TSynVBHighlighter }

  // Extend TSynTextStyle (or create a custom record) to include ANSI codes
  // For a command-line app, you'd typically pre-define ANSI color sequences
  // and assign them based on the style.
  // We'll simulate this by adding a property to access the ANSI start/end codes.
  // NOTE: This TSynTextStyle in SynEdit is internal to how SynEdit renders.
  // For CLI, we will use our own mapping from TSynIdentifier to ANSI codes.

  // Let's define custom ANSI styles for our highlighter
  TVBHighlightStyle = record
    ANSINormal: String;
    ANSIBold: String;
    ANSIReset: String;
  end;

  TSynVBHighlighter = class(TSynCustomHighlighter)
  private
    fKeywords: TStringList;
    fTypes: TStringList;
    fDirectives: TStringList;
    fFunctions: TStringList;
    fOperators: TStringList;
    fInCommentBlock: Boolean;

    // Define ANSI escape codes for colors
    FANSICodes: array[TSynIdentifier] of TVBHighlightStyle;

    procedure InitKeywords;
    procedure InitANSICodes; // New procedure to set up ANSI codes
  protected
    procedure DoIdentifyLine(Line: Integer; var TokenOffset: Integer; var State: TSynHighlighterState); override;
    function GetDefaultKeywords(Index: Integer): String; override;
    function GetToken: TSynIdentifier; override;
    function GetTokenPos(Token: TSynIdentifier): Integer; override;
    function GetTokenString(Token: TSynIdentifier): String; override;
    procedure Reset; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // New method to get the ANSI style for a given token
    function GetANSIStyle(Token: TSynIdentifier): TVBHighlightStyle;
  end;

implementation

// ANSI escape codes for basic colors
// https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
const
  ANSI_RESET = #27'[0m';
  ANSI_BLACK = #27'[30m';
  ANSI_RED = #27'[31m';
  ANSI_GREEN = #27'[32m';
  ANSI_YELLOW = #27'[33m';
  ANSI_BLUE = #27'[34m';
  ANSI_MAGENTA = #27'[35m';
  ANSI_CYAN = #27'[36m';
  ANSI_WHITE = #27'[37m';
  ANSI_BRIGHT_BLACK = #27'[90m';
  ANSI_BRIGHT_RED = #27'[91m';
  ANSI_BRIGHT_GREEN = #27'[92m';
  ANSI_BRIGHT_YELLOW = #27'[93m';
  ANSI_BRIGHT_BLUE = #27'[94m';
  ANSI_BRIGHT_MAGENTA = #27'[95m';
  ANSI_BRIGHT_CYAN = #27'[96m';
  ANSI_BRIGHT_WHITE = #27'[97m';

  // Background colors (optional)
  ANSI_BG_BLACK = #27'[40m';
  ANSI_BG_RED = #27'[41m';
  // ... and so on

  // Font styles
  ANSI_BOLD = #27'[1m';
  ANSI_ITALIC = #27'[3m';
  ANSI_UNDERLINE = #27'[4m';

{ TSynVBHighlighter }

constructor TSynVBHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // SetDefaultStyles is not relevant for CLI directly, but InitANSICodes is.
  InitANSICodes;
  InitKeywords;
  // SetTokenTypes maps to SynEdit's internal renderer, not directly used for CLI output.
  // Still useful to ensure GetToken returns a valid TSynIdentifier
  SetTokenTypes(SYN_IDENTIFIER, SYN_LAST_TOKEN);
end;

destructor TSynVBHighlighter.Destroy;
begin
  fKeywords.Free;
  fTypes.Free;
  fDirectives.Free;
  fFunctions.Free;
  fOperators.Free;
  inherited Destroy;
end;

// Removed SetDefaultStyles as it pertains to graphical SynEdit rendering.
// All styling for CLI will be handled by InitANSICodes and GetANSIStyle.

procedure TSynVBHighlighter.InitANSICodes;
begin
  // Initialize ANSI codes for each token type
  FANSICodes[SYN_KEYWORD].ANSINormal := ANSI_BLUE;
  FANSICodes[SYN_KEYWORD].ANSIBold := ANSI_BLUE + ANSI_BOLD;
  FANSICodes[SYN_KEYWORD].ANSIReset := ANSI_RESET;

  // Custom styles for types (using Bright Cyan)
  FANSICodes[SYN_TYPE].ANSINormal := ANSI_BRIGHT_CYAN;
  FANSICodes[SYN_TYPE].ANSIBold := ANSI_BRIGHT_CYAN + ANSI_BOLD;
  FANSICodes[SYN_TYPE].ANSIReset := ANSI_RESET;

  // Custom styles for directives (using Magenta)
  FANSICodes[SYN_DIRECTIVE].ANSINormal := ANSI_MAGENTA;
  FANSICodes[SYN_DIRECTIVE].ANSIBold := ANSI_MAGENTA + ANSI_BOLD;
  FANSICodes[SYN_DIRECTIVE].ANSIReset := ANSI_RESET;

  // Custom styles for functions (using Teal/Cyan, italic might not render in all terminals)
  FANSICodes[SYN_FUNCTION].ANSINormal := ANSI_CYAN;
  FANSICodes[SYN_FUNCTION].ANSIBold := ANSI_CYAN + ANSI_BOLD;
  FANSICodes[SYN_FUNCTION].ANSIReset := ANSI_RESET;

  FANSICodes[SYN_OPERATOR].ANSINormal := ANSI_RED;
  FANSICodes[SYN_OPERATOR].ANSIBold := ANSI_RED + ANSI_BOLD;
  FANSICodes[SYN_OPERATOR].ANSIReset := ANSI_RESET;

  FANSICodes[SYN_COMMENT].ANSINormal := ANSI_GREEN;
  FANSICodes[SYN_COMMENT].ANSIBold := ANSI_GREEN; // Comments usually not bold
  FANSICodes[SYN_COMMENT].ANSIReset := ANSI_RESET;

  FANSICodes[SYN_STRING].ANSINormal := ANSI_YELLOW; // Often yellow or maroon
  FANSICodes[SYN_STRING].ANSIBold := ANSI_YELLOW;
  FANSICodes[SYN_STRING].ANSIReset := ANSI_RESET;

  FANSICodes[SYN_NUMBER].ANSINormal := ANSI_MAGENTA; // Or default black
  FANSICodes[SYN_NUMBER].ANSIBold := ANSI_MAGENTA;
  FANSICodes[SYN_NUMBER].ANSIReset := ANSI_RESET;

  FANSICodes[SYN_IDENTIFIER].ANSINormal := ANSI_WHITE; // Default text color
  FANSICodes[SYN_IDENTIFIER].ANSIBold := ANSI_WHITE + ANSI_BOLD;
  FANSICodes[SYN_IDENTIFIER].ANSIReset := ANSI_RESET;

  FANSICodes[SYN_WHITESPACE].ANSINormal := ''; // No color for whitespace
  FANSICodes[SYN_WHITESPACE].ANSIBold := '';
  FANSICodes[SYN_WHITESPACE].ANSIReset := '';

  FANSICodes[SYN_SYMBOL].ANSINormal := ANSI_WHITE; // Symbols often default
  FANSICodes[SYN_SYMBOL].ANSIBold := ANSI_WHITE;
  FANSICodes[SYN_SYMBOL].ANSIReset := ANSI_RESET;

  // For Preprocessor directives (using Bright Black/Gray)
  FANSICodes[SYN_PREPROCESSOR].ANSINormal := ANSI_BRIGHT_BLACK;
  FANSICodes[SYN_PREPROCESSOR].ANSIBold := ANSI_BRIGHT_BLACK + ANSI_BOLD;
  FANSICodes[SYN_PREPROCESSOR].ANSIReset := ANSI_RESET;

  // Ensure all undefined tokens default to normal text
  for var i := Low(TSynIdentifier) to High(TSynIdentifier) do
  begin
    if FANSICodes[i].ANSINormal = '' then // If not set above
    begin
      FANSICodes[i].ANSINormal := ANSI_WHITE;
      FANSICodes[i].ANSIBold := ANSI_WHITE;
      FANSICodes[i].ANSIReset := ANSI_RESET;
    end;
  end;
end;

procedure TSynVBHighlighter.InitKeywords;
begin
  fKeywords := TStringList.Create;
  fKeywords.CaseSensitive := False;
  fKeywords.Sorted := True;
  fKeywords.Duplicates := TDuplicates.dupIgnore;

  // VB Keywords (expanded for better highlighting)
  fKeywords.Add('If'); fKeywords.Add('Then'); fKeywords.Add('Else'); fKeywords.Add('ElseIf'); fKeywords.Add('End If');
  fKeywords.Add('For'); fKeywords.Add('Next'); fKeywords.Add('While'); fKeywords.Add('Wend');
  fKeywords.Add('Do'); fKeywords.Add('Loop'); fKeywords.Add('Until'); fKeywords.Add('While'); fKeywords.Add('Exit Do'); fKeywords.Add('Exit For'); fKeywords.Add('Exit Function'); fKeywords.Add('Exit Property'); fKeywords.Add('Exit Sub');
  fKeywords.Add('Select'); fKeywords.Add('Case'); fKeywords.Add('End Select');
  fKeywords.Add('Sub'); fKeywords.Add('End Sub'); fKeywords.Add('Function'); fKeywords.Add('End Function');
  fKeywords.Add('Property'); fKeywords.Add('End Property'); fKeywords.Add('Class'); fKeywords.Add('End Class');
  fKeywords.Add('Module'); fKeywords.Add('End Module'); fKeywords.Add('Structure'); fKeywords.Add('End Structure');
  fKeywords.Add('Enum'); fKeywords.Add('End Enum');
  fKeywords.Add('Dim'); fKeywords.Add('As'); fKeywords.Add('New'); fKeywords.Add('Set'); fKeywords.Add('Nothing');
  fKeywords.Add('Public'); fKeywords.Add('Private'); fKeywords.Add('Protected'); fKeywords.Add('Friend');
  fKeywords.Add('Static'); fKeywords.Add('Const'); fKeywords.Add('WithEvents');
  fKeywords.Add('True'); fKeywords.Add('False'); fKeywords.Add('Not'); fKeywords.Add('And'); fKeywords.Add('Or'); fKeywords.Add('Xor');
  fKeywords.Add('ByVal'); fKeywords.Add('ByRef'); fKeywords.Add('Optional'); fKeywords.Add('ParamArray');
  fKeywords.Add('Call'); fKeywords.Add('GoTo'); fKeywords.Add('Resume'); fKeywords.Add('Stop'); fKeywords.Add('End');
  fKeywords.Add('On Error'); fKeywords.Add('GoSub'); fKeywords.Add('Return');
  fKeywords.Add('With'); fKeywords.Add('End With'); fKeywords.Add('SyncLock'); fKeywords.Add('End SyncLock');
  fKeywords.Add('Is'); fKeywords.Add('Like'); fKeywords.Add('Mod');
  fKeywords.Add('Me'); fKeywords.Add('MyBase'); fKeywords.Add('MyClass'); fKeywords.Add('Shared'); fKeywords.Add('Overrides'); fKeywords.Add('Overloads');
  fKeywords.Add('Handles'); fKeywords.Add('AddHandler'); fKeywords.Add('RemoveHandler'); fKeywords.Add('RaiseEvent');
  fKeywords.Add('Implements'); fKeywords.Add('Inherits');
  fKeywords.Add('Try'); fKeywords.Add('Catch'); fKeywords.Add('Finally'); fKeywords.Add('End Try');
  fKeywords.Add('Throw');
  fKeywords.Add('Yield'); fKeywords.Add('Await'); fKeywords.Add('Async'); // VB.NET additions
  fKeywords.Add('Using'); fKeywords.Add('End Using');
  fKeywords.Add('Get'); fKeywords.Add('Set'); // For properties
  fKeywords.Add('AddressOf');
  fKeywords.Add('TypeOf'); // For TypeOf ... Is ...

  // Interpreter-specific keywords
  fKeywords.Add('Print'); fKeywords.Add('Input'); fKeywords.Add('Rem');

  fTypes := TStringList.Create;
  fTypes.CaseSensitive := False;
  fTypes.Sorted := True;
  fTypes.Duplicates := TDuplicates.dupIgnore;
  fTypes.Add('Integer'); fTypes.Add('Long'); fTypes.Add('Single'); fTypes.Add('Double');
  fTypes.Add('String'); fTypes.Add('Boolean'); fTypes.Add('Date'); fTypes.Add('Object');
  fTypes.Add('Variant'); fTypes.Add('Byte'); fTypes.Add('Currency'); fTypes.Add('Decimal');
  fTypes.Add('Char'); fTypes.Add('Short'); fTypes.Add('ULong'); fTypes.Add('UShort'); fTypes.Add('UInteger');
  fTypes.Add('SByte'); fTypes.Add('DBNull'); fTypes.Add('Void'); fTypes.Add('Any'); // VB6 "Any" type

  fDirectives := TStringList.Create;
  fDirectives.CaseSensitive := False;
  fDirectives.Sorted := True;
  fDirectives.Duplicates := TDuplicates.dupIgnore;
  fDirectives.Add('#If'); fDirectives.Add('#Else'); fDirectives.Add('#ElseIf'); fDirectives.Add('#End If');
  fDirectives.Add('#Const'); fDirectives.Add('#ExternalSource'); fDirectives.Add('#Region'); fDirectives.Add('#End Region');
  fDirectives.Add('#Disable Warning'); fDirectives.Add('#Enable Warning'); // VB.NET specific

  fFunctions := TStringList.Create;
  fFunctions.CaseSensitive := False;
  fFunctions.Sorted := True;
  fFunctions.Duplicates := TDuplicates.dupIgnore;
  fFunctions.Add('MsgBox'); fFunctions.Add('InputBox'); fFunctions.Add('Len'); fFunctions.Add('Mid');
  fFunctions.Add('Left'); fFunctions.Add('Right'); fFunctions.Add('InStr'); fFunctions.Add('Format');
  fFunctions.Add('CDate'); fFunctions.Add('Now'); fFunctions.Add('DateDiff'); fFunctions.Add('IsEmpty');
  fFunctions.Add('IsNumeric'); fFunctions.Add('IsDate'); fFunctions.Add('IsObject'); fFunctions.Add('IsNull');
  fFunctions.Add('Trim'); fFunctions.Add('LTrim'); fFunctions.Add('RTrim'); fFunctions.Add('Replace');
  fFunctions.Add('UBound'); fFunctions.Add('LBound'); fFunctions.Add('Array');
  fFunctions.Add('Chr'); fFunctions.Add('Asc'); fFunctions.Add('String'); fFunctions.Add('Space');
  fFunctions.Add('Rnd'); fFunctions.Add('Randomize'); fFunctions.Add('Sqr'); fFunctions.Add('Abs');
  fFunctions.Add('Sin'); fFunctions.Add('Cos'); fFunctions.Add('Tan'); fFunctions.Add('Atn'); fFunctions.Add('Log'); fFunctions.Add('Exp');
  fFunctions.Add('Fix'); fFunctions.Add('Int'); fFunctions.Add('Round');
  fFunctions.Add('CreateObject'); fFunctions.Add('GetObject');
  fFunctions.Add('Val'); fFunctions.Add('Str'); fFunctions.Add('Hex'); fFunctions.Add('Oct');
  // From your interpreter
  fFunctions.Add('StrToIntDef'); fFunctions.Add('IntToStr'); fFunctions.Add('SplitString');

  fOperators := TStringList.Create;
  fOperators.CaseSensitive := True; // Operators are usually case-sensitive
  fOperators.Sorted := True;
  fOperators.Duplicates := TDuplicates.dupIgnore;
  fOperators.Add('='); fOperators.Add('<>'); fOperators.Add('>'); fOperators.Add('<');
  fOperators.Add('>='); fOperators.Add('<='); fOperators.Add('+'); fOperators.Add('-');
  fOperators.Add('*'); fOperators.Add('/'); fOperators.Add('\'); fOperators.Add('^');
  fOperators.Add('&'); // String concatenation
  fOperators.Add('.'); // Member access
  fOperators.Add('!'); // Default member access (rarely used now)
  fOperators.Add(':'); // Statement separator
  fOperators.Add(','); // Separator

  // fComments is not used as a TStringList for actual comment content,
  // it's more for defining comment delimiters if needed, but ' and Rem are handled directly.
  // The line fComments.Free; is kept for consistency if it was intended for other use.
end;

procedure TSynVBHighlighter.DoIdentifyLine(Line: Integer; var TokenOffset: Integer; var State: TSynHighlighterState);
begin
  inherited DoIdentifyLine(Line, TokenOffset, State);

  // For VB, multi-line comments are not standard.
  // This part of the highlighter is more relevant for languages with /* ... */
  // or multi-line string literals.
  // We explicitly handle ' and Rem as single-line comments in GetToken.
  fInCommentBlock := False; // Always reset, as VB comments are single-line
end;


function TSynVBHighlighter.GetToken: TSynIdentifier;
var
  i: Integer;
  s: String;
begin
  i := TokenPos;
  CurrentTokenOffset := i;

  // --- Handle multi-line comment state (not typical for VB, but kept for general SynHighlighter structure) ---
  // If you *were* to support a custom multi-line comment (e.g., /* ... */)
  // this is where you'd check for its end.
  // Since VB uses single-line ' and Rem, fInCommentBlock should generally be False.
  if fInCommentBlock then
  begin
    // For a *real* block comment, you'd scan for its end delimiter.
    // For now, if fInCommentBlock is somehow true, just treat the rest of the line as comment
    // and reset the state for the next line, as VB comments are always line-ending.
    SetTokenLength(Length(LineBuffer) - i + 1);
    Result := SYN_COMMENT;
    fInCommentBlock := False; // Ensure it's reset
    Exit;
  end;

  // --- Skip whitespace ---
  if (i <= Length(LineBuffer)) and CharInSet(LineBuffer[i], [' ', #9]) then
  begin
    while (i <= Length(LineBuffer)) and CharInSet(LineBuffer[i], [' ', #9]) do
      Inc(i);
    SetTokenLength(i - TokenPos);
    Result := SYN_WHITESPACE;
    Exit;
  end;

  // --- Handle single-line comments (' or Rem) ---
  if (i <= Length(LineBuffer)) and (LineBuffer[i] = '''') then
  begin
    SetTokenLength(Length(LineBuffer) - i + 1);
    Result := SYN_COMMENT;
    Exit;
  end;

  // Using >= for comparison to avoid index out of bounds
  if (i <= Length(LineBuffer) - 2) and (LowerCase(Copy(LineBuffer, i, 3)) = 'rem') then
  begin
    SetTokenLength(Length(LineBuffer) - i + 1);
    Result := SYN_COMMENT;
    Exit;
  end;

  // --- Handle Strings ---
  if (i <= Length(LineBuffer)) and (LineBuffer[i] = '"') then
  begin
    Inc(i); // Skip opening quote
    while (i <= Length(LineBuffer)) do
    begin
      if LineBuffer[i] = '"' then // Found a quote
      begin
        if (i < Length(LineBuffer)) and (LineBuffer[i+1] = '"') then // Escaped quote ""
          Inc(i) // Skip both quotes
        else
          Break; // Found end quote
      end;
      Inc(i);
    end;
    if (i <= Length(LineBuffer)) and (LineBuffer[i] = '"') then
      Inc(i); // Include closing quote
    SetTokenLength(i - TokenPos);
    Result := SYN_STRING;
    Exit;
  end;

  // --- Handle Numbers ---
  if (i <= Length(LineBuffer)) and CharInSet(LineBuffer[i], ['0'..'9']) then
  begin
    while (i <= Length(LineBuffer)) and CharInSet(LineBuffer[i], ['0'..'9']) do
      Inc(i);
    if (i <= Length(LineBuffer)) and (LineBuffer[i] = '.') then // Decimal point
    begin
      Inc(i);
      while (i <= Length(LineBuffer)) and CharInSet(LineBuffer[i], ['0'..'9']) do
        Inc(i);
    end;
    // Handle exponents (e.g., 1.23E+05)
    if (i <= Length(LineBuffer)) and (LowerCase(LineBuffer[i]) = 'e') then
    begin
      Inc(i);
      if (i <= Length(LineBuffer)) and CharInSet(LineBuffer[i], ['+', '-']) then Inc(i);
      while (i <= Length(LineBuffer)) and CharInSet(LineBuffer[i], ['0'..'9']) do
        Inc(i);
    end;
    // Handle type suffixes (e.g., &H for hex, &O for octal, L for long, S for single, D for double)
    if (i <= Length(LineBuffer)) then
    begin
      case LowerCase(LineBuffer[i]) of
        's', 'd', 'f', 'l', 'i', 'r', 'c', '@', '!': // VB.NET type characters or VB6 type declaration characters
          Inc(i);
      end;
    end;
    // Handle &H (Hex) and &O (Octal) prefixes
    if (TokenPos <= Length(LineBuffer) - 2) and (LineBuffer[TokenPos] = '&') and
       (LowerCase(LineBuffer[TokenPos+1]) = 'h') then // Hex
    begin
      Inc(i, 2); // Skip &H
      while (i <= Length(LineBuffer)) and CharInSet(LineBuffer[i], ['0'..'9', 'A'..'F', 'a'..'f']) do
        Inc(i);
    end
    else if (TokenPos <= Length(LineBuffer) - 2) and (LineBuffer[TokenPos] = '&') and
            (LowerCase(LineBuffer[TokenPos+1]) = 'o') then // Octal
    begin
      Inc(i, 2); // Skip &O
      while (i <= Length(LineBuffer)) and CharInSet(LineBuffer[i], ['0'..'7']) do
        Inc(i);
    end;

    SetTokenLength(i - TokenPos);
    Result := SYN_NUMBER;
    Exit;
  end;

  // --- Handle Identifiers (Keywords, Types, Functions, Variables) ---
  if (i <= Length(LineBuffer)) and CharInSet(LineBuffer[i], ['A'..'Z', 'a'..'z', '_']) then
  begin
    while (i <= Length(LineBuffer)) and CharInSet(LineBuffer[i], ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
      Inc(i);
    SetTokenLength(i - TokenPos);
    s := GetTokenString(SYN_IDENTIFIER);

    if fKeywords.IndexOf(s) <> -1 then
      Result := SYN_KEYWORD
    else if fTypes.IndexOf(s) <> -1 then
      Result := SYN_TYPE // Custom token for types
    else if fFunctions.IndexOf(s) <> -1 then
      Result := SYN_FUNCTION // Custom token for functions
    else
      Result := SYN_IDENTIFIER; // Default identifier
    Exit;
  end;

  // --- Handle Pre-processor directives (like #If, #Const) ---
  if (i <= Length(LineBuffer)) and (LineBuffer[i] = '#') then
  begin
    Inc(i); // Skip '#'
    // Directives can have spaces, but the first part is key (e.g., #If, #Const)
    // Read until first space or end of line for the directive name
    while (i <= Length(LineBuffer)) and CharInSet(LineBuffer[i], ['A'..'Z', 'a'..'z']) do
      Inc(i);
    s := Copy(LineBuffer, TokenPos + 1, (i - 1) - (TokenPos)); // Get directive name without #
    if fDirectives.IndexOf('#' + s) <> -1 then
    begin
      SetTokenLength(i - TokenPos);
      Result := SYN_PREPROCESSOR; // Custom token for preprocessor
    end
    else
    begin
      // If it's just '#' followed by something else, treat as symbol
      SetTokenLength(1);
      Result := SYN_SYMBOL;
    end;
    Exit;
  end;

  // --- Handle Symbols and Operators (single or multi-character) ---
  case LineBuffer[i] of
    // Multi-character operators first to prioritize longer matches
    '<':
      if (i < Length(LineBuffer)) and (LineBuffer[i+1] = '=') then
      begin
        SetTokenLength(2); Result := SYN_OPERATOR; Exit;
      end
      else if (i < Length(LineBuffer)) and (LineBuffer[i+1] = '>') then
      begin
        SetTokenLength(2); Result := SYN_OPERATOR; Exit; // Not equal
      end;
    '>':
      if (i < Length(LineBuffer)) and (LineBuffer[i+1] = '=') then
      begin
        SetTokenLength(2); Result := SYN_OPERATOR; Exit;
      end;
    // Single-character operators/symbols
    '=', '+', '-', '*', '/', '\', '^', '&', '(', ')', '[', ']', '.', ',', ';', ':':
      SetTokenLength(1);
      Result := SYN_OPERATOR; // Or SYN_SYMBOL if you want to differentiate
      Exit;
    else
      // Unknown character, advance by one and treat as whitespace/default
      SetTokenLength(1);
      Result := SYN_WHITESPACE;
      Exit;
  end;
end;

function TSynVBHighlighter.GetDefaultKeywords(Index: Integer): String;
begin
  Result := '';
end;

function TSynVBHighlighter.GetTokenPos(Token: TSynIdentifier): Integer;
begin
  Result := CurrentTokenOffset;
end;

function TSynVBHighlighter.GetTokenString(Token: TSynIdentifier): String;
begin
  Result := Copy(LineBuffer, CurrentTokenOffset, GetTokenLength);
end;

procedure TSynVBHighlighter.Reset;
begin
  inherited Reset;
  fInCommentBlock := False; // Reset state for parsing a new document
end;

function TSynVBHighlighter.GetANSIStyle(Token: TSynIdentifier): TVBHighlightStyle;
begin
  Result := FANSICodes[Token];
end;

end.
