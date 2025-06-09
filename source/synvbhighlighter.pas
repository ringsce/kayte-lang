unit SynVBHighlighter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, SynHighlighter, SynPasSyn; // SynPasSyn is a good template

type

  { TSynVBHighlighter }

  TSynVBHighlighter = class(TSynCustomHighlighter)
  private
    // Private fields can be added here if needed for state or optimizations
    fKeywords: TStringList;
    fTypes: TStringList;
    fDirectives: TStringList;
    fFunctions: TStringList;
    fOperators: TStringList;
    fComments: TStringList; // For block comments that might span lines
    fInCommentBlock: Boolean; // State for multi-line comments

    // Styles for highlighting
    fKeywordStyle: TSynTextStyle;
    fTypeStyle: TSynTextStyle;
    fDirectiveStyle: TSynTextStyle;
    fFunctionStyle: TSynTextStyle;
    fOperatorStyle: TSynTextStyle;
    fCommentStyle: TSynTextStyle;
    fStringStyle: TSynTextStyle;
    fNumberStyle: TSynTextStyle;
    fIdentifierStyle: TSynTextStyle;
    fWhitespaceStyle: TSynTextStyle;
    fSymbolStyle: TSynTextStyle;
    fPreProcessorStyle: TSynTextStyle; // For directives like #If, #End If

    procedure SetDefaultStyles;
    procedure InitKeywords;

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

    // Public properties to customize styles
    property KeywordStyle: TSynTextStyle read fKeywordStyle write fKeywordStyle;
    property TypeStyle: TSynTextStyle read fTypeStyle write fTypeStyle;
    property DirectiveStyle: TSynTextStyle read fDirectiveStyle write fDirectiveStyle;
    property FunctionStyle: TSynTextStyle read fFunctionStyle write fFunctionStyle;
    property OperatorStyle: TSynTextStyle read fOperatorStyle write fOperatorStyle;
    property CommentStyle: TSynTextStyle read fCommentStyle write fCommentStyle;
    property StringStyle: TSynTextStyle read fStringStyle write fStringStyle;
    property NumberStyle: TSynTextStyle read fNumberStyle write fNumberStyle;
    property IdentifierStyle: TSynTextStyle read fIdentifierStyle write fIdentifierStyle;
    property WhitespaceStyle: TSynTextStyle read fWhitespaceStyle write fWhitespaceStyle;
    property SymbolStyle: TSynTextStyle read fSymbolStyle write fSymbolStyle;
    property PreProcessorStyle: TSynTextStyle read fPreProcessorStyle write fPreProcessorStyle;
  end;

implementation

{ TSynVBHighlighter }

constructor TSynVBHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDefaultStyles;
  InitKeywords;
  SetTokenTypes(SYN_IDENTIFIER, SYN_LAST_TOKEN); // Set up token types for SynEdit
end;

destructor TSynVBHighlighter.Destroy;
begin
  fKeywords.Free;
  fTypes.Free;
  fDirectives.Free;
  fFunctions.Free;
  fOperators.Free;
  fComments.Free;
  inherited Destroy;
end;

procedure TSynVBHighlighter.SetDefaultStyles;
begin
  // Initialize styles with sensible defaults (can be customized by user later)
  // These colors are just examples, adjust as you like
  fKeywordStyle := CreateTextStyle(clBlue, [], [fsBold]);
  fTypeStyle := CreateTextStyle(clNavy, [], []);
  fDirectiveStyle := CreateTextStyle(clPurple, [], []);
  fFunctionStyle := CreateTextStyle(clTeal, [], [fsItalic]);
  fOperatorStyle := CreateTextStyle(clRed, [], []);
  fCommentStyle := CreateTextStyle(clGreen, [], []);
  fStringStyle := CreateTextStyle(clMaroon, [], []);
  fNumberStyle := CreateTextStyle(clBlack, [], []);
  fIdentifierStyle := CreateTextStyle(clBlack, [], []);
  fWhitespaceStyle := CreateTextStyle(clBlack, [], []);
  fSymbolStyle := CreateTextStyle(clBlack, [], []);
  fPreProcessorStyle := CreateTextStyle(clGray, [], []);

  // Assign styles to internal SynEdit token types
  SetTextStyle(SYN_IDENTIFIER, fIdentifierStyle);
  SetTextStyle(SYN_KEYWORD, fKeywordStyle);
  SetTextStyle(SYN_COMMENT, fCommentStyle);
  SetTextStyle(SYN_STRING, fStringStyle);
  SetTextStyle(SYN_NUMBER, fNumberStyle);
  SetTextStyle(SYN_WHITESPACE, fWhitespaceStyle);
  SetTextStyle(SYN_SYMBOL, fSymbolStyle);

  // Map custom tokens to existing SynEdit types or new ones
  // We'll map them during GetToken based on what we identify
end;

procedure TSynVBHighlighter.InitKeywords;
begin
  fKeywords := TStringList.Create;
  fKeywords.CaseSensitive := False; // VB is case-insensitive
  fKeywords.Sorted := True; // For faster lookup using IndexOf
  fKeywords.Duplicates := TDuplicates.dupIgnore;

  // VB Keywords
  fKeywords.Add('If'); fKeywords.Add('Then'); fKeywords.Add('Else'); fKeywords.Add('ElseIf'); fKeywords.Add('End If');
  fKeywords.Add('For'); fKeywords.Add('Next'); fKeywords.Add('While'); fKeywords.Add('Wend');
  fKeywords.Add('Do'); fKeywords.Add('Loop'); fKeywords.Add('Until'); fKeywords.Add('While');
  fKeywords.Add('Select'); fKeywords.Add('Case'); fKeywords.Add('End Select');
  fKeywords.Add('Sub'); fKeywords.Add('End Sub'); fKeywords.Add('Function'); fKeywords.Add('End Function');
  fKeywords.Add('Property'); fKeywords.Add('End Property'); fKeywords.Add('Class'); fKeywords.Add('End Class');
  fKeywords.Add('Module'); fKeywords.Add('End Module'); fKeywords.Add('Structure'); fKeywords.Add('End Structure');
  fKeywords.Add('Enum'); fKeywords.Add('End Enum');
  fKeywords.Add('Dim'); fKeywords.Add('As'); fKeywords.Add('New'); fKeywords.Add('Set'); fKeywords.Add('Nothing');
  fKeywords.Add('Public'); fKeywords.Add('Private'); fKeywords.Add('Protected'); fKeywords.Add('Friend');
  fKeywords.Add('Static'); fKeywords.Add('Const');
  fKeywords.Add('True'); fKeywords.Add('False'); fKeywords.Add('Not'); fKeywords.Add('And'); fKeywords.Add('Or'); fKeywords.Add('Xor');
  fKeywords.Add('ByVal'); fKeywords.Add('ByRef'); fKeywords.Add('Optional'); fKeywords.Add('ParamArray');
  fKeywords.Add('Call'); fKeywords.Add('GoTo'); fKeywords.Add('Exit'); fKeywords.Add('Resume');
  fKeywords.Add('On Error'); fKeywords.Add('GoSub'); fKeywords.Add('Return');
  fKeywords.Add('With'); fKeywords.Add('End With');
  fKeywords.Add('Is'); fKeywords.Add('Like'); fKeywords.Add('Mod'); fKeywords.Add('CStr'); fKeywords.Add('CInt');
  fKeywords.Add('CBool'); fKeywords.Add('CDbl'); fKeywords.Add('CSng'); fKeywords.Add('CLng');
  // Add more as needed for VB6/VB.NET specifics
  fKeywords.Add('Print'); fKeywords.Add('Input'); fKeywords.Add('Rem'); // For your interpreter BASIC commands

  fTypes := TStringList.Create;
  fTypes.CaseSensitive := False;
  fTypes.Sorted := True;
  fTypes.Duplicates := TDuplicates.dupIgnore;
  fTypes.Add('Integer'); fTypes.Add('Long'); fTypes.Add('Single'); fTypes.Add('Double');
  fTypes.Add('String'); fTypes.Add('Boolean'); fTypes.Add('Date'); fTypes.Add('Object');
  fTypes.Add('Variant'); fTypes.Add('Byte'); fTypes.Add('Currency'); fTypes.Add('Decimal');

  fDirectives := TStringList.Create;
  fDirectives.CaseSensitive := False;
  fDirectives.Sorted := True;
  fDirectives.Duplicates := TDuplicates.dupIgnore;
  fDirectives.Add('#If'); fDirectives.Add('#Else'); fDirectives.Add('#ElseIf'); fDirectives.Add('#End If');
  fDirectives.Add('#Const'); fDirectives.Add('#ExternalSource'); fDirectives.Add('#Region'); fDirectives.Add('#End Region');

  fFunctions := TStringList.Create;
  fFunctions.CaseSensitive := False;
  fFunctions.Sorted := True;
  fFunctions.Duplicates := TDuplicates.dupIgnore;
  fFunctions.Add('MsgBox'); fFunctions.Add('InputBox'); fFunctions.Add('Len'); fFunctions.Add('Mid');
  fFunctions.Add('Left'); fFunctions.Add('Right'); fFunctions.Add('InStr'); fFunctions.Add('Format');
  fFunctions.Add('CDate'); fFunctions.Add('Now'); fFunctions.Add('DateDiff'); fFunctions.Add('IsEmpty');
  // Add more common VB functions
  fFunctions.Add('StrToIntDef'); fFunctions.Add('IntToStr'); // From your interpreter

  fOperators := TStringList.Create;
  fOperators.CaseSensitive := False;
  fOperators.Sorted := True;
  fOperators.Duplicates := TDuplicates.dupIgnore;
  fOperators.Add('='); fOperators.Add('<>'); fOperators.Add('>'); fOperators.Add('<');
  fOperators.Add('>='); fOperators.Add('<='); fOperators.Add('+'); fOperators.Add('-');
  fOperators.Add('*'); fOperators.Add('/'); fOperators.Add('\'); fOperators.Add('^');
  fOperators.Add('&'); // String concatenation

  fComments := TStringList.Create;
  fComments.CaseSensitive := True; // 'Rem' is case-insensitive, but we handle it via tokenizing
  fComments.Sorted := False; // Not used for lookup, just a placeholder if needed for other comment types
  fComments.Duplicates := TDuplicates.dupIgnore;
end;


// DoIdentifyLine is called for each line to determine initial state for GetToken
// Useful for multi-line constructs like block comments or preprocessor directives
procedure TSynVBHighlighter.DoIdentifyLine(Line: Integer; var TokenOffset: Integer; var State: TSynHighlighterState);
var
  S: String;
  CurrentCharIndex: Integer;
begin
  inherited DoIdentifyLine(Line, TokenOffset, State);

  // If the previous line ended in a block comment state, continue it
  if State = SYN_COMMENT then
  begin
    fInCommentBlock := True;
  end else
  begin
    fInCommentBlock := False;
  end;

  S := Lines[Line];
  CurrentCharIndex := 1;

  // Basic check for block comments (if VB6/VBA had them, typically they don't, but for example)
  // If you add /* ... */ for custom block comments, you'd implement it here.
  // VB/VBA/VB.NET primarily uses single-line comments (' or Rem)
  // For now, only single line comments are explicitly handled in GetToken.
  // This state management is more critical for languages like C/C++ or Pascal
  // For VB, it might not be strictly necessary if only ' and Rem are used.
end;


function TSynVBHighlighter.GetToken: TSynIdentifier;
var
  i: Integer;
  s: String;
begin
  // Start with current character pointer (TokenPos)
  i := TokenPos;
  CurrentTokenOffset := i;

  // Handle multi-line comment state (if applicable, though rare for VB)
  if fInCommentBlock then
  begin
    // Search for end of block comment if it existed
    // For VB, just reset if the previous line ended in a comment block state
    // and no new comment start is found.
    // Example for 'Rem' comments:
    if CharInSet(LineBuffer[i], [' ', #9]) then // Whitespace
    begin
      while CharInSet(LineBuffer[i], [' ', #9]) do
        Inc(i);
      SetTokenLength(i - TokenPos);
      Result := SYN_WHITESPACE;
      Exit;
    end;
    // If we're inside a 'Rem' or single-line comment, the whole rest of line is comment
    if (i < Length(LineBuffer)) and (LineBuffer[i] = '''') then
    begin
      SetTokenLength(Length(LineBuffer) - i + 1);
      Result := SYN_COMMENT;
      Exit;
    end;
    if (i <= Length(LineBuffer) - 2) and (LowerCase(Copy(LineBuffer, i, 3)) = 'rem') then
    begin
      SetTokenLength(Length(LineBuffer) - i + 1);
      Result := SYN_COMMENT;
      Exit;
    end;

    // If it's a continuing comment block that's not a single line ' or Rem,
    // and no end of comment found, continue as comment.
    // This part would be complex for proper block comments.
    // For now, let's assume ' and Rem are the only comments.
    // If not a comment, then it must be something else, reset fInCommentBlock.
    fInCommentBlock := False;
  end;


  // Skip whitespace
  if CharInSet(LineBuffer[i], [' ', #9]) then
  begin
    while CharInSet(LineBuffer[i], [' ', #9]) do
      Inc(i);
    SetTokenLength(i - TokenPos);
    Result := SYN_WHITESPACE;
    Exit;
  end;

  // Handle single-line comments (' or Rem)
  if (LineBuffer[i] = '''') then
  begin
    SetTokenLength(Length(LineBuffer) - i + 1);
    Result := SYN_COMMENT;
    Exit;
  end;

  if (i <= Length(LineBuffer) - 2) and (LowerCase(Copy(LineBuffer, i, 3)) = 'rem') then // "rem"
  begin
    SetTokenLength(Length(LineBuffer) - i + 1);
    Result := SYN_COMMENT;
    Exit;
  end;

  // Handle Strings
  if (LineBuffer[i] = '"') then
  begin
    Inc(i);
    while (i <= Length(LineBuffer)) and (LineBuffer[i] <> '"') do
    begin
      if LineBuffer[i] = '\' then Inc(i); // Handle escaped quotes if VB supports them (VB6 typically uses "" for escaped quote)
      Inc(i);
    end;
    if (i <= Length(LineBuffer)) and (LineBuffer[i] = '"') then
      Inc(i); // Include closing quote
    SetTokenLength(i - TokenPos);
    Result := SYN_STRING;
    Exit;
  end;

  // Handle Numbers
  if CharInSet(LineBuffer[i], ['0'..'9']) then
  begin
    while CharInSet(LineBuffer[i], ['0'..'9']) do
      Inc(i);
    if LineBuffer[i] = '.' then // Decimal point
    begin
      Inc(i);
      while CharInSet(LineBuffer[i], ['0'..'9']) do
        Inc(i);
    end;
    // Handle exponents (e.g., 1.23E+05)
    if (LowerCase(LineBuffer[i]) = 'e') then
    begin
      Inc(i);
      if CharInSet(LineBuffer[i], ['+', '-']) then Inc(i);
      while CharInSet(LineBuffer[i], ['0'..'9']) do
        Inc(i);
    end;
    SetTokenLength(i - TokenPos);
    Result := SYN_NUMBER;
    Exit;
  end;

  // Handle Identifiers (Keywords, Types, Functions, Variables)
  if CharInSet(LineBuffer[i], ['A'..'Z', 'a'..'z', '_']) then
  begin
    while CharInSet(LineBuffer[i], ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
      Inc(i);
    SetTokenLength(i - TokenPos);
    s := GetTokenString(SYN_IDENTIFIER); // Get the identified string
    if fKeywords.IndexOf(s) <> -1 then
      Result := SYN_KEYWORD
    else if fTypes.IndexOf(s) <> -1 then
      Result := SYN_TYPE // Use a custom token type for types if SynEdit allows it, otherwise map to SYN_IDENTIFIER with a custom style
    else if fFunctions.IndexOf(s) <> -1 then
      Result := SYN_FUNCTION // Similarly for functions
    else
      Result := SYN_IDENTIFIER; // Default identifier
    Exit;
  end;

  // Handle Pre-processor directives (like #If, #Const)
  if LineBuffer[i] = '#' then
  begin
    Inc(i);
    while (i <= Length(LineBuffer)) and CharInSet(LineBuffer[i], ['A'..'Z', 'a'..'z', '0'..'9']) do
      Inc(i);
    SetTokenLength(i - TokenPos);
    s := GetTokenString(SYN_IDENTIFIER);
    if fDirectives.IndexOf('#' + s) <> -1 then // Check with '#' prefix
      Result := SYN_PREPROCESSOR // Use a custom token type for preprocessor
    else
      Result := SYN_SYMBOL; // Fallback to symbol if not a known directive
    Exit;
  end;


  // Handle Symbols and Operators (single or multi-character)
  case LineBuffer[i] of
    '=', '+', '-', '*', '/', '\', '^', '&', '(', ')', '[', ']', '.', ',', ';', ':':
      // For multi-character operators like '<=' or '>='
      s := Copy(LineBuffer, i, 2);
      if (fOperators.IndexOf(s) <> -1) then
      begin
        SetTokenLength(2);
        Result := SYN_OPERATOR;
      end else
      begin
        SetTokenLength(1);
        Result := SYN_SYMBOL; // Or SYN_OPERATOR if all single char are operators
      end;
    else
      SetTokenLength(1);
      Result := SYN_WHITESPACE; // Fallback for unknown character, treats as whitespace
  end;
end;

function TSynVBHighlighter.GetDefaultKeywords(Index: Integer): String;
begin
  // Not strictly needed if using internal TStringLists for keywords
  Result := '';
end;

function TSynVBHighlighter.GetTokenPos(Token: TSynIdentifier): Integer;
begin
  // SynEdit uses TokenPos internally based on TokenLength
  Result := CurrentTokenOffset;
end;

function TSynVBHighlighter.GetTokenString(Token: TSynIdentifier): String;
begin
  // Returns the actual string of the current token
  Result := Copy(LineBuffer, CurrentTokenOffset, GetTokenLength);
end;

procedure TSynVBHighlighter.Reset;
begin
  inherited Reset;
  fInCommentBlock := False; // Reset state for parsing a new document
end;

end.
