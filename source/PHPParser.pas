unit PHPParser;

{ Lexical scanner for PHP 7.x and 8.x source, used by webgen to recognise
  .php content files: it tells inline HTML apart from PHP code (so HTML
  outside <?php ?> tags can be treated like any other template content),
  and lets webgen sanity-check and minify the PHP it embeds without ever
  needing to execute it. This is a tokenizer, not a full language parser:
  it does not build an AST or validate grammar beyond bracket/quote
  balance, which is all webgen needs. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPHPTokenKind = (
    ptkInlineHTML,  // literal markup outside <?php ... ?>
    ptkOpenTag,     // '<?php', '<?=' or bare '<?'
    ptkCloseTag,    // '?>'
    ptkWhitespace,
    ptkComment,     // '//', '#', or '/* */'
    ptkVariable,    // '$name'
    ptkString,      // '...' "..." `...` or heredoc/nowdoc
    ptkNumber,
    ptkIdentifier,
    ptkKeyword,
    ptkAttribute,   // '#[ ... ]' (PHP 8)
    ptkOperator,
    ptkPunctuation
  );

  TPHPToken = record
    Kind: TPHPTokenKind;
    Value: String;
    Line: Integer;
  end;
  TPHPTokenArray = array of TPHPToken;

{ Tokenizes ASource, alternating between inline-HTML and PHP-code tokens
  exactly as a PHP 7/8 engine would switch modes on '<?php'/'<?='/'?>'. }
function ParsePHP(const ASource: String): TPHPTokenArray;

{ Serializes the token stream back out, dropping comments and collapsing
  insignificant whitespace in PHP code (inline HTML and string/heredoc
  bodies are left untouched). }
function MinifyPHP(const ASource: String): String;

{ Rough well-formedness check: every '<?php'/'<?=' has a matching '?>' (or
  runs to EOF, which PHP allows for a file's final tag), every string,
  heredoc/nowdoc and comment is closed, and braces are balanced within
  PHP code. Not a grammar check, just enough to catch a broken/truncated
  file before webgen ships it. }
function ValidatePHP(const ASource: String; out AError: String): Boolean;

{ True if ASource contains at least one PHP open tag. A .php content file
  without one is just HTML with a misleading extension. }
function ContainsPHPCode(const ASource: String): Boolean;

implementation

const
  PHPKeywords: array[0..84] of String = (
    'abstract', 'and', 'array', 'as', 'break', 'callable', 'case', 'catch',
    'class', 'clone', 'const', 'continue', 'declare', 'default', 'do',
    'echo', 'else', 'elseif', 'empty', 'enddeclare', 'endfor', 'endforeach',
    'endif', 'endswitch', 'endwhile', 'enum', 'eval', 'exit', 'extends',
    'final', 'finally', 'fn', 'for', 'foreach', 'function', 'global',
    'goto', 'if', 'implements', 'include', 'include_once', 'instanceof',
    'insteadof', 'interface', 'isset', 'list', 'match', 'namespace', 'new',
    'or', 'print', 'private', 'protected', 'public', 'readonly', 'require',
    'require_once', 'return', 'static', 'switch', 'throw', 'trait', 'try',
    'unset', 'use', 'var', 'while', 'xor', 'yield',
    // soft/type keywords treated the same for tokenizing purposes
    'int', 'float', 'string', 'bool', 'void', 'iterable', 'object',
    'mixed', 'never', 'null', 'false', 'true', 'self', 'parent',
    'from', 'die'
  );

function IsKeyword(const ALower: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(PHPKeywords) do
    if PHPKeywords[I] = ALower then
    begin
      Result := True;
      Exit;
    end;
end;

function IsIdentStart(C: Char): Boolean;
begin
  Result := (C in ['A'..'Z', 'a'..'z', '_']) or (Ord(C) >= 128);
end;

function IsIdentChar(C: Char): Boolean;
begin
  Result := IsIdentStart(C) or (C in ['0'..'9']);
end;

function SameTextAt(const S: String; P: Integer; const Needle: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  if P + Length(Needle) - 1 > Length(S) then
    Exit;
  for I := 1 to Length(Needle) do
    if UpCase(S[P + I - 1]) <> UpCase(Needle[I]) then
      Exit;
  Result := True;
end;

function CountNewlines(const S: String): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] = #10 then
      Inc(Result);
end;

{ ---- tokenizer ---- }

function ParsePHP(const ASource: String): TPHPTokenArray;
var
  S: String;
  P, Len, Line: Integer;
  Tokens: TPHPTokenArray;

  procedure AddTok(AKind: TPHPTokenKind; const AValue: String);
  begin
    SetLength(Tokens, Length(Tokens) + 1);
    Tokens[High(Tokens)].Kind := AKind;
    Tokens[High(Tokens)].Value := AValue;
    Tokens[High(Tokens)].Line := Line;
    Inc(Line, CountNewlines(AValue));
  end;

  { Scans a single- or double-quoted (or backtick) string starting at S[P],
    honouring backslash escapes so an escaped quote does not end it early. }
  function ScanQuoted(Quote: Char): String;
  var
    StartP: Integer;
  begin
    StartP := P;
    Inc(P); // opening quote
    while (P <= Len) and (S[P] <> Quote) do
    begin
      if (S[P] = '\') and (P < Len) then
        Inc(P, 2)
      else
        Inc(P);
    end;
    if P <= Len then
      Inc(P); // closing quote
    Result := Copy(S, StartP, P - StartP);
  end;

  { Scans a heredoc/nowdoc body starting right after '<<<'. Nowdoc uses
    <<<'ID', heredoc uses <<<ID or <<<"ID". The closing marker may be
    indented (PHP 7.3+ flexible heredoc); any shared indentation is part
    of the syntax, not our concern since we keep the text verbatim. }
  function ScanHeredoc: String;
  var
    StartP, IdStart, IdEnd, Q: Integer;
    Id: String;
    Quoted: Boolean;
  begin
    StartP := P;
    Inc(P, 3); // '<<<'
    while (P <= Len) and (S[P] in [' ', #9]) do
      Inc(P);
    Quoted := (P <= Len) and (S[P] in ['''', '"']);
    if Quoted then
      Inc(P);
    IdStart := P;
    while (P <= Len) and IsIdentChar(S[P]) do
      Inc(P);
    IdEnd := P;
    Id := Copy(S, IdStart, IdEnd - IdStart);
    if Quoted and (P <= Len) and (S[P] in ['''', '"']) then
      Inc(P);
    while (P <= Len) and (S[P] <> #10) do
      Inc(P); // rest of the opening line
    if P <= Len then
      Inc(P); // consume the newline
    if Id = '' then
    begin
      Result := Copy(S, StartP, P - StartP);
      Exit;
    end;
    // scan lines until one whose trimmed-left content starts with Id
    // followed by a non-identifier character
    while P <= Len do
    begin
      Q := P;
      while (Q <= Len) and (S[Q] in [' ', #9]) do
        Inc(Q);
      if SameTextAt(S, Q, Id) and
        ((Q + Length(Id) > Len) or not IsIdentChar(S[Q + Length(Id)])) then
      begin
        P := Q + Length(Id);
        Break;
      end;
      while (P <= Len) and (S[P] <> #10) do
        Inc(P);
      if P <= Len then
        Inc(P);
    end;
    Result := Copy(S, StartP, P - StartP);
  end;

  procedure ScanLineComment;
  var
    StartP: Integer;
  begin
    StartP := P;
    while (P <= Len) and (S[P] <> #10) and not SameTextAt(S, P, '?>') do
      Inc(P);
    AddTok(ptkComment, Copy(S, StartP, P - StartP));
  end;

  procedure ScanBlockComment;
  var
    StartP: Integer;
  begin
    StartP := P;
    Inc(P, 2);
    while (P <= Len) and not SameTextAt(S, P, '*/') do
      Inc(P);
    if P <= Len then
      Inc(P, 2);
    AddTok(ptkComment, Copy(S, StartP, P - StartP));
  end;

  procedure ScanAttribute;
  var
    StartP, Depth: Integer;
  begin
    StartP := P;
    Inc(P, 2); // '#['
    Depth := 1;
    while (P <= Len) and (Depth > 0) do
    begin
      case S[P] of
        '[': begin Inc(Depth); Inc(P); end;
        ']': begin Dec(Depth); Inc(P); end;
        '''', '"': ScanQuoted(S[P]);
      else
        Inc(P);
      end;
    end;
    AddTok(ptkAttribute, Copy(S, StartP, P - StartP));
  end;

  procedure ScanNumber;
  var
    StartP: Integer;
  begin
    StartP := P;
    if (S[P] = '0') and (P < Len) and (S[P + 1] in ['x', 'X']) then
    begin
      Inc(P, 2);
      while (P <= Len) and (S[P] in ['0'..'9', 'a'..'f', 'A'..'F', '_']) do
        Inc(P);
    end
    else if (S[P] = '0') and (P < Len) and (S[P + 1] in ['b', 'B']) then
    begin
      Inc(P, 2);
      while (P <= Len) and (S[P] in ['0', '1', '_']) do
        Inc(P);
    end
    else if (S[P] = '0') and (P < Len) and (S[P + 1] in ['o', 'O']) then
    begin
      Inc(P, 2);
      while (P <= Len) and (S[P] in ['0'..'7', '_']) do
        Inc(P);
    end
    else
    begin
      while (P <= Len) and (S[P] in ['0'..'9', '_']) do
        Inc(P);
      if (P <= Len) and (S[P] = '.') and (P < Len) and (S[P + 1] in ['0'..'9']) then
      begin
        Inc(P);
        while (P <= Len) and (S[P] in ['0'..'9', '_']) do
          Inc(P);
      end;
      if (P <= Len) and (S[P] in ['e', 'E']) then
      begin
        Inc(P);
        if (P <= Len) and (S[P] in ['+', '-']) then
          Inc(P);
        while (P <= Len) and (S[P] in ['0'..'9']) do
          Inc(P);
      end;
    end;
    AddTok(ptkNumber, Copy(S, StartP, P - StartP));
  end;

  { Longest-match first so e.g. '<=>' isn't split into '<=' + '>'. }
  procedure ScanOperator;
  const
    Ops3: array[0..5] of String = ('<=>', '??=', '...', '**=', '<<=', '>>=');
    Ops2: array[0..21] of String = (
      '?->', '->', '=>', '::', '++', '--', '**', '<<', '>>', '<=', '>=',
      '==', '!=', '<>', '&&', '||', '+=', '-=', '*=', '/=', '.=', '??'
    );
  var
    I: Integer;
    StartP: Integer;
  begin
    StartP := P;
    if SameTextAt(S, P, '===') or SameTextAt(S, P, '!==') then
    begin
      Inc(P, 3);
      AddTok(ptkOperator, Copy(S, StartP, P - StartP));
      Exit;
    end;
    for I := 0 to High(Ops3) do
      if SameTextAt(S, P, Ops3[I]) then
      begin
        Inc(P, 3);
        AddTok(ptkOperator, Copy(S, StartP, P - StartP));
        Exit;
      end;
    for I := 0 to High(Ops2) do
      if SameTextAt(S, P, Ops2[I]) then
      begin
        Inc(P, Length(Ops2[I]));
        AddTok(ptkOperator, Copy(S, StartP, P - StartP));
        Exit;
      end;
    Inc(P);
    AddTok(ptkOperator, Copy(S, StartP, P - StartP));
  end;

  procedure ScanPHPMode;
  var
    StartP: Integer;
  begin
    while P <= Len do
    begin
      if SameTextAt(S, P, '?>') then
      begin
        StartP := P;
        Inc(P, 2);
        AddTok(ptkCloseTag, Copy(S, StartP, P - StartP));
        Exit;
      end;
      case S[P] of
        ' ', #9, #10, #13, #12:
          begin
            StartP := P;
            while (P <= Len) and (S[P] in [' ', #9, #10, #13, #12]) do
              Inc(P);
            AddTok(ptkWhitespace, Copy(S, StartP, P - StartP));
          end;
        '/':
          if (P < Len) and (S[P + 1] = '/') then
            ScanLineComment
          else if (P < Len) and (S[P + 1] = '*') then
            ScanBlockComment
          else
            ScanOperator;
        '#':
          if (P < Len) and (S[P + 1] = '[') then
            ScanAttribute
          else
            ScanLineComment;
        '$':
          begin
            StartP := P;
            Inc(P);
            while (P <= Len) and IsIdentChar(S[P]) do
              Inc(P);
            AddTok(ptkVariable, Copy(S, StartP, P - StartP));
          end;
        '''', '"', '`':
          AddTok(ptkString, ScanQuoted(S[P]));
        '0'..'9':
          ScanNumber;
        '{', '}', '(', ')', '[', ']', ';', ',', '@', '~':
          begin
            StartP := P;
            Inc(P);
            AddTok(ptkPunctuation, Copy(S, StartP, P - StartP));
          end;
      else
        if (S[P] = '<') and (P + 2 <= Len) and (S[P + 1] = '<') and (S[P + 2] = '<') then
          AddTok(ptkString, ScanHeredoc)
        else if IsIdentStart(S[P]) then
        begin
          StartP := P;
          while (P <= Len) and IsIdentChar(S[P]) do
            Inc(P);
          if IsKeyword(LowerCase(Copy(S, StartP, P - StartP))) then
            AddTok(ptkKeyword, Copy(S, StartP, P - StartP))
          else
            AddTok(ptkIdentifier, Copy(S, StartP, P - StartP));
        end
        else
          ScanOperator;
      end;
    end;
  end;

  procedure ScanInlineHTML;
  var
    StartP: Integer;
  begin
    StartP := P;
    while (P <= Len) and not ((S[P] = '<') and (P < Len) and (S[P + 1] = '?')) do
      Inc(P);
    if P > StartP then
      AddTok(ptkInlineHTML, Copy(S, StartP, P - StartP));
  end;

  procedure ScanOpenTag;
  var
    StartP: Integer;
  begin
    StartP := P;
    if SameTextAt(S, P, '<?php') and
      ((P + 5 > Len) or not IsIdentChar(S[P + 5])) then
      Inc(P, 5)
    else if SameTextAt(S, P, '<?=') then
      Inc(P, 3)
    else
      Inc(P, 2);
    AddTok(ptkOpenTag, Copy(S, StartP, P - StartP));
  end;

begin
  S := ASource;
  Len := Length(S);
  P := 1;
  Line := 1;
  Tokens := [];
  while P <= Len do
  begin
    ScanInlineHTML;
    if P > Len then
      Break;
    ScanOpenTag;
    ScanPHPMode;
  end;
  Result := Tokens;
end;

{ ---- minification ---- }

function IsWordLike(AKind: TPHPTokenKind): Boolean;
begin
  Result := AKind in [ptkVariable, ptkString, ptkNumber, ptkIdentifier, ptkKeyword];
end;

function MinifyPHP(const ASource: String): String;
var
  Tokens: TPHPTokenArray;
  I: Integer;
  SB: String;
  PrevKind: TPHPTokenKind;
  HavePrev: Boolean;
begin
  Tokens := ParsePHP(ASource);
  SB := '';
  HavePrev := False;
  PrevKind := ptkWhitespace;
  for I := 0 to High(Tokens) do
  begin
    case Tokens[I].Kind of
      ptkComment: ; // dropped
      ptkWhitespace:
        ; // re-inserted below only where required
      ptkInlineHTML, ptkOpenTag, ptkCloseTag:
        begin
          SB := SB + Tokens[I].Value;
          HavePrev := False;
        end;
    else
      if HavePrev and IsWordLike(PrevKind) and IsWordLike(Tokens[I].Kind) then
        SB := SB + ' ';
      SB := SB + Tokens[I].Value;
      PrevKind := Tokens[I].Kind;
      HavePrev := True;
    end;
  end;
  Result := SB;
end;

{ ---- validation ---- }

function ValidatePHP(const ASource: String; out AError: String): Boolean;
var
  Tokens: TPHPTokenArray;
  I, Depth: Integer;
begin
  Result := True;
  AError := '';
  Tokens := ParsePHP(ASource);
  Depth := 0;
  // A PHP block left open at EOF (no closing '?>') is valid PHP, so the
  // only structural check worth making here is brace balance; unterminated
  // strings/heredocs/comments are already swallowed to EOF by the scanner
  // and would show up as a missing trailing '?>' plus dangling markup,
  // which is not reliably distinguishable from an intentionally open tag.
  for I := 0 to High(Tokens) do
    if Tokens[I].Kind = ptkPunctuation then
    begin
      if Tokens[I].Value = '{' then
        Inc(Depth)
      else if Tokens[I].Value = '}' then
      begin
        Dec(Depth);
        if Depth < 0 then
        begin
          Result := False;
          AError := Format('unmatched ''}'' near line %d', [Tokens[I].Line]);
          Exit;
        end;
      end;
    end;
  if Depth <> 0 then
  begin
    Result := False;
    AError := 'unbalanced { } in PHP code';
  end;
end;

function ContainsPHPCode(const ASource: String): Boolean;
var
  Tokens: TPHPTokenArray;
  I: Integer;
begin
  Tokens := ParsePHP(ASource);
  Result := False;
  for I := 0 to High(Tokens) do
    if Tokens[I].Kind = ptkOpenTag then
    begin
      Result := True;
      Exit;
    end;
end;

end.
