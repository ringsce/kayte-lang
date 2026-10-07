unit CSSParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCSSRule = class;
  TCSSRuleArray = array of TCSSRule;

  TCSSDeclaration = record
    Prop: String;
    Value: String;
  end;
  TCSSDeclarationList = array of TCSSDeclaration;

  { A single CSS rule. Regular rules carry a Selector and Declarations.
    At-rules with a block (@media, @supports, ...) carry Children instead
    of Declarations. At-rules without a block (@import "x.css";) set
    IsAtStatement and carry no Declarations/Children. }
  TCSSRule = class
  public
    Selector: String;
    Declarations: TCSSDeclarationList;
    Children: TCSSRuleArray;
    IsAtStatement: Boolean;
    destructor Destroy; override;
  end;

  TCSSStylesheet = class
  public
    Rules: TCSSRuleArray;
    destructor Destroy; override;
    function Minify: String;
  end;

function ParseCSS(const ASource: String): TCSSStylesheet;
function MinifyCSS(const ASource: String): String;

implementation

{ ---- low level scanning helpers ---- }

procedure SkipWhitespaceAndComments(const S: String; var P: Integer);
begin
  while P <= Length(S) do
  begin
    if S[P] in [' ', #9, #10, #13, #12] then
      Inc(P)
    else if (S[P] = '/') and (P < Length(S)) and (S[P + 1] = '*') then
    begin
      Inc(P, 2);
      while (P < Length(S)) and not ((S[P] = '*') and (S[P + 1] = '/')) do
        Inc(P);
      Inc(P, 2);
    end
    else
      Break;
  end;
end;

function CollapseWhitespace(const S: String): String;
var
  I: Integer;
  LastWasSpace: Boolean;
  SB: String;
begin
  SB := '';
  LastWasSpace := False;
  for I := 1 to Length(S) do
  begin
    if S[I] in [' ', #9, #10, #13, #12] then
    begin
      if not LastWasSpace then
        SB := SB + ' ';
      LastWasSpace := True;
    end
    else
    begin
      SB := SB + S[I];
      LastWasSpace := False;
    end;
  end;
  Result := Trim(SB);
end;

{ Reads raw text starting at P until a character in StopChars is found at
  paren-depth 0 and outside of a quoted string. Handles nested parens (for
  url(...), rgba(...), :not(...)) and single/double quoted strings so that
  stop characters inside them are ignored. Does not consume the stop char. }
function ReadRaw(const S: String; var P: Integer; const StopChars: TSysCharSet): String;
var
  StartP: Integer;
  InStr: Char;
  Depth: Integer;
begin
  StartP := P;
  InStr := #0;
  Depth := 0;
  while P <= Length(S) do
  begin
    if InStr <> #0 then
    begin
      if S[P] = '\' then
        Inc(P)
      else if S[P] = InStr then
        InStr := #0;
    end
    else
    begin
      case S[P] of
        '''', '"': InStr := S[P];
        '(': Inc(Depth);
        ')': if Depth > 0 then Dec(Depth);
      end;
      if (Depth = 0) and (InStr = #0) and (S[P] in StopChars) then
        Break;
    end;
    Inc(P);
  end;
  Result := Copy(S, StartP, P - StartP);
end;

{ Splits a selector list on top-level commas, ignoring commas inside
  parens (e.g. :not(a, b)) or quoted strings. }
function SplitSelectorList(const S: String): TStringArray;
var
  P, StartP: Integer;
  InStr: Char;
  Depth: Integer;
begin
  Result := [];
  P := 1;
  StartP := 1;
  InStr := #0;
  Depth := 0;
  while P <= Length(S) do
  begin
    if InStr <> #0 then
    begin
      if S[P] = InStr then
        InStr := #0;
    end
    else
    begin
      case S[P] of
        '''', '"': InStr := S[P];
        '(': Inc(Depth);
        ')': if Depth > 0 then Dec(Depth);
        ',':
          if Depth = 0 then
          begin
            Result := Concat(Result, [Copy(S, StartP, P - StartP)]);
            StartP := P + 1;
          end;
      end;
    end;
    Inc(P);
  end;
  Result := Concat(Result, [Copy(S, StartP, P - StartP)]);
end;

{ True if the block starting at P (the content just after an opening brace)
  contains a top-level opening brace before its matching closing brace,
  meaning the block holds nested rules (as with @media/@supports) rather
  than plain declarations. }
function BlockHoldsNestedRules(const S: String; StartP: Integer): Boolean;
var
  P, Depth: Integer;
  InStr: Char;
begin
  Result := False;
  P := StartP;
  InStr := #0;
  Depth := 0;
  while P <= Length(S) do
  begin
    if InStr <> #0 then
    begin
      if S[P] = '\' then
        Inc(P)
      else if S[P] = InStr then
        InStr := #0;
    end
    else
      case S[P] of
        '''', '"': InStr := S[P];
        '{':
          if Depth = 0 then
          begin
            Result := True;
            Exit;
          end
          else
            Inc(Depth);
        '}':
          if Depth = 0 then
            Exit
          else
            Dec(Depth);
      end;
    Inc(P);
  end;
end;

{ ---- parsing ---- }

function ParseDeclarations(const S: String; var P: Integer): TCSSDeclarationList;
var
  Decl: TCSSDeclaration;
  PropRaw, ValRaw: String;
begin
  Result := [];
  while True do
  begin
    SkipWhitespaceAndComments(S, P);
    if (P > Length(S)) or (S[P] = '}') then
      Break;
    if S[P] = ';' then
    begin
      Inc(P);
      Continue;
    end;
    PropRaw := ReadRaw(S, P, [':', ';', '}']);
    if (P > Length(S)) or (S[P] <> ':') then
    begin
      if (P <= Length(S)) and (S[P] = ';') then
        Inc(P);
      Continue;
    end;
    Inc(P); // consume ':'
    ValRaw := ReadRaw(S, P, [';', '}']);
    Decl.Prop := Trim(PropRaw);
    Decl.Value := Trim(ValRaw);
    if Length(Decl.Prop) > 0 then
      Result := Concat(Result, [Decl]);
    if (P <= Length(S)) and (S[P] = ';') then
      Inc(P);
  end;
end;

function ParseBlock(const S: String; var P: Integer): TCSSRuleArray;
var
  Rule: TCSSRule;
  Prelude: String;
  NestedStart: Integer;
begin
  Result := [];
  while True do
  begin
    SkipWhitespaceAndComments(S, P);
    if P > Length(S) then
      Break;
    if S[P] = '}' then
      Break; // let the caller consume the closing brace

    Prelude := Trim(ReadRaw(S, P, ['{', ';', '}']));

    if P > Length(S) then
      Break;

    if S[P] = ';' then
    begin
      Inc(P);
      if Length(Prelude) = 0 then
        Continue;
      Rule := TCSSRule.Create;
      Rule.Selector := Prelude;
      Rule.IsAtStatement := True;
      Result := Concat(Result, [Rule]);
      Continue;
    end;

    if S[P] = '}' then
      Break; // stray text with no block/terminator; drop it

    // S[P] = '{'
    Inc(P);
    Rule := TCSSRule.Create;
    Rule.Selector := Prelude;
    NestedStart := P;
    if BlockHoldsNestedRules(S, NestedStart) then
      Rule.Children := ParseBlock(S, P)
    else
      Rule.Declarations := ParseDeclarations(S, P);
    SkipWhitespaceAndComments(S, P);
    if (P <= Length(S)) and (S[P] = '}') then
      Inc(P);
    Result := Concat(Result, [Rule]);
  end;
end;

function ParseCSS(const ASource: String): TCSSStylesheet;
var
  P: Integer;
begin
  Result := TCSSStylesheet.Create;
  P := 1;
  Result.Rules := ParseBlock(ASource, P);
end;

{ ---- minification / serialization ---- }

function MinifySelector(const ASelector: String): String;
var
  Parts: TStringArray;
  I: Integer;
begin
  Parts := SplitSelectorList(ASelector);
  for I := 0 to High(Parts) do
    Parts[I] := CollapseWhitespace(Trim(Parts[I]));
  Result := '';
  for I := 0 to High(Parts) do
  begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + Parts[I];
  end;
end;

function SerializeRules(const Rules: TCSSRuleArray): String;
var
  I, J: Integer;
  R: TCSSRule;
  DeclStr: String;
begin
  Result := '';
  for I := 0 to High(Rules) do
  begin
    R := Rules[I];
    if R.IsAtStatement then
    begin
      Result := Result + CollapseWhitespace(R.Selector) + ';';
      Continue;
    end;

    Result := Result + MinifySelector(R.Selector) + '{';
    if Length(R.Children) > 0 then
      Result := Result + SerializeRules(R.Children)
    else
    begin
      DeclStr := '';
      for J := 0 to High(R.Declarations) do
      begin
        if J > 0 then
          DeclStr := DeclStr + ';';
        DeclStr := DeclStr + R.Declarations[J].Prop + ':' +
          CollapseWhitespace(R.Declarations[J].Value);
      end;
      Result := Result + DeclStr;
    end;
    Result := Result + '}';
  end;
end;

function TCSSStylesheet.Minify: String;
begin
  Result := SerializeRules(Rules);
end;

function MinifyCSS(const ASource: String): String;
var
  Sheet: TCSSStylesheet;
begin
  Sheet := ParseCSS(ASource);
  try
    Result := Sheet.Minify;
  finally
    Sheet.Free;
  end;
end;

{ ---- lifetime ---- }

destructor TCSSRule.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(Children) do
    Children[I].Free;
  inherited Destroy;
end;

destructor TCSSStylesheet.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(Rules) do
    Rules[I].Free;
  inherited Destroy;
end;

end.
