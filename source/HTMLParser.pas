unit HTMLParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  THTMLNode = class;
  THTMLNodeArray = array of THTMLNode;

  THTMLAttribute = record
    Name: String;
    Value: String;
    HasValue: Boolean;
    Quote: Char;
  end;
  THTMLAttributeList = array of THTMLAttribute;

  THTMLNodeKind = (nkElement, nkText, nkComment, nkDoctype);

  { nkElement carries TagName/Attributes/Children. nkText/nkComment/nkDoctype
    carry their raw content in Text. RawText marks a text node taken verbatim
    from inside <script>/<style>/<textarea>, whose content is not HTML. }
  THTMLNode = class
  public
    Kind: THTMLNodeKind;
    TagName: String;
    Attributes: THTMLAttributeList;
    SelfClosing: Boolean;
    Children: THTMLNodeArray;
    Text: String;
    RawText: Boolean;
    destructor Destroy; override;
  end;

  THTMLDocument = class
  public
    Nodes: THTMLNodeArray;
    destructor Destroy; override;
    function Minify: String;
  end;

function ParseHTML(const ASource: String): THTMLDocument;
function MinifyHTML(const ASource: String): String;

implementation

{ ---- low level scanning helpers ---- }

function IsNameChar(C: Char): Boolean;
begin
  Result := C in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', ':'];
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

{ True when P is at the end of the string or at a character that cannot
  continue a tag/attribute name, used to avoid e.g. "</script" matching
  the start of "</scriptx>". }
function IsNameBoundary(const S: String; P: Integer): Boolean;
begin
  Result := (P > Length(S)) or not IsNameChar(S[P]);
end;

procedure SkipWhitespace(const S: String; var P: Integer);
begin
  while (P <= Length(S)) and (S[P] in [' ', #9, #10, #13, #12]) do
    Inc(P);
end;

function ReadTagName(const S: String; var P: Integer): String;
var
  StartP: Integer;
begin
  StartP := P;
  while (P <= Length(S)) and IsNameChar(S[P]) do
    Inc(P);
  Result := Copy(S, StartP, P - StartP);
end;

function IsVoidElement(const ALowerTag: String): Boolean;
begin
  case ALowerTag of
    'area', 'base', 'br', 'col', 'embed', 'hr', 'img', 'input',
    'link', 'meta', 'param', 'source', 'track', 'wbr': Result := True;
  else
    Result := False;
  end;
end;

function IsRawTextElement(const ALowerTag: String): Boolean;
begin
  case ALowerTag of
    'script', 'style', 'textarea': Result := True;
  else
    Result := False;
  end;
end;

{ HTML allows several end tags to be omitted (e.g. <li>a<li>b</ul> instead
  of <li>a</li><li>b</li></ul>): a new sibling of one of these kinds
  implicitly closes the still-open one instead of nesting inside it. }
function ClosesOnSibling(const ASelfLower, ANewLower: String): Boolean;
begin
  Result := False;
  case ASelfLower of
    'li': Result := ANewLower = 'li';
    'dd', 'dt': Result := (ANewLower = 'dd') or (ANewLower = 'dt');
    'option': Result := (ANewLower = 'option') or (ANewLower = 'optgroup');
    'optgroup': Result := ANewLower = 'optgroup';
    'tr': Result := ANewLower = 'tr';
    'td', 'th': Result := (ANewLower = 'td') or (ANewLower = 'th') or (ANewLower = 'tr');
    'thead', 'tbody', 'tfoot':
      Result := (ANewLower = 'thead') or (ANewLower = 'tbody') or (ANewLower = 'tfoot');
    'p':
      case ANewLower of
        'address', 'article', 'aside', 'blockquote', 'details', 'div', 'dl',
        'fieldset', 'figcaption', 'figure', 'footer', 'form', 'h1', 'h2',
        'h3', 'h4', 'h5', 'h6', 'header', 'hr', 'main', 'menu', 'nav', 'ol',
        'p', 'pre', 'section', 'table', 'ul': Result := True;
      end;
  end;
end;

function StackContains(const AStack: TStringArray; const AName: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(AStack) do
    if AStack[I] = AName then
    begin
      Result := True;
      Exit;
    end;
end;

function ParseAttributes(const S: String; var P: Integer): THTMLAttributeList;
var
  Attr: THTMLAttribute;
  StartP: Integer;
begin
  Result := [];
  while True do
  begin
    SkipWhitespace(S, P);
    if (P > Length(S)) or (S[P] in ['>', '/']) then
      Break;
    StartP := P;
    while (P <= Length(S)) and not (S[P] in [' ', #9, #10, #13, #12, '=', '>', '/']) do
      Inc(P);
    if P = StartP then
      Break; // stray character, avoid an infinite loop
    Attr.Name := Copy(S, StartP, P - StartP);
    Attr.Value := '';
    Attr.HasValue := False;
    Attr.Quote := #0;
    SkipWhitespace(S, P);
    if (P <= Length(S)) and (S[P] = '=') then
    begin
      Inc(P);
      SkipWhitespace(S, P);
      Attr.HasValue := True;
      if (P <= Length(S)) and (S[P] in ['"', '''']) then
      begin
        Attr.Quote := S[P];
        Inc(P);
        StartP := P;
        while (P <= Length(S)) and (S[P] <> Attr.Quote) do
          Inc(P);
        Attr.Value := Copy(S, StartP, P - StartP);
        if P <= Length(S) then
          Inc(P);
      end
      else
      begin
        StartP := P;
        while (P <= Length(S)) and not (S[P] in [' ', #9, #10, #13, #12, '>']) do
          Inc(P);
        Attr.Value := Copy(S, StartP, P - StartP);
      end;
    end;
    Result := Concat(Result, [Attr]);
  end;
end;

function ParseComment(const S: String; var P: Integer): String;
var
  StartP: Integer;
begin
  Inc(P, 4); // skip '<!--'
  StartP := P;
  while (P <= Length(S)) and not SameTextAt(S, P, '-->') do
    Inc(P);
  Result := Copy(S, StartP, P - StartP);
  if P <= Length(S) then
    Inc(P, 3)
  else
    P := Length(S) + 1;
end;

function ParseDoctypeOrDecl(const S: String; var P: Integer): String;
var
  StartP: Integer;
begin
  Inc(P, 2); // skip '<!'
  StartP := P;
  while (P <= Length(S)) and (S[P] <> '>') do
    Inc(P);
  Result := Copy(S, StartP, P - StartP);
  if P <= Length(S) then
    Inc(P);
end;

{ ---- parsing ---- }

function ParseNodesInternal(const S: String; var P: Integer;
  const AStack: TStringArray): THTMLNodeArray; forward;

{ Called with S[P] = '<' followed by a name-start letter. AStack is the
  chain of lowercased ancestor tag names, used by ParseNodesInternal to
  decide whether a closing tag belongs to this element or to one further
  up (auto-closing this element without consuming the tag). }
function ParseElement(const S: String; var P: Integer;
  const AStack: TStringArray): THTMLNode;
var
  Node, TextNode: THTMLNode;
  LowerTag, Needle, RawContent: String;
  RawStart: Integer;
  NewStack: TStringArray;
begin
  Inc(P); // consume '<'
  Node := THTMLNode.Create;
  Node.Kind := nkElement;
  Node.TagName := ReadTagName(S, P);
  Node.Attributes := ParseAttributes(S, P);
  SkipWhitespace(S, P);
  Node.SelfClosing := (P <= Length(S)) and (S[P] = '/');
  if Node.SelfClosing then
    Inc(P);
  if (P <= Length(S)) and (S[P] = '>') then
    Inc(P);

  LowerTag := LowerCase(Node.TagName);

  if Node.SelfClosing or IsVoidElement(LowerTag) then
  begin
    // no children, no separate closing tag
  end
  else if IsRawTextElement(LowerTag) then
  begin
    RawStart := P;
    Needle := '</' + LowerTag;
    while (P <= Length(S)) and
      not (SameTextAt(S, P, Needle) and IsNameBoundary(S, P + Length(Needle))) do
      Inc(P);
    RawContent := Copy(S, RawStart, P - RawStart);
    if Length(RawContent) > 0 then
    begin
      TextNode := THTMLNode.Create;
      TextNode.Kind := nkText;
      TextNode.Text := RawContent;
      TextNode.RawText := True;
      Node.Children := [TextNode];
    end;
    if P <= Length(S) then
    begin
      Inc(P, 2); // skip '</'
      ReadTagName(S, P);
      SkipWhitespace(S, P);
      if (P <= Length(S)) and (S[P] = '>') then
        Inc(P);
    end;
  end
  else
  begin
    NewStack := Concat(AStack, [LowerTag]);
    Node.Children := ParseNodesInternal(S, P, NewStack);
  end;

  Result := Node;
end;

function ParseNodesInternal(const S: String; var P: Integer;
  const AStack: TStringArray): THTMLNodeArray;
var
  Result_: THTMLNodeArray;
  TextStart, Q: Integer;
  Node: THTMLNode;
  ClosingName, NewTagLower: String;

  procedure FlushText;
  begin
    if P > TextStart then
    begin
      Node := THTMLNode.Create;
      Node.Kind := nkText;
      Node.Text := Copy(S, TextStart, P - TextStart);
      Result_ := Concat(Result_, [Node]);
    end;
  end;

begin
  Result_ := [];
  TextStart := P;
  while P <= Length(S) do
  begin
    if S[P] <> '<' then
    begin
      Inc(P);
      Continue;
    end;

    FlushText;

    if SameTextAt(S, P, '<!--') then
    begin
      Node := THTMLNode.Create;
      Node.Kind := nkComment;
      Node.Text := ParseComment(S, P);
      Result_ := Concat(Result_, [Node]);
      TextStart := P;
      Continue;
    end;

    if SameTextAt(S, P, '</') then
    begin
      Q := P + 2;
      ClosingName := LowerCase(ReadTagName(S, Q));
      SkipWhitespace(S, Q);
      if (Q <= Length(S)) and (S[Q] = '>') then
        Inc(Q);
      if (ClosingName <> '') and StackContains(AStack, ClosingName) then
      begin
        if (Length(AStack) > 0) and (ClosingName = AStack[High(AStack)]) then
          P := Q; // our own closing tag: consume it
        // otherwise it belongs to an ancestor: leave P and bubble up
        Result := Result_;
        Exit;
      end
      else
      begin
        P := Q; // stray closing tag with no open ancestor: discard it
        TextStart := P;
        Continue;
      end;
    end;

    if SameTextAt(S, P, '<!') then
    begin
      Node := THTMLNode.Create;
      Node.Kind := nkDoctype;
      Node.Text := ParseDoctypeOrDecl(S, P);
      Result_ := Concat(Result_, [Node]);
      TextStart := P;
      Continue;
    end;

    if (P < Length(S)) and (S[P + 1] in ['A'..'Z', 'a'..'z']) then
    begin
      Q := P + 1;
      NewTagLower := LowerCase(ReadTagName(S, Q));
      if (Length(AStack) > 0) and ClosesOnSibling(AStack[High(AStack)], NewTagLower) then
      begin
        Result := Result_; // implicitly close self; let the parent level
        Exit;               // re-see this tag and open it as our sibling
      end;
      Node := ParseElement(S, P, AStack);
      Result_ := Concat(Result_, [Node]);
      TextStart := P;
      Continue;
    end;

    // stray '<' with no recognizable construct: keep it as literal text
    TextStart := P;
    Inc(P);
  end;
  FlushText;
  Result := Result_;
end;

function ParseHTML(const ASource: String): THTMLDocument;
var
  P: Integer;
  EmptyStack: TStringArray;
begin
  Result := THTMLDocument.Create;
  P := 1;
  EmptyStack := [];
  Result.Nodes := ParseNodesInternal(ASource, P, EmptyStack);
end;

{ ---- minification / serialization ---- }

function CollapseWhitespaceKeepEdges(const S: String): String;
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
  Result := SB;
end;

function SerializeAttributes(const AAttrs: THTMLAttributeList): String;
var
  I: Integer;
  Quote: Char;
begin
  Result := '';
  for I := 0 to High(AAttrs) do
  begin
    Result := Result + ' ' + AAttrs[I].Name;
    if AAttrs[I].HasValue then
    begin
      Quote := AAttrs[I].Quote;
      if Quote = #0 then
        Quote := '"';
      Result := Result + '=' + Quote + AAttrs[I].Value + Quote;
    end;
  end;
end;

function SerializeNodesMin(const ANodes: THTMLNodeArray; AInPre: Boolean): String; forward;

function SerializeNodeMin(const ANode: THTMLNode; AInPre: Boolean): String;
var
  LowerTag: String;
begin
  case ANode.Kind of
    nkText:
      if ANode.RawText or AInPre then
        Result := ANode.Text
      else
        Result := CollapseWhitespaceKeepEdges(ANode.Text);
    nkComment:
      Result := ''; // comments are dropped when minifying
    nkDoctype:
      Result := '<!' + CollapseWhitespaceKeepEdges(Trim(ANode.Text)) + '>';
  else // nkElement
    LowerTag := LowerCase(ANode.TagName);
    Result := '<' + LowerTag + SerializeAttributes(ANode.Attributes) + '>';
    if not (ANode.SelfClosing or IsVoidElement(LowerTag)) then
      Result := Result + SerializeNodesMin(ANode.Children, AInPre or (LowerTag = 'pre')) +
        '</' + LowerTag + '>';
  end;
end;

function SerializeNodesMin(const ANodes: THTMLNodeArray; AInPre: Boolean): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(ANodes) do
    Result := Result + SerializeNodeMin(ANodes[I], AInPre);
end;

function THTMLDocument.Minify: String;
begin
  Result := SerializeNodesMin(Nodes, False);
end;

function MinifyHTML(const ASource: String): String;
var
  Doc: THTMLDocument;
begin
  Doc := ParseHTML(ASource);
  try
    Result := Doc.Minify;
  finally
    Doc.Free;
  end;
end;

{ ---- lifetime ---- }

destructor THTMLNode.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(Children) do
    Children[I].Free;
  inherited Destroy;
end;

destructor THTMLDocument.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(Nodes) do
    Nodes[I].Free;
  inherited Destroy;
end;

end.
