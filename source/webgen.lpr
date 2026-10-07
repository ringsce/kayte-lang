program webgen;
{$mode objfpc}{$H+}

uses
  Classes, SysUtils, StrUtils, RegExpr, fpTemplate,
  MarkdownProcessor in '../external/MarkdownProcessor.pas',
  MarkdownDaringFireball in '../external/MarkdownDaringFireball.pas',
  MarkdownCommonMark in '../external/MarkdownCommonMark.pas',
  MarkdownUnicodeUtils in '../external/MarkdownUnicodeUtils.pas',
  MarkdownHTMLEntities in '../external/MarkdownHTMLEntities.pas',
  TokenDefs in 'TokenDefs.pas',
  AST in 'AST.pas',
  BytecodeTypes in 'BytecodeTypes.pas',
  Assembler in 'Assembler.pas',
  Lexer in 'Lexer.pas',
  Parser in 'Parser.pas',
  VirtualMachine in 'VirtualMachine.pas',
  CSSParser in 'CSSParser.pas',
  HTMLParser in 'HTMLParser.pas',
  PHPParser in 'PHPParser.pas';

const
  SiteMapHeader = '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
'<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">'  + LineEnding;

var
  DefsRe : TRegExpr;

function ExtractHeader(ARawContent: String; Defs : TStringList) : String;
var
  SList: TStringList;
begin
  SList := TStringList.Create;
  try
    SList.Text := ARawContent;
    while SList.Count > 0 do
    begin
      if not DefsRe.Exec(SList.Strings[0]) then
        Break;
      if Defs.IndexOfName(DefsRe.Match[1]) <> -1 then
          WriteLn('Warning : skipping duplicate header ' + DefsRe.Match[1]);
      Defs.AddPair(DefsRe.Match[1], DefsRe.Match[2]);
      SList.Delete(0);
    end;
    Result := SList.Text;
  finally
    FreeAndNil(SList);
  end;
end;

function XMLEncode(const AValue: String): String;
begin
  Result := AValue;
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&apos;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
end;

function Slugify(const AName: String): String;
var
  I: Integer;
  C: Char;
  Lower: String;
  LastDash: Boolean;
begin
  Result := '';
  Lower := LowerCase(AName);
  LastDash := True; // avoid a leading '-'
  for I := 1 to Length(Lower) do
  begin
    C := Lower[I];
    if C in ['a'..'z', '0'..'9'] then
    begin
      Result := Result + C;
      LastDash := False;
    end
    else if not LastDash then
    begin
      Result := Result + '-';
      LastDash := True;
    end;
  end;
  while (Length(Result) > 0) and (Result[Length(Result)] = '-') do
    Delete(Result, Length(Result), 1);
  if Result = '' then
    Result := 'page';
end;

function CData(const AValue: String): String;
begin
  Result := '<![CDATA[' + StringReplace(AValue, ']]>', ']]]]><![CDATA[>', [rfReplaceAll]) + ']]>';
end;

function RFC822Date(ADate: TDateTime): String;
const
  DayNames: array[1..7] of String = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  MonthNames: array[1..12] of String = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var
  Y, M, D, H, Mi, Se, MS: Word;
begin
  DecodeDate(ADate, Y, M, D);
  DecodeTime(ADate, H, Mi, Se, MS);
  Result := Format('%s, %.2d %s %.4d %.2d:%.2d:%.2d +0000',
    [DayNames[DayOfWeek(ADate)], D, MonthNames[M], Y, H, Mi, Se]);
end;

function MySQLDate(ADate: TDateTime): String;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', ADate);
end;

function ListFiles(APath: String): TStringArray;
var
  Info : TSearchRec;
begin
  Result := [];
  if FindFirst(APath, faAnyFile - faDirectory, Info) = 0 then
  repeat
    Result := Concat(Result, [String(Info.Name)]);
  until FindNext(info) <> 0;
  FindClose(Info);
end;

function LoadFromFile(AFileName: String) : String;
var
  FStream: TFileStream;
  SData: RawByteString = '';
begin
  FStream := TFileStream.Create(AFileName, fmOpenRead, fmShareDenyWrite);
  SetLength(SData, FStream.Size);
  if FStream.Size > 0 then
    FStream.Read(SData[1], FStream.Size);
  Result := UTF8String(SData);
  FreeAndNil(FStream);
end;

procedure SaveToFile(AFileName: String; const AData: String);
var
  FStream: TFileStream;
begin
  FStream := TFileStream.Create(AFileName, fmCreate);
  try
    if Length(AData) > 0 then
      FStream.Write(AData[1], Length(AData));
  finally
    FreeAndNil(FStream);
  end;
end;

procedure CopyBinaryFile(const ASource, ADest: String);
var
  Src, Dst: TFileStream;
begin
  ForceDirectories(ExtractFileDir(ADest));
  Src := TFileStream.Create(ASource, fmOpenRead or fmShareDenyWrite);
  try
    Dst := TFileStream.Create(ADest, fmCreate);
    try
      if Src.Size > 0 then
        Dst.CopyFrom(Src, Src.Size);
    finally
      FreeAndNil(Dst);
    end;
  finally
    FreeAndNil(Src);
  end;
end;

{ Every file under ARoot (recursively), as '/'-separated paths relative to
  ARoot, so they can be used directly as URL paths. }
procedure ListFilesRecursive(const ARoot, ARel: String; AList: TStrings);
var
  Info: TSearchRec;
  Dir, Rel: String;
begin
  Dir := ARoot;
  if ARel <> '' then
    Dir := Dir + DirectorySeparator + StringReplace(ARel, '/', DirectorySeparator, [rfReplaceAll]);
  if FindFirst(Dir + DirectorySeparator + '*', faAnyFile, Info) = 0 then
  repeat
    if (Info.Name = '.') or (Info.Name = '..') or (Copy(Info.Name, 1, 1) = '.') then
      Continue;
    if ARel = '' then
      Rel := Info.Name
    else
      Rel := ARel + '/' + Info.Name;
    if (Info.Attr and faDirectory) <> 0 then
      ListFilesRecursive(ARoot, Rel, AList)
    else
      AList.Add(Rel);
  until FindNext(Info) <> 0;
  FindClose(Info);
end;

{ True for URLs that must be left alone when mapping a static site onto
  WordPress: absolute/protocol-relative/root-relative URLs, anchors, any
  scheme (mailto:, data:, javascript:, ...) and values that are themselves
  PHP or unexpanded template code. }
function IsExternalURL(const AURL: String): Boolean;
var
  ColonPos, SlashPos: Integer;
begin
  Result := True;
  if (AURL = '') or (AURL[1] in ['#', '/', '?']) then
    Exit;
  if (Pos('<?', AURL) > 0) or (Pos('{%', AURL) > 0) then
    Exit;
  ColonPos := Pos(':', AURL);
  SlashPos := Pos('/', AURL);
  if (ColonPos > 0) and ((SlashPos = 0) or (ColonPos < SlashPos)) then
    Exit;
  Result := False;
end;

procedure Main;
var
  T : TTemplateParser;
  Processor : TMarkdownProcessor;
  FStream: TFileStream;
  Global, Content, Defs : TStringList;
  S, Ext, Name, FileName, Tpl, PHPErr : String;
  I, J : Integer;
begin
  try
    DefsRe := TRegExpr.Create('^:(\S+): \s*(.*)\s*$');
    Processor := TMarkdownProcessor.createDialect(mdDaringFireball);
    Processor.AllowUnsafe := True;
    Global := Nil;
    Content := TStringList.Create;
    Content.Sorted := True;
    Content.Duplicates := dupIgnore;

    for FileName in ListFiles('public' + DirectorySeparator + '*') do
    begin
      Ext := LowerCase(ExtractFileExt(FileName));
      if (Ext = '.html') or (Ext = '.xml') or (Ext = '.txt') or (Ext = '.css') or (Ext = '.php') then
        if not DeleteFile('public' + DirectorySeparator + FileName) then
          WriteLn('Warning : could not delte file ' + FileName);
    end;

    if not DirectoryExists('public') then
      CreateDir('public');

    for FileName in ListFiles('content' + DirectorySeparator + '*') do
    begin
        Ext := LowerCase(ExtractFileExt(FileName));
        case Ext of
          '.md', '.html', '.php' :
          begin
            S := LoadFromFile('content' + DirectorySeparator + FileName);
            if Length(S) = 0 then
            begin
              WriteLn('Warning : file empty ' + FileName);
              Continue;
            end;
            if Ext = '.php' then
              Name := FileName // keep the .php extension, it needs a PHP runtime
            else
              Name := ChangeFileExt(FileName, '.html');
            if Content.IndexOfName(Name) <> -1 then
            begin
              WriteLn('Warning : skipping duplicate ' + Name);
              Continue;
            end;
            Defs := TStringList.Create;
            Defs.Sorted := True;
            Defs.Duplicates := dupIgnore;
            S := ExtractHeader(S, Defs);
            if LowerCase(Name) = 'global.html' then
            begin
              if Assigned(Global) then
                WriteLn('Warning : global redefined');
              Global := Defs;
              Continue;
            end;
            if Ext = '.md' then
              S := Processor.Process(S)
            else if Ext = '.php' then
            begin
              if not ValidatePHP(S, PHPErr) then
                WriteLn('Warning : ' + FileName + ' has malformed PHP (' + PHPErr + ')');
            end;
            Content.AddPair(Name, S , Defs);
          end;
          '.css' :
          begin
            S := LoadFromFile('content' + DirectorySeparator + FileName);
            if Length(S) = 0 then
            begin
              WriteLn('Warning : file empty ' + FileName);
              Continue;
            end;
            try
              S := MinifyCSS(S);
            except
              on E: Exception do
              begin
                WriteLn('Warning : could not parse stylesheet ' + FileName + ' (' + E.Message + ')');
                Continue;
              end;
            end;
            SaveToFile('public' + DirectorySeparator + FileName, S);
            WriteLn('Processed ' + FileName);
          end;
        else
          WriteLn('Warning : skipping unknown file ' + FileName);
        end;
    end;

    if Content.Count = 0 then
    begin
      WriteLn('Error : content not found');
      Halt(1);
    end;

    FStream := TFileStream.Create('public' + DirectorySeparator + 'sitemap.xml', fmCreate);
    S := SiteMapHeader;
    FStream.Write(S[1], Length(S));

    T := TTemplateParser.Create;
    T.StartDelimiter := '{%';
    T.EndDelimiter := '%}';
    T.AllowTagParams := False;
    T.Recursive := True;
    for I := 0 to Content.Count - 1 do
    begin
      T.Clear;
      T.Values['content'] := Content.ValueFromIndex[I];
      Defs := TStringList(Content.Objects[I]);
      if Assigned(Global) then
      begin
        for J := 0 to Global.Count - 1 do
          T.Values[Global.Names[J]] := Global.ValueFromIndex[J];
      end;
      for J := 0 to Defs.Count - 1 do
        T.Values[Defs.Names[J]] := Defs.ValueFromIndex[J];

      Tpl := Defs.Values['template'];
      if Length(Tpl) = 0 then
        Tpl := 'default.html';

      Name := Content.Names[I];
      S := T.ParseString(LoadFromFile('template' + DirectorySeparator + Tpl));
      try
        // .php pages mix real HTML with live <?php ?> code: MinifyHTML's
        // tag-aware collapsing would mangle PHP sitting inside tags/attrs,
        // so only PHP code (comments, insignificant whitespace) is
        // stripped and the surrounding markup is left as rendered.
        if LowerCase(ExtractFileExt(Name)) = '.php' then
          S := MinifyPHP(S)
        else
          S := MinifyHTML(S);
      except
        on E: Exception do
          WriteLn('Warning : could not minify ' + Name + ' (' + E.Message + ')');
      end;
      SaveToFile('public' + DirectorySeparator + Name, S);
      WriteLn('Processed ' + Name);
      FreeAndNil(Defs);

      S := XMLEncode(T.ParseString('{%root%}' + Name));
      S := '<url><loc>' + S + '</loc></url>' + LineEnding;
      FStream.Write(S[1], Length(S));
    end;
    S := '</urlset>'  + LineEnding;
    FStream.Write(S[1], Length(S));
    WriteLn('Processed sitemap.xml');

    if FileExists('template' + DirectorySeparator + 'robots.txt') then
    begin
      T.Clear;
      if Assigned(Global) then
      begin
        for J := 0 to Global.Count - 1 do
          T.Values[Global.Names[J]] := Global.ValueFromIndex[J];
      end;
      T.ParseFiles('template' + DirectorySeparator + 'robots.txt', 'public' + DirectorySeparator + 'robots.txt');
      WriteLn('Processed robots.txt');
    end;
  finally
    FreeAndNil(FStream);
    FreeAndNil(Global);
    FreeAndNil(Content);
    FreeAndNil(DefsRe);
    FreeAndNil(T);
  end;
end;

{ Converts the same content/ + template/ project Main() would render to a
  static site into a WordPress theme plus a WXR (WordPress eXtended RSS)
  import file instead. webgen never runs PHP itself: the theme's PHP files
  are generated text, and the .md/.html pages become <item>s that
  WordPress's own Tools -> Import -> WordPress importer turns into Pages.
  Hand-written .php files under content/ are assumed to already be
  WordPress-flavoured (a snippet, a custom template, ...) and are template-
  rendered and copied into the theme as-is rather than exported to WXR.

  Static sites link to their assets and to each other with relative URLs
  (href="style.css", src="img/logo.png", href="about.html"). Those break
  under WordPress, where a page lives at /about/ and the theme's files live
  under /wp-content/themes/<slug>/, so every such URL is rewritten:
  - top-level content/*.css is bundled into style.css and enqueued, so
    its <link> tags are dropped;
  - local .js referenced from a template is enqueued by functions.php, so
    its <script> tags are dropped;
  - every other non-page file under content/ and template/ (images, fonts,
    other scripts/stylesheets, ...) is copied into the theme and pointed at
    with get_theme_file_uri() (or an absolute URL inside imported content,
    which can't run PHP);
  - links to other pages go through get_permalink() (or an absolute
    pretty-permalink URL inside imported content). }
procedure WPConvert;
const
  WPMarker = '@@WEBGEN_WP_CONTENT_MARKER@@';
  // generated by WPConvert itself, never overwritten by a copied asset
  ReservedThemeFiles: array[0..5] of String = ('style.css', 'functions.php',
    'index.php', 'page.php', 'header.php', 'footer.php');
  KindLink = 0;
  KindScript = 1;
  KindURL = 2;
var
  T: TTemplateParser;
  Processor: TMarkdownProcessor;
  Global, Content, Defs: TStringList;
  TemplatesSeen: TStringList; // template file name -> '' (set of distinct templates in use)
  Assets: TStringList;        // theme-relative path -> source file
  BundledCSS: TStringList;    // lower-cased content/*.css names bundled into style.css
  EnqueuedJS: TStringList;    // theme-relative .js path -> '1' (footer) / '0' (head)
  AllFiles: TStringList;
  S, S2, Ext, Name, FileName, Tpl, PHPErr, Rel, PHPFuncs, Title: String;
  ThemeDir, ThemeSlug, FuncPrefix, SiteName, Author, Description, RootURL, CSSBundle: String;
  Now_: TDateTime;
  I, J, PostID: Integer;
  ImportItems: String;
  InFooter: Boolean;

  function GlobalOrDefault(const AKey, ADefault: String): String;
  begin
    if Assigned(Global) and (Global.IndexOfName(AKey) <> -1) then
      Result := Global.Values[AKey]
    else
      Result := ADefault;
  end;

  function PHPQuote(const AValue: String): String;
  begin
    Result := '''' + StringReplace(StringReplace(AValue, '\', '\\', [rfReplaceAll]),
      '''', '\''', [rfReplaceAll]) + '''';
  end;

  { '' for the default template (WordPress' own header.php/footer.php),
    otherwise a filesystem/WP-safe slug used for header-<slug>.php,
    footer-<slug>.php and page-<slug>.php. }
  function TemplateSlug(const ATpl: String): String;
  begin
    if (ATpl = '') or (LowerCase(ATpl) = 'default.html') then
      Result := ''
    else
      Result := Slugify(ChangeFileExt(ATpl, ''));
  end;

  function PageSlug(const APageName: String): String;
  begin
    Result := Slugify(ChangeFileExt(APageName, ''));
  end;

  { Splits a URL into its path (without './' prefixes) and its ?query/#hash
    suffix. }
  procedure SplitURL(const AURL: String; out APath, ASuffix: String);
  var
    P, Q: Integer;
  begin
    P := Pos('?', AURL);
    Q := Pos('#', AURL);
    if (Q > 0) and ((P = 0) or (Q < P)) then
      P := Q;
    if P > 0 then
    begin
      APath := Copy(AURL, 1, P - 1);
      ASuffix := Copy(AURL, P, MaxInt);
    end
    else
    begin
      APath := AURL;
      ASuffix := '';
    end;
    while Copy(APath, 1, 2) = './' do
      Delete(APath, 1, 2);
  end;

  { Maps one relative URL from the static site onto WordPress. AInPHP picks
    between PHP (theme files) and plain absolute URLs (imported content,
    which WordPress stores as data and never executes). }
  function MapURL(const AURL: String; AInPHP: Boolean): String;
  var
    Path, Suffix, PageName, Slug, PathExt: String;
  begin
    Result := AURL;
    if IsExternalURL(AURL) then
      Exit;
    SplitURL(AURL, Path, Suffix);
    if Path = '' then
      Exit;
    PathExt := LowerCase(ExtractFileExt(Path));
    if (PathExt = '.html') or (PathExt = '.htm') or (PathExt = '.md') then
    begin
      PageName := ChangeFileExt(ExtractFileName(Path), '.html');
      if Content.IndexOfName(PageName) = -1 then
        Exit;
      Slug := PageSlug(PageName);
      if AInPHP then
        Result := '<?php echo esc_url(' + FuncPrefix + '_page_url(' + PHPQuote(Slug) + ')); ?>' + Suffix
      else if Slug = 'index' then
        Result := RootURL + Suffix
      else
        Result := RootURL + Slug + '/' + Suffix;
    end
    else if Assets.IndexOfName(Path) <> -1 then
    begin
      if AInPHP then
        Result := '<?php echo esc_url(get_theme_file_uri(' + PHPQuote(Path) + ')); ?>' + Suffix
      else
        Result := RootURL + 'wp-content/themes/' + ThemeSlug + '/' + Path + Suffix;
    end;
  end;

  function AttrValue(const ATag, AAttr: String): String;
  var
    Re: TRegExpr;
  begin
    Result := '';
    Re := TRegExpr.Create('\s' + AAttr + '\s*=\s*(["''])(.*?)\1');
    try
      Re.ModifierI := True;
      if Re.Exec(ATag) then
        Result := Re.Match[2];
    finally
      FreeAndNil(Re);
    end;
  end;

  { Runs APattern over AHTML and replaces each match according to AKind:
    KindLink drops <link>s to CSS bundled into style.css, KindScript drops
    <script src>s to local JS (recording it for functions.php to enqueue),
    KindURL rewrites href/src/poster/action attribute values. }
  function ReplaceMatches(const AHTML, APattern: String; AKind: Integer; AInPHP: Boolean): String;
  var
    Re: TRegExpr;
    Last: Integer;
    Tag, Path, Suffix, Replacement: String;
  begin
    Result := '';
    Last := 1;
    Re := TRegExpr.Create(APattern);
    try
      Re.ModifierI := True;
      Re.ModifierS := True;
      if Re.Exec(AHTML) then
      repeat
        Tag := Re.Match[0];
        Replacement := Tag;
        case AKind of
          KindLink:
            begin
              SplitURL(AttrValue(Tag, 'href'), Path, Suffix);
              if (Pos('/', Path) = 0) and (BundledCSS.IndexOf(LowerCase(Path)) <> -1) then
                Replacement := '';
            end;
          KindScript:
            begin
              SplitURL(AttrValue(Tag, 'src'), Path, Suffix);
              if (not IsExternalURL(Path)) and (LowerCase(ExtractFileExt(Path)) = '.js') and
                 (Assets.IndexOfName(Path) <> -1) then
              begin
                if EnqueuedJS.IndexOfName(Path) = -1 then
                  EnqueuedJS.Values[Path] := IfThen(InFooter, '1', '0');
                Replacement := '';
              end;
            end;
          KindURL:
            Replacement := Re.Match[1] + Re.Match[2] + MapURL(Re.Match[3], AInPHP) + Re.Match[2];
        end;
        Result := Result + Copy(AHTML, Last, Re.MatchPos[0] - Last) + Replacement;
        Last := Re.MatchPos[0] + Re.MatchLen[0];
      until not Re.ExecNext;
      Result := Result + Copy(AHTML, Last, MaxInt);
    finally
      FreeAndNil(Re);
    end;
  end;

  function RewriteURLs(const AHTML: String; AInPHP: Boolean): String;
  begin
    Result := ReplaceMatches(AHTML,
      '(\s(?:href|src|poster|action)\s*=\s*)(["''])(.*?)\2', KindURL, AInPHP);
  end;

  { Drops tags for assets functions.php now enqueues instead: <link> to
    bundled CSS and <script src> to local JS. }
  function StripEnqueuedTags(const AHTML: String): String;
  begin
    Result := ReplaceMatches(AHTML, '<link\b[^>]*>[ \t]*\r?\n?', KindLink, True);
    Result := ReplaceMatches(Result, '<script\b[^>]*\bsrc\s*=[^>]*>\s*</script>[ \t]*\r?\n?',
      KindScript, True);
  end;

  procedure SplitTemplate(const ATpl: String; out AHeader, AFooter: String);
  var
    Rendered: String;
    MarkerPos, K: Integer;
  begin
    T.Clear;
    if Assigned(Global) then
      for K := 0 to Global.Count - 1 do
        T.Values[Global.Names[K]] := Global.ValueFromIndex[K];
    T.Values['content'] := WPMarker;
    Rendered := T.ParseString(LoadFromFile('template' + DirectorySeparator + ATpl));
    MarkerPos := Pos(WPMarker, Rendered);
    if MarkerPos > 0 then
    begin
      AHeader := Copy(Rendered, 1, MarkerPos - 1);
      AFooter := Copy(Rendered, MarkerPos + Length(WPMarker), MaxInt);
    end
    else
    begin
      AHeader := Rendered;
      AFooter := '';
      WriteLn('Warning : template ' + ATpl +
        ' has no {%content%} marker; WordPress header/footer split may be incomplete');
    end;
  end;

  function InjectBeforeTag(const AHTML, ATag, ASnippet: String): String;
  var
    P: Integer;
  begin
    P := Pos(LowerCase(ATag), LowerCase(AHTML));
    if P = 0 then
      Result := AHTML + ASnippet
    else
      Result := Copy(AHTML, 1, P - 1) + ASnippet + Copy(AHTML, P, MaxInt);
  end;

  { Adds wp_body_open() right after <body ...>, and body_class() to the
    <body> tag itself when the template doesn't set its own class. }
  function InjectAfterOpenBody(const AHTML, ASnippet: String): String;
  var
    P, TagStart: Integer;
    BodyTag: String;
  begin
    P := Pos('<body', LowerCase(AHTML));
    if P = 0 then
    begin
      Result := ASnippet + AHTML;
      Exit;
    end;
    TagStart := P;
    while (P <= Length(AHTML)) and (AHTML[P] <> '>') do
      Inc(P);
    BodyTag := Copy(AHTML, TagStart, P - TagStart);
    Inc(P);
    if Pos('class', LowerCase(BodyTag)) = 0 then
      BodyTag := BodyTag + ' <?php body_class(); ?>';
    Result := Copy(AHTML, 1, TagStart - 1) + BodyTag + '>' + ASnippet + Copy(AHTML, P, MaxInt);
  end;

  { Templates fill <title> from the page's own :title: header, which isn't
    available once header.php is shared across every page; functions.php
    declares add_theme_support('title-tag') so WordPress injects the real
    <title> itself via wp_head(), so the template's static (now-empty) one
    is removed rather than left behind as a duplicate. }
  function StripTitleTag(const AHTML: String): String;
  var
    P1, P2: Integer;
    Lower: String;
  begin
    Result := AHTML;
    Lower := LowerCase(Result);
    P1 := Pos('<title', Lower);
    if P1 = 0 then
      Exit;
    P2 := Pos('</title>', Lower);
    if (P2 = 0) or (P2 < P1) then
      Exit;
    Inc(P2, Length('</title>'));
    Delete(Result, P1, P2 - P1);
  end;

  procedure WriteHeaderFooter(const ATpl: String);
  var
    Header, Footer, HName, FName, ASlug, BodyOpen: String;
  begin
    ASlug := TemplateSlug(ATpl);
    SplitTemplate(ATpl, Header, Footer);
    Header := StripTitleTag(Header);
    InFooter := False;
    Header := RewriteURLs(StripEnqueuedTags(Header), True);
    InFooter := True;
    Footer := RewriteURLs(StripEnqueuedTags(Footer), True);
    Header := InjectBeforeTag(Header, '</head>', '<?php wp_head(); ?>' + LineEnding);
    BodyOpen := LineEnding + '<?php wp_body_open(); ?>' + LineEnding;
    // templates with their own hard-coded <nav> keep it (links rewritten
    // above); otherwise the "primary" menu shows up once one is assigned
    if Pos('<nav', LowerCase(Header)) = 0 then
      BodyOpen := BodyOpen +
        '<?php if (has_nav_menu(''primary'')) wp_nav_menu(array(''theme_location'' => ''primary'', ''container'' => ''nav'', ''container_class'' => ''primary-menu'')); ?>' +
        LineEnding;
    Header := InjectAfterOpenBody(Header, BodyOpen);
    Footer := InjectBeforeTag(Footer, '</body>', '<?php wp_footer(); ?>' + LineEnding);
    if ASlug = '' then
    begin
      HName := 'header.php';
      FName := 'footer.php';
    end
    else
    begin
      HName := 'header-' + ASlug + '.php';
      FName := 'footer-' + ASlug + '.php';
    end;
    SaveToFile(ThemeDir + DirectorySeparator + HName, Header);
    SaveToFile(ThemeDir + DirectorySeparator + FName, Footer);
  end;

  function HeaderFooterCalls(const ASlug: String; out AFooterCall: String): String;
  begin
    if ASlug = '' then
    begin
      Result := 'get_header();';
      AFooterCall := 'get_footer();';
    end
    else
    begin
      Result := 'get_header(' + PHPQuote(ASlug) + ');';
      AFooterCall := 'get_footer(' + PHPQuote(ASlug) + ');';
    end;
  end;

  { Pages: the imported body already carries its own headings, so only the
    content is output, exactly like the static build. }
  function WPPageBody(const ASlug, ATemplateNameComment: String): String;
  var
    HeaderCall, FooterCall: String;
  begin
    HeaderCall := HeaderFooterCalls(ASlug, FooterCall);
    Result := '<?php' + LineEnding;
    if ATemplateNameComment <> '' then
      Result := Result + '/**' + LineEnding +
        ' * Template Name: ' + ATemplateNameComment + LineEnding +
        ' */' + LineEnding;
    Result := Result +
      HeaderCall + LineEnding +
      '?>' + LineEnding +
      '<main id="primary" class="site-main">' + LineEnding +
      '<?php while (have_posts()): the_post(); ?>' + LineEnding +
      '<?php the_content(); ?>' + LineEnding +
      '<?php endwhile; ?>' + LineEnding +
      '</main>' + LineEnding +
      '<?php ' + FooterCall + LineEnding;
  end;

  { Fallback for everything that isn't a Page: blog index, single posts,
    archives, search and 404. }
  function WPIndexBody: String;
  begin
    Result := '<?php' + LineEnding +
      'get_header();' + LineEnding +
      '?>' + LineEnding +
      '<main id="primary" class="site-main">' + LineEnding +
      '<?php if (have_posts()): ?>' + LineEnding +
      '<?php while (have_posts()): the_post(); ?>' + LineEnding +
      '<article id="post-<?php the_ID(); ?>" <?php post_class(); ?>>' + LineEnding +
      '<?php if (is_singular()): ?>' + LineEnding +
      '<?php the_title(''<h1 class="entry-title">'', ''</h1>''); ?>' + LineEnding +
      '<?php the_content(); ?>' + LineEnding +
      '<?php else: ?>' + LineEnding +
      '<?php the_title(''<h2 class="entry-title"><a href="'' . esc_url(get_permalink()) . ''">'', ''</a></h2>''); ?>' + LineEnding +
      '<?php the_excerpt(); ?>' + LineEnding +
      '<?php endif; ?>' + LineEnding +
      '</article>' + LineEnding +
      '<?php endwhile; ?>' + LineEnding +
      '<?php the_posts_navigation(); ?>' + LineEnding +
      '<?php else: ?>' + LineEnding +
      '<p><?php esc_html_e(''Nothing found.'', ' + PHPQuote(ThemeSlug) + '); ?></p>' + LineEnding +
      '<?php endif; ?>' + LineEnding +
      '</main>' + LineEnding +
      '<?php get_footer();' + LineEnding;
  end;

  function IsReservedThemeFile(const ARel: String): Boolean;
  var
    K: Integer;
  begin
    Result := False;
    for K := Low(ReservedThemeFiles) to High(ReservedThemeFiles) do
      if LowerCase(ARel) = ReservedThemeFiles[K] then
        Exit(True);
  end;

begin
  Now_ := Now;
  PostID := 1;
  ImportItems := '';
  InFooter := False;
  T := Nil;
  Global := Nil;
  Content := Nil;
  TemplatesSeen := Nil;
  Assets := Nil;
  BundledCSS := Nil;
  EnqueuedJS := Nil;
  AllFiles := Nil;
  try
    DefsRe := TRegExpr.Create('^:(\S+): \s*(.*)\s*$');
    Processor := TMarkdownProcessor.createDialect(mdDaringFireball);
    Processor.AllowUnsafe := True;
    Content := TStringList.Create;
    Content.Sorted := True;
    Content.Duplicates := dupIgnore;
    TemplatesSeen := TStringList.Create;
    TemplatesSeen.Sorted := True;
    TemplatesSeen.Duplicates := dupIgnore;
    Assets := TStringList.Create;
    Assets.Sorted := True;
    Assets.Duplicates := dupIgnore;
    BundledCSS := TStringList.Create;
    BundledCSS.Sorted := True;
    BundledCSS.Duplicates := dupIgnore;
    EnqueuedJS := TStringList.Create;
    AllFiles := TStringList.Create;

    for FileName in ListFiles('content' + DirectorySeparator + '*') do
    begin
      Ext := LowerCase(ExtractFileExt(FileName));
      if (Ext <> '.md') and (Ext <> '.html') and (Ext <> '.php') then
        Continue; // .css and other assets handled below
      S := LoadFromFile('content' + DirectorySeparator + FileName);
      if Length(S) = 0 then
      begin
        WriteLn('Warning : file empty ' + FileName);
        Continue;
      end;
      if Ext = '.php' then
        Name := FileName
      else
        Name := ChangeFileExt(FileName, '.html');
      if Content.IndexOfName(Name) <> -1 then
      begin
        WriteLn('Warning : skipping duplicate ' + Name);
        Continue;
      end;
      Defs := TStringList.Create;
      Defs.Sorted := True;
      Defs.Duplicates := dupIgnore;
      S := ExtractHeader(S, Defs);
      if LowerCase(Name) = 'global.html' then
      begin
        if Assigned(Global) then
          WriteLn('Warning : global redefined');
        Global := Defs;
        Continue;
      end;
      if Ext = '.md' then
        S := Processor.Process(S)
      else if Ext = '.php' then
      begin
        if not ValidatePHP(S, PHPErr) then
          WriteLn('Warning : ' + FileName + ' has malformed PHP (' + PHPErr + ')');
      end;
      Content.AddPair(Name, S, Defs);
    end;

    if Content.Count = 0 then
    begin
      WriteLn('Error : content not found');
      Halt(1);
    end;

    SiteName := GlobalOrDefault('site_name', 'Webgen Export');
    Author := GlobalOrDefault('author', 'admin'); // WordPress' importer needs a non-empty creator to map
    Description := GlobalOrDefault('description', 'Generated by webgen --wp.');
    RootURL := GlobalOrDefault('root', 'http://localhost/');
    if (RootURL = '') or (RootURL[Length(RootURL)] <> '/') then
      RootURL := RootURL + '/';
    ThemeSlug := Slugify(SiteName);
    // PHP identifiers can't contain '-' or start with a digit
    FuncPrefix := StringReplace(ThemeSlug, '-', '_', [rfReplaceAll]);
    if FuncPrefix[1] in ['0'..'9'] then
      FuncPrefix := 'theme_' + FuncPrefix;
    ThemeDir := 'wordpress' + DirectorySeparator + 'theme' + DirectorySeparator + ThemeSlug;
    ForceDirectories(ThemeDir);

    T := TTemplateParser.Create;
    T.StartDelimiter := '{%';
    T.EndDelimiter := '%}';
    T.AllowTagParams := False;
    T.Recursive := True;

    // CSS: minified and bundled straight into the theme's style.css, below
    // the required theme header block.
    CSSBundle := '';
    for FileName in ListFiles('content' + DirectorySeparator + '*.css') do
    begin
      BundledCSS.Add(LowerCase(FileName));
      S := LoadFromFile('content' + DirectorySeparator + FileName);
      if Length(S) = 0 then
        Continue;
      try
        CSSBundle := CSSBundle + MinifyCSS(S) + LineEnding;
      except
        on E: Exception do
          WriteLn('Warning : could not parse stylesheet ' + FileName + ' (' + E.Message + ')');
      end;
    end;

    // Assets: every other file under content/ (pages and bundled CSS aside)
    // and template/ (templates and robots.txt aside) is copied into the
    // theme at the same relative path, so relative url()s inside the
    // bundled style.css keep resolving. content/ wins over template/.
    ListFilesRecursive('content', '', AllFiles);
    for Rel in AllFiles do
    begin
      Ext := LowerCase(ExtractFileExt(Rel));
      if (Pos('/', Rel) = 0) and ((Ext = '.md') or (Ext = '.html') or (Ext = '.php') or (Ext = '.css')) then
        Continue;
      Assets.Values[Rel] := 'content' + DirectorySeparator + StringReplace(Rel, '/', DirectorySeparator, [rfReplaceAll]);
    end;
    AllFiles.Clear;
    ListFilesRecursive('template', '', AllFiles);
    for Rel in AllFiles do
    begin
      Ext := LowerCase(ExtractFileExt(Rel));
      if (Pos('/', Rel) = 0) and ((Ext = '.html') or (LowerCase(Rel) = 'robots.txt')) then
        Continue;
      if Assets.IndexOfName(Rel) = -1 then
        Assets.Values[Rel] := 'template' + DirectorySeparator + StringReplace(Rel, '/', DirectorySeparator, [rfReplaceAll]);
    end;
    for I := Assets.Count - 1 downto 0 do
    begin
      Rel := Assets.Names[I];
      if IsReservedThemeFile(Rel) then
      begin
        WriteLn('Warning : asset ' + Rel + ' clashes with a generated theme file, skipping');
        Assets.Delete(I);
        Continue;
      end;
      CopyBinaryFile(Assets.ValueFromIndex[I],
        ThemeDir + DirectorySeparator + StringReplace(Rel, '/', DirectorySeparator, [rfReplaceAll]));
      WriteLn('Copied asset ' + Rel);
    end;

    S := '/*' + LineEnding +
      'Theme Name: ' + SiteName + LineEnding +
      'Theme URI: ' + RootURL + LineEnding +
      'Author: ' + Author + LineEnding +
      'Description: ' + Description + LineEnding +
      'Version: ' + GlobalOrDefault('version', '1.0.0') + LineEnding +
      'Requires at least: 5.2' + LineEnding +
      'Requires PHP: 7.4' + LineEnding +
      'Text Domain: ' + ThemeSlug + LineEnding +
      '*/' + LineEnding + LineEnding + CSSBundle;
    SaveToFile(ThemeDir + DirectorySeparator + 'style.css', S);
    WriteLn('Processed style.css');

    // one header/footer + page template pair per distinct :template: header
    // used by a .md/.html page (plus the default one, always generated)
    TemplatesSeen.Add('default.html');
    for I := 0 to Content.Count - 1 do
    begin
      Name := Content.Names[I];
      if LowerCase(ExtractFileExt(Name)) = '.php' then
        Continue;
      Defs := TStringList(Content.Objects[I]);
      Tpl := Defs.Values['template'];
      if Length(Tpl) = 0 then
        Tpl := 'default.html';
      if TemplatesSeen.IndexOf(Tpl) = -1 then
        TemplatesSeen.Add(Tpl);
    end;

    for I := 0 to TemplatesSeen.Count - 1 do
    begin
      Tpl := TemplatesSeen[I];
      if not FileExists('template' + DirectorySeparator + Tpl) then
      begin
        WriteLn('Warning : template ' + Tpl + ' referenced but not found, skipping');
        Continue;
      end;
      WriteHeaderFooter(Tpl);
      if TemplateSlug(Tpl) = '' then
      begin
        SaveToFile(ThemeDir + DirectorySeparator + 'index.php', WPIndexBody);
        SaveToFile(ThemeDir + DirectorySeparator + 'page.php', WPPageBody('', ''));
        WriteLn('Processed index.php, page.php (from ' + Tpl + ')');
      end
      else
      begin
        SaveToFile(ThemeDir + DirectorySeparator + 'page-' + TemplateSlug(Tpl) + '.php',
          WPPageBody(TemplateSlug(Tpl), ChangeFileExt(Tpl, '')));
        WriteLn('Processed page-' + TemplateSlug(Tpl) + '.php (from ' + Tpl + ')');
      end;
    end;

    // functions.php comes last: it enqueues the JS the templates referenced
    S := '';
    for I := 0 to EnqueuedJS.Count - 1 do
      S := S + '    wp_enqueue_script(' + PHPQuote(ThemeSlug + '-' + Slugify(ChangeFileExt(EnqueuedJS.Names[I], ''))) +
        ', get_theme_file_uri(' + PHPQuote(EnqueuedJS.Names[I]) + '), array(), $ver, ' +
        IfThen(EnqueuedJS.ValueFromIndex[I] = '1', 'true', 'false') + ');' + LineEnding;
    PHPFuncs :=
      '<?php' + LineEnding +
      '/**' + LineEnding +
      ' * ' + SiteName + ' theme functions, generated by webgen --wp.' + LineEnding +
      ' */' + LineEnding + LineEnding +
      'if (!defined(''ABSPATH'')) {' + LineEnding +
      '    exit;' + LineEnding +
      '}' + LineEnding + LineEnding +
      'if (!isset($content_width)) {' + LineEnding +
      '    $content_width = ' + GlobalOrDefault('content_width', '1200') + ';' + LineEnding +
      '}' + LineEnding + LineEnding +
      'function ' + FuncPrefix + '_setup() {' + LineEnding +
      '    load_theme_textdomain(' + PHPQuote(ThemeSlug) + ', get_template_directory() . ''/languages'');' + LineEnding +
      '    add_theme_support(''title-tag'');' + LineEnding +
      '    add_theme_support(''automatic-feed-links'');' + LineEnding +
      '    add_theme_support(''post-thumbnails'');' + LineEnding +
      '    add_theme_support(''custom-logo'');' + LineEnding +
      '    add_theme_support(''responsive-embeds'');' + LineEnding +
      '    add_theme_support(''align-wide'');' + LineEnding +
      '    add_theme_support(''html5'', array(''search-form'', ''comment-form'', ''comment-list'', ''gallery'', ''caption'', ''style'', ''script''));' + LineEnding +
      '    add_theme_support(''editor-styles'');' + LineEnding +
      '    add_editor_style(''style.css'');' + LineEnding +
      '    register_nav_menus(array(' + LineEnding +
      '        ''primary'' => __(''Primary Menu'', ' + PHPQuote(ThemeSlug) + '),' + LineEnding +
      '        ''footer''  => __(''Footer Menu'', ' + PHPQuote(ThemeSlug) + '),' + LineEnding +
      '    ));' + LineEnding +
      '}' + LineEnding +
      'add_action(''after_setup_theme'', ''' + FuncPrefix + '_setup'');' + LineEnding + LineEnding +
      'function ' + FuncPrefix + '_assets() {' + LineEnding +
      '    // theme version + file time, so browsers and caches pick up every rebuild' + LineEnding +
      '    $ver = wp_get_theme()->get(''Version'') . ''.'' . filemtime(get_stylesheet_directory() . ''/style.css'');' + LineEnding +
      '    wp_enqueue_style(' + PHPQuote(ThemeSlug + '-style') + ', get_stylesheet_uri(), array(), $ver);' + LineEnding +
      S +
      '}' + LineEnding +
      'add_action(''wp_enqueue_scripts'', ''' + FuncPrefix + '_assets'');' + LineEnding + LineEnding +
      '// WordPress < 5.2' + LineEnding +
      'if (!function_exists(''wp_body_open'')) {' + LineEnding +
      '    function wp_body_open() {' + LineEnding +
      '        do_action(''wp_body_open'');' + LineEnding +
      '    }' + LineEnding +
      '}' + LineEnding + LineEnding +
      '// URL of an imported page by its slug; "index" is the front page.' + LineEnding +
      'function ' + FuncPrefix + '_page_url($slug) {' + LineEnding +
      '    if ($slug === ''index'') {' + LineEnding +
      '        return home_url(''/'');' + LineEnding +
      '    }' + LineEnding +
      '    $page = get_page_by_path($slug);' + LineEnding +
      '    return $page ? get_permalink($page) : home_url(''/'' . $slug . ''/'');' + LineEnding +
      '}' + LineEnding + LineEnding +
      '// Make the imported "index" page the static front page instead of the' + LineEnding +
      '// latest posts. Done once (on activation, after the WXR import, or on the' + LineEnding +
      '// next admin page load), so a later choice in Settings -> Reading sticks.' + LineEnding +
      'function ' + FuncPrefix + '_front_page() {' + LineEnding +
      '    if (get_option(''' + FuncPrefix + '_front_page_done'')) {' + LineEnding +
      '        return;' + LineEnding +
      '    }' + LineEnding +
      '    $front = get_page_by_path(''index'');' + LineEnding +
      '    if (!$front) {' + LineEnding +
      '        return;' + LineEnding +
      '    }' + LineEnding +
      '    update_option(''show_on_front'', ''page'');' + LineEnding +
      '    update_option(''page_on_front'', $front->ID);' + LineEnding +
      '    update_option(''' + FuncPrefix + '_front_page_done'', 1);' + LineEnding +
      '}' + LineEnding +
      'add_action(''after_switch_theme'', ''' + FuncPrefix + '_front_page'');' + LineEnding +
      'add_action(''import_end'', ''' + FuncPrefix + '_front_page'');' + LineEnding +
      'add_action(''admin_init'', ''' + FuncPrefix + '_front_page'');' + LineEnding + LineEnding +
      'function ' + FuncPrefix + '_switch_theme() {' + LineEnding +
      '    delete_option(''' + FuncPrefix + '_front_page_done'');' + LineEnding +
      '}' + LineEnding +
      'add_action(''switch_theme'', ''' + FuncPrefix + '_switch_theme'');' + LineEnding;
    SaveToFile(ThemeDir + DirectorySeparator + 'functions.php', PHPFuncs);
    WriteLn('Processed functions.php');

    // .md/.html pages -> WXR <item>s for WordPress' own importer.
    // hand-written .php pages are copied into the theme instead: they are
    // assumed to already be WordPress-aware code, not editorial content.
    for I := 0 to Content.Count - 1 do
    begin
      Name := Content.Names[I];
      Defs := TStringList(Content.Objects[I]);
      S := Content.ValueFromIndex[I];

      if LowerCase(ExtractFileExt(Name)) = '.php' then
      begin
        T.Clear;
        if Assigned(Global) then
          for J := 0 to Global.Count - 1 do
            T.Values[Global.Names[J]] := Global.ValueFromIndex[J];
        for J := 0 to Defs.Count - 1 do
          T.Values[Defs.Names[J]] := Defs.ValueFromIndex[J];
        T.Values['content'] := S;
        S := RewriteURLs(T.ParseString(S), True);
        SaveToFile(ThemeDir + DirectorySeparator + Name, S);
        WriteLn('Processed ' + Name + ' (copied into theme)');
        FreeAndNil(Defs);
        Continue;
      end;

      // page bodies may use {%root%} & co. just like templates do
      T.Clear;
      if Assigned(Global) then
        for J := 0 to Global.Count - 1 do
          T.Values[Global.Names[J]] := Global.ValueFromIndex[J];
      for J := 0 to Defs.Count - 1 do
        T.Values[Defs.Names[J]] := Defs.ValueFromIndex[J];
      S := RewriteURLs(T.ParseString(S), False);

      Tpl := Defs.Values['template'];
      if Length(Tpl) = 0 then
        Tpl := 'default.html';
      if TemplateSlug(Tpl) = '' then
        S2 := 'default'
      else
        S2 := 'page-' + TemplateSlug(Tpl) + '.php';

      Title := Defs.Values['title'];
      if Title = '' then
        Title := ChangeFileExt(Name, '');

      Inc(PostID);
      ImportItems := ImportItems +
        '<item>' + LineEnding +
        '<title>' + XMLEncode(Title) + '</title>' + LineEnding +
        '<link>' + XMLEncode(RootURL + PageSlug(Name) + '/') + '</link>' + LineEnding +
        '<pubDate>' + RFC822Date(Now_) + '</pubDate>' + LineEnding +
        '<dc:creator>' + CData(Author) + '</dc:creator>' + LineEnding +
        '<content:encoded>' + CData(S) + '</content:encoded>' + LineEnding +
        '<excerpt:encoded>' + CData('') + '</excerpt:encoded>' + LineEnding +
        '<wp:post_id>' + IntToStr(PostID) + '</wp:post_id>' + LineEnding +
        '<wp:post_date>' + CData(MySQLDate(Now_)) + '</wp:post_date>' + LineEnding +
        '<wp:post_date_gmt>' + CData(MySQLDate(Now_)) + '</wp:post_date_gmt>' + LineEnding +
        '<wp:comment_status>' + CData('closed') + '</wp:comment_status>' + LineEnding +
        '<wp:ping_status>' + CData('closed') + '</wp:ping_status>' + LineEnding +
        '<wp:post_name>' + CData(PageSlug(Name)) + '</wp:post_name>' + LineEnding +
        '<wp:status>' + CData('publish') + '</wp:status>' + LineEnding +
        '<wp:post_parent>0</wp:post_parent>' + LineEnding +
        '<wp:menu_order>0</wp:menu_order>' + LineEnding +
        '<wp:post_type>' + CData('page') + '</wp:post_type>' + LineEnding +
        '<wp:post_password>' + CData('') + '</wp:post_password>' + LineEnding +
        '<wp:is_sticky>0</wp:is_sticky>' + LineEnding +
        '<wp:postmeta>' + LineEnding +
        '<wp:meta_key>' + CData('_wp_page_template') + '</wp:meta_key>' + LineEnding +
        '<wp:meta_value>' + CData(S2) + '</wp:meta_value>' + LineEnding +
        '</wp:postmeta>' + LineEnding +
        '</item>' + LineEnding;
      WriteLn('Queued ' + Name + ' for WordPress import');
      FreeAndNil(Defs);
    end;

    S := '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
      '<rss version="2.0"' + LineEnding +
      '  xmlns:excerpt="http://wordpress.org/export/1.2/excerpt/"' + LineEnding +
      '  xmlns:content="http://purl.org/rss/1.0/modules/content/"' + LineEnding +
      '  xmlns:wfw="http://wellformedweb.org/CommentAPI/"' + LineEnding +
      '  xmlns:dc="http://purl.org/dc/elements/1.1/"' + LineEnding +
      '  xmlns:wp="http://wordpress.org/export/1.2/">' + LineEnding +
      '<channel>' + LineEnding +
      '<title>' + XMLEncode(SiteName) + '</title>' + LineEnding +
      '<link>' + XMLEncode(RootURL) + '</link>' + LineEnding +
      '<description>' + XMLEncode(Description) + '</description>' + LineEnding +
      '<pubDate>' + RFC822Date(Now_) + '</pubDate>' + LineEnding +
      '<language>en-US</language>' + LineEnding +
      '<wp:wxr_version>1.2</wp:wxr_version>' + LineEnding +
      '<wp:base_site_url>' + CData(RootURL) + '</wp:base_site_url>' + LineEnding +
      '<wp:base_blog_url>' + CData(RootURL) + '</wp:base_blog_url>' + LineEnding +
      ImportItems +
      '</channel>' + LineEnding +
      '</rss>' + LineEnding;
    SaveToFile('wordpress' + DirectorySeparator + 'import.xml', S);
    WriteLn('Processed wordpress' + DirectorySeparator + 'import.xml');

    WriteLn;
    WriteLn('WordPress export ready in ''wordpress''.');
    WriteLn('  1. Copy wordpress/theme/' + ThemeSlug + ' into wp-content/themes/ and activate it.');
    WriteLn('  2. Tools -> Import -> WordPress in wp-admin, import wordpress/import.xml.');
    WriteLn('     The imported "index" page becomes the static front page automatically.');
    WriteLn('  3. Imported pages link to assets under ' + RootURL + 'wp-content/themes/' + ThemeSlug +
      '/ -- set :root: in global.html to the live site URL before exporting.');
  finally
    FreeAndNil(Content);
    FreeAndNil(Global);
    FreeAndNil(TemplatesSeen);
    FreeAndNil(Assets);
    FreeAndNil(BundledCSS);
    FreeAndNil(EnqueuedJS);
    FreeAndNil(AllFiles);
    FreeAndNil(DefsRe);
    FreeAndNil(T);
  end;
end;

(* Procedure to Start REPL *)
procedure StartREPL;
var
  Input: string;
  SourceCode: TStringList;
  LexerInstance: TLexer;
  ParserInstance: TParser;
  BytecodeProgram: TByteCodeProgram;
  VM: TVirtualMachine;
  LineNumber: Integer;
begin
  Writeln('Webgen REPL v0.9.10');
  Writeln('Type "exit" or "quit" to leave, "help" for help');
  Writeln('===============================================');
  Writeln;

  LineNumber := 1;

  while True do
  begin
    Write('webgen[', LineNumber, ']> ');
    ReadLn(Input);

    // Trim whitespace
    Input := Trim(Input);

    // Check for exit commands
    if (Input = 'exit') or (Input = 'quit') then
    begin
      Writeln('Goodbye!');
      Break;
    end;

    // Check for help
    if Input = 'help' then
    begin
      Writeln('REPL Commands:');
      Writeln('  exit, quit - Exit the REPL');
      Writeln('  help       - Show this help message');
      Writeln('  clear      - Clear the screen');
      Writeln;
      Writeln('Enter Kayte code to execute it immediately.');
      Continue;
    end;

    // Check for clear command
    if Input = 'clear' then
    begin
      {$IFDEF WINDOWS}
      Writeln; // Simple approach for Windows
      {$ELSE}
      // For Unix-like systems
      Write(#27'[2J'#27'[H');
      {$ENDIF}
      Continue;
    end;

    // Skip empty lines
    if Input = '' then
    begin
      Inc(LineNumber);
      Continue;
    end;

    // Try to compile and execute the input
    SourceCode := TStringList.Create;
    try
      SourceCode.Add(Input);

      try
        // Create lexer and parser
        LexerInstance := TLexer.Create(SourceCode);
        try
          ParserInstance := TParser.Create(LexerInstance);
          try
            // Parse the input
            BytecodeProgram := ParserInstance.Parse;
            try
              // Execute the bytecode
              VM := TVirtualMachine.Create(BytecodeProgram);
              try
                VM.Run;
              finally
                VM.Free;
              end;
            finally
              BytecodeProgram.Free;
            end;
          finally
            ParserInstance.Free;
          end;
        finally
          LexerInstance.Free;
        end;

      except
        on E: Exception do
        begin
          Writeln('Error: ', E.Message);
        end;
      end;

    finally
      SourceCode.Free;
    end;

    Inc(LineNumber);
  end;
end;


begin
  // Display banner
  Writeln('Webgen Runtime Environment');
  Writeln('===================================');
  Writeln;

  if (ParamCount > 0) and (ParamStr(1) = '--repl') then
    StartREPL
  else if (ParamCount > 0) and (ParamStr(1) = '--wp') then
    WPConvert
  else
    Main;
end.

