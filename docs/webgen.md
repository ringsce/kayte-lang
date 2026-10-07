# webgen

`webgen.lpr` is a static site generator built on top of the Kayte toolchain
(it links the Kayte lexer/parser/VM so it doubles as a Kayte REPL). Given a
`content/` and `template/` directory, it renders a `public/` directory ready
to deploy: HTML pages, minified CSS, minified HTML, `sitemap.xml`, and
`robots.txt`.

## Building

Open `webgen.lpi` in Lazarus, or build from the command line:

```sh
lazbuild source/webgen.lpi
```

This produces a `webgen` executable in `bin/`.

## Running

Run `webgen` from a directory that contains a `content/` folder and a
`template/` folder:

```sh
cd mysite
webgen
```

To start an interactive Kayte REPL instead of generating a site:

```sh
webgen --repl
```

To convert the project into a WordPress theme + import file instead of a
static site:

```sh
webgen --wp
```

## Directory layout

```
mysite/
  content/    source pages: .md, .html, .css
  template/   .html templates (and optionally robots.txt)
  public/     generated output (created/overwritten by webgen)
```

On each run, `webgen` deletes any `.html`, `.xml`, `.txt`, and `.css` files
already in `public/` before regenerating it.

## Content files

Every file directly under `content/` is processed according to its
extension:

- **`.md`** — rendered as Markdown (Daring Fireball dialect, unsafe/raw HTML
  allowed) and treated like an `.html` page afterwards.
- **`.html`** — used as-is (aside from the header block below).
- **`.php`** — parsed with `source/PHPParser.pas` (a PHP 7.x/8.x tokenizer:
  it tells inline HTML apart from `<?php ?>`/`<?= ?>` code, correctly
  handling strings, heredoc/nowdoc, comments and PHP 8 syntax like
  attributes, `match`, nullsafe `?->`, and named/constructor-promoted
  arguments). The file is rendered through the page's template exactly
  like `.html` (front-matter header, `{%content%}`, etc.), then written to
  `public/<name>.php` — kept as `.php`, since it still needs a PHP runtime
  to execute. Minification only strips PHP comments and insignificant
  whitespace inside PHP code; the surrounding markup is left as rendered
  (see the CSS/HTML minifier caveat below for why). Malformed PHP (e.g.
  unbalanced braces) prints a warning but does not stop the build.
- **`.css`** — parsed and minified, then written straight to
  `public/<name>.css`.
- anything else — skipped with a warning.

### Front-matter headers

A `.md` or `.html` file may start with any number of header lines of the
form:

```
:key: value
```

These lines are stripped from the content and become template variables
(`{%key%}`) for that page. The special key `template` picks which file in
`template/` to render the page into (defaults to `default.html`); the
processed body itself is available to the template as `{%content%}`.

### `global.html`

A content file named `global.html` (case-insensitive) is not rendered as a
page. Its header block is parsed the same way, but the resulting key/value
pairs are made available to *every* page's template, in addition to that
page's own headers (which take precedence on conflicts).

## Templates

Templates use `fpTemplate` with `{% %}` delimiters and recursive expansion,
so a template value can itself contain further `{% %}` placeholders. Each
page is rendered with:

- `{%content%}` — the page's processed body
- every key from `global.html`'s header
- every key from the page's own header (overrides globals)

`{%root%}` is expected to resolve to the site's base URL — it is used when
building absolute page URLs for `sitemap.xml`.

The template chosen for a page is `template/<Template>` where `<Template>`
comes from the page's `:template:` header, or `template/default.html` if
that header is absent.

## Output post-processing

- **CSS** in `content/*.css` is minified with the CSS parser in
  `source/CSSParser.pas` before being written to `public/`.
- **Rendered HTML pages** are minified with the HTML parser in
  `source/HTMLParser.pas` (`MinifyHTML`) after template substitution, before
  being written to `public/`. Whitespace inside `<pre>`, `<script>`,
  `<style>`, and `<textarea>` is left untouched; everything else has
  insignificant whitespace collapsed and comments stripped. If minification
  fails for a page, a warning is printed and generation continues.
- **Rendered `.php` pages** are minified with `source/PHPParser.pas`
  (`MinifyPHP`) instead of `MinifyHTML`: `HTMLParser`'s tag-aware collapsing
  would mangle PHP sitting inside a tag or attribute (e.g.
  `<a href="<?php echo $url; ?>">`), so only PHP comments and insignificant
  whitespace *inside* `<?php ?>`/`<?= ?>` blocks are stripped; the
  surrounding markup is left exactly as rendered.

## sitemap.xml and robots.txt

- `public/sitemap.xml` is generated automatically, listing every page under
  `{%root%}<name>.html`.
- If `template/robots.txt` exists, it is rendered (with the global template
  variables available) to `public/robots.txt`.

## WordPress export (`--wp`)

`webgen --wp` reads the same `content/` + `template/` project as `webgen`
but, instead of a static `public/`, writes a `wordpress/` directory:

```
wordpress/
  theme/<theme-slug>/   an installable WordPress theme
    style.css            theme header (Theme Name/Author/...) + your CSS
    functions.php        theme supports (title-tag, thumbnails, custom-logo,
                          html5, editor styles, ...), "primary"/"footer" menus,
                          enqueues style.css + template JS, sets the imported
                          "index" page as the static front page
    <assets>             every non-page file under content/ and template/
                          (images, fonts, js/, ...) at the same relative path
    header.php / footer.php               split from template/default.html
    header-<slug>.php / footer-<slug>.php one pair per other :template: used
    page.php                              default WordPress Page template
    page-<slug>.php                       one per other :template: used,
                                           with a "Template Name:" doc block
                                           so it shows up in the WP editor
    <name>.php                            any hand-written content/*.php
                                           file, template-rendered and
                                           copied in as-is (see below)
  import.xml            a WXR (WordPress eXtended RSS) file
```

`.md`/`.html` pages become `<item>`s in `import.xml` (title, rendered body,
slug, and which `page-<slug>.php` template to use), ready for **Tools →
Import → WordPress** in `wp-admin`. `.php` content files are assumed to
already be WordPress-aware code (a snippet, a hand-written custom template,
...) rather than editorial content, so they are template-rendered the same
way `webgen` renders them for a static build and copied straight into the
theme instead of going into the WXR file.

Relative URLs are rewritten so they keep working under WordPress:
`<link>`s to `content/*.css` (bundled into `style.css`) and `<script src>`s
to local JS (enqueued from `functions.php`) are removed from the templates;
other asset URLs become `get_theme_file_uri(...)` in theme files, and links
to other pages (`about.html`) go through `get_permalink()`. Imported page
bodies can't run PHP, so there they become absolute URLs built from
`:root:` — set it to the live site URL (e.g. `https://example.com/`) before
exporting. After the WXR import, the `index` page is made the static front
page once; changing it later in **Settings → Reading** sticks.

webgen never executes PHP itself — everything under `theme/` is generated
text meant to run under an actual WordPress install. `header.php`/
`footer.php` are shared across every page, so any `{%...%}` placeholder in
`template/*.html` other than `{%content%}` and per-page ones used only
inside the (removed) `<title>` tag will only ever see `global.html`'s
values, never a page's own header — check the generated files if your
templates lean on page-specific placeholders outside of `{%content%}`.

## Example

```
content/
  global.html      # :root: https://example.com/
  index.md          # :title: Home
  about.html         # :title: About  \n  :template: page.html
  styles.css
template/
  default.html       # <html>...{%content%}...</html>
  page.html
  robots.txt
```

Running `webgen` in this directory produces `public/index.html`,
`public/about.html`, `public/styles.css`, `public/sitemap.xml`, and
`public/robots.txt`.
