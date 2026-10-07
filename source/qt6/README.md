# Qt6 GUI support

Kayte scripts can create native Qt6 windows with the `QT` statement.

```vb
QT "init"
QT "window", "Hello", 300, 120 TO win
QT "button", win, "Close me", 20, 40 TO btn
QT "show", win
QT "wait" TO ev            ' blocks until btn is clicked (or window closed)
```

See `examples/qt6_hello.kayte` (widgets positioned by pixel),
`examples/qt6_widgets.kayte` (an order form using most widgets, in layouts),
`examples/qt6_layouts.kayte` (a resizable contact book built with layouts),
`examples/qt6_menus_tabs.kayte` (a notes app with menus and tabs) and
`examples/qt6_callbacks.kayte` (a to-do list using SUB event handlers) for
complete programs. For UIs written in QML, see [QML](#qml) below.

## How the binding works

Qt is a C++ library, so unlike SDL (`source/kayte_sdl2.pas`) it can't be
bound with `cdecl; external` declarations. There are three layers:

| Layer | File | Role |
|---|---|---|
| C ABI shim | `source/qt6/kayte_qt6.cpp` | `extern "C"` functions (`kqt_*`) over QtWidgets and Qt Quick (QML), built into `libkayte_qt6` |
| Pascal binding | `source/kayte_qt6.pas` | Loads the shim with `dynlibs` on the first `QT` statement and forwards commands to `kqt_call()` |
| Native runtime | `source/native/kayte_native_rt.c` | Same, for `--native` executables (via `dlopen`) |
| Language | `Lexer.pas`, `Parser.pas`, `VirtualMachine.pas` | `QT` and `QML` keywords → `BC_QT` opcode (Operand3 says which) → `QtCall`; `on`/`run`/`event` are handled in the VM itself, since they call SUBs |

The shim is loaded at runtime, not linked, so `kayte` starts and runs
non-Qt scripts on machines without Qt installed.

## Native executables

QT programs also compile with `kayte --native app.kayte -o app` (see the
main README). The native runtime calls the same `kqt_call()` entry point
as the VM, so behavior is identical. The executable looks for
`libkayte_qt6` in `$KAYTE_QT6_LIB`, next to itself, on the system library
path, and finally at the path kayte loaded it from when compiling - so
ship the library next to the executable when distributing it.

## iOS (and tvOS)

QT and QML programs build into iOS apps with `scripts/build-kayte-ios.sh`.
The bytecode VM doesn't run on iOS, but `--native` output does:

1. `kayte --native app.kayte -o app.c` translates the program to C (an
   output ending in `.c` writes only the C).
2. `source/ios/CMakeLists.txt` compiles that C, the native runtime and this
   shim against Qt for iOS with Xcode. iOS apps can't load libraries from
   outside their bundle, so the shim is linked in (`KAYTE_QT_STATIC`)
   instead of loaded with `dlopen`.
3. Files passed with `--resource` are bundled at the same relative path.
   On iOS the program starts in the bundle's directory, so relative paths
   in the script keep working.

```sh
pip install aqtinstall                      # or: brew install aqtinstall
aqt install-qt mac ios 6.11.1 -O ~/Qt       # same version as your desktop Qt
scripts/build-qt-ios-simulator.sh 6.11.1    # once, for the Simulator on Apple Silicon (see below)
scripts/build-kayte-ios.sh examples/qml_todo.kayte --resource examples/qml            # Simulator
scripts/build-kayte-ios.sh examples/qml_todo.kayte --resource examples/qml --run      # ...and launch it
scripts/build-kayte-ios.sh examples/qml_todo.kayte --resource examples/qml --device --team ABCDE12345
```

The app lands in `build/ios/<name>-<sdk>.app`. See `--help` for the app
name, bundle id and other options.

Notes:

- Qt's prebuilt iOS libraries have arm64 only for devices. For the
  Simulator they are x86_64, and current Simulators on Apple Silicon
  (iOS 26+) refuse x86_64 apps, even with Rosetta installed.
  `scripts/build-qt-ios-simulator.sh` builds qtbase, qtshadertools and
  qtdeclarative for the arm64 Simulator into
  `~/Qt/<version>/ios-simulator-arm64` (about 30-60 minutes, once).
  `build-kayte-ios.sh` then uses that build for Simulator apps
  automatically, and the prebuilt Qt for device apps.
- QML modules are linked in statically. The script links the ones that
  the bundled `.qml` files import, plus QtQuick, Controls, Layouts and
  Window for inline QML (`QML "source"`).
- iOS has no windows to close, so `run` / `wait` only return if the
  script closes its windows itself. The app ends when the user leaves it.
- `PROCESS` stops with a runtime error, since iOS apps can't start other
  programs.
- **tvOS**: Qt 6 has no tvOS port, so QT/QML programs can't run there.
  The native runtime itself compiles for tvOS (`xcrun --sdk appletvos
  clang -target arm64-apple-tvos17.0 -I source/native app.c -framework
  CoreFoundation`). On tvOS, QT/QML statements stop with a clear runtime
  error.

## Building

```sh
brew install qt cmake          # macOS  (Debian: apt install qt6-base-dev qt6-declarative-dev cmake g++)
scripts/build-kayte-qt6.sh     # builds build/qt6/libkayte_qt6.*, copies it next to kayte
```

QML support is built in when CMake finds Qt Quick (`qt6-declarative-dev`
on Debian; Homebrew's `qt` includes it). The configure step prints
`kayte_qt6: QML support enabled` or `disabled`. Without it the `qml*`
commands (and `QML "load"` etc.) stop with a message saying how to get it. Pass
`-DKAYTE_QT6_QML=OFF` to CMake to leave it out on purpose.

`kayte` looks for the library in this order: `$KAYTE_QT6_LIB`, the
directory containing the `kayte` executable, then the system library path.

## Statement syntax

```
QT <command> [, <arg>]* [TO <variable>]
```

Same shape as `PROCESS`. Widgets and timers are identified by integer
handles; positions and sizes are in pixels.

### Application and events

There are two ways to react to events.

**Handlers** (recommended): attach a `SUB` to a widget, menu item or
timer with `on`, then hand control to `run`. See
`examples/qt6_callbacks.kayte`.

```vb
QT "on", saveBtn, "SaveFile"
QT "on", quitAct, "QuitApp"
QT "run"                     ' returns once every window is closed

SUB SaveFile()
  QT "event" TO src          ' which handle fired (one SUB can serve several)
  ...
END SUB
```

**Polling**: call `wait` (or `poll`) yourself in a `WHILE` loop and
compare the returned handle - see `examples/qt6_hello.kayte`.

| Command | Arguments | Result |
|---|---|---|
| `init` | – | 1 (must run first) |
| `on` | handle, "SubName" | 1 – run that SUB (no parameters) on the handle's events; `""` removes it |
| `run` | – | event loop calling `on` handlers; returns 0 when all windows are closed. Events without a handler are ignored |
| `event` | – | inside a handler: the handle that fired |
| `wait` | – | id of the next event's widget/timer (blocks), or 0 when all windows are closed |
| `poll` | – | like `wait` but never blocks: -1 when nothing happened |
| `exec` | – | runs until all windows are closed, ignoring events |

What queues an event: button click, checkbox toggle, Enter in an `edit`,
combo/list selection change, spin/slider value change, menu item chosen,
tab switch, timer tick. Changes
the script makes itself (`setvalue`, `additem`, ...) don't. Repeats of the
same event that pile up (slider drags, fast timers) are merged into one.

### Widgets

Arguments in `[brackets]` are optional. Positions and sizes are in
pixels; leave them out for widgets that go into a layout (below), which
then manages their geometry. A width/height of 0 means natural size.

| Command | Arguments | Notes |
|---|---|---|
| `window` | title, width, height | top-level window |
| `label` | parent, text [, x, y] | |
| `button` | parent, text [, x, y] | event on click |
| `checkbox` | parent, text [, x, y] | event on toggle; value 0/1 |
| `edit` | parent, text [, x, y, width] | single line; event on Enter |
| `textedit` | parent [, x, y, width, height] | multi-line; no events |
| `combo` | parent [, x, y, width] | drop-down; value = selected index |
| `list` | parent [, x, y, width, height] | value = selected row (-1 none) |
| `spin` | parent, min, max [, x, y] | integer spin box |
| `slider` | parent, min, max [, x, y, width] | horizontal |
| `progress` | parent [, x, y, width] | value 0–100 |
| `group` | parent, title [, x, y] | titled frame, a container for other widgets |
| `panel` | parent [, x, y, width, height] | plain container |
| `timer` | interval ms | repeating; its id arrives via `wait`/`poll` |

Each returns the new handle.

### Layouts

Layouts place and resize widgets automatically, so windows can be resized
and everything reflows. See `examples/qt6_layouts.kayte`.

```vb
QT "vbox", win TO main          ' install a vertical layout on the window
QT "button", win, "OK" TO ok    ' no x, y needed
QT "hbox" TO row                ' standalone layout, to nest
QT "stretch", row               ' push what follows to the right
QT "add", row, ok
QT "add", main, row             ' nest the row inside main
```

| Command | Arguments | Notes |
|---|---|---|
| `vbox` / `hbox` | [parent] | stack items vertically / horizontally |
| `grid` | [parent] | rows and columns |
| `form` | [parent] | two-column "label: field" rows |
| `add` | layout, item [, stretch] | box: item is a widget or layout; stretch > 0 shares extra space |
| `add` | grid, item, row, col [, rowspan, colspan] | grid: row/col required |
| `add` | form, item | form: item spans the whole row |
| `addrow` | form, label, item | form: labelled row |
| `stretch` | box [, factor] | expanding empty space |
| `colstretch` / `rowstretch` | grid, index, factor | how a grid shares extra width/height (default: evenly) |
| `spacing` | layout, px | gap between items |
| `margins` | layout, px | padding around the edges |

With a parent (window, `group` or `panel`), the layout is installed on it;
each container holds one layout. Without a parent it's standalone and must
be nested into another with `add`. Adding a widget to a layout moves it
into that layout's container, so the parent it was created with only
matters until then.

### Menus and tabs

See `examples/qt6_menus_tabs.kayte`.

```vb
QT "menu", win, "File" TO fileMenu
QT "action", fileMenu, "Save", "Ctrl+S" TO saveAct
QT "tabs", win TO tabs
QT "tab", tabs, "General" TO page   ' a container: give it a layout
QT "wait" TO ev                     ' = saveAct when Save is chosen
```

| Command | Arguments | Notes |
|---|---|---|
| `menu` | window or menu, title | menu bar menu (bar created on first use), or a submenu |
| `action` | menu, text [, shortcut [, checkable]] | menu item; event when chosen. Shortcut like `"Ctrl+S"` (Cmd on macOS); checkable 1 adds a check mark, value 0/1 |
| `separator` | menu | divider line |
| `tabs` | parent [, x, y, width, height] | tab container; event on tab switch; value = current tab index |
| `tab` | tabs, title | adds a page and returns it (a container, like `panel`) |

`settext`/`gettext` rename menus, menu items and tab pages (the tab's
title); `enable`/`disable` work on menu items.

On macOS the menu bar is native, at the top of the screen. Elsewhere it's
drawn inside the window: with a layout on the window it gets its own space
automatically; without one it covers the top ~25 px, so start
pixel-positioned widgets below that.

### Working with widgets

| Command | Arguments | Result |
|---|---|---|
| `settext` | widget, text | 1 – sets a window's title |
| `gettext` | widget | text; combo/list give the selected item's text |
| `getvalue` | widget | checkbox / checkable menu item 0/1, combo/list/tabs index, spin/slider/progress value |
| `setvalue` | widget, number | 1 |
| `additem` | combo or list, text | 1 |
| `removeitem` | combo or list, index | 1 |
| `clear` | combo, list, edit or textedit | 1 |
| `style` | widget, css | 1 – Qt style sheet, e.g. `"color: red; font-size: 18px"` |
| `enable` / `disable` | widget or timer | 1 – timers start/stop |
| `show` / `hide` | widget | 1 |
| `close` | window or timer | 1 – timers stop |
| `geometry` | widget, x, y, width, height | 1 |

### QML

A UI can be written in QML (Qt Quick) instead of built widget by widget,
using the `QML` statement. The `.qml` file draws the UI and the Kayte
script holds the logic. The script finds QML objects by `objectName`,
reads and writes their properties, calls their functions, and handles
their signals with `on`/`run` (or `wait`). See `examples/qml_todo.kayte`
with `examples/qml/todo.qml`, and `examples/qml_hello.kayte` (inline QML).

```
QML <command> [, <arg>]* [TO <variable>]
```

`QML` has the same shape and runtime as `QT` (it compiles to the same
`BC_QT` instruction, marked as QML in Operand3), with two differences:

- It starts Qt by itself, so no `init` is needed.
- It has the QML command names below. Every `QT` command also works
  under `QML` (`show`, `close`, `on`, `run`, `wait`, `event`, `message`,
  ...), so a QML program never needs a `QT` line. The two statements share
  handles and can be mixed.

```qml
// ui.qml
import QtQuick
import QtQuick.Controls
ApplicationWindow {
    width: 300; height: 120
    property string status: ""
    signal picked(int index)
    function double(x) { return x * 2 }
    Button { objectName: "ok"; text: "OK" }
}
```

```vb
QML "load", "ui.qml" TO win           ' path relative to the current directory
QML "find", win, "ok" TO okBtn
QML "connect", okBtn, "clicked" TO okClicked
QML "on", okClicked, "OkPressed"
QML "show", win
QML "run"

SUB OkPressed()
  QML "call", win, "double", 21 TO n        ' 42
  QML "set", win, "status", "got " & n
END SUB
```

| `QML` command | `QT` equivalent | Arguments | Result |
|---|---|---|---|
| `load` | `qml` | path or URL | handle of the window. Relative paths are resolved against the current directory; `qrc:/...` and other URLs work too |
| `source` | `qmlsource` | QML text | same, from source text (relative imports resolve against the current directory) |
| `widget` | `qmlwidget` | parent, path [, x, y, width, height] | a QML scene inside a widget window, so it can go into a layout. The root must be an Item, and it is resized to fill the widget |
| `import` | `qmlimport` | directory | 1. Adds a directory to search for QML modules (`import MyModule`) |
| `find` | `find` | handle, objectName | the named object inside it (searches the object tree and Qt Quick's visual tree). `""` gives a `widget`'s root item, or the handle itself |
| `connect` | `connect` | handle, signal | a **new handle** that receives that signal's events, for `on` / `wait`. Signal is a name (`"clicked"`, `"textChanged"`, a QML `signal`) or a signature (`"valueChanged(int)"`) |
| `arg` | `eventarg` | connect-handle, index | argument `index` (from 0) of the signal's latest emission |
| `get` | `getprop` | handle, name | property value |
| `set` | `setprop` | handle, name, value | 1. The value is converted to the property's type (`"#ff0000"` to a color, `1` to `true`, ...) |
| `call` | `call` | handle, name [, arg]* | calls a QML `function`, slot or invokable method (up to 8 arguments) and returns its result (0 if none) |

How values come back: integers, bools (0/1), enums and whole-number reals
are numbers. Objects come back as handles (0 for null). Everything else is
text, including reals with a fraction (`"2.5"`), colors (`"#ff0000"`), urls,
and lists (one item per line).

Notes:

- The root of a `load`ed file can be a `Window` / `ApplicationWindow`, or
  any `Item` (which then gets a window of its own that it fills). Windows
  start hidden, like `window`, unless the QML sets `visible: true`.
  `show`, `hide`, `close` and `settext`/`gettext` (title) work on them.
- A window ends `run` / `wait` when it closes, like a widget window.
  `Qt.quit()` in QML closes every window.
- Each `connect` gives a separate handle, so one object can feed several
  SUBs (e.g. `clicked` and `pressAndHold`). `connect`, `find`, `get`, `set`
  and `call` also work on widgets and their Qt signals.
- As with widgets, changes the script makes (`set`, `call`, `settext`,
  ...) don't come back as events. Bindings in the QML still update.
- QML handlers like `onClicked: ...` still run. They don't stop Kayte from
  connecting to the same signal.

### Forms from files (.kfm, Designer .ui)

A window can be described in a file and loaded in one statement, the
way Qt loads a Designer `.ui` file. The file holds the widgets and names
the handler SUBs, and the script holds the logic. See
`examples/kfm_login.kayte` (loads `examples/form1.kfm`) and
`examples/ui.kayte` (`examples/ui.kfm`).

```
// login.kfm
form LoginWindow {
  title: "User Login"
  width: 400
  height: 250
  layout: VBox {
    textfield { id: "user", placeholder: "Username" }
    textfield { id: "pass", placeholder: "Password", type: Password }
    button { text: "Log In", onclick: handleLogin() }
  }
}
```

```vb
QT "init"
QT "loadform", "login.kfm" TO win      ' path relative to the current directory
QT "find", win, "user" TO userBox      ' widgets are found by id
QT "show", win
QT "run"

SUB handleLogin()                      ' attached by loadform, as with QT "on"
  QT "gettext", userBox TO who
  QT "message", "Hello", who
END SUB
```

| Command | Arguments | Result |
|---|---|---|
| `loadform` | path | handle of the (hidden) window built from a `.kfm` or `.ui` file. The SUBs the file names are attached to their widgets' events. A missing handler SUB, an unknown widget or property, or a syntax error stops the script with the file and line |

**Declarative .kfm**: `form Name { ... }` (or `window` / `dialog`) at the
top. A block is `type [name] { key: value ... }`, and blocks nest. Values
are `"text"`, numbers, `true`/`false`, words (`Center`), lists
(`["A", "B"]`) and handler calls (`DoIt()`). `//` and `/* */` are comments,
and `,` / `;` between entries are optional.

- **Window** (`form`): `title`, `width`, `height`, `style`, ... Its
  content is either `layout: VBox { ... }` (or `HBox`, `Grid`, `Form`), a
  `content { ... }` block, or widgets listed directly (stacked
  vertically). `menu "File" { action { text, shortcut, checkable,
  onclick } separator {} menu "Sub" {...} }` blocks add a menu bar.
- **Layouts**: `VBox`/`vbox`/`column`, `HBox`/`hbox`/`row`, `Grid`
  (`columns: N`, children fill row by row, or give `row` / `col` /
  `rowspan` / `colspan`), `Form` (children's `label: "Name:"` makes
  labelled rows). They take `spacing` and `margins`, and nest as children
  (`hbox { ... }`). `stretch {}` adds expanding space in a box, and a
  child's `stretch: N` gives it extra room. A VBox where nothing grows
  vertically packs its items at the top.
- **Widgets**: `label`, `button`, `checkbox`, `textfield` (alias `edit`,
  `input`), `textarea`, `combo` (`dropdown`), `list`, `spin` (`min`,
  `max`), `slider` (`min`, `max`), `progress`, `group` (`title`, holds a
  layout like a window), `panel`, `tabs` (with `tab { title: "..." ... }`
  pages).
- **Properties**: `id`, `text`, `enabled`, `visible`, `tooltip`, `style`
  (Qt style sheet), `color`, `background`, `fontsize`, `bold`, `width` /
  `height` (minimum size). Also, per widget: `placeholder`, `type:
  Password`, `readonly` (textfield / textarea); `align: Left|Center|Right`
  and `wrap` (label); `checked` (checkbox); `items` and `selected`
  (combo / list); `value` (spin / slider / progress).
- **Events**: `onclick` (button, checkbox, list, combo, menu action),
  `onchange` (checkbox, combo, list, spin, slider, tabs, textfield,
  textarea), `onenter` (Enter in a textfield). Each names a SUB without
  parameters (use `QT "event"` in it to see which widget fired).

**INI .kfm**: the format `source/KfmParser.pas` and `formgenerator` write
also loads, pixel-positioned like a VB form:

```
[FORM:Form1]
[CONTROL:Form1:vctForm]
  Caption="My Form"
  Width=300
[CONTROL:Button1:vctButton]
  Caption="Click Me!"
  Left=20
  Top=20
```

Controls: `Button`, `Label`, `TextBox`, `CheckBox`, `ComboBox`, `ListBox`
(with or without the `vct` prefix), with `Caption` / `Text`, `Left`,
`Top`, `Width`, `Height`, `Enabled`, `Visible`. Handlers follow the VB
convention: a `SUB Button1_Click()` is attached if it exists. `OnClick=` /
`OnChange=` name one explicitly.

**Designer .ui**: a `.ui` file from Qt Designer / Qt Creator loads with
Qt's UiTools, which is built in when the Qt it's built against has it
(Homebrew's does). Its widgets are found by object name, and `QT "on"`
works on them as on QT-made widgets.

### Dialogs

| Command | Arguments | Result |
|---|---|---|
| `message` | title, text | 1 |
| `confirm` | title, text | 1 for Yes, 0 for No |
| `input` | title, prompt [, default] | entered text, `""` if cancelled |
| `openfile` / `savefile` | title [, filter] | chosen path, `""` if cancelled; filter like `"Text (*.txt);;All (*)"` |

Invalid handles, commands that don't apply to a widget (e.g. `additem` on
a button), unknown commands and wrong argument counts stop the script with
a `Runtime Error`.

## Limitations

- Handlers take no parameters (use `QT "event"`), and since Kayte
  variables are global, handlers share state through ordinary variables.
- Windows isn't supported by `--native` (the runtime uses POSIX APIs);
  the bytecode VM works there.

## Adding a widget or command

1. Add a `KQT_API` function in `kayte_qt6.cpp`.
2. Add the command to `dispatch()` at the bottom of the same file - the
   one place that defines command names, arguments and error messages.
   QML-only short names go in `qmlAlias()`.
   Rebuild the shim; the VM and native programs pick it up unchanged.
