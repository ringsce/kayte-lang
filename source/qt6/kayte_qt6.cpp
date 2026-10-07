/*
 * kayte_qt6.cpp - C ABI shim over QtWidgets for the Kayte VM.
 *
 * Qt is a C++ library, so Free Pascal can't bind to it directly the way
 * source/kayte_sdl2.pas binds to SDL's plain C API. This file exposes a
 * small extern "C" surface that source/kayte_qt6.pas loads at runtime
 * (dynlibs), keeping the kayte binary itself free of any Qt dependency.
 *
 * Widgets (and timers) are referred to by small integer ids rather than
 * raw pointers, so a script passing a stale or made-up handle gets an
 * error (0) instead of crashing the interpreter. Ids are tracked with
 * QPointer, so an object Qt deletes (e.g. a child of a destroyed window)
 * reads as invalid.
 *
 * Events use a polling model, because Kayte can't call back into a user
 * SUB yet (BC_CALL is unimplemented): user interaction (click, Enter,
 * toggle, selection/value change) and timer ticks queue the source id,
 * which kqt_wait_event() / kqt_poll_event() hand back to the script.
 * Changes made by the script itself (kqt_set_value, kqt_add_item, ...)
 * run under a QSignalBlocker so they don't echo back as events.
 *
 * Layouts (QBoxLayout/QGridLayout/QFormLayout) are registered like widgets
 * and share the same id space; kqt_layout_add accepts either a widget or
 * another layout as the item. Widgets created with a position keep it
 * until they're added to a layout, which then manages their geometry.
 *
 * Menu actions (QAction) and timers are QObjects but not QWidgets; the
 * state functions below check for them before treating an id as a widget.
 *
 * Language front ends don't call the kqt_* functions one by one: they
 * pass a command name and arguments to kqt_call() (bottom of this file),
 * the single place that knows the QT statement's command set, argument
 * rules and error messages. Both the bytecode VM (source/kayte_qt6.pas)
 * and natively compiled programs (source/native/kayte_native_rt.c) use it.
 *
 * QML (built when Qt Quick is found, KAYTE_QT6_QML): a .qml file or
 * inline source is created with a shared QQmlEngine, and its objects join
 * the same id space - "find" looks them up by objectName, "getprop" /
 * "setprop" / "call" reach their properties and functions, and "connect"
 * turns any signal (QML or widget) into a queued event via SignalRelay.
 *
 * Build: see source/qt6/CMakeLists.txt (no MOC needed - signals are
 * connected with lambdas, there are no Q_OBJECT classes; SignalRelay
 * dispatches its one slot by hand, like QSignalSpy).
 */

#include <QAction>
#include <QApplication>
#include <QCheckBox>
#include <QBoxLayout>
#include <QComboBox>
#include <QFile>
#include <QFileDialog>
#include <QFormLayout>
#include <QGridLayout>
#include <QGroupBox>
#include <QInputDialog>
#include <QLabel>
#include <QLineEdit>
#include <QKeySequence>
#include <QMetaMethod>
#include <QMetaProperty>
#include <QListWidget>
#include <QMenu>
#include <QMenuBar>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QEvent>
#include <QEventLoop>
#include <QPointer>
#include <QProgressBar>
#include <QPushButton>
#include <QSignalBlocker>
#include <QSlider>
#include <QSpinBox>
#include <QTabWidget>
#include <QTimer>
#include <QVariant>
#include <QWidget>
#include <QWindow>

#ifdef KAYTE_QT6_UITOOLS
#include <QUiLoader>
#endif

#ifdef KAYTE_QT6_QML
#include <QDir>
#include <QFileInfo>
#include <QJSValue>
#include <QQmlComponent>
#include <QQmlEngine>
#include <QQuickItem>
#include <QQuickWidget>
#include <QQuickWindow>
#include <QUrl>
#endif

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <stdexcept>
#include <deque>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#if defined(_WIN32)
#define KQT_API extern "C" __declspec(dllexport)
#else
#define KQT_API extern "C" __attribute__((visibility("default")))
#endif

namespace {

// QApplication keeps references to argc/argv for its whole lifetime.
int g_argc = 1;
char g_arg0[] = "kayte";
char *g_argv[] = {g_arg0, nullptr};

QApplication *g_app = nullptr;
std::unordered_map<int64_t, QPointer<QObject>> g_objects;
int64_t g_nextId = 1;
std::deque<int64_t> g_events;
std::string g_textBuf; // backing store for returned strings
// Set while a QT command runs (see kqt_call), so signals the script's own
// changes cause - settext firing textChanged, setprop, call - don't echo
// back as events. (QSignalBlocker would also silence the notify signals
// QML bindings depend on.)
bool g_scriptChange = false;

int64_t registerObject(QObject *o) {
  const int64_t id = g_nextId++;
  g_objects[id] = o;
  return id;
}

QObject *lookupObject(int64_t id) {
  auto it = g_objects.find(id);
  return it == g_objects.end() ? nullptr : it->second.data();
}

QWidget *lookup(int64_t id) { return qobject_cast<QWidget *>(lookupObject(id)); }

QLayout *lookupLayout(int64_t id) { return qobject_cast<QLayout *>(lookupObject(id)); }

QAction *lookupAction(int64_t id) { return qobject_cast<QAction *>(lookupObject(id)); }

// The tab widget a page (made by kqt_tab) belongs to, with its index.
QTabWidget *tabOwner(QWidget *page, int *index) {
  for (QWidget *p = page->parentWidget(); p; p = p->parentWidget())
    if (auto *tabs = qobject_cast<QTabWidget *>(p)) {
      *index = tabs->indexOf(page);
      return *index >= 0 ? tabs : nullptr;
    }
  return nullptr;
}

// Queues an event for the script. A repeat of the event already at the
// back of the queue is dropped, so a dragged slider or a fast timer can't
// flood a script that's slow to call wait/poll - it just sees the latest.
// The event loop kqt_wait_event / kqt_exec is running, if any.
QEventLoop *g_waitLoop = nullptr;

// Lets a waiting kqt_wait_event / kqt_exec re-check what it waits for.
void wakeWait() {
  if (g_waitLoop)
    g_waitLoop->quit();
}

void pushEvent(int64_t id) {
  if (g_scriptChange)
    return;
  if (g_events.empty() || g_events.back() != id)
    g_events.push_back(id);
  wakeWait();
}

// Application-wide filter waking the wait loop when a window hides or
// closes, so wait/exec notice when the last one is gone.
class WindowWatcher : public QObject {
public:
  bool eventFilter(QObject *o, QEvent *e) override {
    if (e->type() == QEvent::Hide || e->type() == QEvent::Close) {
      const bool window = o->isWindowType() || (o->isWidgetType() && static_cast<QWidget *>(o)->isWindow());
      if (window)
        wakeWait();
    }
    return false;
  }
};

// Runs a Qt event loop until wakeWait(). A real loop rather than repeated
// processEvents() calls: on macOS, quitting with Cmd+Q while no Qt event
// loop is running terminates the process on the spot, skipping the rest
// of the script (and its unflushed output). With one, Qt closes the
// windows instead, wait returns 0, and the script ends normally.
// Not on iOS: there a top-level exec() hands the thread to UIKit's run
// loop until the app ends - and there's no Cmd+Q to guard against.
void runWaitLoop() {
#ifdef Q_OS_IOS
  QCoreApplication::processEvents(QEventLoop::WaitForMoreEvents);
#else
  QEventLoop loop;
  g_waitLoop = &loop;
  loop.exec();
  g_waitLoop = nullptr;
#endif
}

int64_t popEvent() {
  const int64_t id = g_events.front();
  g_events.pop_front();
  return id;
}

// Qt only emits lastWindowClosed from inside QApplication::exec(), which
// the shim never enters (it pumps processEvents itself), so "are we done"
// is answered by checking the script's own top-level windows directly.
bool anyWindowVisible() {
  for (const auto &entry : g_objects) {
    QObject *o = entry.second.data();
    if (auto *w = qobject_cast<QWidget *>(o); w && w->isWindow() && w->isVisible())
      return true;
    if (auto *win = qobject_cast<QWindow *>(o); win && win->isVisible())
      return true;
  }
  return false;
}

// The id an object is already registered under, or a new one - so finding
// the same QML object twice gives the same handle (and the same events).
int64_t idOf(QObject *o) {
  for (const auto &entry : g_objects)
    if (entry.second.data() == o)
      return entry.first;
  return registerObject(o);
}

QWindow *lookupWindow(int64_t id) { return qobject_cast<QWindow *>(lookupObject(id)); }

// Forwards one signal of any object to the event queue under its own id,
// keeping the signal's arguments of the latest emission for "eventarg".
// Connected by index through QMetaObject::connect, so the signal's
// signature needn't be known at compile time; with no Q_OBJECT, the one
// slot (index = QObject's method count) is dispatched in qt_metacall.
class SignalRelay : public QObject {
public:
  SignalRelay(QObject *source, const QMetaMethod &signal) : QObject(source), signal_(signal) {}

  bool connectSignal(QObject *source) {
    return QMetaObject::connect(source, signal_.methodIndex(), this,
                                QObject::staticMetaObject.methodCount(), Qt::DirectConnection, nullptr);
  }

  int qt_metacall(QMetaObject::Call call, int id, void **a) override {
    id = QObject::qt_metacall(call, id, a);
    if (id < 0)
      return id;
    if (call == QMetaObject::InvokeMetaMethod) {
      if (id == 0)
        fired(a);
      --id;
    }
    return id;
  }

  int64_t id = 0;
  std::vector<QVariant> args;

private:
  void fired(void **a) {
    if (g_scriptChange)
      return;
    args.clear();
    for (int i = 0; i < signal_.parameterCount(); ++i) {
      const QMetaType t = signal_.parameterMetaType(i);
      if (t == QMetaType::fromType<QVariant>())
        args.push_back(*static_cast<QVariant *>(a[i + 1]));
#ifdef KAYTE_QT6_QML
      else if (t == QMetaType::fromType<QJSValue>())
        args.push_back(static_cast<QJSValue *>(a[i + 1])->toVariant());
#endif
      else
        args.push_back(QVariant(t, a[i + 1]));
    }
    pushEvent(id);
  }

  QMetaMethod signal_;
};

// A child widget needs a live parent; top-level windows are created by
// kqt_window only.
QWidget *parentOrNull(int64_t parent) { return g_app ? lookup(parent) : nullptr; }

// Positions a widget created (or moved) at pixel x, y. On iOS a window
// spans the whole screen - status bar, notch and home indicator included
// - so for windows without a layout, x, y count from the safe area's
// corner instead: applySafeArea() records that offset on the window as
// "kayteSafeOffset" (elsewhere it's never set, so this is a plain move).
void place(QWidget *w, int x, int y) {
  QWidget *p = w->parentWidget();
  const QPoint offset = p && p->isWindow() ? p->property("kayteSafeOffset").toPoint() : QPoint();
  w->move(x + offset.x(), y + offset.y());
}

#ifdef Q_OS_IOS
// Shifts a layout-less window's children into its safe area, now and
// whenever the margins change (e.g. on rotation). Layouts already keep
// clear of it themselves.
void applySafeArea(QWidget *w) {
  if (w->layout() || !w->windowHandle())
    return;
  const QMargins m = w->windowHandle()->safeAreaMargins();
  const QPoint now(m.left(), m.top());
  const QPoint delta = now - w->property("kayteSafeOffset").toPoint();
  if (!delta.isNull())
    for (QWidget *child : w->findChildren<QWidget *>(QString(), Qt::FindDirectChildrenOnly))
      if (!child->isWindow())
        child->move(child->pos() + delta);
  w->setProperty("kayteSafeOffset", now);
  if (!w->property("kayteSafeWatch").toBool()) {
    w->setProperty("kayteSafeWatch", true);
    QPointer<QWidget> guard(w);
    QObject::connect(w->windowHandle(), &QWindow::safeAreaMarginsChanged, w, [guard] {
      if (guard)
        applySafeArea(guard);
    });
  }
}
#endif

const char *returnString(const QString &s) {
  g_textBuf = s.toUtf8().toStdString();
  return g_textBuf.c_str();
}

} // namespace

KQT_API int kqt_init(void) {
  if (g_app)
    return 1;
  g_app = new QApplication(g_argc, g_argv);
  // The script, not Qt, decides when the program ends: closing the last
  // window just makes kqt_wait_event() return 0.
  g_app->setQuitOnLastWindowClosed(false);
  g_app->installEventFilter(new WindowWatcher);
  return 1;
}

/* ---- Widget creation: each returns the new id, or 0 for a bad parent ---- */

KQT_API int64_t kqt_window(const char *title, int width, int height) {
  if (!g_app)
    return 0;
  auto *w = new QWidget();
  w->setWindowTitle(QString::fromUtf8(title));
  w->resize(width, height);
  return registerObject(w);
}

KQT_API int64_t kqt_label(int64_t parent, const char *text, int x, int y) {
  QWidget *p = parentOrNull(parent);
  if (!p)
    return 0;
  auto *l = new QLabel(QString::fromUtf8(text), p);
  place(l, x, y);
  l->adjustSize();
  return registerObject(l);
}

KQT_API int64_t kqt_button(int64_t parent, const char *text, int x, int y) {
  QWidget *p = parentOrNull(parent);
  if (!p)
    return 0;
  auto *b = new QPushButton(QString::fromUtf8(text), p);
  place(b, x, y);
  b->adjustSize();
  const int64_t id = registerObject(b);
  QObject::connect(b, &QPushButton::clicked, [id] { pushEvent(id); });
  return id;
}

KQT_API int64_t kqt_checkbox(int64_t parent, const char *text, int x, int y) {
  QWidget *p = parentOrNull(parent);
  if (!p)
    return 0;
  auto *c = new QCheckBox(QString::fromUtf8(text), p);
  place(c, x, y);
  c->adjustSize();
  const int64_t id = registerObject(c);
  QObject::connect(c, &QCheckBox::toggled, [id] { pushEvent(id); });
  return id;
}

KQT_API int64_t kqt_edit(int64_t parent, const char *text, int x, int y, int width) {
  QWidget *p = parentOrNull(parent);
  if (!p)
    return 0;
  auto *e = new QLineEdit(QString::fromUtf8(text), p);
  place(e, x, y);
  if (width > 0)
    e->resize(width, e->sizeHint().height());
  const int64_t id = registerObject(e);
  QObject::connect(e, &QLineEdit::returnPressed, [id] { pushEvent(id); });
  return id;
}

// Multi-line plain-text editor. Doesn't queue events (every keystroke
// would be one) - read it with kqt_get_text when the script needs it.
KQT_API int64_t kqt_textedit(int64_t parent, int x, int y, int width, int height) {
  QWidget *p = parentOrNull(parent);
  if (!p)
    return 0;
  auto *t = new QPlainTextEdit(p);
  place(t, x, y);
  if (width > 0 && height > 0)
    t->resize(width, height);
  return registerObject(t);
}

KQT_API int64_t kqt_combo(int64_t parent, int x, int y, int width) {
  QWidget *p = parentOrNull(parent);
  if (!p)
    return 0;
  auto *c = new QComboBox(p);
  place(c, x, y);
  if (width > 0)
    c->resize(width, c->sizeHint().height());
  const int64_t id = registerObject(c);
  QObject::connect(c, &QComboBox::currentIndexChanged, [id] { pushEvent(id); });
  return id;
}

KQT_API int64_t kqt_list(int64_t parent, int x, int y, int width, int height) {
  QWidget *p = parentOrNull(parent);
  if (!p)
    return 0;
  auto *l = new QListWidget(p);
  place(l, x, y);
  if (width > 0 && height > 0)
    l->resize(width, height);
  const int64_t id = registerObject(l);
  QObject::connect(l, &QListWidget::currentRowChanged, [id] { pushEvent(id); });
  return id;
}

KQT_API int64_t kqt_spin(int64_t parent, int minimum, int maximum, int x, int y) {
  QWidget *p = parentOrNull(parent);
  if (!p)
    return 0;
  auto *s = new QSpinBox(p);
  s->setRange(minimum, maximum);
  place(s, x, y);
  s->adjustSize();
  const int64_t id = registerObject(s);
  QObject::connect(s, &QSpinBox::valueChanged, [id] { pushEvent(id); });
  return id;
}

KQT_API int64_t kqt_slider(int64_t parent, int minimum, int maximum, int x, int y, int width) {
  QWidget *p = parentOrNull(parent);
  if (!p)
    return 0;
  auto *s = new QSlider(Qt::Horizontal, p);
  s->setRange(minimum, maximum);
  place(s, x, y);
  s->resize(width > 0 ? width : s->sizeHint().width(), s->sizeHint().height());
  const int64_t id = registerObject(s);
  QObject::connect(s, &QSlider::valueChanged, [id] { pushEvent(id); });
  return id;
}

KQT_API int64_t kqt_progress(int64_t parent, int x, int y, int width) {
  QWidget *p = parentOrNull(parent);
  if (!p)
    return 0;
  auto *b = new QProgressBar(p);
  b->setRange(0, 100);
  b->setValue(0);
  place(b, x, y);
  b->resize(width > 0 ? width : b->sizeHint().width(), b->sizeHint().height());
  return registerObject(b);
}

// Repeating timer: queues its id as an event every interval_ms. Stop and
// restart it with kqt_set_enabled.
KQT_API int64_t kqt_timer(int interval_ms) {
  if (!g_app || interval_ms <= 0)
    return 0;
  auto *t = new QTimer(g_app);
  const int64_t id = registerObject(t);
  QObject::connect(t, &QTimer::timeout, [id] { pushEvent(id); });
  t->start(interval_ms);
  return id;
}

/* ---- Containers and layouts ---- */

// Titled frame grouping other widgets; usually given its own layout.
KQT_API int64_t kqt_group(int64_t parent, const char *title, int x, int y) {
  QWidget *p = parentOrNull(parent);
  if (!p)
    return 0;
  auto *g = new QGroupBox(QString::fromUtf8(title), p);
  place(g, x, y);
  return registerObject(g);
}

// Plain borderless container, e.g. to hold a nested layout in a grid cell.
KQT_API int64_t kqt_panel(int64_t parent, int x, int y, int width, int height) {
  QWidget *p = parentOrNull(parent);
  if (!p)
    return 0;
  auto *w = new QWidget(p);
  place(w, x, y);
  if (width > 0 && height > 0)
    w->resize(width, height);
  return registerObject(w);
}

enum { KQT_VBOX = 0, KQT_HBOX = 1, KQT_GRID = 2, KQT_FORM = 3 };

// Creates a layout of the given kind. With a parent (window, group or
// panel) the layout is installed on it - a widget can only hold one, so
// that fails if it already has a layout. With parent 0 the layout is
// standalone, waiting to be nested into another via kqt_layout_add.
KQT_API int64_t kqt_layout(int kind, int64_t parent) {
  if (!g_app)
    return 0;
  QWidget *p = nullptr;
  if (parent != 0) {
    p = lookup(parent);
    if (!p || p->layout())
      return 0;
  }
  QLayout *l = nullptr;
  switch (kind) {
  case KQT_VBOX: l = new QVBoxLayout(p); break;
  case KQT_HBOX: l = new QHBoxLayout(p); break;
  case KQT_GRID: l = new QGridLayout(p); break;
  case KQT_FORM: l = new QFormLayout(p); break;
  default: return 0;
  }
  // A window given menus before its layout: let the layout reserve the
  // menu bar's space (a no-op on macOS, where the bar is native).
  if (p)
    if (auto *bar = p->findChild<QMenuBar *>(QString(), Qt::FindDirectChildrenOnly))
      l->setMenuBar(bar);
  return registerObject(l);
}

// Adds a widget or nested layout. Box layouts: a = stretch factor (< 0
// for none). Grids: a, b = row, column (required), c, d = row/column span
// (< 1 for 1). Forms: the item spans the whole row (see kqt_layout_add_row
// for label + field rows).
KQT_API int kqt_layout_add(int64_t layout, int64_t item, int a, int b, int c, int d) {
  QLayout *l = lookupLayout(layout);
  QWidget *w = lookup(item);
  QLayout *sub = lookupLayout(item);
  if (!l || (!w && !sub) || sub == l)
    return 0;
  if (auto *box = qobject_cast<QBoxLayout *>(l)) {
    const int stretch = a > 0 ? a : 0;
    if (w)
      box->addWidget(w, stretch);
    else
      box->addLayout(sub, stretch);
  } else if (auto *grid = qobject_cast<QGridLayout *>(l)) {
    if (a < 0 || b < 0)
      return 0;
    const int rowSpan = c > 0 ? c : 1, colSpan = d > 0 ? d : 1;
    if (w)
      grid->addWidget(w, a, b, rowSpan, colSpan);
    else
      grid->addLayout(sub, a, b, rowSpan, colSpan);
  } else if (auto *form = qobject_cast<QFormLayout *>(l)) {
    if (w)
      form->addRow(w);
    else
      form->addRow(sub);
  } else
    return 0;
  return 1;
}

// Form layouts: a "label: field" row, where field is a widget or layout.
KQT_API int kqt_layout_add_row(int64_t layout, const char *label, int64_t item) {
  auto *form = qobject_cast<QFormLayout *>(lookupLayout(layout));
  QWidget *w = lookup(item);
  QLayout *sub = lookupLayout(item);
  if (!form || (!w && !sub))
    return 0;
  if (w)
    form->addRow(QString::fromUtf8(label), w);
  else
    form->addRow(QString::fromUtf8(label), sub);
  return 1;
}

// Box layouts: adds stretchable empty space, pushing later items to the
// far end (e.g. right-aligning buttons in an hbox).
KQT_API int kqt_layout_stretch(int64_t layout, int factor) {
  auto *box = qobject_cast<QBoxLayout *>(lookupLayout(layout));
  if (!box)
    return 0;
  box->addStretch(factor > 0 ? factor : 1);
  return 1;
}

// Grids: how extra space is shared between rows / columns. By default
// it's split evenly; a column with factor 1 while the rest stay 0 takes
// all of it.
KQT_API int kqt_grid_stretch(int64_t layout, int isRow, int index, int factor) {
  auto *grid = qobject_cast<QGridLayout *>(lookupLayout(layout));
  if (!grid || index < 0)
    return 0;
  if (isRow)
    grid->setRowStretch(index, factor);
  else
    grid->setColumnStretch(index, factor);
  return 1;
}

// Gap between items, in pixels.
KQT_API int kqt_layout_spacing(int64_t layout, int px) {
  QLayout *l = lookupLayout(layout);
  if (!l)
    return 0;
  l->setSpacing(px);
  return 1;
}

// Padding around the layout's edges, in pixels (all four sides).
KQT_API int kqt_layout_margins(int64_t layout, int px) {
  QLayout *l = lookupLayout(layout);
  if (!l)
    return 0;
  l->setContentsMargins(px, px, px, px);
  return 1;
}

/* ---- Menus and tabs ---- */

// A menu in a window's menu bar (created on first use), or - with a menu
// as parent - a submenu. On macOS the bar is shown natively at the top of
// the screen while the window is active.
KQT_API int64_t kqt_menu(int64_t parent, const char *title) {
  QObject *o = g_app ? lookupObject(parent) : nullptr;
  const QString t = QString::fromUtf8(title);
  QMenu *menu = nullptr;
  if (auto *parentMenu = qobject_cast<QMenu *>(o))
    menu = parentMenu->addMenu(t);
  else if (auto *w = qobject_cast<QWidget *>(o)) {
    if (!w->isWindow())
      return 0;
    auto *bar = w->findChild<QMenuBar *>(QString(), Qt::FindDirectChildrenOnly);
    if (!bar) {
      bar = new QMenuBar(w);
      if (w->layout())
        w->layout()->setMenuBar(bar);
    }
    menu = bar->addMenu(t);
    // Without a layout the bar has to be sized by hand - and only now
    // that it has a menu does it have a height.
    if (!w->layout())
      bar->setGeometry(0, 0, w->width(), bar->sizeHint().height());
  }
  return menu ? registerObject(menu) : 0;
}

// A menu item; choosing it queues its id. shortcut uses Qt's syntax, e.g.
// "Ctrl+S" (Cmd+S on macOS), or "" for none. A checkable item toggles a
// check mark each time it's chosen; read it with kqt_get_value.
KQT_API int64_t kqt_action(int64_t menu, const char *text, const char *shortcut, int checkable) {
  auto *m = qobject_cast<QMenu *>(lookupObject(menu));
  if (!m)
    return 0;
  QAction *a = m->addAction(QString::fromUtf8(text));
  if (shortcut && *shortcut)
    a->setShortcut(QKeySequence::fromString(QString::fromUtf8(shortcut)));
  a->setCheckable(checkable != 0);
  const int64_t id = registerObject(a);
  QObject::connect(a, &QAction::triggered, [id] { pushEvent(id); });
  return id;
}

KQT_API int kqt_separator(int64_t menu) {
  auto *m = qobject_cast<QMenu *>(lookupObject(menu));
  if (!m)
    return 0;
  m->addSeparator();
  return 1;
}

// Tab container; switching tabs queues its id, and its value is the
// current tab's index.
KQT_API int64_t kqt_tabs(int64_t parent, int x, int y, int width, int height) {
  QWidget *p = parentOrNull(parent);
  if (!p)
    return 0;
  auto *t = new QTabWidget(p);
  place(t, x, y);
  if (width > 0 && height > 0)
    t->resize(width, height);
  const int64_t id = registerObject(t);
  QObject::connect(t, &QTabWidget::currentChanged, [id] { pushEvent(id); });
  return id;
}

// Adds a page to a tab container and returns it: a plain container to put
// widgets (or a layout) on, like kqt_panel.
KQT_API int64_t kqt_tab(int64_t tabs, const char *title) {
  auto *t = qobject_cast<QTabWidget *>(lookup(tabs));
  if (!t)
    return 0;
  const QSignalBlocker block(t); // the first page would otherwise fire currentChanged
  auto *page = new QWidget();
  t->addTab(page, QString::fromUtf8(title));
  return registerObject(page);
}

/* ---- Widget state: each returns 1, or 0 for a bad/unsupported id ---- */

KQT_API int kqt_set_text(int64_t id, const char *text) {
  const QString s = QString::fromUtf8(text);
  if (QAction *a = lookupAction(id)) {
    a->setText(s);
    return 1;
  }
  if (QWindow *win = lookupWindow(id)) {
    win->setTitle(s);
    return 1;
  }
  QWidget *w = lookup(id);
  if (!w)
    return 0;
  int tabIndex;
  if (auto *m = qobject_cast<QMenu *>(w))
    m->setTitle(s);
  else if (QTabWidget *tabs = tabOwner(w, &tabIndex))
    tabs->setTabText(tabIndex, s);
  else if (auto *l = qobject_cast<QLabel *>(w)) {
    l->setText(s);
    l->adjustSize();
  } else if (auto *b = qobject_cast<QAbstractButton *>(w)) {
    b->setText(s);
    b->adjustSize();
  } else if (auto *e = qobject_cast<QLineEdit *>(w))
    e->setText(s);
  else if (auto *t = qobject_cast<QPlainTextEdit *>(w))
    t->setPlainText(s);
  else if (w->isWindow())
    w->setWindowTitle(s);
  else
    return 0;
  return 1;
}

// Returns a UTF-8 string owned by the shim, valid until the next call
// that returns a string. Combo/list return their current item's text.
KQT_API const char *kqt_get_text(int64_t id) {
  QString s;
  int tabIndex;
  if (QAction *a = lookupAction(id))
    s = a->text();
  else if (QWindow *win = lookupWindow(id))
    s = win->title();
  else if (QWidget *w = lookup(id)) {
    if (auto *m = qobject_cast<QMenu *>(w))
      s = m->title();
    else if (QTabWidget *tabs = tabOwner(w, &tabIndex))
      s = tabs->tabText(tabIndex);
    else if (auto *l = qobject_cast<QLabel *>(w))
      s = l->text();
    else if (auto *b = qobject_cast<QAbstractButton *>(w))
      s = b->text();
    else if (auto *e = qobject_cast<QLineEdit *>(w))
      s = e->text();
    else if (auto *t = qobject_cast<QPlainTextEdit *>(w))
      s = t->toPlainText();
    else if (auto *c = qobject_cast<QComboBox *>(w))
      s = c->currentText();
    else if (auto *lw = qobject_cast<QListWidget *>(w))
      s = lw->currentItem() ? lw->currentItem()->text() : QString();
    else if (auto *sp = qobject_cast<QSpinBox *>(w))
      s = sp->text();
    else
      s = w->windowTitle();
  }
  return returnString(s);
}

// The widget's numeric state: checkbox 0/1, combo/list current index (-1
// for none), spin/slider/progress value, tabs current index, checkable
// menu item 0/1.
KQT_API int kqt_get_value(int64_t id, int64_t *value) {
  if (QAction *a = lookupAction(id)) {
    *value = a->isChecked() ? 1 : 0;
    return 1;
  }
  QWidget *w = lookup(id);
  if (!w)
    return 0;
  if (auto *t = qobject_cast<QTabWidget *>(w))
    *value = t->currentIndex();
  else if (auto *c = qobject_cast<QCheckBox *>(w))
    *value = c->isChecked() ? 1 : 0;
  else if (auto *cb = qobject_cast<QComboBox *>(w))
    *value = cb->currentIndex();
  else if (auto *l = qobject_cast<QListWidget *>(w))
    *value = l->currentRow();
  else if (auto *s = qobject_cast<QAbstractSlider *>(w))
    *value = s->value();
  else if (auto *sp = qobject_cast<QSpinBox *>(w))
    *value = sp->value();
  else if (auto *p = qobject_cast<QProgressBar *>(w))
    *value = p->value();
  else
    return 0;
  return 1;
}

KQT_API int kqt_set_value(int64_t id, int64_t value) {
  if (QAction *a = lookupAction(id)) {
    if (!a->isCheckable())
      return 0;
    const QSignalBlocker block(a);
    a->setChecked(value != 0);
    return 1;
  }
  QWidget *w = lookup(id);
  if (!w)
    return 0;
  const QSignalBlocker block(w);
  if (auto *t = qobject_cast<QTabWidget *>(w))
    t->setCurrentIndex(static_cast<int>(value));
  else if (auto *c = qobject_cast<QCheckBox *>(w))
    c->setChecked(value != 0);
  else if (auto *cb = qobject_cast<QComboBox *>(w))
    cb->setCurrentIndex(static_cast<int>(value));
  else if (auto *l = qobject_cast<QListWidget *>(w))
    l->setCurrentRow(static_cast<int>(value));
  else if (auto *s = qobject_cast<QAbstractSlider *>(w))
    s->setValue(static_cast<int>(value));
  else if (auto *sp = qobject_cast<QSpinBox *>(w))
    sp->setValue(static_cast<int>(value));
  else if (auto *p = qobject_cast<QProgressBar *>(w))
    p->setValue(static_cast<int>(value));
  else
    return 0;
  return 1;
}

// Appends an entry to a combo box or list.
KQT_API int kqt_add_item(int64_t id, const char *text) {
  QWidget *w = lookup(id);
  if (!w)
    return 0;
  const QSignalBlocker block(w);
  if (auto *c = qobject_cast<QComboBox *>(w))
    c->addItem(QString::fromUtf8(text));
  else if (auto *l = qobject_cast<QListWidget *>(w))
    l->addItem(QString::fromUtf8(text));
  else
    return 0;
  return 1;
}

// Removes the entry at index from a combo box or list.
KQT_API int kqt_remove_item(int64_t id, int index) {
  QWidget *w = lookup(id);
  if (!w)
    return 0;
  const QSignalBlocker block(w);
  if (auto *c = qobject_cast<QComboBox *>(w)) {
    if (index < 0 || index >= c->count())
      return 0;
    c->removeItem(index);
  } else if (auto *l = qobject_cast<QListWidget *>(w)) {
    if (index < 0 || index >= l->count())
      return 0;
    delete l->takeItem(index);
  } else
    return 0;
  return 1;
}

// Removes every entry from a combo box / list, or empties a text field.
KQT_API int kqt_clear(int64_t id) {
  QWidget *w = lookup(id);
  if (!w)
    return 0;
  const QSignalBlocker block(w);
  if (auto *c = qobject_cast<QComboBox *>(w))
    c->clear();
  else if (auto *l = qobject_cast<QListWidget *>(w))
    l->clear();
  else if (auto *e = qobject_cast<QLineEdit *>(w))
    e->clear();
  else if (auto *t = qobject_cast<QPlainTextEdit *>(w))
    t->clear();
  else
    return 0;
  return 1;
}

KQT_API int kqt_geometry(int64_t id, int x, int y, int width, int height) {
  QWidget *w = lookup(id);
  if (!w)
    return 0;
  place(w, x, y);
  w->resize(width, height);
  return 1;
}

// Widgets and menu items: enable/disable input. Timers: start/stop.
KQT_API int kqt_set_enabled(int64_t id, int enabled) {
  QObject *o = lookupObject(id);
  if (auto *a = qobject_cast<QAction *>(o)) {
    a->setEnabled(enabled != 0);
    return 1;
  }
  if (auto *t = qobject_cast<QTimer *>(o)) {
    if (enabled)
      t->start();
    else
      t->stop();
    return 1;
  }
  QWidget *w = qobject_cast<QWidget *>(o);
  if (!w)
    return 0;
  w->setEnabled(enabled != 0);
  return 1;
}

// Applies a Qt style sheet (CSS-like), e.g. "color: red; font-size: 18px".
KQT_API int kqt_set_style(int64_t id, const char *css) {
  QWidget *w = lookup(id);
  if (!w)
    return 0;
  w->setStyleSheet(QString::fromUtf8(css));
  return 1;
}

KQT_API int kqt_show(int64_t id) {
  if (QWindow *win = lookupWindow(id)) {
    win->show();
    win->raise();
    win->requestActivate();
    return 1;
  }
  QWidget *w = lookup(id);
  if (!w)
    return 0;
  w->show();
  if (w->isWindow()) {
    w->raise();
    w->activateWindow();
#ifdef Q_OS_IOS
    applySafeArea(w);
#endif
  }
  return 1;
}

KQT_API int kqt_hide(int64_t id) {
  if (QWindow *win = lookupWindow(id)) {
    win->hide();
    return 1;
  }
  QWidget *w = lookup(id);
  if (!w)
    return 0;
  w->hide();
  return 1;
}

// Windows close (making wait return 0 once none are left); timers stop.
KQT_API int kqt_close(int64_t id) {
  QObject *o = lookupObject(id);
  if (auto *t = qobject_cast<QTimer *>(o)) {
    t->stop();
    return 1;
  }
  if (auto *win = qobject_cast<QWindow *>(o)) {
    win->close();
    return 1;
  }
  QWidget *w = qobject_cast<QWidget *>(o);
  if (!w)
    return 0;
  w->close();
  return 1;
}

/* ---- Dialogs (modal; they run their own event loop) ---- */

KQT_API int kqt_message(const char *title, const char *text) {
  if (!g_app)
    return 0;
  QMessageBox::information(nullptr, QString::fromUtf8(title), QString::fromUtf8(text));
  return 1;
}

// Yes/No question: 1 for Yes, 0 for No (or dismissed).
KQT_API int kqt_confirm(const char *title, const char *text) {
  if (!g_app)
    return 0;
  return QMessageBox::question(nullptr, QString::fromUtf8(title), QString::fromUtf8(text)) ==
         QMessageBox::Yes;
}

// Text prompt: the entered text, or "" if cancelled.
KQT_API const char *kqt_input(const char *title, const char *label, const char *def) {
  if (!g_app)
    return returnString(QString());
  bool ok = false;
  const QString s = QInputDialog::getText(nullptr, QString::fromUtf8(title), QString::fromUtf8(label),
                                          QLineEdit::Normal, QString::fromUtf8(def), &ok);
  return returnString(ok ? s : QString());
}

// File pickers: the chosen path, or "" if cancelled. filter uses Qt's
// syntax, e.g. "Text files (*.txt);;All files (*)", or "" for any file.
KQT_API const char *kqt_open_file(const char *title, const char *filter) {
  if (!g_app)
    return returnString(QString());
  return returnString(QFileDialog::getOpenFileName(nullptr, QString::fromUtf8(title), QString(),
                                                   QString::fromUtf8(filter)));
}

KQT_API const char *kqt_save_file(const char *title, const char *filter) {
  if (!g_app)
    return returnString(QString());
  return returnString(QFileDialog::getSaveFileName(nullptr, QString::fromUtf8(title), QString(),
                                                   QString::fromUtf8(filter)));
}

/* ---- Objects by name and signals: QML items and widgets alike ---- */

namespace {

// Depth-first search by objectName through QObject children and - for
// QML - the visual tree (items inside a Window hang off its contentItem).
QObject *findNamed(QObject *o, const QString &name) {
  if (!o)
    return nullptr;
  if (o->objectName() == name)
    return o;
  for (QObject *child : o->children())
    if (QObject *r = findNamed(child, name))
      return r;
#ifdef KAYTE_QT6_QML
  if (auto *item = qobject_cast<QQuickItem *>(o))
    for (QQuickItem *child : item->childItems())
      if (QObject *r = findNamed(child, name))
        return r;
  if (auto *win = qobject_cast<QQuickWindow *>(o))
    return findNamed(win->contentItem(), name);
  if (auto *qw = qobject_cast<QQuickWidget *>(o))
    return findNamed(qw->rootObject(), name);
#endif
  return nullptr;
}

} // namespace

// The object named objectName under id (or id itself if it has that
// name). "" gives a qmlwidget's root object, or id itself otherwise.
KQT_API int64_t kqt_find(int64_t id, const char *objectName) {
  QObject *o = lookupObject(id);
  if (!o)
    return 0;
  const QString name = QString::fromUtf8(objectName);
  QObject *found = nullptr;
  if (name.isEmpty()) {
    found = o;
#ifdef KAYTE_QT6_QML
    if (auto *qw = qobject_cast<QQuickWidget *>(o))
      found = qw->rootObject();
#endif
  } else
    found = findNamed(o, name);
  return found ? idOf(found) : 0;
}

// Turns a signal of id - "clicked", "textChanged", a QML "signal save()"...
// or a full signature like "valueChanged(int)" - into events under a new
// handle of its own, so one object can feed several handlers.
KQT_API int64_t kqt_connect(int64_t id, const char *signal) {
  QObject *o = lookupObject(id);
  if (!o)
    return 0;
  const QMetaObject *mo = o->metaObject();
  const QByteArray name(signal);
  int index = name.contains('(') ? mo->indexOfSignal(QMetaObject::normalizedSignature(signal)) : -1;
  for (int i = 0; index < 0 && i < mo->methodCount(); ++i)
    if (mo->method(i).methodType() == QMetaMethod::Signal && mo->method(i).name() == name)
      index = i;
  if (index < 0)
    return 0;
  auto *relay = new SignalRelay(o, mo->method(index));
  if (!relay->connectSignal(o)) {
    delete relay;
    return 0;
  }
  relay->id = registerObject(relay);
  return relay->id;
}

#ifdef KAYTE_QT6_QML

/* ---- QML ---- */

namespace {

QQmlEngine *g_qml = nullptr;
std::string g_qmlError; // why the last QML load failed

QQmlEngine *qmlEngine() {
  if (!g_qml) {
    g_qml = new QQmlEngine(g_app);
    // Qt.quit() / Qt.exit() close every window, ending QT "run" / "wait".
    const auto closeAll = [] {
      for (const auto &entry : g_objects) {
        if (auto *win = qobject_cast<QWindow *>(entry.second.data()))
          win->close();
        else if (auto *w = qobject_cast<QWidget *>(entry.second.data()); w && w->isWindow())
          w->close();
      }
    };
    QObject::connect(g_qml, &QQmlEngine::quit, closeAll);
    QObject::connect(g_qml, &QQmlEngine::exit, closeAll);
  }
  return g_qml;
}

// A script path (relative to the current directory) or a URL ("qrc:/...").
QUrl qmlUrl(const char *path) {
  const QString p = QString::fromUtf8(path);
  return p.contains(QStringLiteral("://")) ? QUrl(p) : QUrl::fromLocalFile(QFileInfo(p).absoluteFilePath());
}

// Instantiates a loaded component. A Window / ApplicationWindow root is
// registered as is; any other Item is given a window of its own that it
// fills, so a file whose root is a plain Rectangle works too. Like
// kqt_window's, the window starts hidden unless the QML says visible: true.
int64_t createRoot(QQmlComponent &component) {
  QObject *root = component.isError() ? nullptr : component.create();
  if (!root) {
    g_qmlError = component.errorString().trimmed().toStdString();
    return 0;
  }
  if (auto *item = qobject_cast<QQuickItem *>(root)) {
    auto *win = new QQuickWindow();
    item->setParent(win);
    item->setParentItem(win->contentItem());
    win->resize(item->width() > 0 ? int(item->width()) : 400, item->height() > 0 ? int(item->height()) : 300);
    item->setSize(win->size());
    QObject::connect(win, &QWindow::widthChanged, item, [item](int w) { item->setWidth(w); });
    QObject::connect(win, &QWindow::heightChanged, item, [item](int h) { item->setHeight(h); });
    root = win;
  }
  return registerObject(root);
}

} // namespace

// Extra directory to search for QML modules (import MyModule 1.0).
KQT_API int kqt_qml_import_path(const char *dir) {
  if (!g_app)
    return 0;
  qmlEngine()->addImportPath(QFileInfo(QString::fromUtf8(dir)).absoluteFilePath());
  return 1;
}

// Loads a .qml file and returns its window, or 0 (see g_qmlError).
KQT_API int64_t kqt_qml_load(const char *path) {
  if (!g_app) {
    g_qmlError = "run QT \"init\" first (QML statements do it themselves)";
    return 0;
  }
  QQmlComponent component(qmlEngine(), qmlUrl(path), QQmlComponent::PreferSynchronous);
  return createRoot(component);
}

// Same, from QML source text; relative imports and URLs resolve against
// the current directory.
KQT_API int64_t kqt_qml_source(const char *source) {
  if (!g_app) {
    g_qmlError = "run QT \"init\" first (QML statements do it themselves)";
    return 0;
  }
  QQmlComponent component(qmlEngine());
  component.setData(QByteArray(source), QUrl::fromLocalFile(QDir::current().filePath(QStringLiteral("inline.qml"))));
  return createRoot(component);
}

// A QML scene embedded in a widget window - it goes into layouts like any
// widget. Its root must be an Item (not a Window); it's resized to fit.
KQT_API int64_t kqt_qml_widget(int64_t parent, const char *path, int x, int y, int width, int height) {
  QWidget *p = parentOrNull(parent);
  if (!p) {
    g_qmlError = "the parent must be a valid window (and QT \"init\" must run first; QML statements do it themselves)";
    return 0;
  }
  auto *qw = new QQuickWidget(qmlEngine(), p);
  qw->setResizeMode(QQuickWidget::SizeRootObjectToView);
  qw->setSource(qmlUrl(path));
  if (qw->status() == QQuickWidget::Error) {
    QStringList errors;
    for (const QQmlError &e : qw->errors())
      errors << e.toString();
    g_qmlError = errors.join('\n').toStdString();
    delete qw;
    return 0;
  }
  place(qw, x, y);
  if (width > 0 && height > 0)
    qw->resize(width, height);
  return registerObject(qw);
}

#endif // KAYTE_QT6_QML

/* ---- Forms from files: .kfm (Kayte forms) and Qt Designer .ui ---- */

// kqt_load_form builds a window from a form file, the way QUiLoader does
// for Designer's .ui files. Two .kfm syntaxes are read:
//
//   Declarative (examples/form1.kfm):      INI-style (source/KfmParser.pas):
//     form Login {                            [FORM:Form1]
//       title: "Login"                        [CONTROL:Form1:vctForm]
//       layout: VBox {                          Caption="My Form"
//         textfield { id: "user" }            [CONTROL:Button1:vctButton]
//         button { text: "OK"                   Caption="OK"
//                  onclick: doLogin() }         Left=20
//       }                                       Top=20
//     }
//
// Widgets are made with the same kqt_* functions as QT statements, so they
// behave identically, and every id / control name becomes the widget's
// objectName for QT "find". Event handlers named in the file (onclick:
// doLogin()) - or, for INI forms, VB-style <Control>_Click SUBs - are
// recorded per window; the VM / native runtime fetch them right after
// loading (QT "formhandlers") and attach the SUBs, as QT "on" would.

namespace {

struct KfmNode;

struct KfmValue {
  enum Kind { Str, Num, Ident, Call, List, Node } kind = Str;
  std::string s; // Str / Ident text, Call: the function name
  int64_t n = 0;
  std::vector<KfmValue> list;
  std::shared_ptr<KfmNode> node; // Node: e.g. the VBox { ... } of "layout: VBox { ... }"
  int line = 0;
};

struct KfmNode {
  std::string type; // lower case: "button", "vbox", ...
  std::string name;
  std::vector<std::pair<std::string, KfmValue>> props; // keys lower case, in file order
  std::vector<KfmNode> children;
  int line = 0;

  const KfmValue *prop(const char *key) const {
    for (const auto &p : props)
      if (p.first == key)
        return &p.second;
    return nullptr;
  }
};

struct KfmBinding {
  int64_t handle;
  std::string sub;
  bool optional; // VB-style default (Button1_Click): skipped if there's no such SUB
};

// Window id -> the handlers its form file names, for QT "formhandlers".
std::unordered_map<int64_t, std::vector<KfmBinding>> g_formBindings;

std::string lower(std::string s) {
  for (char &ch : s)
    ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
  return s;
}

[[noreturn]] void kfmFail(const std::string &file, int line, const std::string &msg) {
  throw std::runtime_error(file + (line > 0 ? ":" + std::to_string(line) : "") + ": " + msg);
}

/* -- Declarative syntax -- */

class KfmReader {
public:
  KfmReader(const std::string &src, const std::string &file) : src_(src), file_(file) {}

  KfmNode parseFile() {
    skip();
    const int l = line_;
    KfmNode root = node(lower(ident("a form, e.g. \"form MyForm {\"")), l);
    skip();
    if (pos_ < src_.size())
      fail("unexpected text after the form's closing }");
    return root;
  }

private:
  [[noreturn]] void fail(const std::string &msg) const { kfmFail(file_, line_, msg); }

  char peek() const { return pos_ < src_.size() ? src_[pos_] : '\0'; }

  // Skips spaces, newlines and // or /* */ comments.
  void skip() {
    while (pos_ < src_.size()) {
      const char c = src_[pos_];
      if (c == '\n') {
        ++line_;
        ++pos_;
      } else if (std::isspace(static_cast<unsigned char>(c)))
        ++pos_;
      else if (c == '/' && pos_ + 1 < src_.size() && src_[pos_ + 1] == '/') {
        while (pos_ < src_.size() && src_[pos_] != '\n')
          ++pos_;
      } else if (c == '/' && pos_ + 1 < src_.size() && src_[pos_ + 1] == '*') {
        pos_ += 2;
        while (pos_ < src_.size() && !(src_[pos_] == '*' && pos_ + 1 < src_.size() && src_[pos_ + 1] == '/')) {
          if (src_[pos_] == '\n')
            ++line_;
          ++pos_;
        }
        if (pos_ >= src_.size())
          fail("unterminated /* comment");
        pos_ += 2;
      } else
        break;
    }
  }

  bool isIdentStart(char c) const { return std::isalpha(static_cast<unsigned char>(c)) || c == '_'; }

  std::string ident(const char *what) {
    skip();
    if (!isIdentStart(peek()))
      fail(std::string("expected ") + what);
    const size_t start = pos_;
    while (pos_ < src_.size() &&
           (std::isalnum(static_cast<unsigned char>(src_[pos_])) || src_[pos_] == '_' || src_[pos_] == '-' ||
            src_[pos_] == '.'))
      ++pos_;
    return src_.substr(start, pos_ - start);
  }

  std::string string() {
    const char quote = src_[pos_++];
    std::string out;
    while (pos_ < src_.size() && src_[pos_] != quote) {
      char c = src_[pos_++];
      if (c == '\n')
        fail("unterminated string");
      if (c == '\\' && pos_ < src_.size()) {
        c = src_[pos_++];
        if (c == 'n')
          c = '\n';
        else if (c == 't')
          c = '\t';
      }
      out += c;
    }
    if (pos_ >= src_.size())
      fail("unterminated string");
    ++pos_;
    return out;
  }

  // type [name] { (key: value | child)* }  - the type is already read.
  KfmNode node(const std::string &type, int line) {
    KfmNode n;
    n.type = type;
    n.line = line;
    skip();
    if (isIdentStart(peek()))
      n.name = ident("a name");
    else if (peek() == '"' || peek() == '\'')
      n.name = string();
    skip();
    if (peek() != '{')
      fail("expected { after \"" + type + "\"");
    ++pos_;
    for (;;) {
      skip();
      while (peek() == ';' || peek() == ',') {
        ++pos_;
        skip();
      }
      if (peek() == '}') {
        ++pos_;
        return n;
      }
      if (pos_ >= src_.size())
        kfmFail(file_, line, "\"" + type + "\" has no closing }");
      const int l = line_;
      const std::string key = lower(ident("a property (key: value) or a widget"));
      skip();
      if (peek() == ':') {
        ++pos_;
        n.props.emplace_back(key, value());
      } else
        n.children.push_back(node(key, l));
    }
  }

  KfmValue value() {
    skip();
    KfmValue v;
    v.line = line_;
    const char c = peek();
    if (c == '"' || c == '\'') {
      v.kind = KfmValue::Str;
      v.s = string();
    } else if (std::isdigit(static_cast<unsigned char>(c)) || c == '-') {
      const size_t start = pos_++;
      while (pos_ < src_.size() && std::isdigit(static_cast<unsigned char>(src_[pos_])))
        ++pos_;
      v.kind = KfmValue::Num;
      v.s = src_.substr(start, pos_ - start);
      if (v.s == "-")
        fail("expected a number after -");
      v.n = std::stoll(v.s);
    } else if (c == '[') {
      ++pos_;
      v.kind = KfmValue::List;
      for (;;) {
        skip();
        if (peek() == ']') {
          ++pos_;
          break;
        }
        v.list.push_back(value());
        skip();
        if (peek() == ',')
          ++pos_;
        else if (peek() != ']')
          fail("expected , or ] in list");
      }
    } else if (isIdentStart(c)) {
      v.s = ident("a value");
      v.kind = KfmValue::Ident;
      skip();
      if (peek() == '(') { // handler call: doLogin()
        ++pos_;
        skip();
        if (peek() != ')')
          fail("handlers take no arguments - write " + v.s + "()");
        ++pos_;
        v.kind = KfmValue::Call;
      } else if (peek() == '{') { // layout: VBox { ... }
        v.kind = KfmValue::Node;
        v.node = std::make_shared<KfmNode>(node(lower(v.s), v.line));
      }
    } else
      fail("expected a value");
    return v;
  }

  const std::string &src_;
  const std::string file_;
  size_t pos_ = 0;
  int line_ = 1;
};

/* -- INI syntax: the format KfmParser.pas / formgenerator / kfmlibgen use -- */

std::string trim(const std::string &s) {
  const auto b = s.find_first_not_of(" \t\r\n");
  if (b == std::string::npos)
    return std::string();
  return s.substr(b, s.find_last_not_of(" \t\r\n") - b + 1);
}

// Root type "ini": its props are the vctForm control's, its children the
// other controls (type = control type without "vct", lower case).
KfmNode readIniKfm(const std::string &src, const std::string &file) {
  KfmNode root;
  root.type = "ini";
  KfmNode *current = nullptr;
  bool currentIsForm = false;
  int lineNo = 0;
  size_t start = 0;
  while (start <= src.size()) {
    size_t end = src.find('\n', start);
    if (end == std::string::npos)
      end = src.size();
    const std::string line = trim(src.substr(start, end - start));
    start = end + 1;
    ++lineNo;
    if (line.empty() || line.rfind("//", 0) == 0 || line[0] == ';')
      continue;
    if (line.front() == '[' && line.back() == ']') {
      const std::string header = line.substr(1, line.size() - 2);
      const auto c1 = header.find(':');
      const std::string kind = lower(header.substr(0, c1));
      if (c1 != std::string::npos && kind == "form") {
        root.name = header.substr(c1 + 1);
        current = nullptr;
      } else if (c1 != std::string::npos && kind == "control") {
        const auto c2 = header.find(':', c1 + 1);
        if (c2 == std::string::npos)
          kfmFail(file, lineNo, "expected [CONTROL:Name:Type]");
        std::string type = lower(header.substr(c2 + 1));
        if (type.rfind("vct", 0) == 0)
          type.erase(0, 3);
        currentIsForm = type == "form";
        if (currentIsForm)
          current = &root;
        else {
          root.children.emplace_back();
          current = &root.children.back();
          current->type = type;
          current->name = header.substr(c1 + 1, c2 - c1 - 1);
          current->line = lineNo;
        }
      } else
        kfmFail(file, lineNo, "unknown section " + line + " (expected [FORM:Name] or [CONTROL:Name:Type])");
      continue;
    }
    const auto eq = line.find('=');
    if (eq == std::string::npos)
      kfmFail(file, lineNo, "expected Key=Value");
    if (!current)
      kfmFail(file, lineNo, "property before any [CONTROL:...] section");
    KfmValue v;
    v.line = lineNo;
    v.s = trim(line.substr(eq + 1));
    if (v.s.size() >= 2 && v.s.front() == '"' && v.s.back() == '"')
      v.s = v.s.substr(1, v.s.size() - 2);
    else if (!v.s.empty() && (std::isdigit(static_cast<unsigned char>(v.s[0])) || v.s[0] == '-') &&
             v.s.find_first_not_of("-0123456789") == std::string::npos && v.s != "-") {
      v.kind = KfmValue::Num;
      v.n = std::stoll(v.s);
    }
    current->props.emplace_back(lower(trim(line.substr(0, eq))), v);
  }
  return root;
}

/* -- Building widgets -- */

int layoutKindOf(const std::string &type) {
  if (type == "vbox" || type == "column" || type == "vertical")
    return KQT_VBOX;
  if (type == "hbox" || type == "row" || type == "horizontal")
    return KQT_HBOX;
  if (type == "grid")
    return KQT_GRID;
  if (type == "form" || type == "formlayout")
    return KQT_FORM;
  return -1;
}

class FormBuilder {
public:
  explicit FormBuilder(std::string file) : file_(std::move(file)) {}

  std::vector<KfmBinding> bindings;
  QPointer<QWidget> window; // deleted again if building fails

  int64_t build(const KfmNode &root) { return root.type == "ini" ? buildIni(root) : buildDeclarative(root); }

private:
  [[noreturn]] void fail(int line, const std::string &msg) const { kfmFail(file_, line, msg); }

  // Value helpers. Text accepts identifiers too (align: Center).
  std::string text(const KfmValue &v, const std::string &key) const {
    if (v.kind == KfmValue::List || v.kind == KfmValue::Node)
      fail(v.line, "\"" + key + "\" must be text");
    return v.s;
  }
  int64_t number(const KfmValue &v, const std::string &key) const {
    if (v.kind == KfmValue::Num)
      return v.n;
    fail(v.line, "\"" + key + "\" must be a number");
  }
  bool flag(const KfmValue &v, const std::string &key) const {
    if (v.kind == KfmValue::Num)
      return v.n != 0;
    const std::string s = lower(v.s);
    if (s == "true" || s == "yes" || s == "on")
      return true;
    if (s == "false" || s == "no" || s == "off")
      return false;
    fail(v.line, "\"" + key + "\" must be true or false");
  }
  std::string handlerName(const KfmValue &v, const std::string &key) const {
    if (v.kind != KfmValue::Call && v.kind != KfmValue::Ident && v.kind != KfmValue::Str)
      fail(v.line, "\"" + key + "\" must name a SUB, e.g. " + key + ": DoSomething()");
    return v.s;
  }
  std::string textOr(const KfmNode &n, const char *key, const char *def) const {
    const KfmValue *v = n.prop(key);
    return v ? text(*v, key) : std::string(def);
  }
  int numberOr(const KfmNode &n, const char *key, int def) const {
    const KfmValue *v = n.prop(key);
    return v ? static_cast<int>(number(*v, key)) : def;
  }

  // Attaches SUB `sub` to `widget`'s `event`. A widget's own event (a
  // button's click, a list's selection change, Enter in a text field -
  // what QT "on" uses) binds the widget's handle; other events get a
  // signal handle of their own, as QT "connect" would make.
  void bindEvent(int64_t widget, const std::string &type, const std::string &event, const std::string &sub,
                 int line, bool optional = false) {
    static const std::unordered_map<std::string, std::vector<std::string>> own = {
        {"button", {"onclick"}},
        {"checkbox", {"onclick", "onchange", "ontoggle"}},
        {"edit", {"onenter", "onsubmit"}},
        {"combo", {"onchange", "onselect", "onclick"}},
        {"list", {"onchange", "onselect", "onclick"}},
        {"spin", {"onchange"}},
        {"slider", {"onchange"}},
        {"tabs", {"onchange"}},
        {"action", {"onclick"}},
    };
    auto it = own.find(type);
    if (it != own.end() && std::find(it->second.begin(), it->second.end(), event) != it->second.end()) {
      bindings.push_back({widget, sub, optional});
      return;
    }
    if ((type == "edit" || type == "textedit") && event == "onchange") {
      const int64_t relay = kqt_connect(widget, "textChanged");
      bindings.push_back({relay, sub, optional});
      return;
    }
    std::string known;
    if (it != own.end())
      for (const auto &e : it->second)
        known += (known.empty() ? "" : ", ") + e;
    if (type == "edit" || type == "textedit")
      known += known.empty() ? "onchange" : ", onchange";
    fail(line, "a " + type + " has no \"" + event + "\" event" + (known.empty() ? "" : " (it has: " + known + ")"));
  }

  /* Declarative forms */

  int64_t buildDeclarative(const KfmNode &root) {
    if (root.type != "form" && root.type != "window" && root.type != "dialog")
      fail(root.line, "a form file must start with \"form Name {\" (or window / dialog), not \"" + root.type + "\"");
    const int64_t win = kqt_window(textOr(root, "title", root.name.c_str()).c_str(), numberOr(root, "width", 400),
                                   numberOr(root, "height", 300));
    window = lookup(win);
    window->setObjectName(QString::fromStdString(root.name));
    applyProps(win, "window", root, {"title", "width", "height", "layout"});
    for (const KfmNode &child : root.children)
      if (child.type == "menu")
        addMenu(win, child);
    fillContainer(win, root);
    return win;
  }

  // Lays out a window / group / panel / tab page: from "layout: VBox {...}",
  // a "content {...}" block (vertical), or its widget children directly
  // (vertical too).
  void fillContainer(int64_t container, const KfmNode &n) {
    std::vector<const KfmNode *> items;
    for (const KfmNode &child : n.children)
      if (child.type != "menu" && child.type != "tab" && child.type != "page")
        items.push_back(&child);
    const KfmValue *layout = n.prop("layout");
    if (layout) {
      if (layout->kind != KfmValue::Node || layoutKindOf(layout->node->type) < 0)
        fail(layout->line, "layout must be VBox { ... }, HBox { ... }, Grid { ... } or Form { ... }");
      if (!items.empty())
        fail(items.front()->line, "\"" + items.front()->type + "\" must go inside the layout block");
      makeLayout(*layout->node, container);
      return;
    }
    if (items.empty())
      return;
    if (items.size() == 1 && items.front()->type == "content") {
      KfmNode box = *items.front();
      box.type = "vbox";
      makeLayout(box, container);
      return;
    }
    KfmNode box;
    box.type = "vbox";
    box.line = n.line;
    for (const KfmNode *item : items)
      box.children.push_back(*item);
    makeLayout(box, container);
  }

  // A layout node: installed on `container`, or standalone (container 0)
  // to be nested. Widgets in it are created in `owner`.
  int64_t makeLayout(const KfmNode &n, int64_t container, int64_t owner = 0) {
    const int kind = layoutKindOf(n.type);
    const int64_t l = kqt_layout(kind, container);
    if (l == 0)
      fail(n.line, "cannot install a layout here (a container holds one layout)");
    if (owner == 0)
      owner = container;
    for (const auto &p : n.props) {
      if (p.first == "spacing")
        kqt_layout_spacing(l, static_cast<int>(number(p.second, p.first)));
      else if (p.first == "margins" || p.first == "margin" || p.first == "padding")
        kqt_layout_margins(l, static_cast<int>(number(p.second, p.first)));
      else if (p.first == "columns" && kind == KQT_GRID)
        ;
      else if (p.first == "stretch" || p.first == "row" || p.first == "col" || p.first == "column" ||
               p.first == "rowspan" || p.first == "colspan" || p.first == "label")
        ; // where this layout sits in its parent - read by place()
      else
        fail(p.second.line, "unknown layout property \"" + p.first + "\" (spacing, margins" +
                                (kind == KQT_GRID ? ", columns" : "") + ")");
    }
    const int columns = numberOr(n, "columns", 2);
    int cursor = 0; // grid auto-placement: next free cell, row-major
    for (const KfmNode &item : n.children) {
      if (item.type == "stretch" || item.type == "spacer") {
        if (kind != KQT_VBOX && kind != KQT_HBOX)
          fail(item.line, "stretch only works in VBox / HBox");
        kqt_layout_stretch(l, numberOr(item, "factor", 1));
        continue;
      }
      const int64_t id = layoutKindOf(item.type) >= 0 ? makeLayout(item, 0, owner) : createWidget(item, owner);
      place(l, kind, id, item, columns, cursor);
    }
    // A column of fixed-height items packs to the top rather than having
    // Qt spread the spare height between them (which on a tall window or
    // phone leaves labels floating mid-screen). Forms that say how to share
    // the space - a stretch {} or a "stretch:" factor - are left alone.
    if (kind == KQT_VBOX && !(lookupLayout(l)->expandingDirections() & Qt::Vertical)) {
      bool explicitStretch = false;
      for (const KfmNode &item : n.children)
        explicitStretch = explicitStretch || item.type == "stretch" || item.type == "spacer" || item.prop("stretch");
      if (!explicitStretch)
        kqt_layout_stretch(l, 1);
    }
    return l;
  }

  void place(int64_t l, int kind, int64_t id, const KfmNode &item, int columns, int &cursor) {
    bool ok;
    if (kind == KQT_GRID) {
      const KfmValue *row = item.prop("row");
      const KfmValue *col = item.prop("col") ? item.prop("col") : item.prop("column");
      int r, c;
      if (row || col) {
        r = row ? static_cast<int>(number(*row, "row")) : cursor / columns;
        c = col ? static_cast<int>(number(*col, "col")) : cursor % columns;
      } else {
        r = cursor / columns;
        c = cursor % columns;
      }
      const int colSpan = numberOr(item, "colspan", 1);
      cursor = r * columns + c + colSpan;
      ok = kqt_layout_add(l, id, r, c, numberOr(item, "rowspan", 1), colSpan);
    } else if (kind == KQT_FORM && item.prop("label"))
      ok = kqt_layout_add_row(l, text(*item.prop("label"), "label").c_str(), id);
    else
      ok = kqt_layout_add(l, id, numberOr(item, "stretch", -1), -1, -1, -1);
    if (!ok)
      fail(item.line, "could not add \"" + item.type + "\" to its layout");
  }

  static std::string canonicalType(const std::string &t) {
    static const std::unordered_map<std::string, std::string> names = {
        {"label", "label"},         {"text", "label"},           {"button", "button"},
        {"checkbox", "checkbox"},   {"check", "checkbox"},       {"textfield", "edit"},
        {"edit", "edit"},           {"input", "edit"},           {"lineedit", "edit"},
        {"textbox", "edit"},        {"textarea", "textedit"},    {"textedit", "textedit"},
        {"combo", "combo"},         {"combobox", "combo"},       {"dropdown", "combo"},
        {"select", "combo"},        {"list", "list"},            {"listbox", "list"},
        {"spin", "spin"},           {"spinbox", "spin"},         {"number", "spin"},
        {"slider", "slider"},       {"progress", "progress"},    {"progressbar", "progress"},
        {"group", "group"},         {"groupbox", "group"},       {"frame", "group"},
        {"panel", "panel"},         {"container", "panel"},      {"tabs", "tabs"},
        {"tabwidget", "tabs"},
    };
    auto it = names.find(t);
    return it == names.end() ? std::string() : it->second;
  }

  int64_t createWidget(const KfmNode &n, int64_t parent) {
    const std::string type = canonicalType(n.type);
    const std::string label = textOr(n, "text", "");
    const char *txt = label.c_str();
    int64_t id = 0;
    std::vector<std::string> used;
    if (type == "label" || type == "button" || type == "checkbox" || type == "edit" || type == "textedit") {
      used = {"text"};
      if (type == "label")
        id = kqt_label(parent, txt, 0, 0);
      else if (type == "button")
        id = kqt_button(parent, txt, 0, 0);
      else if (type == "checkbox")
        id = kqt_checkbox(parent, txt, 0, 0);
      else if (type == "edit")
        id = kqt_edit(parent, txt, 0, 0, 0);
      else if ((id = kqt_textedit(parent, 0, 0, 0, 0)) != 0)
        kqt_set_text(id, txt);
    } else if (type == "combo")
      id = kqt_combo(parent, 0, 0, 0);
    else if (type == "list")
      id = kqt_list(parent, 0, 0, 0, 0);
    else if (type == "spin" || type == "slider") {
      used = {"min", "max"};
      const int lo = numberOr(n, "min", 0), hi = numberOr(n, "max", 100);
      id = type == "spin" ? kqt_spin(parent, lo, hi, 0, 0) : kqt_slider(parent, lo, hi, 0, 0, 0);
    } else if (type == "progress")
      id = kqt_progress(parent, 0, 0, 0);
    else if (type == "group") {
      id = kqt_group(parent, textOr(n, "title", n.name.c_str()).c_str(), 0, 0);
      used = {"title", "layout"};
    } else if (type == "panel") {
      id = kqt_panel(parent, 0, 0, 0, 0);
      used = {"layout"};
    } else if (type == "tabs") {
      id = kqt_tabs(parent, 0, 0, 0, 0);
    } else
      fail(n.line, "unknown widget \"" + n.type +
                       "\" (label, button, checkbox, textfield, textarea, combo, list, spin, slider, progress, "
                       "group, panel, tabs, or a VBox / HBox / Grid / Form layout)");
    if (id == 0)
      fail(n.line, "could not create \"" + n.type + "\"");
    applyProps(id, type, n, used);
    if (type == "group" || type == "panel")
      fillContainer(id, n);
    else if (type == "tabs") {
      for (const KfmNode &page : n.children) {
        if (page.type != "tab" && page.type != "page")
          fail(page.line, "tabs can only contain tab { ... } pages");
        const int64_t p = kqt_tab(id, textOr(page, "title", page.name.c_str()).c_str());
        applyProps(p, "tab", page, {"title", "layout"});
        fillContainer(p, page);
      }
    } else if (!n.children.empty())
      fail(n.children.front().line, "a " + n.type + " can't contain other widgets");
    return id;
  }

  // Properties every widget takes, plus the per-type ones. `used` are the
  // ones the caller already handled.
  void applyProps(int64_t id, const std::string &type, const KfmNode &n, std::vector<std::string> used) {
    QWidget *w = lookup(id);
    std::string css;
    for (const auto &p : n.props) {
      const std::string &k = p.first;
      const KfmValue &v = p.second;
      if (std::find(used.begin(), used.end(), k) != used.end())
        continue;
      if (k == "id" || k == "name")
        w->setObjectName(QString::fromStdString(text(v, k)));
      else if (k.rfind("on", 0) == 0 && k.size() > 2)
        bindEvent(id, type, k, handlerName(v, k), v.line);
      else if (k == "enabled")
        kqt_set_enabled(id, flag(v, k));
      else if (k == "visible") {
        if (!flag(v, k))
          w->hide();
      } else if (k == "tooltip")
        w->setToolTip(QString::fromStdString(text(v, k)));
      else if (k == "style")
        css += text(v, k) + ";";
      else if (k == "color")
        css += "color: " + text(v, k) + ";";
      else if (k == "background")
        css += "background: " + text(v, k) + ";";
      else if (k == "fontsize" || k == "font-size")
        css += "font-size: " + std::to_string(number(v, k)) + "px;";
      else if (k == "bold") {
        if (flag(v, k))
          css += "font-weight: bold;";
      } else if ((k == "width" || k == "height") && type != "window") {
        if (k == "width")
          w->setMinimumWidth(static_cast<int>(number(v, k)));
        else
          w->setMinimumHeight(static_cast<int>(number(v, k)));
      } else if (k == "stretch" || k == "row" || k == "col" || k == "column" || k == "rowspan" ||
                 k == "colspan" || k == "label")
        ; // placement in the parent layout - read by place()
      else if (k == "placeholder" && type == "edit")
        static_cast<QLineEdit *>(w)->setPlaceholderText(QString::fromStdString(text(v, k)));
      else if (k == "placeholder" && type == "textedit")
        static_cast<QPlainTextEdit *>(w)->setPlaceholderText(QString::fromStdString(text(v, k)));
      else if (k == "type" && type == "edit") {
        const std::string t = lower(text(v, k));
        if (t == "password")
          static_cast<QLineEdit *>(w)->setEchoMode(QLineEdit::Password);
        else if (t != "text" && t != "normal")
          fail(v.line, "textfield type must be Password or Text");
      } else if (k == "readonly" && type == "edit")
        static_cast<QLineEdit *>(w)->setReadOnly(flag(v, k));
      else if (k == "readonly" && type == "textedit")
        static_cast<QPlainTextEdit *>(w)->setReadOnly(flag(v, k));
      else if (k == "align" && type == "label") {
        const std::string a = lower(text(v, k));
        const Qt::Alignment al = a == "center" ? Qt::AlignCenter : a == "right" ? Qt::AlignRight | Qt::AlignVCenter
                                 : a == "left" ? Qt::AlignLeft | Qt::AlignVCenter : Qt::Alignment();
        if (!al)
          fail(v.line, "align must be Left, Center or Right");
        static_cast<QLabel *>(w)->setAlignment(al);
      } else if (k == "wrap" && type == "label")
        static_cast<QLabel *>(w)->setWordWrap(flag(v, k));
      else if (k == "checked" && type == "checkbox")
        kqt_set_value(id, flag(v, k));
      else if (k == "items" && (type == "combo" || type == "list")) {
        if (v.kind != KfmValue::List)
          fail(v.line, "items must be a list, e.g. items: [\"One\", \"Two\"]");
        for (const KfmValue &item : v.list)
          kqt_add_item(id, text(item, "items").c_str());
      } else if ((k == "selected" || k == "index") && (type == "combo" || type == "list"))
        kqt_set_value(id, number(v, k));
      else if (k == "value" && (type == "spin" || type == "slider" || type == "progress"))
        kqt_set_value(id, number(v, k));
      else
        fail(v.line, "unknown property \"" + k + "\" for " + (type == "window" ? std::string("a form") : "a " + type));
    }
    if (!css.empty())
      kqt_set_style(id, css.c_str());
  }

  // menu "File" { action { text: "Open", shortcut: "Ctrl+O", onclick: Open() }  separator {}  menu "Recent" {...} }
  void addMenu(int64_t parent, const KfmNode &n) {
    const int64_t m = kqt_menu(parent, textOr(n, "title", n.name.c_str()).c_str());
    if (m == 0)
      fail(n.line, "could not create menu");
    for (const auto &p : n.props)
      if (p.first != "title")
        fail(p.second.line, "unknown property \"" + p.first + "\" for a menu");
    for (const KfmNode &item : n.children) {
      if (item.type == "menu")
        addMenu(m, item);
      else if (item.type == "separator")
        kqt_separator(m);
      else if (item.type == "action" || item.type == "item") {
        const KfmValue *checkable = item.prop("checkable");
        const int64_t a = kqt_action(m, textOr(item, "text", item.name.c_str()).c_str(),
                                     textOr(item, "shortcut", "").c_str(), checkable && flag(*checkable, "checkable"));
        for (const auto &p : item.props) {
          if (p.first == "text" || p.first == "shortcut" || p.first == "checkable")
            continue;
          if (p.first == "onclick")
            bindEvent(a, "action", "onclick", handlerName(p.second, p.first), p.second.line);
          else if (p.first == "id" || p.first == "name")
            lookupObject(a)->setObjectName(QString::fromStdString(text(p.second, p.first)));
          else if (p.first == "enabled")
            kqt_set_enabled(a, flag(p.second, p.first));
          else if (p.first == "checked")
            kqt_set_value(a, flag(p.second, p.first));
          else
            fail(p.second.line, "unknown property \"" + p.first + "\" for a menu action");
        }
      } else
        fail(item.line, "a menu holds action { }, separator { } or menu { }, not \"" + item.type + "\"");
    }
  }

  /* INI forms: VB-style, pixel positioned. Unknown properties are ignored
     (VB forms carry many), unknown control types are not. */

  int64_t buildIni(const KfmNode &root) {
    const int64_t win = kqt_window(textOr(root, "caption", root.name.c_str()).c_str(), numberOr(root, "width", 400),
                                   numberOr(root, "height", 300));
    window = lookup(win);
    window->setObjectName(QString::fromStdString(root.name));
    for (const KfmNode &c : root.children) {
      const std::string caption = c.prop("caption") ? textOr(c, "caption", "") : textOr(c, "text", "");
      const int x = numberOr(c, "left", 0), y = numberOr(c, "top", 0);
      int64_t id = 0;
      std::string type;
      if (c.type == "button" || c.type == "commandbutton") {
        type = "button";
        id = kqt_button(win, caption.c_str(), x, y);
      } else if (c.type == "label") {
        type = "label";
        id = kqt_label(win, caption.c_str(), x, y);
      } else if (c.type == "textbox") {
        type = "edit";
        id = kqt_edit(win, caption.c_str(), x, y, 0);
      } else if (c.type == "checkbox") {
        type = "checkbox";
        id = kqt_checkbox(win, caption.c_str(), x, y);
      } else if (c.type == "combobox") {
        type = "combo";
        id = kqt_combo(win, x, y, 0);
      } else if (c.type == "listbox") {
        type = "list";
        id = kqt_list(win, x, y, 0, 0);
      } else
        fail(c.line, "unknown control type \"" + c.type + "\" (Button, Label, TextBox, CheckBox, ComboBox, ListBox)");
      QWidget *w = lookup(id);
      w->setObjectName(QString::fromStdString(c.name));
      const KfmValue *wv = c.prop("width"), *hv = c.prop("height");
      if (wv || hv)
        w->resize(wv ? static_cast<int>(number(*wv, "Width")) : w->width(),
                  hv ? static_cast<int>(number(*hv, "Height")) : w->height());
      if (const KfmValue *e = c.prop("enabled"))
        kqt_set_enabled(id, flag(*e, "Enabled"));
      if (const KfmValue *v = c.prop("visible"); v && !flag(*v, "Visible"))
        w->hide();
      // Events: OnClick= / OnChange= name a SUB; otherwise VB's convention
      // applies - a SUB named <Control>_Click is used if there is one.
      bool clickBound = false;
      for (const auto &p : c.props)
        if (p.first.rfind("on", 0) == 0 && p.first.size() > 2) {
          bindEvent(id, type, p.first, handlerName(p.second, p.first), p.second.line);
          clickBound = clickBound || p.first == "onclick";
        }
      if (!clickBound && type != "label" && type != "edit")
        bindEvent(id, type, "onclick", c.name + "_Click", c.line, true);
    }
    return win;
  }

  const std::string file_;
};

std::string readFile(const std::string &path) {
  QFile f(QString::fromStdString(path));
  if (!f.open(QIODevice::ReadOnly))
    throw std::runtime_error("cannot open " + path);
  return f.readAll().toStdString();
}

// Builds a window from a .kfm (or, with Qt UiTools, Designer .ui) file and
// returns its handle. Throws std::runtime_error with "file:line: why".
int64_t loadFormFile(const std::string &path) {
  if (lower(path).size() >= 3 && lower(path).substr(lower(path).size() - 3) == ".ui") {
#ifdef KAYTE_QT6_UITOOLS
    QFile f(QString::fromStdString(path));
    if (!f.open(QIODevice::ReadOnly))
      throw std::runtime_error("cannot open " + path);
    QUiLoader loader;
    QWidget *w = loader.load(&f);
    if (!w)
      throw std::runtime_error(path + ": " + loader.errorString().toStdString());
    const int64_t win = registerObject(w);
    // Give every named widget the event its QT-made equivalent has, so QT
    // "on" / "wait" work on what QT "find" returns. Unnamed and qt_*
    // objects are Qt's internals (a spin box's line edit, ...).
    for (QObject *o : w->findChildren<QObject *>()) {
      const QString name = o->objectName();
      if (name.isEmpty() || name.startsWith(QStringLiteral("qt_")))
        continue;
      const int64_t id = idOf(o);
      const auto fire = [id] { pushEvent(id); };
      if (auto *check = qobject_cast<QCheckBox *>(o))
        QObject::connect(check, &QCheckBox::toggled, fire);
      else if (auto *button = qobject_cast<QAbstractButton *>(o))
        QObject::connect(button, &QAbstractButton::clicked, fire);
      else if (auto *edit = qobject_cast<QLineEdit *>(o))
        QObject::connect(edit, &QLineEdit::returnPressed, fire);
      else if (auto *combo = qobject_cast<QComboBox *>(o))
        QObject::connect(combo, &QComboBox::currentIndexChanged, fire);
      else if (auto *list = qobject_cast<QListWidget *>(o))
        QObject::connect(list, &QListWidget::currentRowChanged, fire);
      else if (auto *spin = qobject_cast<QSpinBox *>(o))
        QObject::connect(spin, &QSpinBox::valueChanged, fire);
      else if (auto *slider = qobject_cast<QAbstractSlider *>(o))
        QObject::connect(slider, &QAbstractSlider::valueChanged, fire);
      else if (auto *tabs = qobject_cast<QTabWidget *>(o))
        QObject::connect(tabs, &QTabWidget::currentChanged, fire);
      else if (auto *action = qobject_cast<QAction *>(o))
        QObject::connect(action, &QAction::triggered, fire);
    }
    return win;
#else
    throw std::runtime_error(".ui files need Qt's UiTools module, which this libkayte_qt6 was built without");
#endif
  }
  const std::string src = readFile(path);
  const auto first = src.find_first_not_of(" \t\r\n");
  // INI files start with a [section]; skip leading // comments to see.
  size_t at = first;
  while (at != std::string::npos && src.compare(at, 2, "//") == 0) {
    const auto nl = src.find('\n', at);
    at = nl == std::string::npos ? nl : src.find_first_not_of(" \t\r\n", nl);
  }
  const KfmNode root = at != std::string::npos && src[at] == '[' ? readIniKfm(src, path) : KfmReader(src, path).parseFile();
  FormBuilder builder(path);
  try {
    const int64_t win = builder.build(root);
    g_formBindings[win] = builder.bindings;
    return win;
  } catch (...) {
    delete builder.window.data(); // don't leave a half-built window behind
    throw;
  }
}

// "handle<TAB>SubName<TAB>optional" lines for a window loadFormFile made;
// the VM / native runtime attach them like QT "on". Consumed once.
std::string takeFormBindings(int64_t win) {
  std::string out;
  auto it = g_formBindings.find(win);
  if (it == g_formBindings.end())
    return out;
  for (const KfmBinding &b : it->second)
    out += std::to_string(b.handle) + "\t" + b.sub + "\t" + (b.optional ? "1" : "0") + "\n";
  g_formBindings.erase(it);
  return out;
}

} // namespace

/* ---- Event loop ---- */

// Blocks until an event is queued. Returns the id of the widget/timer
// that fired it, or 0 when no windows remain open.
KQT_API int64_t kqt_wait_event(void) {
  if (!g_app)
    return 0;
  while (g_events.empty() && anyWindowVisible())
    runWaitLoop();
  return g_events.empty() ? 0 : popEvent();
}

// Non-blocking variant for scripts doing their own work between events:
// the next queued id, -1 if nothing happened, or 0 when no windows remain.
KQT_API int64_t kqt_poll_event(void) {
  if (!g_app)
    return 0;
  QCoreApplication::processEvents();
  if (!g_events.empty())
    return popEvent();
  return anyWindowVisible() ? -1 : 0;
}

// Hands control to Qt until every window is closed - for scripts that
// don't need to react to events themselves.
KQT_API int kqt_exec(void) {
  if (!g_app)
    return 0;
  while (anyWindowVisible())
    runWaitLoop();
  return 1;
}

/* ---- Generic entry point for the QT statement ---- */

// One QT statement argument or result: a Kayte value is an integer or a
// UTF-8 string.
struct kqt_value {
  int32_t is_str;
  int32_t reserved;
  int64_t i;
  const char *s;
};

namespace {

std::string g_error;     // message for the last failed kqt_call
std::string g_resultStr; // backing store for a string result

// Wraps one kqt_call: argument access/validation, raising the QT
// statement's error messages as std::runtime_error.
class Call {
public:
  // cmd is the canonical command name, shown the name as the script
  // wrote it (QML statements use short aliases), keyword "QT" or "QML".
  Call(std::string cmd, std::string shown, const char *keyword, int argc, const kqt_value *args)
      : cmd_(std::move(cmd)), shown_(std::move(shown)), keyword_(keyword), argc_(argc), args_(args) {}

  [[noreturn]] void fail(const std::string &msg) const { throw std::runtime_error(msg); }

  std::string q() const { return keyword_ + " \"" + shown_ + "\""; }

  void argCount(int min, int max) const {
    if (argc_ >= min && argc_ <= max)
      return;
    if (min == max)
      fail(q() + " expects " + std::to_string(min) + " argument(s), got " + std::to_string(argc_));
    fail(q() + " expects " + std::to_string(min) + " to " + std::to_string(max) + " arguments, got " +
         std::to_string(argc_));
  }
  void need(int n) const { argCount(n, n); }

  std::string str(int i) const {
    return args_[i].is_str ? std::string(args_[i].s ? args_[i].s : "") : std::to_string(args_[i].i);
  }
  std::string strOr(int i, const char *def) const { return i < argc_ ? str(i) : std::string(def); }

  int64_t num(int i) const {
    if (!args_[i].is_str)
      return args_[i].i;
    std::string t = str(i);
    const auto notSpace = [](unsigned char c) { return !std::isspace(c); };
    t.erase(t.begin(), std::find_if(t.begin(), t.end(), notSpace));
    t.erase(std::find_if(t.rbegin(), t.rend(), notSpace).base(), t.end());
    char *end = nullptr;
    const long long v = t.empty() ? 0 : std::strtoll(t.c_str(), &end, 10);
    if (t.empty() || *end != '\0')
      fail(q() + " argument " + std::to_string(i + 1) + " must be a number, got \"" + str(i) + "\"");
    return v;
  }
  int n(int i) const { return static_cast<int>(num(i)); }
  int nOr(int i, int def) const { return i < argc_ ? n(i) : def; }

  // Creating something returns its id; 0 means the parent was invalid.
  int64_t created(int64_t id) const {
    if (id == 0)
      fail(q() + " failed - is the parent a valid window (and was " + keyword_ + " \"init\" run)?");
    return id;
  }
  // Commands on an existing object return 1, or 0 for a bad handle or an
  // object the command doesn't apply to (e.g. "additem" on a button).
  int64_t checked(int ok) const {
    if (ok == 0)
      fail(q() + " - invalid handle, or not supported by this kind of widget");
    return 1;
  }

  const std::string cmd_;
  const std::string shown_;
  const std::string keyword_;
  const int argc_;
  const kqt_value *args_;
};

// A Kayte value (integer or string) as a QVariant.
QVariant toVariant(const kqt_value &v) {
  if (v.is_str)
    return QString::fromUtf8(v.s ? v.s : "");
  return QVariant::fromValue<qlonglong>(v.i);
}

// A QVariant as a Kayte value: integers, bools (0/1), enums and whole
// reals as numbers; objects as handles (0 for null); everything else -
// fractional reals, strings, colors, urls, lists (one item per line) - as
// text in *str.
int64_t fromVariant(const QVariant &v, std::optional<std::string> *str) {
  const QMetaType t = v.metaType();
#ifdef KAYTE_QT6_QML
  if (t == QMetaType::fromType<QJSValue>())
    return fromVariant(v.value<QJSValue>().toVariant(), str);
#endif
  if (!v.isValid() || t.id() == QMetaType::Nullptr)
    return 0;
  switch (t.id()) {
  case QMetaType::Bool:
    return v.toBool() ? 1 : 0;
  case QMetaType::Int: case QMetaType::UInt: case QMetaType::LongLong: case QMetaType::ULongLong:
  case QMetaType::Long: case QMetaType::ULong: case QMetaType::Short: case QMetaType::UShort:
  case QMetaType::Char: case QMetaType::SChar: case QMetaType::UChar:
    return v.toLongLong();
  case QMetaType::Double: case QMetaType::Float: {
    const double d = v.toDouble();
    if (std::isfinite(d) && d == std::trunc(d) && std::fabs(d) < 9e18)
      return static_cast<int64_t>(d);
    *str = QString::number(d).toStdString();
    return 0;
  }
  default:
    break;
  }
  if (t.flags() & QMetaType::IsEnumeration)
    return v.toLongLong();
  if (t.flags() & QMetaType::PointerToQObject) {
    QObject *o = v.value<QObject *>();
    return o ? idOf(o) : 0;
  }
  if (v.canConvert<QVariantList>() && !v.canConvert<QString>()) {
    QStringList items;
    for (const QVariant &item : v.toList())
      items << item.toString();
    *str = items.join('\n').toStdString();
    return 0;
  }
  *str = v.toString().toStdString();
  return 0;
}

QObject *object(const Call &c, int i) {
  QObject *o = lookupObject(c.num(i));
  if (!o)
    c.fail(c.q() + " - invalid handle");
  return o;
}

// Converts a script value for a property/parameter of type t; QVariant
// ("var") ones take it unchanged.
QVariant convertTo(const Call &c, QVariant v, QMetaType t, const std::string &what) {
  if (t == QMetaType::fromType<QVariant>() || v.metaType() == t)
    return v;
  if (!v.convert(t))
    c.fail(c.q() + " - cannot convert \"" + v.toString().toStdString() + "\" to " + t.name() + " for " + what);
  return v;
}

int64_t getProperty(const Call &c, std::optional<std::string> *str) {
  QObject *o = object(c, 0);
  const std::string name = c.str(1);
  if (o->metaObject()->indexOfProperty(name.c_str()) < 0 && !o->dynamicPropertyNames().contains(name.c_str()))
    c.fail(c.q() + " - the object has no property \"" + name + "\"");
  return fromVariant(o->property(name.c_str()), str);
}

int64_t setProperty(const Call &c, const kqt_value &value) {
  QObject *o = object(c, 0);
  const std::string name = c.str(1);
  const int index = o->metaObject()->indexOfProperty(name.c_str());
  if (index < 0)
    c.fail(c.q() + " - the object has no property \"" + name + "\"");
  const QMetaProperty p = o->metaObject()->property(index);
  if (!p.isWritable())
    c.fail(c.q() + " - property \"" + name + "\" is read-only");
  const QVariant v = convertTo(c, toVariant(value), p.metaType(), "property \"" + name + "\"");
  const bool ok = p.write(o, v);
  if (!ok)
    c.fail(c.q() + " - could not set property \"" + name + "\"");
  return 1;
}

// Calls a method, slot or QML function by name with up to 8 arguments.
int64_t callMethod(const Call &c, std::optional<std::string> *str) {
  QObject *o = object(c, 0);
  const std::string name = c.str(1);
  const int argc = c.argc_ - 2;
  const QMetaObject *mo = o->metaObject();
  QMetaMethod m;
  for (int i = mo->methodCount() - 1; i >= 0 && !m.isValid(); --i)
    if (mo->method(i).name() == name.c_str() && mo->method(i).parameterCount() == argc)
      m = mo->method(i);
  if (!m.isValid())
    c.fail(c.q() + " - the object has no method \"" + name + "\" taking " + std::to_string(argc) + " argument(s)");

  QVariant values[8];
  QByteArray typeNames[8];
  QGenericArgument args[8];
  for (int k = 0; k < argc; ++k) {
    const QMetaType t = m.parameterMetaType(k);
    if (t == QMetaType::fromType<QVariant>()) {
      values[k] = toVariant(c.args_[k + 2]);
      args[k] = QGenericArgument("QVariant", &values[k]);
    } else {
      values[k] = convertTo(c, toVariant(c.args_[k + 2]), t, "argument " + std::to_string(k + 1) + " of \"" + name + "\"");
      typeNames[k] = m.parameterTypeName(k);
      args[k] = QGenericArgument(typeNames[k].constData(), values[k].constData());
    }
  }

  const QMetaType rt = m.returnMetaType();
  QVariant ret;
  QGenericReturnArgument retArg;
  if (rt == QMetaType::fromType<QVariant>())
    retArg = QGenericReturnArgument("QVariant", &ret);
  else if (rt.isValid() && rt.id() != QMetaType::Void) {
    ret = QVariant(rt);
    retArg = QGenericReturnArgument(m.typeName(), ret.data());
  }
  const bool ok = m.invoke(o, Qt::DirectConnection, retArg, args[0], args[1], args[2], args[3], args[4], args[5],
                           args[6], args[7]);
  if (!ok)
    c.fail(c.q() + " - calling \"" + name + "\" failed");
  return fromVariant(ret, str);
}

// Argument index of the latest emission of a "connect" handle's signal.
int64_t eventArg(const Call &c, std::optional<std::string> *str) {
  auto *relay = dynamic_cast<SignalRelay *>(lookupObject(c.num(0)));
  if (!relay)
    c.fail(c.q() + " - the handle must come from " + c.keyword_ + " \"connect\"");
  const int64_t i = c.num(1);
  if (i < 0 || i >= static_cast<int64_t>(relay->args.size()))
    c.fail(c.q() + " - the signal has " + std::to_string(relay->args.size()) + " argument(s), asked for index " +
           std::to_string(i));
  return fromVariant(relay->args[i], str);
}

// Returns the integer result, or sets *strResult for commands that
// produce text.
int64_t dispatch(const Call &c, std::optional<std::string> *strResult) {
  const std::string &cmd = c.cmd_;
  auto text = [](const std::string &s) { return s.c_str(); };

  // --- Application / event loop ---
  if (cmd == "init") { c.need(0); return kqt_init(); }
  if (cmd == "wait") { c.need(0); return kqt_wait_event(); }
  if (cmd == "poll") { c.need(0); return kqt_poll_event(); }
  if (cmd == "exec") { c.need(0); return kqt_exec(); }

  // --- Widget creation (positions/sizes optional; 0 = natural size) ---
  if (cmd == "window") { c.need(3); return c.created(kqt_window(text(c.str(0)), c.n(1), c.n(2))); }
  if (cmd == "label") { c.argCount(2, 4); return c.created(kqt_label(c.num(0), text(c.str(1)), c.nOr(2, 0), c.nOr(3, 0))); }
  if (cmd == "button") { c.argCount(2, 4); return c.created(kqt_button(c.num(0), text(c.str(1)), c.nOr(2, 0), c.nOr(3, 0))); }
  if (cmd == "checkbox") { c.argCount(2, 4); return c.created(kqt_checkbox(c.num(0), text(c.str(1)), c.nOr(2, 0), c.nOr(3, 0))); }
  if (cmd == "edit") { c.argCount(2, 5); return c.created(kqt_edit(c.num(0), text(c.str(1)), c.nOr(2, 0), c.nOr(3, 0), c.nOr(4, 0))); }
  if (cmd == "textedit") { c.argCount(1, 5); return c.created(kqt_textedit(c.num(0), c.nOr(1, 0), c.nOr(2, 0), c.nOr(3, 0), c.nOr(4, 0))); }
  if (cmd == "combo") { c.argCount(1, 4); return c.created(kqt_combo(c.num(0), c.nOr(1, 0), c.nOr(2, 0), c.nOr(3, 0))); }
  if (cmd == "list") { c.argCount(1, 5); return c.created(kqt_list(c.num(0), c.nOr(1, 0), c.nOr(2, 0), c.nOr(3, 0), c.nOr(4, 0))); }
  if (cmd == "spin") { c.argCount(3, 5); return c.created(kqt_spin(c.num(0), c.n(1), c.n(2), c.nOr(3, 0), c.nOr(4, 0))); }
  if (cmd == "slider") { c.argCount(3, 6); return c.created(kqt_slider(c.num(0), c.n(1), c.n(2), c.nOr(3, 0), c.nOr(4, 0), c.nOr(5, 0))); }
  if (cmd == "progress") { c.argCount(1, 4); return c.created(kqt_progress(c.num(0), c.nOr(1, 0), c.nOr(2, 0), c.nOr(3, 0))); }
  if (cmd == "timer") { c.need(1); return c.created(kqt_timer(c.n(0))); }

  // --- Containers and layouts ---
  if (cmd == "group") { c.argCount(2, 4); return c.created(kqt_group(c.num(0), text(c.str(1)), c.nOr(2, 0), c.nOr(3, 0))); }
  if (cmd == "panel") { c.argCount(1, 5); return c.created(kqt_panel(c.num(0), c.nOr(1, 0), c.nOr(2, 0), c.nOr(3, 0), c.nOr(4, 0))); }
  if (cmd == "vbox" || cmd == "hbox" || cmd == "grid" || cmd == "form") {
    c.argCount(0, 1);
    const int kind = cmd == "vbox" ? KQT_VBOX : cmd == "hbox" ? KQT_HBOX : cmd == "grid" ? KQT_GRID : KQT_FORM;
    const int64_t id = kqt_layout(kind, c.argc_ > 0 ? c.num(0) : 0);
    if (id == 0)
      c.fail(c.q() + " failed - the parent must be a valid window/group/panel without a layout already");
    return id;
  }
  if (cmd == "add") {
    c.argCount(2, 6);
    if (!kqt_layout_add(c.num(0), c.num(1), c.nOr(2, -1), c.nOr(3, -1), c.nOr(4, -1), c.nOr(5, -1)))
      c.fail(c.q() + " failed - needs a layout and a widget or layout handle (and row, column for a grid)");
    return 1;
  }
  if (cmd == "addrow") { c.need(3); return c.checked(kqt_layout_add_row(c.num(0), text(c.str(1)), c.num(2))); }
  if (cmd == "stretch") { c.argCount(1, 2); return c.checked(kqt_layout_stretch(c.num(0), c.nOr(1, 1))); }
  if (cmd == "colstretch" || cmd == "rowstretch") {
    c.need(3);
    return c.checked(kqt_grid_stretch(c.num(0), cmd == "rowstretch", c.n(1), c.n(2)));
  }
  if (cmd == "spacing") { c.need(2); return c.checked(kqt_layout_spacing(c.num(0), c.n(1))); }
  if (cmd == "margins") { c.need(2); return c.checked(kqt_layout_margins(c.num(0), c.n(1))); }

  // --- Menus and tabs ---
  if (cmd == "menu") {
    c.need(2);
    const int64_t id = kqt_menu(c.num(0), text(c.str(1)));
    if (id == 0)
      c.fail(c.q() + " failed - the parent must be a window or a menu");
    return id;
  }
  if (cmd == "action") {
    c.argCount(2, 4);
    const int64_t id = kqt_action(c.num(0), text(c.str(1)), text(c.strOr(2, "")), c.nOr(3, 0));
    if (id == 0)
      c.fail(c.q() + " failed - the first argument must be a menu");
    return id;
  }
  if (cmd == "separator") { c.need(1); return c.checked(kqt_separator(c.num(0))); }
  if (cmd == "tabs") { c.argCount(1, 5); return c.created(kqt_tabs(c.num(0), c.nOr(1, 0), c.nOr(2, 0), c.nOr(3, 0), c.nOr(4, 0))); }
  if (cmd == "tab") {
    c.need(2);
    const int64_t id = kqt_tab(c.num(0), text(c.str(1)));
    if (id == 0)
      c.fail(c.q() + " failed - the first argument must be a tabs widget");
    return id;
  }

  // --- Widget state ---
  if (cmd == "settext") { c.need(2); return c.checked(kqt_set_text(c.num(0), text(c.str(1)))); }
  if (cmd == "gettext") { c.need(1); *strResult = kqt_get_text(c.num(0)); return 0; }
  if (cmd == "getvalue") {
    c.need(1);
    int64_t v = 0;
    c.checked(kqt_get_value(c.num(0), &v));
    return v;
  }
  if (cmd == "setvalue") { c.need(2); return c.checked(kqt_set_value(c.num(0), c.num(1))); }
  if (cmd == "additem") { c.need(2); return c.checked(kqt_add_item(c.num(0), text(c.str(1)))); }
  if (cmd == "removeitem") { c.need(2); return c.checked(kqt_remove_item(c.num(0), c.n(1))); }
  if (cmd == "clear") { c.need(1); return c.checked(kqt_clear(c.num(0))); }
  if (cmd == "geometry") { c.need(5); return c.checked(kqt_geometry(c.num(0), c.n(1), c.n(2), c.n(3), c.n(4))); }
  if (cmd == "enable") { c.need(1); return c.checked(kqt_set_enabled(c.num(0), 1)); }
  if (cmd == "disable") { c.need(1); return c.checked(kqt_set_enabled(c.num(0), 0)); }
  if (cmd == "style") { c.need(2); return c.checked(kqt_set_style(c.num(0), text(c.str(1)))); }
  if (cmd == "show") { c.need(1); return c.checked(kqt_show(c.num(0))); }
  if (cmd == "hide") { c.need(1); return c.checked(kqt_hide(c.num(0))); }
  if (cmd == "close") { c.need(1); return c.checked(kqt_close(c.num(0))); }

  // --- Dialogs ---
  if (cmd == "message") { c.need(2); return kqt_message(text(c.str(0)), text(c.str(1))); }
  if (cmd == "confirm") { c.need(2); return kqt_confirm(text(c.str(0)), text(c.str(1))); }
  if (cmd == "input") {
    c.argCount(2, 3);
    *strResult = kqt_input(text(c.str(0)), text(c.str(1)), text(c.strOr(2, "")));
    return 0;
  }
  if (cmd == "openfile" || cmd == "savefile") {
    c.argCount(1, 2);
    const std::string title = c.str(0), filter = c.strOr(1, "");
    *strResult = cmd == "openfile" ? kqt_open_file(title.c_str(), filter.c_str())
                                   : kqt_save_file(title.c_str(), filter.c_str());
    return 0;
  }

  // --- Forms from files (.kfm, Designer .ui) ---
  if (cmd == "loadform") {
    c.need(1);
    if (!g_app)
      c.fail(c.q() + " - run " + c.keyword_ + " \"init\" first");
    try {
      return loadFormFile(c.str(0));
    } catch (const std::runtime_error &e) {
      c.fail(c.q() + " - " + e.what());
    }
  }
  if (cmd == "formhandlers") { c.need(1); *strResult = takeFormBindings(c.num(0)); return 0; }

  // --- Objects by name, properties, methods and signals (QML and widgets) ---
  if (cmd == "find") {
    c.need(2);
    const int64_t id = kqt_find(c.num(0), text(c.str(1)));
    if (id == 0)
      c.fail(c.q() + " - no object named \"" + c.str(1) + "\" (set objectName in QML / for the widget)");
    return id;
  }
  if (cmd == "connect") {
    c.need(2);
    object(c, 0);
    const int64_t id = kqt_connect(c.num(0), text(c.str(1)));
    if (id == 0)
      c.fail(c.q() + " - the object has no signal \"" + c.str(1) + "\"");
    return id;
  }
  if (cmd == "getprop") { c.need(2); return getProperty(c, strResult); }
  if (cmd == "setprop") { c.need(3); return setProperty(c, c.args_[2]); }
  if (cmd == "call") { c.argCount(2, 10); return callMethod(c, strResult); }
  if (cmd == "eventarg") { c.need(2); return eventArg(c, strResult); }

  // --- QML ---
#ifdef KAYTE_QT6_QML
  if (cmd == "qml" || cmd == "qmlsource") {
    c.need(1);
    const int64_t id = cmd == "qml" ? kqt_qml_load(text(c.str(0))) : kqt_qml_source(text(c.str(0)));
    if (id == 0)
      c.fail(c.q() + " failed: " + g_qmlError);
    return id;
  }
  if (cmd == "qmlwidget") {
    c.argCount(2, 6);
    const int64_t id = kqt_qml_widget(c.num(0), text(c.str(1)), c.nOr(2, 0), c.nOr(3, 0), c.nOr(4, 0), c.nOr(5, 0));
    if (id == 0)
      c.fail(c.q() + " failed: " + g_qmlError);
    return id;
  }
  if (cmd == "qmlimport") { c.need(1); return c.checked(kqt_qml_import_path(text(c.str(0)))); }
#else
  if (cmd.rfind("qml", 0) == 0)
    c.fail(c.q() + " - libkayte_qt6 was built without QML support; install Qt Quick "
           "(Debian: qt6-declarative-dev) and rerun scripts/build-kayte-qt6.sh");
#endif

  c.fail("unknown " + c.keyword_ + " command \"" + c.shown_ + "\"");
}

} // namespace

namespace {

// QML statement commands that are spelled differently from QT's; every
// other QT command works under QML as is.
const char *qmlAlias(const std::string &name) {
  static const std::unordered_map<std::string, const char *> aliases = {
      {"load", "qml"},      {"source", "qmlsource"}, {"widget", "qmlwidget"}, {"import", "qmlimport"},
      {"get", "getprop"},   {"set", "setprop"},      {"arg", "eventarg"},
  };
  auto it = aliases.find(name);
  return it == aliases.end() ? nullptr : it->second;
}

} // namespace

// Runs one QT or QML statement command. cmd is matched case-insensitively;
// a "qml." prefix marks a QML statement, which starts the application by
// itself (no "init" needed) and has the short names in qmlAlias(). On
// success fills *result (an integer, or a string valid until the next
// call) and returns 1; on failure returns 0 and kqt_last_error() says why.
KQT_API int kqt_call(const char *cmd, int argc, const kqt_value *args, kqt_value *result) {
  std::string name = cmd ? cmd : "";
  for (char &ch : name)
    ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
  const bool qml = name.rfind("qml.", 0) == 0;
  if (qml)
    name.erase(0, 4);
  std::string canonical = name;
  if (qml) {
    kqt_init();
    if (const char *alias = qmlAlias(name))
      canonical = alias;
  }
  // Signals a command itself causes (a settext firing textChanged, ...)
  // aren't events - except while waiting for the user: in the event loop
  // and in modal dialogs, what happens is the user's doing.
  static const char *const userTime[] = {"wait", "poll", "exec", "message", "confirm", "input", "openfile", "savefile"};
  bool quiet = true;
  for (const char *c : userTime)
    if (canonical == c)
      quiet = false;
  struct QuietScope {
    bool previous = g_scriptChange;
    explicit QuietScope(bool on) { g_scriptChange = on; }
    ~QuietScope() { g_scriptChange = previous; }
  } quietScope(quiet);
  try {
    std::optional<std::string> strResult;
    const int64_t v = dispatch(Call(canonical, name, qml ? "QML" : "QT", argc, args), &strResult);
    result->reserved = 0;
    if (strResult) {
      g_resultStr = *strResult;
      result->is_str = 1;
      result->i = 0;
      result->s = g_resultStr.c_str();
    } else {
      result->is_str = 0;
      result->i = v;
      result->s = nullptr;
    }
    return 1;
  } catch (const std::exception &e) {
    g_error = e.what();
    return 0;
  }
}

KQT_API const char *kqt_last_error(void) { return g_error.c_str(); }
