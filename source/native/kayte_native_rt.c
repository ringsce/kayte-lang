/*
 * kayte_native_rt.c - runtime for Kayte programs compiled with `kayte --native`.
 *
 * source/kayte_native.pas translates a program's bytecode into C - one
 * label per jump target, gotos for jumps - and that C #includes this file,
 * so a native program is a single translation unit built with the system C
 * compiler. Everything here mirrors the bytecode VM (VirtualMachine.pas)
 * so a program behaves the same whether it's run with --run or compiled
 * with --native:
 *
 *   - Values are dynamically typed: a 64-bit integer, a string, or an array
 *     (shared by reference; a class object is an array tagged with its
 *     class name). "+" adds two integers and otherwise concatenates; -, *,
 *     / need numbers (numeric strings convert); comparisons are numeric for
 *     two integers, by identity for arrays, and byte-wise otherwise.
 *   - TRY / CATCH: rt_error jumps (longjmp) back to main() and on to the
 *     CATCH when a TRY is active, instead of ending the program.
 *   - SUBs are reached with goto; CALL pushes a frame holding the label id
 *     to resume at, RETURN pops it and the generated code dispatches on it.
 *   - QT and QML statements go to libkayte_qt6's kqt_call() (loaded with dlopen on
 *     first use), except "on", "run" and "event", which need to call SUBs
 *     and are handled here, as the VM does. With KAYTE_QT_STATIC the shim
 *     (source/qt6/kayte_qt6.cpp) is linked into the program instead - how
 *     iOS apps are built (scripts/build-kayte-ios.sh), since iOS has no
 *     loadable libraries outside the app bundle.
 *
 * Strings are immutable and reference counted (kv_retain / kv_release), so
 * long-running GUI programs don't leak. Ownership rule: rt_pop() hands the
 * caller a reference it must release; rt_push() takes one over.
 *
 * Supported on macOS and Linux (PROCESS uses fork/exec, QT uses dlopen),
 * Windows (CreateProcess / LoadLibrary), iOS / tvOS (TARGET_OS_IPHONE),
 * where apps can't start processes, so PROCESS is a runtime error, and the
 * program starts in its bundle's resource directory so relative paths
 * (e.g. QML "load") find bundled files - and WebAssembly (WASI), which has
 * neither processes nor loadable libraries, so PROCESS and QT are errors.
 *
 * Two ways to build it:
 *   - #included by the C that `kayte --native` generates (the default): the
 *     program's tables (K_sub_names, ...) are defined before the #include
 *     and everything here is static.
 *   - On its own with KAYTE_RT_LIBRARY, for the LLVM backend (`kayte
 *     --llvm`, source/kayte_llvm.pas): the rt_* entry points the generated
 *     IR calls are exported (RT_API) and the tables are extern, defined by
 *     the IR.
 */

#define _POSIX_C_SOURCE 200809L
#define _XOPEN_SOURCE 700 /* realpath (X/Open): glibc / musl only declare it with this */
#define _DARWIN_C_SOURCE

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <locale.h>
#ifdef _WIN32
#include <windows.h>
#define strcasecmp _stricmp
#ifndef PATH_MAX
#define PATH_MAX MAX_PATH
#endif
#else
#include <strings.h>
#include <unistd.h>
#ifndef __wasi__
#include <dlfcn.h>
#include <sys/wait.h>
#endif
#endif
#ifndef PATH_MAX
#define PATH_MAX 4096
#endif
#ifdef __APPLE__
#include <TargetConditionals.h>
#include <mach-o/dyld.h>
#endif
#if defined(__APPLE__) && TARGET_OS_IPHONE
#include <CoreFoundation/CoreFoundation.h>
#define KAYTE_APPLE_MOBILE 1
#endif

/* Platforms without other processes / loadable libraries. */
#if defined(KAYTE_APPLE_MOBILE)
#define KAYTE_NO_PROCESS "iOS/tvOS (apps can't start other programs)"
#define KAYTE_NO_DYNLOAD 1 /* Qt is linked in instead (KAYTE_QT_STATIC) */
#elif defined(__wasi__)
#define KAYTE_NO_PROCESS "WebAssembly"
#define KAYTE_NO_DYNLOAD 1
#ifndef KAYTE_WASM_SJLJ /* setjmp through WebAssembly exception handling (kayte_llvm.pas) */
#define KAYTE_NO_SETJMP 1 /* else TRY can't catch errors there: they end the program */
#endif
#endif
#ifndef KAYTE_NO_SETJMP
#include <setjmp.h>
#endif

/* Entry points the generated program calls: static when the program
 * #includes this file, exported when it's built as a library. */
#ifdef KAYTE_RT_LIBRARY
#if defined(_WIN32)
#define RT_API __declspec(dllexport)
#else
#define RT_API __attribute__((visibility("default")))
#endif
#else
/* A program uses only the statements it has, so some go unused. */
#define RT_API static __attribute__((unused))
#endif

/* ------------------------------------------------------------------ */
/* Values                                                              */
/* ------------------------------------------------------------------ */

typedef struct kstr {
    int64_t rc;
    size_t len;
    char d[]; /* always NUL-terminated */
} kstr;

enum { K_INT = 0, K_STR = 1, K_ARR = 2, K_FLT = 3 };

typedef struct karr karr;

typedef struct {
    int kind;  /* K_INT, K_STR, K_ARR or K_FLT */
    int64_t i; /* K_INT */
    kstr *s;   /* K_STR */
    karr *a;   /* K_ARR */
    double f;  /* K_FLT: always finite */
} kv;

/* An array (or, with a tag, a class object): reference counted. */
struct karr {
    int64_t rc;
    int64_t len;
    kv *items;
    kstr *tag; /* class name, or NULL for a plain array */
};

RT_API void rt_error(const char *fmt, ...) __attribute__((noreturn, format(printf, 1, 2)));
static void rt_catch(char *msg); /* returns only when no TRY is active */

RT_API void rt_error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    int n = vsnprintf(NULL, 0, fmt, ap);
    va_end(ap);
    char *msg = malloc((size_t)(n > 0 ? n : 0) + 1);
    if (msg) {
        va_start(ap, fmt);
        vsnprintf(msg, (size_t)n + 1, fmt, ap);
        va_end(ap);
        rt_catch(msg); /* to the CATCH of an active TRY, if any */
    }
    fflush(stdout);
    fprintf(stderr, "Runtime Error: %s\n", msg ? msg : "out of memory");
    exit(1);
}

static void *rt_alloc(size_t n)
{
    void *p = malloc(n);
    if (!p)
        rt_error("out of memory");
    return p;
}

static kv kv_int(int64_t i)
{
    kv v = {K_INT, i, NULL, NULL, 0};
    return v;
}

static kv kv_flt(double d)
{
    if (!isfinite(d))
        rt_error("overflow (the result is too large for a number)");
    kv v = {K_FLT, 0, NULL, NULL, d == 0 ? 0.0 : d}; /* no -0 */
    return v;
}

/* A double's text, as the VM writes it (VB style): 15 significant digits
 * without trailing zeros, scientific notation (1.5E+20, 1E-07) outside
 * 1E-05 .. 1E+15. qb: QuickBASIC's ".5" for "0.5". */
static int fmt_flt(double v, char *out, int qb)
{
    char buf[48], digits[24];
    int nd = 0, neg = 0, e;
    char *o = out, *p;
    if (v == 0)
        return sprintf(out, "0");
    snprintf(buf, sizeof buf, "%.14e", v); /* -d.dddddddddddddde+XX */
    p = buf;
    if (*p == '-') {
        neg = 1;
        p++;
    }
    for (; *p && *p != 'e' && *p != 'E'; p++)
        if (*p >= '0' && *p <= '9' && nd < 20)
            digits[nd++] = *p;
    e = atoi(p + 1);
    while (nd > 1 && digits[nd - 1] == '0')
        nd--;
    if (neg)
        *o++ = '-';
    if (e >= 15 || e < -5) {
        *o++ = digits[0];
        if (nd > 1) {
            *o++ = '.';
            memcpy(o, digits + 1, (size_t)(nd - 1));
            o += nd - 1;
        }
        o += sprintf(o, "E%c%02d", e < 0 ? '-' : '+', e < 0 ? -e : e);
    } else if (e >= 0) {
        for (int k = 0; k <= e; k++)
            *o++ = k < nd ? digits[k] : '0';
        if (nd > e + 1) {
            *o++ = '.';
            memcpy(o, digits + e + 1, (size_t)(nd - e - 1));
            o += nd - e - 1;
        }
    } else {
        if (!qb)
            *o++ = '0';
        *o++ = '.';
        for (int k = 0; k < -e - 1; k++)
            *o++ = '0';
        memcpy(o, digits, (size_t)nd);
        o += nd;
    }
    *o = '\0';
    return (int)(o - out);
}

static kv kv_str(const char *s, size_t len)
{
    kstr *k = rt_alloc(sizeof(kstr) + len + 1);
    k->rc = 1;
    k->len = len;
    memcpy(k->d, s, len);
    k->d[len] = '\0';
    kv v = {K_STR, 0, k, NULL, 0};
    return v;
}

static kv kv_retain(kv v)
{
    if (v.kind == K_STR)
        v.s->rc++;
    else if (v.kind == K_ARR)
        v.a->rc++;
    return v;
}

static void kv_release(kv v)
{
    if (v.kind == K_STR) {
        if (--v.s->rc == 0)
            free(v.s);
    } else if (v.kind == K_ARR && --v.a->rc == 0) {
        for (int64_t j = 0; j < v.a->len; j++)
            kv_release(v.a->items[j]);
        free(v.a->items);
        if (v.a->tag && --v.a->tag->rc == 0)
            free(v.a->tag);
        free(v.a);
    }
}

/* A new array of len zeros (an object when tag isn't NULL; retained). */
static kv kv_arr(int64_t len, kstr *tag)
{
    karr *a = rt_alloc(sizeof(karr));
    a->rc = 1;
    a->len = len;
    a->items = rt_alloc(sizeof(kv) * (size_t)(len > 0 ? len : 1));
    for (int64_t j = 0; j < len; j++)
        a->items[j] = kv_int(0);
    a->tag = tag;
    if (tag)
        tag->rc++;
    kv v = {K_ARR, 0, NULL, a, 0};
    return v;
}

/* A growable byte buffer, for building text. */
typedef struct {
    char *d;
    size_t len, cap;
} kbuf;

static void kb_add(kbuf *b, const char *s, size_t n)
{
    if (b->len + n + 1 > b->cap) {
        b->cap = (b->len + n + 1) * 2;
        b->d = realloc(b->d, b->cap);
        if (!b->d)
            rt_error("out of memory");
    }
    memcpy(b->d + b->len, s, n);
    b->len += n;
    b->d[b->len] = '\0';
}

/* Arrays as [a, b, c], objects as <ClassName> - as the VM shows them. */
static void kb_value(kbuf *b, kv v, int depth)
{
    char t[32];
    if (v.kind == K_INT) {
        int n = snprintf(t, sizeof(t), "%" PRId64, v.i);
        kb_add(b, t, (size_t)n);
    } else if (v.kind == K_FLT) {
        int n = fmt_flt(v.f, t, 0);
        kb_add(b, t, (size_t)n);
    } else if (v.kind == K_STR) {
        kb_add(b, v.s->d, v.s->len);
    } else if (v.a->tag) {
        kb_add(b, "<", 1);
        kb_add(b, v.a->tag->d, v.a->tag->len);
        kb_add(b, ">", 1);
    } else if (depth > 16) {
        kb_add(b, "[...]", 5);
    } else {
        kb_add(b, "[", 1);
        for (int64_t j = 0; j < v.a->len; j++) {
            if (j > 0)
                kb_add(b, ", ", 2);
            kb_value(b, v.a->items[j], depth + 1);
        }
        kb_add(b, "]", 1);
    }
}

/* A value's text. owned is set (and must be freed with kt_done) when it
 * had to be built, as for arrays. */
typedef struct {
    const char *s;
    size_t len;
    char *owned;
    char tmp[48];
} ktext;

static void kt_get(kv v, ktext *t)
{
    t->owned = NULL;
    if (v.kind == K_STR) {
        t->s = v.s->d;
        t->len = v.s->len;
    } else if (v.kind == K_INT) {
        t->len = (size_t)snprintf(t->tmp, sizeof(t->tmp), "%" PRId64, v.i);
        t->s = t->tmp;
    } else if (v.kind == K_FLT) {
        t->len = (size_t)fmt_flt(v.f, t->tmp, 0);
        t->s = t->tmp;
    } else {
        kbuf b = {NULL, 0, 0};
        kb_add(&b, "", 0);
        kb_value(&b, v, 0);
        t->owned = b.d;
        t->s = b.d;
        t->len = b.len;
    }
}

static void kt_done(ktext *t) { free(t->owned); }

/* Strict string -> integer, like FPC's TryStrToInt64(Trim(s)). */
static int str_to_int(const char *s, int64_t *out)
{
    while (isspace((unsigned char)*s))
        s++;
    if (!*s)
        return 0;
    char *end;
    errno = 0;
    long long v = strtoll(s, &end, 10);
    while (isspace((unsigned char)*end))
        end++;
    if (*end || errno == ERANGE)
        return 0;
    *out = v;
    return 1;
}

/* Strict string -> number (whole or with a fraction / exponent), like the
 * VM's StrToNumber: optional spaces, a sign, digits with at most one ".",
 * an optional exponent. Independent of the C locale (Qt may set one). */
static int str_to_num(const char *s, kv *out)
{
    int64_t i;
    if (str_to_int(s, &i)) {
        *out = kv_int(i);
        return 1;
    }
    while (isspace((unsigned char)*s))
        s++;
    size_t n = strlen(s);
    while (n > 0 && isspace((unsigned char)s[n - 1]))
        n--;
    if (n == 0 || n > 400)
        return 0;
    size_t p = 0;
    int digits = 0, dots = 0;
    if (s[p] == '+' || s[p] == '-')
        p++;
    for (; p < n && (isdigit((unsigned char)s[p]) || s[p] == '.'); p++) {
        if (s[p] == '.')
            dots++;
        else
            digits++;
    }
    if (!digits || dots > 1)
        return 0;
    if (p < n && (s[p] == 'e' || s[p] == 'E')) {
        p++;
        if (p < n && (s[p] == '+' || s[p] == '-'))
            p++;
        if (p >= n || !isdigit((unsigned char)s[p]))
            return 0;
        while (p < n && isdigit((unsigned char)s[p]))
            p++;
    }
    if (p != n)
        return 0;
    char buf[408];
    memcpy(buf, s, n);
    buf[n] = '\0';
    char *dot = strchr(buf, '.');
    const char *lp = localeconv()->decimal_point;
    if (dot && lp && *lp)
        *dot = *lp;
    double d = strtod(buf, NULL);
    if (!isfinite(d))
        return 0;
    *out = kv_flt(d);
    return 1;
}

static int is_num(kv v) { return v.kind == K_INT || v.kind == K_FLT; }

/* v as a number (K_INT or K_FLT); strings are converted. */
static kv kv_num(kv v)
{
    kv r;
    if (is_num(v))
        return v;
    if (v.kind == K_ARR) {
        ktext t;
        kt_get(v, &t);
        rt_error("cannot use %s as a number", t.s);
    }
    if (!str_to_num(v.s->d, &r))
        rt_error("cannot convert \"%s\" to a number", v.s->d);
    return r;
}

static double kv_to_flt(kv v)
{
    kv n = kv_num(v);
    return n.kind == K_INT ? (double)n.i : n.f;
}

/* A double rounded to a whole number (to even on .5, like VB's CInt). */
static int64_t flt_to_int(double d)
{
    double r = rint(d);
    if (r >= 9223372036854775807.0 || r < -9223372036854775808.0)
        rt_error("overflow (the number is too large for a whole number)");
    return (int64_t)r;
}

static int64_t kv_to_int(kv v)
{
    kv n = kv_num(v);
    return n.kind == K_INT ? n.i : flt_to_int(n.f);
}

static int kv_truthy(kv v)
{
    return v.kind == K_STR ? v.s->len > 0 : v.kind == K_INT ? v.i != 0 : v.kind == K_FLT ? v.f != 0 : 1;
}

/* ------------------------------------------------------------------ */
/* Machine state                                                       */
/* ------------------------------------------------------------------ */

static kv *K_vars;
static int K_nvars;
/* RND's generator: splitmix64 (libc's rand() is weak - on macOS its
 * first value barely changes with the seed). */
static uint64_t K_rnd;
static kv *K_stack;
static int K_sp, K_stack_cap;

typedef struct {
    int ret;  /* label id to resume at */
    int argc; /* arguments pushed for the SUB, checked by rt_enter */
} kframe;

#define K_MAX_CALL_DEPTH 10000
static kframe K_frames[K_MAX_CALL_DEPTH];
static int K_depth;

static const char *K_argv0;

/* The generated program defines these before #including this file:
 *   K_nsubs, K_sub_names[], K_sub_addrs[], K_sub_params[] - its SUBs
 *     (name, entry label, parameter count), for QT "on"
 *   K_qt_lib_fallback - the libkayte_qt6 kayte used at compile time, or ""
 * In library mode the LLVM IR defines them as globals. */
#ifdef KAYTE_RT_LIBRARY
extern const int K_nsubs;
extern const char *const K_sub_names[];
extern const int K_sub_addrs[];
extern const int K_sub_params[];
extern const char *const K_qt_lib_fallback;
#endif

/* End of the program: flush output, give main() its exit code. */
RT_API int rt_finish(void)
{
    fflush(stdout);
    return 0;
}

RT_API void rt_init(int nvars, const char *argv0)
{
    K_argv0 = argv0;
    /* RND differs from run to run, as in the VM (the address varies with ASLR). */
    K_rnd = (uint64_t)time(NULL) * 0x9E3779B97F4A7C15ULL ^ (uint64_t)(uintptr_t)&nvars;
    K_nvars = nvars;
    K_vars = rt_alloc(sizeof(kv) * (size_t)(nvars > 0 ? nvars : 1));
    for (int i = 0; i < nvars; i++)
        K_vars[i] = kv_int(0);
    setvbuf(stdout, NULL, _IOLBF, 0);
#ifdef KAYTE_APPLE_MOBILE
    /* An app starts in "/"; make relative paths mean "in the app bundle". */
    CFURLRef url = CFBundleCopyResourcesDirectoryURL(CFBundleGetMainBundle());
    char dir[PATH_MAX];
    if (url && CFURLGetFileSystemRepresentation(url, true, (UInt8 *)dir, sizeof(dir)))
        chdir(dir);
    if (url)
        CFRelease(url);
#endif
}

static void rt_push(kv v)
{
    if (K_sp == K_stack_cap) {
        K_stack_cap = K_stack_cap * 2 + 16;
        K_stack = realloc(K_stack, sizeof(kv) * (size_t)K_stack_cap);
        if (!K_stack)
            rt_error("out of memory");
    }
    K_stack[K_sp++] = v;
}

static kv rt_pop(void)
{
    if (K_sp <= 0)
        rt_error("evaluation stack underflow (malformed program)");
    return K_stack[--K_sp];
}

RT_API void rt_push_int(int64_t i) { rt_push(kv_int(i)); }

/* A floating-point literal, as the bits of the double. */
RT_API void rt_push_flt(int64_t bits)
{
    double d;
    memcpy(&d, &bits, sizeof d);
    rt_push(kv_flt(d));
}

RT_API void rt_push_lit(const char *s, int64_t len) { rt_push(kv_str(s, (size_t)len)); }

RT_API void rt_drop(void) { kv_release(rt_pop()); }

static void rt_check_var(int idx)
{
    if (idx < 0 || idx >= K_nvars)
        rt_error("variable index %d out of range", idx);
}

RT_API void rt_load(int idx)
{
    rt_check_var(idx);
    rt_push(kv_retain(K_vars[idx]));
}

RT_API void rt_store(int idx)
{
    rt_check_var(idx);
    kv v = rt_pop();
    kv_release(K_vars[idx]);
    K_vars[idx] = v;
}

static void rt_set_var(int idx, kv v) /* takes ownership of v */
{
    rt_check_var(idx);
    kv_release(K_vars[idx]);
    K_vars[idx] = v;
}

/* ------------------------------------------------------------------ */
/* Operators                                                           */
/* ------------------------------------------------------------------ */

static kv concat(kv a, kv b)
{
    ktext ta, tb;
    kt_get(a, &ta);
    kt_get(b, &tb);
    kstr *k = rt_alloc(sizeof(kstr) + ta.len + tb.len + 1);
    k->rc = 1;
    k->len = ta.len + tb.len;
    memcpy(k->d, ta.s, ta.len);
    memcpy(k->d + ta.len, tb.s, tb.len);
    k->d[ta.len + tb.len] = '\0';
    kt_done(&ta);
    kt_done(&tb);
    kv v = {K_STR, 0, k, NULL, 0};
    return v;
}

RT_API void rt_add(void)
{
    kv b = rt_pop(), a = rt_pop();
    if (a.kind == K_INT && b.kind == K_INT)
        rt_push_int((int64_t)((uint64_t)a.i + (uint64_t)b.i));
    else if (is_num(a) && is_num(b))
        rt_push(kv_flt(kv_to_flt(a) + kv_to_flt(b)));
    else
        rt_push(concat(a, b)); /* loosely-typed "+", like VB6 */
    kv_release(a);
    kv_release(b);
}

RT_API void rt_concat(void)
{
    kv b = rt_pop(), a = rt_pop();
    rt_push(concat(a, b));
    kv_release(a);
    kv_release(b);
}

/* op: 0 -, 1 *, 2 / (exact: 7 / 2 is 3.5, 6 / 2 is 3), 3 \ (whole:
 * the operands rounded, the result truncated), 4 MOD. Whole numbers stay
 * whole; with a fraction anywhere the result is a double. */
RT_API void rt_arith(int op)
{
    kv b = rt_pop(), a = rt_pop();
    kv x = kv_num(a), y = kv_num(b);
    kv_release(a);
    kv_release(b);
    if (op == 3) {
        int64_t p = kv_to_int(x), q = kv_to_int(y);
        if (q == 0)
            rt_error("division by zero");
        rt_push_int(q == -1 ? (int64_t)(0 - (uint64_t)p) : p / q);
        return;
    }
    if (x.kind == K_INT && y.kind == K_INT) {
        int64_t p = x.i, q = y.i;
        switch (op) {
        case 0:
            rt_push_int((int64_t)((uint64_t)p - (uint64_t)q));
            return;
        case 1:
            rt_push_int((int64_t)((uint64_t)p * (uint64_t)q));
            return;
        case 2:
            if (q == 0)
                rt_error("division by zero");
            if (q == -1)
                rt_push_int((int64_t)(0 - (uint64_t)p));
            else if (p % q == 0)
                rt_push_int(p / q);
            else
                rt_push(kv_flt((double)p / (double)q));
            return;
        default:
            if (q == 0)
                rt_error("division by zero");
            rt_push_int(q == -1 ? 0 : p % q);
            return;
        }
    }
    double p = kv_to_flt(x), q = kv_to_flt(y);
    switch (op) {
    case 0:
        rt_push(kv_flt(p - q));
        break;
    case 1:
        rt_push(kv_flt(p * q));
        break;
    case 2:
        if (q == 0)
            rt_error("division by zero");
        rt_push(kv_flt(p / q));
        break;
    default:
        if (q == 0)
            rt_error("division by zero");
        rt_push(kv_flt(fmod(p, q)));
        break;
    }
}

RT_API void rt_neg(void)
{
    kv a = rt_pop();
    kv x = kv_num(a);
    kv_release(a);
    if (x.kind == K_INT)
        rt_push_int((int64_t)(0 - (uint64_t)x.i));
    else
        rt_push(kv_flt(-x.f));
}

RT_API void rt_not(void)
{
    kv a = rt_pop();
    int t = kv_truthy(a);
    kv_release(a);
    rt_push_int(!t);
}

/* op: 0 =, 1 <>, 2 <, 3 >, 4 <=, 5 >= */
RT_API void rt_cmp(int op)
{
    kv b = rt_pop(), a = rt_pop();
    int rel;
    if (a.kind == K_ARR || b.kind == K_ARR) {
        /* arrays and objects: identity, = and <> only */
        if (op > 1)
            rt_error("arrays and objects can only be compared with = and <>");
        int same = a.kind == K_ARR && b.kind == K_ARR && a.a == b.a;
        kv_release(a);
        kv_release(b);
        rt_push_int(op == 0 ? same : !same);
        return;
    }
    if (a.kind == K_INT && b.kind == K_INT)
        rel = a.i < b.i ? -1 : a.i > b.i;
    else if (is_num(a) && is_num(b)) {
        double x = kv_to_flt(a), y = kv_to_flt(b);
        rel = x < y ? -1 : x > y;
    } else {
        ktext ta, tb;
        kt_get(a, &ta);
        kt_get(b, &tb);
        int c = memcmp(ta.s, tb.s, ta.len < tb.len ? ta.len : tb.len);
        rel = c ? c : (ta.len < tb.len ? -1 : ta.len > tb.len);
        kt_done(&ta);
        kt_done(&tb);
    }
    kv_release(a);
    kv_release(b);
    int r = op == 0 ? rel == 0 : op == 1 ? rel != 0 : op == 2 ? rel < 0
          : op == 3 ? rel > 0 : op == 4 ? rel <= 0 : rel >= 0;
    rt_push_int(r);
}

RT_API int rt_pop_truthy(void)
{
    kv a = rt_pop();
    int t = kv_truthy(a);
    kv_release(a);
    return t;
}

/* ------------------------------------------------------------------ */
/* Statements                                                          */
/* ------------------------------------------------------------------ */

/* Pops n values into args[0..n-1], in push order. */
static kv *pop_args(int n)
{
    kv *args = rt_alloc(sizeof(kv) * (size_t)(n > 0 ? n : 1));
    for (int j = n - 1; j >= 0; j--)
        args[j] = rt_pop();
    return args;
}

static void release_args(kv *args, int n)
{
    for (int j = 0; j < n; j++)
        kv_release(args[j]);
    free(args);
}

static int64_t rt_pop_int(void)
{
    kv v = rt_pop();
    int64_t i = kv_to_int(v);
    kv_release(v);
    return i;
}


static uint64_t rnd_next(void)
{
    uint64_t z = (K_rnd += 0x9E3779B97F4A7C15ULL);
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
    return z ^ (z >> 31);
}

static int64_t K_col; /* output column, for QuickBASIC PRINT's "," and TAB */

/* Writes n bytes to stdout, keeping track of the column. */
static void kout(const char *s, size_t n)
{
    fwrite(s, 1, n, stdout);
    for (size_t j = 0; j < n; j++)
        K_col = s[j] == '\n' ? 0 : K_col + 1;
}

static void kspaces(int64_t n)
{
    for (; n > 0; n--)
        kout(" ", 1);
}

RT_API void rt_print(int n)
{
    kv *args = pop_args(n);
    for (int j = 0; j < n; j++) {
        ktext t;
        kt_get(args[j], &t);
        if (j > 0)
            fputc(' ', stdout);
        fwrite(t.s, 1, t.len, stdout);
        kt_done(&t);
    }
    fputc('\n', stdout);
    K_col = 0;
    release_args(args, n);
}

/* A piece of a QuickBASIC PRINT (QP_* in bytecodetypes.pas). */
RT_API void rt_qprint(int kind)
{
    switch (kind) {
    case 0:   /* QP_VALUE: numbers as " 5 " / "-5 " */
    case 5: { /* QP_TEXT */
        kv v = rt_pop();
        if (kind == 0 && v.kind == K_INT) {
            char buf[32];
            int len = snprintf(buf, sizeof buf, v.i >= 0 ? " %" PRId64 " " : "%" PRId64 " ", v.i);
            kout(buf, (size_t)len);
        } else if (kind == 0 && v.kind == K_FLT) {
            char buf[52];
            int len = 0;
            if (v.f >= 0)
                buf[len++] = ' ';
            len += fmt_flt(v.f, buf + len, 1);
            buf[len++] = ' ';
            kout(buf, (size_t)len);
        } else {
            ktext t;
            kt_get(v, &t);
            kout(t.s, t.len);
            kt_done(&t);
        }
        kv_release(v);
        break;
    }
    case 1: /* QP_COMMA */
        kspaces(14 - K_col % 14);
        break;
    case 2: /* QP_NEWLINE */
        kout("\n", 1);
        break;
    case 3: { /* QP_TAB */
        int64_t c = rt_pop_int() - 1;
        if (c < K_col)
            kout("\n", 1);
        kspaces(c - K_col);
        break;
    }
    case 4: /* QP_SPC */
        kspaces(rt_pop_int());
        break;
    }
}

/* INPUT: a line from stdin, without its line break ("" at the end). */
RT_API void rt_input(void)
{
    fflush(stdout);
    kbuf b = {0};
    int c;
    while ((c = getchar()) != EOF && c != '\n') {
        char ch = (char)c;
        kb_add(&b, &ch, 1);
    }
    if (b.len > 0 && b.d[b.len - 1] == '\r')
        b.len--;
    K_col = 0;
    rt_push(kv_str(b.d ? b.d : "", b.len));
    free(b.d);
}

/* Copy of a value's text, NUL-terminated, for C APIs. */
static char *kv_cstr(kv v)
{
    ktext t;
    kt_get(v, &t);
    char *c = rt_alloc(t.len + 1);
    memcpy(c, t.s, t.len);
    c[t.len] = '\0';
    kt_done(&t);
    return c;
}

/* PROCESS: run args[0] with the rest as arguments (no shell), capturing
 * stdout. Fails, like the VM's RunCommand, if it can't start or exits
 * non-zero. dest >= 0 stores the output there; otherwise it's printed. */
#if defined(KAYTE_NO_PROCESS)
RT_API void rt_process(int n, int dest)
{
    (void)dest;
    kv *args = pop_args(n);
    rt_error("PROCESS is not available on %s - tried \"%s\"", KAYTE_NO_PROCESS, kv_cstr(args[0]));
}
#elif defined(_WIN32)
/* Appends arg to cmd, quoted the way CommandLineToArgvW / the C runtime
 * split it again (backslashes only matter before a quote). */
static void win_quote(char **cmd, size_t *len, size_t *cap, const char *arg)
{
    size_t need = *len + strlen(arg) * 2 + 4;
    if (need > *cap) {
        *cap = need * 2;
        *cmd = realloc(*cmd, *cap);
        if (!*cmd)
            rt_error("out of memory");
    }
    char *o = *cmd + *len;
    if (*len)
        *o++ = ' ';
    *o++ = '"';
    for (const char *p = arg;; p++) {
        size_t slashes = 0;
        while (*p == '\\') {
            slashes++;
            p++;
        }
        if (!*p) {
            for (size_t k = 0; k < slashes * 2; k++)
                *o++ = '\\';
            break;
        }
        if (*p == '"') {
            for (size_t k = 0; k < slashes * 2 + 1; k++)
                *o++ = '\\';
        } else
            for (size_t k = 0; k < slashes; k++)
                *o++ = '\\';
        *o++ = *p;
    }
    *o++ = '"';
    *o = '\0';
    *len = (size_t)(o - *cmd);
}

RT_API void rt_process(int n, int dest)
{
    kv *args = pop_args(n);
    size_t len = 0, cap = 256;
    char *cmd = rt_alloc(cap);
    cmd[0] = '\0';
    for (int j = 0; j < n; j++) {
        char *a = kv_cstr(args[j]);
        win_quote(&cmd, &len, &cap, a);
        free(a);
    }
    char *name = kv_cstr(args[0]);

    SECURITY_ATTRIBUTES sa = {sizeof(sa), NULL, TRUE};
    HANDLE rd, wr;
    if (!CreatePipe(&rd, &wr, &sa, 0))
        rt_error("failed to run process \"%s\"", name);
    SetHandleInformation(rd, HANDLE_FLAG_INHERIT, 0);
    STARTUPINFOA si;
    PROCESS_INFORMATION pi;
    memset(&si, 0, sizeof(si));
    si.cb = sizeof(si);
    si.dwFlags = STARTF_USESTDHANDLES;
    si.hStdOutput = wr;
    si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
    si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    fflush(stdout);
    if (!CreateProcessA(NULL, cmd, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi))
        rt_error("failed to run process \"%s\"", name);
    CloseHandle(wr);

    size_t outLen = 0, outCap = 4096;
    char *out = rt_alloc(outCap);
    DWORD r;
    while (ReadFile(rd, out + outLen, (DWORD)(outCap - outLen), &r, NULL) && r > 0) {
        outLen += r;
        if (outLen == outCap) {
            outCap *= 2;
            out = realloc(out, outCap);
            if (!out)
                rt_error("out of memory");
        }
    }
    CloseHandle(rd);
    WaitForSingleObject(pi.hProcess, INFINITE);
    DWORD code = 1;
    GetExitCodeProcess(pi.hProcess, &code);
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
    if (code != 0)
        rt_error("failed to run process \"%s\"", name);

    if (dest >= 0)
        rt_set_var(dest, kv_str(out, outLen));
    else {
        fwrite(out, 1, outLen, stdout);
        fputc('\n', stdout);
    }
    free(out);
    free(cmd);
    free(name);
    release_args(args, n);
}
#else
RT_API void rt_process(int n, int dest)
{
    kv *args = pop_args(n);
    char **argv = rt_alloc(sizeof(char *) * (size_t)(n + 1));
    for (int j = 0; j < n; j++)
        argv[j] = kv_cstr(args[j]);
    argv[n] = NULL;

    int fd[2];
    if (pipe(fd) != 0)
        rt_error("failed to run process \"%s\"", argv[0]);
    fflush(stdout);
    pid_t pid = fork();
    if (pid == 0) {
        dup2(fd[1], STDOUT_FILENO);
        close(fd[0]);
        close(fd[1]);
        execvp(argv[0], argv);
        _exit(127);
    }
    close(fd[1]);
    if (pid < 0)
        rt_error("failed to run process \"%s\"", argv[0]);

    size_t len = 0, cap = 4096;
    char *out = rt_alloc(cap);
    ssize_t r;
    while ((r = read(fd[0], out + len, cap - len)) > 0) {
        len += (size_t)r;
        if (len == cap) {
            cap *= 2;
            out = realloc(out, cap);
            if (!out)
                rt_error("out of memory");
        }
    }
    close(fd[0]);
    int status = 0;
    waitpid(pid, &status, 0);
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0)
        rt_error("failed to run process \"%s\"", argv[0]);

    if (dest >= 0)
        rt_set_var(dest, kv_str(out, len));
    else {
        fwrite(out, 1, len, stdout);
        fputc('\n', stdout);
    }
    free(out);
    for (int j = 0; j < n; j++)
        free(argv[j]);
    free(argv);
    release_args(args, n);
}
#endif

/* ------------------------------------------------------------------ */
/* SUB calls                                                           */
/* ------------------------------------------------------------------ */

RT_API void rt_call(int ret, int argc)
{
    if (K_depth >= K_MAX_CALL_DEPTH)
        rt_error("call stack overflow (more than %d nested CALLs)", K_MAX_CALL_DEPTH);
    K_frames[K_depth].ret = ret;
    K_frames[K_depth].argc = argc;
    K_depth++;
}

RT_API void rt_enter(int nparams, const char *name)
{
    if (K_depth == 0)
        rt_error("SUB \"%s\" entered without a CALL", name);
    if (K_frames[K_depth - 1].argc != nparams)
        rt_error("SUB \"%s\" takes %d argument(s), got %d", name, nparams, K_frames[K_depth - 1].argc);
}

static void drop_stale_tries(void);

RT_API int rt_return(void)
{
    if (K_depth == 0)
        rt_error("RETURN without a GOSUB (or outside a SUB call)");
    int ret = K_frames[--K_depth].ret;
    drop_stale_tries(); /* TRYs started inside the call that's ending */
    return ret;
}

static int find_sub(const char *name)
{
    for (int i = 0; i < K_nsubs; i++)
        if (strcasecmp(K_sub_names[i], name) == 0)
            return i;
    return -1;
}

/* ------------------------------------------------------------------ */
/* Arrays                                                              */
/* ------------------------------------------------------------------ */

static karr *need_array(kv v, const char *what)
{
    if (v.kind != K_ARR) {
        ktext t;
        kt_get(v, &t);
        rt_error("%s needs an array, got \"%s\"", what, t.s);
    }
    return v.a;
}

static int64_t check_index(karr *a, kv idx)
{
    int64_t i = kv_to_int(idx);
    if (i < 0 || i >= a->len)
        rt_error("index %" PRId64 " is out of range (0 to %" PRId64 ")", i, a->len - 1);
    return i;
}

RT_API void rt_index_get(void)
{
    kv idx = rt_pop(), a = rt_pop();
    karr *arr = need_array(a, "indexing");
    int64_t i = check_index(arr, idx);
    rt_push(kv_retain(arr->items[i]));
    kv_release(idx);
    kv_release(a);
}

RT_API void rt_index_set(void)
{
    kv v = rt_pop(), idx = rt_pop(), a = rt_pop();
    karr *arr = need_array(a, "indexing");
    int64_t i = check_index(arr, idx);
    kv_release(arr->items[i]);
    arr->items[i] = v;
    kv_release(idx);
    kv_release(a);
}

/* DIM a(d1, d2, ...): d1 + 1 elements, each an array for the next
 * dimension (or 0 in the last). */
static kv new_array(const int64_t *dims, int n)
{
    if (dims[0] < -1)
        rt_error("an array's upper bound can't be %" PRId64, dims[0]);
    kv v = kv_arr(dims[0] + 1, NULL);
    if (n > 1)
        for (int64_t j = 0; j < v.a->len; j++)
            v.a->items[j] = new_array(dims + 1, n - 1);
    return v;
}

/* Built-in function numbers - the same as BI_* in source/bytecodetypes.pas. */
enum {
    BI_LEN = 1, BI_LEFT, BI_RIGHT, BI_MID, BI_UCASE, BI_LCASE, BI_TRIM, BI_LTRIM, BI_RTRIM, BI_INSTR,
    BI_REPLACE, BI_STR, BI_VAL, BI_CHR, BI_ASC, BI_SPACE, BI_ABS, BI_SGN, BI_MIN, BI_MAX, BI_UBOUND,
    BI_LBOUND, BI_ARRAY, BI_JOIN, BI_SPLIT, BI_TYPENAME, BI_ISARRAY, BI_NEWARRAY, BI_RESIZE, BI_NEWOBJECT,
    BI_ISNUMERIC, BI_CINT, BI_RND, BI_POW, BI_SQR, BI_STRING, BI_TIMER, BI_DATE, BI_TIME,
    BI_HEX, BI_OCT, BI_SLEEP, BI_INKEY, BI_QBSTR, BI_INT, BI_FIX, BI_CDBL, BI_ROUND, BI_SIN, BI_COS,
    BI_TAN, BI_ATN, BI_EXP, BI_LOG, BI_USING, BI_FOPEN, BI_FCLOSE, BI_FPRINT, BI_FREADLINE, BI_FREADFIELD,
    BI_EOF, BI_FREEFILE, BI_LOF, BI_KILL, BI_NAME, BI_FGET, BI_FPUT, BI_FSEEK, BI_FSEEKPOS, BI_FLOC,
    BI_FINPUTS, BI_MKI, BI_MKL, BI_MKS, BI_MKD, BI_CVI, BI_CVL, BI_CVS, BI_CVD, BI_BAND, BI_BOR, BI_BXOR,
    BI_BNOT, BI_ERRCODE, BI_ERRMSG, BI_MIDSET, BI_LSET, BI_RSET
};

/* QuickBASIC's error numbers and messages (ERR, ERROR n) - the VM has the
 * same table (QBErrors in virtualmachine.pas). */
static const struct {
    int code;
    const char *text;
} K_qb_errors[] = {
    {3, "RETURN without GOSUB"}, {4, "Out of DATA"}, {5, "Illegal function call"}, {6, "Overflow"},
    {7, "Out of memory"}, {9, "Subscript out of range"}, {10, "Duplicate definition"},
    {11, "Division by zero"}, {13, "Type mismatch"}, {14, "Out of string space"},
    {20, "RESUME without error"}, {52, "Bad file name or number"}, {53, "File not found"},
    {54, "Bad file mode"}, {55, "File already open"}, {57, "Device I/O error"},
    {58, "File already exists"}, {61, "Disk full"}, {62, "Input past end of file"},
    {63, "Bad record number"}, {64, "Bad file name"}, {67, "Too many files"},
    {70, "Permission denied"}, {75, "Path/File access error"}, {76, "Path not found"}};

/* Kayte's own runtime messages -> the QuickBASIC error number (checked in
 * order; anything else is 5, Illegal function call). */
static const struct {
    const char *part;
    int code;
} K_qb_patterns[] = {
    {"division by zero", 11}, {"overflow", 6}, {"out of range", 9}, {"out of data", 4},
    {"no such file", 53}, {"permission denied", 70}, {"input past the end", 62},
    {"isn't open", 52}, {"is open for", 54}, {"already open", 55}, {"bad record number", 63},
    {"too many open files", 67}, {"return without", 3}, {"out of memory", 7},
    {"cannot convert", 13}, {"got a string", 13}, {"got a number", 13}, {"needs an array", 13},
    {"cannot use", 13}, {"can't open", 75}};

static int ci_contains(const char *hay, size_t hlen, const char *needle)
{
    size_t n = strlen(needle);
    for (size_t i = 0; i + n <= hlen; i++) {
        size_t k = 0;
        while (k < n && tolower((unsigned char)hay[i + k]) == tolower((unsigned char)needle[k]))
            k++;
        if (k == n)
            return 1;
    }
    return 0;
}

static int qb_error_code(const char *m, size_t len)
{
    for (size_t j = 0; j < sizeof K_qb_errors / sizeof K_qb_errors[0]; j++)
        if (strlen(K_qb_errors[j].text) == len && ci_contains(m, len, K_qb_errors[j].text))
            return K_qb_errors[j].code;
    if (len > 6 && strncmp(m, "Error ", 6) == 0 && isdigit((unsigned char)m[6]))
        return atoi(m + 6); /* ERROR n with a number QuickBASIC has no message for */
    for (size_t j = 0; j < sizeof K_qb_patterns / sizeof K_qb_patterns[0]; j++)
        if (ci_contains(m, len, K_qb_patterns[j].part))
            return K_qb_patterns[j].code;
    return 5;
}

/* ------------------------------------------------------------------ */
/* PRINT USING (QuickBASIC) - mirrors FormatUsing in virtualmachine.pas */
/* ------------------------------------------------------------------ */

typedef struct {
    char type;        /* '!', '&', '\\' (fixed width) or 'N' (number) */
    size_t len;       /* characters of the format it takes */
    int width;        /* '\\': the text's width */
    int lead_plus, trail_plus, trail_minus, stars, dollar, comma, point;
    int before, decs; /* digit positions before / after the point */
    int expn;         /* 0, or the exponent's characters (^^^^ = 4) */
} kfield;

/* The field that starts at f[p], if one does. */
static int using_field(const char *f, size_t n, size_t p, kfield *fd)
{
    memset(fd, 0, sizeof *fd);
    char c = f[p], c1 = p + 1 < n ? f[p + 1] : 0;
    if (c == '!' || c == '&') {
        fd->type = c;
        fd->len = 1;
        return 1;
    }
    if (c == '\\') {
        size_t q = p + 1;
        while (q < n && f[q] == ' ')
            q++;
        if (q < n && f[q] == '\\') {
            fd->type = '\\';
            fd->len = q - p + 1;
            fd->width = (int)fd->len;
            return 1;
        }
        return 0;
    }
    if (!(c == '#' || (c == '.' && c1 == '#') || (c == '+' && (c1 == '#' || c1 == '.' || c1 == '$' || c1 == '*')) ||
          (c == '*' && c1 == '*') || (c == '$' && c1 == '$')))
        return 0;
    size_t i = p;
    fd->type = 'N';
    if (f[i] == '+') {
        fd->lead_plus = 1;
        i++;
    }
    if (i + 2 < n && f[i] == '*' && f[i + 1] == '*' && f[i + 2] == '$') {
        fd->stars = fd->dollar = 1;
        fd->before += 2;
        i += 3;
    } else if (i + 1 < n && f[i] == '*' && f[i + 1] == '*') {
        fd->stars = 1;
        fd->before += 2;
        i += 2;
    } else if (i + 1 < n && f[i] == '$' && f[i + 1] == '$') {
        fd->dollar = 1;
        fd->before += 1;
        i += 2;
    }
    for (; i < n && (f[i] == '#' || f[i] == ','); i++) {
        if (f[i] == '#')
            fd->before++;
        else
            fd->comma = 1;
    }
    if (i < n && f[i] == '.') {
        fd->point = 1;
        for (i++; i < n && f[i] == '#'; i++)
            fd->decs++;
    }
    if (i + 3 < n && memcmp(f + i, "^^^^", 4) == 0) {
        fd->expn = i + 4 < n && f[i + 4] == '^' ? 5 : 4;
        i += (size_t)fd->expn;
    }
    if (i < n && f[i] == '+' && !fd->lead_plus) {
        fd->trail_plus = 1;
        i++;
    } else if (i < n && f[i] == '-') {
        fd->trail_minus = 1;
        i++;
    }
    fd->len = i - p;
    return 1;
}

/* "%.*f" of a >= 0 into an integer part and a fraction (C locale digits). */
static void fixed_parts(double a, int decs, char *ip, char *fp)
{
    char buf[400];
    snprintf(buf, sizeof buf, "%.*f", decs, a);
    char *d = buf;
    while (*d && *d >= '0' && *d <= '9')
        d++;
    size_t il = (size_t)(d - buf);
    memcpy(ip, buf, il);
    ip[il] = '\0';
    fp[0] = '\0';
    if (*d) /* the decimal point, whatever the locale makes it */
        strcpy(fp, d + 1);
}

static void using_number(kbuf *b, const kfield *fd, double v)
{
    char ip[400], fp[400], text[1100], body[1050]; /* grouped (600) + fp (400) + signs */
    int neg = v < 0;
    double a = fabs(v);
    size_t k = 0;
    if (fd->expn) {
        int intd = fd->before - (fd->lead_plus || fd->trail_plus || fd->trail_minus ? 0 : 1);
        int e = 0;
        if (intd < 0)
            intd = 0;
        if (a != 0)
            e = (int)floor(log10(a)) - (intd - 1);
        for (int tries = 0; tries < 2; tries++) {
            fixed_parts(a == 0 ? 0 : a / pow(10, e), fd->decs, ip, fp);
            if ((int)strlen(ip) > (intd > 0 ? intd : 1) || (intd == 0 && strcmp(ip, "0") != 0))
                e++; /* rounding carried into another digit */
            else
                break;
        }
        if (intd == 0)
            ip[0] = '\0';
        int ed = fd->expn - 2;
        snprintf(body, sizeof body, "%s%s%s%sE%c%0*d", fd->dollar ? "$" : "", ip, fd->point ? "." : "", fp,
                 e < 0 ? '-' : '+', ed, e < 0 ? -e : e);
    } else {
        fixed_parts(a, fd->decs, ip, fp);
        if (fd->before == 0 && strcmp(ip, "0") == 0)
            ip[0] = '\0';
        char grouped[600];
        size_t il = strlen(ip), g = 0;
        for (size_t j = 0; j < il; j++) {
            if (fd->comma && j > 0 && (il - j) % 3 == 0)
                grouped[g++] = ',';
            grouped[g++] = ip[j];
        }
        grouped[g] = '\0';
        snprintf(body, sizeof body, "%s%s%s%s", fd->dollar ? "$" : "", grouped, fd->point ? "." : "", fp);
        if (neg && strspn(body, "$0.,") == strlen(body)) /* -0.00 shows as 0.00 */
            neg = 0;
    }
    if (fd->lead_plus)
        text[k++] = neg ? '-' : '+';
    else if (neg && !fd->trail_plus && !fd->trail_minus)
        text[k++] = '-';
    strcpy(text + k, body);
    k = strlen(text);
    if (fd->trail_plus)
        text[k++] = neg ? '-' : '+';
    else if (fd->trail_minus)
        text[k++] = neg ? '-' : ' ';
    text[k] = '\0';
    if (k > fd->len) {
        kb_add(b, "%", 1); /* too wide for the field */
        kb_add(b, text, k);
    } else {
        for (size_t j = k; j < fd->len; j++)
            kb_add(b, fd->stars ? "*" : " ", 1);
        kb_add(b, text, k);
    }
}

/* PRINT USING fmt; items: the formatted text. The format is used again
 * from the start when there are more items than fields. */
static kv using_format(kv *a, int n)
{
    ktext ft;
    kt_get(a[0], &ft);
    const char *f = ft.s;
    size_t len = ft.len, pos = 0;
    kfield fd;
    int any = 0;
    for (size_t q = 0; q < len && !any; q++)
        if (f[q] == '_')
            q++;
        else
            any = using_field(f, len, q, &fd);
    if (!any)
        rt_error("PRINT USING: the format has no field (#, !, &, \\ \\)");
    kbuf b = {NULL, 0, 0};
    kb_add(&b, "", 0);
    for (int j = 1; j < n; j++) {
        for (;;) {
            if (pos >= len)
                pos = 0;
            if (f[pos] == '_' && pos + 1 < len) {
                kb_add(&b, f + pos + 1, 1);
                pos += 2;
                continue;
            }
            if (using_field(f, len, pos, &fd))
                break;
            kb_add(&b, f + pos, 1);
            pos++;
        }
        pos += fd.len;
        if (fd.type == 'N') {
            if (!is_num(a[j]))
                rt_error("PRINT USING: a number field (#) got a string");
            using_number(&b, &fd, kv_to_flt(a[j]));
        } else {
            if (a[j].kind != K_STR)
                rt_error("PRINT USING: a string field (! & \\ \\) got a number");
            const char *t = a[j].s->d;
            size_t tl = a[j].s->len;
            if (fd.type == '!')
                kb_add(&b, tl ? t : " ", 1);
            else if (fd.type == '&')
                kb_add(&b, t, tl);
            else
                for (int c = 0; c < fd.width; c++)
                    kb_add(&b, (size_t)c < tl ? t + c : " ", 1);
        }
    }
    /* the literal text after the last field */
    while (pos < len && !using_field(f, len, pos, &fd)) {
        if (f[pos] == '_' && pos + 1 < len) {
            kb_add(&b, f + pos + 1, 1);
            pos += 2;
        } else
            kb_add(&b, f + pos++, 1);
    }
    kt_done(&ft);
    kv r = kv_str(b.d, b.len);
    free(b.d);
    return r;
}

/* ------------------------------------------------------------------ */
/* Files (QuickBASIC OPEN / PRINT # / INPUT # ...) - mirrors the VM    */
/* ------------------------------------------------------------------ */

#define K_MAX_FILES 255
static FILE *K_files[K_MAX_FILES + 1];
static int K_fmode[K_MAX_FILES + 1];    /* 0 INPUT, 1 OUTPUT, 2 APPEND, 3 BINARY, 4 RANDOM */
static int64_t K_fcol[K_MAX_FILES + 1]; /* output column, for "," and TAB */
static int64_t K_flen[K_MAX_FILES + 1]; /* RANDOM: the record length */
static int64_t K_frec[K_MAX_FILES + 1]; /* RANDOM: the last record read or written */

static const char *mode_name(int mode)
{
    static const char *names[] = {"INPUT", "OUTPUT", "APPEND", "BINARY", "RANDOM"};
    return names[mode];
}

static void need_open(int64_t num)
{
    if (num < 1 || num > K_MAX_FILES || !K_files[num])
        rt_error("file #%" PRId64 " isn't open", num);
}

/* A text file, for reading (input) or writing. */
static FILE *need_file(int64_t num, int input)
{
    need_open(num);
    int m = K_fmode[num];
    if (m >= 3 || input != (m == 0))
        rt_error("file #%" PRId64 " is open for %s", num, mode_name(m));
    return K_files[num];
}

static void fout(int64_t num, const char *s, size_t n)
{
    FILE *fh = need_file(num, 0);
    fwrite(s, 1, n, fh);
    for (size_t j = 0; j < n; j++)
        K_fcol[num] = s[j] == '\n' ? 0 : K_fcol[num] + 1;
}

/* PRINT # piece: QP_* kind, value (or 0). */
static void file_print(int64_t num, int kind, kv v)
{
    char buf[64];
    switch (kind) {
    case 0: /* QP_VALUE */
        if (v.kind == K_INT || v.kind == K_FLT) {
            int len = 0;
            if (v.kind == K_INT)
                len = snprintf(buf, sizeof buf, v.i >= 0 ? " %" PRId64 " " : "%" PRId64 " ", v.i);
            else {
                if (v.f >= 0)
                    buf[len++] = ' ';
                len += fmt_flt(v.f, buf + len, 1);
                buf[len++] = ' ';
            }
            fout(num, buf, (size_t)len);
            break;
        }
        /* fall through - as text */
    case 5: { /* QP_TEXT */
        ktext t;
        kt_get(v, &t);
        fout(num, t.s, t.len);
        kt_done(&t);
        break;
    }
    case 1: { /* QP_COMMA */
        need_file(num, 0);
        int64_t sp = 14 - K_fcol[num] % 14;
        while (sp-- > 0)
            fout(num, " ", 1);
        break;
    }
    case 2: /* QP_NEWLINE */
        fout(num, "\n", 1);
        break;
    case 3: { /* QP_TAB */
        need_file(num, 0);
        int64_t c = kv_to_int(v) - 1;
        if (c < K_fcol[num])
            fout(num, "\n", 1);
        while (K_fcol[num] < c)
            fout(num, " ", 1);
        break;
    }
    case 4: { /* QP_SPC */
        int64_t c = kv_to_int(v);
        while (c-- > 0)
            fout(num, " ", 1);
        break;
    }
    }
}

static int fpeek(FILE *fh)
{
    int c = fgetc(fh);
    if (c != EOF)
        ungetc(c, fh);
    return c;
}

/* LINE INPUT #: the next line, without its line break. */
static kv file_line(int64_t num)
{
    FILE *fh = need_file(num, 1);
    if (fpeek(fh) == EOF)
        rt_error("input past the end of file #%" PRId64, num);
    kbuf b = {NULL, 0, 0};
    kb_add(&b, "", 0);
    int c;
    while ((c = fgetc(fh)) != EOF && c != '\n') {
        char ch = (char)c;
        kb_add(&b, &ch, 1);
    }
    if (b.len > 0 && b.d[b.len - 1] == '\r')
        b.len--;
    kv r = kv_str(b.d, b.len);
    free(b.d);
    return r;
}

/* INPUT #: the next comma- or line-separated field ("quoted" or not). */
static kv file_field(int64_t num)
{
    FILE *fh = need_file(num, 1);
    int c;
    while ((c = fpeek(fh)) == ' ' || c == '\t' || c == '\r' || c == '\n')
        fgetc(fh);
    if (c == EOF)
        rt_error("input past the end of file #%" PRId64, num);
    kbuf b = {NULL, 0, 0};
    kb_add(&b, "", 0);
    if (c == '"') {
        fgetc(fh);
        while ((c = fgetc(fh)) != EOF && c != '"') {
            char ch = (char)c;
            kb_add(&b, &ch, 1);
        }
        while ((c = fpeek(fh)) == ' ' || c == '\t' || c == '\r')
            fgetc(fh);
        if (c == ',' || c == '\n')
            fgetc(fh);
    } else {
        while ((c = fgetc(fh)) != EOF && c != ',' && c != '\n') {
            char ch = (char)c;
            kb_add(&b, &ch, 1);
        }
        while (b.len > 0 && (b.d[b.len - 1] == ' ' || b.d[b.len - 1] == '\t' || b.d[b.len - 1] == '\r'))
            b.len--;
    }
    kv r = kv_str(b.d, b.len);
    free(b.d);
    return r;
}

/* ---- BINARY / RANDOM: GET and PUT --------------------------------- */

/* A GET / PUT layout, as the compiler writes it: comma-separated codes -
 * I2 I4 I8 (little-endian whole numbers), F4 F8 (IEEE floats), S<n> (n
 * bytes of text, padded with spaces), V (a string, its own length), ?
 * (by the value: whole -> I8, fraction -> F8, string -> its bytes) - and
 * {...} for a TYPE record's fields in order. Mirrors the VM. */
typedef struct {
    const char *s;
    size_t p;
} klay;

static int lay_code(klay *L, char *code, int64_t *size)
{
    while (L->s[L->p] == ',')
        L->p++;
    *code = L->s[L->p];
    if (!*code || *code == '}')
        return 0;
    L->p++;
    *size = 0;
    while (L->s[L->p] >= '0' && L->s[L->p] <= '9')
        *size = *size * 10 + (L->s[L->p++] - '0');
    return 1;
}

static void put_le(kbuf *b, uint64_t v, int nbytes)
{
    for (int k = 0; k < nbytes; k++) {
        char c = (char)(v >> (8 * k));
        kb_add(b, &c, 1);
    }
}

static uint64_t get_le(const unsigned char *d, int nbytes)
{
    uint64_t v = 0;
    for (int k = 0; k < nbytes; k++)
        v |= (uint64_t)d[k] << (8 * k);
    return v;
}

static void put_whole(kbuf *b, kv v, int nbytes)
{
    int64_t i = kv_to_int(v);
    if (nbytes < 8) {
        int64_t lim = (int64_t)1 << (8 * nbytes - 1);
        if (i < -lim || i >= lim)
            rt_error("overflow: %" PRId64 " doesn't fit in %d bytes", i, nbytes);
    }
    put_le(b, (uint64_t)i, nbytes);
}

static void put_float(kbuf *b, double d, int nbytes)
{
    if (nbytes == 4) {
        if (fabs(d) > 3.4028234663852886e38)
            rt_error("overflow: too large for a SINGLE");
        float f = (float)d;
        uint32_t u;
        memcpy(&u, &f, 4);
        put_le(b, u, 4);
    } else {
        uint64_t u;
        memcpy(&u, &d, 8);
        put_le(b, u, 8);
    }
}

static void put_value(kbuf *b, klay *L, kv v)
{
    char code;
    int64_t size;
    if (!lay_code(L, &code, &size))
        rt_error("internal error: bad GET / PUT layout");
    switch (code) {
    case '{': {
        if (v.kind != K_ARR)
            rt_error("GET / PUT: a TYPE variable holds no record");
        int64_t k = 0;
        while (L->s[L->p] && L->s[L->p] != '}') {
            if (L->s[L->p] == ',') {
                L->p++;
                continue;
            }
            if (k >= v.a->len)
                rt_error("internal error: the record has fewer fields than its TYPE");
            put_value(b, L, v.a->items[k++]);
        }
        L->p++; /* } */
        break;
    }
    case 'I':
        put_whole(b, v, (int)size);
        break;
    case 'F':
        put_float(b, kv_to_flt(v), (int)size);
        break;
    case 'S':
    case 'V':
    case '?': {
        if (code == '?' && v.kind == K_INT) {
            put_le(b, (uint64_t)v.i, 8);
            break;
        }
        if (code == '?' && v.kind == K_FLT) {
            put_float(b, v.f, 8);
            break;
        }
        ktext t;
        kt_get(v, &t);
        if (code == 'S') {
            for (int64_t k = 0; k < size; k++)
                kb_add(b, (size_t)k < t.len ? t.s + k : " ", 1);
        } else
            kb_add(b, t.s, t.len);
        kt_done(&t);
        break;
    }
    default:
        rt_error("internal error: bad GET / PUT layout");
    }
}

/* How many bytes the layout takes for the value cur. */
static int64_t lay_size(klay *L, kv cur)
{
    char code;
    int64_t size, total = 0;
    if (!lay_code(L, &code, &size))
        return 0;
    switch (code) {
    case '{': {
        int64_t k = 0;
        while (L->s[L->p] && L->s[L->p] != '}') {
            if (L->s[L->p] == ',') {
                L->p++;
                continue;
            }
            kv item = cur.kind == K_ARR && k < cur.a->len ? cur.a->items[k] : kv_int(0);
            k++;
            total += lay_size(L, item);
        }
        L->p++;
        return total;
    }
    case 'V':
        return cur.kind == K_STR ? (int64_t)cur.s->len : 0;
    case '?':
        return cur.kind == K_STR ? (int64_t)cur.s->len : 8;
    default:
        return size;
    }
}

/* A float read from 4 bytes, rounded to a SINGLE's 7 digits (3.14, not
 * 3.14000010490417). */
static kv single_value(float f)
{
    char buf[32];
    kv r;
    snprintf(buf, sizeof buf, "%.7g", (double)f);
    if (!str_to_num(buf, &r))
        r = kv_flt((double)f);
    return r;
}

/* The value the layout reads from d (cur: the variable's value; a record
 * is filled in place). */
static kv get_value(const unsigned char *d, size_t *off, klay *L, kv cur)
{
    char code;
    int64_t size;
    if (!lay_code(L, &code, &size))
        rt_error("internal error: bad GET / PUT layout");
    switch (code) {
    case '{': {
        if (cur.kind != K_ARR)
            rt_error("GET / PUT: a TYPE variable holds no record");
        int64_t k = 0;
        while (L->s[L->p] && L->s[L->p] != '}') {
            if (L->s[L->p] == ',') {
                L->p++;
                continue;
            }
            kv nv = get_value(d, off, L, cur.a->items[k]);
            kv_release(cur.a->items[k]);
            cur.a->items[k++] = nv;
        }
        L->p++;
        return kv_retain(cur);
    }
    case 'I': {
        uint64_t u = get_le(d + *off, (int)size);
        *off += (size_t)size;
        if (size < 8 && (u >> (8 * size - 1)) & 1) /* sign-extend */
            u |= ~(uint64_t)0 << (8 * size);
        return kv_int((int64_t)u);
    }
    case 'F': {
        uint64_t u = get_le(d + *off, (int)size);
        *off += (size_t)size;
        if (size == 4) {
            uint32_t w = (uint32_t)u;
            float f;
            memcpy(&f, &w, 4);
            return single_value(f);
        }
        double x;
        memcpy(&x, &u, 8);
        if (!isfinite(x))
            rt_error("GET: not a valid DOUBLE in the file");
        return kv_flt(x);
    }
    default: { /* S, V, ? */
        int64_t len = code == 'S' ? size : code == 'V' ? (cur.kind == K_STR ? (int64_t)cur.s->len : 0)
                                     : (cur.kind == K_STR ? (int64_t)cur.s->len : 8);
        if (code == '?' && cur.kind != K_STR) {
            uint64_t u = get_le(d + *off, 8);
            *off += 8;
            if (cur.kind == K_FLT) {
                double x;
                memcpy(&x, &u, 8);
                if (!isfinite(x))
                    rt_error("GET: not a valid DOUBLE in the file");
                return kv_flt(x);
            }
            return kv_int((int64_t)u);
        }
        kv r = kv_str((const char *)d + *off, (size_t)len);
        *off += (size_t)len;
        return r;
    }
    }
}

/* Where a GET / PUT goes: RANDOM - record pos (or the next one), BINARY -
 * byte pos (or the current position). */
static FILE *bin_seek(int64_t num, int64_t pos, int64_t *reclen)
{
    need_open(num);
    int m = K_fmode[num];
    FILE *fh = K_files[num];
    if (m < 3)
        rt_error("GET / PUT need a BINARY or RANDOM file (file #%" PRId64 " is open for %s)", num, mode_name(m));
    *reclen = m == 4 ? K_flen[num] : 0;
    if (m == 4) {
        int64_t rec = pos < 0 ? K_frec[num] + 1 : pos;
        if (rec < 1)
            rt_error("bad record number %" PRId64, rec);
        K_frec[num] = rec;
        fseek(fh, (long)((rec - 1) * K_flen[num]), SEEK_SET);
    } else if (pos >= 0) {
        if (pos < 1)
            rt_error("bad file position %" PRId64, pos);
        fseek(fh, (long)(pos - 1), SEEK_SET);
    } else
        fseek(fh, 0, SEEK_CUR); /* between a read and a write */
    return fh;
}

static void file_put(int64_t num, int64_t pos, kv v, const char *layout)
{
    int64_t reclen;
    FILE *fh = bin_seek(num, pos, &reclen);
    kbuf b = {NULL, 0, 0};
    kb_add(&b, "", 0);
    klay L = {layout, 0};
    put_value(&b, &L, v);
    if (reclen && (int64_t)b.len > reclen) {
        free(b.d);
        rt_error("PUT: the record (%" PRId64 " bytes) is longer than the file's LEN = %" PRId64, (int64_t)b.len, reclen);
    }
    fwrite(b.d, 1, b.len, fh);
    free(b.d);
}

static kv file_get(int64_t num, int64_t pos, kv cur, const char *layout)
{
    int64_t reclen;
    FILE *fh = bin_seek(num, pos, &reclen);
    klay L = {layout, 0};
    int64_t size = lay_size(&L, cur);
    if (reclen && size > reclen)
        rt_error("GET: the record (%" PRId64 " bytes) is longer than the file's LEN = %" PRId64, size, reclen);
    unsigned char *d = calloc((size_t)size + 1, 1); /* past the end: zeros */
    if (!d)
        rt_error("out of memory");
    size_t got = fread(d, 1, (size_t)size, fh);
    (void)got;
    size_t off = 0;
    L.p = 0;
    kv r = get_value(d, &off, &L, cur);
    free(d);
    return r;
}

/* MKI$ / MKL$ / MKS$ / MKD$ and CVI / CVL / CVS / CVD. */
static kv mk_cv(int id, kv v)
{
    static const char *codes[] = {"I2", "I4", "F4", "F8"};
    int which = id >= BI_CVI ? id - BI_CVI : id - BI_MKI;
    klay L = {codes[which], 0};
    if (id < BI_CVI) {
        kbuf b = {NULL, 0, 0};
        kb_add(&b, "", 0);
        put_value(&b, &L, v);
        kv r = kv_str(b.d, b.len);
        free(b.d);
        return r;
    }
    static const int sizes[] = {2, 4, 4, 8};
    ktext t;
    kt_get(v, &t);
    if ((int)t.len < sizes[which]) {
        kt_done(&t);
        rt_error("CV%c needs a string of at least %d bytes", "ILSD"[which], sizes[which]);
    }
    size_t off = 0;
    kv r = get_value((const unsigned char *)t.s, &off, &L, kv_int(0));
    kt_done(&t);
    return r;
}

static kv file_builtin(int id, kv *a, int n)
{
    switch (id) {
    case BI_FOPEN: { /* name, mode, number */
        int64_t num = kv_to_int(a[2]), mode = kv_to_int(a[1]);
        if (num < 1 || num > K_MAX_FILES)
            rt_error("file numbers are 1 to %d", K_MAX_FILES);
        if (K_files[num])
            rt_error("file #%" PRId64 " is already open", num);
        int64_t reclen = n > 3 ? kv_to_int(a[3]) : 128;
        if (mode == 4 && (reclen < 1 || reclen > 32767))
            rt_error("OPEN ... LEN: the record length must be 1 to 32767");
        char *name = kv_cstr(a[0]);
        FILE *fh;
        if (mode >= 3) { /* BINARY / RANDOM: read and write, created if missing */
            fh = fopen(name, "r+b");
            if (!fh)
                fh = fopen(name, "w+b");
        } else
            fh = fopen(name, mode == 0 ? "rb" : mode == 1 ? "wb" : "ab");
        if (!fh) {
            const char *why = strerror(errno);
            char msg[600];
            snprintf(msg, sizeof msg, "can't open \"%s\": %s", name, why);
            free(name);
            rt_error("%s", msg);
        }
        free(name);
        K_files[num] = fh;
        K_fmode[num] = (int)mode;
        K_fcol[num] = 0;
        K_flen[num] = reclen;
        K_frec[num] = 0;
        return kv_int(0);
    }
    case BI_FCLOSE: { /* number, or 0: all */
        int64_t num = kv_to_int(a[0]);
        for (int64_t j = 1; j <= K_MAX_FILES; j++)
            if ((num == 0 || j == num) && K_files[j]) {
                fclose(K_files[j]);
                K_files[j] = NULL;
            }
        return kv_int(0);
    }
    case BI_FPRINT:
        file_print(kv_to_int(a[0]), (int)kv_to_int(a[1]), n > 2 ? a[2] : kv_int(0));
        return kv_int(0);
    case BI_FREADLINE:
        return file_line(kv_to_int(a[0]));
    case BI_FREADFIELD:
        return file_field(kv_to_int(a[0]));
    case BI_EOF: {
        int64_t num = kv_to_int(a[0]);
        if (num < 1 || num > K_MAX_FILES || !K_files[num])
            rt_error("file #%" PRId64 " isn't open", num);
        if (K_fmode[num] >= 3) { /* past the last byte / the last record read */
            FILE *fh = K_files[num];
            long here = ftell(fh);
            fseek(fh, 0, SEEK_END);
            long size = ftell(fh);
            fseek(fh, here, SEEK_SET);
            if (K_fmode[num] == 4)
                return kv_int(K_frec[num] * K_flen[num] >= size);
            return kv_int(here >= size);
        }
        return kv_int(K_fmode[num] != 0 || fpeek(K_files[num]) == EOF);
    }
    case BI_FREEFILE:
        for (int j = 1; j <= K_MAX_FILES; j++)
            if (!K_files[j])
                return kv_int(j);
        rt_error("too many open files");
    case BI_LOF: {
        int64_t num = kv_to_int(a[0]);
        if (num < 1 || num > K_MAX_FILES || !K_files[num])
            rt_error("file #%" PRId64 " isn't open", num);
        FILE *fh = K_files[num];
        fflush(fh);
        long here = ftell(fh);
        fseek(fh, 0, SEEK_END);
        long size = ftell(fh);
        fseek(fh, here, SEEK_SET);
        return kv_int(size);
    }
    case BI_FGET: /* number, position (-1: next), the variable's value, layout */
        return file_get(kv_to_int(a[0]), kv_to_int(a[1]), a[2], a[3].s->d);
    case BI_FPUT:
        file_put(kv_to_int(a[0]), kv_to_int(a[1]), a[2], a[3].s->d);
        return kv_int(0);
    case BI_FSEEK: { /* SEEK #n, position */
        int64_t num = kv_to_int(a[0]), pos = kv_to_int(a[1]);
        need_open(num);
        if (pos < 1)
            rt_error("bad file position %" PRId64, pos);
        if (K_fmode[num] == 4) {
            K_frec[num] = pos - 1;
            fseek(K_files[num], (long)((pos - 1) * K_flen[num]), SEEK_SET);
        } else
            fseek(K_files[num], (long)(pos - 1), SEEK_SET);
        return kv_int(0);
    }
    case BI_FSEEKPOS: /* SEEK(n): the next record / byte */
    case BI_FLOC: {   /* LOC(n): the last record / the current byte */
        int64_t num = kv_to_int(a[0]);
        need_open(num);
        if (K_fmode[num] == 4)
            return kv_int(K_frec[num] + (id == BI_FSEEKPOS));
        return kv_int(ftell(K_files[num]) + (id == BI_FSEEKPOS));
    }
    case BI_FINPUTS: { /* INPUT$(count, #n) */
        int64_t cnt = kv_to_int(a[0]), num = kv_to_int(a[1]);
        need_open(num);
        if (K_fmode[num] == 1 || K_fmode[num] == 2)
            rt_error("file #%" PRId64 " is open for %s", num, mode_name(K_fmode[num]));
        if (cnt < 0)
            rt_error("INPUT$: the count can't be negative");
        char *d = rt_alloc((size_t)cnt + 1);
        size_t got = fread(d, 1, (size_t)cnt, K_files[num]);
        if ((int64_t)got < cnt) {
            free(d);
            rt_error("input past the end of file #%" PRId64, num);
        }
        kv r = kv_str(d, got);
        free(d);
        return r;
    }
    case BI_MKI: case BI_MKL: case BI_MKS: case BI_MKD:
    case BI_CVI: case BI_CVL: case BI_CVS: case BI_CVD:
        return mk_cv(id, a[0]);
    case BI_KILL:
    case BI_NAME: {
        char *p = kv_cstr(a[0]), *q = n > 1 ? kv_cstr(a[1]) : NULL;
        int bad = id == BI_KILL ? remove(p) : rename(p, q);
        if (bad) {
            char msg[600];
            snprintf(msg, sizeof msg, "%s \"%s\": %s", id == BI_KILL ? "can't delete" : "can't rename", p, strerror(errno));
            free(p);
            free(q);
            rt_error("%s", msg);
        }
        free(p);
        free(q);
        return kv_int(0);
    }
    }
    return kv_int(0);
}

/* -1 / 0 / 1: how two numbers compare. */
static int num_cmp(kv a, kv b)
{
    if (a.kind == K_INT && b.kind == K_INT)
        return a.i < b.i ? -1 : a.i > b.i;
    double x = kv_to_flt(a), y = kv_to_flt(b);
    return x < y ? -1 : x > y;
}

static int64_t nonneg(kv v, const char *name)
{
    int64_t n = kv_to_int(v);
    if (n < 0)
        rt_error("%s: the length can't be negative", name);
    return n;
}

/* A string value from n bytes at s. */
static kv sub_str(const char *s, int64_t n) { return kv_str(s, (size_t)(n > 0 ? n : 0)); }

/* Built-in functions: n arguments on the stack, result pushed. Mirrors
 * TVirtualMachine.CallBuiltin - strings are byte strings, positions
 * 1-based, as in VB. */
RT_API void rt_builtin(int id, int n)
{
    kv *a = pop_args(n);
    kv r = kv_int(0);
    ktext t, u, w;
    switch (id) {
    case BI_LEN:
        if (a[0].kind == K_ARR)
            r = kv_int(a[0].a->len);
        else {
            kt_get(a[0], &t);
            r = kv_int((int64_t)t.len);
            kt_done(&t);
        }
        break;
    case BI_LEFT: case BI_RIGHT: {
        int64_t k = nonneg(a[1], id == BI_LEFT ? "LEFT" : "RIGHT");
        kt_get(a[0], &t);
        if (k > (int64_t)t.len)
            k = (int64_t)t.len;
        r = sub_str(id == BI_LEFT ? t.s : t.s + t.len - k, k);
        kt_done(&t);
        break;
    }
    case BI_MID: {
        int64_t st = kv_to_int(a[1]);
        if (st < 1)
            rt_error("MID: the start position must be 1 or more");
        int64_t k = n > 2 ? nonneg(a[2], "MID") : INT64_MAX;
        kt_get(a[0], &t);
        if (st > (int64_t)t.len)
            r = sub_str("", 0);
        else {
            int64_t rest = (int64_t)t.len - (st - 1);
            r = sub_str(t.s + st - 1, k < rest ? k : rest);
        }
        kt_done(&t);
        break;
    }
    case BI_UCASE: case BI_LCASE: {
        char *c = kv_cstr(a[0]);
        for (char *p = c; *p; p++)
            if (id == BI_UCASE ? (*p >= 'a' && *p <= 'z') : (*p >= 'A' && *p <= 'Z'))
                *p = (char)(*p ^ 0x20);
        r = kv_str(c, strlen(c));
        free(c);
        break;
    }
    case BI_TRIM: case BI_LTRIM: case BI_RTRIM: {
        kt_get(a[0], &t);
        size_t st = 0, en = t.len;
        if (id != BI_RTRIM)
            while (st < en && t.s[st] == ' ')
                st++;
        if (id != BI_LTRIM)
            while (en > st && t.s[en - 1] == ' ')
                en--;
        r = sub_str(t.s + st, (int64_t)(en - st));
        kt_done(&t);
        break;
    }
    case BI_INSTR: {
        int64_t st = 1;
        kv sv = a[0], fv = a[1];
        if (n == 3) {
            st = kv_to_int(a[0]);
            sv = a[1];
            fv = a[2];
        }
        if (st < 1)
            rt_error("INSTR: the start position must be 1 or more");
        kt_get(sv, &t);
        kt_get(fv, &u);
        if (u.len == 0)
            r = kv_int(st <= (int64_t)t.len + 1 ? st : 0);
        else {
            int64_t found = 0;
            for (int64_t p = st - 1; p + (int64_t)u.len <= (int64_t)t.len; p++)
                if (memcmp(t.s + p, u.s, u.len) == 0) {
                    found = p + 1;
                    break;
                }
            r = kv_int(found);
        }
        kt_done(&t);
        kt_done(&u);
        break;
    }
    case BI_REPLACE: {
        kt_get(a[0], &t);
        kt_get(a[1], &u);
        kt_get(a[2], &w);
        if (u.len == 0)
            r = sub_str(t.s, (int64_t)t.len);
        else {
            kbuf b = {NULL, 0, 0};
            kb_add(&b, "", 0);
            size_t p = 0;
            while (p < t.len) {
                if (p + u.len <= t.len && memcmp(t.s + p, u.s, u.len) == 0) {
                    kb_add(&b, w.s, w.len);
                    p += u.len;
                } else
                    kb_add(&b, t.s + p++, 1);
            }
            r = sub_str(b.d, (int64_t)b.len);
            free(b.d);
        }
        kt_done(&t);
        kt_done(&u);
        kt_done(&w);
        break;
    }
    case BI_STR: {
        kt_get(a[0], &t);
        r = sub_str(t.s, (int64_t)t.len);
        kt_done(&t);
        break;
    }
    case BI_VAL:
        if (is_num(a[0]))
            r = a[0];
        else {
            /* like VB's Val: spaces, an optional sign, then the longest
             * number (digits, a ".", an exponent); nothing numeric gives 0 */
            kt_get(a[0], &t);
            size_t p = 0, start, end;
            int digits = 0;
            while (p < t.len && t.s[p] == ' ')
                p++;
            start = p;
            if (p < t.len && (t.s[p] == '+' || t.s[p] == '-'))
                p++;
            while (p < t.len && isdigit((unsigned char)t.s[p])) {
                p++;
                digits++;
            }
            if (p < t.len && t.s[p] == '.') {
                p++;
                while (p < t.len && isdigit((unsigned char)t.s[p])) {
                    p++;
                    digits++;
                }
            }
            end = p;
            if (digits && p < t.len && (t.s[p] == 'e' || t.s[p] == 'E')) {
                size_t q = p + 1;
                if (q < t.len && (t.s[q] == '+' || t.s[q] == '-'))
                    q++;
                if (q < t.len && isdigit((unsigned char)t.s[q])) {
                    while (q < t.len && isdigit((unsigned char)t.s[q]))
                        q++;
                    end = q;
                }
            }
            r = kv_int(0);
            if (digits) {
                char buf[64];
                size_t len = end - start < 63 ? end - start : 63;
                memcpy(buf, t.s + start, len);
                buf[len] = '\0';
                if (len > 0 && buf[len - 1] == '.')
                    buf[len - 1] = '\0';
                if (!str_to_num(buf, &r))
                    r = kv_int(0);
            }
            kt_done(&t);
        }
        break;
    case BI_CHR: {
        int64_t c = kv_to_int(a[0]);
        if (c < 0 || c > 255)
            rt_error("CHR: the character code must be 0 to 255");
        char ch = (char)c;
        r = kv_str(&ch, 1);
        break;
    }
    case BI_ASC:
        kt_get(a[0], &t);
        if (t.len == 0)
            rt_error("ASC of an empty string");
        r = kv_int((unsigned char)t.s[0]);
        kt_done(&t);
        break;
    case BI_SPACE: {
        int64_t k = nonneg(a[0], "SPACE");
        char *c = rt_alloc((size_t)k + 1);
        memset(c, ' ', (size_t)k);
        r = kv_str(c, (size_t)k);
        free(c);
        break;
    }
    case BI_ABS: {
        kv v = kv_num(a[0]);
        r = v.kind == K_INT ? kv_int(v.i < 0 ? (int64_t)(0 - (uint64_t)v.i) : v.i) : kv_flt(fabs(v.f));
        break;
    }
    case BI_SGN:
        r = kv_int(num_cmp(kv_num(a[0]), kv_int(0)));
        break;
    case BI_MIN: case BI_MAX: {
        kv v = kv_num(a[0]);
        for (int j = 1; j < n; j++) {
            kv x = kv_num(a[j]);
            int c = num_cmp(x, v);
            if (id == BI_MIN ? c < 0 : c > 0)
                v = x;
        }
        r = v;
        break;
    }
    case BI_UBOUND: case BI_LBOUND: {
        karr *arr = need_array(a[0], "UBOUND / LBOUND");
        int64_t d = n > 1 ? kv_to_int(a[1]) : 1;
        if (d < 1)
            rt_error("UBOUND / LBOUND: the dimension must be 1 or more");
        for (int64_t j = 2; j <= d; j++) {
            if (arr->len == 0 || arr->items[0].kind != K_ARR)
                rt_error("the array has fewer than %" PRId64 " dimensions", d);
            arr = arr->items[0].a;
        }
        r = kv_int(id == BI_UBOUND ? arr->len - 1 : 0);
        break;
    }
    case BI_ARRAY:
        r = kv_arr(n, NULL);
        for (int j = 0; j < n; j++)
            r.a->items[j] = kv_retain(a[j]);
        break;
    case BI_JOIN: {
        karr *arr = need_array(a[0], "JOIN");
        kt_get(n > 1 ? a[1] : kv_int(0), &u);
        const char *sep = n > 1 ? u.s : " ";
        size_t seplen = n > 1 ? u.len : 1;
        kbuf b = {NULL, 0, 0};
        kb_add(&b, "", 0);
        for (int64_t j = 0; j < arr->len; j++) {
            if (j > 0)
                kb_add(&b, sep, seplen);
            kb_value(&b, arr->items[j], 1);
        }
        r = sub_str(b.d, (int64_t)b.len);
        free(b.d);
        kt_done(&u);
        break;
    }
    case BI_SPLIT: {
        kt_get(a[0], &t);
        kt_get(n > 1 ? a[1] : kv_int(0), &u);
        const char *sep = n > 1 ? u.s : " ";
        size_t seplen = n > 1 ? u.len : 1;
        int64_t count = 0;
        size_t p = 0, start = 0;
        if (t.len > 0) {
            count = 1;
            if (seplen > 0) {
                for (p = 0; p + seplen <= t.len;) {
                    if (memcmp(t.s + p, sep, seplen) == 0) {
                        count++;
                        p += seplen;
                    } else
                        p++;
                }
            }
        }
        r = kv_arr(count, NULL);
        int64_t k = 0;
        if (t.len > 0) {
            if (seplen == 0)
                r.a->items[0] = sub_str(t.s, (int64_t)t.len);
            else {
                for (p = 0, start = 0; p + seplen <= t.len;)
                    if (memcmp(t.s + p, sep, seplen) == 0) {
                        r.a->items[k++] = sub_str(t.s + start, (int64_t)(p - start));
                        p += seplen;
                        start = p;
                    } else
                        p++;
                r.a->items[k] = sub_str(t.s + start, (int64_t)(t.len - start));
            }
        }
        kt_done(&t);
        kt_done(&u);
        break;
    }
    case BI_TYPENAME:
        if (a[0].kind == K_INT)
            r = kv_str("Integer", 7);
        else if (a[0].kind == K_FLT)
            r = kv_str("Double", 6);
        else if (a[0].kind == K_STR)
            r = kv_str("String", 6);
        else if (a[0].a->tag)
            r = kv_str(a[0].a->tag->d, a[0].a->tag->len);
        else
            r = kv_str("Array", 5);
        break;
    case BI_ISARRAY:
        r = kv_int(a[0].kind == K_ARR && !a[0].a->tag);
        break;
    case BI_NEWARRAY: {
        int64_t *dims = rt_alloc(sizeof(int64_t) * (size_t)(n > 0 ? n : 1));
        for (int j = 0; j < n; j++)
            dims[j] = kv_to_int(a[j]);
        r = new_array(dims, n);
        free(dims);
        break;
    }
    case BI_RESIZE: {
        karr *arr = need_array(a[0], "REDIM PRESERVE");
        int64_t ub = kv_to_int(a[1]);
        if (ub < -1)
            rt_error("an array's upper bound can't be %" PRId64, ub);
        r = kv_arr(ub + 1, NULL);
        for (int64_t j = 0; j <= ub && j < arr->len; j++)
            r.a->items[j] = kv_retain(arr->items[j]);
        break;
    }
    case BI_NEWOBJECT: {
        kv name = kv_retain(a[0]);
        if (name.kind != K_STR) {
            kv_release(name);
            rt_error("NEWOBJECT needs a class name");
        }
        r = kv_arr(kv_to_int(a[1]), name.s);
        kv_release(name);
        break;
    }
    case BI_ISNUMERIC: {
        kv dummy;
        r = kv_int(is_num(a[0]) || (a[0].kind == K_STR && str_to_num(a[0].s->d, &dummy)));
        break;
    }
    case BI_CINT:
        r = kv_int(kv_to_int(a[0]));
        break;
    case BI_RND: {
        if (n == 0) { /* RND: a fraction 0 <= x < 1 */
            r = kv_flt((double)(rnd_next() >> 11) * (1.0 / 9007199254740992.0));
            break;
        }
        int64_t k = kv_to_int(a[0]);
        if (k < 1)
            rt_error("RND: the range must be 1 or more");
        r = kv_int((int64_t)(rnd_next() % (uint64_t)k));
        break;
    }
    case BI_POW: {
        /* Whole numbers to a power >= 0 stay whole; anything else is a double. */
        kv x = kv_num(a[0]), y = kv_num(a[1]);
        /* (also when the whole result wouldn't fit: 10 ^ 20 is 1E+20) */
        if (x.kind != K_INT || y.kind != K_INT || y.i < 0 || fabs(pow((double)x.i, (double)y.i)) >= 9.2e18) {
            double fb = kv_to_flt(x), fe = kv_to_flt(y);
            if (fb == 0 && fe < 0)
                rt_error("division by zero");
            if (fb < 0 && fe != floor(fe))
                rt_error("a negative number to a fractional power");
            r = kv_flt(pow(fb, fe));
            break;
        }
        int64_t base = x.i, e = y.i;
        {
            uint64_t acc = 1, b = (uint64_t)base;
            while (e > 0) {
                if (e & 1)
                    acc *= b;
                b *= b;
                e >>= 1;
            }
            r = kv_int((int64_t)acc);
        }
        break;
    }
    case BI_SQR: {
        double v = kv_to_flt(a[0]);
        if (v < 0)
            rt_error("SQR of a negative number");
        r = kv_flt(sqrt(v));
        break;
    }
    case BI_INT: /* the whole number at or below (INT(-2.5) is -3) */
    case BI_FIX: { /* the fraction cut off (FIX(-2.5) is -2) */
        kv v = kv_num(a[0]);
        r = v.kind == K_INT ? v : kv_int(flt_to_int(id == BI_INT ? floor(v.f) : trunc(v.f)));
        break;
    }
    case BI_CDBL:
        r = kv_flt(kv_to_flt(a[0]));
        break;
    case BI_ROUND: { /* to even on .5, like VB's Round */
        int64_t d = n > 1 ? kv_to_int(a[1]) : 0;
        if (d < 0 || d > 15)
            rt_error("ROUND: the number of decimals must be 0 to 15");
        kv v = kv_num(a[0]);
        if (v.kind == K_INT)
            r = v;
        else if (d == 0)
            r = kv_int(flt_to_int(v.f));
        else {
            double m = pow(10, (double)d);
            r = kv_flt(rint(v.f * m) / m);
        }
        break;
    }
    case BI_SIN: case BI_COS: case BI_TAN: case BI_ATN: case BI_EXP: case BI_LOG: {
        double v = kv_to_flt(a[0]);
        if (id == BI_LOG && v <= 0)
            rt_error("LOG of a number <= 0");
        r = kv_flt(id == BI_SIN ? sin(v) : id == BI_COS ? cos(v) : id == BI_TAN ? tan(v)
                   : id == BI_ATN ? atan(v) : id == BI_EXP ? exp(v) : log(v));
        break;
    }
    case BI_STRING: {
        int64_t cnt = nonneg(a[0], "STRING$");
        char ch;
        if (a[1].kind == K_STR) {
            if (a[1].s->len == 0)
                rt_error("STRING$ needs a character");
            ch = a[1].s->d[0];
        } else {
            int64_t code = kv_to_int(a[1]);
            if (code < 0 || code > 255)
                rt_error("STRING$: the character code must be 0 to 255");
            ch = (char)code;
        }
        char *buf = rt_alloc((size_t)cnt + 1);
        memset(buf, ch, (size_t)cnt);
        r = kv_str(buf, (size_t)cnt);
        free(buf);
        break;
    }
    case BI_TIMER:
    case BI_DATE:
    case BI_TIME: {
        time_t now = time(NULL);
        struct tm *tm = localtime(&now);
        char buf[32];
        if (id == BI_TIMER) {
            r = kv_int(tm->tm_hour * 3600 + tm->tm_min * 60 + tm->tm_sec);
            break;
        }
        if (id == BI_DATE)
            snprintf(buf, sizeof buf, "%02d-%02d-%04d", tm->tm_mon + 1, tm->tm_mday, tm->tm_year + 1900);
        else
            snprintf(buf, sizeof buf, "%02d:%02d:%02d", tm->tm_hour, tm->tm_min, tm->tm_sec);
        r = kv_str(buf, strlen(buf));
        break;
    }
    case BI_HEX:
    case BI_OCT: {
        char buf[32];
        snprintf(buf, sizeof buf, id == BI_HEX ? "%" PRIX64 : "%" PRIo64, (uint64_t)kv_to_int(a[0]));
        r = kv_str(buf, strlen(buf));
        break;
    }
    case BI_SLEEP: {
        fflush(stdout);
        int64_t secs = kv_to_int(a[0]);
        if (secs > 0) {
#ifdef _WIN32
            Sleep((DWORD)(secs * 1000));
#else
            struct timespec ts = {(time_t)secs, 0};
            nanosleep(&ts, NULL);
#endif
        }
        r = kv_int(0);
        break;
    }
    case BI_INKEY:
        r = kv_str("", 0);
        break;
    case BI_QBSTR:
        if (a[0].kind == K_INT && a[0].i >= 0) {
            char buf[32];
            snprintf(buf, sizeof buf, " %" PRId64, a[0].i);
            r = kv_str(buf, strlen(buf));
        } else if (a[0].kind == K_FLT) { /* " .5", "-1.25" */
            char buf[52];
            int len = 0;
            if (a[0].f >= 0)
                buf[len++] = ' ';
            len += fmt_flt(a[0].f, buf + len, 1);
            r = kv_str(buf, (size_t)len);
        } else {
            ktext t;
            kt_get(a[0], &t);
            r = kv_str(t.s, t.len);
            kt_done(&t);
        }
        break;
    case BI_BAND: case BI_BOR: case BI_BXOR: {
        int64_t x = kv_to_int(a[0]), y = kv_to_int(a[1]);
        r = kv_int(id == BI_BAND ? (x & y) : id == BI_BOR ? (x | y) : (x ^ y));
        break;
    }
    case BI_BNOT:
        r = kv_int(~kv_to_int(a[0]));
        break;
    case BI_ERRCODE:
        kt_get(a[0], &t);
        r = kv_int(qb_error_code(t.s, t.len));
        kt_done(&t);
        break;
    case BI_ERRMSG: {
        int64_t code = kv_to_int(a[0]);
        if (code < 1 || code > 255)
            rt_error("Illegal function call"); /* ERROR n needs 1 .. 255 */
        r = kv_int(0);
        for (size_t j = 0; j < sizeof K_qb_errors / sizeof K_qb_errors[0]; j++)
            if (K_qb_errors[j].code == code)
                r = kv_str(K_qb_errors[j].text, strlen(K_qb_errors[j].text));
        if (r.kind != K_STR) {
            char buf[32];
            snprintf(buf, sizeof buf, "Error %" PRId64, code);
            r = kv_str(buf, strlen(buf));
        }
        break;
    }
    case BI_MIDSET: { /* MID$(s$, start, len) = v$: s$ with part replaced, same length */
        int64_t st = kv_to_int(a[1]), ln = kv_to_int(a[2]);
        if (st < 1)
            rt_error("Illegal function call");
        kt_get(a[0], &t);
        kt_get(a[3], &u);
        char *d = rt_alloc(t.len + 1);
        memcpy(d, t.s, t.len);
        int64_t room = (int64_t)t.len - (st - 1);
        int64_t cnt = (int64_t)u.len;
        if (ln >= 0 && ln < cnt)
            cnt = ln;
        if (cnt > room)
            cnt = room;
        if (cnt > 0)
            memcpy(d + st - 1, u.s, (size_t)cnt);
        r = kv_str(d, t.len);
        free(d);
        kt_done(&t);
        kt_done(&u);
        break;
    }
    case BI_LSET: case BI_RSET: { /* LSET / RSET a$ = v$: v$ in a$'s length, padded with spaces */
        kt_get(a[0], &t);
        kt_get(a[1], &u);
        size_t w = t.len, n = u.len < w ? u.len : w;
        char *d = rt_alloc(w + 1);
        memset(d, ' ', w);
        memcpy(id == BI_LSET ? d : d + (w - n), u.s, n);
        r = kv_str(d, w);
        free(d);
        kt_done(&t);
        kt_done(&u);
        break;
    }
    case BI_USING:
        r = using_format(a, n);
        break;
    case BI_FOPEN: case BI_FCLOSE: case BI_FPRINT: case BI_FREADLINE: case BI_FREADFIELD:
    case BI_EOF: case BI_FREEFILE: case BI_LOF: case BI_KILL: case BI_NAME: case BI_FGET: case BI_FPUT:
    case BI_FSEEK: case BI_FSEEKPOS: case BI_FLOC: case BI_FINPUTS: case BI_MKI: case BI_MKL: case BI_MKS:
    case BI_MKD: case BI_CVI: case BI_CVL: case BI_CVS: case BI_CVD:
        r = file_builtin(id, a, n);
        break;
    default:
        rt_error("unknown built-in function %d", id);
    }
    release_args(a, n);
    rt_push(r);
}

/* ------------------------------------------------------------------ */
/* TRY / CATCH                                                         */
/* ------------------------------------------------------------------ */

typedef struct {
    int label; /* the CATCH */
    int sp;    /* evaluation stack height at TRY */
    int depth; /* call depth at TRY */
} ktry;
static ktry *K_tries;
static int K_ntries, K_tries_cap;

/* main() calls setjmp(K_jb) when the program has a TRY; a caught error
 * longjmps there, and main() continues at label K_catch_target. */
#ifndef KAYTE_NO_SETJMP
#ifdef KAYTE_RT_LIBRARY
RT_API jmp_buf K_jb;
RT_API int K_catch_target;
#else
static jmp_buf K_jb;
static int K_catch_target;
#endif
#endif

RT_API void rt_try(int label)
{
    if (K_ntries == K_tries_cap) {
        K_tries_cap = K_tries_cap * 2 + 8;
        K_tries = realloc(K_tries, sizeof(ktry) * (size_t)K_tries_cap);
        if (!K_tries)
            rt_error("out of memory");
    }
    K_tries[K_ntries].label = label;
    K_tries[K_ntries].sp = K_sp;
    K_tries[K_ntries].depth = K_depth;
    K_ntries++;
}

static void drop_stale_tries(void)
{
    while (K_ntries > 0 && K_tries[K_ntries - 1].depth > K_depth)
        K_ntries--;
}

RT_API void rt_try_end(void)
{
    if (K_ntries > 0)
        K_ntries--;
}

RT_API void rt_throw(void)
{
    kv m = rt_pop();
    char *s = kv_cstr(m);
    kv_release(m);
    rt_error("%s", s);
}

/* From rt_error: with a TRY active, unwind to it and jump to its CATCH
 * with the message on the stack; otherwise return (and the error ends
 * the program). Takes ownership of msg. */
static void rt_catch(char *msg)
{
#ifndef KAYTE_NO_SETJMP
    if (K_ntries == 0)
        return;
    ktry h = K_tries[--K_ntries];
    while (K_sp > h.sp)
        kv_release(K_stack[--K_sp]);
    K_depth = h.depth;
    rt_push(kv_str(msg, strlen(msg)));
    free(msg);
    K_catch_target = h.label;
    longjmp(K_jb, 1);
#else
    (void)msg;
#endif
}

/* ------------------------------------------------------------------ */
/* QT                                                                  */
/* ------------------------------------------------------------------ */

typedef struct {
    int32_t is_str;
    int32_t reserved;
    int64_t i;
    const char *s;
} kqt_value; /* matches source/qt6/kayte_qt6.cpp */

#ifdef KAYTE_QT_STATIC

/* The shim is linked in: nothing to load. */
int kqt_call(const char *, int, const kqt_value *, kqt_value *);
const char *kqt_last_error(void);

static void qt_load(void) {}

#else

static int (*kqt_call)(const char *, int, const kqt_value *, kqt_value *);
static const char *(*kqt_last_error)(void);

#if defined(_WIN32)
#define KQT_LIB "kayte_qt6.dll"
#elif defined(__APPLE__)
#define KQT_LIB "libkayte_qt6.dylib"
#else
#define KQT_LIB "libkayte_qt6.so"
#endif

#if defined(_WIN32)
static void *kqt_dlopen(const char *path) { return (void *)LoadLibraryA(path); }
static void *kqt_dlsym(void *h, const char *name) { return (void *)GetProcAddress((HMODULE)h, name); }
#elif !defined(KAYTE_NO_DYNLOAD)
static void *kqt_dlopen(const char *path) { return dlopen(path, RTLD_NOW); }
static void *kqt_dlsym(void *h, const char *name) { return dlsym(h, name); }
#endif

#ifndef KAYTE_NO_DYNLOAD
static void exe_dir(char *buf, size_t size)
{
    char path[PATH_MAX], real[PATH_MAX];
    buf[0] = '\0';
#if defined(_WIN32)
    DWORD n = GetModuleFileNameA(NULL, path, sizeof(path));
    if (n == 0 || n >= sizeof(path))
        return;
    if (!_fullpath(real, path, sizeof(real)))
        return;
    char *sep = strrchr(real, '\\');
    if (sep)
        *sep = '\0';
    snprintf(buf, size, "%s", real);
    return;
#elif defined(__APPLE__)
    uint32_t n = sizeof(path);
    if (_NSGetExecutablePath(path, &n) != 0)
        return;
#else
    ssize_t n = readlink("/proc/self/exe", path, sizeof(path) - 1);
    if (n < 0)
        return;
    path[n] = '\0';
#endif
#ifndef _WIN32
    if (!realpath(path, real))
        return;
    char *slash = strrchr(real, '/');
    if (slash)
        *slash = '\0';
    snprintf(buf, size, "%s", real);
#endif
}
#endif

/* Lookup order: $KAYTE_QT6_LIB, next to the executable, the system search
 * path, then the library kayte itself used when compiling this program. */
static void qt_load(void)
{
    if (kqt_call)
        return;
#if defined(KAYTE_APPLE_MOBILE)
    rt_error("QT/QML on iOS need the app built by scripts/build-kayte-ios.sh (Qt has no tvOS support)");
#elif defined(KAYTE_NO_DYNLOAD)
    rt_error("QT/QML are not available on WebAssembly (no Qt there)");
#else
    char dir[PATH_MAX], beside[PATH_MAX + 64];
    exe_dir(dir, sizeof(dir));
    snprintf(beside, sizeof(beside), "%s/%s", dir, KQT_LIB);
    const char *candidates[] = {getenv("KAYTE_QT6_LIB"), dir[0] ? beside : NULL, KQT_LIB, K_qt_lib_fallback};
    void *h = NULL;
    for (size_t i = 0; i < sizeof(candidates) / sizeof(*candidates) && !h; i++)
        if (candidates[i] && *candidates[i])
            h = kqt_dlopen(candidates[i]);
    if (!h)
        rt_error("cannot load %s - build it from source/qt6 and put it next to this program or set KAYTE_QT6_LIB", KQT_LIB);
    *(void **)&kqt_call = kqt_dlsym(h, "kqt_call");
    *(void **)&kqt_last_error = kqt_dlsym(h, "kqt_last_error");
    if (!kqt_call || !kqt_last_error)
        rt_error("%s is missing kqt_call - rebuild it from source/qt6", KQT_LIB);
#endif
}

#endif /* KAYTE_QT_STATIC */

/* Calls kqt_call with args[0] as the command - prefixed "qml." for a QML
 * statement, which is how the shim tells them apart; returns an owned
 * value. */
static kv qt_call(kv *args, int n, int qml)
{
    qt_load();
    char *name = kv_cstr(args[0]);
    char *cmd = rt_alloc(strlen(name) + 5);
    snprintf(cmd, strlen(name) + 5, "%s%s", qml ? "qml." : "", name);
    free(name);
    kqt_value *cargs = rt_alloc(sizeof(kqt_value) * (size_t)(n > 1 ? n - 1 : 1));
    char **texts = calloc((size_t)(n > 0 ? n : 1), sizeof(char *));
    if (!texts)
        rt_error("out of memory");
    for (int j = 1; j < n; j++) {
        cargs[j - 1].is_str = args[j].kind != K_INT;
        cargs[j - 1].reserved = 0;
        cargs[j - 1].i = args[j].i;
        cargs[j - 1].s = args[j].kind == K_STR ? args[j].s->d : NULL;
        if (args[j].kind == K_ARR || args[j].kind == K_FLT) /* an array or double goes as its text */
            cargs[j - 1].s = (texts[j] = kv_cstr(args[j]));
    }
    kqt_value res;
    int ok = kqt_call(cmd, n - 1, cargs, &res);
    for (int j = 0; j < n; j++)
        free(texts[j]);
    free(texts);
    free(cargs);
    free(cmd);
    if (!ok)
        rt_error("%s", kqt_last_error());
    return res.is_str ? kv_str(res.s, strlen(res.s)) : kv_int(res.i);
}

/* Qt handle -> SUB entry address, for QT "on". */
typedef struct {
    int64_t handle;
    int addr;
} khandler;
static khandler *K_handlers;
static int K_nhandlers;
static int64_t K_qt_event; /* handle whose event QT "run" is handling */

static int handler_index(int64_t handle)
{
    for (int i = 0; i < K_nhandlers; i++)
        if (K_handlers[i].handle == handle)
            return i;
    return -1;
}

/* Runs SUB entry `addr` on `handle`'s events (QT "on"). */
static void handler_put(int64_t handle, int addr)
{
    int hi = handler_index(handle);
    if (hi < 0) {
        K_handlers = realloc(K_handlers, sizeof(khandler) * (size_t)(K_nhandlers + 1));
        if (!K_handlers)
            rt_error("out of memory");
        hi = K_nhandlers++;
    }
    K_handlers[hi].handle = handle;
    K_handlers[hi].addr = addr;
}

/* After QT "loadform": attaches the SUBs the form file names to the
 * handles it created, as QT "on" would. The shim lists them as
 * "handle<TAB>SubName<TAB>optional" lines; optional ones (VB-style
 * Button1_Click defaults) are skipped when there's no such SUB. */
static void attach_form_handlers(int64_t window, const char *kw, int qml)
{
    kv req[2] = {kv_str("formhandlers", 12), kv_int(window)};
    kv list = qt_call(req, 2, qml);
    kv_release(req[0]);
    char *text = kv_cstr(list);
    kv_release(list);
    for (char *line = text, *next; line && *line; line = next) {
        next = strchr(line, '\n');
        if (next)
            *next++ = '\0';
        char *name = strchr(line, '\t');
        char *opt = name ? strchr(name + 1, '\t') : NULL;
        if (!opt)
            continue;
        *name++ = '\0';
        *opt++ = '\0';
        int si = find_sub(name);
        if (si < 0) {
            if (*opt == '1')
                continue;
            rt_error("%s \"loadform\" - the form's handler SUB \"%s\" doesn't exist", kw, name);
        }
        if (K_sub_params[si] != 0)
            rt_error("%s \"loadform\" - handler SUB \"%s\" must take no parameters (use %s \"event\" inside it to see which widget fired)", kw, name, kw);
        handler_put(strtoll(line, NULL, 10), K_sub_addrs[si]);
    }
    free(text);
}

/*
 * QT statement (or QML, when qml is 1 - same commands, see kayte_qt6.cpp)
 * with n pushed values (command name first); dest >= 0 is
 * the TO variable. "run" may need to call a handler SUB: then it pushes
 * its own arguments back, records a frame resuming at label `self` (this
 * very statement, which re-executes and waits for the next event), sets
 * *target to the SUB's entry and returns 1 - the generated code jumps
 * there. Otherwise returns 0.
 */
RT_API int rt_qt(int n, int dest, int self, int *target, int qml)
{
    const char *kw = qml ? "QML" : "QT";
    kv *args = pop_args(n);
    char *cmd = kv_cstr(args[0]);
    for (char *p = cmd; *p; p++)
        *p = (char)tolower((unsigned char)*p);
    kv res = kv_int(0);

    if (strcmp(cmd, "on") == 0) {
        if (n != 3)
            rt_error("%s \"on\" expects 2 argument(s), got %d", kw, n - 1);
        int64_t handle = kv_to_int(args[1]);
        char *name = kv_cstr(args[2]);
        int hi = handler_index(handle);
        if (!*name) {
            if (hi >= 0)
                K_handlers[hi] = K_handlers[--K_nhandlers];
        } else {
            int si = find_sub(name);
            if (si < 0)
                rt_error("%s \"on\" - there is no SUB named \"%s\"", kw, name);
            if (K_sub_params[si] != 0)
                rt_error("%s \"on\" - handler SUB \"%s\" must take no parameters (use %s \"event\" inside it to see which widget fired)", kw, name, kw);
            handler_put(handle, K_sub_addrs[si]);
            res = kv_int(1);
        }
        free(name);
    } else if (strcmp(cmd, "event") == 0) {
        if (n != 1)
            rt_error("%s \"event\" takes no arguments", kw);
        res = kv_int(K_qt_event);
    } else if (strcmp(cmd, "run") == 0) {
        if (n != 1)
            rt_error("%s \"run\" takes no arguments", kw);
        kv wait = kv_str("wait", 4);
        for (;;) {
            kv ev = qt_call(&wait, 1, 0);
            int64_t handle = ev.i;
            kv_release(ev);
            if (handle == 0) {
                K_qt_event = 0;
                break; /* every window closed - continue after QT "run" */
            }
            int hi = handler_index(handle);
            if (hi >= 0) {
                K_qt_event = handle;
                for (int j = 0; j < n; j++)
                    rt_push(kv_retain(args[j]));
                rt_call(self, 0);
                *target = K_handlers[hi].addr;
                kv_release(wait);
                free(cmd);
                release_args(args, n);
                return 1;
            }
        }
        kv_release(wait);
    } else {
        res = qt_call(args, n, qml);
        if (strcmp(cmd, "loadform") == 0)
            attach_form_handlers(res.i, kw, qml);
    }

    if (dest >= 0)
        rt_set_var(dest, res);
    else
        kv_release(res);
    free(cmd);
    release_args(args, n);
    return 0;
}

/* ------------------------------------------------------------------ */
/* Program entry for the LLVM backend                                  */
/* ------------------------------------------------------------------ */

#ifdef KAYTE_RT_LIBRARY
/* Defined by the generated IR: its variable count, and its code, which
 * starts at the beginning (start < 0) or at label `start` (a CATCH). */
extern const int K_nvars_init;
extern int kayte_main(int start);

int main(int argc, char **argv)
{
    rt_init(K_nvars_init, argc > 0 ? argv[0] : "kayte");
    int start = -1;
#ifndef KAYTE_NO_SETJMP
    /* A caught runtime error comes back here (rt_catch) and re-enters the
     * program at its CATCH; kayte_main's state lives in this runtime. */
    if (setjmp(K_jb))
        start = K_catch_target;
#endif
    return kayte_main(start);
}
#endif
