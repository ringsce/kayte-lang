# Kayte Lang: A Modern, Cross-Platform Programming Language

Kayte Lang is an experimental programming language for building applications quickly and cross-platform. Programs are written in a BASIC-style syntax (`.kayte`) or a JavaScript-like one (`.kjs`); both compile to the same bytecode. A program compiles to bytecode for the Kayte Virtual Machine (KVM), or to a standalone native executable through C or LLVM. The native path reaches macOS, Linux, Windows, iOS / tvOS and WebAssembly. Native Qt6 GUIs can be built from code, from QML, or from `.kfm` form files.

---

## 🎯 Quick Start

```bash
# Clone the repository
git clone https://github.com/ringsce/kayte-lang.git
cd kayte-lang

# Build kayte (macOS; see "Building from Source" for other platforms)
scripts/build-kayte-macos.sh

# Write a program
cat > hello.kayte <<'EOF'
name = "World"
PRINT "Hello, " & name & "!"
EOF

# Compile to bytecode and run it on the VM
bin/kayte --compile hello.kayte -o hello.bytecode
bin/kayte --run hello.bytecode

# Or compile to a native executable (through C; needs cc)
bin/kayte --native hello.kayte -o hello
./hello

# Or through LLVM, for this machine or another platform
bin/kayte --llvm hello.kayte -o hello
bin/kayte --llvm hello.kayte --target x86_64-w64-mingw32 -o hello.exe

# Prefer JavaScript-like syntax? Use a .kjs file - every command works the same
bin/kayte --compile examples/hello.kjs -o hello.bytecode
```

---

## 🗂️ Project Layout

```
source/      all the Pascal sources and the Lazarus projects: kayte.lpi (the compiler / VM), webgen,
             vb6interpreter, mathlibdylib, kfmlibgen; native/ C runtime, qt6/ Qt bridge, ios/ app template
examples/    example programs (.kayte, .kjs), forms (.kfm) and QML
scripts/     build scripts: macOS / Linux containers / musl, Qt, iOS, .kfm and mathlib libraries
docs/        guides (musl builds, KayteIDE), grammar and images
kfrm/        KFRM form framework units (parser, renderer, event router) used by vb6interpreter
external/    third-party Markdown units used by webgen
jvm/         experimental Java VM + JNI bridge
tools/       kcc and kldb C tools
cmake/, components/gui/   CMake helpers and GUI components
```

Build output goes to `bin/` (the programs: `bin/kayte` …) and `build/` (compiled units in `build/units/`, and the platform builds), both ignored by git.

---

## 🔧 Building from Source

### Requirements

**All platforms:**
- FreePascal 3.2.2 or newer, and Lazarus's `lazbuild`
- Git

**Optional:**
- A C compiler (`cc`) for `--native`, and `clang` for `--llvm`
- Qt 6 and CMake for GUI programs (`QT` / `QML` statements)
- Xcode and Qt for iOS for iOS apps
- musl cross toolchains for static Linux builds (installed by the setup script)

### Build Options

```bash
# macOS: finds fpc + lazbuild and builds source/kayte.lpi
scripts/build-kayte-macos.sh

# Or directly with lazbuild, on any platform (builds bin/kayte)
lazbuild source/kayte.lpi

# Linux ARM64 in a container (Apple's `container` tool, on Apple silicon)
scripts/build-kayte-debian-container.sh          # glibc: build/debian/kayte
scripts/build-kayte-alpine-container.sh          # musl: build/alpine/kayte
scripts/build-kayte-alpine-container.sh static   # static musl, runs on any Linux ARM64 (no QT / QML)
ARCH=amd64 scripts/build-kayte-alpine-container.sh build    # x86_64 (through Rosetta): build/alpine-amd64/
ARCH=amd64 scripts/build-kayte-alpine-container.sh static   # static x86_64, runs on any Linux x86_64

# Windows, cross-compiled with fpcupdeluxe's FPC + llvm-mingw:
# build/windows-arm64/{kayte.exe, kaytearm64pe.dll, kayte_native_rt.c}
scripts/build-kayte-windows.sh arm64      # (x86_64 needs that target's units rebuilt for the current FPC)

# Qt6 GUI support (the libkayte_qt6 bridge library, copied next to kayte)
scripts/build-kayte-qt6.sh
```

These build **`bin/kayte`**. Add `bin/` to your `PATH`, or call it as `bin/kayte`; the rest of this README writes just `kayte`.

**musl builds (Linux, portable static binaries):**

```bash
sudo scripts/setup_and_build_kayte.sh both   # set up the toolchains and build
scripts/build_kayte_musl.sh both             # or, with toolchains already installed
make -f Makefile.kayte all             # or with Make
```

musl builds are statically linked, so they have no runtime dependencies and run on any Linux distribution, on ARM64 and AMD64. See **[docs/README_KAYTE_MUSL.md](docs/README_KAYTE_MUSL.md)**.

---

## 🚀 The Language

Kayte has two front ends that compile to the same bytecode, so their programs run identically on the VM, `--native`, `--llvm` and iOS:

- **BASIC-style** (`.kayte`): described in this section. Keywords are case-insensitive.
- **JavaScript-like** (`.kjs` or `.js`): see [JavaScript-like Front End](#-javascript-like-front-end-kjs). Both have functions with return values, local variables and recursion.

This section describes what the compiler accepts today. Features that are designed but not implemented yet are listed under [Not Yet Supported](#-not-yet-supported).

### ✅ Variables & Expressions
**Status:** Implemented

Variables need no declaration and are dynamically typed: a value is a number, a string, an [array](#-arrays) or an [object](#-classes). `+` adds two numbers and concatenates as soon as either side is a string (so `"2" + 3` is `"23"`). `&` always concatenates. `-`, `*`, `/`, `\` and `MOD` need numbers (numeric strings convert). `/` divides exactly (`17 / 5` is 3.4), and `\` is VB's whole-number division (`17 \ 5` is 3). `MOD` is the remainder, with the sign of the left side (`-17 MOD 5` is `-2`, `5.5 MOD 2` is 1.5). Comparisons (`=`, `<>`, `<`, `>`, `<=`, `>=`) give `1` or `0`. `TRUE` is 1 and `FALSE` is 0.

`AND`, `OR`, `XOR` and `NOT` are logical: they give `1` or `0`. `AND` / `OR` short-circuit, so the right side is skipped when the left decides the result (like VB.NET's `AndAlso` / `OrElse`). `XOR` is true when exactly one side is. They aren't bitwise, so `2 AND 3` is `1`. `a ^ b` is a power (`2 ^ 10` is 1024, `2 ^ 0.5` is 1.4142135623731). Precedence, from loosest (as in VB): `XOR`, `OR`, `AND`, `NOT`, comparisons, `+ - &`, `MOD`, `\`, `* /`, unary `-`, `^` (so `-2 ^ 2` is -4). So `NOT x = 5` means `NOT (x = 5)`, and `IF x > 1 AND x < 9 OR done THEN` needs no parentheses.

```kayte
' Comments start with ', // or REM
total = (2 + 3) * 4
name = "Kayte"
PRINT name & " says " & total      ' Kayte says 20
PRINT "2" + 3, "ab" + "cd"         ' 23 abcd
PRINT 17 / 5, 17 \ 5, 17 MOD 5    ' 3.4 3 2
IF total > 10 AND name <> "" THEN PRINT "both"
```

**Numbers** are 64-bit whole numbers or 64-bit floating-point (doubles), as in VB:

- **Literals:** `3.14`, `.5`, `2.5E-3` and `6.02E+23`. A whole number too large for 64 bits becomes a double.
- **Staying whole:** whole numbers stay whole under `+ - * \ MOD` and a power `>= 0`. With a fraction anywhere the result is a double. `/` gives a whole number only when the division is exact (`10 / 2` is 5, `10 / 4` is 2.5).
- **Printing:** doubles print with 15 significant digits and no trailing zeros (`1 / 3` is `0.333333333333333`, `0.1 + 0.2` is `0.3`). Outside 0.00001 … 10^15 they use scientific notation (`1.5E+20`, `1E-07`). `TYPENAME` gives `Double`.
- **Whole numbers from fractions:** where a whole number is needed (an array index, `MID`'s position, `CHR` ...), a double is rounded to the nearest whole number, to even on .5 (VB's banker's rounding: `CInt(2.5)` is 2).
- **Errors:** dividing by zero, and results too large for a double (`Exp(1000)`), are runtime errors (no infinities or NaN).
- **Comparing:** doubles compare exactly, so `0.1 + 0.2 = 0.3` is false (as in VB). Compare with a tolerance (`Abs(a - b) < 1E-9`).
- **Backends:** the VM, `--native` and `--llvm` give identical results and print identical text.

`DIM … AS` declares a variable with a type name, optionally with a starting value (`DIM n AS Integer = 5`, `DIM total = 0`). Types aren't checked yet, so the name is mostly documentation; `AS String` starts the variable as `""` (other variables start as 0), and so do names ending in `$`. `CONST name = value` sets a constant. `STRUCT` defines records with fields:

```kayte
STRUCT Point
    X AS Integer
    Y AS Integer
END STRUCT

DIM p AS Point
p.X = 3
p.Y = 4
PRINT p.X & "," & p.Y              ' 3,4
```

Type arguments after `AS` are accepted and ignored, e.g. `DIM items AS List<Integer>`. Generic *definitions* (`STRUCT Box<T>`) are on the roadmap.

### ✅ Conditions & Loops
**Status:** Implemented (bytecode VM, `--native` and `--llvm`)

```kayte
FOR i = 1 TO 5
    IF i = 1 THEN
        PRINT i, "one"
    ELSEIF i < 5 THEN
        PRINT i, "two to four"
    ELSE
        PRINT i, "five"
    END IF
NEXT i

FOR k = 10 TO 1 STEP -3          ' 10 7 4 1
    PRINT k
NEXT

total = 0
WHILE total < 100
    total = total + 30
END WHILE

IF total > 100 THEN PRINT "over" ELSE PRINT "exactly 100 or less"

FOR i = 1 TO 1000
    IF i * i > 50 THEN EXIT FOR                 ' leaves the loop: i = 8
NEXT i

SELECT CASE i
    CASE 1, 2, 3
        PRINT "small"
    CASE 4 TO 9
        PRINT "medium"                          ' runs
    CASE IS >= 10
        PRINT "large"
    CASE ELSE
        PRINT "negative"
END SELECT
```

- **`IF … THEN`:** if `THEN` ends the line, it's a block. Any number of `ELSEIF … THEN` branches can follow, then an optional `ELSE`, closed by `END IF` (or `ENDIF`). Otherwise it's the single-line form, `IF cond THEN statements [ELSE statements]`, where each branch can be several statements separated by `:` (`IF x THEN a = 1: b = 2 ELSE c = 3`).
- **`FOR var = start TO end [STEP n] … NEXT [var]`:** `end` and `n` are evaluated once. The loop counts down when the step is negative, even when the step is a variable. If it doesn't run at all, the variable keeps its start value. `NEXT var`, when given, must name the loop's variable.
- **`WHILE … END WHILE`** (or `WEND`).
- **`SELECT CASE expr`:** the expression is evaluated once. Each `CASE` lists values (`CASE 1, 2, 3`), ranges (`CASE 4 TO 9`) or comparisons (`CASE IS >= 10`, or just `CASE >= 10`), and they can be mixed. The first matching `CASE` runs, with no fall-through. `CASE ELSE` must come last. Closed by `END SELECT`.
- **`DO … LOOP`:** the condition can be at the top (`DO WHILE c` / `DO UNTIL c`, tested before each pass) or at the bottom (`LOOP WHILE c` / `LOOP UNTIL c`, so the body runs at least once), but not both. With neither, the loop runs until `EXIT DO`.
- **`EXIT FOR` / `EXIT WHILE` / `EXIT DO`:** leave the innermost loop of that kind, even from inside another kind of loop nested in it. `EXIT SUB` leaves a SUB, like `RETURN`.
- **`CONTINUE FOR` / `CONTINUE WHILE` / `CONTINUE DO`:** skip the rest of the body and go on with that loop's next iteration. A `FOR` still applies its step; a `WHILE` or `DO` re-tests its condition.
- **`GOTO label`:** jumps to a `label:` line, forward or back. Labels are local to their SUB or FUNCTION (or the top level), so a `GOTO` can't jump into or out of a procedure. A statement can follow the label on the same line (`done: PRINT x`).
- **`GOSUB label` … `RETURN`:** runs the code at `label:` until a `RETURN`, then continues after the `GOSUB`. Labels are scoped like `GOTO`'s. Inside a SUB or FUNCTION, a plain `RETURN` returns from a `GOSUB` in progress, and otherwise leaves the procedure, as in VB. Leaving the procedure from inside a `GOSUB` routine (`EXIT SUB`, `RETURN value`) is fine. A `RETURN` with no `GOSUB` is a runtime error.
- **`END`** on its own stops the program, for example before the `GOSUB` routines at the bottom of a file. `END` followed by a keyword closes a block (`END IF`, `END SUB`, …).
- **`:`** separates statements on one line: `a = 1 : b = 2`.

All of them nest, inside each other and inside `SUB`s. An unclosed block is reported with the line it starts on (`… the IF block that starts at line 2 - is END IF missing?`), and so is an `END` that closes the wrong kind of block. See `examples/control_flow.kayte` and `examples/operators.kayte`.

### ✅ Subroutines and Functions (SUB / FUNCTION)
**Status:** Implemented (bytecode VM, `--native` and `--llvm`)

`SUB` defines a subroutine and `FUNCTION` one that returns a value. Both can be defined before or after their first use.

```kayte
PRINT "fib(20) =", Fib(20)                ' 6765
PRINT Grade(95), Grade(72)                ' A B
CALL Greet("Ada")                         ' or just: Greet("Ada")

FUNCTION Fib(n)
    IF n < 2 THEN RETURN n
    RETURN Fib(n - 1) + Fib(n - 2)        ' recursion works
END FUNCTION

FUNCTION Grade(score AS Integer) AS String
    IF score >= 90 THEN
        Grade = "A"                       ' VB style: assign to the name
        EXIT FUNCTION
    END IF
    Grade = "B"
END FUNCTION

SUB Greet(ByVal who)
    DIM line                              ' local to Greet
    line = "Hello, " & who & "!"
    PRINT line
END SUB
```

- **Calling:** `CALL Name(args)`, `Name(args)`, or VB-style `Name arg1, arg2` (or just `Name` with no arguments) runs a SUB or FUNCTION as a statement. A bare word on its own line is therefore a call, and a misspelt statement is reported as a call to an undefined SUB. A FUNCTION called inside an expression gives its result.
- **Results:** a FUNCTION returns what was assigned to its name (`Grade = "A"`), or the value of `RETURN value`. If nothing was assigned, it returns `0`. `EXIT FUNCTION` / `EXIT SUB`, or a plain `RETURN`, leave early.
- **Local variables:** parameters and anything `DIM`med inside a SUB or FUNCTION are local. Each call gets its own copies, so recursion works. Any other name is a global, so a SUB can still update the program's variables (as GUI event handlers do). `DIM` a name to keep it local.
- **Parameters:** `[ByVal | ByRef] name [AS Type]`; the default is `ByVal`. Changes to a `ByRef` parameter are copied back into the variable the caller passed, including struct fields, when the procedure returns (`SUB Swap(ByRef a, ByRef b)`). A literal or expression passed `ByRef` gets a temporary copy, as in VB. Types aren't checked yet.

These are reported at compile time:
- calling an undefined procedure;
- the wrong number of arguments;
- using a SUB's "value" in an expression;
- `RETURN value` inside a SUB.

Recursion deeper than 10,000 calls stops with a "call stack overflow" error. See `examples/functions.kayte`.

### ✅ Arrays
**Status:** Implemented (bytecode VM, `--native` and `--llvm`)

```kayte
DIM scores(4)                    ' scores(0) .. scores(4), all 0
scores(0) = 90
DIM grid(2, 3)                   ' an array of arrays: grid(row, col)
grid(1, 2) = "X"
words = Split("a b c", " ")      ' arrays also come from Array(...) and SPLIT
PRINT words(1), UBOUND(words), LEN(words), words   ' b 2 3 [a, b, c]

DIM list()                       ' empty
REDIM PRESERVE list(UBOUND(list) + 1)   ' grow by one, keeping the items
list(UBOUND(list)) = "first"

FOR EACH w IN words
    PRINT w
NEXT
```

- `DIM a(n)` has `n + 1` elements, `a(0)` to `a(n)`, as in VB. Every element starts as `0`. Arrays always start at 0, so `DIM a(1 TO 5)` is an error (QuickBASIC mode accepts it).
- `REDIM a(n)` makes a new, empty array, and `REDIM PRESERVE a(n)` keeps the items that fit (for one-dimensional arrays). `UBOUND(a)` is the last index, and `UBOUND(m, 2)` gives the second dimension's.
- Arrays are references, like objects: `b = a` makes `b` the same array, and a SUB that changes an array it's passed changes the caller's array.
- Elements can hold anything, including other arrays and objects. `PRINT` shows an array as `[1, 2, 3]`. Arrays compare by identity, and only with `=` and `<>`.
- An index outside the array is a runtime error (`index 5 is out of range (0 to 2)`).
- `FOR EACH x IN array … NEXT` visits each element; `EXIT FOR` and `CONTINUE FOR` work in it.
- `ByRef` copies back into plain variables; array elements and fields are passed by value.

### ✅ Built-in Functions
**Status:** Implemented (bytecode VM, `--native` and `--llvm`)

String positions start at 1, as in VB, and a trailing `$` is accepted (`LEFT$`, `MID$`, `CHR$`).

| Kind | Functions |
|------|-----------|
| Strings | `LEN(s)`, `LEFT(s, n)`, `RIGHT(s, n)`, `MID(s, start[, n])`, `UCASE(s)` / `UPPER`, `LCASE(s)` / `LOWER`, `TRIM`, `LTRIM`, `RTRIM`, `INSTR([start,] s, find)` (0 if not found), `REPLACE(s, find, with)`, `SPACE(n)`, `CHR(code)`, `ASC(s)` |
| Strings (more) | `STRING(n, c)` (`n` copies of a character or code), `HEX(n)`, `OCT(n)` |
| Numbers | `ABS`, `SGN`, `MIN(a, b, ...)`, `MAX(a, b, ...)`, `RND` (a random fraction 0 ≤ x < 1) and `RND(n)` (a random whole number 0 … n-1), `SQR`, `SIN`, `COS`, `TAN`, `ATN`, `EXP`, `LOG` (natural), `VAL(s)` (the leading number, `"19.99 EUR"` is 19.99; else 0), `ISNUMERIC(x)` |
| Rounding and conversion | `CINT(x)` / `CLNG` (nearest, to even on .5), `INT(x)` (down: `INT(-2.7)` is -3), `FIX(x)` (toward 0: -2), `ROUND(x[, decimals])` (to even on .5), `CDBL(x)` / `CSNG` (to a double), `STR(x)` / `CSTR` |
| Arrays | `ARRAY(a, b, ...)`, `SPLIT(s[, separator])`, `JOIN(array[, separator])`, `LEN(array)`, `UBOUND(array[, dimension])`, `LBOUND` |
| Types | `TYPENAME(x)` (`Integer`, `Double`, `String`, `Array` or the class name), `ISARRAY(x)` |

A SUB or FUNCTION of your own with the same name replaces the built-in function. Wrong argument counts are compile errors.

### ✅ Classes
**Status:** Implemented (bytecode VM, `--native` and `--llvm`)

```kayte
DIM acct AS NEW Account("Ada", 100)
acct.Deposit 50
PRINT acct.Owner, acct.Balance           ' Ada 150

CLASS Account
    PUBLIC Owner
    PRIVATE total, history()             ' fields; history starts as an empty array

    SUB New(name, opening)               ' the constructor: NEW Account(...) calls it
        Owner = name
        total = opening
    END SUB

    SUB Deposit(amount)
        total = total + amount           ' fields by name, or Me.total
    END SUB

    PROPERTY GET Balance()               ' read as acct.Balance
        Balance = total
    END PROPERTY

    PROPERTY LET Balance(value)          ' acct.Balance = 5 (SET works too)
        total = value
    END PROPERTY
END CLASS
```

- **Fields** are declared with `DIM`, `PUBLIC` or `PRIVATE`, and can have a starting value: `PUBLIC Count = 0`, `DIM items(9)`, `DIM child AS NEW Node`. Inside methods, fields are used by name or as `Me.field`, and `Me` is the object itself.
- **Methods** are `SUB`s and `FUNCTION`s, called as `obj.Method(args)`, or `obj.Method arg1, arg2` as a statement. Inside the class, a method can call another one by name. `SUB New(...)` (or VB6's `Class_Initialize`) is the constructor.
- **Properties:** `PROPERTY GET` is read like a field, and `PROPERTY LET` / `PROPERTY SET` run when one is assigned.
- **Objects are references:** `DIM p AS Point` is `Nothing` (0) until it's set (`SET p = NEW Point`, or just `p = NEW Point`), and `b = a` shares the object. Test with `p Is Nothing` and compare with `=`. `PRINT` shows `<Point>`, and `TYPENAME(p)` gives `Point`.
- **Any object with the member works:** the class is found at run time, so arrays can mix classes that share method names (`s.Area()` on rectangles and circles). A member that no class has is a compile error, and an object without it at run time is a runtime error (`Square has no member 'Radius'`).
- `obj.items(2)`, `shapes(0).Area()` and `list.First.Value` chain as you'd expect, and `WITH obj` … `END WITH` lets you write `.Member`.
- Classes can be used above their definition.

Not supported yet: static type checks, interfaces and abstract classes (`MUSTOVERRIDE` is accepted but not enforced), and hiding of `PRIVATE` members.

#### Inheritance

```kayte
CLASS Animal
    PUBLIC Name
    SUB New(n)
        Name = n
    END SUB
    OVERRIDABLE FUNCTION Speak()
        RETURN "..."
    END FUNCTION
    FUNCTION Introduce()
        RETURN Name & " says " & Speak()   ' calls the object's own Speak()
    END FUNCTION
END CLASS

CLASS Dog INHERITS Animal                  ' or INHERITS Animal on the next line
    OVERRIDES FUNCTION Speak()
        RETURN "Woof (" & MYBASE.Speak() & ")"
    END FUNCTION
END CLASS

d = NEW Dog("Rex")                         ' Dog has no SUB New: Animal's is used
PRINT d.Introduce()                        ' Rex says Woof (...)
PRINT TypeOf d Is Animal, TypeName(d)      ' 1 Dog
```

- A derived class has all of its base class's fields and methods, and can add its own. A method with the same name **overrides** the base's. Methods are always virtual: code in the base class (`Speak()` above) calls the object's own version. `OVERRIDABLE` / `OVERRIDES` are optional, as documentation.
- `MYBASE.Method(...)` calls the base class's version, and `MYBASE.New(...)` runs the base constructor from a derived `SUB New`. A class without its own `SUB New` uses its base's.
- Field initial values run base first, then derived. A derived class can't declare a field its base already has.
- `TypeOf x Is ClassName` is 1 for objects of that class or of classes derived from it. `TYPENAME(x)` gives the exact class.
- A class inherits from one class. Unknown base classes and inheritance cycles are compile errors.

### ✅ Error Handling (TRY / CATCH / FINALLY)
**Status:** Implemented (bytecode VM, `--native` and `--llvm`)

```kayte
TRY
    PRINT ParseAge("abc")
CATCH e                                  ' or CATCH e AS Exception
    PRINT "invalid:", e                  ' e is the message; e.Message works too
END TRY

FUNCTION ParseAge(text)
    IF NOT IsNumeric(text) THEN THROW "'" & text & "' is not a number"
    ParseAge = CInt(text)
END FUNCTION
```

- **What's caught:** any runtime error in the `TRY` block, including errors inside the SUBs and FUNCTIONs it calls, such as division by zero, a bad index, or a missing member. `THROW message` raises your own error, and `THROW NEW Exception("…")` works too.
- **Unwinding:** an error jumps straight to the nearest `CATCH`. Each procedure it leaves gets its local variables back, so recursive code can catch errors.
- **Nesting:** a plain `THROW` inside `CATCH` re-throws, and errors inside a `CATCH` go to the next `TRY` out.
- **Leaving early:** `EXIT FOR` / `EXIT DO`, `CONTINUE`, `RETURN` and `EXIT SUB` can leave a `TRY` block. `GOTO` and labels can't be used inside one.
- **FINALLY:** `TRY … [CATCH …] FINALLY … END TRY` runs the `FINALLY` block however the block is left. That means at the normal end, after the `CATCH`, when an error goes on to an outer `TRY` (no `CATCH`, or the `CATCH` threw), and on `RETURN`, `EXIT SUB` or `EXIT FOR` out of the block. The `FINALLY` block itself can't be left with `EXIT`, `RETURN` or `GOTO`. A `TRY` needs a `CATCH`, a `FINALLY`, or both.
- **Uncaught errors:** an error with no `TRY` ends the program with `Runtime Error: …` and exit code 1.
- **WebAssembly:** `TRY` uses WebAssembly exception handling there, so run programs that have a `TRY` with `wasmtime -W exceptions=y`.

### ✅ Output & Dialogs
**Status:** Implemented

`PRINT a, b` writes its arguments on one line, separated by spaces. `;` joins them with nothing in between (`PRINT "a"; 1` gives `a1`), and a `,` or `;` at the end leaves the line open for the next `PRINT`.

`INPUT ["prompt" {; | ,}] var [, var ...]` reads a line from the keyboard (stdin), showing the prompt and `? ` (none after a `,`). A whole number is stored as a number, an empty line as 0, and anything else as a string. A `name$` variable always gets the string. With several variables, the line's comma-separated parts go into them in turn. `LINE INPUT ["prompt";] var$` stores the whole line as it is. Both work in `--native` and `--llvm` executables too.

`MSGBOX "text"` shows a message (in the console VM it prints `[MsgBox] text`). GUI programs use the `QT` statement instead (see below).

### ✅ Process Execution
**Status:** Implemented (all platforms except iOS / tvOS and WebAssembly)

The `PROCESS` statement runs another program. The first expression is the executable, and any further comma-separated expressions are passed as separate arguments. There's no shell interpretation, so arguments need no quoting or escaping. Add an optional `TO <variable>` clause to capture the output instead of printing it.

```kayte
PROCESS "echo", "Hello from Kayte!"   ' prints the output
PROCESS "whoami" TO currentUser       ' captures it
PRINT currentUser
```

### ✅ JavaScript-like Front End (.kjs)
**Status:** Implemented (bytecode VM, `--native`, `--llvm`, iOS)

Files ending in `.kjs` (or `.js`) are compiled by a JavaScript-like front end (`source/jsfrontend.pas`) to the same bytecode as `.kayte` files:

```javascript
const LIMIT = 10;
let total = 0;
for (let i = 1; i <= LIMIT; i++) {
  if (i % 2 == 0) continue;           // odd numbers only
  total += i * i;
}
console.log("sum of odd squares:", total);   // 165

function fib(n) {
  let a = 0, b = 1;
  for k from 1 to n {                 // Kayte's counting loop also works
    let t = a + b;
    a = b;
    b = t;
  }
  return a;
}
print("fib(20) =", fib(20));          // 6765
print(fib(10) > 50 && fib(10) % 5 == 0 ? "yes" : "no");

const who = process("whoami");        // PROCESS / QT / QML return their result
```

What it supports:

- **Declarations:** `var` / `let` / `const`, and `function` declarations with `return` values.
- **Control flow:** `if` / `else`, `while`, `do … while`, `for (…;…;…)` and `for i from A to B [step S]`, `break` / `continue`, blocks, and `call f()` as a call statement.
- **Operators:** `+ - * / %`, `== != === !==`, `< > <= >=`, `&& || !`, `?:`, `+= -= *= /= %=`, `++ --`.
- **Values:** integers; strings in `"…"`, `'…'` or `` `…` `` with `\n \t \" …` escapes; `true` / `false` / `null`.
- **Comments:** `//` and `/* */`. Semicolons are optional.
- **Built-ins:** `print` / `console.log`, `alert` / `msgbox`, and `qt(…)` / `qml(…)` / `process(…)`. Those three are the `QT`, `QML` and `PROCESS` statements: as a statement `process` prints the output, and in an expression it returns it.

GUIs work the same as from BASIC: a JS `function` is a SUB, so `qt("on", btn, "clicked")` and `.kfm` handlers like `onclick: handleLogin()` call it. See `examples/kfm_login.kjs`.

Differences from JavaScript:

- **Numbers** are 64-bit integers, so `/` divides integers. `+` concatenates as soon as either side is a string, as in JS (`1 + 2 + "a"` is `"3a"`).
- **`&&` and `||`** give `1` / `0`, not one of their operands (`0 || 3` is `1`). They do short-circuit.
- **Scope:** parameters and `var` / `let` / `const` inside a function are local to that function, and other names are globals. Declare a local before you use it. Blocks don't create their own scope.
- **Recursion works:** each call gets its own parameters and locals (`function fib(n) { return n < 2 ? n : fib(n - 1) + fib(n - 2); }`). Recursion deeper than 10,000 calls stops with a "call stack overflow" error. See `examples/recursion.kjs`.
- **No arrays, objects, closures, classes, floats or `${…}` templates** yet. These are reported as errors, as are calls to undefined functions and wrong argument counts. All errors are reported with `file:line:column`, as in BASIC.

### ✅ QuickBASIC Mode (--qbs)
**Status:** Implemented (bytecode VM, `--native` and `--llvm`)

`--qbs` compiles QuickBASIC / QBasic programs (usually `.bas` files) with the BASIC front end in a QuickBASIC-compatible mode:

```bash
kayte --qbs --native examples/quickbasic/report.bas -o report && ./report
kayte --qbs --compile game.bas -o game.bytecode && kayte --run game.bytecode
```

```basic
10 CLS: COLOR 14: PRINT "SCORES": COLOR 7
TYPE Player
  pname AS STRING * 10
  score AS INTEGER
END TYPE
DIM SHARED team(2) AS Player
FOR i = 0 TO 2: READ team(i).pname, team(i).score: NEXT
PRINT team(1).pname; team(1).score;      ' Bob 20  (no newline yet)
PRINT TAB(30); "done"
INPUT "Your name"; n$
IF n$ = "" THEN 10
DATA Ann, 10, Bob, 20, "Cy, Jr", 30
```

What's QuickBASIC-specific in this mode:

- **Program layout:** line numbers are labels (`GOTO 100`, `GOSUB 100`, `IF x THEN 100`, `ON n GOTO 10, 20`, `ON n GOSUB …`). `:` separates statements, also after `THEN` / `ELSE`, after `CASE x:`, and in `WHILE … : WEND` and `FOR … : NEXT` on one line. `END`, `SYSTEM` and `STOP` end the program.
- **Names:** names can end in `$ % & ! #`. `name$` variables start as `""`. Numbers can be written `&HFF` / `&O17`, or with a suffix (`10&`).
- **PRINT:** QuickBASIC formatting: numbers print as `" 5 "` / `"-5 "`, `;` puts nothing between items, `,` moves to the next 14-column zone, and `TAB(n)` / `SPC(n)` position the text. A trailing `;` or `,` keeps the line open, and `?` means `PRINT`. `WRITE` prints comma-separated values with strings in quotes.
- **Input:** `INPUT` and `LINE INPUT` (as described above).
- **PRINT USING:** `PRINT USING format$; items` formats with QuickBASIC's fields:
  - numbers: `#` digits, `.`, `,` (thousands), leading `+`, trailing `+` / `-`, `$$`, `**`, `**$` and `^^^^` (exponent);
  - strings: `!` (the first character), `&` (all of it) and `\  \` (a fixed width);
  - `_` makes the next character literal.

  A number too wide for its field prints with a leading `%`, and the format is reused when there are more items than fields. Example: `PRINT USING "Total: $$#,###.##"; 1234.5` prints `Total:  $1,234.50`.
- **Files:** sequential files, as in QuickBASIC:
  - `OPEN file$ FOR INPUT | OUTPUT | APPEND AS #n`, or the older `OPEN "O", #n, file$`;
  - `CLOSE [#n, ...]`, which closes every file when given no number;
  - `PRINT #n, ...` (also `PRINT #n, USING ...`) and `WRITE #n, ...`, which quotes strings and separates with commas;
  - `INPUT #n, a, b$`, which reads the next comma- or line-separated fields (quoted or not), and `LINE INPUT #n, a$`;
  - `EOF(n)`, `LOF(n)` (the file's size), `FREEFILE`, `KILL file$` and `NAME old$ AS new$`.
- **BINARY and RANDOM files:**
  - opening: `OPEN f$ FOR BINARY AS #n`, `OPEN f$ FOR RANDOM AS #n LEN = reclen` (default 128 bytes), or `OPEN f$ AS #n`. The file is created if missing.
  - `GET #n, [position], variable` and `PUT #n, [position], variable` read and write a record number (RANDOM) or a byte position (BINARY). Without a position they use the next record or the current position.
  - The variable's type decides the bytes, little-endian as in QuickBASIC:
    - `INTEGER` / `%` is 2 bytes, `LONG` / `&` 4, `SINGLE` / `!` 4, `DOUBLE` / `#` 8;
    - `STRING * n` is n bytes, padded with spaces; a `name$` uses its own length (as many bytes as it holds);
    - a `TYPE` record writes its fields in order, nested TYPEs included, so the files are byte-compatible with QuickBASIC's.
    - A number without a type is 8 bytes.
  - Positioning: `SEEK #n, position`, `SEEK(n)` (the next record / byte), `LOC(n)` (the last record / the current byte), `LOF(n)`, and `EOF(n)` (no more records or bytes after the current position).
  - Bytes: `INPUT$(count, #n)` reads raw bytes; `MKI$` / `MKL$` / `MKS$` / `MKD$` and `CVI` / `CVL` / `CVS` / `CVD` convert numbers to their bytes and back.
  - `LEN(record)` is a TYPE's size in bytes, for `LEN = LEN(record)`.
  - A record longer than `LEN`, or a number too large for its bytes (`40000` in an INTEGER), is a runtime error.
- **File errors and details:** reading past the end, or using a file that isn't open, is a runtime error (trappable with `ON ERROR`). Files are flushed and closed when the program ends, even on an error. Text lines end in `\n`, and a `\r\n` from a DOS file is accepted. On WebAssembly, give the program access to its directory (`wasmtime --dir=. app.wasm`).
- **Screen:** `CLS`, `LOCATE row, col` and `COLOR fg, bg` (the 16 QuickBASIC colors) use ANSI terminal codes. `BEEP` and `SLEEP [seconds]` work, and a plain `SLEEP` waits for Enter.
- **Data:** `DATA`, `READ` and `RESTORE [label]`.
- **Truth values and bit operators:** comparisons give `-1` for true and `0` for false, and `AND`, `OR`, `XOR`, `NOT`, `EQV` and `IMP` work bit by bit on integers, as in QuickBASIC: `NOT 0` is `-1`, `6 AND 3` is `2`, and `IF flags AND 4 THEN` tests a bit. Both sides are always evaluated (no short-circuit). `TRUE` and `FALSE` are ordinary names, so the usual `CONST TRUE = -1, FALSE = 0` works.
- **DEF FN:** one-line functions (`DEF FNarea (w, h) = w * h`) and block ones (`DEF FNmax (a, b)` … `FNmax = …` … `END DEF`, with `EXIT DEF`). Their parameters are local; other names are the program's variables, as in QuickBASIC. `DEF SEG` is ignored.
- **String statements:** `MID$(s$, start[, length]) = text$` overwrites characters in place without changing the length. `LSET` / `RSET` `s$ = text$` left- or right-justify the text in `s$`'s current width (padded with spaces or cut), also on TYPE fields.
- **Error trapping:**
  - `ON ERROR GOTO label` jumps to a handler on a runtime error; `ON ERROR GOTO 0` turns trapping off, and an error then stops the program as usual.
  - In the handler, `ERR` is the QuickBASIC error code (11 division by zero, 9 subscript out of range, 53 file not found, 62 input past end, 52 bad file number, 5 illegal function call …), and `ERL` is the last line number reached.
  - `RESUME` (or `RESUME 0`) retries the failing statement, `RESUME NEXT` continues with the next statement, and `RESUME label` continues at a label. `RESUME` outside a handler is error 20.
  - `ON ERROR RESUME NEXT` skips every failing statement.
  - `ERROR n` raises error `n`. Errors raised inside a SUB or FUNCTION are trapped by the program's handler and resume at the statement that called it.
  - Handlers and `ON ERROR` are at program level, as in QuickBASIC (a SUB can't have its own handler).
- **Types:** `TYPE … END TYPE` records. `DIM p AS Point` and `DIM a(n) AS Point` create the records (fields `AS STRING` start as `""`, nested TYPEs are created too).
- **Variable scope:** variables in a `SUB` / `FUNCTION` are local unless they're `DIM SHARED`, `COMMON SHARED` or top-level `CONST`, or named in `SHARED` inside the SUB. `STATIC` variables keep their values between calls.
- **Arrays:** `DIM a(1 TO 10)`, `SWAP a, b`, `ERASE a`, and `a()` to pass a whole array.
- **Numbers:** doubles as in the main language (QuickBASIC's single precision is a double too). They print as QuickBASIC does (`PRINT .5` gives ` .5 `), and `INT(RND * 6) + 1` works.
- **Built-in functions:** `STR$` gives a leading space for numbers ≥ 0, as in QuickBASIC. Also `TIMER`, `DATE$`, `TIME$`, `INKEY$` (always `""`), `INT`, `FIX`, `SQR`, `STRING$`, `HEX$`, `OCT$` and `^`.
- **Ignored:** `DECLARE`, `DEFINT` and the other `DEF` types, `RANDOMIZE`, `WIDTH`, `KEY`, `VIEW PRINT`, `SCREEN 0` and `OPTION BASE`.
- **Reserved words:** Kayte's own statements (`CLASS`, `TRY`, `SHOW`, `QT`, `PROCESS` …) are ordinary names in this mode.

Not supported yet, each reported at compile time:
- `FIELD` buffers (GET / PUT a TYPE record instead, or use `MKI$` / `CVI` …);
- graphics and sound (`SCREEN 1+`, `LINE`, `PSET`, `CIRCLE`, `PLAY`, `SOUND`).

This also behaves differently from QuickBASIC: a TYPE assignment `p = q` shares the record instead of copying it.

See `examples/quickbasic/`.

### ✅ Compiler Errors

The compiler reports every error in a file with its `line:column`, then stops. Nothing is written (no bytecode, C or IR), and `kayte` exits with status 1:

```
Parser Error: Unexpected token: NEXT at 4:1 (Token: "NEXT" Type: KEYWORD)
Parser Error: Call to undefined SUB or FUNCTION "MISSING" at line 5
Error: 2 error(s) - compilation stopped, nothing was written
```

A lexer error, such as a character the language doesn't use, stops at the first one.

Every failure exits with a non-zero status, so scripts and CI can rely on it: compile errors, a runtime error under `--run` (or in a `--native` / `--llvm` executable), a missing input file, or a file that isn't bytecode.

---

## 🎨 GUI: Qt6 Widgets, QML and Form Files

**Status:** Implemented (bytecode VM, `--native` and `--llvm`; requires Qt 6)

The `QT` statement builds native Qt6 windows: widgets, layouts, menus, tabs, dialogs, timers, and event handlers written as SUBs.

```kayte
QT "init"
QT "window", "Hello", 300, 120 TO win
QT "vbox", win TO main
QT "button", win, "Click me" TO btn
QT "add", main, btn
QT "on", btn, "Clicked"
QT "show", win
QT "run"

SUB Clicked()
    QT "message", "Kayte", "Hello from Qt6!"
END SUB
```

### ✅ QML

UIs can also be written in **QML** with the `QML` statement. It loads a `.qml` file, finds its objects by `objectName`, and drives them with `get` / `set` / `call`. QML signals reach SUBs through `connect` + `on`, and no `init` is needed:

```kayte
QML "load", "examples/qml/todo.qml" TO win
QML "find", win, "addButton" TO addBtn
QML "connect", addBtn, "clicked" TO added
QML "on", added, "AddTask"
QML "show", win
QML "run"

SUB AddTask()
    QML "call", win, "addTask", "Learn Kayte"
END SUB
```

### ✅ Form Files (.kfm and Qt Designer .ui)

A window can be described in a form file and loaded in one statement, the way Qt loads a Designer `.ui` file. The file describes the widgets and names the handler SUBs, and the script holds the logic:

```
// login.kfm
form LoginWindow {
  title: "User Login"
  width: 400
  height: 250
  layout: VBox {
    label { text: "Please enter your credentials", align: Center }
    textfield { id: "usernameInput", placeholder: "Username" }
    textfield { id: "passwordInput", placeholder: "Password", type: Password }
    button { id: "loginButton", text: "Log In", onclick: handleLogin() }
    label { id: "messageLabel", color: "red" }
  }
}
```

```kayte
QT "init"
QT "loadform", "login.kfm" TO win
QT "find", win, "usernameInput" TO userBox   ' widgets are found by id
QT "show", win
QT "run"

' Attached by loadform (onclick: handleLogin() in login.kfm)
SUB handleLogin()
    QT "gettext", userBox TO who
    QT "message", "Hello", who
END SUB
```

Form files support:
- **Declarative `.kfm`:** layouts (`VBox`, `HBox`, `Grid`, `Form`), most common widgets including tabs, groups and menus, properties such as placeholders, password fields, items and colours, and `onclick` / `onchange` / `onenter` handlers.
- **INI-style `.kfm`:** the format `source/KfmParser.pas` writes. Controls are placed by pixel, and VB-style `Button1_Click` SUBs are wired automatically.
- **Qt Designer `.ui`:** loaded through Qt's UiTools.

Mistakes are reported with the file and line, e.g. `login.kfm:3: unknown property "txt" for a button`.

### ✅ iOS Apps

QT and QML programs also build into **iOS apps**, for the Simulator or a device. Files the program loads are bundled with `--resource`:

```bash
scripts/build-kayte-ios.sh examples/qml_todo.kayte --resource examples/qml --run
scripts/build-kayte-ios.sh examples/kfm_login.kjs --resource examples/form1.kfm --device --team <TEAM_ID>
```

The Simulator on Apple Silicon needs Qt built for the arm64 Simulator, once, with `scripts/build-qt-ios-simulator.sh`. tvOS isn't supported for GUIs, because Qt 6 has no tvOS port.

Build the Qt bridge library with `scripts/build-kayte-qt6.sh`. See [source/qt6/README.md](source/qt6/README.md) for the full command reference, form-file syntax and iOS details.

### ✅ Compiling .kfm Forms to a Library
**Status:** Implemented

Besides being loaded at run time, an INI-style `.kfm` form can be compiled into a standalone `.so` / `.dylib` / `.dll` / `.a` library that any C-compatible application can load, independent of Kayte's VM.

```bash
# Compiles examples/vbform.kfm into build/kfm/vbform/{vbform.dylib,vbform.so,vbform.dll,libvbform.a}
scripts/build_kfm_lib.sh examples/vbform.kfm
```

The library exports a small C API (generated by `source/kfmlibgen.lpr`):

```c
int kfm_form_name(char *buf, int bufLen);
int kfm_control_count(void);
int kfm_control_name(int index, char *buf, int bufLen);
int kfm_control_type(int index);
int kfm_get_property(const char *controlName, const char *propName, char *buf, int bufLen);
```

`scripts/build_kfm_lib.sh` builds the native host library plus a `.a` archive. It also cross-builds for Linux and Windows when those toolchains are available.

---

## ⚡ Compilation & Execution

All backends share one value model, so a program behaves the same however it's run.

### ✅ Bytecode VM
**Status:** Implemented

```bash
kayte --compile myapp.kayte -o myapp.bytecode   # or just: kayte myapp.kayte
kayte --run myapp.bytecode
```

`--run` checks the file it's given. A native executable or any other non-bytecode file gets a clear error instead of being run.

### ✅ Native Compilation (via C)
**Status:** Implemented (macOS, Linux; needs a C compiler)

`--native` translates the program's bytecode to C. It then builds it with the system C compiler (`cc`, or `$KAYTE_CC`), together with a small runtime (`source/native/kayte_native_rt.c`) that implements the same value model and statements as the VM.

```bash
kayte --native myapp.kayte -o myapp
./myapp

kayte --native myapp.kayte -o myapp --keep-c    # also writes myapp.c
kayte --native myapp.kayte -o myapp.c           # only the C
```

- 📦 **Standalone**: no interpreter is needed. A typical program is about 40 KB and links only the C library. Qt programs load `libkayte_qt6` at run time, from `$KAYTE_QT6_LIB`, next to the executable, or the library kayte used when compiling.
- 🚀 **Faster**: about 5–15× faster than the bytecode VM on an integer-loop benchmark (3M iterations: 1.3 s with `--run`, 0.08–0.28 s native).
- 🔎 The runtime source is found via `$KAYTE_NATIVE_RT`, next to the kayte executable, or in `source/native` of the repository.

### ✅ LLVM Backend
**Status:** Implemented (macOS, Linux, Windows, iOS / tvOS / watchOS / visionOS, WebAssembly; needs clang)

`--llvm` compiles through **LLVM IR** instead of C, and `--target <triple>` cross-compiles for another platform. It uses the same runtime as `--native`.

```bash
kayte --llvm myapp.kayte -o myapp                                   # this machine
kayte --llvm myapp.kayte --target x86_64-w64-mingw32 -o myapp.exe   # Windows (also aarch64-, i686-)
kayte --llvm myapp.kayte --target x86_64-apple-macos12 -o myapp     # Intel Mac
kayte --llvm myapp.kayte --target arm64-apple-ios17.0-simulator -o myapp
kayte --llvm myapp.kayte --target wasm32-wasi -o myapp.wasm         # run with: wasmtime myapp.wasm
kayte --llvm myapp.kayte --target aarch64-linux-gnu -o myapp        # needs a Linux sysroot, see below
kayte --llvm myapp.kayte -o myapp.ll                                # only the IR
```

How each kind of target finds its toolchain:

- **Host:** `clang`, or `$KAYTE_CLANG`.
- **Apple platforms:** the SDK from Xcode (`xcrun --sdk …`).
- **Windows:** a `<triple>-clang` cross compiler on `PATH`, such as [llvm-mingw](https://github.com/mstorsjo/llvm-mingw)'s `x86_64-w64-mingw32-clang`.
- **WebAssembly:** the WASI sysroot from `brew install wasi-libc wasi-runtimes`, or `$KAYTE_WASI_SYSROOT`.
- **Linux from another OS:** `lld` plus a Linux sysroot: `KAYTE_LLVM_FLAGS="--sysroot=/path/to/sysroot"`. On Linux itself nothing extra is needed.

Extra clang flags go in `$KAYTE_LLVM_FLAGS`, and `--keep-c` keeps the `.ll` next to the output.

Per-platform notes:
- **Windows:** `PROCESS` uses `CreateProcess` (no shell, as on POSIX), and `QT` loads `kayte_qt6.dll`.
- **WebAssembly:** has no processes or Qt, so `PROCESS` and `QT` stop with a runtime error. A program with `TRY` is built with WebAssembly exception handling, so it needs a runtime that has it (`wasmtime -W exceptions=y`).
- **iOS / tvOS:** `PROCESS` is unavailable, and QT apps for iOS are built with `scripts/build-kayte-ios.sh`.

**Experimental direct ARM64 emitter:** `--native-arm64` is an older backend that writes Mach-O machine code directly (`source/KayteArm64.pas`, `source/kayte_arm64_emit.c`). It supports only a few integer opcodes, and current macOS refuses to launch the static executables it produces. It's kept for development of that backend only.

---

## 🧩 Other Components

### HTTP Server
**Status:** Optional build

`kayte --http` starts a simple HTTP server on port 9090 (`source/simplehttpserver.pas`). It's left out of default builds; rebuild with `-dKAYTE_HTTP` to enable it. Kayte scripts can't define routes yet.

### REPL
**Status:** Implemented

`kayte --repl` starts an interactive Read-Eval-Print Loop.

### JVM Bridge
**Status:** Experimental, separate component

`jvm/` contains a Java implementation of the Kayte VM (`KayteVM.java`, built with Maven) and a Pascal JNI wrapper (`jvm/jvm.pas`) that can hand bytecode to it. It isn't reachable from Kayte scripts.

### Other Programs in `source/`
**Status:** Separate tools, built on their own

- **webgen** (`source/webgen.lpi`): a static site generator built on the Kayte toolchain. `lazbuild source/webgen.lpi` builds `bin/webgen`; see [docs/webgen.md](docs/webgen.md).
- **vb6interpreter** (`source/vb6interpreter.lpi`): an experimental VB6 interpreter with LCL forms (KFRM, in `kfrm/`). It runs `program.bas` from the current directory (an example is `examples/program.bas`). On current macOS it doesn't link: Xcode's linker rejects a Lazarus Cocoa unit (`cocoawsextctrls.o`). Linux builds go through `scripts/build-kayte-debian-container.sh vb6`.
- **mathlibdylib** (`source/mathlibdylib.lpr`): the math library (`source/mathlib.pas`) as a C-callable dynamic library, built with `scripts/build_mathlib_dylib.sh` (into `lib/libmathlib.dylib`).
- **kfmlibgen** (`source/kfmlibgen.lpr`): compiles `.kfm` forms to libraries; used by `scripts/build_kfm_lib.sh` (see [Compiling .kfm Forms to a Library](#-compiling-kfm-forms-to-a-library)).

---

## 🛠️ Command-Line Reference

```bash
# <file> is .kayte (BASIC-style) or .kjs / .js (JavaScript-like)
kayte --compile <file> [-o out.bytecode]   # compile to bytecode
kayte <file.kayte>                         # same, output next to the source
kayte --run <file.bytecode>                # run bytecode on the VM
kayte --native <file> [-o out] [--keep-c]  # native executable via C (-o x.c: only the C)
kayte --llvm <file> [--target <triple>] [-o out] [--keep-c]   # via LLVM (-o x.ll: only the IR)
kayte --qbs ...                            # the file is QuickBASIC (with --compile, --native, --llvm)
kayte --native-arm64 <file>                # experimental direct ARM64 emitter
kayte --repl                               # interactive REPL
kayte --http                               # HTTP server (builds with -dKAYTE_HTTP)
kayte --verbose ...                        # show compiler commands
kayte --version | --help
```

Environment variables:

| Variable | Used for |
|---|---|
| `KAYTE_CC` | C compiler for `--native` (default `cc`) |
| `KAYTE_CLANG` | compiler for `--llvm` (default `clang`) |
| `KAYTE_LLVM_FLAGS` | extra clang flags for `--llvm`, e.g. `--sysroot=…` |
| `KAYTE_WASI_SYSROOT` | WASI sysroot for `--target wasm32-wasi` |
| `KAYTE_NATIVE_RT` | directory containing `kayte_native_rt.c` |
| `KAYTE_QT6_LIB` | path to `libkayte_qt6` for QT programs |

### IDE Support

**KayteIDE**, the official Kayte IDE: https://github.com/ringsce/kayteide

---

## 📊 Example Programs

All of these compile cleanly with the current compiler. Run them from the repository root, because paths to `.qml` and `.kfm` files are relative to the current directory:

| Example | Shows |
|---|---|
| `examples/qt6_hello.kayte` | a window with pixel-positioned widgets, polling `wait` loop |
| `examples/qt6_widgets.kayte` | an order form using most widgets, in layouts |
| `examples/qt6_layouts.kayte` | a resizable contact book |
| `examples/qt6_menus_tabs.kayte` | a notes app with menus and tabs |
| `examples/qt6_callbacks.kayte` | a to-do list with SUB event handlers |
| `examples/qml_hello.kayte` | inline QML |
| `examples/qml_todo.kayte` | a to-do list in QML (`examples/qml/todo.qml`) |
| `examples/kfm_login.kayte` | a login form from `examples/form1.kfm` |
| `examples/ui.kayte` | a window from `examples/ui.kfm` |
| `examples/control_flow.kayte` | block `IF` / `ELSEIF` / `ELSE`, `FOR … NEXT` with `STEP`, nesting |
| `examples/functions.kayte` | `FUNCTION` return values, local variables, recursion (`Fib`, `Fact`, `IsEven` / `IsOdd`) |
| `examples/byref.kayte` | `ByRef` parameters: `Swap`, output parameters, struct fields, recursion |
| `examples/loops_goto.kayte` | `DO … LOOP` (all five forms), `EXIT DO`, `CONTINUE`, `GOTO` / `GOSUB` and labels, `END`, `:` between statements |
| `examples/gosub.kayte` | `GOSUB` / `RETURN` at the top level and in SUBs / FUNCTIONs, nested, with `END` |
| `examples/operators.kayte` | `AND` / `OR` / `XOR` / `NOT`, `MOD` (FizzBuzz), `\`, `SELECT CASE`, `EXIT FOR` / `EXIT WHILE` / `EXIT SUB` |
| `examples/numbers.kayte` | floating-point numbers: interest, statistics, trigonometry, rounding, `/` vs `\` |
| `examples/arrays.kayte` | arrays (`DIM`, `REDIM PRESERVE`, several dimensions, `FOR EACH`, sorting), string and array functions |
| `examples/classes.kayte` | classes: fields, constructors, methods, properties, objects in arrays, a linked list |
| `examples/try_catch.kayte` | `TRY` / `CATCH` / `FINALLY` / `THROW`: runtime errors, errors from FUNCTIONs, re-throwing, retries, clean-up |
| `examples/inheritance.kayte` | `INHERITS`, overriding, `MYBASE`, inherited constructors, `TypeOf … Is` |
| `examples/quickbasic/report.bas` | QuickBASIC (`--qbs`): `TYPE`, `DATA` / `READ`, line numbers, `GOSUB`, `ON … GOTO`, `PRINT ;` and `TAB`, `DIM SHARED` |
| `examples/quickbasic/inventory.bas` | QuickBASIC (`--qbs`): `OPEN` / `WRITE #` / `INPUT #` / `EOF`, `PRINT USING` reports to the screen and a file |
| `examples/quickbasic/records.bas` | QuickBASIC (`--qbs`): a RANDOM file of `TYPE` records (`GET` / `PUT`, update in place, `LOC`), a BINARY dump with `INPUT$`, `CVI` / `CVD` |
| `examples/quickbasic/errors.bas` | QuickBASIC (`--qbs`): `ON ERROR GOTO` / `RESUME` / `RESUME NEXT`, `ERR` / `ERL`, `ERROR n`, `DEF FN`, bitwise `AND` / `OR`, `MID$ =`, `LSET` / `RSET` |
| `examples/quickbasic/guess.bas` | QuickBASIC (`--qbs`), interactive: `INPUT`, `LINE INPUT`, `CLS`, `COLOR`, `RND` |
| `examples/variables.kayte` | `DIM`, variables and types |
| `examples/hello.kjs` | hello world in the JavaScript-like syntax |
| `examples/calculator.kjs` | functions, locals and globals, `for … from … to` |
| `examples/recursion.kjs` | recursive functions in `.kjs`: `fib`, `gcd`, `power`, mutual recursion |
| `examples/kfm_login.kjs` | the login form, driven from `.kjs` |

```bash
bin/kayte --compile examples/qt6_callbacks.kayte -o todo.bytecode
bin/kayte --run todo.bytecode
```


---

## 📌 Roadmap

### ✅ Completed (v0.9)
- Core language: variables, expressions with `AND` / `OR` / `XOR` / `NOT`, `MOD` and `\`, block and single-line `IF` / `ELSEIF` / `ELSE`, `SELECT CASE`, `FOR … NEXT`, `WHILE`, `DO … LOOP`, `EXIT` and `CONTINUE` for `FOR` / `WHILE` / `DO`, `GOTO` and labels, `SUB` and `FUNCTION` with return values, local variables, recursion and `ByRef` parameters, `STRUCT`, `DIM`, `PRINT`
- **Arrays** (`DIM a(n)`, several dimensions, `REDIM PRESERVE`, `FOR EACH`), **built-in functions** (strings, numbers, arrays), **classes** (fields, methods, constructors, properties, inheritance), **`TRY` / `CATCH` / `FINALLY` / `THROW`**, `INPUT`, **floating-point numbers**
- **QuickBASIC mode** (`--qbs`): line numbers, QuickBASIC `PRINT` and `PRINT USING`, sequential, `BINARY` and `RANDOM` files, `DATA` / `READ`, `TYPE`, SUB-local variables, `CLS` / `LOCATE` / `COLOR`, `ON ERROR` / `RESUME`, `DEF FN`, -1 truth values and bitwise operators
- **JavaScript-like front end** (`.kjs`): `let` / `const`, functions with return values, `if` / `else`, `for`, `do … while`, `&&` / `||`, `?:`, `%`, compound assignment
- Bytecode VM with bytecode files
- **Native compilation via C** (`--native`) and an **LLVM backend** (`--llvm`) with cross-compilation for macOS, Linux, Windows, iOS / tvOS / watchOS / visionOS and WebAssembly (WASI)
- **Qt6 GUIs**: widgets, layouts, menus, tabs, dialogs, timers, SUB event handlers (`QT`)
- **QML** UIs (`QML` statement)
- **Form files**: `.kfm` (declarative and INI) and Qt Designer `.ui`, loaded with `QT "loadform"`
- **iOS apps** for QT / QML programs (`scripts/build-kayte-ios.sh`)
- **Process execution** (`PROCESS`)
- **musl libc builds** (Linux ARM64 & AMD64, statically linked)
- **.kfm form compiler** to `.so` / `.a` / `.dylib` / `.dll` with a C-callable API
- REPL, optional HTTP server, experimental JVM bridge

### 🚧 Not Yet Supported

These are designed but the compiler doesn't accept them yet:

- **Generic definitions** (`STRUCT Box<T>`, `SUB Show<T>`). Type arguments after `AS` already parse.
- **JavaScript extras**: floating-point numbers (in `.kjs`, `/` divides whole numbers), arrays, objects, built-in functions, `try` / `catch`, classes, closures, `${…}` template strings, `foreach` / `for … of`
- QuickBASIC: `FIELD`, graphics, local error handlers in SUBs

### 🎯 Planned (v1.0+)
- **Static type system** with type inference, and statically-checked generics
- **Optimized VM**: JIT compilation for hot paths
- **Standard library**: file I/O in the main language (QuickBASIC mode already has it), networking, utilities
- **Package manager** and **module system**
- **Debugger protocol**: VS Code integration
- **Async/await**
- **FFI**: call C/C++ libraries directly
- **WebAssembly in browsers** (WASI runtimes such as wasmtime already work)
- **Scriptable HTTP server** and **Node.js integration**

---

## 🤝 Contributing

Kayte Lang is open-source and welcomes contributors of all skill levels!

### Ways to Contribute

1. **Core development**: the compiler (`source/parser.pas`), VM (`source/virtualmachine.pas`), native runtime (`source/native/`) and backends
2. **Language features**: anything under [Not Yet Supported](#-not-yet-supported)
3. **Documentation**: guides, tutorials and examples
4. **Testing**: test cases and bug reports
5. **UI/UX**: the IDE and developer tools
6. **Community**: help others learn Kayte Lang

### Getting Started

```bash
# Fork https://github.com/ringsce/kayte-lang on GitHub, then:
git clone https://github.com/YOUR_USERNAME/kayte-lang.git
cd kayte-lang

# Build
scripts/build-kayte-macos.sh          # or: lazbuild source/kayte.lpi

# Create a feature branch, commit, push, and open a pull request
git checkout -b feature/my-awesome-feature
git commit -am "Add awesome feature"
git push origin feature/my-awesome-feature
```

### Code Style

- Match the surrounding code's indentation (2 spaces in Pascal and C++, 4 in the C runtime)
- Follow the existing naming conventions
- Write tests for new features
- Update the documentation

---

## 📚 Resources

### Official Links
- **Website**: https://ringscejs.gleentech.com
- **Documentation**: https://ringscejs.gleentech.com
- **Compiler & VM**: https://github.com/ringsce/kayte-lang
- **Kayte IDE**: https://github.com/ringsce/kayteide
- **Discord Community**: https://discord.gg/d6gV8W2W
- **Video Tutorials**: https://youtube.com/@ringsce

### In This Repository
- **source/qt6/README.md**: Qt6, QML, form files and iOS reference
- **examples/**: example programs
- **CONTRIBUTING.md**, **CODE_OF_CONDUCT.md**
- **[docs/INDEX.md](docs/INDEX.md)**: overview of the build system files
- **[docs/QUICKSTART_KAYTE.md](docs/QUICKSTART_KAYTE.md)**: quick start for building with musl
- **[docs/README_KAYTE_MUSL.md](docs/README_KAYTE_MUSL.md)**: complete musl build documentation
- **scripts/setup_and_build_kayte.sh**, **scripts/build_kayte_musl.sh**, **Makefile.kayte**, **scripts/musl/**: musl build files

---

## 📄 License

Kayte Lang is released under the MIT License. See the LICENSE file for details.

---

## 🌟 Acknowledgments

Kayte Lang is built with love by the open-source community. Special thanks to all contributors who have helped shape this project.

---

**Current Version:** 0.9.10 (Beta)
**Last Updated:** October 2026
