# Kayte Lang: A Modern, Cross-Platform Programming Language

Kayte Lang is a modern experimental programming language designed for building applications quickly, efficiently, and cross-platform. It compiles into bytecode that runs on the Kayte Virtual Machine (KVM), ensuring portability across macOS, Linux, and Windows.

---

## 🎯 Quick Start

```bash
# Clone the repository
git clone https://github.com/ringsce/kayte-lang.git
cd kayte-lang

# Compile a Kayte program
kaytecc hello.kayte -o hello.kbyte

# Run the bytecode
kayte hello.kbyte

# Or compile to native ARM64 (macOS)
kaytecc hello.kayte --native -o hello
./hello
```

---

## 🔧 Building from Source

### Requirements

**All Platforms:**
- FreePascal 3.2.2 or newer
- Git

**For musl builds (Linux):**
- musl cross-compilation toolchains (installed automatically by setup script)
- wget (for downloading toolchains)

### Build Options

**Standard Build:**
```bash
make build
```

**musl Build (Linux - Portable, Static Binaries):**
```bash
# All-in-one: setup toolchains and build
sudo ./setup_and_build_kayte.sh both

# Or if toolchains already installed
./build_kayte_musl.sh both

# Or using Make
make -f Makefile.kayte all
```

**What is musl?**
musl libc builds create statically-linked binaries that:
- Have no runtime dependencies
- Run on any Linux distribution
- Are 20-30% smaller than glibc builds
- Support both ARM64 and AMD64 architectures

For complete musl build documentation, see **README_KAYTE_MUSL.md**.

---

## 🚀 Core Features

### ✅ Functions
**Status:** Fully Implemented

Function definition and calls are fully functional with support for parameters and return values.

```kayte
function greet(name) {
    print("Hello, " + name + "!");
    return "Success";
}

function calculate(a, b) {
    return a + b;
}

var result = calculate(10, 20);
```

### ✅ Variables & Data Types
**Status:** Fully Implemented

Variable declaration, assignment, and multiple data types are supported.

```kayte
var name = "Kayte Lang";
var version = 1.0;
var isActive = true;
var numbers = [1, 2, 3, 4, 5];
var person = {
    name: "John",
    age: 30
};
```

### ✅ Conditionals
**Status:** Fully Implemented

Efficient conditional branching with `if`, `else if`, and `else` statements. The compiler uses optimized jump instructions for maximum performance.

```kayte
if (score >= 90) {
    print("Grade: A");
} else if (score >= 80) {
    print("Grade: B");
} else if (score >= 70) {
    print("Grade: C");
} else {
    print("Grade: F");
}
```

### ✅ Loops
**Status:** Fully Implemented

Both `for` and `foreach` loops provide robust iteration capabilities.

```kayte
// Standard for loop
for (var i = 0; i < 10; i++) {
    print("Number: " + i);
}

// Foreach loop for collections
var fruits = ["apple", "banana", "cherry"];
foreach fruit in fruits {
    print("I have a " + fruit);
}

// While loop
var count = 0;
while (count < 5) {
    print(count);
    count++;
}
```

### ✅ Error Handling
**Status:** Fully Implemented

Robust try-catch mechanism with exception handler stack management.

```kayte
try {
    var result = riskyOperation();
    var data = parseJSON(result);
    print("Success: " + data.message);
} catch (error) {
    print("An error occurred: " + error);
    logError(error);
}
```

### ✅ Built-in Functions
**Status:** Fully Implemented

Optimized built-in functions compiled directly into single bytecode instructions for maximum performance.

```kayte
// String and Array operations
var fruits = ["apple", "banana", "cherry"];
var count = len(fruits);          // Get length
var first = fruits[0];             // Array access
var upper = toUpper("hello");      // String manipulation
var joined = join(fruits, ", ");   // Join array elements

// Type conversions
var num = parseInt("42");
var str = toString(123);
var float = parseFloat("3.14");

// Math operations
var result = abs(-10);
var rounded = round(3.7);
var power = pow(2, 8);
```

### ✅ Generics (Type-Erased)
**Status:** Implemented (parse-time only)

The Kayte VM is dynamically typed (like a BASIC `Variant`) with no static type-checking pass, so `<T, U, ...>` type parameter lists are parsed and discarded rather than specialized or checked — generic code runs exactly like its non-generic equivalent. This lets you write type-parameterized signatures today without the VM enforcing them; see the Roadmap below for statically-checked generics.

Supported after the name in `STRUCT`, `CLASS`, `SUB`, and `FUNCTION` definitions, at `CALL`/`NEW` instantiation sites, and after `AS Type` type names (note: this uses Kayte's actual BASIC-style statement syntax, not the JS-like syntax shown in some examples above):

```kayte
STRUCT Pair<A, B>
    First AS A
    Second AS B
END STRUCT

FUNCTION Identity<T>(x)
    RETURN x
END FUNCTION

DIM p AS Pair<Integer, String>
DIM items AS List<Integer>
```

### ✅ Process Execution
**Status:** Fully Implemented

The `PROCESS` statement spawns an OS process. The first expression is the executable; any further comma-separated expressions are passed as separate arguments (no shell interpretation, so arguments need no quoting/escaping). Add an optional `TO <variable>` clause to capture the process's output instead of printing it straight to the console.

```kayte
' Runs the process and prints its output directly
PROCESS "echo", "Hello from Kayte!"

' Captures output into a variable instead
PROCESS "whoami" TO currentUser
PRINT currentUser
```

---

## 🎨 Declarative UI Framework

### ✅ Window & Component System
**Status:** Fully Implemented

Build cross-platform UIs with a clean, declarative syntax.

```kayte
window main {
    title: "Kayte Lang Demo"
    width: 800
    height: 600
    
    content {
        vbox {
            padding: 20
            spacing: 10
            
            label {
                text: "Welcome to Kayte Lang"
                font: "Arial, 24, bold"
            }
            
            button {
                id: "btnClickMe"
                text: "Click Me!"
                onclick: handleClick()
            }
            
            textfield {
                id: "userInput"
                placeholder: "Enter your name..."
            }
        }
    }
}

function handleClick() {
    var name = getValue("userInput");
    showDialog("Hello, " + name + "!");
}
```

### ✅ .kfm Files
**Status:** Fully Implemented

Separate UI definitions using `.kfm` files for cleaner project organization.

**Example: login.kfm**

```kayte
form LoginWindow {
    title: "User Login"
    width: 400
    height: 300
    centered: true
    
    layout: VBox {
        padding: 30
        spacing: 15
        
        label {
            text: "Please enter your credentials"
            align: center
            font: "Arial, 16"
        }
        
        textfield {
            id: "usernameInput"
            placeholder: "Username"
            icon: "user.png"
        }
        
        textfield {
            id: "passwordInput"
            type: Password
            placeholder: "Password"
            icon: "lock.png"
        }
        
        hbox {
            spacing: 10
            
            button {
                id: "loginButton"
                text: "Log In"
                style: "primary"
                onclick: handleLogin()
            }
            
            button {
                text: "Cancel"
                style: "secondary"
                onclick: closeWindow()
            }
        }
        
        label {
            id: "messageLabel"
            text: ""
            color: "red"
            align: center
        }
    }
}
```

**Usage in code:**

```kayte
import "login.kfm";

function showLoginScreen() {
    var window = createWindow(LoginWindow);
    window.show();
}

function handleLogin() {
    var username = getValue("usernameInput");
    var password = getValue("passwordInput");
    
    if (authenticate(username, password)) {
        closeWindow();
        showMainApp();
    } else {
        setValue("messageLabel", "Invalid credentials");
    }
}
```

### ✅ Compiling .kfm Forms to a Library
**Status:** Implemented

Beyond being read by the runtime, a `.kfm` form definition can be compiled into a standalone shared/static library - a `.so`/`.dylib`/`.dll`/`.a` that any C-compatible host application can load, independent of Kayte's own VM. This targets the bracket-style `.kfm` format (`[FORM:Name]` / `[CONTROL:Name:Type]` / `Key=Value`, as written by `source/KfmParser.pas` and shown in `Form1.kfm`) - not the brace-style syntax in the example above, which is a separate, aspirational declarative-UI syntax without its own compiler yet.

```bash
# Compiles Form1.kfm into build/kfm/Form1/{Form1.dylib,Form1.so,Form1.dll,libForm1.a}
./build_kfm_lib.sh Form1.kfm
```

The library exports a small, fixed C API (generated by `projects/kfmlibgen.lpr`) so any language with C FFI can introspect the compiled-in form:

```c
int kfm_form_name(char *buf, int bufLen);
int kfm_control_count(void);
int kfm_control_name(int index, char *buf, int bufLen);
int kfm_control_type(int index);
int kfm_get_property(const char *controlName, const char *propName, char *buf, int bufLen);
```

`build_kfm_lib.sh` always builds the native host library (`.dylib` on macOS, `.so` on Linux) plus a `.a` static archive, and attempts Linux/Windows cross-builds when the corresponding cross-toolchain is available, mirroring `build_mathlib_dylib.sh`'s pattern for `mathlib.pas`.

---

## ⚡ Compilation & Execution

### ✅ Bytecode VM
**Status:** Fully Implemented

The Kayte Virtual Machine provides fast, portable execution across all platforms.

```bash
# Compile to bytecode
kaytecc myapp.kayte -o myapp.kbyte

# Run bytecode
kayte myapp.kbyte

# View bytecode (debug)
kayte --disassemble myapp.kbyte
```

**Example Bytecode:**

```
0000: LOAD_CONST       0    ; "Button clicked!"
0008: LOAD_NAME        1    ; print
0016: CALL_FUNCTION    1
0024: LOAD_CONST       2    ; None
0032: RETURN_VALUE
```

### ✅ ARM64 Native Compilation
**Status:** Fully Implemented (macOS, Linux)

Compile directly to native ARM64 executables for maximum performance.

```bash
# Compile to native ARM64
kaytecc myapp.kayte --native -o myapp

# Run native binary
./myapp
```

**Performance Benefits:**
- 🚀 **5-10x faster** than bytecode execution
- 📦 **Standalone executables** - no VM required
- 💪 **Direct machine code** - optimal CPU utilization

**Architecture:**
- **KayteArm64.pas**: Free Pascal unit (macOS Mach-O)
- **KayteArm64ELF.pas**: Free Pascal unit (Linux ELF)
- **KayteArm64PE.pas**: Free Pascal unit (Windows PE)
- **kayte_arm64_emit.c**: C-based ARM64 code generator

### ✅ musl libc Builds
**Status:** Fully Implemented (Linux ARM64 & AMD64)

Build statically-linked, portable Kayte binaries using musl libc for maximum portability across Linux distributions.

```bash
# One-command setup and build
sudo ./setup_and_build_kayte.sh both

# Or use the build script
./build_kayte_musl.sh both

# Or use Make
make -f Makefile.kayte all
```

**Benefits:**
- ✅ **Statically linked** - no runtime dependencies
- ✅ **Portable** - runs on any Linux distribution
- ✅ **Smaller binaries** - 20-30% smaller than glibc builds
- ✅ **ARM64 & AMD64** - both architectures supported

**Quick Start:**
```bash
# Setup musl toolchains and build
chmod +x setup_and_build_kayte.sh
sudo ./setup_and_build_kayte.sh both

# Outputs:
# bin/aarch64-linux-musl/kayte  (ARM64)
# bin/x86_64-linux-musl/kayte   (AMD64)
```

**Build Methods:**

1. **All-in-One Script** (Easiest - installs toolchains and builds):
   ```bash
   sudo ./setup_and_build_kayte.sh both
   ```

2. **Build Script** (Assumes toolchains installed):
   ```bash
   ./build_kayte_musl.sh both
   ```

3. **Makefile**:
   ```bash
   make -f Makefile.kayte all
   make -f Makefile.kayte test
   sudo make -f Makefile.kayte install
   ```

4. **FreePascal Config Files**:
   ```bash
   fpc @fpc-arm64-musl.cfg kayte.lpr
   fpc @fpc-amd64-musl.cfg kayte.lpr
   ```

**Documentation:**
- **QUICKSTART_KAYTE.md** - Quick start guide
- **README_KAYTE_MUSL.md** - Complete build documentation
- **INDEX.md** - File overview and build methods

---

## 🌐 Web & Networking

### ✅ HTTP Server
**Status:** Fully Implemented

Built-in HTTP server for web applications and APIs.

```bash
# Start HTTP server
kayte --http --port 8080 myapp.kayte
```

**Example: Simple Web API**

```kayte
import http;

server.on("/", function(request, response) {
    response.send("<h1>Welcome to Kayte Lang!</h1>");
});

server.on("/api/users", function(request, response) {
    var users = getUsers();
    response.json(users);
});

server.on("/api/data", function(request, response) {
    if (request.method == "POST") {
        var data = request.body;
        processData(data);
        response.status(201).send("Created");
    }
});

server.listen(8080);
```

### ✅ Node.js Integration
**Status:** Fully Implemented

Seamless integration with Node.js for leveraging the npm ecosystem.

```kayte
import nodejs;

// Use Node.js modules
var express = nodejs.require("express");
var app = express();

app.get("/", function(req, res) {
    res.send("Hello from Node.js integration!");
});

app.listen(3000);
```

---

## ⚙️ Platform Interoperability

### ✅ JVM Integration (JNI)
**Status:** Fully Implemented and Tested

Run Kayte bytecode on the JVM and access Java libraries.

**Architecture:**
1. **Bytecode Serialization**: Kayte programs saved to `.kbyte` files
2. **JNI Bridge**: Pascal code loads bytecode and calls Java methods
3. **Java VM**: Deserializes and executes bytecode
4. **Bidirectional Calls**: Kayte can call Java, Java can call Kayte

**Example Usage:**

```kayte
import jvm;

// Use Java libraries
var ArrayList = jvm.import("java.util.ArrayList");
var list = new ArrayList();
list.add("Item 1");
list.add("Item 2");

var size = list.size();
print("List size: " + size);

// Call Java methods
var String = jvm.import("java.lang.String");
var str = new String("Hello");
var upper = str.toUpperCase();
```

**Test Results:**
- ✅ Bytecode serialization and file I/O
- ✅ JNI bridge (`JNI_CreateJavaVM`, `CallStaticVoidMethod`)
- ✅ Java deserialization of Kayte bytecode
- ✅ VM execution within Java environment
- ✅ Bidirectional method calls

---

## 🛠️ Development Tools

### Command-Line Interface

```bash
# Compile
kaytecc input.kayte -o output.kbyte

# Run bytecode
kayte program.kbyte

# Native compilation (ARM64)
kaytecc input.kayte --native -o executable

# Debug mode
kayte --debug program.kbyte

# Disassemble bytecode
kayte --disassemble program.kbyte

# Start HTTP server
kayte --http --port 8080 server.kayte

# Run tests
kayte --test tests/

# Interactive REPL
kayte --repl
```

### IDE Support

**KayteIDE** - The official Kayte IDE:
- Syntax highlighting
- Code completion
- Integrated debugger
- Visual UI designer for .kfm files
- Project management
- Git integration

```bash
# Install Tilde Desktop
git clone https://github.com/ringsce/kayteide.git
cd kayteide
mkdir build && cd build
cmake ..
cmake --build .
```

---

## 📊 Example Projects

### Simple Calculator

```kayte
function main() {
    print("=== Kayte Calculator ===");
    
    while (true) {
        print("\nEnter operation (+, -, *, /) or 'q' to quit:");
        var op = input();
        
        if (op == "q") {
            break;
        }
        
        print("Enter first number:");
        var a = parseFloat(input());
        
        print("Enter second number:");
        var b = parseFloat(input());
        
        var result = calculate(a, b, op);
        print("Result: " + result);
    }
}

function calculate(a, b, op) {
    if (op == "+") return a + b;
    if (op == "-") return a - b;
    if (op == "*") return a * b;
    if (op == "/") {
        if (b == 0) {
            throw "Division by zero!";
        }
        return a / b;
    }
    throw "Invalid operation!";
}
```

### Todo List Application

```kayte
var todos = [];

window main {
    title: "Todo List"
    width: 600
    height: 400
    
    content {
        vbox {
            hbox {
                textfield { id: "todoInput" placeholder: "Enter task..." }
                button { text: "Add" onclick: addTodo() }
            }
            
            listview {
                id: "todoList"
                onItemClick: toggleTodo()
            }
            
            button { text: "Clear Completed" onclick: clearCompleted() }
        }
    }
}

function addTodo() {
    var task = getValue("todoInput");
    if (task != "") {
        todos.push({ text: task, done: false });
        setValue("todoInput", "");
        updateList();
    }
}

function toggleTodo(index) {
    todos[index].done = !todos[index].done;
    updateList();
}

function clearCompleted() {
    todos = filter(todos, function(todo) {
        return !todo.done;
    });
    updateList();
}

function updateList() {
    var items = map(todos, function(todo) {
        var prefix = todo.done ? "[X]" : "[ ]";
        return prefix + " " + todo.text;
    });
    setItems("todoList", items);
}
```

---

## 📌 Roadmap

### ✅ Completed (v0.9)
- Core language features (variables, functions, loops, conditionals)
- Bytecode VM with file I/O
- Declarative UI framework (.kfm files)
- JVM Interoperability (JNI)
- HTTP Server with Node.js integration
- Error handling (try-catch)
- Built-in functions
- ARM64 native compilation (macOS, Linux, Windows)
- **musl libc builds** (Linux ARM64 & AMD64 - statically linked, portable)
- **Type-erased generics** (parse-time `<T, U, ...>` on structs, classes, functions, and type names)
- **Process execution** (`PROCESS` statement to spawn OS processes and capture output)
- **.kfm form compiler** (compiles a `.kfm` form definition into a `.so`/`.a`/`.dylib`/`.dll` with a C-callable introspection API)

### 🚧 In Progress (v1.0)
- **Advanced Type System**: Static typing with type inference
- **Optimized VM**: JIT compilation for hot paths
- **Standard Library**: Comprehensive file I/O, networking, utilities
- **Package Manager**: npm-style dependency management
- **Cross-Platform UI**: Native rendering on Windows/Linux
- **Debugger Protocol**: VS Code integration

### 🎯 Planned (v1.1+)
- **WebAssembly Target**: Run Kayte in browsers
- **LLVM Backend**: Compile to native code on all platforms
- **Async/Await**: Modern asynchronous programming
- **Statically-Checked Generics**: Type-checked specialization, building on today's type-erased generics once the static type system lands
- **Module System**: Better code organization
- **FFI**: Call C/C++ libraries directly

---

## 🤝 Contributing

Kayte Lang is open-source and welcomes contributors of all skill levels!

### Ways to Contribute

1. **Core Development**: Improve the compiler, VM, or runtime
2. **Standard Library**: Build useful modules and utilities
3. **Documentation**: Write guides, tutorials, and examples
4. **Testing**: Write test cases and report bugs
5. **UI/UX**: Improve the IDE and developer tools
6. **Community**: Help others learn Kayte Lang

### Getting Started

```bash
# Fork and clone
git fork https://github.com/ringsce/kayte-lang.git
git clone https://github.com/YOUR_USERNAME/kayte-lang.git
cd kayte-lang

# Build from source (standard)
make build

# Build with musl (Linux ARM64 & AMD64 - portable, statically-linked)
sudo ./setup_and_build_kayte.sh both
# Or: ./build_kayte_musl.sh both
# Or: make -f Makefile.kayte all

# Run tests
make test

# Create a feature branch
git checkout -b feature/my-awesome-feature

# Make changes and commit
git commit -am "Add awesome feature"

# Push and create PR
git push origin feature/my-awesome-feature
```

### Code Style

- Use 4 spaces for indentation
- Follow existing naming conventions
- Write tests for new features
- Update documentation

---

## 📚 Resources

### Official Links
- **Website**: https://ringscejs.gleentech.com
- **Documentation**: https://ringscejs.gleentech.com
- **Compiler & VM**: https://github.com/ringsce/kayte-lang
- **Kayte IDE**: https://github.com/ringsce/kayteide
- **Discord Community**: https://discord.gg/d6gV8W2W

### Learning Resources
- **Getting Started Guide**: docs/getting-started.md
- **Language Reference**: docs/language-reference.md
- **API Documentation**: docs/api/
- **Example Projects**: examples/
- **Video Tutorials**: https://youtube.com/@ringsce

### Build System Documentation
- **INDEX.md** - Overview of all build system files
- **QUICKSTART_KAYTE.md** - Quick start for building with musl
- **README_KAYTE_MUSL.md** - Complete musl build documentation
- **setup_and_build_kayte.sh** - All-in-one build script
- **build_kayte_musl.sh** - Standalone build script
- **Makefile.kayte** - Makefile for musl builds
- **fpc-arm64-musl.cfg** - FreePascal config for ARM64
- **fpc-amd64-musl.cfg** - FreePascal config for AMD64

---

## 📄 License

Kayte Lang is released under the MIT License. See LICENSE file for details.

---

## 🌟 Acknowledgments

Kayte Lang is built with love by the open-source community. Special thanks to all contributors who have helped shape this project.

---

## ✨ Conclusion

Kayte Lang combines the simplicity of modern scripting languages with the performance of compiled languages. With bytecode portability, native ARM64 compilation, JVM integration, and a powerful UI framework, Kayte is ready for real-world application development.

**Join us in building the future of cross-platform programming!** 🚀

---

**Current Version:** 0.9.0 (Beta)  
**Status:** Production-ready for most use cases  
**Last Updated:** February 2026
