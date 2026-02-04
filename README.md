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

### ✅ .kfrm Files
**Status:** Fully Implemented

Separate UI definitions using `.kfrm` files for cleaner project organization.

**Example: login.kfrm**

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
import "login.kfrm";

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
**Status:** Fully Implemented (macOS)

Compile directly to native ARM64 Mach-O executables for maximum performance on Apple Silicon.

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
- **KayteArm64.pas**: Free Pascal unit that interfaces with the native compiler
- **kayte_arm64_emit.c**: C-based ARM64 code generator
- **Mach-O generation**: Creates standard macOS executables

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

**Tilde Desktop** - The official Kayte IDE:
- Syntax highlighting
- Code completion
- Integrated debugger
- Visual UI designer for .kfrm files
- Project management
- Git integration

```bash
# Install Tilde Desktop
git clone https://github.com/ringsce/tilde-desktop.git
cd tilde-desktop
make install
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
- Declarative UI framework (.kfrm files)
- JVM Interoperability (JNI)
- HTTP Server with Node.js integration
- Error handling (try-catch)
- Built-in functions
- ARM64 native compilation (macOS)

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
- **Generic Types**: Type-safe collections and utilities
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

# Build from source
make build

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
- **Tilde Desktop IDE**: https://github.com/ringsce/tilde-desktop
- **Discord Community**: https://discord.gg/kaytelang

### Learning Resources
- **Getting Started Guide**: docs/getting-started.md
- **Language Reference**: docs/language-reference.md
- **API Documentation**: docs/api/
- **Example Projects**: examples/
- **Video Tutorials**: https://youtube.com/@ringsce
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
