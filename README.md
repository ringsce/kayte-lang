# Kayte Lang: A Modern, Cross-Platform Programming Language

Kayte Lang is a modern experimental programming language designed for building applications quickly, efficiently, and cross-platform. It compiles into bytecode that runs on the Kayte Virtual Machine (KVM), ensuring portability across macOS, Linux, and Windows.

---

## 🚀 Language Features

### Functions 🟢
**Status:** Implemented

Function definition and calls are fully functional.

```kayte
function greet() {
    print("Hello, World from Kayte Lang!");
}
```

### Variables 🟢
**Status:** Implemented

Variable declaration and assignment are functional.

```kayte
var name = "Kayte Lang";
```

### Conditionals ✅
**Status:** Implemented

The core if/else logic, including support for `if`, `else if`, and `else` statements, is now highly efficient. The compiler and VM use explicit jump instructions (e.g., `JUMP_IF_FALSE`, `JUMP`) instead of simple markers, which provides a significant boost in performance and reliability for conditional branching.

```kayte
if (name == "Kayte Lang") {
    print("Welcome to Kayte Lang!");
} else {
    print("Unknown language");
}
```

### Loops ✅
**Status:** Implemented

Both `for` and `foreach` loops are fully functional, providing robust iteration capabilities.

```kayte
// Standard for loop
for (var i = 0; i < 10; i++) {
    print(i);
}

// Foreach loop for collections
var fruits = ["apple", "banana", "cherry"];
foreach fruit in fruits {
    print("I have a " + fruit);
}
```

### Error Handling ✅
**Status:** Implemented

The try-catch mechanism is now fully functional. The compiler generates jump instructions and the VM manages an exception handler stack, allowing for robust runtime error handling.

```kayte
try {
    var result = riskyOperation();
} catch (error) {
    print("An error occurred: " + error);
}
```

### Compiler Built-in Functions ✅
**Status:** Implemented

The compiler recognizes a set of special, highly-optimized built-in functions that are compiled directly into a single bytecode instruction. This is far more efficient than a standard function call. An example is the `len()` function, which quickly determines the length of a string or array.

```kayte
var fruits = ["apple", "banana", "cherry"];
var count = len(fruits);
print("There are " + count + " fruits.");
```

---

## 🎨 Declarative UI

### Simple UI Script
**Status:** Implemented

The declarative UI syntax is implemented. You can define windows, buttons, and bind events to functions.

```kayte
window main {
    title: "Kayte Lang Demo"
    width: 800
    height: 600

    content {
        button {
            id: "btnClickMe"
            text: "Click Me!"
            onclick: showMessage()
        }
    }
}

function showMessage() {
    print("Button clicked!");
}
```

### .kfrm Files
**Status:** Implemented

The `.kfrm` file format for declarative UI is now supported. This enables a clean, structured way to build cross-platform user interfaces.

**Example: login.kfrm**

```kayte
form LoginWindow {
    title: "User Login"
    width: 400
    height: 250

    layout: VBox {
        label { text: "Please enter your credentials" }
        textfield { id: "usernameInput" }
        textfield { id: "passwordInput" type: Password }
        button { id: "loginButton" text: "Log In" onclick: handleLogin() }
        label { id: "messageLabel" text: "" }
    }
}
```

---

## ⚡ Bytecode Compilation & Execution

**Status:** Implemented

The compiler and VM can generate and execute bytecode. The VM can now load and save bytecode to/from a file. This makes Kayte Lang fast, lightweight, and portable.

### Example: Factorial Calculator

```kayte
' Factorial Calculator in Kayte
sub main()
    dim myNumber as integer
    dim myFactorial as integer
    myFactorial = 1
    
    print "Enter a non-negative number:"
    input myNumber

    if myNumber < 0 then
        print "Error: Factorial is not defined for negative numbers."
    else
        for i = 1 to myNumber
            myFactorial = myFactorial * i
        next i
            
        print "The factorial of " & myNumber & " is " & myFactorial
    end if
end sub
```

### Example Bytecode Output

```
LOAD_CONST       0 (Button clicked!)
CALL_FUNCTION    1
RETURN_VALUE
```

---

## 🌐 HTTP Server

**Status:** Implemented

A new command-line flag `--http` has been added to start a simple HTTP server. This feature allows Kayte Lang to serve web content, opening up possibilities for web-based applications.

### Node.js Integration

The Pascal-based HTTP server now includes deeper integration with Node.js. The application can:

- **Check for dependencies:** Verify that Node.js and npm are installed on the system using a TProcess
- **Start a Node.js server:** The Pascal application can now launch a separate Node.js process as a child, allowing it to delegate web serving to an existing ecosystem

This is a crucial step towards building hybrid applications that combine the performance of compiled code with the extensive libraries of the Node.js world.

---

## ⚙️ JVM Interoperability (JNI) ✅

**Status:** Implemented and Tested

A crucial step toward wider platform adoption is the ability to run Kayte bytecode on the JVM. The JNI bridge is fully functional.

### Test Objective

Verify that a compiled Kayte program can be loaded from a file, passed to a Java VM as a byte array, and successfully deserialized and executed within a Java class.

### Test Architecture

- **Pascal-side (JVM.pas):** Reads the compiled `.kbyte` file into a TBytes array and uses JNI to call a static Java method
- **Java-side (KayteVM.java):** A static method `execute()` receives the byte array, deserializes the Kayte program, and runs the VM execution logic

### Test Status

- ✅ **Bytecode serialization:** Kayte programs are correctly saved to a binary file
- ✅ **File loading:** The Pascal code can read the entire `.kbyte` file into a byte array
- ✅ **JNI bridge:** The `JNI_CreateJavaVM` and `CallStaticVoidMethod` calls are successfully linking the Pascal application to the JVM
- ✅ **Java Deserialization:** The `KayteVM.java` class can correctly read the bytecode stream and reconstruct the program's components (title, instructions, literals, etc.)
- ✅ **Execution:** The VM execution logic within the Java environment is implemented and has successfully run basic test cases

This JNI test confirms the technical feasibility of running Kayte on a platform as robust as the JVM, opening the door for future integration with Java libraries and frameworks.

---

## 📌 Roadmap

### Completed
- ✅ Proof-of-Concept VM
- ✅ Bytecode File I/O (save/load)
- ✅ Declarative UI with .kfrm files
- ✅ JVM Interoperability (JNI) Test
- ✅ Simple HTTP Server
- ✅ Error Handling
- ✅ Loops
- ✅ Compiler Built-in Functions

### In Progress
- 🚧 Advanced Type System → safer & faster programs
- 🚧 Optimized VM → better runtime performance
- 🚧 Standard Libraries → file handling, networking, utilities
- 🚧 Cross-Platform UI Toolkit → build once, run everywhere

---

## 🤝 Get Involved

Kayte Lang is open-source — we welcome contributors of all levels!

### Repositories

- **Tilde Desktop (Kayte IDE/Environment):** [ringsce/tilde-desktop](https://github.com/ringsce/tilde-desktop)
- **Kayte Lang Compiler & VM:** [ringsce/kayte-lang](https://github.com/ringsce/kayte-lang)

### Ways to Help

- Improve the language & VM
- Build standard libraries
- Share feedback & ideas

---

## ✨ Conclusion

Kayte Lang is in its **Proof of Concept (PoC)** stage — but its clean syntax, bytecode execution, and UI-first approach make it a strong foundation for a modern, cross-platform development ecosystem.

**Stay tuned — we're just getting started!** 🚀
