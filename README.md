# Kayte Lang

[](https://circleci.com/gh/ringsce/kayte-lang/tree/main)

-----

**Kayte Lang** is a modern **experimental programming language** designed for building applications **quickly, efficiently, and cross-platform**.

It compiles into **bytecode** that runs on the **Kayte Virtual Machine (KVM)**, ensuring portability across macOS, Linux, and Windows.

-----

## 🚀 Language Overview

### 1\. Functions 🟢

**Implemented:** Function definition and calls are fully functional.

```kayte
function greet() {
    print("Hello, World from Kayte Lang!");
}
```

### 2\. Variables 🟢

**Implemented:** Variable declaration and assignment are functional.

```kayte
var name = "Kayte Lang";
```

### 3\. Conditionals 🟡

**Partially Implemented:** The core `if/else` logic is implemented.

```kayte
if (name == "Kayte Lang") {
    print("Welcome to Kayte Lang!");
} else {
    print("Unknown language");
}
```

### 4\. Loops 🟡

**Partially Implemented:** The basic `for` loop is implemented.

```kayte
for (var i = 0; i < 10; i++) {
    print(i);
}
```

### 5\. Error Handling 🟡

**Partially Implemented:** The `try-catch` mechanism is in the proof-of-concept stage.

```kayte
try {
    var result = riskyOperation();
} catch (error) {
    print("An error occurred: " + error);
}
```

-----

## 🎨 Example: Simple UI Script

**Implemented:** The declarative UI syntax is implemented. You can define windows, buttons, and bind events to functions.

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

➡️ Defines a **basic window with a button**. When clicked, it runs `showMessage()`.

-----

## ⚡ Bytecode Compilation & Execution

**Implemented:** The compiler and VM can generate and execute bytecode.
**Implemented:** The VM can now **load and save bytecode to/from a file**.

This makes Kayte Lang **fast, lightweight, and portable**.

### Example Bytecode

```plaintext
LOAD_CONST      0 (Button clicked!)
CALL_FUNCTION   1
RETURN_VALUE
```

-----

## 🖼️ Declarative UI with `.kfrm` Files and XML

**Implemented:** The `.kfrm` file format for declarative UI is now supported. This enables a clean, structured way to build **cross-platform user interfaces**. The XML example is a style you are considering, but the `.kfrm` style is the one that's currently implemented.

**Example `login.kfrm`**

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

-----

## 📌 Roadmap

  * ✅ Proof-of-Concept VM
  * ✅ Bytecode File I/O (save/load)
  * ✅ Declarative UI with `.kfrm` files
  * 🚧 Advanced Type System → safer & faster programs
  * 🚧 Optimized VM → better runtime performance
  * 🚧 Standard Libraries → file handling, networking, utilities
  * 🚧 Cross-Platform UI Toolkit → **build once, run everywhere**

-----

## 🤝 Get Involved

Kayte Lang is **open-source** — we welcome contributors of all levels\!

  * **Tilde Desktop (Kayte IDE / Environment)**: [ringsce/tilde-desktop](https://github.com/ringsce/tilde-desktop)
  * **Kayte Lang Compiler & VM**: [ringsce/kayte-lang](https://github.com/ringsce/kayte-lang)

Ways to help:

  * Improve the language & VM
  * Build standard libraries
  * Share feedback & ideas

-----

## ✨ Conclusion

Kayte Lang is in its **Proof of Concept (PoC)** stage — but its **clean syntax**, **bytecode execution**, and **UI-first approach** make it a strong foundation for a **modern, cross-platform development ecosystem**.

Stay tuned — we’re just getting started\! 🚀
