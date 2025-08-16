# Kayte Lang

[![CircleCI](https://circleci.com/gh/ringsce/kayte-lang/tree/main.svg?style=svg)](https://circleci.com/gh/ringsce/kayte-lang/tree/main)  
![macOS Passing](https://img.shields.io/badge/macOS-Passing-brightgreen)  
![Linux Passing](https://img.shields.io/badge/Linux-Passing-brightgreen)  
![Windows Passing](https://img.shields.io/badge/Windows-Passing-brightgreen)

---

**Kayte Lang** is a modern **experimental programming language** designed for building applications **quickly, efficiently, and cross-platform**.  

It compiles into **bytecode** that runs on the **Kayte Virtual Machine (KVM)**, ensuring portability across macOS, Linux, and Windows.

---

## 🚀 Language Overview

### 1. Functions

```kayte
function greet() {
    print("Hello, World from Kayte Lang!");
}
````

### 2. Variables

```kayte
var name = "Kayte Lang";
```

### 3. Conditionals

```kayte
if (name == "Kayte Lang") {
    print("Welcome to Kayte Lang!");
} else {
    print("Unknown language");
}
```

### 4. Loops

```kayte
for (var i = 0; i < 10; i++) {
    print(i);
}
```

### 5. Error Handling

```kayte
try {
    var result = riskyOperation();
} catch (error) {
    print("An error occurred: " + error);
}
```

---

## 🎨 Example: Simple UI Script

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

---

## ⚡ Bytecode Compilation & Execution

Kayte Lang compiles its `.kayte` source files into **bytecode**, then runs them inside the **KVM**.

### Example Bytecode

```plaintext
LOAD_CONST     0 (Button clicked!)
CALL_FUNCTION  1
RETURN_VALUE
```

This makes Kayte Lang **fast, lightweight, and portable**.

---

## 🖼️ Declarative UI with XML

Kayte Lang also supports **XML/DTD-style UI definitions**:

```xml
<window title="Kayte Lang UI" width="800" height="600">
  <button id="btnHello" text="Say Hello" onclick="sayHello()" />
</window>
```

A clean, structured way to build **cross-platform user interfaces**.

---

## 📌 Roadmap

* ✅ Proof-of-Concept VM
* 🚧 Advanced Type System → safer & faster programs
* 🚧 Optimized VM → better runtime performance
* 🚧 Standard Libraries → file handling, networking, utilities
* 🚧 Cross-Platform UI Toolkit → **build once, run everywhere**

---

## 🤝 Get Involved

Kayte Lang is **open-source** — we welcome contributors of all levels!

* **Tilde Desktop (Kayte IDE / Environment)**: [ringsce/tilde-desktop](https://github.com/ringsce/tilde-desktop)
* **Kayte Lang Compiler & VM**: [ringsce/kayte-lang](https://github.com/ringsce/kayte-lang)

Ways to help:

* Improve the language & VM
* Build standard libraries
* Share feedback & ideas

---

## ✨ Conclusion

Kayte Lang is in its **Proof of Concept (PoC)** stage — but its **clean syntax**, **bytecode execution**, and **UI-first approach** make it a strong foundation for a **modern, cross-platform development ecosystem**.

Stay tuned — we’re just getting started! 🚀

