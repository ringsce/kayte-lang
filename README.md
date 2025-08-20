Kayte Lang
Kayte Lang is a modern experimental programming language designed for building applications quickly, efficiently, and cross-platform.

It compiles into bytecode that runs on the Kayte Virtual Machine (KVM), ensuring portability across macOS, Linux, and Windows.

🚀 Language Overview
1. Functions 🟢
Implemented: Function definition and calls are fully functional.

function greet() {
    print("Hello, World from Kayte Lang!");
}

2. Variables 🟢
Implemented: Variable declaration and assignment are functional.

var name = "Kayte Lang";

3. Conditionals ✅
Implemented: The core if/else logic, including support for if, else if, and else statements, is now highly efficient. The compiler and VM use explicit jump instructions (e.g., JUMP_IF_FALSE, JUMP) instead of simple markers, which provides a significant boost in performance and reliability for conditional branching.

if (name == "Kayte Lang") {
    print("Welcome to Kayte Lang!");
} else {
    print("Unknown language");
}

4. Loops ✅
Implemented: Both for and foreach loops are fully functional, providing robust iteration capabilities.

// Standard for loop
for (var i = 0; i < 10; i++) {
    print(i);
}

// Foreach loop for collections
var fruits = ["apple", "banana", "cherry"];
foreach fruit in fruits {
    print("I have a " + fruit);
}

5. Error Handling ✅
Implemented: The try-catch mechanism is now fully functional. The compiler generates jump instructions and the VM manages an exception handler stack, allowing for robust runtime error handling.

try {
    var result = riskyOperation();
} catch (error) {
    print("An error occurred: " + error);
}

🎨 Example: Simple UI Script
Implemented: The declarative UI syntax is implemented. You can define windows, buttons, and bind events to functions.

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

➡️ Defines a basic window with a button. When clicked, it runs showMessage().

⚡ Bytecode Compilation & Execution
Implemented: The compiler and VM can generate and execute bytecode.
Implemented: The VM can now load and save bytecode to/from a file.

This makes Kayte Lang fast, lightweight, and portable.

Example Bytecode Program
This is an example of a simple factorial calculator written in Kayte, which is a great test for bytecode generation.

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

Example Bytecode
LOAD_CONST      0 (Button clicked!)
CALL_FUNCTION   1
RETURN_VALUE

🖼️ Declarative UI with .kfrm Files and XML
Implemented: The .kfrm file format for declarative UI is now supported. This enables a clean, structured way to build cross-platform user interfaces. The XML example is a style you are considering, but the .kfrm style is the one that's currently implemented.

Example login.kfrm

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

🌐 HTTP Server
Implemented: A new command-line flag --http has been added to start a simple HTTP server. This feature allows Kayte Lang to serve web content, opening up possibilities for web-based applications.

⚙️ JVM Interoperability (JNI) Test ✅
Implemented and Tested: A crucial step toward wider platform adoption is the ability to run Kayte bytecode on the JVM. The JNI bridge is fully functional.

Test Objective
Verify that a compiled Kayte program can be loaded from a file, passed to a Java VM as a byte array, and successfully deserialized and executed within a Java class.

Test Architecture
Pascal-side (JVM.pas): Reads the compiled .kbyte file into a TBytes array and uses JNI to call a static Java method.

Java-side (KayteVM.java): A static method execute() receives the byte array, deserializes the Kayte program, and runs the VM execution logic.

Test Status
✅ Bytecode serialization: Kayte programs are correctly saved to a binary file.

✅ File loading: The Pascal code can read the entire .kbyte file into a byte array.

✅ JNI bridge: The JNI_CreateJavaVM and CallStaticVoidMethod calls are successfully linking the Pascal application to the JVM.

✅ Java Deserialization: The KayteVM.java class can correctly read the bytecode stream and reconstruct the program's components (title, instructions, literals, etc.).

✅ Execution: The VM execution logic within the Java environment is implemented and has successfully run basic test cases, confirming the feasibility of a JVM-based KVM.

This JNI test confirms the technical feasibility of running Kayte on a platform as robust as the JVM, opening the door for future integration with Java libraries and frameworks.

📌 Roadmap
✅ Proof-of-Concept VM

✅ Bytecode File I/O (save/load)

✅ Declarative UI with .kfrm files

✅ JVM Interoperability (JNI) Test

✅ Simple HTTP Server

✅ Error Handling

✅ Loops

🚧 Advanced Type System → safer & faster programs

🚧 Optimized VM → better runtime performance

🚧 Standard Libraries → file handling, networking, utilities

🚧 Cross-Platform UI Toolkit → build once, run everywhere

🤝 Get Involved
Kayte Lang is open-source — we welcome contributors of all levels!

Tilde Desktop (Kayte IDE / Environment): ringsce/tilde-desktop

Kayte Lang Compiler & VM: ringsce/kayte-lang

Ways to help:

Improve the language & VM

Build standard libraries

Share feedback & ideas

✨ Conclusion
Kayte Lang is in its Proof of Concept (PoC) stage — but its clean syntax, bytecode execution, and UI-first approach make it a strong foundation for a modern, cross-platform development ecosystem.

Stay tuned — we’re just getting started! 🚀
