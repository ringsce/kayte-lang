# KayteIDE ✨

\![KayteIDE Logo]

KayteIDE is a lightweight, cross-platform Integrated Development Environment (IDE) built with **Qt 6 and C++17**. It provides core functionalities for editing, saving, and managing Visual Basic (VB) code, enhanced with syntax highlighting for improved readability.

-----

## Features

* **File Management:**
    * Open existing Visual Basic source files.
    * Save changes to the current file.
    * Save the current file to a new location (Save As).
* **Syntax Highlighting:**
    * Basic syntax highlighting for Visual Basic keywords, data types, functions, operators, comments, strings, numbers, and preprocessor directives.
    * Colors are inspired by common IDE themes for improved code readability.
* **File Properties Dialog:**
    * View detailed information about the currently open file, including general details (size, dates), permissions, and suggested applications.
* **Project Actions (Placeholder):**
    * "Build," "Run," "Clean," and "Debug" actions are included as placeholders, signaling future expansion into a more comprehensive IDE experience.

-----

## Supported Platforms

KayteIDE officially supports building and running on:

* **macOS** (Intel and Apple Silicon - Universal Binary support)
* **Linux** (x86\_64 and other architectures where Qt 6 is available)

-----

## Getting Started

To get a local copy of KayteIDE up and running, follow these steps.

### Prerequisites

You'll need the following tools installed on your system:

* **Git**: For cloning the repository.
* **CMake**: Version 3.16 or higher.
* **C++ Compiler**: A C++17 compliant compiler (e.g., GCC, Clang, MSVC).
* **Qt 6 Development Libraries**:
    * **macOS**: Recommended installation via Homebrew (`brew install qt@6`) or the official Qt installer. Ensure your `CMAKE_PREFIX_PATH` is correctly set (e.g., `export CMAKE_PREFIX_PATH="$(brew --prefix qt6)/lib/cmake"`).
    * **Linux**: Install Qt 6 development packages via your distribution's package manager (e.g., `sudo apt install qt6-base-dev` on Debian/Ubuntu, `sudo dnf install qt6-qtbase-devel` on Fedora).

### Building the Project

1.  **Clone the repository** (if applicable):

    ```bash
    git clone <your-repository-url>
    cd <your-repository-name>
    ```

    *Note: If you're building directly from the provided code, just navigate to your project directory.*

2.  **Create a build directory:**

    ```bash
    mkdir build
    cd build
    ```

3.  **Configure with CMake:**

    ```bash
    cmake ..
    ```

    * *For macOS, CMake will automatically detect and configure for your native architecture, including universal binary support on Apple Silicon based on the `CMakeLists.txt` configuration.*

4.  **Build the application:**

    ```bash
    cmake --build . --parallel # Use --parallel for faster builds
    ```

### Running the Application

After a successful build, the executable will be located in the `build/bin/` directory.

* **macOS/Linux:**
  ```bash
  ./bin/KayteIDE
  ```
* **Windows:** (While the `CMakeLists.txt` in the prompt isn't explicitly for Windows, if you adapt it, this would be the run command)
  ```bash
  .\bin\KayteIDE.exe
  ```

-----

## Usage

1.  **Open File:** Use **File \> Open** from the menu or the toolbar button to load a Visual Basic (`.vb`, `.bas`, etc.) file into the editor.
2.  **Save File:** Use **File \> Save** to save changes to the currently open file. If it's a new file, it will prompt you to save as.
3.  **Save As:** Use **File \> Save As** to save the current content to a new file.
4.  **File Properties:** Select **File \> Properties...** to view detailed information about the currently open file in a separate tabbed dialog.
5.  **Project Actions:** The "Build," "Run," "Clean," and "Debug" buttons/menu items currently show placeholder messages, indicating future development.

-----

## Customization and Development

* **Syntax Highlighting:** The `vbsyntaxhighlighter.cpp` and `vbsyntaxhighlighter.h` files contain the logic for VB syntax highlighting. You can modify the keyword lists or add more sophisticated parsing rules there.
* **UI Layout:** The main application layout is defined in `mainwindow.ui` (editable with Qt Designer).
* **Icons:** Icons are managed via `resources/icons.qrc`. To change or add icons, replace the image files in `resources/icons/` and update `resources/icons.qrc` and `mainwindow.cpp` accordingly.
* **Extend Functionality:** The placeholder project actions provide starting points for integrating build systems, debuggers, or a custom VB interpreter, paving the way for a richer IDE experience.

-----

## Contributing

Contributions are welcome\! If you have suggestions for improvements or encounter issues, please feel free to open an issue or submit a pull request.

-----

## License

This project is licensed under a permissive license, likely a BSD-3-Clause or similar, as is common with Qt examples and open-source projects. Please refer to the specific license headers within the source files for precise details regarding usage and distribution.

-----

## Contact

**Pedro Dias Vicente / KD ** - [pdvicente@gleentech.com](mailto:pedvicente@gleentech.com)

Project Link: [https://github.com/ringsce/KayteIDE](https://www.google.com/search?q=https://github.com/your-username/KayteIDE)

-----