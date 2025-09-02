# KCC - Kayte C Compiler

A comprehensive C compiler implemented in C with full macro preprocessing support and CMake build system.

## Features

- **Complete C Preprocessor**: Full macro support with expansion, stringizing, and token pasting
- **Lexical Analysis**: Tokenizes C source code with comprehensive token support
- **Syntax Analysis**: Builds Abstract Syntax Tree (AST) using recursive descent parsing
- **Semantic Analysis**: Symbol table management with proper scoping
- **Code Generation**: Generates x86-64 assembly code
- **Error Handling**: Comprehensive error reporting with line/column information
- **Debug Mode**: Token, AST, and preprocessor output visualization

## Supported C Features

### Preprocessor Features
- **Object-like macros**: `#define PI 3.14159`
- **Function-like macros**: `#define MAX(a,b) ((a) > (b) ? (a) : (b))`
- **Macro expansion**: Recursive expansion with proper parameter substitution
- **Stringizing operator**: `#define STR(x) #x`
- **Token pasting**: `#define CONCAT(a,b) a##b`
- **Conditional compilation**: `#if`, `#ifdef`, `#ifndef`, `#else`, `#elif`, `#endif`
- **Include files**: `#include "file.h"` and `#include <file.h>`
- **Predefined macros**: `__DATE__`, `__TIME__`, `__FILE__`, `__LINE__`, `__KCC__`
- **Macro undefining**: `#undef MACRO_NAME`
- **Error/Warning directives**: `#error`, `#warning`
- **Pragma support**: `#pragma` (basic handling)

### Data Types
- `int` - Integer type
- `char` - Character type  
- `void` - Void type

### Control Structures
- `if`/`else` statements
- `while` loops
- `for` loops
- `break` and `continue`

### Operators
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logical: `&&`, `||`, `!`
- Assignment: `=`

### Functions
- Function declarations and definitions
- Function calls with parameters
- Return statements
- Parameter passing

### Variables
- Variable declarations with initialization
- Local and parameter scoping
- Symbol table management

## Building

### Prerequisites
- CMake 3.16 or higher
- GCC or Clang compiler
- Make

### Quick Build
```bash
chmod +x build.sh
./build.sh
```

### Manual Build
```bash
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j$(nproc)
```

## Usage

### Basic Compilation
```bash
# Compile a C file
./kcc input.c

# Specify output file
./kcc -o output input.c

# Verbose mode with detailed information
./kcc -v input.c

# Debug mode (shows tokens, AST, and preprocessed code)
./kcc -d input.c
```

### Preprocessor Options
```bash
# Run preprocessor only (output to stdout)
./kcc -E macros.c

# Skip preprocessing (compile raw C)
./kcc --no-preprocess raw.c

# Save preprocessed output to file
./kcc -E macros.c > preprocessed.c
```

### Command Line Options
```
Usage: kcc [options] <input_file>

Options:
  -o <file>        Specify output file
  -v, --verbose    Enable verbose output
  -d, --debug      Enable debug mode (shows internal structures)
  -O               Enable optimization
  -S               Keep assembly output
  -E               Run preprocessor only
  --no-preprocess  Skip preprocessing step
  -h, --help       Show help message
  --version        Show version information
```

## Macro Examples

### Object-like Macros
```c
#define PI 3.14159
#define VERSION "1.0.0"
#define MAX_SIZE 1024

// Usage
double area = PI * radius * radius;
printf("Version: %s\n", VERSION);
```

### Function-like Macros
```c
#define SQUARE(x) ((x) * (x))
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define CLAMP(x, min, max) (MAX(MIN(x, max), min))

// Usage
int result = SQUARE(5);        // Expands to: ((5) * (5))
int maximum = MAX(10, 20);     // Expands to: ((10) > (20) ? (10) : (20))
```

### Stringizing and Token Pasting
```c
#define STRINGIFY(x) #x
#define CONCAT(a, b) a##b

// Usage
const char* name = STRINGIFY(hello);    // Expands to: "hello"
int CONCAT(var_, 123) = 42;            // Creates: int var_123 = 42;
```

### Conditional Compilation
```c
#define DEBUG 1

#ifdef DEBUG
    #define DBG_PRINT(x) printf("DEBUG: " x "\n")
#else
    #define DBG_PRINT(x)
#endif

#if MAX_SIZE > 512
    printf("Large buffer\n");
#else
    printf("Small buffer\n");
#endif
```

### Complex Macros
```c
#define DECLARE_GETTER_SETTER(type, name) \
    type get_##name(void) { return name; } \
    void set_##name(type value) { name = value; }

#define SAFE_FREE(ptr) do { \
    if (ptr) { \
        free(ptr); \
        ptr = NULL; \
    } \
} while(0)

// Usage
DECLARE_GETTER_SETTER(int, count)  // Creates get_count() and set_count()
SAFE_FREE(my_ptr);                 // Safe pointer freeing
```

## Example Programs

### Simple Program with Macros
```c
#include "common.h"

#define BUFFER_SIZE 256
#define SQUARE(x) ((x) * (x))

int main() {
    int numbers[BUFFER_SIZE];
    
    FOR_RANGE(i, 0, 10) {
        numbers[i] = SQUARE(i);
    }
    
    #ifdef DEBUG
        DEBUG_PRINT("Program compiled successfully");
    #endif
    
    return 0;
}
```

### Macro-Heavy Example
```c
#define PI 3.14159
#define CIRCLE_AREA(r) (PI * SQUARE(r))
#define SQUARE(x) ((x) * (x))

#if defined(METRIC_UNITS)
    #define UNIT "meters"
#else
    #define UNIT "feet"
#endif

int calculate_area() {
    double radius = 5.0;
    double area = CIRCLE_AREA(radius);
    
    printf("Circle area: %.2f square %s\n", area, UNIT);
    
    return 0;
}
```

## Project Structure

```
kcc/
├── CMakeLists.txt              # CMake build configuration
├── build.sh                   # Automated build script
├── README.md                  # This documentation
├── include/                   # Header files
│   ├── kcc.h                 # Main compiler header
│   ├── types.h               # Common type definitions
│   ├── preprocessor.h        # Preprocessor interface
│   ├── lexer.h               # Lexical analyzer
│   ├── parser.h              # Syntax analyzer
│   ├── ast.h                 # Abstract Syntax Tree
│   ├── codegen.h             # Code generation
│   ├── error.h               # Error handling
│   └── symbol_table.h        # Symbol management
├── src/                      # Source files
│   ├── main.c                # Main entry point
│   ├── preprocessor_simple.c # Macro preprocessor
│   ├── lexer.c               # Lexical analyzer
│   ├── parser.c              # Syntax analyzer
│   ├── ast.c                 # AST implementation
│   ├── codegen.c             # Code generator
│   ├── error.c               # Error handling
│   └── symbol_table.c        # Symbol table
├── example/                  # Example programs
│   ├── hello.c               # Basic C program
│   ├── macros.c              # Comprehensive macro examples
│   └── common.h              # Utility macros header
└── tests/                    # Test suite
    ├── test_main.c
    ├── test_lexer.c
    ├── test_parser.c
    └── test_preprocessor.c
```

## Architecture

### Compilation Pipeline
1. **Preprocessing** - Macro expansion and directive processing
2. **Lexical Analysis** - Convert preprocessed code to tokens
3. **Syntax Analysis** - Build AST from token stream
4. **Semantic Analysis** - Type checking and symbol resolution
5. **Code Generation** - Generate x86-64 assembly code

### Key Components

#### Preprocessor
- **Macro Storage**: Hash table for efficient macro lookup
- **Expansion Engine**: Recursive macro expansion with parameter substitution
- **Conditional Compilation**: Stack-based `#if/#endif` processing
- **Include System**: File inclusion with depth tracking
- **Error Reporting**: Detailed preprocessing error messages

#### Lexer
- **Token Recognition**: Comprehensive C token support
- **Keyword Detection**: Automatic keyword vs identifier classification
- **String/Character Literals**: Proper escape sequence handling
- **Comments**: Single-line and multi-line comment removal
- **Line Tracking**: Accurate line/column position tracking

#### Parser
- **Recursive Descent**: Clean, maintainable parsing algorithm
- **Error Recovery**: Continue parsing after syntax errors
- **AST Construction**: Build complete abstract syntax tree
- **Precedence Handling**: Proper operator precedence and associativity

#### Code Generator
- **x86-64 Assembly**: Generate standard assembly output
- **Register Management**: Efficient register allocation
- **Function Calls**: Proper calling convention support
- **Control Flow**: Accurate jump and label generation

## Testing

### Run All Tests
```bash
cd build
make test_all
```

### Manual Testing
```bash
# Test preprocessor
./kcc -E -d example/macros.c

# Test with debug output
./kcc -v -d example/hello.c

# Test macro expansion
echo '#define TEST 42\nint x = TEST;' | ./kcc -E -
```

### Example Test Cases
```bash
# Basic macro expansion
echo '#define MAX 100\nint size = MAX;' | ./kcc -E -

# Function macro
echo '#define SQUARE(x) ((x)*(x))\nint area = SQUARE(5);' | ./kcc -E -

# Conditional compilation
echo '#define DEBUG\n#ifdef DEBUG\nint debug = 1;\n#endif' | ./kcc -E -
```

## Performance

- **Fast Preprocessing**: Efficient macro expansion and file processing
- **Memory Efficient**: Careful memory management with proper cleanup
- **Scalable**: Handles large programs with many macros
- **Debug-Friendly**: Extensive debug output for development

## Advanced Features

### Predefined Macros
KCC automatically defines several useful macros:
- `__KCC__` - Compiler identification
- `__VERSION__` - Compiler version string
- `__DATE__` - Compilation date
- `__TIME__` - Compilation time
- `__FILE__` - Current filename
- `__LINE__` - Current line number

### Macro Best Practices
1. **Use parentheses** around macro parameters: `#define SQUARE(x) ((x) * (x))`
2. **Multi-statement macros** should use do-while(0): `#define MACRO() do { ... } while(0)`
3. **Avoid side effects** in macro arguments
4. **Use meaningful names** for macros
5. **Document complex macros** thoroughly

## Limitations

This is an educational/demonstration compiler with some limitations:
- Simplified preprocessing (some advanced features not fully implemented)
- Limited C language support (subset of full C)
- No linker integration
- Simplified code generation
- No optimization passes
- Limited error recovery

## Future Enhancements

Potential improvements for the compiler:
- **Full Preprocessor**: Complete C preprocessor implementation
- **More C Features**: Structures, unions, pointers, arrays
- **Optimization**: Dead code elimination, constant folding
- **Better Codegen**: Improved register allocation, instruction selection
- **Standards Compliance**: Full C11/C18 support
- **Cross-platform**: Support for multiple target architectures

## Contributing

This is a reference implementation perfect for:
- Learning compiler construction
- Understanding macro preprocessing
- Extending with additional C features
- Educational purposes
- Research projects

## License

This project is provided as-is for educational purposes.

## Version History

- **v1.0.0** - Initial release with full macro preprocessing support
- Complete lexer, parser, AST, and code generation
- Comprehensive macro system with expansion and directives
- Professional build system and documentation

---

**KCC - Kayte C Compiler**: A full-featured educational C compiler with comprehensive macro preprocessing support.d
cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j$(nproc)
```

## Usage

### Basic Usage
```bash
# Compile a C file
./kcc input.c

# Specify output file
./kcc -o output input.c

# Verbose mode
./kcc -v input.c

# Debug mode (shows tokens and AST)
./kcc -d input.c

# Enable optimization
./kcc -O input.c

# Keep assembly output
./kcc -S input.c
```

### Command Line Options
```
Usage: kcc [options] <input_file>

Options:
  -o <file>     Specify output file
  -v, --verbose Enable verbose output
  -d, --debug   Enable debug mode
  -O            Enable optimization
  -S            Keep assembly output
  -h, --help    Show help message
  --version     Show version information
```

## Example

Create a simple C program:

```c
// hello.c
int main() {
    int x = 42;
    int y = 10;
    int result = x + y;
    
    if (result > 50) {
        return 1;
    } else {
        return 0;
    }
}
```

Compile with KCC:
```bash
./kcc -v hello.c
```

## Project Structure

```
kcc/
├── CMakeLists.txt          # CMake build configuration
├── build.sh               # Build script
├── README.md              # This file
├── include/               # Header files
│   ├── kcc.h             # Main header
│   ├── lexer.h           # Lexer definitions
│   ├── parser.h          # Parser definitions
│   ├── ast.h             # AST definitions
│   ├── codegen.h         # Code generation
│   ├── error.h           # Error handling
│   └── symbol_table.h    # Symbol table
├── src/                  # Source files
│   ├── main.c            # Main entry point
│   ├── lexer.c           # Lexical analyzer
│   ├── parser.c          # Syntax analyzer
│   ├── ast.c             # AST implementation
│   ├── codegen.c         # Code generator
│   ├── error.c           # Error handling
│   └── symbol_table.c    # Symbol table
├── example/              # Example programs
│   └── hello.c           # Sample C program
└── tests/                # Test files
    ├── test_main.c
    ├── test_lexer.c
    └── test_parser.c
```

## Architecture

### Compilation Pipeline
1. **Lexical Analysis** - Convert source code to tokens
2. **Syntax Analysis** - Build AST from tokens
3. **Semantic Analysis** - Type checking and symbol resolution
4. **Code Generation** - Generate x86-64 assembly

### Components
- **Lexer**: Converts source code into a stream of tokens
- **Parser**: Builds an Abstract Syntax Tree (AST) using recursive descent
- **Symbol Table**: Manages variable and function scoping
- **Code Generator**: Emits x86-64 assembly code
- **Error Handler**: Provides comprehensive error reporting

## Testing

Run the test suite:
```bash
cd build
make test_all
```

Or run tests manually:
```bash
cd build
./kcc_tests
```

## Limitations

This is a educational/demonstration compiler with several limitations:
- Limited C language support (subset only)
- No preprocessor
- No linker integration
- Simplified code generation
- No optimization passes
- Limited error recovery

## Contributing

This is a reference implementation. Feel free to extend it with:
- More C language features
- Better error messages
- Code optimization
- Additional target architectures
- Preprocessor support

## License

This project is provided as-is for educational purposes.

## Version

Current version: 1.0.0
