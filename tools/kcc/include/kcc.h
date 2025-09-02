#ifndef KCC_H
#define KCC_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <stdarg.h>

#define KCC_VERSION "1.10.0"
#define MAX_TOKENS 1024
#define MAX_NODES 512

// Include common types first
#include "types.h"

// Forward declarations
typedef struct Token Token;
typedef struct ASTNode ASTNode;
typedef struct SymbolTable SymbolTable;
typedef struct Lexer Lexer;
typedef struct Parser Parser;
typedef struct Preprocessor Preprocessor;

// Include headers in correct order
#include "error.h"
#include "preprocessor.h"
#include "lexer.h"
#include "AST.h"
#include "symbol_table.h"
#include "parser.h"
#include "codegen.h"

// Compiler options
typedef struct {
    char *input_file;
    char *output_file;
    bool verbose;
    bool debug;
    bool optimize;
    bool keep_asm;
    bool no_preprocess;  // Skip preprocessing
    bool preprocess_only; // Only run preprocessor
} CompilerOptions;

// Main compiler functions
int compile_file(const char *input_file, const char *output_file, CompilerOptions *opts);
void print_usage(const char *program_name);
void print_version(void);

#endif // KCC_H