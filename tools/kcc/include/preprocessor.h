#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include "types.h"

#define MAX_MACRO_NAME 64
#define MAX_MACRO_BODY 512
#define MAX_MACRO_PARAMS 32
#define MAX_MACROS 256
#define MAX_INCLUDE_DEPTH 32
#define MAX_LINE_LENGTH 1024

// Forward declaration - avoid redefinition
typedef struct Preprocessor Preprocessor;

// Macro types
typedef enum {
    MACRO_OBJECT,       // #define NAME value
    MACRO_FUNCTION      // #define NAME(params) body
} MacroType;

// Macro parameter
typedef struct MacroParam {
    char name[MAX_MACRO_NAME];
} MacroParam;

// Macro definition
typedef struct Macro {
    char name[MAX_MACRO_NAME];
    MacroType type;
    char body[MAX_MACRO_BODY];
    MacroParam params[MAX_MACRO_PARAMS];
    int param_count;
    bool is_predefined;
    int line_defined;
    char *file_defined;
} Macro;

// Conditional compilation state
typedef enum {
    COND_NONE,
    COND_IF,
    COND_IFDEF,
    COND_IFNDEF,
    COND_ELIF,
    COND_ELSE
} ConditionalType;

typedef struct ConditionalState {
    ConditionalType type;
    bool condition_met;
    bool else_taken;
    int line_number;
} ConditionalState;

// Include file tracking
typedef struct IncludeFile {
    char *filename;
    char *content;
    int line;
    int pos;
} IncludeFile;

// Preprocessor state
struct Preprocessor {
    Macro macros[MAX_MACROS];
    int macro_count;

    ConditionalState cond_stack[32];
    int cond_stack_depth;

    IncludeFile include_stack[MAX_INCLUDE_DEPTH];
    int include_depth;

    char *current_file;
    int current_line;

    bool skip_lines;  // For conditional compilation

    // Output buffer
    char *output;
    size_t output_size;
    size_t output_capacity;
};

// Preprocessor creation and destruction
Preprocessor *preprocessor_create(void);
void preprocessor_destroy(Preprocessor *preprocessor);

// Main preprocessing functions
char *preprocessor_process_file(Preprocessor *pp, const char *filename);
char *preprocessor_process_string(Preprocessor *pp, const char *source, const char *filename);

// Macro management
bool preprocessor_define_macro(Preprocessor *pp, const char *name, const char *body);
bool preprocessor_define_function_macro(Preprocessor *pp, const char *name, 
                                      const char *params[], int param_count, const char *body);
bool preprocessor_undefine_macro(Preprocessor *pp, const char *name);
Macro *preprocessor_find_macro(Preprocessor *pp, const char *name);
bool preprocessor_is_macro_defined(Preprocessor *pp, const char *name);

// Macro expansion
char *preprocessor_expand_macros(Preprocessor *pp, const char *line);
char *preprocessor_expand_function_macro(Preprocessor *pp, Macro *macro, 
                                       const char *args[], int arg_count);

// Directive processing
bool preprocessor_process_directive(Preprocessor *pp, const char *line);
bool preprocessor_handle_define(Preprocessor *pp, const char *directive);
bool preprocessor_handle_undef(Preprocessor *pp, const char *directive);
bool preprocessor_handle_include(Preprocessor *pp, const char *directive);
bool preprocessor_handle_if(Preprocessor *pp, const char *directive);
bool preprocessor_handle_ifdef(Preprocessor *pp, const char *directive);
bool preprocessor_handle_ifndef(Preprocessor *pp, const char *directive);
bool preprocessor_handle_elif(Preprocessor *pp, const char *directive);
bool preprocessor_handle_else(Preprocessor *pp, const char *directive);
bool preprocessor_handle_endif(Preprocessor *pp, const char *directive);
bool preprocessor_handle_error(Preprocessor *pp, const char *directive);
bool preprocessor_handle_warning(Preprocessor *pp, const char *directive);
bool preprocessor_handle_pragma(Preprocessor *pp, const char *directive);
bool preprocessor_handle_line(Preprocessor *pp, const char *directive);

// Conditional compilation
bool preprocessor_evaluate_condition(Preprocessor *pp, const char *condition);
bool preprocessor_should_skip_line(Preprocessor *pp);
void preprocessor_push_conditional(Preprocessor *pp, ConditionalType type, bool condition);
bool preprocessor_pop_conditional(Preprocessor *pp);

// Utility functions
char *preprocessor_read_file(const char *filename);
void preprocessor_append_output(Preprocessor *pp, const char *text);
char *preprocessor_parse_macro_args(const char *call, char *args[], int max_args);
char *preprocessor_substitute_params(const char *body, const char *params[], 
                                   const char *args[], int param_count);
bool preprocessor_is_directive(const char *line);
char *preprocessor_get_directive_name(const char *line);
char *preprocessor_get_directive_args(const char *line);
void preprocessor_add_predefined_macros(Preprocessor *pp);

// String utilities for macros
char *preprocessor_stringify(const char *text);
char *preprocessor_concatenate(const char *left, const char *right);
char *preprocessor_trim_whitespace(char *str);

// Error handling for preprocessor
void preprocessor_error(Preprocessor *pp, const char *format, ...);
void preprocessor_warning(Preprocessor *pp, const char *format, ...);

#endif // PREPROCESSOR_H