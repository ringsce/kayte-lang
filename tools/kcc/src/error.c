#include "kcc.h"

static int g_error_count = 0;

void error_init(void) {
    g_error_count = 0;
}

int error_count(void) {
    return g_error_count;
}

bool error_has_errors(void) {
    return g_error_count > 0;
}

void error_reset(void) {
    g_error_count = 0;
}

static const char *error_type_to_string(ErrorType type) {
    switch (type) {
        case ERROR_LEXICAL: return "Lexical Error";
        case ERROR_SYNTAX: return "Syntax Error";
        case ERROR_SEMANTIC: return "Semantic Error";
        case ERROR_INTERNAL: return "Internal Error";
        case ERROR_FATAL: return "Fatal Error";
        default: return "Unknown Error";
    }
}

void error_report(ErrorType type, int line, int column, const char *format, ...) {
    fprintf(stderr, "%s", error_type_to_string(type));
    
    if (line > 0) {
        fprintf(stderr, " at line %d", line);
        if (column > 0) {
            fprintf(stderr, ", column %d", column);
        }
    }
    
    fprintf(stderr, ": ");
    
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    
    fprintf(stderr, "\n");
    
    g_error_count++;
    
    if (type == ERROR_FATAL) {
        exit(EXIT_FAILURE);
    }
}

void error_syntax(int line, int column, const char *format, ...) {
    fprintf(stderr, "Syntax Error at line %d, column %d: ", line, column);
    
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    
    fprintf(stderr, "\n");
    g_error_count++;
}

void error_semantic(int line, int column, const char *format, ...) {
    fprintf(stderr, "Semantic Error at line %d, column %d: ", line, column);
    
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    
    fprintf(stderr, "\n");
    g_error_count++;
}

void error_fatal(const char *format, ...) {
    fprintf(stderr, "Fatal Error: ");
    
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
}