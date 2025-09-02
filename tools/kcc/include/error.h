#ifndef ERROR_H
#define ERROR_H

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

typedef enum {
    ERROR_LEXICAL,
    ERROR_SYNTAX,
    ERROR_SEMANTIC,
    ERROR_INTERNAL,
    ERROR_FATAL
} ErrorType;

// Error reporting functions
void error_report(ErrorType type, int line, int column, const char *format, ...);
void error_syntax(int line, int column, const char *format, ...);
void error_semantic(int line, int column, const char *format, ...);
void error_fatal(const char *format, ...);

// Error state management
void error_init(void);
int error_count(void);
bool error_has_errors(void);
void error_reset(void);

#endif // ERROR_H