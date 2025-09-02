#ifndef LEXER_H
#define LEXER_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include "types.h"

typedef struct Token {
    TokenType type;
    char value[MAX_STRING_LENGTH];
    int line;
    int column;
} Token;

struct Lexer {
    char *input;
    size_t pos;
    size_t line;
    size_t column;
    size_t input_length;
};

// Lexer functions
struct Lexer *lexer_create(const char *input);
void lexer_destroy(struct Lexer *lexer);
Token lexer_next_token(struct Lexer *lexer);
Token lexer_peek_token(struct Lexer *lexer);
const char *token_type_to_string(TokenType type);
bool is_keyword(const char *str, TokenType *type);
void print_token(const Token *token);

#endif // LEXER_H