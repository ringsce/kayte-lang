#include "kcc.h"

static const struct {
    const char *keyword;
    TokenType type;
} keywords[] = {
    {"int", TOKEN_INT},
    {"char", TOKEN_CHAR_KW},
    {"void", TOKEN_VOID},
    {"if", TOKEN_IF},
    {"else", TOKEN_ELSE},
    {"while", TOKEN_WHILE},
    {"for", TOKEN_FOR},
    {"return", TOKEN_RETURN},
    {"break", TOKEN_BREAK},
    {"continue", TOKEN_CONTINUE},
    {NULL, TOKEN_UNKNOWN}
};

Lexer *lexer_create(const char *input) {
    Lexer *lexer = malloc(sizeof(Lexer));
    if (!lexer) {
        error_fatal("Memory allocation failed for lexer");
        return NULL;
    }

    lexer->input_length = strlen(input);
    lexer->input = malloc(lexer->input_length + 1);
    if (!lexer->input) {
        free(lexer);
        error_fatal("Memory allocation failed for input buffer");
        return NULL;
    }

    strcpy(lexer->input, input);
    lexer->pos = 0;
    lexer->line = 1;
    lexer->column = 1;

    return lexer;
}

void lexer_destroy(Lexer *lexer) {
    if (lexer) {
        free(lexer->input);
        free(lexer);
    }
}

static char lexer_current_char(Lexer *lexer) {
    if (lexer->pos >= lexer->input_length) {
        return '\0';
    }
    return lexer->input[lexer->pos];
}

static char lexer_peek_char(Lexer *lexer) {
    if (lexer->pos + 1 >= lexer->input_length) {
        return '\0';
    }
    return lexer->input[lexer->pos + 1];
}

static void lexer_advance(Lexer *lexer) {
    if (lexer->pos < lexer->input_length) {
        if (lexer->input[lexer->pos] == '\n') {
            lexer->line++;
            lexer->column = 1;
        } else {
            lexer->column++;
        }
        lexer->pos++;
    }
}

static void lexer_skip_whitespace(Lexer *lexer) {
    while (isspace(lexer_current_char(lexer))) {
        lexer_advance(lexer);
    }
}

static void lexer_skip_comment(Lexer *lexer) {
    if (lexer_current_char(lexer) == '/' && lexer_peek_char(lexer) == '/') {
        while (lexer_current_char(lexer) != '\n' && lexer_current_char(lexer) != '\0') {
            lexer_advance(lexer);
        }
    } else if (lexer_current_char(lexer) == '/' && lexer_peek_char(lexer) == '*') {
        lexer_advance(lexer); // skip '/'
        lexer_advance(lexer); // skip '*'
        while (!(lexer_current_char(lexer) == '*' && lexer_peek_char(lexer) == '/') &&
               lexer_current_char(lexer) != '\0') {
            lexer_advance(lexer);
        }
        if (lexer_current_char(lexer) == '*') {
            lexer_advance(lexer); // skip '*'
            lexer_advance(lexer); // skip '/'
        }
    }
}

bool is_keyword(const char *str, TokenType *type) {
    for (int i = 0; keywords[i].keyword != NULL; i++) {
        if (strcmp(str, keywords[i].keyword) == 0) {
            *type = keywords[i].type;
            return true;
        }
    }
    return false;
}

static Token lexer_read_identifier(Lexer *lexer) {
    Token token;
    token.line = lexer->line;
    token.column = lexer->column;

    int i = 0;
    while ((isalnum(lexer_current_char(lexer)) || lexer_current_char(lexer) == '_') &&
           i < MAX_IDENTIFIER_LENGTH - 1) {
        token.value[i++] = lexer_current_char(lexer);
        lexer_advance(lexer);
    }
    token.value[i] = '\0';

    if (!is_keyword(token.value, &token.type)) {
        token.type = TOKEN_IDENTIFIER;
    }

    return token;
}

static Token lexer_read_number(Lexer *lexer) {
    Token token;
    token.type = TOKEN_NUMBER;
    token.line = lexer->line;
    token.column = lexer->column;

    int i = 0;
    while (isdigit(lexer_current_char(lexer)) && i < MAX_STRING_LENGTH - 1) {
        token.value[i++] = lexer_current_char(lexer);
        lexer_advance(lexer);
    }
    token.value[i] = '\0';

    return token;
}

static Token lexer_read_string(Lexer *lexer) {
    Token token;
    token.type = TOKEN_STRING;
    token.line = lexer->line;
    token.column = lexer->column;

    lexer_advance(lexer); // skip opening quote

    int i = 0;
    while (lexer_current_char(lexer) != '"' && lexer_current_char(lexer) != '\0' &&
           i < MAX_STRING_LENGTH - 1) {
        if (lexer_current_char(lexer) == '\\') {
            lexer_advance(lexer);
            switch (lexer_current_char(lexer)) {
                case 'n': token.value[i++] = '\n'; break;
                case 't': token.value[i++] = '\t'; break;
                case 'r': token.value[i++] = '\r'; break;
                case '\\': token.value[i++] = '\\'; break;
                case '"': token.value[i++] = '"'; break;
                default: token.value[i++] = lexer_current_char(lexer); break;
            }
        } else {
            token.value[i++] = lexer_current_char(lexer);
        }
        lexer_advance(lexer);
    }

    if (lexer_current_char(lexer) == '"') {
        lexer_advance(lexer); // skip closing quote
    } else {
        error_syntax(lexer->line, lexer->column, "Unterminated string literal");
    }

    token.value[i] = '\0';
    return token;
}

Token lexer_next_token(Lexer *lexer) {
    Token token;

    while (lexer_current_char(lexer) != '\0') {
        lexer_skip_whitespace(lexer);

        if (lexer_current_char(lexer) == '/' &&
            (lexer_peek_char(lexer) == '/' || lexer_peek_char(lexer) == '*')) {
            lexer_skip_comment(lexer);
            continue;
        }

        token.line = lexer->line;
        token.column = lexer->column;

        char c = lexer_current_char(lexer);

        if (isalpha(c) || c == '_') {
            return lexer_read_identifier(lexer);
        }

        if (isdigit(c)) {
            return lexer_read_number(lexer);
        }

        if (c == '"') {
            return lexer_read_string(lexer);
        }

        lexer_advance(lexer);
        token.value[0] = c;
        token.value[1] = '\0';

        switch (c) {
            case '+': token.type = TOKEN_PLUS; break;
            case '-': token.type = TOKEN_MINUS; break;
            case '*': token.type = TOKEN_MULTIPLY; break;
            case '/': token.type = TOKEN_DIVIDE; break;
            case '%': token.type = TOKEN_MODULO; break;
            case '=':
                if (lexer_current_char(lexer) == '=') {
                    lexer_advance(lexer);
                    token.type = TOKEN_EQUAL;
                    strcpy(token.value, "==");
                } else {
                    token.type = TOKEN_ASSIGN;
                }
                break;
            case '!':
                if (lexer_current_char(lexer) == '=') {
                    lexer_advance(lexer);
                    token.type = TOKEN_NOT_EQUAL;
                    strcpy(token.value, "!=");
                } else {
                    token.type = TOKEN_NOT;
                }
                break;
            case '<':
                if (lexer_current_char(lexer) == '=') {
                    lexer_advance(lexer);
                    token.type = TOKEN_LESS_EQUAL;
                    strcpy(token.value, "<=");
                } else {
                    token.type = TOKEN_LESS;
                }
                break;
            case '>':
                if (lexer_current_char(lexer) == '=') {
                    lexer_advance(lexer);
                    token.type = TOKEN_GREATER_EQUAL;
                    strcpy(token.value, ">=");
                } else {
                    token.type = TOKEN_GREATER;
                }
                break;
            case '&':
                if (lexer_current_char(lexer) == '&') {
                    lexer_advance(lexer);
                    token.type = TOKEN_AND;
                    strcpy(token.value, "&&");
                } else {
                    token.type = TOKEN_UNKNOWN;
                }
                break;
            case '|':
                if (lexer_current_char(lexer) == '|') {
                    lexer_advance(lexer);
                    token.type = TOKEN_OR;
                    strcpy(token.value, "||");
                } else {
                    token.type = TOKEN_UNKNOWN;
                }
                break;
            case ';': token.type = TOKEN_SEMICOLON; break;
            case ',': token.type = TOKEN_COMMA; break;
            case '(': token.type = TOKEN_LPAREN; break;
            case ')': token.type = TOKEN_RPAREN; break;
            case '{': token.type = TOKEN_LBRACE; break;
            case '}': token.type = TOKEN_RBRACE; break;
            case '[': token.type = TOKEN_LBRACKET; break;
            case ']': token.type = TOKEN_RBRACKET; break;
            default: token.type = TOKEN_UNKNOWN; break;
        }

        return token;
    }

    token.type = TOKEN_EOF;
    token.line = lexer->line;
    token.column = lexer->column;
    strcpy(token.value, "EOF");

    return token;
}

Token lexer_peek_token(Lexer *lexer) {
    size_t saved_pos = lexer->pos;
    size_t saved_line = lexer->line;
    size_t saved_column = lexer->column;

    Token token = lexer_next_token(lexer);

    lexer->pos = saved_pos;
    lexer->line = saved_line;
    lexer->column = saved_column;

    return token;
}

const char *token_type_to_string(TokenType type) {
    switch (type) {
        case TOKEN_EOF: return "EOF";
        case TOKEN_IDENTIFIER: return "IDENTIFIER";
        case TOKEN_NUMBER: return "NUMBER";
        case TOKEN_STRING: return "STRING";
        case TOKEN_CHAR: return "CHAR";
        case TOKEN_INT: return "INT";
        case TOKEN_CHAR_KW: return "CHAR_KW";
        case TOKEN_VOID: return "VOID";
        case TOKEN_IF: return "IF";
        case TOKEN_ELSE: return "ELSE";
        case TOKEN_WHILE: return "WHILE";
        case TOKEN_FOR: return "FOR";
        case TOKEN_RETURN: return "RETURN";
        case TOKEN_BREAK: return "BREAK";
        case TOKEN_CONTINUE: return "CONTINUE";
        case TOKEN_PLUS: return "PLUS";
        case TOKEN_MINUS: return "MINUS";
        case TOKEN_MULTIPLY: return "MULTIPLY";
        case TOKEN_DIVIDE: return "DIVIDE";
        case TOKEN_MODULO: return "MODULO";
        case TOKEN_ASSIGN: return "ASSIGN";
        case TOKEN_EQUAL: return "EQUAL";
        case TOKEN_NOT_EQUAL: return "NOT_EQUAL";
        case TOKEN_LESS: return "LESS";
        case TOKEN_LESS_EQUAL: return "LESS_EQUAL";
        case TOKEN_GREATER: return "GREATER";
        case TOKEN_GREATER_EQUAL: return "GREATER_EQUAL";
        case TOKEN_AND: return "AND";
        case TOKEN_OR: return "OR";
        case TOKEN_NOT: return "NOT";
        case TOKEN_SEMICOLON: return "SEMICOLON";
        case TOKEN_COMMA: return "COMMA";
        case TOKEN_LPAREN: return "LPAREN";
        case TOKEN_RPAREN: return "RPAREN";
        case TOKEN_LBRACE: return "LBRACE";
        case TOKEN_RBRACE: return "RBRACE";
        case TOKEN_LBRACKET: return "LBRACKET";
        case TOKEN_RBRACKET: return "RBRACKET";
        default: return "UNKNOWN";
    }
}

void print_token(const Token *token) {
    printf("Token: %s '%s' at line %d, column %d\n",
           token_type_to_string(token->type),
           token->value,
           token->line,
           token->column);
}
