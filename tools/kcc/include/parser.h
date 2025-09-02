#ifndef PARSER_H
#define PARSER_H

#include <stdbool.h>
#include "types.h"

struct Parser {
    struct Lexer *lexer;
    Token current_token;
    Token peek_token;
};

// Parser creation and destruction
struct Parser *parser_create(struct Lexer *lexer);
void parser_destroy(struct Parser *parser);

// Main parsing function
struct ASTNode *parser_parse_program(struct Parser *parser);

// Token management
void parser_advance(struct Parser *parser);
bool parser_match(struct Parser *parser, TokenType type);
bool parser_expect(struct Parser *parser, TokenType type);

// Parsing functions for different constructs
struct ASTNode *parser_parse_declaration(struct Parser *parser);
struct ASTNode *parser_parse_function_declaration(struct Parser *parser, DataType return_type, const char *name);
struct ASTNode *parser_parse_variable_declaration(struct Parser *parser, DataType var_type);
struct ASTNode *parser_parse_statement(struct Parser *parser);
struct ASTNode *parser_parse_compound_statement(struct Parser *parser);
struct ASTNode *parser_parse_expression_statement(struct Parser *parser);
struct ASTNode *parser_parse_return_statement(struct Parser *parser);
struct ASTNode *parser_parse_if_statement(struct Parser *parser);
struct ASTNode *parser_parse_while_statement(struct Parser *parser);
struct ASTNode *parser_parse_for_statement(struct Parser *parser);
struct ASTNode *parser_parse_expression(struct Parser *parser);
struct ASTNode *parser_parse_assignment_expression(struct Parser *parser);
struct ASTNode *parser_parse_logical_or_expression(struct Parser *parser);
struct ASTNode *parser_parse_logical_and_expression(struct Parser *parser);
struct ASTNode *parser_parse_equality_expression(struct Parser *parser);
struct ASTNode *parser_parse_relational_expression(struct Parser *parser);
struct ASTNode *parser_parse_additive_expression(struct Parser *parser);
struct ASTNode *parser_parse_multiplicative_expression(struct Parser *parser);
struct ASTNode *parser_parse_unary_expression(struct Parser *parser);
struct ASTNode *parser_parse_primary_expression(struct Parser *parser);
struct ASTNode *parser_parse_call_expression(struct Parser *parser, struct ASTNode *primary);

// Utility functions
DataType parser_parse_type_specifier(struct Parser *parser);
bool parser_is_type_specifier(TokenType type);

#endif // PARSER_H