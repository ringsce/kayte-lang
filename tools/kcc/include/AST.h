#ifndef AST_H
#define AST_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "types.h"

typedef enum {
    AST_PROGRAM,
    AST_FUNCTION_DECL,
    AST_VAR_DECL,
    AST_PARAMETER,
    AST_COMPOUND_STMT,
    AST_EXPRESSION_STMT,
    AST_RETURN_STMT,
    AST_IF_STMT,
    AST_WHILE_STMT,
    AST_FOR_STMT,
    AST_BREAK_STMT,
    AST_CONTINUE_STMT,
    AST_BINARY_EXPR,
    AST_UNARY_EXPR,
    AST_CALL_EXPR,
    AST_IDENTIFIER,
    AST_NUMBER,
    AST_STRING,
    AST_ASSIGNMENT
} ASTNodeType;

struct ASTNode {
    ASTNodeType type;
    DataType data_type;
    int line;
    int column;

    union {
        struct {
            struct ASTNode **declarations;
            int declaration_count;
        } program;

        struct {
            DataType return_type;
            char *name;
            struct ASTNode **parameters;
            int parameter_count;
            struct ASTNode *body;
        } function_decl;

        struct {
            DataType var_type;
            char *name;
            struct ASTNode *initializer;
        } var_decl;

        struct {
            DataType param_type;
            char *name;
        } parameter;

        struct {
            struct ASTNode **statements;
            int statement_count;
        } compound_stmt;

        struct {
            struct ASTNode *expression;
        } expression_stmt;

        struct {
            struct ASTNode *expression;
        } return_stmt;

        struct {
            struct ASTNode *condition;
            struct ASTNode *then_stmt;
            struct ASTNode *else_stmt;
        } if_stmt;

        struct {
            struct ASTNode *condition;
            struct ASTNode *body;
        } while_stmt;

        struct {
            struct ASTNode *init;
            struct ASTNode *condition;
            struct ASTNode *update;
            struct ASTNode *body;
        } for_stmt;

        struct {
            TokenType operator;
            struct ASTNode *left;
            struct ASTNode *right;
        } binary_expr;

        struct {
            TokenType operator;
            struct ASTNode *operand;
        } unary_expr;

        struct {
            char *function_name;
            struct ASTNode **arguments;
            int argument_count;
        } call_expr;

        struct {
            char *name;
        } identifier;

        struct {
            int value;
        } number;

        struct {
            char *value;
        } string;

        struct {
            char *variable;
            struct ASTNode *value;
        } assignment;
    } data;
};

// AST creation functions
struct ASTNode *ast_create_program(void);
struct ASTNode *ast_create_function_decl(DataType return_type, const char *name);
struct ASTNode *ast_create_var_decl(DataType var_type, const char *name, struct ASTNode *initializer);
struct ASTNode *ast_create_parameter(DataType param_type, const char *name);
struct ASTNode *ast_create_compound_stmt(void);
struct ASTNode *ast_create_expression_stmt(struct ASTNode *expression);
struct ASTNode *ast_create_return_stmt(struct ASTNode *expression);
struct ASTNode *ast_create_if_stmt(struct ASTNode *condition, struct ASTNode *then_stmt, struct ASTNode *else_stmt);
struct ASTNode *ast_create_while_stmt(struct ASTNode *condition, struct ASTNode *body);
struct ASTNode *ast_create_for_stmt(struct ASTNode *init, struct ASTNode *condition, struct ASTNode *update, struct ASTNode *body);
struct ASTNode *ast_create_break_stmt(void);
struct ASTNode *ast_create_continue_stmt(void);
struct ASTNode *ast_create_binary_expr(TokenType operator, struct ASTNode *left, struct ASTNode *right);
struct ASTNode *ast_create_unary_expr(TokenType operator, struct ASTNode *operand);
struct ASTNode *ast_create_call_expr(const char *function_name);
struct ASTNode *ast_create_identifier(const char *name);
struct ASTNode *ast_create_number(int value);
struct ASTNode *ast_create_string(const char *value);
struct ASTNode *ast_create_assignment(const char *variable, struct ASTNode *value);

// AST manipulation functions
void ast_add_declaration(struct ASTNode *program, struct ASTNode *declaration);
void ast_add_parameter(struct ASTNode *function, struct ASTNode *parameter);
void ast_add_statement(struct ASTNode *compound_stmt, struct ASTNode *statement);
void ast_add_argument(struct ASTNode *call_expr, struct ASTNode *argument);

// AST utility functions
void ast_destroy(struct ASTNode *node);
void ast_print(struct ASTNode *node, int indent);
const char *ast_node_type_to_string(ASTNodeType type);
const char *data_type_to_string(DataType type);
DataType token_to_data_type(TokenType token);

#endif // AST_H