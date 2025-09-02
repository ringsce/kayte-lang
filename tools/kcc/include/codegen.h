#ifndef CODEGEN_H
#define CODEGEN_H

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

typedef struct CodeGenerator {
    FILE *output_file;
    int label_counter;
    int temp_counter;
} CodeGenerator;

// Code generator creation and destruction
CodeGenerator *codegen_create(const char *output_filename);
void codegen_destroy(CodeGenerator *codegen);

// Main code generation function
bool codegen_generate(CodeGenerator *codegen, struct ASTNode *ast);

// Code generation for different AST nodes
void codegen_program(CodeGenerator *codegen, struct ASTNode *node);
void codegen_function_declaration(CodeGenerator *codegen, struct ASTNode *node);
void codegen_variable_declaration(CodeGenerator *codegen, struct ASTNode *node);
void codegen_statement(CodeGenerator *codegen, struct ASTNode *node);
void codegen_compound_statement(CodeGenerator *codegen, struct ASTNode *node);
void codegen_expression_statement(CodeGenerator *codegen, struct ASTNode *node);
void codegen_return_statement(CodeGenerator *codegen, struct ASTNode *node);
void codegen_if_statement(CodeGenerator *codegen, struct ASTNode *node);
void codegen_while_statement(CodeGenerator *codegen, struct ASTNode *node);
void codegen_for_statement(CodeGenerator *codegen, struct ASTNode *node);
void codegen_expression(CodeGenerator *codegen, struct ASTNode *node);
void codegen_binary_expression(CodeGenerator *codegen, struct ASTNode *node);
void codegen_unary_expression(CodeGenerator *codegen, struct ASTNode *node);
void codegen_call_expression(CodeGenerator *codegen, struct ASTNode *node);
void codegen_identifier(CodeGenerator *codegen, struct ASTNode *node);
void codegen_number(CodeGenerator *codegen, struct ASTNode *node);
void codegen_string(CodeGenerator *codegen, struct ASTNode *node);
void codegen_assignment(CodeGenerator *codegen, struct ASTNode *node);

// Utility functions
void codegen_emit(CodeGenerator *codegen, const char *format, ...);
char *codegen_new_label(CodeGenerator *codegen);
char *codegen_new_temp(CodeGenerator *codegen);

#endif // CODEGEN_H