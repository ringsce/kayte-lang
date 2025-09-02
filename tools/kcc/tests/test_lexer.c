#include "../include/kcc.h"
#include <assert.h>

void test_parser(void) {
    // Test basic parsing
    const char *source = "int main() { return 0; }";
    Lexer *lexer = lexer_create(source);
    Parser *parser = parser_create(lexer);
    
    ASTNode *ast = parser_parse_program(parser);
    
    // Basic checks
    assert(ast != NULL);
    assert(ast->type == AST_PROGRAM);
    assert(ast->data.program.declaration_count == 1);
    
    ASTNode *func = ast->data.program.declarations[0];
    assert(func->type == AST_FUNCTION_DECL);
    assert(strcmp(func->data.function_decl.name, "main") == 0);
    assert(func->data.function_decl.return_type == TYPE_INT);
    
    ast_destroy(ast);
    parser_destroy(parser);
    lexer_destroy(lexer);
}