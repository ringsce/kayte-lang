#include "../include/kcc.h"
#include <assert.h>

void test_lexer(void) {
    // Test basic tokenization
    const char *source = "int x = 42;";
    Lexer *lexer = lexer_create(source);
    
    Token token1 = lexer_next_token(lexer);
    assert(token1.type == TOKEN_INT);
    
    Token token2 = lexer_next_token(lexer);
    assert(token2.type == TOKEN_IDENTIFIER);
    assert(strcmp(token2.value, "x") == 0);
    
    Token token3 = lexer_next_token(lexer);
    assert(token3.type == TOKEN_ASSIGN);
    
    Token token4 = lexer_next_token(lexer);
    assert(token4.type == TOKEN_NUMBER);
    assert(strcmp(token4.value, "42") == 0);
    
    Token token5 = lexer_next_token(lexer);
    assert(token5.type == TOKEN_SEMICOLON);
    (void)token1; (void)token2; (void)token3; (void)token4; (void)token5;

    lexer_destroy(lexer);
}