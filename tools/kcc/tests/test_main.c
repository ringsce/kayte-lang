#include <stdio.h>
#include <assert.h>

void test_lexer(void);
void test_parser(void);

int main(void) {
    printf("Running KCC tests...\n");

    printf("Testing lexer... ");
    test_lexer();
    printf("PASSED\n");

    printf("Testing parser... ");
    test_parser();
    printf("PASSED\n");

    printf("All tests passed!\n");
    return 0;
}