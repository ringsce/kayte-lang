#include "kcc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

void print_usage(const char *program_name) {
    printf("KCC - Kayte C Compiler v%s\n\n", KCC_VERSION);
    printf("Usage: %s [options] <input_file>\n\n", program_name);
    printf("Options:\n");
    printf("  -o <file>     Specify output file\n");
    printf("  -v, --verbose Enable verbose output\n");
    printf("  -d, --debug   Enable debug mode\n");
    printf("  -O            Enable optimization\n");
    printf("  -S            Keep assembly output\n");
    printf("  -E            Run preprocessor only\n");
    printf("  --no-preprocess Skip preprocessing step\n");
    printf("  -h, --help    Show this help message\n");
    printf("  --version     Show version information\n");
    printf("\nExamples:\n");
    printf("  %s hello.c\n", program_name);
    printf("  %s -o hello hello.c\n", program_name);
    printf("  %s -v -O hello.c\n", program_name);
    printf("  %s -E macros.c > preprocessed.c\n", program_name);
}

void print_version(void) {
    printf("KCC (Kayte C Compiler) version %s\n", KCC_VERSION);
    printf("Copyright (c) 2025 KCC Contributors\n");
    printf("This is free software; see the source for copying conditions.\n");
}

char *read_file(const char *filename) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        error_fatal("Could not open file '%s'", filename);
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *content = malloc(file_size + 1);
    if (!content) {
        fclose(file);
        error_fatal("Memory allocation failed");
        return NULL;
    }

    size_t bytes_read = fread(content, 1, file_size, file);
    content[bytes_read] = '\0';

    fclose(file);
    return content;
}

int compile_file(const char *input_file, const char *output_file, CompilerOptions *opts) {
    if (opts->verbose) {
        printf("KCC: Compiling '%s'...\n", input_file);
    }

    // Initialize error handling
    error_init();

    char *source_code = NULL;

    // Preprocessing phase
    if (!opts->no_preprocess) {
        if (opts->verbose) {
            printf("KCC: Preprocessing...\n");
        }

        Preprocessor *preprocessor = preprocessor_create();
        if (!preprocessor) {
            return 1;
        }

        char *preprocessed_code = preprocessor_process_file(preprocessor, input_file);
        preprocessor_destroy(preprocessor);

        if (!preprocessed_code) {
            printf("KCC: Preprocessing failed\n");
            return 1;
        }

        // If only preprocessing requested, output and exit
        if (opts->preprocess_only) {
            printf("%s", preprocessed_code);
            free(preprocessed_code);
            return 0;
        }

        source_code = preprocessed_code;

        if (opts->debug) {
            printf("=== PREPROCESSED CODE ===\n");
            printf("%s", preprocessed_code);
            printf("=== END PREPROCESSED CODE ===\n\n");
        }
    } else {
        // Read source without preprocessing
        source_code = read_file(input_file);
        if (!source_code) {
            return 1;
        }
    }

    if (opts->verbose) {
        printf("KCC: Lexical analysis...\n");
    }

    // Lexical analysis
    Lexer *lexer = lexer_create(source_code);
    if (!lexer) {
        free(source_code);
        return 1;
    }

    if (opts->debug) {
        printf("=== TOKENS ===\n");
        Lexer *debug_lexer = lexer_create(source_code);
        Token token;
        do {
            token = lexer_next_token(debug_lexer);
            print_token(&token);
        } while (token.type != TOKEN_EOF);
        lexer_destroy(debug_lexer);
        printf("=== END TOKENS ===\n\n");
    }

    if (opts->verbose) {
        printf("KCC: Syntax analysis...\n");
    }

    // Syntax analysis
    Parser *parser = parser_create(lexer);
    if (!parser) {
        lexer_destroy(lexer);
        free(source_code);
        return 1;
    }

    ASTNode *ast = parser_parse_program(parser);
    if (!ast || error_has_errors()) {
        printf("KCC: Compilation failed with %d error(s)\n", error_count());
        ast_destroy(ast);
        parser_destroy(parser);
        lexer_destroy(lexer);
        free(source_code);
        return 1;
    }

    if (opts->debug) {
        printf("=== AST ===\n");
        ast_print(ast, 0);
        printf("=== END AST ===\n\n");
    }

    if (opts->verbose) {
        printf("KCC: Code generation...\n");
    }

    // Code generation
    CodeGenerator *codegen = codegen_create(output_file);
    if (!codegen) {
        ast_destroy(ast);
        parser_destroy(parser);
        lexer_destroy(lexer);
        free(source_code);
        return 1;
    }

    bool codegen_success = codegen_generate(codegen, ast);

    // Cleanup
    codegen_destroy(codegen);
    ast_destroy(ast);
    parser_destroy(parser);
    lexer_destroy(lexer);
    free(source_code);

    if (!codegen_success || error_has_errors()) {
        printf("KCC: Compilation failed with %d error(s)\n", error_count());
        return 1;
    }

    if (opts->verbose) {
        printf("KCC: Compilation successful. Output written to '%s'\n", output_file);
    }

    return 0;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        print_usage(argv[0]);
        return 1;
    }

    CompilerOptions opts = {0};
    opts.input_file = NULL;
    opts.output_file = "a.out";
    opts.verbose = false;
    opts.debug = false;
    opts.optimize = false;
    opts.keep_asm = false;
    opts.preprocess_only = false;
    opts.no_preprocess = false;

    // Parse command line arguments
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            print_usage(argv[0]);
            return 0;
        } else if (strcmp(argv[i], "--version") == 0) {
            print_version();
            return 0;
        } else if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--verbose") == 0) {
            opts.verbose = true;
        } else if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--debug") == 0) {
            opts.debug = true;
        } else if (strcmp(argv[i], "-O") == 0) {
            opts.optimize = true;
        } else if (strcmp(argv[i], "-S") == 0) {
            opts.keep_asm = true;
        } else if (strcmp(argv[i], "-E") == 0) {
            opts.preprocess_only = true;
        } else if (strcmp(argv[i], "--no-preprocess") == 0) {
            opts.no_preprocess = true;
        } else if (strcmp(argv[i], "-o") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: -o requires an output filename\n");
                return 1;
            }
            opts.output_file = argv[++i];
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Error: Unknown option '%s'\n", argv[i]);
            print_usage(argv[0]);
            return 1;
        } else {
            if (opts.input_file) {
                fprintf(stderr, "Error: Multiple input files specified\n");
                return 1;
            }
            opts.input_file = argv[i];
        }
    }

    if (!opts.input_file) {
        fprintf(stderr, "Error: No input file specified\n");
        print_usage(argv[0]);
        return 1;
    }

    return compile_file(opts.input_file, opts.output_file, &opts);
}
