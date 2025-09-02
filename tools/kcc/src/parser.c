#include "kcc.h"

Parser *parser_create(Lexer *lexer) {
    Parser *parser = malloc(sizeof(Parser));
    if (!parser) {
        error_fatal("Memory allocation failed for parser");
        return NULL;
    }

    parser->lexer = lexer;
    parser->current_token = lexer_next_token(lexer);
    parser->peek_token = lexer_next_token(lexer);

    return parser;
}

void parser_destroy(Parser *parser) {
    if (parser) {
        free(parser);
    }
}

void parser_advance(Parser *parser) {
    parser->current_token = parser->peek_token;
    parser->peek_token = lexer_next_token(parser->lexer);
}

bool parser_match(Parser *parser, TokenType type) {
    return parser->current_token.type == type;
}

bool parser_expect(Parser *parser, TokenType type) {
    if (parser_match(parser, type)) {
        parser_advance(parser);
        return true;
    } else {
        error_syntax(parser->current_token.line, parser->current_token.column,
                    "Expected %s, got %s",
                    token_type_to_string(type),
                    token_type_to_string(parser->current_token.type));
        return false;
    }
}

bool parser_is_type_specifier(TokenType type) {
    return type == TOKEN_INT || type == TOKEN_CHAR_KW || type == TOKEN_VOID;
}

DataType parser_parse_type_specifier(Parser *parser) {
    DataType type = token_to_data_type(parser->current_token.type);
    if (type != TYPE_UNKNOWN) {
        parser_advance(parser);
        return type;
    }

    error_syntax(parser->current_token.line, parser->current_token.column,
                "Expected type specifier");
    return TYPE_UNKNOWN;
}

ASTNode *parser_parse_program(Parser *parser) {
    ASTNode *program = ast_create_program();

    while (!parser_match(parser, TOKEN_EOF)) {
        ASTNode *declaration = parser_parse_declaration(parser);
        if (declaration) {
            ast_add_declaration(program, declaration);
        } else {
            // Skip to next potential declaration
            while (!parser_match(parser, TOKEN_EOF) &&
                   !parser_is_type_specifier(parser->current_token.type)) {
                parser_advance(parser);
            }
        }
    }

    return program;
}

ASTNode *parser_parse_declaration(Parser *parser) {
    if (!parser_is_type_specifier(parser->current_token.type)) {
        error_syntax(parser->current_token.line, parser->current_token.column,
                    "Expected declaration");
        return NULL;
    }

    DataType type = parser_parse_type_specifier(parser);

    if (!parser_match(parser, TOKEN_IDENTIFIER)) {
        error_syntax(parser->current_token.line, parser->current_token.column,
                    "Expected identifier");
        return NULL;
    }

    char *name = strdup(parser->current_token.value);
    parser_advance(parser);

    if (parser_match(parser, TOKEN_LPAREN)) {
        // Function declaration
        return parser_parse_function_declaration(parser, type, name);
    } else {
        // Variable declaration
        ASTNode *var_decl = parser_parse_variable_declaration(parser, type);
        if (var_decl) {
            free(var_decl->data.var_decl.name);
            var_decl->data.var_decl.name = name;
        } else {
            free(name);
        }
        return var_decl;
    }
}

ASTNode *parser_parse_function_declaration(Parser *parser, DataType return_type, const char *name) {
    ASTNode *func_decl = ast_create_function_decl(return_type, name);

    parser_expect(parser, TOKEN_LPAREN);

    // Parse parameters
    if (!parser_match(parser, TOKEN_RPAREN)) {
        do {
            if (!parser_is_type_specifier(parser->current_token.type)) {
                error_syntax(parser->current_token.line, parser->current_token.column,
                            "Expected parameter type");
                break;
            }

            DataType param_type = parser_parse_type_specifier(parser);

            if (!parser_match(parser, TOKEN_IDENTIFIER)) {
                error_syntax(parser->current_token.line, parser->current_token.column,
                            "Expected parameter name");
                break;
            }

            char *param_name = strdup(parser->current_token.value);
            parser_advance(parser);

            ASTNode *parameter = ast_create_parameter(param_type, param_name);
            ast_add_parameter(func_decl, parameter);
            free(param_name);

            if (parser_match(parser, TOKEN_COMMA)) {
                parser_advance(parser);
            } else {
                break;
            }
        } while (true);
    }

    parser_expect(parser, TOKEN_RPAREN);

    // Parse function body
    if (parser_match(parser, TOKEN_LBRACE)) {
        func_decl->data.function_decl.body = parser_parse_compound_statement(parser);
    } else {
        parser_expect(parser, TOKEN_SEMICOLON);
    }

    return func_decl;
}

ASTNode *parser_parse_variable_declaration(Parser *parser, DataType var_type) {
    ASTNode *initializer = NULL;

    if (parser_match(parser, TOKEN_ASSIGN)) {
        parser_advance(parser);
        initializer = parser_parse_expression(parser);
    }

    parser_expect(parser, TOKEN_SEMICOLON);

    return ast_create_var_decl(var_type, "", initializer);
}

ASTNode *parser_parse_statement(Parser *parser) {
    switch (parser->current_token.type) {
        case TOKEN_LBRACE:
            return parser_parse_compound_statement(parser);
        case TOKEN_RETURN:
            return parser_parse_return_statement(parser);
        case TOKEN_IF:
            return parser_parse_if_statement(parser);
        case TOKEN_WHILE:
            return parser_parse_while_statement(parser);
        case TOKEN_FOR:
            return parser_parse_for_statement(parser);
        case TOKEN_BREAK:
            parser_advance(parser);
            parser_expect(parser, TOKEN_SEMICOLON);
            return ast_create_break_stmt();
        case TOKEN_CONTINUE:
            parser_advance(parser);
            parser_expect(parser, TOKEN_SEMICOLON);
            return ast_create_continue_stmt();
        default:
            return parser_parse_expression_statement(parser);
    }
}

ASTNode *parser_parse_compound_statement(Parser *parser) {
    ASTNode *compound = ast_create_compound_stmt();

    parser_expect(parser, TOKEN_LBRACE);

    while (!parser_match(parser, TOKEN_RBRACE) && !parser_match(parser, TOKEN_EOF)) {
        ASTNode *stmt = NULL;

        if (parser_is_type_specifier(parser->current_token.type)) {
            // Variable declaration
            DataType var_type = parser_parse_type_specifier(parser);

            if (parser_match(parser, TOKEN_IDENTIFIER)) {
                char *var_name = strdup(parser->current_token.value);
                parser_advance(parser);

                ASTNode *var_decl = parser_parse_variable_declaration(parser, var_type);
                if (var_decl) {
                    free(var_decl->data.var_decl.name);
                    var_decl->data.var_decl.name = var_name;
                    stmt = var_decl;
                } else {
                    free(var_name);
                }
            }
        } else {
            stmt = parser_parse_statement(parser);
        }

        if (stmt) {
            ast_add_statement(compound, stmt);
        }
    }

    parser_expect(parser, TOKEN_RBRACE);

    return compound;
}

ASTNode *parser_parse_expression_statement(Parser *parser) {
    ASTNode *expr = parser_parse_expression(parser);
    parser_expect(parser, TOKEN_SEMICOLON);
    return ast_create_expression_stmt(expr);
}

ASTNode *parser_parse_return_statement(Parser *parser) {
    parser_advance(parser); // consume 'return'

    ASTNode *expr = NULL;
    if (!parser_match(parser, TOKEN_SEMICOLON)) {
        expr = parser_parse_expression(parser);
    }

    parser_expect(parser, TOKEN_SEMICOLON);

    return ast_create_return_stmt(expr);
}

ASTNode *parser_parse_if_statement(Parser *parser) {
    parser_advance(parser); // consume 'if'

    parser_expect(parser, TOKEN_LPAREN);
    ASTNode *condition = parser_parse_expression(parser);
    parser_expect(parser, TOKEN_RPAREN);

    ASTNode *then_stmt = parser_parse_statement(parser);

    ASTNode *else_stmt = NULL;
    if (parser_match(parser, TOKEN_ELSE)) {
        parser_advance(parser);
        else_stmt = parser_parse_statement(parser);
    }

    return ast_create_if_stmt(condition, then_stmt, else_stmt);
}

ASTNode *parser_parse_while_statement(Parser *parser) {
    parser_advance(parser); // consume 'while'

    parser_expect(parser, TOKEN_LPAREN);
    ASTNode *condition = parser_parse_expression(parser);
    parser_expect(parser, TOKEN_RPAREN);

    ASTNode *body = parser_parse_statement(parser);

    return ast_create_while_stmt(condition, body);
}

ASTNode *parser_parse_for_statement(Parser *parser) {
    parser_advance(parser); // consume 'for'

    parser_expect(parser, TOKEN_LPAREN);

    ASTNode *init = NULL;
    if (!parser_match(parser, TOKEN_SEMICOLON)) {
        init = parser_parse_expression(parser);
    }
    parser_expect(parser, TOKEN_SEMICOLON);

    ASTNode *condition = NULL;
    if (!parser_match(parser, TOKEN_SEMICOLON)) {
        condition = parser_parse_expression(parser);
    }
    parser_expect(parser, TOKEN_SEMICOLON);

    ASTNode *update = NULL;
    if (!parser_match(parser, TOKEN_RPAREN)) {
        update = parser_parse_expression(parser);
    }
    parser_expect(parser, TOKEN_RPAREN);

    ASTNode *body = parser_parse_statement(parser);

    return ast_create_for_stmt(init, condition, update, body);
}

ASTNode *parser_parse_expression(Parser *parser) {
    return parser_parse_assignment_expression(parser);
}

ASTNode *parser_parse_assignment_expression(Parser *parser) {
    ASTNode *left = parser_parse_logical_or_expression(parser);

    if (parser_match(parser, TOKEN_ASSIGN)) {
        parser_advance(parser);
        ASTNode *right = parser_parse_assignment_expression(parser);

        if (left->type == AST_IDENTIFIER) {
            return ast_create_assignment(left->data.identifier.name, right);
        } else {
            error_syntax(parser->current_token.line, parser->current_token.column,
                        "Invalid left-hand side in assignment");
        }
    }

    return left;
}

ASTNode *parser_parse_logical_or_expression(Parser *parser) {
    ASTNode *left = parser_parse_logical_and_expression(parser);

    while (parser_match(parser, TOKEN_OR)) {
        TokenType op = parser->current_token.type;
        parser_advance(parser);
        ASTNode *right = parser_parse_logical_and_expression(parser);
        left = ast_create_binary_expr(op, left, right);
    }

    return left;
}

ASTNode *parser_parse_logical_and_expression(Parser *parser) {
    ASTNode *left = parser_parse_equality_expression(parser);

    while (parser_match(parser, TOKEN_AND)) {
        TokenType op = parser->current_token.type;
        parser_advance(parser);
        ASTNode *right = parser_parse_equality_expression(parser);
        left = ast_create_binary_expr(op, left, right);
    }

    return left;
}

ASTNode *parser_parse_equality_expression(Parser *parser) {
    ASTNode *left = parser_parse_relational_expression(parser);

    while (parser_match(parser, TOKEN_EQUAL) || parser_match(parser, TOKEN_NOT_EQUAL)) {
        TokenType op = parser->current_token.type;
        parser_advance(parser);
        ASTNode *right = parser_parse_relational_expression(parser);
        left = ast_create_binary_expr(op, left, right);
    }

    return left;
}

ASTNode *parser_parse_relational_expression(Parser *parser) {
    ASTNode *left = parser_parse_additive_expression(parser);

    while (parser_match(parser, TOKEN_LESS) || parser_match(parser, TOKEN_LESS_EQUAL) ||
           parser_match(parser, TOKEN_GREATER) || parser_match(parser, TOKEN_GREATER_EQUAL)) {
        TokenType op = parser->current_token.type;
        parser_advance(parser);
        ASTNode *right = parser_parse_additive_expression(parser);
        left = ast_create_binary_expr(op, left, right);
    }

    return left;
}

ASTNode *parser_parse_additive_expression(Parser *parser) {
    ASTNode *left = parser_parse_multiplicative_expression(parser);

    while (parser_match(parser, TOKEN_PLUS) || parser_match(parser, TOKEN_MINUS)) {
        TokenType op = parser->current_token.type;
        parser_advance(parser);
        ASTNode *right = parser_parse_multiplicative_expression(parser);
        left = ast_create_binary_expr(op, left, right);
    }

    return left;
}

ASTNode *parser_parse_multiplicative_expression(Parser *parser) {
    ASTNode *left = parser_parse_unary_expression(parser);

    while (parser_match(parser, TOKEN_MULTIPLY) || parser_match(parser, TOKEN_DIVIDE) ||
           parser_match(parser, TOKEN_MODULO)) {
        TokenType op = parser->current_token.type;
        parser_advance(parser);
        ASTNode *right = parser_parse_unary_expression(parser);
        left = ast_create_binary_expr(op, left, right);
    }

    return left;
}

ASTNode *parser_parse_unary_expression(Parser *parser) {
    if (parser_match(parser, TOKEN_MINUS) || parser_match(parser, TOKEN_NOT)) {
        TokenType op = parser->current_token.type;
        parser_advance(parser);
        ASTNode *operand = parser_parse_unary_expression(parser);
        return ast_create_unary_expr(op, operand);
    }

    return parser_parse_primary_expression(parser);
}

ASTNode *parser_parse_primary_expression(Parser *parser) {
    ASTNode *primary = NULL;

    switch (parser->current_token.type) {
        case TOKEN_NUMBER: {
            int value = atoi(parser->current_token.value);
            primary = ast_create_number(value);
            parser_advance(parser);
            break;
        }
        case TOKEN_STRING: {
            primary = ast_create_string(parser->current_token.value);
            parser_advance(parser);
            break;
        }
        case TOKEN_IDENTIFIER: {
            primary = ast_create_identifier(parser->current_token.value);
            parser_advance(parser);
            break;
        }
        case TOKEN_LPAREN: {
            parser_advance(parser);
            primary = parser_parse_expression(parser);
            parser_expect(parser, TOKEN_RPAREN);
            break;
        }
        default:
            error_syntax(parser->current_token.line, parser->current_token.column,
                        "Expected primary expression");
            return NULL;
    }

    // Handle function calls
    if (primary && primary->type == AST_IDENTIFIER && parser_match(parser, TOKEN_LPAREN)) {
        primary = parser_parse_call_expression(parser, primary);
    }

    return primary;
}

ASTNode *parser_parse_call_expression(Parser *parser, ASTNode *primary) {
    if (primary->type != AST_IDENTIFIER) {
        return primary;
    }

    ASTNode *call = ast_create_call_expr(primary->data.identifier.name);

    parser_advance(parser); // consume '('

    if (!parser_match(parser, TOKEN_RPAREN)) {
        do {
            ASTNode *arg = parser_parse_expression(parser);
            if (arg) {
                ast_add_argument(call, arg);
            }

            if (parser_match(parser, TOKEN_COMMA)) {
                parser_advance(parser);
            } else {
                break;
            }
        } while (true);
    }

    parser_expect(parser, TOKEN_RPAREN);

    // Clean up the original identifier node
    ast_destroy(primary);

    return call;
}
