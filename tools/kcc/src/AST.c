#include "kcc.h"

ASTNode *ast_create_node(ASTNodeType type) {
    ASTNode *node = malloc(sizeof(ASTNode));
    if (!node) {
        error_fatal("Memory allocation failed for AST node");
        return NULL;
    }
    
    memset(node, 0, sizeof(ASTNode));
    node->type = type;
    node->data_type = TYPE_UNKNOWN;
    
    return node;
}

ASTNode *ast_create_program(void) {
    ASTNode *node = ast_create_node(AST_PROGRAM);
    node->data.program.declarations = NULL;
    node->data.program.declaration_count = 0;
    return node;
}

ASTNode *ast_create_function_decl(DataType return_type, const char *name) {
    ASTNode *node = ast_create_node(AST_FUNCTION_DECL);
    node->data.function_decl.return_type = return_type;
    node->data.function_decl.name = strdup(name);
    node->data.function_decl.parameters = NULL;
    node->data.function_decl.parameter_count = 0;
    node->data.function_decl.body = NULL;
    return node;
}

ASTNode *ast_create_var_decl(DataType var_type, const char *name, ASTNode *initializer) {
    ASTNode *node = ast_create_node(AST_VAR_DECL);
    node->data.var_decl.var_type = var_type;
    node->data.var_decl.name = strdup(name);
    node->data.var_decl.initializer = initializer;
    return node;
}

ASTNode *ast_create_parameter(DataType param_type, const char *name) {
    ASTNode *node = ast_create_node(AST_PARAMETER);
    node->data.parameter.param_type = param_type;
    node->data.parameter.name = strdup(name);
    return node;
}

ASTNode *ast_create_compound_stmt(void) {
    ASTNode *node = ast_create_node(AST_COMPOUND_STMT);
    node->data.compound_stmt.statements = NULL;
    node->data.compound_stmt.statement_count = 0;
    return node;
}

ASTNode *ast_create_expression_stmt(ASTNode *expression) {
    ASTNode *node = ast_create_node(AST_EXPRESSION_STMT);
    node->data.expression_stmt.expression = expression;
    return node;
}

ASTNode *ast_create_return_stmt(ASTNode *expression) {
    ASTNode *node = ast_create_node(AST_RETURN_STMT);
    node->data.return_stmt.expression = expression;
    return node;
}

ASTNode *ast_create_if_stmt(ASTNode *condition, ASTNode *then_stmt, ASTNode *else_stmt) {
    ASTNode *node = ast_create_node(AST_IF_STMT);
    node->data.if_stmt.condition = condition;
    node->data.if_stmt.then_stmt = then_stmt;
    node->data.if_stmt.else_stmt = else_stmt;
    return node;
}

ASTNode *ast_create_while_stmt(ASTNode *condition, ASTNode *body) {
    ASTNode *node = ast_create_node(AST_WHILE_STMT);
    node->data.while_stmt.condition = condition;
    node->data.while_stmt.body = body;
    return node;
}

ASTNode *ast_create_for_stmt(ASTNode *init, ASTNode *condition, ASTNode *update, ASTNode *body) {
    ASTNode *node = ast_create_node(AST_FOR_STMT);
    node->data.for_stmt.init = init;
    node->data.for_stmt.condition = condition;
    node->data.for_stmt.update = update;
    node->data.for_stmt.body = body;
    return node;
}

ASTNode *ast_create_break_stmt(void) {
    return ast_create_node(AST_BREAK_STMT);
}

ASTNode *ast_create_continue_stmt(void) {
    return ast_create_node(AST_CONTINUE_STMT);
}

ASTNode *ast_create_binary_expr(TokenType operator, ASTNode *left, ASTNode *right) {
    ASTNode *node = ast_create_node(AST_BINARY_EXPR);
    node->data.binary_expr.operator = operator;
    node->data.binary_expr.left = left;
    node->data.binary_expr.right = right;
    return node;
}

ASTNode *ast_create_unary_expr(TokenType operator, ASTNode *operand) {
    ASTNode *node = ast_create_node(AST_UNARY_EXPR);
    node->data.unary_expr.operator = operator;
    node->data.unary_expr.operand = operand;
    return node;
}

ASTNode *ast_create_call_expr(const char *function_name) {
    ASTNode *node = ast_create_node(AST_CALL_EXPR);
    node->data.call_expr.function_name = strdup(function_name);
    node->data.call_expr.arguments = NULL;
    node->data.call_expr.argument_count = 0;
    return node;
}

ASTNode *ast_create_identifier(const char *name) {
    ASTNode *node = ast_create_node(AST_IDENTIFIER);
    node->data.identifier.name = strdup(name);
    return node;
}

ASTNode *ast_create_number(int value) {
    ASTNode *node = ast_create_node(AST_NUMBER);
    node->data.number.value = value;
    node->data_type = TYPE_INT;
    return node;
}

ASTNode *ast_create_string(const char *value) {
    ASTNode *node = ast_create_node(AST_STRING);
    node->data.string.value = strdup(value);
    return node;
}

ASTNode *ast_create_assignment(const char *variable, ASTNode *value) {
    ASTNode *node = ast_create_node(AST_ASSIGNMENT);
    node->data.assignment.variable = strdup(variable);
    node->data.assignment.value = value;
    return node;
}

void ast_add_declaration(ASTNode *program, ASTNode *declaration) {
    if (program->type != AST_PROGRAM) return;

    program->data.program.declaration_count++;
    program->data.program.declarations = realloc(
        program->data.program.declarations,
        sizeof(ASTNode*) * program->data.program.declaration_count
    );

    if (!program->data.program.declarations) {
        error_fatal("Memory allocation failed for program declarations");
        return;
    }

    program->data.program.declarations[program->data.program.declaration_count - 1] = declaration;
}

void ast_add_parameter(ASTNode *function, ASTNode *parameter) {
    if (function->type != AST_FUNCTION_DECL) return;

    function->data.function_decl.parameter_count++;
    function->data.function_decl.parameters = realloc(
        function->data.function_decl.parameters,
        sizeof(ASTNode*) * function->data.function_decl.parameter_count
    );

    if (!function->data.function_decl.parameters) {
        error_fatal("Memory allocation failed for function parameters");
        return;
    }

    function->data.function_decl.parameters[function->data.function_decl.parameter_count - 1] = parameter;
}

void ast_add_statement(ASTNode *compound_stmt, ASTNode *statement) {
    if (compound_stmt->type != AST_COMPOUND_STMT) return;

    compound_stmt->data.compound_stmt.statement_count++;
    compound_stmt->data.compound_stmt.statements = realloc(
        compound_stmt->data.compound_stmt.statements,
        sizeof(ASTNode*) * compound_stmt->data.compound_stmt.statement_count
    );

    if (!compound_stmt->data.compound_stmt.statements) {
        error_fatal("Memory allocation failed for compound statement");
        return;
    }

    compound_stmt->data.compound_stmt.statements[compound_stmt->data.compound_stmt.statement_count - 1] = statement;
}

void ast_add_argument(ASTNode *call_expr, ASTNode *argument) {
    if (call_expr->type != AST_CALL_EXPR) return;

    call_expr->data.call_expr.argument_count++;
    call_expr->data.call_expr.arguments = realloc(
        call_expr->data.call_expr.arguments,
        sizeof(ASTNode*) * call_expr->data.call_expr.argument_count
    );

    if (!call_expr->data.call_expr.arguments) {
        error_fatal("Memory allocation failed for call expression arguments");
        return;
    }

    call_expr->data.call_expr.arguments[call_expr->data.call_expr.argument_count - 1] = argument;
}

void ast_destroy(ASTNode *node) {
    if (!node) return;

    switch (node->type) {
        case AST_PROGRAM:
            for (int i = 0; i < node->data.program.declaration_count; i++) {
                ast_destroy(node->data.program.declarations[i]);
            }
            free(node->data.program.declarations);
            break;

        case AST_FUNCTION_DECL:
            free(node->data.function_decl.name);
            for (int i = 0; i < node->data.function_decl.parameter_count; i++) {
                ast_destroy(node->data.function_decl.parameters[i]);
            }
            free(node->data.function_decl.parameters);
            ast_destroy(node->data.function_decl.body);
            break;

        case AST_VAR_DECL:
            free(node->data.var_decl.name);
            ast_destroy(node->data.var_decl.initializer);
            break;

        case AST_PARAMETER:
            free(node->data.parameter.name);
            break;

        case AST_COMPOUND_STMT:
            for (int i = 0; i < node->data.compound_stmt.statement_count; i++) {
                ast_destroy(node->data.compound_stmt.statements[i]);
            }
            free(node->data.compound_stmt.statements);
            break;

        case AST_EXPRESSION_STMT:
            ast_destroy(node->data.expression_stmt.expression);
            break;

        case AST_RETURN_STMT:
            ast_destroy(node->data.return_stmt.expression);
            break;

        case AST_IF_STMT:
            ast_destroy(node->data.if_stmt.condition);
            ast_destroy(node->data.if_stmt.then_stmt);
            ast_destroy(node->data.if_stmt.else_stmt);
            break;

        case AST_WHILE_STMT:
            ast_destroy(node->data.while_stmt.condition);
            ast_destroy(node->data.while_stmt.body);
            break;

        case AST_FOR_STMT:
            ast_destroy(node->data.for_stmt.init);
            ast_destroy(node->data.for_stmt.condition);
            ast_destroy(node->data.for_stmt.update);
            ast_destroy(node->data.for_stmt.body);
            break;

        case AST_BINARY_EXPR:
            ast_destroy(node->data.binary_expr.left);
            ast_destroy(node->data.binary_expr.right);
            break;

        case AST_UNARY_EXPR:
            ast_destroy(node->data.unary_expr.operand);
            break;

        case AST_CALL_EXPR:
            free(node->data.call_expr.function_name);
            for (int i = 0; i < node->data.call_expr.argument_count; i++) {
                ast_destroy(node->data.call_expr.arguments[i]);
            }
            free(node->data.call_expr.arguments);
            break;

        case AST_IDENTIFIER:
            free(node->data.identifier.name);
            break;

        case AST_STRING:
            free(node->data.string.value);
            break;

        case AST_ASSIGNMENT:
            free(node->data.assignment.variable);
            ast_destroy(node->data.assignment.value);
            break;

        default:
            break;
    }

    free(node);
}

void ast_print(ASTNode *node, int indent) {
    if (!node) return;

    for (int i = 0; i < indent; i++) {
        printf("  ");
    }

    printf("%s", ast_node_type_to_string(node->type));

    switch (node->type) {
        case AST_FUNCTION_DECL:
            printf(" '%s' -> %s", node->data.function_decl.name,
                   data_type_to_string(node->data.function_decl.return_type));
            break;
        case AST_VAR_DECL:
            printf(" '%s' : %s", node->data.var_decl.name,
                   data_type_to_string(node->data.var_decl.var_type));
            break;
        case AST_PARAMETER:
            printf(" '%s' : %s", node->data.parameter.name,
                   data_type_to_string(node->data.parameter.param_type));
            break;
        case AST_IDENTIFIER:
            printf(" '%s'", node->data.identifier.name);
            break;
        case AST_NUMBER:
            printf(" %d", node->data.number.value);
            break;
        case AST_STRING:
            printf(" \"%s\"", node->data.string.value);
            break;
        case AST_BINARY_EXPR:
            printf(" %s", token_type_to_string(node->data.binary_expr.operator));
            break;
        case AST_UNARY_EXPR:
            printf(" %s", token_type_to_string(node->data.unary_expr.operator));
            break;
        case AST_CALL_EXPR:
            printf(" '%s'", node->data.call_expr.function_name);
            break;
        case AST_ASSIGNMENT:
            printf(" '%s'", node->data.assignment.variable);
            break;
        default:
            break;
    }

    printf("\n");

    // Print children
    switch (node->type) {
        case AST_PROGRAM:
            for (int i = 0; i < node->data.program.declaration_count; i++) {
                ast_print(node->data.program.declarations[i], indent + 1);
            }
            break;

        case AST_FUNCTION_DECL:
            for (int i = 0; i < node->data.function_decl.parameter_count; i++) {
                ast_print(node->data.function_decl.parameters[i], indent + 1);
            }
            if (node->data.function_decl.body) {
                ast_print(node->data.function_decl.body, indent + 1);
            }
            break;

        case AST_VAR_DECL:
            if (node->data.var_decl.initializer) {
                ast_print(node->data.var_decl.initializer, indent + 1);
            }
            break;

        case AST_COMPOUND_STMT:
            for (int i = 0; i < node->data.compound_stmt.statement_count; i++) {
                ast_print(node->data.compound_stmt.statements[i], indent + 1);
            }
            break;

        case AST_EXPRESSION_STMT:
            if (node->data.expression_stmt.expression) {
                ast_print(node->data.expression_stmt.expression, indent + 1);
            }
            break;

        case AST_RETURN_STMT:
            if (node->data.return_stmt.expression) {
                ast_print(node->data.return_stmt.expression, indent + 1);
            }
            break;

        case AST_IF_STMT:
            if (node->data.if_stmt.condition) {
                ast_print(node->data.if_stmt.condition, indent + 1);
            }
            if (node->data.if_stmt.then_stmt) {
                ast_print(node->data.if_stmt.then_stmt, indent + 1);
            }
            if (node->data.if_stmt.else_stmt) {
                ast_print(node->data.if_stmt.else_stmt, indent + 1);
            }
            break;

        case AST_WHILE_STMT:
            if (node->data.while_stmt.condition) {
                ast_print(node->data.while_stmt.condition, indent + 1);
            }
            if (node->data.while_stmt.body) {
                ast_print(node->data.while_stmt.body, indent + 1);
            }
            break;

        case AST_FOR_STMT:
            if (node->data.for_stmt.init) {
                ast_print(node->data.for_stmt.init, indent + 1);
            }
            if (node->data.for_stmt.condition) {
                ast_print(node->data.for_stmt.condition, indent + 1);
            }
            if (node->data.for_stmt.update) {
                ast_print(node->data.for_stmt.update, indent + 1);
            }
            if (node->data.for_stmt.body) {
                ast_print(node->data.for_stmt.body, indent + 1);
            }
            break;

        case AST_BINARY_EXPR:
            if (node->data.binary_expr.left) {
                ast_print(node->data.binary_expr.left, indent + 1);
            }
            if (node->data.binary_expr.right) {
                ast_print(node->data.binary_expr.right, indent + 1);
            }
            break;

        case AST_UNARY_EXPR:
            if (node->data.unary_expr.operand) {
                ast_print(node->data.unary_expr.operand, indent + 1);
            }
            break;

        case AST_CALL_EXPR:
            for (int i = 0; i < node->data.call_expr.argument_count; i++) {
                ast_print(node->data.call_expr.arguments[i], indent + 1);
            }
            break;

        case AST_ASSIGNMENT:
            if (node->data.assignment.value) {
                ast_print(node->data.assignment.value, indent + 1);
            }
            break;

        default:
            break;
    }
}

const char *ast_node_type_to_string(ASTNodeType type) {
    switch (type) {
        case AST_PROGRAM: return "PROGRAM";
        case AST_FUNCTION_DECL: return "FUNCTION_DECL";
        case AST_VAR_DECL: return "VAR_DECL";
        case AST_PARAMETER: return "PARAMETER";
        case AST_COMPOUND_STMT: return "COMPOUND_STMT";
        case AST_EXPRESSION_STMT: return "EXPRESSION_STMT";
        case AST_RETURN_STMT: return "RETURN_STMT";
        case AST_IF_STMT: return "IF_STMT";
        case AST_WHILE_STMT: return "WHILE_STMT";
        case AST_FOR_STMT: return "FOR_STMT";
        case AST_BREAK_STMT: return "BREAK_STMT";
        case AST_CONTINUE_STMT: return "CONTINUE_STMT";
        case AST_BINARY_EXPR: return "BINARY_EXPR";
        case AST_UNARY_EXPR: return "UNARY_EXPR";
        case AST_CALL_EXPR: return "CALL_EXPR";
        case AST_IDENTIFIER: return "IDENTIFIER";
        case AST_NUMBER: return "NUMBER";
        case AST_STRING: return "STRING";
        case AST_ASSIGNMENT: return "ASSIGNMENT";
        default: return "UNKNOWN";
    }
}

const char *data_type_to_string(DataType type) {
    switch (type) {
        case TYPE_VOID: return "void";
        case TYPE_INT: return "int";
        case TYPE_CHAR: return "char";
        default: return "unknown";
    }
}

DataType token_to_data_type(TokenType token) {
    switch (token) {
        case TOKEN_VOID: return TYPE_VOID;
        case TOKEN_INT: return TYPE_INT;
        case TOKEN_CHAR_KW: return TYPE_CHAR;
        default: return TYPE_UNKNOWN;
    }
}