#include "kcc.h"
#include "preprocessor.h"
#include <stdarg.h>
#include <time.h>

Preprocessor *preprocessor_create(void) {
    Preprocessor *pp = malloc(sizeof(Preprocessor));
    if (!pp) {
        error_fatal("Memory allocation failed for preprocessor");
        return NULL;
    }

    memset(pp, 0, sizeof(Preprocessor));

    pp->output_capacity = 4096;
    pp->output = malloc(pp->output_capacity);
    if (!pp->output) {
        free(pp);
        error_fatal("Memory allocation failed for preprocessor output");
        return NULL;
    }

    pp->output[0] = '\0';
    pp->output_size = 0;
    pp->current_line = 1;
    pp->skip_lines = false;

    // Add predefined macros
    preprocessor_add_predefined_macros(pp);

    return pp;
}

void preprocessor_destroy(Preprocessor *pp) {
    if (!pp) return;

    // Free include stack
    for (int i = 0; i < pp->include_depth; i++) {
        free(pp->include_stack[i].filename);
        free(pp->include_stack[i].content);
    }

    free(pp->output);
    free(pp->current_file);
    free(pp);
}

void preprocessor_add_predefined_macros(Preprocessor *pp) {
    // Standard predefined macros
    preprocessor_define_macro(pp, "__KCC__", "1");
    preprocessor_define_macro(pp, "__VERSION__", "\"1.0.0\"");

    // Date and time
    time_t now = time(NULL);
    struct tm *tm_info = localtime(&now);

    char date_str[32];
    char time_str[32];
    strftime(date_str, sizeof(date_str), "\"%b %d %Y\"", tm_info);
    strftime(time_str, sizeof(time_str), "\"%H:%M:%S\"", tm_info);

    preprocessor_define_macro(pp, "__DATE__", date_str);
    preprocessor_define_macro(pp, "__TIME__", time_str);

    // Architecture and system
    preprocessor_define_macro(pp, "__x86_64__", "1");
    preprocessor_define_macro(pp, "__unix__", "1");

    // C standard
    preprocessor_define_macro(pp, "__STDC__", "1");
    preprocessor_define_macro(pp, "__STDC_VERSION__", "201112L");

    // Mark predefined macros
    for (int i = 0; i < pp->macro_count; i++) {
        pp->macros[i].is_predefined = true;
    }
}

char *preprocessor_process_file(Preprocessor *pp, const char *filename) {
    char *content = preprocessor_read_file(filename);
    if (!content) {
        preprocessor_error(pp, "Could not read file: %s", filename);
        return NULL;
    }

    char *result = preprocessor_process_string(pp, content, filename);
    free(content);
    return result;
}

char *preprocessor_process_string(Preprocessor *pp, const char *source, const char *filename) {
    pp->current_file = strdup(filename);
    pp->current_line = 1;
    pp->output_size = 0;
    pp->output[0] = '\0';

    char line[MAX_LINE_LENGTH];
    const char *src_ptr = source;

    while (*src_ptr) {
        // Extract line
        int i = 0;
        while (*src_ptr && *src_ptr != '\n' && i < MAX_LINE_LENGTH - 1) {
            line[i++] = *src_ptr++;
        }
        line[i] = '\0';

        if (*src_ptr == '\n') {
            src_ptr++;
        }

        // Process line
        if (preprocessor_should_skip_line(pp)) {
            // Still need to process conditional directives when skipping
            if (preprocessor_is_directive(line)) {
                char *directive = preprocessor_get_directive_name(line);
                if (strcmp(directive, "endif") == 0 ||
                    strcmp(directive, "else") == 0 ||
                    strcmp(directive, "elif") == 0) {
                    preprocessor_process_directive(pp, line);
                }
                free(directive);
            }
        } else {
            if (preprocessor_is_directive(line)) {
                if (!preprocessor_process_directive(pp, line)) {
                    preprocessor_error(pp, "Error processing directive: %s", line);
                }
            } else {
                // Expand macros and add to output
                char *expanded = preprocessor_expand_macros(pp, line);
                preprocessor_append_output(pp, expanded);
                preprocessor_append_output(pp, "\n");
                free(expanded);
            }
        }

        pp->current_line++;
    }

    // Check for unmatched conditionals
    if (pp->cond_stack_depth > 0) {
        preprocessor_error(pp, "Unmatched conditional directive");
    }

    return strdup(pp->output);
}

bool preprocessor_define_macro(Preprocessor *pp, const char *name, const char *body) {
    if (pp->macro_count >= MAX_MACROS) {
        preprocessor_error(pp, "Too many macros defined");
        return false;
    }

    // Check if macro already exists
    Macro *existing = preprocessor_find_macro(pp, name);
    if (existing && !existing->is_predefined) {
        preprocessor_warning(pp, "Macro '%s' redefined", name);
        // Replace existing macro
        strncpy(existing->body, body, MAX_MACRO_BODY - 1);
        existing->body[MAX_MACRO_BODY - 1] = '\0';
        return true;
    }

    Macro *macro = &pp->macros[pp->macro_count++];
    strncpy(macro->name, name, MAX_MACRO_NAME - 1);
    macro->name[MAX_MACRO_NAME - 1] = '\0';

    strncpy(macro->body, body, MAX_MACRO_BODY - 1);
    macro->body[MAX_MACRO_BODY - 1] = '\0';

    macro->type = MACRO_OBJECT;
    macro->param_count = 0;
    macro->is_predefined = false;
    macro->line_defined = pp->current_line;
    macro->file_defined = strdup(pp->current_file);

    return true;
}

bool preprocessor_define_function_macro(Preprocessor *pp, const char *name,
                                      const char *params[], int param_count, const char *body) {
    if (pp->macro_count >= MAX_MACROS) {
        preprocessor_error(pp, "Too many macros defined");
        return false;
    }

    if (param_count > MAX_MACRO_PARAMS) {
        preprocessor_error(pp, "Too many macro parameters");
        return false;
    }

    Macro *macro = &pp->macros[pp->macro_count++];
    strncpy(macro->name, name, MAX_MACRO_NAME - 1);
    macro->name[MAX_MACRO_NAME - 1] = '\0';

    strncpy(macro->body, body, MAX_MACRO_BODY - 1);
    macro->body[MAX_MACRO_BODY - 1] = '\0';

    macro->type = MACRO_FUNCTION;
    macro->param_count = param_count;
    macro->is_predefined = false;
    macro->line_defined = pp->current_line;
    macro->file_defined = strdup(pp->current_file);

    for (int i = 0; i < param_count; i++) {
        strncpy(macro->params[i].name, params[i], MAX_MACRO_NAME - 1);
        macro->params[i].name[MAX_MACRO_NAME - 1] = '\0';
    }

    return true;
}

bool preprocessor_undefine_macro(Preprocessor *pp, const char *name) {
    for (int i = 0; i < pp->macro_count; i++) {
        if (strcmp(pp->macros[i].name, name) == 0) {
            // Don't allow undefining predefined macros
            if (pp->macros[i].is_predefined) {
                preprocessor_warning(pp, "Cannot undefine predefined macro '%s'", name);
                return false;
            }

            // Shift remaining macros
            free(pp->macros[i].file_defined);
            for (int j = i; j < pp->macro_count - 1; j++) {
                pp->macros[j] = pp->macros[j + 1];
            }
            pp->macro_count--;
            return true;
        }
    }
    return false;
}

Macro *preprocessor_find_macro(Preprocessor *pp, const char *name) {
    for (int i = 0; i < pp->macro_count; i++) {
        if (strcmp(pp->macros[i].name, name) == 0) {
            return &pp->macros[i];
        }
    }
    return NULL;
}

bool preprocessor_is_macro_defined(Preprocessor *pp, const char *name) {
    return preprocessor_find_macro(pp, name) != NULL;
}

char *preprocessor_expand_macros(Preprocessor *pp, const char *line) {
    char *result = strdup(line);
    char *temp;
    bool expanded;

    // Keep expanding until no more macros are found
    do {
        expanded = false;
        temp = malloc(strlen(result) * 2 + 256); // Extra space for expansion
        temp[0] = '\0';

        const char *ptr = result;
        while (*ptr) {
            if (isalpha(*ptr) || *ptr == '_') {
                // Found potential identifier
                char identifier[MAX_MACRO_NAME];
                int i = 0;

                while ((isalnum(*ptr) || *ptr == '_') && i < MAX_MACRO_NAME - 1) {
                    identifier[i++] = *ptr++;
                }
                identifier[i] = '\0';

                Macro *macro = preprocessor_find_macro(pp, identifier);
                if (macro) {
                    expanded = true;

                    if (macro->type == MACRO_OBJECT) {
                        strcat(temp, macro->body);
                    } else if (macro->type == MACRO_FUNCTION) {
                        // Check for function call syntax
                        if (*ptr == '(') {
                            ptr++; // skip '('
                            char *args[MAX_MACRO_PARAMS];
                            int arg_count = 0;
                            char arg_buffer[1024];
                            int buffer_pos = 0;
                            int paren_depth = 0;

                            // Parse arguments
                            while (*ptr && (paren_depth > 0 || *ptr != ')')) {
                                if (*ptr == '(') {
                                    paren_depth++;
                                } else if (*ptr == ')') {
                                    paren_depth--;
                                }

                                if (paren_depth == 0 && *ptr == ',' && arg_count < MAX_MACRO_PARAMS) {
                                    arg_buffer[buffer_pos] = '\0';
                                    args[arg_count++] = strdup(preprocessor_trim_whitespace(arg_buffer));
                                    buffer_pos = 0;
                                } else if (buffer_pos < sizeof(arg_buffer) - 1) {
                                    arg_buffer[buffer_pos++] = *ptr;
                                }
                                ptr++;
                            }

                            if (*ptr == ')') {
                                ptr++; // skip ')'
                                if (buffer_pos > 0 && arg_count < MAX_MACRO_PARAMS) {
                                    arg_buffer[buffer_pos] = '\0';
                                    args[arg_count++] = strdup(preprocessor_trim_whitespace(arg_buffer));
                                }

                                char *expanded_macro = preprocessor_expand_function_macro(pp, macro, (const char**)args, arg_count);
                                strcat(temp, expanded_macro);
                                free(expanded_macro);

                                // Free argument strings
                                for (int j = 0; j < arg_count; j++) {
                                    free(args[j]);
                                }
                            } else {
                                // Malformed function call, treat as identifier
                                strcat(temp, identifier);
                            }
                        } else {
                            // Function macro without parentheses, treat as identifier
                            strcat(temp, identifier);
                        }
                    }
                } else {
                    strcat(temp, identifier);
                }
            } else {
                // Not an identifier, copy character
                size_t len = strlen(temp);
                temp[len] = *ptr++;
                temp[len + 1] = '\0';
            }
        }

        free(result);
        result = temp;
    } while (expanded);

    return result;
}

char *preprocessor_expand_function_macro(Preprocessor *pp, Macro *macro,
                                       const char *args[], int arg_count) {
    if (arg_count != macro->param_count) {
        preprocessor_error(pp, "Macro '%s' expects %d arguments, got %d",
                         macro->name, macro->param_count, arg_count);
        return strdup(macro->body);
    }

    char *param_names[MAX_MACRO_PARAMS];
    for (int i = 0; i < macro->param_count; i++) {
        param_names[i] = macro->params[i].name;
    }

    return preprocessor_substitute_params(macro->body, (const char**)param_names, args, macro->param_count);
}

char *preprocessor_substitute_params(const char *body, const char *params[],
                                   const char *args[], int param_count) {
    char *result = malloc(strlen(body) * 4 + 256); // Extra space for expansion
    result[0] = '\0';

    const char *ptr = body;
    while (*ptr) {
        if (*ptr == '#') {
            ptr++;
            if (*ptr == '#') {
                // Token pasting operator
                ptr++;
                // Remove trailing whitespace from result
                int len = strlen(result);
                while (len > 0 && isspace(result[len - 1])) {
                    result[--len] = '\0';
                }
                // Skip leading whitespace in remaining body
                while (*ptr && isspace(*ptr)) {
                    ptr++;
                }
            } else {
                // Stringizing operator
                char param_name[MAX_MACRO_NAME];
                int i = 0;
                while ((isalnum(*ptr) || *ptr == '_') && i < MAX_MACRO_NAME - 1) {
                    param_name[i++] = *ptr++;
                }
                param_name[i] = '\0';

                // Find parameter
                for (int j = 0; j < param_count; j++) {
                    if (strcmp(param_name, params[j]) == 0) {
                        char *stringized = preprocessor_stringify(args[j]);
                        strcat(result, stringized);
                        free(stringized);
                        break;
                    }
                }
            }
        } else if (isalpha(*ptr) || *ptr == '_') {
            // Check if this is a parameter name
            char identifier[MAX_MACRO_NAME];
            int i = 0;
            const char *start = ptr;

            while ((isalnum(*ptr) || *ptr == '_') && i < MAX_MACRO_NAME - 1) {
                identifier[i++] = *ptr++;
            }
            identifier[i] = '\0';

            bool found_param = false;
            for (int j = 0; j < param_count; j++) {
                if (strcmp(identifier, params[j]) == 0) {
                    strcat(result, args[j]);
                    found_param = true;
                    break;
                }
            }

            if (!found_param) {
                // Not a parameter, copy the identifier
                strncat(result, start, ptr - start);
            }
        } else {
            // Copy character as-is
            size_t len = strlen(result);
            result[len] = *ptr++;
            result[len + 1] = '\0';
        }
    }

    return result;
}

bool preprocessor_process_directive(Preprocessor *pp, const char *line) {
    char *directive = preprocessor_get_directive_name(line);
    if (!directive) return false;

    bool result = false;

    if (strcmp(directive, "define") == 0) {
        result = preprocessor_handle_define(pp, line);
    } else if (strcmp(directive, "undef") == 0) {
        result = preprocessor_handle_undef(pp, line);
    } else if (strcmp(directive, "include") == 0) {
        result = preprocessor_handle_include(pp, line);
    } else if (strcmp(directive, "if") == 0) {
        result = preprocessor_handle_if(pp, line);
    } else if (strcmp(directive, "ifdef") == 0) {
        result = preprocessor_handle_ifdef(pp, line);
    } else if (strcmp(directive, "ifndef") == 0) {
        result = preprocessor_handle_ifndef(pp, line);
    } else if (strcmp(directive, "elif") == 0) {
        result = preprocessor_handle_elif(pp, line);
    } else if (strcmp(directive, "else") == 0) {
        result = preprocessor_handle_else(pp, line);
    } else if (strcmp(directive, "endif") == 0) {
        result = preprocessor_handle_endif(pp, line);
    } else if (strcmp(directive, "error") == 0) {
        result = preprocessor_handle_error(pp, line);
    } else if (strcmp(directive, "warning") == 0) {
        result = preprocessor_handle_warning(pp, line);
    } else if (strcmp(directive, "pragma") == 0) {
        result = preprocessor_handle_pragma(pp, line);
    } else if (strcmp(directive, "line") == 0) {
        result = preprocessor_handle_line(pp, line);
    } else {
        preprocessor_error(pp, "Unknown directive: #%s", directive);
    }

    free(directive);
    return result;
}

bool preprocessor_handle_define(Preprocessor *pp, const char *directive) {
    char *args = preprocessor_get_directive_args(directive);
    if (!args) {
        preprocessor_error(pp, "Invalid #define directive");
        return false;
    }

    char *name = strtok(args, " \t");
    if (!name) {
        free(args);
        preprocessor_error(pp, "Missing macro name in #define");
        return false;
    }

    // Check for function-like macro
    char *paren = strchr(name, '(');
    if (paren) {
        *paren = '\0';
        char *param_str = paren + 1;
        char *close_paren = strchr(param_str, ')');
        if (!close_paren) {
            free(args);
            preprocessor_error(pp, "Missing ')' in macro definition");
            return false;
        }
        *close_paren = '\0';

        // Parse parameters
        const char *params[MAX_MACRO_PARAMS];
        int param_count = 0;

        if (strlen(param_str) > 0) {
            char *param = strtok(param_str, ",");
            while (param && param_count < MAX_MACRO_PARAMS) {
                params[param_count++] = preprocessor_trim_whitespace(param);
                param = strtok(NULL, ",");
            }
        }

        // Get body (everything after the parameter list)
        char *body = close_paren + 1;
        while (*body && isspace(*body)) body++; // Skip whitespace

        bool result = preprocessor_define_function_macro(pp, name, params, param_count, body);
        free(args);
        return result;
    } else {
        // Object-like macro
        char *body = strtok(NULL, ""); // Get rest of line
        if (!body) body = "";

        // Skip leading whitespace in body
        while (*body && isspace(*body)) body++;

        bool result = preprocessor_define_macro(pp, name, body);
        free(args);
        return result;
    }
}

bool preprocessor_handle_undef(Preprocessor *pp, const char *directive) {
    char *args = preprocessor_get_directive_args(directive);
    if (!args) {
        preprocessor_error(pp, "Invalid #undef directive");
        return false;
    }

    char *name = strtok(args, " \t\n");
    if (!name) {
        free(args);
        preprocessor_error(pp, "Missing macro name in #undef");
        return false;
    }

    bool result = preprocessor_undefine_macro(pp, name);
    free(args);
    return result;
}

// Utility functions implementation continues...

bool preprocessor_is_directive(const char *line) {
    const char *ptr = line;
    while (*ptr && isspace(*ptr)) ptr++;
    return *ptr == '#';
}

char *preprocessor_get_directive_name(const char *line) {
    const char *ptr = line;
    while (*ptr && isspace(*ptr)) ptr++;
    if (*ptr != '#') return NULL;

    ptr++; // skip '#'
    while (*ptr && isspace(*ptr)) ptr++;

    const char *start = ptr;
    while (*ptr && (isalnum(*ptr) || *ptr == '_')) ptr++;

    if (ptr == start) return NULL;

    char *name = malloc(ptr - start + 1);
    strncpy(name, start, ptr - start);
    name[ptr - start] = '\0';

    return name;
}

char *preprocessor_get_directive_args(const char *line) {
    const char *ptr = line;
    while (*ptr && isspace(*ptr)) ptr++;
    if (*ptr != '#') return NULL;

    ptr++; // skip '#'
    while (*ptr && isspace(*ptr)) ptr++;

    // Skip directive name
    while (*ptr && (isalnum(*ptr) || *ptr == '_')) ptr++;
    while (*ptr && isspace(*ptr)) ptr++;

    return strdup(ptr);
}

char *preprocessor_trim_whitespace(char *str) {
    // Trim leading whitespace
    while (*str && isspace(*str)) str++;

    // Trim trailing whitespace
    char *end = str + strlen(str) - 1;
    while (end > str && isspace(*end)) {
        *end = '\0';
        end--;
    }

    return str;
}

char *preprocessor_stringify(const char *text) {
    size_t len = strlen(text);
    char *result = malloc(len * 2 + 3); // Worst case: every char needs escaping + quotes
    char *ptr = result;

    *ptr++ = '"';

    for (size_t i = 0; i < len; i++) {
        if (text[i] == '"' || text[i] == '\\') {
            *ptr++ = '\\';
        }
        *ptr++ = text[i];
    }

    *ptr++ = '"';
    *ptr = '\0';

    return result;
}

void preprocessor_append_output(Preprocessor *pp, const char *text) {
    size_t text_len = strlen(text);
    size_t needed = pp->output_size + text_len + 1;

    if (needed > pp->output_capacity) {
        pp->output_capacity = needed * 2;
        pp->output = realloc(pp->output, pp->output_capacity);
        if (!pp->output) {
            error_fatal("Memory allocation failed for preprocessor output");
            return;
        }
    }

    strcpy(pp->output + pp->output_size, text);
    pp->output_size += text_len;
}

char *preprocessor_read_file(const char *filename) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *content = malloc(file_size + 1);
    if (!content) {
        fclose(file);
        return NULL;
    }

    size_t bytes_read = fread(content, 1, file_size, file);
    content[bytes_read] = '\0';

    fclose(file);
    return content;
}

bool preprocessor_handle_include(Preprocessor *pp, const char *directive) {
    char *args = preprocessor_get_directive_args(directive);
    if (!args) {
        preprocessor_error(pp, "Invalid #include directive");
        return false;
    }

    // Parse include filename
    char *filename = NULL;
    bool is_system_header = false;

    char *trimmed = preprocessor_trim_whitespace(args);
    if (trimmed[0] == '"') {
        // User header: "filename.h"
        char *end = strchr(trimmed + 1, '"');
        if (end) {
            *end = '\0';
            filename = strdup(trimmed + 1);
        }
    } else if (trimmed[0] == '<') {
        // System header: <filename.h>
        char *end = strchr(trimmed + 1, '>');
        if (end) {
            *end = '\0';
            filename = strdup(trimmed + 1);
            is_system_header = true;
        }
    }

    free(args);

    if (!filename) {
        preprocessor_error(pp, "Invalid include filename");
        return false;
    }

    // Check include depth
    if (pp->include_depth >= MAX_INCLUDE_DEPTH) {
        preprocessor_error(pp, "Include depth exceeded");
        free(filename);
        return false;
    }

    // Read and process included file
    char *content = preprocessor_read_file(filename);
    if (!content) {
        if (!is_system_header) {
            preprocessor_error(pp, "Could not open include file: %s", filename);
        }
        free(filename);
        return !is_system_header; // System headers are optional
    }

    // Save current state
    IncludeFile *current = &pp->include_stack[pp->include_depth++];
    current->filename = pp->current_file;
    current->content = NULL; // We don't need to save content
    current->line = pp->current_line;
    current->pos = 0;

    // Process included file
    char *processed = preprocessor_process_string(pp, content, filename);
    if (processed) {
        preprocessor_append_output(pp, processed);
        free(processed);
    }

    // Restore state
    pp->include_depth--;
    pp->current_file = current->filename;
    pp->current_line = current->line;

    free(content);
    free(filename);
    return true;
}

bool preprocessor_handle_ifdef(Preprocessor *pp, const char *directive) {
    char *args = preprocessor_get_directive_args(directive);
    if (!args) {
        preprocessor_error(pp, "Invalid #ifdef directive");
        return false;
    }

    char *name = strtok(args, " \t\n");
    if (!name) {
        free(args);
        preprocessor_error(pp, "Missing macro name in #ifdef");
        return false;
    }

    bool is_defined = preprocessor_is_macro_defined(pp, name);
    preprocessor_push_conditional(pp, COND_IFDEF, is_defined);

    free(args);
    return true;
}

bool preprocessor_handle_ifndef(Preprocessor *pp, const char *directive) {
    char *args = preprocessor_get_directive_args(directive);
    if (!args) {
        preprocessor_error(pp, "Invalid #ifndef directive");
        return false;
    }

    char *name = strtok(args, " \t\n");
    if (!name) {
        free(args);
        preprocessor_error(pp, "Missing macro name in #ifndef");
        return false;
    }

    bool is_defined = preprocessor_is_macro_defined(pp, name);
    preprocessor_push_conditional(pp, COND_IFNDEF, !is_defined);

    free(args);
    return true;
}

bool preprocessor_handle_if(Preprocessor *pp, const char *directive) {
    char *args = preprocessor_get_directive_args(directive);
    if (!args) {
        preprocessor_error(pp, "Invalid #if directive");
        return false;
    }

    bool condition = preprocessor_evaluate_condition(pp, args);
    preprocessor_push_conditional(pp, COND_IF, condition);

    free(args);
    return true;
}

bool preprocessor_handle_elif(Preprocessor *pp, const char *directive) {
    if (pp->cond_stack_depth == 0) {
        preprocessor_error(pp, "#elif without matching #if");
        return false;
    }

    ConditionalState *current = &pp->cond_stack[pp->cond_stack_depth - 1];
    if (current->type == COND_ELSE) {
        preprocessor_error(pp, "#elif after #else");
        return false;
    }

    char *args = preprocessor_get_directive_args(directive);
    if (!args) {
        preprocessor_error(pp, "Invalid #elif directive");
        return false;
    }

    bool condition = false;
    if (!current->condition_met) {
        condition = preprocessor_evaluate_condition(pp, args);
        if (condition) {
            current->condition_met = true;
        }
    }

    current->type = COND_ELIF;
    pp->skip_lines = !condition;

    free(args);
    return true;
}

bool preprocessor_handle_else(Preprocessor *pp, const char *directive) {
    if (pp->cond_stack_depth == 0) {
        preprocessor_error(pp, "#else without matching #if");
        return false;
    }

    ConditionalState *current = &pp->cond_stack[pp->cond_stack_depth - 1];
    if (current->type == COND_ELSE) {
        preprocessor_error(pp, "Multiple #else directives");
        return false;
    }

    bool should_process = !current->condition_met && !current->else_taken;
    current->type = COND_ELSE;
    current->else_taken = true;

    pp->skip_lines = !should_process;

    return true;
}

bool preprocessor_handle_endif(Preprocessor *pp, const char *directive) {
    if (pp->cond_stack_depth == 0) {
        preprocessor_error(pp, "#endif without matching #if");
        return false;
    }

    preprocessor_pop_conditional(pp);
    return true;
}

bool preprocessor_handle_error(Preprocessor *pp, const char *directive) {
    char *args = preprocessor_get_directive_args(directive);
    char *message = args ? args : "User error";

    preprocessor_error(pp, "%s", message);

    if (args) free(args);
    return false; // Error directive always fails compilation
}

bool preprocessor_handle_warning(Preprocessor *pp, const char *directive) {
    char *args = preprocessor_get_directive_args(directive);
    char *message = args ? args : "User warning";

    preprocessor_warning(pp, "%s", message);

    if (args) free(args);
    return true;
}

bool preprocessor_handle_pragma(Preprocessor *pp, const char *directive) {
    char *args = preprocessor_get_directive_args(directive);

    // Simple pragma handling - just ignore for now
    // In a full implementation, you'd handle specific pragmas
    preprocessor_warning(pp, "Pragma ignored: %s", args ? args : "");

    if (args) free(args);
    return true;
}

bool preprocessor_handle_line(Preprocessor *pp, const char *directive) {
    char *args = preprocessor_get_directive_args(directive);
    if (!args) {
        preprocessor_error(pp, "Invalid #line directive");
        return false;
    }

    char *line_str = strtok(args, " \t");
    char *filename = strtok(NULL, " \t");

    if (line_str) {
        int line_num = atoi(line_str);
        if (line_num > 0) {
            pp->current_line = line_num - 1; // Will be incremented
        }
    }

    if (filename) {
        // Remove quotes if present
        if (filename[0] == '"') {
            filename++;
            char *end = strrchr(filename, '"');
            if (end) *end = '\0';
        }
        free(pp->current_file);
        pp->current_file = strdup(filename);
    }

    free(args);
    return true;
}

bool preprocessor_evaluate_condition(Preprocessor *pp, const char *condition) {
    // Simple condition evaluation - expand macros first
    char *expanded = preprocessor_expand_macros(pp, condition);

    // Very basic evaluation - just check for non-zero numbers
    // A full implementation would have a proper expression evaluator
    char *trimmed = preprocessor_trim_whitespace(expanded);

    bool result = false;
    if (strlen(trimmed) == 0) {
        result = false;
    } else if (strcmp(trimmed, "0") == 0) {
        result = false;
    } else if (strncmp(trimmed, "defined", 7) == 0) {
        // Handle defined() operator
        char *ptr = trimmed + 7;
        while (*ptr && isspace(*ptr)) ptr++;

        bool has_parens = (*ptr == '(');
        if (has_parens) ptr++;

        char macro_name[MAX_MACRO_NAME];
        int i = 0;
        while (*ptr && (isalnum(*ptr) || *ptr == '_') && i < MAX_MACRO_NAME - 1) {
            macro_name[i++] = *ptr++;
        }
        macro_name[i] = '\0';

        result = preprocessor_is_macro_defined(pp, macro_name);
    } else {
        // Non-empty, non-zero - assume true
        result = true;
    }

    free(expanded);
    return result;
}

bool preprocessor_should_skip_line(Preprocessor *pp) {
    if (pp->cond_stack_depth == 0) return false;

    // Check all conditional levels
    for (int i = 0; i < pp->cond_stack_depth; i++) {
        ConditionalState *cond = &pp->cond_stack[i];

        switch (cond->type) {
            case COND_IF:
            case COND_IFDEF:
            case COND_IFNDEF:
                if (!cond->condition_met) return true;
                break;
            case COND_ELIF:
                if (!cond->condition_met) return true;
                break;
            case COND_ELSE:
                if (cond->condition_met || !cond->else_taken) return true;
                break;
            default:
                break;
        }
    }

    return false;
}

void preprocessor_push_conditional(Preprocessor *pp, ConditionalType type, bool condition) {
    if (pp->cond_stack_depth >= 32) {
        preprocessor_error(pp, "Conditional nesting too deep");
        return;
    }

    ConditionalState *cond = &pp->cond_stack[pp->cond_stack_depth++];
    cond->type = type;
    cond->condition_met = condition;
    cond->else_taken = false;
    cond->line_number = pp->current_line;

    pp->skip_lines = !condition;
}

bool preprocessor_pop_conditional(Preprocessor *pp) {
    if (pp->cond_stack_depth == 0) {
        return false;
    }

    pp->cond_stack_depth--;
    pp->skip_lines = preprocessor_should_skip_line(pp);

    return true;
}

void preprocessor_error(Preprocessor *pp, const char *format, ...) {
    fprintf(stderr, "Preprocessor error in %s:%d: ",
            pp->current_file ? pp->current_file : "unknown", pp->current_line);

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);

    fprintf(stderr, "\n");
}

void preprocessor_warning(Preprocessor *pp, const char *format, ...) {
    fprintf(stderr, "Preprocessor warning in %s:%d: ",
            pp->current_file ? pp->current_file : "unknown", pp->current_line);

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);

    fprintf(stderr, "\n");
}