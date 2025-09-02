#include "kcc.h"

SymbolTable *symbol_table_create(void) {
    SymbolTable *table = malloc(sizeof(SymbolTable));
    if (!table) {
        error_fatal("Memory allocation failed for symbol table");
        return NULL;
    }
    
    for (int i = 0; i < SYMBOL_TABLE_SIZE; i++) {
        table->table[i] = NULL;
    }
    table->current_scope = 0;
    
    return table;
}

void symbol_table_destroy(SymbolTable *table) {
    if (!table) return;
    
    for (int i = 0; i < SYMBOL_TABLE_SIZE; i++) {
        Symbol *symbol = table->table[i];
        while (symbol) {
            Symbol *next = symbol->next;
            free(symbol->name);
            free(symbol);
            symbol = next;
        }
    }
    
    free(table);
}

void symbol_table_enter_scope(SymbolTable *table) {
    table->current_scope++;
}

void symbol_table_exit_scope(SymbolTable *table) {
    // Remove all symbols in the current scope
    for (int i = 0; i < SYMBOL_TABLE_SIZE; i++) {
        Symbol **current = &table->table[i];
        while (*current) {
            if ((*current)->scope_level == table->current_scope) {
                Symbol *to_remove = *current;
                *current = (*current)->next;
                free(to_remove->name);
                free(to_remove);
            } else {
                current = &(*current)->next;
            }
        }
    }
    
    if (table->current_scope > 0) {
        table->current_scope--;
    }
}

unsigned int symbol_table_hash(const char *name) {
    unsigned int hash = 0;
    for (int i = 0; name[i] != '\0'; i++) {
        hash = hash * 31 + name[i];
    }
    return hash % SYMBOL_TABLE_SIZE;
}

bool symbol_table_insert(SymbolTable *table, const char *name, SymbolType symbol_type, DataType data_type) {
    // Check if symbol already exists in current scope
    if (symbol_table_lookup_current_scope(table, name)) {
        return false; // Symbol already exists in current scope
    }
    
    unsigned int index = symbol_table_hash(name);
    
    Symbol *symbol = malloc(sizeof(Symbol));
    if (!symbol) {
        error_fatal("Memory allocation failed for symbol");
        return false;
    }
    
    symbol->name = strdup(name);
    symbol->symbol_type = symbol_type;
    symbol->data_type = data_type;
    symbol->scope_level = table->current_scope;
    symbol->next = table->table[index];
    
    table->table[index] = symbol;
    
    return true;
}

Symbol *symbol_table_lookup(SymbolTable *table, const char *name) {
    unsigned int index = symbol_table_hash(name);
    Symbol *symbol = table->table[index];
    
    while (symbol) {
        if (strcmp(symbol->name, name) == 0) {
            return symbol;
        }
        symbol = symbol->next;
    }
    
    return NULL;
}

Symbol *symbol_table_lookup_current_scope(SymbolTable *table, const char *name) {
    unsigned int index = symbol_table_hash(name);
    Symbol *symbol = table->table[index];
    
    while (symbol) {
        if (strcmp(symbol->name, name) == 0 && symbol->scope_level == table->current_scope) {
            return symbol;
        }
        symbol = symbol->next;
    }
    
    return NULL;
}

void symbol_table_print(SymbolTable *table) {
    printf("=== Symbol Table ===\n");
    printf("Current scope: %d\n", table->current_scope);
    
    for (int i = 0; i < SYMBOL_TABLE_SIZE; i++) {
        Symbol *symbol = table->table[i];
        while (symbol) {
            const char *symbol_type_str;
            switch (symbol->symbol_type) {
                case SYMBOL_VARIABLE: symbol_type_str = "VARIABLE"; break;
                case SYMBOL_FUNCTION: symbol_type_str = "FUNCTION"; break;
                case SYMBOL_PARAMETER: symbol_type_str = "PARAMETER"; break;
                default: symbol_type_str = "UNKNOWN"; break;
            }
            
            printf("  %s: %s %s (scope %d)\n", 
                   symbol->name, 
                   data_type_to_string(symbol->data_type),
                   symbol_type_str,
                   symbol->scope_level);
            
            symbol = symbol->next;
        }
    }
    printf("=== End Symbol Table ===\n");
}