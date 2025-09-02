#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "types.h"

#define SYMBOL_TABLE_SIZE 128

typedef enum {
    SYMBOL_VARIABLE,
    SYMBOL_FUNCTION,
    SYMBOL_PARAMETER
} SymbolType;

typedef struct Symbol {
    char *name;
    SymbolType symbol_type;
    DataType data_type;
    int scope_level;
    struct Symbol *next;
} Symbol;

typedef struct SymbolTable {
    Symbol *table[SYMBOL_TABLE_SIZE];
    int current_scope;
} SymbolTable;

// Symbol table creation and destruction
SymbolTable *symbol_table_create(void);
void symbol_table_destroy(SymbolTable *table);

// Scope management
void symbol_table_enter_scope(SymbolTable *table);
void symbol_table_exit_scope(SymbolTable *table);

// Symbol operations
bool symbol_table_insert(SymbolTable *table, const char *name, SymbolType symbol_type, DataType data_type);
Symbol *symbol_table_lookup(SymbolTable *table, const char *name);
Symbol *symbol_table_lookup_current_scope(SymbolTable *table, const char *name);

// Utility functions
unsigned int symbol_table_hash(const char *name);
void symbol_table_print(SymbolTable *table);

#endif // SYMBOL_TABLE_H