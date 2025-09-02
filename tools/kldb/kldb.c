#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

// --- Simulated KVM Instruction Set ---
typedef enum {
    opPUSH,       // Push a literal value onto the stack
    opADD,        // Pop two values, add them, and push the result
    opSUB,        // Pop two values, subtract, and push the result
    opPRINT,      // Pop a value and print it to the console
    opHALT        // Stop execution
} TKayteOpcode;

typedef struct {
    TKayteOpcode Opcode;
    int Operand; // Used by opPUSH for the value to push
} TInstruction;

// --- A simple Stack implementation for the VM ---
typedef struct StackNode {
    int data;
    struct StackNode *next;
} StackNode;

typedef struct {
    StackNode *top;
    int count;
} TStack;

TStack* stack_create() {
    TStack *stack = (TStack*)malloc(sizeof(TStack));
    if (stack == NULL) {
        fprintf(stderr, "Memory allocation failed for stack.\n");
        exit(1);
    }
    stack->top = NULL;
    stack->count = 0;
    return stack;
}

void stack_push(TStack *stack, int value) {
    StackNode *newNode = (StackNode*)malloc(sizeof(StackNode));
    if (newNode == NULL) {
        fprintf(stderr, "Memory allocation failed for stack node.\n");
        exit(1);
    }
    newNode->data = value;
    newNode->next = stack->top;
    stack->top = newNode;
    stack->count++;
}

int stack_pop(TStack *stack) {
    if (stack->top == NULL) {
        fprintf(stderr, "Stack underflow.\n");
        exit(1);
    }
    StackNode *temp = stack->top;
    int data = temp->data;
    stack->top = temp->next;
    free(temp);
    stack->count--;
    return data;
}

int stack_peek(TStack *stack) {
    if (stack->top == NULL) {
        return 0; // Or handle error
    }
    return stack->top->data;
}

void stack_destroy(TStack *stack) {
    while (stack->top != NULL) {
        stack_pop(stack);
    }
    free(stack);
}

// --- The Kayte Virtual Machine State ---
typedef struct {
    int program_counter;
    TStack *stack;
    TInstruction *instructions;
    int instruction_count;
} TKayteVM;

TKayteVM* vm_create() {
    TKayteVM *vm = (TKayteVM*)malloc(sizeof(TKayteVM));
    if (vm == NULL) {
        fprintf(stderr, "Memory allocation failed for VM.\n");
        exit(1);
    }
    vm->program_counter = 0;
    vm->stack = stack_create();
    vm->instructions = NULL;
    vm->instruction_count = 0;
    return vm;
}

void vm_destroy(TKayteVM *vm) {
    stack_destroy(vm->stack);
    if (vm->instructions != NULL) {
        free(vm->instructions);
    }
    free(vm);
}

void vm_load_program(TKayteVM *vm, const TInstruction *program, int count) {
    // In a real application, you would deserialize from a .kbyte file here.
    // For this example, we copy the provided instruction array.
    vm->instructions = (TInstruction*)malloc(count * sizeof(TInstruction));
    if (vm->instructions == NULL) {
        fprintf(stderr, "Memory allocation failed for program instructions.\n");
        exit(1);
    }
    memcpy(vm->instructions, program, count * sizeof(TInstruction));
    vm->instruction_count = count;
    vm->program_counter = 0;
    printf("Program loaded. Ready to debug.\n");
}

TInstruction vm_get_current_instruction(TKayteVM *vm) {
    if (vm->program_counter < vm->instruction_count) {
        return vm->instructions[vm->program_counter];
    }
    // Return a HALT instruction if program has finished
    return (TInstruction){opHALT, 0};
}

bool vm_is_halted(TKayteVM *vm) {
    return vm->program_counter >= vm->instruction_count;
}

void vm_step(TKayteVM *vm) {
    if (vm_is_halted(vm)) {
        printf("VM is already halted.\n");
        return;
    }

    TInstruction current_instr = vm_get_current_instruction(vm);
    int a, b;

    switch (current_instr.Opcode) {
        case opPUSH:
            printf("  > PUSH %d\n", current_instr.Operand);
            stack_push(vm->stack, current_instr.Operand);
            break;
        case opADD:
            printf("  > ADD\n");
            if (vm->stack->count < 2) {
                fprintf(stderr, "VM Error: Not enough operands for ADD\n");
                exit(1);
            }
            b = stack_pop(vm->stack);
            a = stack_pop(vm->stack);
            stack_push(vm->stack, a + b);
            break;
        case opSUB:
            printf("  > SUB\n");
            if (vm->stack->count < 2) {
                fprintf(stderr, "VM Error: Not enough operands for SUB\n");
                exit(1);
            }
            b = stack_pop(vm->stack);
            a = stack_pop(vm->stack);
            stack_push(vm->stack, a - b);
            break;
        case opPRINT:
            printf("  > PRINT\n");
            if (vm->stack->count < 1) {
                fprintf(stderr, "VM Error: Not enough operands for PRINT\n");
                exit(1);
            }
            printf("Output: %d\n", stack_pop(vm->stack));
            break;
        case opHALT:
            printf("  > HALT\n");
            break;
    }

    vm->program_counter++;
}

void vm_print_stack(TKayteVM *vm) {
    printf("--- Stack ---\n");
    if (vm->stack->count == 0) {
        printf("  (empty)\n");
    } else {
        StackNode *current = vm->stack->top;
        while (current != NULL) {
            printf("  %d\n", current->data);
            current = current->next;
        }
    }
    printf("-------------\n");
}

// --- The Debugger Application ---
typedef struct {
    TKayteVM *vm;
} TKayteDebugger;

TKayteDebugger* debugger_create() {
    TKayteDebugger *debugger = (TKayteDebugger*)malloc(sizeof(TKayteDebugger));
    if (debugger == NULL) {
        fprintf(stderr, "Memory allocation failed for debugger.\n");
        exit(1);
    }
    debugger->vm = vm_create();
    return debugger;
}

void debugger_destroy(TKayteDebugger *debugger) {
    vm_destroy(debugger->vm);
    free(debugger);
}

const char* disassemble_instruction(TInstruction instruction) {
    static char buffer[50];
    switch (instruction.Opcode) {
        case opPUSH:
            sprintf(buffer, "PUSH %d", instruction.Operand);
            break;
        case opADD:
            strcpy(buffer, "ADD");
            break;
        case opSUB:
            strcpy(buffer, "SUB");
            break;
        case opPRINT:
            strcpy(buffer, "PRINT");
            break;
        case opHALT:
            strcpy(buffer, "HALT");
            break;
        default:
            strcpy(buffer, "UNKNOWN_OPCODE");
            break;
    }
    return buffer;
}

void debugger_run(TKayteDebugger *debugger) {
    // Create a sample program: print(10 + 5)
    TInstruction sample_program[] = {
        {opPUSH, 10},
        {opPUSH, 5},
        {opADD, 0},
        {opPRINT, 0},
        {opHALT, 0}
    };
    int program_size = sizeof(sample_program) / sizeof(TInstruction);

    vm_load_program(debugger->vm, sample_program, program_size);

    printf("Kayte VM Debugger. Type 'help' for a list of commands.\n");

    char input_line[256];
    while (!vm_is_halted(debugger->vm)) {
        TInstruction current_instr = vm_get_current_instruction(debugger->vm);
        printf("PC=%d | %s\n", debugger->vm->program_counter, disassemble_instruction(current_instr));

        printf("kayte-dbg> ");
        if (fgets(input_line, sizeof(input_line), stdin) == NULL) {
            printf("\nExiting debugger.\n");
            break;
        }
        
        // Remove newline character
        input_line[strcspn(input_line, "\n")] = 0;

        // Convert to lowercase for case-insensitive comparison
        for (int i = 0; input_line[i]; i++) {
            input_line[i] = tolower(input_line[i]);
        }
        
        if (strcmp(input_line, "help") == 0) {
            printf("Commands:\n");
            printf("  s / step       : Execute the next instruction.\n");
            printf("  p / print      : Print the current contents of the stack.\n");
            printf("  q / quit       : Exit the debugger.\n");
        } else if (strcmp(input_line, "s") == 0 || strcmp(input_line, "step") == 0) {
            vm_step(debugger->vm);
        } else if (strcmp(input_line, "p") == 0 || strcmp(input_line, "print") == 0) {
            vm_print_stack(debugger->vm);
        } else if (strcmp(input_line, "q") == 0 || strcmp(input_line, "quit") == 0) {
            printf("Exiting debugger.\n");
            break;
        } else {
            printf("Unknown command. Type 'help' for a list of commands.\n");
        }
    }

    printf("Program finished.\n");
}

int main() {
    TKayteDebugger *debugger = debugger_create();
    debugger_run(debugger);
    debugger_destroy(debugger);
    return 0;
}

