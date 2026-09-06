#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <elf.h>

// Instruction structure matching Pascal
typedef struct {
    int32_t opcode;
    int64_t operand;
} kayte_insn_t;

// ELF64 ARM64 header constants
#define ELF_MACHINE_AARCH64 183
#define PAGE_SIZE 4096

static int write_elf_header(FILE *fp, size_t code_size) {
    Elf64_Ehdr ehdr = {0};
    
    // ELF Magic
    ehdr.e_ident[EI_MAG0] = ELFMAG0;
    ehdr.e_ident[EI_MAG1] = ELFMAG1;
    ehdr.e_ident[EI_MAG2] = ELFMAG2;
    ehdr.e_ident[EI_MAG3] = ELFMAG3;
    ehdr.e_ident[EI_CLASS] = ELFCLASS64;
    ehdr.e_ident[EI_DATA] = ELFDATA2LSB;
    ehdr.e_ident[EI_VERSION] = EV_CURRENT;
    ehdr.e_ident[EI_OSABI] = ELFOSABI_LINUX;
    
    ehdr.e_type = ET_EXEC;
    ehdr.e_machine = ELF_MACHINE_AARCH64;
    ehdr.e_version = EV_CURRENT;
    ehdr.e_entry = 0x400000 + sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr);
    ehdr.e_phoff = sizeof(Elf64_Ehdr);
    ehdr.e_shoff = 0;
    ehdr.e_flags = 0;
    ehdr.e_ehsize = sizeof(Elf64_Ehdr);
    ehdr.e_phentsize = sizeof(Elf64_Phdr);
    ehdr.e_phnum = 1;
    
    return fwrite(&ehdr, sizeof(ehdr), 1, fp) == 1 ? 0 : -1;
}

static int write_program_header(FILE *fp, size_t code_size) {
    Elf64_Phdr phdr = {0};
    
    phdr.p_type = PT_LOAD;
    phdr.p_flags = PF_R | PF_X;
    phdr.p_offset = 0;
    phdr.p_vaddr = 0x400000;
    phdr.p_paddr = 0x400000;
    phdr.p_filesz = sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr) + code_size;
    phdr.p_memsz = phdr.p_filesz;
    phdr.p_align = PAGE_SIZE;
    
    return fwrite(&phdr, sizeof(phdr), 1, fp) == 1 ? 0 : -1;
}

static void emit_arm64_insn(uint32_t **code_ptr, uint32_t instruction) {
    **code_ptr = instruction;
    (*code_ptr)++;
}

static size_t generate_arm64_code(kayte_insn_t *instructions, int count, uint32_t **out_code) {
    // each Kayte instruction can expand to several ARM64 ones, 10x is a safe upper bound
    uint32_t *code = malloc(count * 10 * sizeof(uint32_t));
    uint32_t *code_ptr = code;
    
    if (!code) return 0;
    
    // Prologue
    emit_arm64_insn(&code_ptr, 0xD10043FF); // sub sp, sp, #16
    emit_arm64_insn(&code_ptr, 0xF90007E0); // str x0, [sp, #8]
    
    // Translate instructions
    for (int i = 0; i < count; i++) {
        switch (instructions[i].opcode) {
            case 0: // OP_NOP
                emit_arm64_insn(&code_ptr, 0xD503201F); // nop
                break;
                
            case 1: // OP_PUSH
                // mov x0, #operand
                emit_arm64_insn(&code_ptr, 0xD2800000 | (instructions[i].operand & 0xFFFF) << 5);
                // str x0, [sp, #-16]!
                emit_arm64_insn(&code_ptr, 0xF81F0FE0);
                break;
                
            case 13: // OP_PRINT
                // Call printf or syscall
                emit_arm64_insn(&code_ptr, 0xD503201F); // nop (placeholder)
                break;
                
            case 14: // OP_HALT
                // Exit syscall
                emit_arm64_insn(&code_ptr, 0xD2800BA8); // mov x8, #93 (exit syscall)
                emit_arm64_insn(&code_ptr, 0xD2800000); // mov x0, #0
                emit_arm64_insn(&code_ptr, 0xD4000001); // svc #0
                break;
                
            default:
                emit_arm64_insn(&code_ptr, 0xD503201F); // nop
                break;
        }
    }
    
    // Epilogue
    emit_arm64_insn(&code_ptr, 0xF94007E0); // ldr x0, [sp, #8]
    emit_arm64_insn(&code_ptr, 0x910043FF); // add sp, sp, #16
    emit_arm64_insn(&code_ptr, 0xD65F03C0); // ret
    
    *out_code = code;
    return (code_ptr - code) * sizeof(uint32_t);
}

int kayte_arm64_compile_elf(kayte_insn_t *instructions, int instruction_count, const char *output_path) {
    FILE *fp;
    uint32_t *code = NULL;
    size_t code_size;
    int result = -1;
    
    if (!instructions || instruction_count <= 0 || !output_path) {
        fprintf(stderr, "Invalid parameters\n");
        return -1;
    }
    
    code_size = generate_arm64_code(instructions, instruction_count, &code);
    if (code_size == 0) {
        fprintf(stderr, "Failed to generate machine code\n");
        return -1;
    }

    fp = fopen(output_path, "wb");
    if (!fp) {
        fprintf(stderr, "Failed to open output file: %s\n", output_path);
        free(code);
        return -1;
    }

    if (write_elf_header(fp, code_size) != 0) {
        fprintf(stderr, "Failed to write ELF header\n");
        goto cleanup;
    }

    if (write_program_header(fp, code_size) != 0) {
        fprintf(stderr, "Failed to write program header\n");
        goto cleanup;
    }

    if (fwrite(code, code_size, 1, fp) != 1) {
        fprintf(stderr, "Failed to write code section\n");
        goto cleanup;
    }
    
    result = 0;
    printf("ELF executable generated successfully: %s\n", output_path);
    
cleanup:
    fclose(fp);
    free(code);
    
    // Make executable
    if (result == 0) {
        chmod(output_path, 0755);
    }
    
    return result;
}
