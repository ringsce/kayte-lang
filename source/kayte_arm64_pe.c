#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <time.h>

// Instruction structure matching Pascal
typedef struct {
    int32_t opcode;
    int64_t operand;
} kayte_insn_t;

// PE/COFF constants
#define IMAGE_FILE_MACHINE_ARM64 0xAA64
#define IMAGE_FILE_EXECUTABLE_IMAGE 0x0002
#define IMAGE_FILE_LINE_NUMS_STRIPPED 0x0004
#define IMAGE_FILE_LARGE_ADDRESS_AWARE 0x0020

// DOS Header (MZ header)
typedef struct {
    uint16_t e_magic;    // "MZ"
    uint16_t e_cblp;
    uint16_t e_cp;
    uint16_t e_crlc;
    uint16_t e_cparhdr;
    uint16_t e_minalloc;
    uint16_t e_maxalloc;
    uint16_t e_ss;
    uint16_t e_sp;
    uint16_t e_csum;
    uint16_t e_ip;
    uint16_t e_cs;
    uint16_t e_lfarlc;
    uint16_t e_ovno;
    uint16_t e_res[4];
    uint16_t e_oemid;
    uint16_t e_oeminfo;
    uint16_t e_res2[10];
    uint32_t e_lfanew;   // Offset to PE header
} IMAGE_DOS_HEADER;

// PE File Header
typedef struct {
    uint16_t Machine;
    uint16_t NumberOfSections;
    uint32_t TimeDateStamp;
    uint32_t PointerToSymbolTable;
    uint32_t NumberOfSymbols;
    uint16_t SizeOfOptionalHeader;
    uint16_t Characteristics;
} IMAGE_FILE_HEADER;

// PE Optional Header (simplified for ARM64)
typedef struct {
    uint16_t Magic;
    uint8_t  MajorLinkerVersion;
    uint8_t  MinorLinkerVersion;
    uint32_t SizeOfCode;
    uint32_t SizeOfInitializedData;
    uint32_t SizeOfUninitializedData;
    uint32_t AddressOfEntryPoint;
    uint32_t BaseOfCode;
    uint64_t ImageBase;
    uint32_t SectionAlignment;
    uint32_t FileAlignment;
    uint16_t MajorOperatingSystemVersion;
    uint16_t MinorOperatingSystemVersion;
    uint16_t MajorImageVersion;
    uint16_t MinorImageVersion;
    uint16_t MajorSubsystemVersion;
    uint16_t MinorSubsystemVersion;
    uint32_t Win32VersionValue;
    uint32_t SizeOfImage;
    uint32_t SizeOfHeaders;
    uint32_t CheckSum;
    uint16_t Subsystem;
    uint16_t DllCharacteristics;
    uint64_t SizeOfStackReserve;
    uint64_t SizeOfStackCommit;
    uint64_t SizeOfHeapReserve;
    uint64_t SizeOfHeapCommit;
    uint32_t LoaderFlags;
    uint32_t NumberOfRvaAndSizes;
} IMAGE_OPTIONAL_HEADER64;

// Section Header
typedef struct {
    char     Name[8];
    uint32_t VirtualSize;
    uint32_t VirtualAddress;
    uint32_t SizeOfRawData;
    uint32_t PointerToRawData;
    uint32_t PointerToRelocations;
    uint32_t PointerToLinenumbers;
    uint16_t NumberOfRelocations;
    uint16_t NumberOfLinenumbers;
    uint32_t Characteristics;
} IMAGE_SECTION_HEADER;

// Emit ARM64 instruction
static void emit_arm64_insn(uint32_t **code_ptr, uint32_t instruction) {
    **code_ptr = instruction;
    (*code_ptr)++;
}

// Generate ARM64 machine code
static size_t generate_arm64_code(kayte_insn_t *instructions, int count, uint32_t **out_code) {
    uint32_t *code = malloc(count * 10 * sizeof(uint32_t));
    uint32_t *code_ptr = code;
    
    if (!code) return 0;
    
    // Prologue
    emit_arm64_insn(&code_ptr, 0xD10043FF); // sub sp, sp, #16
    
    // Translate instructions
    for (int i = 0; i < count; i++) {
        switch (instructions[i].opcode) {
            case 0: // OP_NOP
                emit_arm64_insn(&code_ptr, 0xD503201F); // nop
                break;
                
            case 1: // OP_PUSH
                emit_arm64_insn(&code_ptr, 0xD2800000 | ((instructions[i].operand & 0xFFFF) << 5));
                emit_arm64_insn(&code_ptr, 0xF81F0FE0); // str x0, [sp, #-16]!
                break;
                
            case 14: // OP_HALT
                // Exit via ExitProcess (syscall or function call)
                emit_arm64_insn(&code_ptr, 0xD2800000); // mov x0, #0
                emit_arm64_insn(&code_ptr, 0xD4000001); // svc #0 (simplified)
                break;
                
            default:
                emit_arm64_insn(&code_ptr, 0xD503201F); // nop
                break;
        }
    }
    
    // Epilogue
    emit_arm64_insn(&code_ptr, 0x910043FF); // add sp, sp, #16
    emit_arm64_insn(&code_ptr, 0xD65F03C0); // ret
    
    *out_code = code;
    return (code_ptr - code) * sizeof(uint32_t);
}

// Main compilation function
int kayte_arm64_compile_pe(kayte_insn_t *instructions, int instruction_count, const char *output_path) {
    FILE *fp;
    uint32_t *code = NULL;
    size_t code_size;
    IMAGE_DOS_HEADER dos_header = {0};
    IMAGE_FILE_HEADER file_header = {0};
    IMAGE_OPTIONAL_HEADER64 opt_header = {0};
    IMAGE_SECTION_HEADER text_section = {0};
    uint32_t pe_signature = 0x00004550; // "PE\0\0"
    
    if (!instructions || instruction_count <= 0 || !output_path) {
        fprintf(stderr, "Invalid parameters\n");
        return -1;
    }
    
    // Generate ARM64 machine code
    code_size = generate_arm64_code(instructions, instruction_count, &code);
    if (code_size == 0) {
        fprintf(stderr, "Failed to generate machine code\n");
        return -1;
    }
    
    // Setup DOS header
    dos_header.e_magic = 0x5A4D; // "MZ"
    dos_header.e_lfanew = sizeof(IMAGE_DOS_HEADER);
    
    // Setup PE file header
    file_header.Machine = IMAGE_FILE_MACHINE_ARM64;
    file_header.NumberOfSections = 1;
    file_header.TimeDateStamp = (uint32_t)time(NULL);
    file_header.SizeOfOptionalHeader = sizeof(IMAGE_OPTIONAL_HEADER64);
    file_header.Characteristics = IMAGE_FILE_EXECUTABLE_IMAGE | IMAGE_FILE_LARGE_ADDRESS_AWARE;
    
    // Setup optional header
    opt_header.Magic = 0x020B; // PE32+
    opt_header.SizeOfCode = (uint32_t)code_size;
    opt_header.AddressOfEntryPoint = 0x1000;
    opt_header.BaseOfCode = 0x1000;
    opt_header.ImageBase = 0x140000000;
    opt_header.SectionAlignment = 0x1000;
    opt_header.FileAlignment = 0x200;
    opt_header.MajorOperatingSystemVersion = 10;
    opt_header.MajorSubsystemVersion = 10;
    opt_header.SizeOfImage = 0x2000;
    opt_header.SizeOfHeaders = 0x200;
    opt_header.Subsystem = 3; // Console
    
    // Setup .text section
    strncpy((char*)text_section.Name, ".text", 8);
    text_section.VirtualSize = (uint32_t)code_size;
    text_section.VirtualAddress = 0x1000;
    text_section.SizeOfRawData = (uint32_t)((code_size + 0x1FF) & ~0x1FF);
    text_section.PointerToRawData = 0x200;
    text_section.Characteristics = 0x60000020; // CODE | EXECUTE | READ
    
    // Write PE file
    fp = fopen(output_path, "wb");
    if (!fp) {
        fprintf(stderr, "Failed to open output file: %s\n", output_path);
        free(code);
        return -1;
    }
    
    // Write headers
    fwrite(&dos_header, sizeof(dos_header), 1, fp);
    fwrite(&pe_signature, sizeof(pe_signature), 1, fp);
    fwrite(&file_header, sizeof(file_header), 1, fp);
    fwrite(&opt_header, sizeof(opt_header), 1, fp);
    fwrite(&text_section, sizeof(text_section), 1, fp);
    
    // Pad to file alignment
    fseek(fp, 0x200, SEEK_SET);
    
    // Write code
    fwrite(code, code_size, 1, fp);
    
    fclose(fp);
    free(code);
    
    printf("PE executable generated successfully: %s\n", output_path);
    return 0;
}
