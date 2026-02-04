/*
 * kayte_arm64_emit.c
 * ------------------
 * Direct ARM64 (AArch64) machine code emitter for Kayte Lang.
 * Zero external dependencies — emits raw Mach-O binaries on macOS Silicon.
 *
 * Responsibilities:
 *   1. Take Kayte bytecode opcodes and translate them to ARM64 instructions
 *   2. Generate complete Mach-O executable files from scratch
 *   3. Manage code layout, data sections, relocations
 *   4. Provide FFI trampolines for calling C functions
 *
 * Architecture:
 *   - Stack-based VM bytecode → register-based ARM64
 *   - Uses x19-x28 as virtual registers for the top of stack
 *   - x29 = frame pointer, x30 = link register (standard AArch64 ABI)
 *   - x0-x7 used for function arguments/returns (standard calling convention)
 *
 * Build:
 *   clang -c -O2 -std=c11 -arch arm64 kayte_arm64_emit.c -o kayte_arm64_emit.o
 */

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>   /* chmod */

/* ================================================================== */
/* 1. ARM64 INSTRUCTION ENCODING                                      */
/* ================================================================== */

/*
 * ARM64 instructions are fixed 32-bit width.
 * We build them as uint32_t and write to the code buffer.
 */

/* --- Arithmetic (register) ---------------------------------------- */

static inline uint32_t arm64_add_reg(uint8_t rd, uint8_t rn, uint8_t rm)
{
    /* ADD Xd, Xn, Xm  (64-bit registers)
     * Encoding: 1000_1011_000_Rm_000000_Rn_Rd */
    return 0x8b000000 | ((uint32_t)rm << 16) | ((uint32_t)rn << 5) | rd;
}

static inline uint32_t arm64_sub_reg(uint8_t rd, uint8_t rn, uint8_t rm)
{
    /* SUB Xd, Xn, Xm */
    return 0xcb000000 | ((uint32_t)rm << 16) | ((uint32_t)rn << 5) | rd;
}

static inline uint32_t arm64_mul_reg(uint8_t rd, uint8_t rn, uint8_t rm)
{
    /* MUL Xd, Xn, Xm  →  MADD Xd, Xn, Xm, XZR */
    return 0x9b007c00 | ((uint32_t)rm << 16) | (31 << 10) | ((uint32_t)rn << 5) | rd;
}

/* --- Arithmetic (immediate) --------------------------------------- */

static inline uint32_t arm64_add_imm(uint8_t rd, uint8_t rn, uint16_t imm12)
{
    /* ADD Xd, Xn, #imm12  (shift=0) */
    return 0x91000000 | ((uint32_t)(imm12 & 0xfff) << 10) | ((uint32_t)rn << 5) | rd;
}

static inline uint32_t arm64_sub_imm(uint8_t rd, uint8_t rn, uint16_t imm12)
{
    /* SUB Xd, Xn, #imm12 */
    return 0xd1000000 | ((uint32_t)(imm12 & 0xfff) << 10) | ((uint32_t)rn << 5) | rd;
}

/* --- Load/Store --------------------------------------------------- */

static inline uint32_t arm64_ldr_imm(uint8_t rt, uint8_t rn, uint16_t offset)
{
    /* LDR Xt, [Xn, #offset]  (unsigned offset, scaled by 8 for 64-bit) */
    uint16_t imm9 = offset / 8;
    return 0xf9400000 | ((uint32_t)(imm9 & 0xfff) << 10) | ((uint32_t)rn << 5) | rt;
}

static inline uint32_t arm64_str_imm(uint8_t rt, uint8_t rn, uint16_t offset)
{
    /* STR Xt, [Xn, #offset] */
    uint16_t imm9 = offset / 8;
    return 0xf9000000 | ((uint32_t)(imm9 & 0xfff) << 10) | ((uint32_t)rn << 5) | rt;
}

static inline uint32_t arm64_stp_pre(uint8_t rt1, uint8_t rt2, uint8_t rn, int16_t offset)
{
    /* STP Xt1, Xt2, [Xn, #offset]!  (pre-indexed, offset is scaled /8) */
    int16_t imm7 = offset / 8;
    return 0xa9800000 | (3 << 30) | ((uint32_t)(imm7 & 0x7f) << 15)
         | ((uint32_t)rt2 << 10) | ((uint32_t)rn << 5) | rt1;
}

static inline uint32_t arm64_ldp_post(uint8_t rt1, uint8_t rt2, uint8_t rn, int16_t offset)
{
    /* LDP Xt1, Xt2, [Xn], #offset  (post-indexed) */
    int16_t imm7 = offset / 8;
    return 0xa8c00000 | (3 << 30) | ((uint32_t)(imm7 & 0x7f) << 15)
         | ((uint32_t)rt2 << 10) | ((uint32_t)rn << 5) | rt1;
}

/* --- Move / Load immediate ---------------------------------------- */

static inline uint32_t arm64_movz(uint8_t rd, uint16_t imm16, uint8_t shift)
{
    /* MOVZ Xd, #imm16, LSL #(shift*16)  (shift = 0,1,2,3) */
    return 0xd2800000 | ((uint32_t)(shift & 3) << 21) | ((uint32_t)imm16 << 5) | rd;
}

static inline uint32_t arm64_movk(uint8_t rd, uint16_t imm16, uint8_t shift)
{
    /* MOVK Xd, #imm16, LSL #(shift*16) */
    return 0xf2800000 | ((uint32_t)(shift & 3) << 21) | ((uint32_t)imm16 << 5) | rd;
}

static inline uint32_t arm64_mov_reg(uint8_t rd, uint8_t rm)
{
    /* MOV Xd, Xm  →  ORR Xd, XZR, Xm */
    return 0xaa0003e0 | ((uint32_t)rm << 16) | rd;
}

/* --- Branch / Call ------------------------------------------------ */

static inline uint32_t arm64_b(int32_t offset_words)
{
    /* B label  (offset in instructions, ±128MB range) */
    return 0x14000000 | ((uint32_t)offset_words & 0x03ffffff);
}

static inline uint32_t arm64_bl(int32_t offset_words)
{
    /* BL label  (branch with link → call) */
    return 0x94000000 | ((uint32_t)offset_words & 0x03ffffff);
}

static inline uint32_t arm64_blr(uint8_t rn)
{
    /* BLR Xn  (branch to register) */
    return 0xd63f0000 | ((uint32_t)rn << 5);
}

static inline uint32_t arm64_ret(uint8_t rn)
{
    /* RET Xn  (return, defaults to x30) */
    return 0xd65f0000 | ((uint32_t)rn << 5);
}

static inline uint32_t arm64_cbz(uint8_t rt, int32_t offset_words)
{
    /* CBZ Xt, label  (compare and branch if zero) */
    return 0xb4000000 | (1 << 31) | ((uint32_t)(offset_words & 0x7ffff) << 5) | rt;
}

static inline uint32_t arm64_cbnz(uint8_t rt, int32_t offset_words)
{
    /* CBNZ Xt, label */
    return 0xb5000000 | (1 << 31) | ((uint32_t)(offset_words & 0x7ffff) << 5) | rt;
}

/* --- Compare ------------------------------------------------------ */

static inline uint32_t arm64_cmp_reg(uint8_t rn, uint8_t rm)
{
    /* CMP Xn, Xm  →  SUBS XZR, Xn, Xm */
    return 0xeb00001f | ((uint32_t)rm << 16) | ((uint32_t)rn << 5);
}

static inline uint32_t arm64_cmp_imm(uint8_t rn, uint16_t imm12)
{
    /* CMP Xn, #imm12  →  SUBS XZR, Xn, #imm12 */
    return 0xf100001f | ((uint32_t)(imm12 & 0xfff) << 10) | ((uint32_t)rn << 5);
}

/* --- Conditional select ------------------------------------------- */

static inline uint32_t arm64_csel(uint8_t rd, uint8_t rn, uint8_t rm, uint8_t cond)
{
    /* CSEL Xd, Xn, Xm, cond */
    return 0x9a800000 | ((uint32_t)rm << 16) | ((uint32_t)cond << 12)
         | ((uint32_t)rn << 5) | rd;
}

/* Condition codes */
#define ARM64_COND_EQ  0x0   /* equal */
#define ARM64_COND_NE  0x1   /* not equal */
#define ARM64_COND_GT  0xc   /* signed greater than */
#define ARM64_COND_LT  0xb   /* signed less than */
#define ARM64_COND_GE  0xa   /* signed greater or equal */
#define ARM64_COND_LE  0xd   /* signed less or equal */

/* --- System / Utility --------------------------------------------- */

static inline uint32_t arm64_nop(void)
{
    return 0xd503201f;
}

static inline uint32_t arm64_brk(uint16_t imm16)
{
    /* BRK #imm16  (software breakpoint) */
    return 0xd4200000 | ((uint32_t)imm16 << 5);
}

/* ================================================================== */
/* 2. CODE BUFFER — where we write ARM64 instructions                */
/* ================================================================== */

typedef struct {
    uint32_t *code;        /* array of 32-bit ARM64 instructions */
    size_t    size;        /* current count of instructions */
    size_t    capacity;    /* allocated capacity */
} arm64_buffer_t;

static void arm64_buf_init(arm64_buffer_t *buf, size_t initial_cap)
{
    buf->code     = (uint32_t *)malloc(initial_cap * sizeof(uint32_t));
    buf->size     = 0;
    buf->capacity = initial_cap;
}

static void arm64_buf_free(arm64_buffer_t *buf)
{
    free(buf->code);
    buf->code = NULL;
    buf->size = buf->capacity = 0;
}

static void arm64_buf_emit(arm64_buffer_t *buf, uint32_t insn)
{
    if (buf->size >= buf->capacity) {
        buf->capacity *= 2;
        buf->code = (uint32_t *)realloc(buf->code, buf->capacity * sizeof(uint32_t));
    }
    buf->code[buf->size++] = insn;
}

/* Convenience: emit multiple instructions */
static void arm64_buf_emit_many(arm64_buffer_t *buf, const uint32_t *insns, size_t count)
{
    for (size_t i = 0; i < count; ++i) arm64_buf_emit(buf, insns[i]);
}

/* Get current instruction offset (for labels/jumps) */
static size_t arm64_buf_offset(const arm64_buffer_t *buf) { return buf->size; }

/* Patch a previously-emitted instruction (for forward jumps) */
static void arm64_buf_patch(arm64_buffer_t *buf, size_t offset, uint32_t insn)
{
    if (offset < buf->size) buf->code[offset] = insn;
}

/* ================================================================== */
/* 3. KAYTE BYTECODE → ARM64 TRANSLATOR                               */
/* ================================================================== */

/*
 * Kayte VM opcodes (simplified set matching the documented PoC).
 * Extend this enum as the real bytecode grows.
 */
typedef enum {
    OP_HALT = 0x00,      /* end execution                              */
    OP_NOP  = 0x01,      /* no operation                               */

    /* Stack manipulation */
    OP_PUSH_INT  = 0x10, /* push 64-bit immediate onto stack           */
    OP_POP       = 0x11, /* discard top of stack                       */
    OP_DUP       = 0x12, /* duplicate top of stack                     */

    /* Arithmetic */
    OP_ADD       = 0x20, /* pop b, pop a, push a+b                     */
    OP_SUB       = 0x21, /* pop b, pop a, push a-b                     */
    OP_MUL       = 0x22, /* pop b, pop a, push a*b                     */
    OP_DIV       = 0x23, /* pop b, pop a, push a/b (signed)            */

    /* Comparison */
    OP_CMP_EQ    = 0x30, /* pop b, pop a, push (a == b)                */
    OP_CMP_LT    = 0x31, /* pop b, pop a, push (a < b)                 */
    OP_CMP_GT    = 0x32, /* pop b, pop a, push (a > b)                 */

    /* Control flow */
    OP_JMP       = 0x40, /* unconditional jump to offset               */
    OP_JZ        = 0x41, /* pop a, jump if a == 0                      */
    OP_JNZ       = 0x42, /* pop a, jump if a != 0                      */

    /* Function calls */
    OP_CALL      = 0x50, /* call function at offset                    */
    OP_RET       = 0x51, /* return from function                       */
    OP_CALL_NATIVE = 0x52, /* call C FFI function by index             */

    /* Variables (locals / globals) */
    OP_LOAD_LOCAL  = 0x60, /* push local variable by index             */
    OP_STORE_LOCAL = 0x61, /* pop and store to local variable          */
    OP_LOAD_GLOBAL = 0x62, /* push global variable by index            */
    OP_STORE_GLOBAL= 0x63, /* pop and store to global                  */

    /* Print (debug) */
    OP_PRINT     = 0x70  /* pop and print (for testing)                */
} kayte_opcode_t;

/*
 * Bytecode instruction structure.
 * Real Kayte bytecode would be more compact; this is a clear intermediate.
 */
typedef struct {
    kayte_opcode_t op;
    int64_t        arg;   /* immediate value or jump offset */
} kayte_insn_t;

/*
 * Compiler state — tracks stack depth, local variables, labels.
 */
typedef struct {
    arm64_buffer_t code;
    int            stack_depth;   /* current virtual stack depth       */
    int            max_stack;     /* max depth seen (for frame alloc)  */
    int            local_count;   /* number of local variables         */

    /* Simple label table for forward jumps */
    size_t        *labels;        /* ARM64 code offsets (NULL = unresolved) */
    size_t         label_cap;
} kayte_compiler_t;

static void kc_init(kayte_compiler_t *kc, size_t initial_code_cap)
{
    arm64_buf_init(&kc->code, initial_code_cap);
    kc->stack_depth = 0;
    kc->max_stack   = 0;
    kc->local_count = 0;
    kc->labels      = (size_t *)calloc(256, sizeof(size_t));
    kc->label_cap   = 256;
}

static void kc_free(kayte_compiler_t *kc)
{
    arm64_buf_free(&kc->code);
    free(kc->labels);
}

/* Track stack operations */
static void kc_push(kayte_compiler_t *kc)
{
    kc->stack_depth++;
    if (kc->stack_depth > kc->max_stack) kc->max_stack = kc->stack_depth;
}

static void kc_pop(kayte_compiler_t *kc)
{
    if (kc->stack_depth > 0) kc->stack_depth--;
}

/* Map stack slots to ARM64 registers:
 *   x19 = stack[0] (top)
 *   x20 = stack[1]
 *   ...
 *   x27 = stack[8]
 * If stack_depth > 9, we'd spill to memory (not implemented here for simplicity).
 */
static uint8_t kc_stack_reg(int depth)
{
    /* depth 0 = top of stack → x19, depth 1 → x20, etc. */
    if (depth >= 0 && depth < 9) return 19 + depth;
    return 0; /* error / spill needed */
}

/* ================================================================== */
/* 4. BYTECODE → ARM64 COMPILER (instruction-by-instruction)          */
/* ================================================================== */

static void kc_compile_insn(kayte_compiler_t *kc, const kayte_insn_t *insn)
{
    arm64_buffer_t *buf = &kc->code;
    uint8_t r0, r1, r2;

    switch (insn->op) {

    /* ---- HALT ---------------------------------------------------- */
    case OP_HALT:
        /* Exit by calling exit(0).  In a real app, return to runtime. */
        arm64_buf_emit(buf, arm64_movz(0, 0, 0));      /* x0 = 0       */
        arm64_buf_emit(buf, arm64_movz(16, 1, 0));     /* x16 = 1 (exit syscall) */
        arm64_buf_emit(buf, 0xd4000001);               /* SVC #0       */
        break;

    /* ---- NOP ----------------------------------------------------- */
    case OP_NOP:
        arm64_buf_emit(buf, arm64_nop());
        break;

    /* ---- PUSH_INT <imm> ------------------------------------------ */
    case OP_PUSH_INT:
        r0 = kc_stack_reg(kc->stack_depth);
        /* Load 64-bit immediate into r0 (4 instructions) */
        arm64_buf_emit(buf, arm64_movz(r0, (uint16_t)(insn->arg & 0xffff), 0));
        arm64_buf_emit(buf, arm64_movk(r0, (uint16_t)((insn->arg >> 16) & 0xffff), 1));
        arm64_buf_emit(buf, arm64_movk(r0, (uint16_t)((insn->arg >> 32) & 0xffff), 2));
        arm64_buf_emit(buf, arm64_movk(r0, (uint16_t)((insn->arg >> 48) & 0xffff), 3));
        kc_push(kc);
        break;

    /* ---- POP ----------------------------------------------------- */
    case OP_POP:
        kc_pop(kc);
        break;

    /* ---- DUP ----------------------------------------------------- */
    case OP_DUP:
        r0 = kc_stack_reg(kc->stack_depth - 1);  /* current top      */
        r1 = kc_stack_reg(kc->stack_depth);      /* new top          */
        arm64_buf_emit(buf, arm64_mov_reg(r1, r0));
        kc_push(kc);
        break;

    /* ---- ADD ----------------------------------------------------- */
    case OP_ADD:
        r1 = kc_stack_reg(kc->stack_depth - 2);  /* a = stack[1]     */
        r0 = kc_stack_reg(kc->stack_depth - 1);  /* b = stack[0]     */
        arm64_buf_emit(buf, arm64_add_reg(r1, r1, r0));
        kc_pop(kc);  /* result in stack[1], pop stack[0] */
        break;

    /* ---- SUB ----------------------------------------------------- */
    case OP_SUB:
        r1 = kc_stack_reg(kc->stack_depth - 2);
        r0 = kc_stack_reg(kc->stack_depth - 1);
        arm64_buf_emit(buf, arm64_sub_reg(r1, r1, r0));
        kc_pop(kc);
        break;

    /* ---- MUL ----------------------------------------------------- */
    case OP_MUL:
        r1 = kc_stack_reg(kc->stack_depth - 2);
        r0 = kc_stack_reg(kc->stack_depth - 1);
        arm64_buf_emit(buf, arm64_mul_reg(r1, r1, r0));
        kc_pop(kc);
        break;

    /* ---- CMP_EQ -------------------------------------------------- */
    case OP_CMP_EQ:
        r1 = kc_stack_reg(kc->stack_depth - 2);
        r0 = kc_stack_reg(kc->stack_depth - 1);
        arm64_buf_emit(buf, arm64_cmp_reg(r1, r0));
        /* CSEL: if EQ → r1=1, else r1=0 */
        arm64_buf_emit(buf, arm64_movz(2, 1, 0));      /* x2 = 1       */
        arm64_buf_emit(buf, arm64_movz(3, 0, 0));      /* x3 = 0       */
        arm64_buf_emit(buf, arm64_csel(r1, 2, 3, ARM64_COND_EQ));
        kc_pop(kc);
        break;

    /* ---- CMP_LT -------------------------------------------------- */
    case OP_CMP_LT:
        r1 = kc_stack_reg(kc->stack_depth - 2);
        r0 = kc_stack_reg(kc->stack_depth - 1);
        arm64_buf_emit(buf, arm64_cmp_reg(r1, r0));
        arm64_buf_emit(buf, arm64_movz(2, 1, 0));
        arm64_buf_emit(buf, arm64_movz(3, 0, 0));
        arm64_buf_emit(buf, arm64_csel(r1, 2, 3, ARM64_COND_LT));
        kc_pop(kc);
        break;

    /* ---- JMP <offset> -------------------------------------------- */
    case OP_JMP:
        /* Unconditional branch.  offset is in bytecode instructions;
           convert to ARM64 instruction offset. */
        {
            int32_t target = (int32_t)insn->arg;
            int32_t current = (int32_t)arm64_buf_offset(buf);
            int32_t delta = target - current;
            arm64_buf_emit(buf, arm64_b(delta));
        }
        break;

    /* ---- JZ <offset> --------------------------------------------- */
    case OP_JZ:
        r0 = kc_stack_reg(kc->stack_depth - 1);
        {
            int32_t target = (int32_t)insn->arg;
            int32_t current = (int32_t)arm64_buf_offset(buf);
            int32_t delta = target - current;
            arm64_buf_emit(buf, arm64_cbz(r0, delta));
        }
        kc_pop(kc);
        break;

    /* ---- JNZ <offset> -------------------------------------------- */
    case OP_JNZ:
        r0 = kc_stack_reg(kc->stack_depth - 1);
        {
            int32_t target = (int32_t)insn->arg;
            int32_t current = (int32_t)arm64_buf_offset(buf);
            int32_t delta = target - current;
            arm64_buf_emit(buf, arm64_cbnz(r0, delta));
        }
        kc_pop(kc);
        break;

    /* ---- RET ----------------------------------------------------- */
    case OP_RET:
        /* Standard epilogue: restore x29, x30, return */
        arm64_buf_emit(buf, arm64_ldp_post(29, 30, 31, 16));  /* sp += 16 */
        arm64_buf_emit(buf, arm64_ret(30));
        break;

    /* ---- CALL_NATIVE <index> ------------------------------------- */
    case OP_CALL_NATIVE:
        /* Placeholder: would call kayte_ffi_call.
         * For now, just emit a BRK for demonstration. */
        arm64_buf_emit(buf, arm64_brk((uint16_t)insn->arg));
        break;

    /* ---- PRINT (debug helper) ------------------------------------ */
    case OP_PRINT:
        /* Pop top of stack and print (would call printf via FFI).
         * For demonstration, just BRK. */
        kc_pop(kc);
        arm64_buf_emit(buf, arm64_brk(0x70));
        break;

    default:
        /* Unknown opcode — insert BRK to trap */
        arm64_buf_emit(buf, arm64_brk(0xff));
        break;
    }
}

/* Compile an array of Kayte bytecode instructions */
static void kc_compile(kayte_compiler_t *kc,
                       const kayte_insn_t *bytecode,
                       size_t count)
{
    /* Emit function prologue */
    arm64_buf_emit(&kc->code, arm64_stp_pre(29, 30, 31, -16)); /* save fp,lr */
    arm64_buf_emit(&kc->code, arm64_mov_reg(29, 31));          /* fp = sp    */

    /* Compile each bytecode instruction */
    for (size_t i = 0; i < count; ++i) {
        kc_compile_insn(kc, &bytecode[i]);
    }

    /* If no explicit RET was emitted, add one */
    if (count == 0 || bytecode[count - 1].op != OP_RET) {
        arm64_buf_emit(&kc->code, arm64_ldp_post(29, 30, 31, 16));
        arm64_buf_emit(&kc->code, arm64_ret(30));
    }
}

/* ================================================================== */
/* 5. MACH-O EXECUTABLE GENERATOR (macOS Silicon)                     */
/* ================================================================== */

/*
 * Mach-O structures (minimal subset for a working executable).
 * See: /usr/include/mach-o/loader.h
 */

#define MH_MAGIC_64        0xfeedfacf
#define MH_EXECUTE         0x2
#define CPU_TYPE_ARM64     0x0100000c
#define CPU_SUBTYPE_ARM64  0x0
#define MH_NOUNDEFS        0x1

#define LC_SEGMENT_64      0x19
#define LC_UNIXTHREAD      0x5

#define VM_PROT_READ       0x01
#define VM_PROT_WRITE      0x02
#define VM_PROT_EXECUTE    0x04

typedef struct {
    uint32_t magic;
    uint32_t cputype;
    uint32_t cpusubtype;
    uint32_t filetype;
    uint32_t ncmds;
    uint32_t sizeofcmds;
    uint32_t flags;
    uint32_t reserved;
} mach_header_64_t;

typedef struct {
    uint32_t cmd;
    uint32_t cmdsize;
} load_command_t;

typedef struct {
    uint32_t  cmd;
    uint32_t  cmdsize;
    char      segname[16];
    uint64_t  vmaddr;
    uint64_t  vmsize;
    uint64_t  fileoff;
    uint64_t  filesize;
    uint32_t  maxprot;
    uint32_t  initprot;
    uint32_t  nsects;
    uint32_t  flags;
} segment_command_64_t;

typedef struct {
    uint32_t cmd;
    uint32_t cmdsize;
    uint32_t flavor;       /* ARM_THREAD_STATE64 = 6 */
    uint32_t count;        /* state size / 4         */
    /* Followed by ARM64 thread state (68 registers * 8 bytes = 544 bytes) */
} thread_command_t;

static void write_macho(const char *filename, const arm64_buffer_t *code)
{
    FILE *f = fopen(filename, "wb");
    if (!f) return;

    /* Entry point virtual address (standard for macOS) */
    uint64_t entry_va = 0x100000000;
    size_t code_size = code->size * 4;  /* bytes */
    size_t page_size = 16384;           /* 16KB pages on Apple Silicon */
    size_t aligned_size = ((code_size + page_size - 1) / page_size) * page_size;

    /* Mach-O header */
    mach_header_64_t hdr = {
        .magic       = MH_MAGIC_64,
        .cputype     = CPU_TYPE_ARM64,
        .cpusubtype  = CPU_SUBTYPE_ARM64,
        .filetype    = MH_EXECUTE,
        .ncmds       = 2,  /* SEGMENT_64 + UNIXTHREAD */
        .sizeofcmds  = sizeof(segment_command_64_t) + sizeof(thread_command_t) + 272,
        .flags       = MH_NOUNDEFS,
        .reserved    = 0
    };
    fwrite(&hdr, sizeof(hdr), 1, f);

    /* LC_SEGMENT_64 __TEXT */
    segment_command_64_t seg = {
        .cmd      = LC_SEGMENT_64,
        .cmdsize  = sizeof(segment_command_64_t),
        .vmaddr   = entry_va,
        .vmsize   = aligned_size,
        .fileoff  = 4096,  /* first page after header */
        .filesize = code_size,
        .maxprot  = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE,
        .initprot = VM_PROT_READ | VM_PROT_EXECUTE,
        .nsects   = 0,
        .flags    = 0
    };
    memcpy(seg.segname, "__TEXT", 7);
    fwrite(&seg, sizeof(seg), 1, f);

    /* LC_UNIXTHREAD (sets PC to entry point) */
    thread_command_t thrd = {
        .cmd     = LC_UNIXTHREAD,
        .cmdsize = sizeof(thread_command_t) + 272,
        .flavor  = 6,     /* ARM_THREAD_STATE64 */
        .count   = 68     /* 544 bytes / 4 */
    };
    fwrite(&thrd, sizeof(thrd), 1, f);

    /* ARM64 thread state: 34 x 64-bit registers + PC + CPSR */
    uint64_t regs[68] = {0};
    regs[32] = entry_va;  /* PC = entry point */
    fwrite(regs, sizeof(regs), 1, f);

    /* Pad to page boundary */
    size_t hdr_end = ftell(f);
    while (ftell(f) < 4096) fputc(0, f);

    /* Write code */
    fwrite(code->code, 4, code->size, f);

    /* Pad to aligned size */
    while (ftell(f) < (4096 + aligned_size)) fputc(0, f);

    fclose(f);

    /* Make executable */
#ifdef __APPLE__
    chmod(filename, 0755);
#endif
}

/* ================================================================== */
/* 6. PUBLIC API — called from Pascal or tests                        */
/* ================================================================== */

/*
 * kayte_compile_to_macho
 * ----------------------
 * Takes Kayte bytecode, translates to ARM64, writes Mach-O executable.
 *
 * Returns 0 on success, non-zero on error.
 */
int kayte_compile_to_macho(const kayte_insn_t *bytecode,
                           size_t              count,
                           const char         *output_path)
{
    if (!bytecode || !output_path) return -1;

    kayte_compiler_t kc;
    kc_init(&kc, 1024);

    kc_compile(&kc, bytecode, count);

    write_macho(output_path, &kc.code);

    kc_free(&kc);
    return 0;
}

/* ================================================================== */
/* 7. STANDALONE TEST / DEMO                                          */
/* ================================================================== */

#ifdef KAYTE_ARM64_TEST_MAIN

#include <sys/stat.h>

int main(void)
{
    /* Simple test program:
     *   push 40
     *   push 2
     *   add
     *   halt
     * Expected: exits with code 0 (the result 42 is in x19 but unused)
     */
    kayte_insn_t program[] = {
        { OP_PUSH_INT, 40 },
        { OP_PUSH_INT, 2 },
        { OP_ADD, 0 },
        { OP_HALT, 0 }
    };

    printf("Compiling Kayte bytecode to ARM64 Mach-O...\n");
    int ret = kayte_compile_to_macho(program, 4, "kayte_test");

    if (ret == 0) {
        printf("Success! Created 'kayte_test' executable.\n");
        printf("Run with:  ./kayte_test\n");
        return 0;
    } else {
        printf("Compilation failed.\n");
        return 1;
    }
}

#endif /* KAYTE_ARM64_TEST_MAIN */
