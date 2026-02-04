{
  KayteArm64.pas
  -------------
  Free Pascal unit that declares the ARM64 native code compiler.
  The Kayte compiler/VM includes this to emit native Mach-O executables.
  Build:
    1. Compile the C side:
       clang -c -O2 -std=c11 kayte_arm64_emit.c -o kayte_arm64_emit.o
    2. Link with your Kayte project:
       Add to your .lpi or use:
         {$LINKOBJ kayte_arm64_emit.o}
}
unit KayteArm64;

{$mode objfpc}
{$H+}
{$modeswitch advancedrecords}

interface

uses
  ctypes;

{ ------------------------------------------------------------------ }
{ Kayte bytecode opcode enum (must match C side exactly)             }
{ ------------------------------------------------------------------ }
type
  TKayteOpcode = (
    OP_HALT         = $00,
    OP_NOP          = $01,
    OP_PUSH_INT     = $10,
    OP_POP          = $11,
    OP_DUP          = $12,
    OP_ADD          = $20,
    OP_SUB          = $21,
    OP_MUL          = $22,
    OP_DIV          = $23,
    OP_CMP_EQ       = $30,
    OP_CMP_LT       = $31,
    OP_CMP_GT       = $32,
    OP_JMP          = $40,
    OP_JZ           = $41,
    OP_JNZ          = $42,
    OP_CALL         = $50,
    OP_RET          = $51,
    OP_CALL_NATIVE  = $52,
    OP_LOAD_LOCAL   = $60,
    OP_STORE_LOCAL  = $61,
    OP_LOAD_GLOBAL  = $62,
    OP_STORE_GLOBAL = $63,
    OP_PRINT        = $70
  );

{ ------------------------------------------------------------------ }
{ Bytecode instruction record (matches kayte_insn_t in C)            }
{ ------------------------------------------------------------------ }
type
  TKayteInsn = packed record
    op  : cint32;   { TKayteOpcode as int32 }
    pad : cint32;   { alignment padding     }
    arg : cint64;   { immediate or offset   }
  end;
  PKayteInsn = ^TKayteInsn;

{ ------------------------------------------------------------------ }
{ Pascal-level convenience wrappers                                  }
{ ------------------------------------------------------------------ }
{ Build a TKayteInsn from Pascal values }
function MakeInsn(op: TKayteOpcode; arg: cint64): TKayteInsn;

{
  CompileToMachO
  --------------
  High-level wrapper:
    - Accepts dynamic array of TKayteInsn
    - Converts to C-compatible pointer
    - Calls the native compiler
    - Returns 0 on success
}
function CompileToMachO(const bytecode: array of TKayteInsn;
                        const output: string): Integer;

implementation

uses
  SysUtils;

{ ------------------------------------------------------------------ }
{ Platform-specific linking                                          }
{ ------------------------------------------------------------------ }
{$IFDEF DARWIN}
  {$IFDEF CPUAARCH64}
    // Link the object file from parent directory
    {$L ../kayte_arm64_emit.o}

    // External C function - note the underscore prefix for macOS
    function kayte_compile_to_macho(
               bytecode    : PKayteInsn;
               count       : csize_t;
               output_path : PAnsiChar
             ) : cint; cdecl; external name '_kayte_compile_to_macho';
  {$ELSE}
    {$FATAL ARM64 native compilation requires aarch64 CPU}
  {$ENDIF}
{$ELSE}
  {$FATAL ARM64 native compilation is only available on macOS}
{$ENDIF}

{ ------------------------------------------------------------------ }
{ Helper: build a TKayteInsn                                         }
{ ------------------------------------------------------------------ }
function MakeInsn(op: TKayteOpcode; arg: cint64): TKayteInsn;
begin
  Result.op  := cint32(Ord(op));
  Result.pad := 0;
  Result.arg := arg;
end;

{ ------------------------------------------------------------------ }
{ CompileToMachO — high-level wrapper                               }
{ ------------------------------------------------------------------ }
function CompileToMachO(const bytecode: array of TKayteInsn;
                        const output: string): Integer;
var
  count: csize_t;
  ptr: PKayteInsn;
  outputCStr: AnsiString;
begin
  count := csize_t(Length(bytecode));

  if count > 0 then
    ptr := @bytecode[Low(bytecode)]
  else
    ptr := nil;

  // Convert to null-terminated C string
  outputCStr := AnsiString(output);

  Result := kayte_compile_to_macho(ptr, count, PAnsiChar(outputCStr));
end;

end.
