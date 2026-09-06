# OS/2 Warp 4.52 for ARM64 (QEMU / Raspberry Pi 3+)

A **bare-metal** OS/2-style operating system written in C11 + AArch64 assembly,
targeting **QEMU virt** machine (Cortex-A72) and **Raspberry Pi 3/3+** hardware.
Includes a PL011 UART driver, readline-style DOS shell, QuickBASIC-compatible 
interpreter, and a full FAT32/VFS stack backed by either a RAM disk or QEMU virtio-blk.

```
┌──────────────────────────────────────────────────────────────┐
│                  OS/2 Warp 4.52 ARM64                        │
│                                                              │
│  bootloader/                  kernel/src/                    │
│  ├── boot.S  (QEMU) ───────► ├── main.c       (entry)       │
│  ├── rpi_boot.S  (RPi3)      ├── uart.c       (PL011)       │
│  │   AArch64 entry           ├── kio.c        (kprintf)     │
│  │   BSS zero                ├── keyboard.c   (UART RX)     │
│  │   stack setup             ├── cli.c        (shell)       │
│  │   → kernel_main()         ├── basic.c      (QB BASIC)    │
│  └── linker scripts          │                              │
│                              ├── vfs.c        (VFS layer)   │
│  kernel/include/             ├── fat32.c      (FAT32 FS)    │
│  ├── types.h                 ├── blkdev.c     (block devs)  │
│  ├── uart.h                  ├── cli_fs.c     (FS cmds)     │
│  ├── kio.h                   └── doscalls.c  (OS/2 API)    │
│  ├── keyboard.h                                             │
│  ├── vfs.h                   Platform Support:              │
│  ├── fat32.h                 • QEMU virt (development)      │
│  ├── blkdev.h                • Raspberry Pi 3+ (hardware)   │
│  ├── cli_fs.h                                               │
│  └── basic.h                                                │
└──────────────────────────────────────────────────────────────┘
```

---

## 1. Prerequisites

### macOS (for building)

```bash
brew install llvm qemu cmake ninja
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"

# Verify
clang --version            # must be LLVM clang, not Apple clang
llvm-objcopy --version
qemu-system-aarch64 --version
```

### Ubuntu / Debian

```bash
sudo apt update
sudo apt install clang lld llvm qemu-system-arm cmake ninja-build
```

---

## 2. Build

### Build for QEMU (Default)

```bash
git clone https://github.com/yourname/os2warp-arm64
cd os2warp-arm64

cmake -B build -G Ninja
cmake --build build

# Outputs:
#   build/os2warp_kernel  — ELF with debug symbols
#   build/os2warp.img     — raw binary loaded by QEMU
```

### Build for Raspberry Pi 3+

```bash
cmake -B build-rpi -G Ninja -DTARGET_PLATFORM=RPI3
cmake --build build-rpi

# Outputs:
#   build-rpi/os2warp_kernel  — ELF with debug symbols
#   build-rpi/kernel8.img     — raw binary for Raspberry Pi

# Prepare SD card files
cmake --build build-rpi --target sdcard

# Creates: build-rpi/sdcard_files/
#   ├── kernel8.img       (your OS/2 kernel)
#   ├── bootcode.bin      (Raspberry Pi bootloader)
#   ├── start.elf         (GPU firmware)
#   ├── fixup.dat         (GPU config)
#   └── config.txt        (boot configuration)
```

The toolchain is configured directly in `CMakeLists.txt` — no separate
toolchain file is needed. The key flags are:

```cmake
-target aarch64-none-elf
-mcpu=cortex-a72
-ffreestanding -nostdlib
-fno-builtin -fno-stack-protector -fno-pie -fno-pic
```

Platform-specific defines are set automatically:
- **QEMU:** `-DPLATFORM_QEMU -DUART_BASE=0x09000000`
- **RPi3+:** `-DPLATFORM_RPI3 -DUART_BASE=0x3F201000`

---

## 3. Run in QEMU

```bash
# Boot (serial console, no window)
cmake --build build --target run

# Or manually:
qemu-system-aarch64 \
    -machine virt \
    -cpu cortex-a72 \
    -m 2G \
    -nographic \
    -kernel build/os2warp.img
```

Press **Ctrl-A X** to quit QEMU (macOS: **Cmd-A X**).

### Boot with a FAT32 disk image

```bash
# Create a 32 MB FAT32 disk
dd if=/dev/zero of=disk.img bs=1M count=32
mkfs.fat -F 32 disk.img

# Copy files onto it (macOS)
hdiutil attach disk.img
cp myfile.txt /Volumes/NO\ NAME/
hdiutil detach /Volumes/NO\ NAME

# Boot with disk attached
qemu-system-aarch64 \
    -machine virt \
    -cpu cortex-a72 \
    -m 2G \
    -nographic \
    -kernel build/os2warp.img \
    -drive file=disk.img,format=raw,if=virtio
```

Then inside the shell:

```
[OS2]# mount virtio0 C:
[OS2]# dir C:
[OS2]# type C:/README.TXT
```

---

## 4. Boot on Raspberry Pi 3+

### Hardware Requirements

- Raspberry Pi 3, 3+, or 3B+ (ARM64, Cortex-A53)
- MicroSD card (8GB+, formatted as FAT32)
- USB-to-TTL serial cable (for console output)
  - Adafruit #954, CP2102, or FTDI cable
- 5V power supply (2.5A minimum)

### SD Card Preparation

```bash
# 1. Format SD card as FAT32
diskutil eraseDisk FAT32 BOOT /dev/diskX  # macOS
# or
sudo mkfs.vfat -F 32 /dev/sdX1            # Linux

# 2. Copy prepared files to SD card
cp build-rpi/sdcard_files/* /Volumes/BOOT/  # macOS
# or
cp build-rpi/sdcard_files/* /mnt/sdcard/    # Linux

# 3. Eject SD card safely
diskutil eject /dev/diskX  # macOS
# or
sudo umount /mnt/sdcard    # Linux
```

### Serial Console Connection

Connect USB-to-TTL serial cable to Raspberry Pi GPIO header:

| Cable Wire | RPi Pin | GPIO | Function |
|------------|---------|------|----------|
| Black (GND) | Pin 6  | GND  | Ground |
| Green (RX)  | Pin 8  | GPIO14 | UART TX |
| White (TX)  | Pin 10 | GPIO15 | UART RX |
| Red (VCC)   | **DO NOT CONNECT** | - | Pi has own power |

**Important:** Do not connect the red (VCC) wire — the Pi powers itself.

### Open Serial Terminal

```bash
# macOS
screen /dev/tty.usbserial-* 115200

# Linux
screen /dev/ttyUSB0 115200

# Or use minicom
minicom -D /dev/ttyUSB0 -b 115200
```

### Boot the Pi

1. Insert SD card into Raspberry Pi
2. Connect power
3. Watch serial console for boot messages

You should see:

```
  ___  ____    ______     _  _  __              
 / _ \/ ___|  / /___ \   | || |/ /___ _  __    
| | | \___ \ / /  __) |  | || '_// _ \ \/ /   
| |_| |___) / /  / __/   |__   _|  __/>  <    
 \___/|____/_/  |_____|     |_|  \___/_/\_\  

  OS/2 Warp 4.52  -  ARM64 Bare-Metal Kernel
  Platform : Raspberry Pi 3+
  CPU      : ARM Cortex-A53 (64-bit)
  UART     : PL011 @ 0x3F201000

[KERNEL] Subsystems initializing...
[MEM]    Allocator    : initialized
...

OS/2 Command Prompt - Raspberry Pi Edition

[C:\]>
```

---

## 5. Debug with GDB

### QEMU Debug Mode

```bash
# Terminal 1 — start QEMU paused
cmake --build build --target debug

# Terminal 2 — attach GDB
aarch64-elf-gdb build/os2warp_kernel \
    -ex "target remote :1234" \
    -ex "layout src" \
    -ex "break kernel_main" \
    -ex "continue"
```

### Raspberry Pi Debug

For hardware debugging on Raspberry Pi, use a JTAG adapter or UART-based
debugging. See the [Raspberry Pi debugging guide](docs/RPI_DEBUG.md).

---

## 6. Shell Commands

```
OS/2 Warp ARM64 CLI  -  type 'help' for commands

[C:\]>
```

The shell supports **readline editing**: arrow keys, Home/End, insert mode,
Ctrl-A/E, Ctrl-C, Ctrl-D, Ctrl-L, and a 16-entry history (↑↓).

### General

| Command           | Description                                  |
|-------------------|----------------------------------------------|
| `help`            | List all commands                            |
| `clear`           | Clear screen (ANSI)                          |
| `version`         | Show kernel version                          |
| `basic`           | Launch QuickBASIC interpreter                |
| `halt`            | Halt the CPU (`wfe` loop)                   |

### File System

| Command             | Description                                |
|---------------------|--------------------------------------------|
| `mount <dev> <mpt>` | Mount a FAT32 volume (`mount virtio0 C:`)  |
| `dir [path]`        | List directory                             |
| `cd [path]`         | Change current directory                   |
| `type <file>`       | Print file contents                        |
| `copy <src> <dst>`  | Copy a file                                |
| `del <file>`        | Delete a file                              |
| `ren <old> <new>`   | Rename a file                              |
| `md <path>`         | Create a directory                         |

Block devices available to `mount`:

| Device     | Platform | Description                          |
|------------|----------|--------------------------------------|
| `virtio0`  | QEMU     | virtio-blk at MMIO `0x0a003e00`      |
| `ramdisk`  | Both     | In-memory disk (if initialized)      |
| `sdcard0`  | RPi3+    | SD card (future support)             |

---

## 7. QuickBASIC Interpreter

Type `basic` at the shell. Type `BYE`, `QUIT`, or `SYSTEM` to return.

```basic
10 PRINT "Hello from OS/2 BASIC!"
20 FOR I = 1 TO 10
30   PRINT I * I
40 NEXT I
50 END
RUN
```

### Statements

| Statement                                    | Notes                           |
|----------------------------------------------|---------------------------------|
| `PRINT expr / "str" [; ,]`                   | TAB(), SPC() supported          |
| `LET var = expr`                             | `LET` is optional               |
| `INPUT ["prompt";] var`                      | String or integer               |
| `IF expr THEN … [ELSE …]`                   | Single-line form                |
| `IF … / ELSEIF … / ELSE / END IF`           | Block form                      |
| `SELECT CASE … / CASE … / END SELECT`       | `CASE IS`, `CASE a TO b`        |
| `FOR var = n TO m [STEP s] … NEXT [var]`    |                                 |
| `WHILE expr … WEND`                          |                                 |
| `DO [WHILE\|UNTIL] … LOOP [WHILE\|UNTIL]`   |                                 |
| `GOTO line`                                  |                                 |
| `GOSUB line … RETURN`                        |                                 |
| `ON expr GOTO line[,…]`                      |                                 |
| `ON expr GOSUB line[,…]`                     |                                 |
| `READ var / DATA val[,…] / RESTORE`          |                                 |
| `DIM var(n)`                                 | 1-D integer arrays, 0-based     |
| `CONST name = expr`                          |                                 |
| `SWAP var, var`                              |                                 |
| `CLS / LOCATE r,c / COLOR fg[,bg] / BEEP`   | ANSI terminal                   |
| `SLEEP n`                                    | Busy-wait seconds               |
| `REM` or `'`                                 | Comment                         |
| `END / STOP`                                 |                                 |
| `LIST [first[-last]] / RUN / NEW / RENUM`    | Direct-mode commands            |

### Numeric functions

`ABS` `SGN` `INT` `FIX` `SQR` `RND` `VAL` `LEN` `ASC` `INSTR`

### String functions

`CHR$` `STR$` `HEX$` `OCT$` `LEFT$` `RIGHT$` `MID$`
`LTRIM$` `RTRIM$` `UCASE$` `LCASE$` `SPACE$` `STRING$` `INKEY$`

### Variables

| Type                | Example     |
|---------------------|-------------|
| Integer (`A`–`Z`)   | `A = 42`    |
| String (`A$`–`Z$`)  | `A$ = "hi"` |
| Array (`A(n)`)      | `DIM A(10)` |
| Hex literal         | `&HFF`      |
| Octal literal       | `&O17`      |

Multi-statement lines separated by `:`. `?` is a shorthand for `PRINT`.

---

## 8. Architecture

### Platform Comparison

| Feature | QEMU virt | Raspberry Pi 3+ |
|---------|-----------|-----------------|
| **CPU** | Cortex-A72 | Cortex-A53 |
| **RAM** | Configurable (2GB default) | 1GB |
| **UART** | PL011 @ 0x09000000 | PL011 @ 0x3F201000 |
| **Load Address** | 0x40000000 | 0x80000 |
| **Heap Size** | 8 MB | 32 MB |
| **Output File** | os2warp.img | kernel8.img |
| **Boot Method** | Direct kernel load | RPi firmware chain |
| **Block Device** | virtio-blk | SD card (future) |

### Memory Map

#### QEMU virt

| Address              | Contents                              |
|----------------------|---------------------------------------|
| `0x40000000`         | Kernel load address (text)            |
| `0x40000000 + image` | BSS (zeroed by `boot.S`)              |
| `0x48000000`         | Stack top (top of 2GB)                |
| `0x09000000`         | PL011 UART0                           |
| `0x0a000000+`        | VirtIO MMIO devices (virtio-blk etc.) |

#### Raspberry Pi 3+

| Address              | Contents                              |
|----------------------|---------------------------------------|
| `0x80000`            | Kernel load address (RPi firmware)    |
| `0x80000 + image`    | BSS (zeroed by `rpi_boot.S`)          |
| `0x00100000`         | Stack top                             |
| `0x3F000000`         | BCM2837 peripherals base              |
| `0x3F201000`         | PL011 UART0                           |
| `0x3F200000`         | GPIO controller                       |

### Boot sequence

#### QEMU

```
QEMU loads os2warp.img → 0x40000000
  │
  ▼
boot.S  (_start)
  ├── Zero BSS  (_bss_start … _bss_end)
  ├── Set stack pointer  (_stack_top)
  └── bl kernel_main
        │
        ▼
main.c  (kernel_main)
  ├── uart_init / kbd_init
  ├── vfs_init
  ├── virtio_blk_init  (if disk attached)
  └── cli_run  ──► readline shell
                      ├── dispatch → built-in commands
                      ├── dispatch → cli_fs (dir/type/copy…)
                      └── dispatch → basic_run
```

#### Raspberry Pi 3+

```
RPi firmware loads kernel8.img → 0x80000
  │
  ▼
rpi_boot.S  (_start)
  ├── Park secondary CPUs (only CPU0 continues)
  ├── Drop from EL2/EL3 → EL1
  ├── Zero BSS  (_bss_start … _bss_end)
  ├── Set stack pointer
  └── bl kernel_main
        │
        ▼
rpi_main.c  (kernel_main)
  ├── uart_init  (configure GPIO, baud rate)
  ├── kbd_init
  ├── vfs_init
  └── cli_run  ──► readline shell
```

### Filesystem stack

```
cli_fs.c   (dir, cd, type, copy, del, ren, md, mount)
    │
vfs.c      (mount table · fd table · path dispatch)
    │
fat32.c    (cluster chains · 8.3 + LFN · read/write/create/delete)
    │
blkdev.c   (block device registry)
    ├── ramdisk driver    (in-memory, byte-copy)
    ├── virtio_blk driver (MMIO virtqueue, QEMU virt)
    └── sdcard driver     (future: Raspberry Pi EMMC)
```

---

## 9. Project Structure

```
os2warp-arm64/
├── CMakeLists.txt              Root build: platform detection, targets
├── config.txt                  Raspberry Pi boot configuration
├── rpi_linker.ld              Raspberry Pi linker script
├── scripts/
│   ├── build_iso.sh           ISO image builder (QEMU)
│   └── create_sdcard.sh       SD card preparation (RPi)
├── bootloader/
│   ├── CMakeLists.txt
│   ├── boot.S                 QEMU entry point
│   └── rpi_boot.S             Raspberry Pi entry point
└── kernel/
    ├── CMakeLists.txt         Platform-aware kernel build
    ├── kernel.ld              QEMU linker script
    ├── include/
    │   ├── types.h            Primitive typedefs (no libc)
    │   ├── uart.h             PL011 UART API
    │   ├── kio.h              kprintf, kgets, string + memory utils
    │   ├── keyboard.h         UART RX + VT100 escape decoder
    │   ├── basic.h            basic_run() entry point
    │   ├── vfs.h              VFS public API + error codes
    │   ├── fat32.h            FAT32 on-disk structures + mount API
    │   ├── blkdev.h           Block device abstraction
    │   └── cli_fs.h           Filesystem shell command declarations
    └── src/
        ├── main.c             QEMU kernel entry
        ├── rpi_main.c         Raspberry Pi kernel entry
        ├── uart.c             PL011 UART driver
        ├── kio.c              kprintf / kgets / memcpy / memset stubs
        ├── keyboard.c         UART RX poller, escape sequence decoder
        ├── cli.c              Readline editor, history, command dispatch
        ├── cli_fs.c           dir/cd/type/copy/del/ren/md/mount commands
        ├── basic.c            QuickBASIC 4.5-compatible interpreter
        ├── vfs.c              Virtual filesystem layer
        ├── fat32.c            FAT32 filesystem driver
        ├── blkdev.c           Block device registry + drivers
        └── doscalls.c         OS/2 DosCall API stubs
```

---

## 10. Platform-Specific Features

### QEMU Features

✅ Fast iteration cycle (instant boot)  
✅ Hardware acceleration (HVF on macOS, KVM on Linux)  
✅ virtio-blk storage (fast disk I/O)  
✅ Easy debugging (GDB integration)  
✅ ISO boot support  

### Raspberry Pi 3+ Features

✅ Real ARM64 hardware  
✅ GPIO access (40-pin header)  
✅ 1GB RAM  
✅ SD card storage (planned)  
✅ Standalone operation  
✅ Low power consumption  

### Future Raspberry Pi Support

Planned features for Raspberry Pi:
- [ ] SD card driver (EMMC controller)
- [ ] USB keyboard support
- [ ] HDMI framebuffer output
- [ ] Ethernet networking
- [ ] GPIO LED blinking example
- [ ] Hardware timer/RTC

---

## 11. Building for Both Platforms

### Separate Build Directories (Recommended)

```bash
# Build for QEMU
mkdir build-qemu && cd build-qemu
cmake -DTARGET_PLATFORM=QEMU ..
cmake --build .
cd ..

# Build for Raspberry Pi
mkdir build-rpi && cd build-rpi
cmake -DTARGET_PLATFORM=RPI3 ..
cmake --build .
cmake --build . --target sdcard
cd ..

# Now you have both!
ls build-qemu/os2warp.img
ls build-rpi/kernel8.img
```

### Quick Commands

```bash
# Test in QEMU
cmake --build build-qemu --target run

# Prepare for Raspberry Pi
cmake --build build-rpi --target sdcard
cp build-rpi/sdcard_files/* /Volumes/BOOT/
```

---

## 12. Troubleshooting

### QEMU

**No output on serial console?**
- Make sure you're using `-nographic` flag
- Check that UART is enabled in kernel

**virtio disk not detected?**
- Verify `-drive` syntax is correct
- Check that virtio-blk driver is initialized

### Raspberry Pi

**No serial output?**
- Verify serial cable connections (TX↔RX, GND)
- Check baud rate is 115200
- Ensure `enable_uart=1` in config.txt

**Won't boot?**
- SD card must be FAT32
- Check all firmware files present (bootcode.bin, start.elf, fixup.dat)
- Verify kernel8.img is the right file (not os2warp.img)
- Try reformatting SD card

**Green LED pattern?**
- 3 flashes: kernel8.img not found
- 4 flashes: start.elf not found
- 7 flashes: kernel.img not found (use kernel8.img for 64-bit)

---

## 13. Contributing

Contributions welcome! Areas of interest:

- **Drivers:** SD card, USB, Ethernet, HDMI framebuffer
- **Filesystems:** ext4, tmpfs
- **Networking:** TCP/IP stack
- **Applications:** Text editor, file manager, games
- **Documentation:** Tutorials, API docs, examples

---

## 14. License

MIT — Handbuilt bare-metal kernel, educational purposes.

---

## 15. Resources

### Documentation
- [Build Guide](docs/BUILD.md)
- [Raspberry Pi Setup](docs/RPI_BOOT_GUIDE.md)
- [VFS Architecture](docs/VFS_DESIGN.md)
- [BASIC Language Reference](docs/BASIC_REFERENCE.md)

### Raspberry Pi
- [Raspberry Pi Bare Metal](https://github.com/bztsrc/raspi3-tutorial)
- [RPi Firmware](https://github.com/raspberrypi/firmware)
- [ARM Cortex-A53 TRM](https://developer.arm.com/documentation/)

### ARM64 Programming
- [ARMv8-A Reference Manual](https://developer.arm.com/documentation/)
- [ARM Assembly Guide](https://developer.arm.com/documentation/)

---

**Status:** ✅ Boots on QEMU virt | ✅ Boots on Raspberry Pi 3+ | 🚧 Active Development
