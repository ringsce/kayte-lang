// n64.c - Custom N64 support layer for Free Pascal integration
// Compile with: mips64-elf-gcc -c n64.c -o n64.o
// Archive with: mips64-elf-ar rcs n64.a n64.o

#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <libdragon.h>  // Only needed if you're mixing with libdragon initially

void n64_init_display() {
    display_init(RESOLUTION_320x240, DEPTH_16_BPP, 2);
}

void n64_init_controller() {
    controller_init();
}

void n64_enable_interrupts() {
    irq_init();
    irq_enable();
}

void n64_console_print(const char *msg) {
    console_init();
    printf("%s\n", msg);
}

int n64_controller_pressed(int button) {
    controller_scan();
    struct controller_data c = get_keys_down();
    return c.c[0].buttons & button;
}

