#include "library/draw.h"
#include "library/vga.h"

void main() {
    VGA_CFG_STASH = 0;
    VGA_CFG = 0;
    draw_rect_filled(10, 10, 10, 10, 0xFF);
    vga_flip();
    while (1);
}