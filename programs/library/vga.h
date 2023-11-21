#ifndef _VGA_H_
#define _VGA_H_

#include <stdint.h>

extern volatile uint8_t VGA_CFG; // write-only
extern volatile uint8_t VGA_BUF[0x2000]; // write-only
extern volatile uint8_t VGA_CFG_STASH;
#pragma zpsym("VGA_CFG_STASH")
extern volatile uint8_t VGA_FRAME_DONE;
#pragma zpsym("VGA_FRAME_DONE")

#define VGA_CFG_SET(_bits) { \
    VGA_CFG_STASH |= _bits; \
    VGA_CFG = VGA_CFG_STASH; \
}

#define VGA_CFG_CLR(_bits) { \
    VGA_CFG_STASH &= ~_bits; \
    VGA_CFG = VGA_CFG_STASH; \
}

#define VGA_BUFSEL     0b00000001
#define VGA_BANKSEL    0b00000010
#define VGA_NMIENABLE  0b00000100
#define VGA_BLANKBLACK 0b00001000
#define VGA_BLANKWHITE 0b00010000

void __cdecl__ vga_flip(void);

#endif ; _VGA_H_