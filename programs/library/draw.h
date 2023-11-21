#ifndef _DRAW_H_
#define _DRAW_H_

#include <stdint.h>

void __fastcall__ draw_rect_filled(uint8_t x, uint8_t y, uint8_t width, uint8_t height, uint8_t color);
void __fastcall__ draw_line(uint8_t x0, uint8_t y0, uint8_t x1, uint8_t y1, uint8_t color);
void __fastcall__ draw_circle(uint8_t x, uint8_t y, uint8_t radius, uint8_t color);

#endif // _DRAW_H_