.include "vga.inc"

.pc02
.code

FRAME_DONE = $FE

; Draw a filled rectangle.
; Parameters:
; X   -> color
; $00 -> starting column
; $01 -> starting row
; $02 -> width
; $03 -> height
_COL = $00
_ROW = $01
_WIDTH = $02
_HEIGHT = $03
; Locals
_IND = $04 ; word
_BANK = $06 ; byte

.code
.proc draw_rect_filled
    ; Make sure we're in bank 0
    vga_cfg_clr VGA_BANKSEL
    ; Load topleft address
    lda _COL
    sta _IND
    lda _ROW
    clc
    adc #>VGA_START
    sta _IND + 1
    ; Start in bank 0
    lda #0
    sta _BANK
    ; Move color into A for writing
    txa
    ; Remember starting width for later
    ldx _WIDTH

    ; Loop over rows or skip if none
    ldy _HEIGHT
    beq done
    row_loop:
        ; If not in bank 1...
        ldy _BANK
        bne no_swap_bank
        ; Check if we're past the end of bank 0, if so go to bank 1
        ldy _IND + 1
        cpy #>VGA_END
        bmi no_swap_bank
            pha
            vga_cfg_set VGA_BANKSEL
            pla
            ldy #>VGA_START
            sty _IND + 1
            sty _BANK ; just a boolean so don't care about value
        no_swap_bank:
    
        ; Loop over cols or skip if none
        ldy _WIDTH
        beq done
        col_loop:

            ; Write pixel
            ldy #0
            sta (_IND),y
            
            ; Next col
            inc _IND
            dec _WIDTH
            bne col_loop
        ; end col_loop
        ; Reset col to initial
        ldy _COL
        sty _IND
        stx _WIDTH
        
        ; Next row
        inc _IND + 1
        dec _HEIGHT
        bne row_loop
    ; end row_loop
    done:
  rts

.endproc ; draw_rect_filled
; C wrapper
.proc _draw_rect_filled
    ; TODO:
    jmp draw_rect_filled
.endproc