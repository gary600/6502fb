.ifndef _VGA_INC_
.define _VGA_INC_

; Registers
.import     VGA_START
.import     VGA_END
.import     VGA_CFG
; Globals
.importzp   VGA_CFG_STASH
.importzp   VGA_FRAME_DONE
; Constants
.importzp   VGA_BUFSEL
.importzp   VGA_BANKSEL
.importzp   VGA_NMIENABLE
.importzp   VGA_BLANKBLACK
.importzp   VGA_BLANKWHITE
; Subroutines
.import     vga_set_bank0
.import     vga_set_bank1
.import     vga_flip
.import     vga_nmi

.macro vga_cfg_set bits
    lda VGA_CFG_STASH
    ora #<bits
    sta VGA_CFG_STASH
    sta VGA_CFG
.endmacro

.macro vga_cfg_clr bits
    lda VGA_CFG_STASH
    and #<~bits
    sta VGA_CFG_STASH
    sta VGA_CFG
.endmacro

.endif ; _VGA_INC_