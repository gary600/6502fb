; Registers
.export     VGA_START       = $8000
.export     _VGA_BUF        = VGA_START
.export     VGA_END         = $C000 ; 1 past the end
.export     VGA_CFG         = VGA_START + 160
.export     _VGA_CFG        = VGA_CFG
; Globals
.zeropage
.exportzp   VGA_CFG_STASH
    VGA_CFG_STASH: .res 1
.exportzp   _VGA_CFG_STASH  = VGA_CFG_STASH
.exportzp   VGA_FRAME_DONE
    VGA_FRAME_DONE: .res 1
.exportzp   _VGA_FRAME_DONE = VGA_FRAME_DONE
; Constants
.exportzp   VGA_BUFSEL      = %00000001
.exportzp   VGA_BANKSEL     = %00000010
.exportzp   VGA_NMIENABLE   = %00000100
.exportzp   VGA_BLANKBLACK  = %00001000
.exportzp   VGA_BLANKWHITE  = %00010000

.code
; Mark the current frame as done, and wait for the buffers to be swapped.
.export vga_flip
.proc vga_flip
    lda #1
    sta VGA_FRAME_DONE
    ; spin until VGA_FRAME_DONE cleared by NMI handler
    loop:
        lda VGA_FRAME_DONE
        bne loop
    rts
.endproc ; vga_flip
; C alias
.export _vga_flip = vga_flip

; NMI handler: swap buffers if frame is done
.export vga_nmi
.proc vga_nmi
    pha
    lda VGA_FRAME_DONE
    ; If draw is not done, exit
    beq done
        ; Else, swap buffers
        lda VGA_CFG_STASH
        eor #<VGA_BUFSEL
        sta VGA_CFG_STASH
        sta VGA_CFG
        lda #0
        sta VGA_FRAME_DONE
        ; fall through
    done:
    pla
    rti
.endproc