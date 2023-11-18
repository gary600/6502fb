  processor 6502

  org $E000

VGA_START = $8000
VGA_END = $C000 ; 1 past the end
; Config register is at column 160 in any row
VGA_CFG = [VGA_START+160]
; Variable storage for the config register (since the actual register is write-only)
CFG_STASH = $FF
FRAME_DONE = $FE
; Config bits
CFG_BUFSEL     = %00000001
CFG_BANKSEL    = %00000010
CFG_NMIENABLE  = %00000100
CFG_BLANKBLACK = %00001000
CFG_BLANKWHITE = $00010000

; Draw a filled rectangle.
; TODO: account for banks
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
  subroutine
draw_rect_filled:
  ; Make sure we're in bank 0
  jsr set_bank0
  ; Load topleft address
  lda _COL
  sta _IND
  lda _ROW
  clc
  adc #>VGA_START
  sta [_IND+1]
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
  ldy [_IND+1]
  cpy #>VGA_END
  bmi no_swap_bank
  jsr set_bank1
  ldy #>VGA_START
  sty [_IND+1]
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
  inc [_IND+1]
  dec _HEIGHT
  bne row_loop
; end row_loop
done:
  rts

; Safe-call
  subroutine
set_bank0:
  pha
  lda CFG_STASH
  and #[~CFG_BANKSEL]
  sta CFG_STASH
  sta VGA_CFG
  pla
  rts

; Safe-call
  subroutine
set_bank1:
  pha
  lda CFG_STASH
  ora #CFG_BANKSEL
  sta CFG_STASH
  sta VGA_CFG
  pla
  rts
  
  subroutine
nmi:
  pha
  lda FRAME_DONE
  ; If draw is not done, exit
  beq .done
  ; Else, swap buffers
  lda CFG_STASH
  eor CFG_BUFSEL
  sta CFG_STASH
  sta VGA_CFG
  lda #0
  sta FRAME_DONE
  ; fall through
.done:
  pla

  subroutine
irq:
  rti
  
; Mark the current frame as done, and wait for the
; frames to be swapped.
  subroutine
flip:
  lda #1
  sta FRAME_DONE
  ; loop until FRAME_DONE cleared by interrupt
.loop:
  lda FRAME_DONE
  bne .loop
  rts
  
main:
  lda #CFG_NMIENABLE
  sta CFG_STASH
  sta VGA_CFG

  lda #63
  sta _COL
  sta _ROW
  lda #16
  sta _WIDTH
  sta _HEIGHT
  ldx #$FF
  jsr draw_rect_filled
  jsr flip
  lda #63
  sta _COL
  sta _ROW
  lda #16
  sta _WIDTH
  sta _HEIGHT
  ldx #$FF
  jsr draw_rect_filled
  jsr flip
halt:
  jmp halt
  
  org $FFFA
ivt:
  dc.w nmi
  dc.w main
  dc.w irq