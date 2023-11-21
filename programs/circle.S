  processor 6502

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

; TODO: account for banks
; Parameters:
; X   -> color
; $00 -> starting column
; $01 -> starting row
; $02 -> width
; $03 -> height

  SEG.U zp_variables
  ORG $0000

temp_1: DC.B 0
temp_2: DC.B 0

radius: DC.B 0

circ_x: DC.B 0
circ_y: DC.B 0

circ_center_x: DC.B 0
circ_center_y: DC.B 0

_BANK: DC.B 0 ; byte

_IND: DC.W 0

  SEG text
  ORG $E000

  SUBROUTINE

draw_circle:
  lda radius
  sta temp_1
  sta circ_x
  lda #0
  sta circ_y

  ldy #0

  ; repeat until x < y

.loop:
  ; plot x, y
  lda circ_center_x
  clc
  adc circ_x
  sta [_IND+0]
  lda circ_center_y
  clc
  adc circ_y
  ora #[>VGA_START]
  sta [_IND+1]
  lda #$FF
  sta (_IND),y

  ; plot x, -y
  lda circ_center_y
  sec
  sbc circ_y
  ora #[>VGA_START]
  sta [_IND+1]
  lda #$E0
  sta (_IND),y

  ; plot -x, -y
  lda circ_center_x
  sec
  sbc circ_x
  sta [_IND+0]
  lda #$1C
  sta (_IND),y

  ; plot -x, y
  lda circ_center_y
  clc
  adc circ_y
  ora #[>VGA_START]
  sta [_IND+1]
  lda #$03
  sta (_IND),y

  ; plot x, y
  lda circ_center_x
  clc
  adc circ_y
  sta [_IND+0]
  lda circ_center_y
  clc
  adc circ_x
  ora #[>VGA_START]
  sta [_IND+1]
  lda #$FF
  sta (_IND),y

  ; plot x, -y
  lda circ_center_y
  sec
  sbc circ_x
  ora #[>VGA_START]
  sta [_IND+1]
  lda #$E0
  sta (_IND),y

  ; plot -x, -y
  lda circ_center_x
  sec
  sbc circ_y
  sta [_IND+0]
  lda #$1C
  sta (_IND),y

  ; plot -x, y
  lda circ_center_y
  clc
  adc circ_x
  ora #[>VGA_START]
  sta [_IND+1]
  lda #$03
  sta (_IND),y

  ; y <- y + 1
  inc circ_y

  ; t1 <- t1 + y
  ; t2 <- t1 - x
  ; if t2 < 0 skip
  lda temp_1
  clc
  adc circ_y
  sta temp_1
  sec
  sbc circ_x
  bmi .no_decr_x

.do_decr_x:
  ; t1 <- t2
  sta temp_1

  ; x <- x - 1
  dec circ_x

.no_decr_x:

  lda circ_x
  cmp circ_y
  bpl .trampoline

.done:
  rts

.trampoline:
  jmp .loop


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
  eor #[CFG_BUFSEL]
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

  lda #30
  sta circ_center_x
  sta circ_center_y

  lda #23
  sta radius

  ldx #$FF
  jsr draw_circle
  jsr flip
halt:
  jmp halt
  
  org $FFFA
ivt:
  dc.w nmi
  dc.w main
  dc.w irq