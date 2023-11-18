	PROCESSOR 6502

; Usage: add_16 dst, lhs, rhs
; lhs and rhs must be constant addresses pointing to 16-bit values.
; Computes dst <- lhs + rhs
; Clobbers the A register and the status flags.
	MACRO add_16
		clc
		lda [{2}]+0
		adc [{3}]+0
		sta [{1}]+0
		lda [{2}]+1
		adc [{3}]+1
		sta [{1}]+1
	ENDM

; Usage: sub_16 dst, lhs, rhs
; lhs and rhs must be constant addresses pointing to 16-bit values.
; Computes dst <- lhs - rhs
; Clobbers the A register and the status flags.
	MACRO sub_16
		sec
		lda [{2}]+0
		sbc [{3}]+0
		sta [{1}]+0
		lda [{2}]+1
		sbc [{3}]+1
		sta [{1}]+1
	ENDM


	SEG.U		variables
	ORG $0000

line_x0:	DC.W 0
line_y0:	DC.W 0

line_x1:	DC.W 0
line_y1:	DC.W 0

line_dx:	DC.W 0
line_dy:	DC.W 0
line_err:	DC.W 0

; The current point being examined
line_x:		DC.W 0
line_y:		DC.W 0

line_x_inc:	DC.W 0

addr: DC.W 0

; ========================================

	SEG		text
	ORG $E000

start:
  lda #10
  sta line_x1
  sta line_y1
	jsr draw_line
halt:
	jmp halt

	SUBROUTINE

; Draws a line from (x0, y0) to (x1, y1)
draw_line:

	; dx  <- x1 - x0
	sub_16 line_dx, line_x1, line_x0

	; dy  <- y1 - y0
	sub_16 line_dy, line_y1, line_y0

	; dy <- dy * 2
	asl line_dy+0
	rol line_dy+1 

	; err <- 2 * dy - dx
	sub_16 line_err, line_dy, line_dx

	; dx <- dx * 2
	asl line_dx+0
	rol line_dx+1

	; y <- y0
	lda line_y0
	sta line_y
	lda line_y0+1
	sta line_y +1

	; x <- x0
	lda line_x0
	sta line_x
	lda line_x0+1
	sta line_x +1

	; now loop over all x from x0 (inclusive) to x1 (exclusive)

.loop:
	; if x == x1, stop
	lda line_x
	cmp line_x1
	bne .not_finished
	lda line_x +1
	cmp line_x1+1
	beq .finished

.not_finished:
	jsr plot_point

	; if err > 0
	; 	do the increment

	lda line_err+1
	bmi .no_increment_y
.do_increment_y:
	; y   <- y + 1
	; err <- err - 2 * dx

	clc
	lda line_y
	adc #1
	sta line_y
	lda line_y+1
	adc #0
	sta line_y+1

	sub_16 line_err, line_err, line_dx

.no_increment_y:
	; err <- err + 2 * dy

	add_16 line_err, line_err, line_dy
	
	; x <- x + 1
	clc
	lda line_x
	adc #1
	sta line_x
	lda line_x+1
	adc #0
	sta line_x+1

	jmp .loop

.finished:
	rts

	SUBROUTINE

; Plots the point currently at line_x, line_y
plot_point:
  lda line_x
  sta addr
  lda line_y
  clc
  adc #$80
  sta [addr+1]
  ldy #0
  lda #$FF
  sta (addr),y
	rts



; ======================== ;
; Constants and other data ;
; ======================== ;

my_string:
	DC.B "Hello, world!", 0

	ORG $FFFA
vector_table:
	DC.W start			; NMI
	DC.W start			; RESET
	DC.W start			; IRQ
