.include "library/vga.inc"
.import _main

.pc02

.segment "VECTORS"
ivt:
    .word vga_nmi   ; NMI
    .word reset     ; RESET
    .word irq       ; IRQ

.code
.proc reset
    ; TODO: clear .bss, copy .data
    jsr _main
halt:
    brk
    jmp halt
.endproc ; reset

; .proc nmi
;     rti
; .endproc ; nmi

.proc irq
    rti
.endproc ; irq