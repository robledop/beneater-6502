; 6502 and AT28C256 EEPROM
; ========================
;
; The 6502 has 16 address bits, which means it can access address that go from 0x0000 up to 0xffff. 
; That is 64KB of addresses.
; The AT28C256 has 256 kbits of memory (32KB), half of what the 6502 can address.
; We wouldn't want to use all the addresses of the 6502 just to access the EEPROM anyway.
; 16 bits are necessary to access 64KB, but only 15 are needed for 32KB, so the EEPROM only has 15 address bits.
; If we just connected the last 15 address lines of the 6502 to the 15 address lines of the EEPROM, 
; the 6502 would read addresses 0x0000 all the way up to 0x0000 to 0x7fff (the first 32KB) from the EEPROM,
; but then the addresses from 0x8000 up to 0xffff (the last 32KB) would also be read from the EEPROM, 
; but it would be as if they were repeated. The 6502 would, for example, see the address 0x0000 and 0x8000 
; as having the same content, and the same would happen to addresses 0x7fff and 0xffff and everything in between. 
; That would be fine, but would also be a waste, as we could use  half of these addresses for something else, 
; like accessing the RAM or controlling something.
; 
; To avoid that we can connect the 16th address line from the 6502 to the chip enable bit on the EEPROM 
; (that is an "active low" bit), that way the EEPROM would only be active when the 16th address line 
; (the first bit of the address) is low. The 6502 would then be able to only access the addresses 
; 0x0000 to 0x7fff from the EEPROM, but then there's a problem: during the initialization process, 
; the 6502 reads from addresses 0xfffc and 0xfffd in order to find where the address where it is 
; supposed to start executing the program, but since those addresses start with a binary 1, 
; the EEPROM would be disabled. To fix that, we can invert the signal coming from the address 
; line 16 from the 6502 to the chip enable pin on the EEPROM. This way the 6502 will actually access 
; addresses 0x8000 to 0xffff from the EEPROM (where the addresses 0xfffc and 0xfffd reside), and the 
; first half of the address space will be free to be used with something else. That will be kind of 
; weird because what is address 0x0000 from the perspective of the EEPROM will actually be address 0x8000 
; from the perspective of the 6502.
; 
; 6502 and 6522
; =============
;
; The 6522 will be active when the 6502 points to any address between 0x6000 (0110 0000 0000 0000) 
; and 0x7ff (0111 1111 1111 1111). What matters are the first 3 bits, they must be 011 in order for 
; the chip select pins on the 6522 to be high. The rest of the address is being ignored for simplicity.
; 
; The 6522 has 16 registers that can be used by the 6502. The register that will be used are selected by
; 4 register select pins. In order for the 6502 to communicate that to the 6522, the address lines 
; A0, A1, A2, and A3 on the 6502 are connected to the pins RS0, RS1, RS2, and RS3 on the 6522. So the 
; 6502 can select the register by pointing to an address between 0x6000 to 0x600F:
; 
; 0x6000 = 0110 0000 0000 0000 => the last four bits are zeros, 
; which sets all register select pins to 0, which activates PORT B on the 6522.
; 
; 0x6001 = 0110 0000 0000 0001 => the last bit is 1, which sets RS0 to 1, which activates PORT A on the 6522.
; 
; 0x6002 = 0110 0000 0000 0010 => the second to last bit is 1, which sets RS1 to 1, 
; which activates the Data Direction Register "B" on the 6522.
; 
; 0x6003 = 0110 0000 0000 0011 => the last 2 bits are 1, which sets RS0 and RS1 to 1, 
; which activates the Data Direction Register "A" (DDRA) on the 6522.
;
; 6502 and memory
; ===============
;
;
;



PORTB = $6000
PORTA = $6001
DDRB = $6002
DDRA = $6003

value = $0200           ; 2 bytes
mod10 = $0202           ; 2 bytes
message = $0204         ; 6 bytes
counter = $020a         ; 2 bytes

E  = %10000000
RW = %01000000
RS = %00100000

    .org $8000

reset:
    ldx #$ff            ; Initialize the stack pointer
    txs
    cli                 ; Enable interrupts

    lda #%11111111      ; Set all pins on port B to output
    sta DDRB

    lda #%11100000      ; Set top 3 pins on port A to output
    sta DDRA

    lda #%00111000      ; Set 8-bit mode; 2-line display; 5x8 font
    jsr lcd_instruction
    lda #%00001110      ; Set display on; cursor on; blink off
    jsr lcd_instruction
    lda #%00000110      ; Increment and shift cursor; don't shift display
    jsr lcd_instruction
    lda #%00000001      ; Clear display
    jsr lcd_instruction

    lda #0              ; Initialize counter to zero
    sta counter
    sta counter + 1

loop:
    lda #%00000010     ; Put cursor at home position
    jsr lcd_instruction

    lda #0              ; null-terminated string
    sta message

    ; Initialize value to be the number we want to convert
    lda counter
    sta value
    lda counter + 1
    sta value + 1

divide:
    ; Initialize the remainder to zero
    lda #0
    sta mod10
    sta mod10 + 1
    clc

    ldx #16
divLoop:
    ; Rotate the quotient and the remainder
    rol value
    rol value + 1
    rol mod10
    rol mod10 + 1

    ; a,y = dividend - divisor
    sec
    lda mod10
    sbc #10
    tay                 ; save low byte in Y
    lda mod10 + 1
    sbc #0
    bcc ignore_result   ; branch if dividend is < divisor
    sty mod10
    sta mod10 + 1

ignore_result:
    dex 
    bne divLoop
    rol value           ; shift in the last bit of the quotient
    rol value + 1

    lda mod10
    clc
    adc #"0"
    jsr push_char

    ; if value != 0, then we need to divide again
    lda value
    ora value + 1
    bne divide          ; branch if value != 0

    ldx #0
print:
    lda message,x
    beq loop
    jsr print_char
    inx
    jmp print

; Add the character in the A register to the beginning of the
; null-terminated string `message`
push_char:
    pha                 ; push new first character onto stack
    ldy #0

char_loop:
    lda message,y       ; Get char on the string and put that into X
    tax
    pla
    sta message,y       ; Pull char off stack and add it to the string
    iny
    txa
    pha                 ; Push char from string onto stack
    bne char_loop

    pla
    sta message,y       ; Push the null off the stack and add to the end of the string

    rts

lcd_wait:
    pha
    lda #%00000000      ; Port B is input
    sta DDRB
lcdbusy:
    lda #RW
    sta PORTA
    lda #(RW | E)
    sta PORTA
    lda PORTB
    and #%10000000
    bne lcdbusy

    lda #RW
    sta PORTA
    lda #%11111111      ; Port B is output
    sta DDRB
    pla
    rts

lcd_instruction:
    jsr lcd_wait
    sta PORTB
    lda #0              ; Clear RS/RW/E bits
    sta PORTA
    lda #E              ; Set E bit to send instruction
    sta PORTA
    lda #0
    sta PORTA
    rts

print_char:
    jsr lcd_wait
    sta PORTB
    lda #RS             ; Set RS; Clear RW/E bits  
    sta PORTA
    lda #(RS | E)       ; Set E bit to send instruction       
    sta PORTA
    lda #RS             ; Clear E bit
    sta PORTA
    rts

nmi:
irq:
    inc counter
    bne exit_irq
    inc counter + 1;
exit_irq:
    rti

    .org $fffa
    .word nmi
    .word reset
    .word irq