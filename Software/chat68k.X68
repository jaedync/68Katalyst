; Define the starting address
RESET:  ORG $01C000

; Define the ROM addresses for the I/O subroutines
PRINT_STR_A:    EQU $066A  ; Replace with the actual ROM address of PRINT_STR_A
; prints null terminated string pointed by A1
GET_LINE_A:       EQU $06E2  ; Replace with the actual ROM address of GET_LINE

PRINT_STR_B:    EQU $0680  ; Replace with the actual ROM address of PRINT_STR_B

GET_CHAR_B:     EQU $05F8

PUT_CHAR_A:     EQU $069C

PUT_CHAR_B:     EQU $06B8

NEW_LINE:       EQU $0642

MAIN:           EQU $0534

RAM:            EQU $010000
RAMOFFSET:      EQU $000500

LNBUFF:         EQU $000000
; location of monitor program return


START:
    LEA         WELCOME(PC),A1
    jsr        (PRINT_STR_A)
    jsr        (GET_LINE_A)
    LEA         LNBUFF+RAM+RAMOFFSET,A1
    jsr         (PRINT_STR_B)
    move.l      #$0A,D1
    jsr         (PUT_CHAR_B)
    bsr         RECIEVE
    bra         START

RECIEVE:
        jsr     (NEW_LINE)
        jsr     (NEW_LINE)
.loop   jsr     (GET_CHAR_B)
        cmp.l   #$00,D1
        beq     .end
        jsr     (PUT_CHAR_A)
        bra     .loop
.end    rts



    ; Call the PRINT_STR_ROM subroutine
    ; Call the GET_LINE_ROM subroutine
    ;JSR (GET_LINE_ROM)

    ; End of the program. go back to monitor program
    JSR (MAIN)

WELCOME:
    DC.B 'KatalystChat!', 13, 10, 0

    END     RESET




*~Font name~Courier New~
*~Font size~10~
*~Tab type~1~
*~Tab size~4~
