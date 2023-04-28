; Define the starting address
RESET:  ORG $01C000

; Define the ROM addresses for the I/O subroutines
PRINT_STR_ROM   EQU $0654  ; Replace with the actual ROM address of PRINT_STR
; prints null terminated string pointed by A1
GET_LINE_ROM    EQU $06E2  ; Replace with the actual ROM address of GET_LINE

MAIN            EQU $0534
; location of monitor program return


START:
    ; Load the address of the hello_world string into A0
    LEA hello_world, A1

    ; Call the PRINT_STR_ROM subroutine
    JSR (PRINT_STR_ROM)

    ; Call the GET_LINE_ROM subroutine
    ;JSR (GET_LINE_ROM)

    ; End of the program. go back to monitor program
    JSR (MAIN)

hello_world:
    DC.B 'Hello, World!', 13, 10, 0

    END     RESET




*~Font name~Courier New~
*~Font size~10~
*~Tab type~1~
*~Tab size~4~
