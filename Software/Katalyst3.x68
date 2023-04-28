ROM:        EQU     $000000
RAM:        EQU     $010000
DUART:      EQU     $020000

STACK:      EQU     $01FFF0

RAMOFFSET:   EQU     $500
MAXBUFF:    EQU     48
LNBUFF:     DS.B    MAXBUFF
ECHO:       DS.B    1
U_CASE:     DS.B    1
START_ADDR: DS.B    1
FIRST_REC:  DS.L    1
REG_VALUES: DS.L 16  ; Declare memory locations for saving register values


sim:        EQU     $0              ; 1 -> simulator / 0 -> hardware

        ORG     $000
        DC.L    STACK
        DC.L    RESET

RESET:  ORG     $000500


    * Compiled on simulator only
    IFNE sim                    ; Disable keyboard echo on simulator
        move.b  #$00,D1
        move.b  #12,D0
        trap    #15
        move.l  #758*$10000+700,D1 ; Set screen size to ~80 columns
        move.b  #33,D0
        trap    #15
        move.l  #STACK,A7       
        clr.l   D0              
        clr.l   D1
    ENDC

        bsr     INIT_DUART
        bsr     CLEARALL
        lea     RAM+RAMOFFSET,A6
        MOVEQ #0, D0            ; Set D0 to zero (longword)
        MOVE.L D0, FIRST_REC(A6) ; Store the zero value from D0 to the address FIRST_REC(A6)
        bsr     SAVE_REG
        clr.B   ECHO(A6)
        clr.B   U_CASE(A6)
        move.b  #1, ECHO(A6)     ; Set the byte value at ECHO(A5) to 1
        move.b  #1, U_CASE(A6)     ; Set the byte value at U_CASE(A5) to 1

        lea     WELCOME(PC),A1
        bsr     PRINT_STR

; MAIN subroutine:
; - Repeats in an infinite loop.
MAIN:
        bsr     GET_LINE
        bsr     TRY_EXEC
        bra     MAIN


* //////////// *
* DUART Driver *
* \\\\\\\\\\\\ *

* DUART Address Offsets
* DUART         EQU     $020000
MRA         EQU     $01         ; Mode Register A (read)
SRA         EQU     $03         ; Status Register A (read)
CSRA        EQU     $03         ; Clock Select Register A (write)
CRA         EQU     $05         ; Command Register A (write)
HRA         EQU     $07         ; Holding Register A (Rx-read, Tx-write)
MRB         EQU     $11         ; Mode Register B (read)
SRB         EQU     $13         ; Status Register B (read)
CSRB        EQU     $13         ; Clock Select Register B (write)
CRB         EQU     $15         ; Command Register B (write)
HRB         EQU     $17         ; Holding Register B (Rx-read, Tx-write)
ACR         EQU     $09         ; Auxiliary Control Register

* Duart Values
BAUD_RATE   EQU     $BB         ; 9600Hz Baud Value
BAUD2       EQU     $80         ; 9600Hz Baud 2nd
Rx_rdy      EQU     $00         ; Recieve Ready Bit Position
Tx_rdy      EQU     $02         ; Transmit Ready Bit Position
MR1         EQU     $13         ; 8 Bits, 1 Stop Bit, No Parity
MR2         EQU     $07         ; CTS/RTS Disabled, 1 Stop Bit
MR_rst      EQU     $10         ; Reset MRA/B pointer
Rx_rst      EQU     $20         ; Reset and disable Rx
Tx_rst      EQU     $30         ; Reset and disable Tx
TxRxON      EQU     $05         ; Enable Tx/Rx, Duart Ready

INIT_DUART:
    lea     DUART,A0            ; write duart memory map to A0

    * Software Reset
    move.b  #MR_rst,CRA(A0)     ; Reset Mode Register A
    move.b  #Rx_rst,CRA(A0)     ; Reset RxA
    move.b  #Tx_rst,CRA(A0)     ; Reset TxA
    move.b  #MR_rst,CRB(A0)     ; Reset Mode Register B
    move.b  #Rx_rst,CRB(A0)     ; Reset RxB
    move.b  #Tx_rst,CRB(A0)     ; Reset TxB

    * Initialize Duart
    move.b  #BAUD2,ACR(A0)      ; Select Baud Rate
    move.b  #BAUD_RATE,CSRA(A0) ; Set 9600Hz Baud for Channel A
    move.b  #BAUD_RATE,CSRB(A0) ; Set 9600Hz Baud for Channel B

    move.b  #MR1,MRA(A0)        ; Set Channel A 8 Bits, 1 Stop Bit, No Parity
    move.b  #MR1,MRB(A0)        ; Set Channel B 8 Bits, 1 Stop Bit, No Parity
    move.b  #MR2,MRA(A0)        ; Set Channel A Disable CTS/RTS, 1 Stop Bit
    move.b  #MR2,MRB(A0)
    move.b  #MR2,MRB(A0)        ; Set Channel B Disable CTS/RTS, 1 Stop Bit

    move.b  #TxRxON,CRA(A0)     ; Enable Channel A Tx/Rx
    move.b  #TxRxON,CRB(A0)     ; Enable Channel B Tx/Rx

    rts

; store result in D1
GET_CHAR:
        ;move.l D0,-(SP)
        ;move.l A0,-(SP)
        ;move.l A5,-(SP)
        lea     RAM+RAMOFFSET,A5
        IFNE sim
                move.l  #05,D0
                trap    #15
                bra     .skip
        ENDC
        lea     DUART,A0            ; write DUART location to A0
.hold   btst    #Rx_rdy,SRA(A0)     ; ask UART A to get data
        bne     .ch_a               ; wait for DUART confirmation
        ;btst    #Rx_rdy,SRB(A0)     ; ask UART B to get data
        ;bne     .ch_b               ; wait for DUART confirmation
        bra     .hold               ; loop back to keep polling

.ch_a   move.b  HRA(A0),D1          ; Get byte from UART A into D1
        bra     .skip
.ch_b   move.b  HRB(A0),D1          ; Get byte from UART B into D1

.skip   and.B   #$7F,D1             ; remove MSB for ASCII
        tst.b   U_CASE(A5)          ; Upper case required?
        beq     .echo               ; If not requested, skip
        cmp.b   #$61,D1             ; Set check condition to lowercase letter 'a'
        blt     .echo               ; If less than 'a', skip conversion
        cmp.b   #$7A,D1             ; Set check condition to lowercase letter 'z'
        bgt     .echo               ; If greater than 'z', skip conversion
        eor.b   #$20,D1             ; Toggle the 5th bit to convert to uppercase
.echo   tst.B   ECHO(A5)            ; echo input byte?
        beq     .done               ; skip if no echo
        bsr     PUT_CHAR            ; do the echo
        ;move.l  (SP)+,A5
        ;move.l  (SP)+,A0
        ;move.l  (SP)+,D0
.done   rts                         ; return


; store result in D1
GET_CHAR_B:
        ;move.l D0,-(SP)
        ;move.l A0,-(SP)
        ;move.l A5,-(SP)
        lea     RAM+RAMOFFSET,A5
        IFNE sim
                move.l  #05,D0
                trap    #15
                bra     .skip
        ENDC
        lea     DUART,A0            ; write DUART location to A0
.hold   btst    #Rx_rdy,SRB(A0)     ; ask UART B to get data
        bne     .ch_b               ; wait for DUART confirmation
        bra     .hold               ; loop back to keep polling

.ch_b   move.b  HRB(A0),D1          ; Get byte from UART B into D1

.skip   and.B   #$7F,D1             ; remove MSB for ASCII
        tst.b   U_CASE(A5)          ; Upper case required?
        beq     .echo               ; If not requested, skip
        cmp.b   #$61,D1             ; Set check condition to lowercase letter 'a'
        blt     .echo               ; If less than 'a', skip conversion
        cmp.b   #$7A,D1             ; Set check condition to lowercase letter 'z'
        bgt     .echo               ; If greater than 'z', skip conversion
        eor.b   #$20,D1             ; Toggle the 5th bit to convert to uppercase
.echo   tst.B   ECHO(A5)            ; echo input byte?
        beq     .done               ; skip if no echo
        bsr     PUT_CHAR            ; do the echo
        ;move.l  (SP)+,A5
        ;move.l  (SP)+,A0
        ;move.l  (SP)+,D0
.done   rts                         ; return


NEW_LINE:
        movem.l A1,-(SP)
        lea CRLF(PC),A1
        bsr PRINT_STR
        movem.l (SP)+,A1
        rts

PRINT_STR:
        move.l  A1,-(SP)
        move.l  D1,-(SP)
.loop   move.b  (A1)+,D1
        beq     .end
        bsr     PUT_CHAR
        bra     .loop
.end    move.l  (SP)+,D1
        move.l  (SP)+,A1
        rts

PRINT_STR_A:
        move.l  A1,-(SP)
        move.l  D1,-(SP)
.loop   move.b  (A1)+,D1
        beq     .end
        bsr     PUT_CHAR_A
        bra     .loop
.end    move.l  (SP)+,D1
        move.l  (SP)+,A1
        rts

PRINT_STR_B:
        move.l  A1,-(SP)
        move.l  D1,-(SP)
.loop   move.b  (A1)+,D1
        beq     .end
        bsr     PUT_CHAR_B
        bra     .loop
.end    move.l  (SP)+,D1
        move.l  (SP)+,A1
        rts

; output D1
PUT_CHAR:
        bra     PUT_CHAR_A
        ;bra     PUT_CHAR_B
        rts

PUT_CHAR_A:
        movem.l D0/A0,-(SP)
    IFNE sim
        move.l  #06,D0
        trap    #15
        bra     .end
    ENDC
        lea     DUART,A0
.hold   btst    #tx_rdy,SRA(A0)
        beq     .hold
        move.b  D1,HRA(A0)
.end    movem.l (SP)+,D0/A0
        rts

PUT_CHAR_B:
        movem.l D0/A0,-(SP)
    IFNE sim
        bra     .end
    ENDC
        lea     DUART,A0
.hold   btst    #tx_rdy,SRB(A0)
        beq     .hold
        move.b  D1,HRB(A0)
.end    movem.l (SP)+,D0/A0
        rts

P_CLRF:
        move.l  A1,-(SP)
        lea     CRLF(PC),A1
        bsr     PRINT_STR
        move.l  (SP)+,A1
        rts
        

GET_LINE:
        MOVE.L  #0, D3
        lea     RAM+RAMOFFSET,A6
        lea     LNBUFF(A6),A1
        lea     (A1),A2
        lea     MAXBUFF(A1),A3
        lea     PROMPT(PC),A1
        bsr     PRINT_STR
.loop   bsr     GET_CHAR
        cmp.b   #CTRL_C,D1
        beq     .rst
        cmp.b   #CR,D1
        bne     .skip
        cmp.l   A1,A2
        beq     .rst
.skip   cmp.b   #BS,D1
        beq     .del
        cmp.b   #LA,D1
        bne     .proc
.del    cmp.l   A1,A2
        beq     .loop
        SUBQ.L  #1, D3  ; Decrement D3 by 1
        lea     -1(A2),A2

        bra     .loop
.proc:  ADDQ.L  #1, D3  ; Increment D3 by 1
        move.b  D1,(A2)+
        cmp.b   #CR,D1
        bne     .ovfl
        move.b  #0,(A2)+
        bsr     NEW_LINE
        rts
.ovfl   cmp.l   A2,A3
        bne     .loop
.rst    bsr     NEW_LINE
        bra     GET_LINE

TRY_EXEC:
        lea     COMTAB(PC),A3
        lea     RAM+RAMOFFSET,A6
.back   clr.l   D0
        move.b  (A3),D0
        beq     .exit
        lea     6(A3,D0.W),A4
        move.b  1(A3),D1
        lea     LNBUFF(A6),A5
        move.b  2(A3),D2
        cmp.b   (A5)+,D2
        beq     .skip
.next   move.l  A4,A3
        bra     .back
.skip   sub.b   #1,D1
        beq     .end
        lea     3(A3),A3
.loop   move.b  (A3)+,D2
        cmp.b   (A5)+,D2
        bne     .next
        sub.b   #1,D1
        bne     .loop
.end    lea     -4(A4),A3
        or.b    #1,CCR
        bra     .out
.exit   and.b   #$FE,CCR

.out    bcs     .exec
        lea     INVALID(PC),A1
        bra     PRINT_STR

.exec   move.l  (A3),A3
        lea     COMTAB(PC),A4
        add.l   A4,A3
        jmp     (A3)


error_invalid_address:
    lea     ERROR_INVALID_ADDRESS_TXT(PC), A1
    bsr     PRINT_STR
    rts

        error_invalid_value:
        lea     ERROR_INVALID_VALUE_TXT(PC), A1
        bsr     PRINT_STR
        rts

OUT_BYTE:
    MOVE.L D1, -(A7)    ; Save D1 to the stack
    ; Store high nibble (first digit)
    MOVE.B D1, D2       ; Copy D1 to D2 to not modify the original value
    ANDI.B #$F0, D2     ; Mask low nibble
    LSR.B #4, D2        ; Shift right 4 times to get high nibble in position
    ; Convert high nibble to ASCII
    ADDI.B #$30, D2     ; Add '0' (ASCII value 48, hex 30) to get ASCII value of hex digit
    CMP.B #$3A, D2      ; Check if ASCII value is greater than '9'
    BLO.S .HIGH_VALID   ; If not, skip next instruction
    ADDI.B #7, D2       ; If ASCII value > '9', add 7 to get correct ASCII value for A-F
.HIGH_VALID:
    ; Store low nibble (second digit)
    MOVE.B D1, D3       ; Copy original D1 value to D3
    ANDI.B #$0F, D3     ; Mask high nibble
    ; Convert low nibble to ASCII
    ADDI.B #$30, D3     ; Add '0' (ASCII value 48, hex 30) to get ASCII value of hex digit
    CMP.B #$3A, D3      ; Check if ASCII value is greater than '9'
    BLS.S .LOW_VALID    ; If not, skip next instruction
    ADDI.B #7, D3       ; If ASCII value > '9', add 7 to get correct ASCII value for A-F
.LOW_VALID:
    ; Print high nibble
    MOVE.B D2, D1       ; Copy D2 back to D1 for printing
    bsr   PUT_CHAR      ; Call PRINT_CHAR to print high nibble
    ; Print low nibble
    MOVE.B D3, D1       ; Copy D3 back to D1 for printing
    bsr   PUT_CHAR      ; Call PRINT_CHAR to print low nibble
    MOVE.L (A7)+, D1    ; Restore D1 from the stack
    RTS                 ; Return from subroutine

N1:
        move.b  D1,-(SP)
        and.b   #$0F,D1
        add.b   #$30,D1
        cmp.b   #$39,D1
        bls     .prnt
        add.b   #$07,D1
.prnt   bsr     PUT_CHAR
        move.b (SP)+,D1
        rts
N2:
        ror.b   #4,D1
        bsr     N1
        rol.b   #4,D1
        bra     N1
N4:
        ror.w   #8,D1
        bsr     N2
        rol.w   #8,D1
        bra     N2
N6:
        swap    D1
        bsr     N2
        swap    D1
        bra     N4
N8:
        swap    D1
        bsr     N4
        swap    D1
        bra     N4

; print a3 and byte pointed by a3
MEM_OUT:
        move.l  D1,-(SP)
        move.l  A3,D1
        move.l  D1,-(SP)
        move.l  #'$',D1
        BSR     PUT_CHAR
        move.l  (SP)+,D1
        bsr     N6
        bsr     P_SPACE
        move.l  #'(',D1
        BSR     PUT_CHAR
        move.b  (A3)+,D1
        bsr     N2
        move.l  D1,-(SP)
        move.l  #')',D1
        BSR     PUT_CHAR
        bsr     P_SPACE
        move.l  #'>',D1
        BSR     PUT_CHAR
        move.l  (SP)+,D1
        move.l  (SP)+,D1
        rts

BYTE_OP:
    lea     BYTE_MOD_MSG(PC),A1
    jsr     PRINT_STR
    bsr     GET_STR_HEX
    tst.b   D7
    bne     .eradd
    move.l  D1,A3
.loop:
    bsr     NEW_LINE
    bsr     MEM_OUT
    bsr     P_SPACE
    bsr     GET_CHAR
    cmp.b   #ESC,D1
    beq     .exit
    cmp.b   #CTRL_C,D1
    beq     .exit
    cmp.b   #BS,D1
    bne     .skip_backstep
.backstep:
    lea     -2(A3),A3
    bra     .loop
.skip_backstep:
    cmp.b   #CR,D1
    beq     .loop
    move.l  #.continue,-(SP)
    move.l  D2,-(SP)
    move.l  #intercept_get_byte,-(SP)
    bra     partial_hex_call
.continue:
    tst.b   D7
    bne     .erhex
    move.b  D1,-1(A3)
    bra     .loop
.exit:
    rts
.eradd:
    bra     error_invalid_address
.erhex:
    bra     error_invalid_value

GRAB_BYTE:
        move.l  D2,-(SP)
        bsr     HEX
intercept_get_byte:
        asl.b   #4,D1
        move.b  D1,D2
        bsr     HEX
        add.b   D2,D1
        move.l  (SP)+,D2
        rts

HEX:
        bsr     GET_CHAR
partial_hex_call:
        sub.b   #$30,D1
        bmi     .inv
        cmp.b   #$09,D1
        ble     .exit
        sub.b   #$07,D1
        cmp.b   #$0F,D1
        ble     .exit
.inv    or.b    #1,D7
.exit   rts





P_SPACE:
        move.b  D1,-(SP)
        move.b  #SPACE,D1
        bsr     PUT_CHAR
        move.b  (SP)+,D1
        rts


; Subroutine to dump 4 lines with 16 bytes of each starting from a given memory location
DUMP:
    ; Prompt for a memory location
    lea     PROMPT_LOC_TXT(PC), A1
    bsr     PRINT_STR

    ; Call GET_STR_HEX to receive the memory location
    bsr     GET_STR_HEX
    tst.l   D7
    beq     no_error_3
    bra     error_invalid_address

no_error_3:
    ; Check if the address is within bounds
    cmpi.l  #$000000, D1
    blt     error_invalid_address
    cmpi.l  #$020000, D1
    bge     error_invalid_address

    move.l D1,D6

    ; Initialize loop counter
    move.l  #4, D5

outer_loop:
    ; Print 16 bytes per line
    move.l  #16, D4

inner_loop:
    ; Read the value at the memory location
    move.l  D6, A0
    move.b  (A0), D1

    ; Print the value
    bsr     OUT_BYTE

    ; Print a space
    move.b  #' ', D1
    bsr     PUT_CHAR

    ; Increment memory location
    add.l   #1, D1

    ; Decrement inner loop counter
    sub.l   #1, D4
    bne     inner_loop

    ; Print newline
    move.b  #CR, D1
    bsr     PUT_CHAR
    move.b  #LF, D1
    bsr     PUT_CHAR

    ; Decrement outer loop counter
    sub.l   #1, D5
    bne     outer_loop

    rts

OUT_D0:
    MOVE.L  D1, -(SP)       ; Save the contents of D1 onto the stack

    MOVE.L  D0, D1          ; Copy the contents of D0 to D1
    LSR.L   #8, D1          ; Shift D1 right by 8 bits
    SWAP    D1              ; Swap the upper and lower words of D1
    BSR     OUT_BYTE        ; Output the most significant byte as a two-digit hex character

    MOVE.L  D0, D1          ; Copy the contents of D0 to D1
    SWAP    D1              ; Swap the upper and lower words of D1
    ANDI.L  #$00FF, D1      ; Mask out all but the least significant byte of D1
    BSR     OUT_BYTE        ; Output the second most significant byte as a two-digit hex character

    MOVE.L  D0, D1          ; Copy the contents of D0 to D1
    LSR.L   #8, D1          ; Shift D1 right by 8 bits to isolate the third most significant byte
    ANDI.L  #$00FF, D1      ; Mask out all but the least significant byte of D1
    BSR     OUT_BYTE        ; Output the third most significant byte as a two-digit hex character

    MOVE.L  D0, D1          ; Copy the contents of D0 to D1
    ANDI.L  #$00FF, D1      ; Mask out all but the least significant byte of D1
    BSR     OUT_BYTE        ; Output the least significant byte as a two-digit hex character

    MOVE.L  (SP)+, D1       ; Restore the contents of D1 from the stack

    RTS                     ; Return from subroutine

; Add the EDIT_REGISTER subroutine to prompt for a register to edit and save the value
EDIT_REGISTER:
        clr.l   D2
        lea REG_REQ(PC),A1
        bsr PRINT_STR
        bsr NEW_LINE
        move.b  #'>', D1
        bsr     PUT_CHAR
        bsr     GET_CHAR
        CMP.B #'A', D1                  ; Check if the input is an address register
        BEQ .address                    ; If the input is an address register, branch to .address
        CMP.B #'D', D1                  ; Check if the input is a data register
        BNE .invtype                    ; If the first character is not 'A' or 'D', jump to error handling 
        BRA .skip
.address:
        ADD.B #8, D2
.skip   bsr     GET_CHAR
        SUB.B #'0', D1                  ; Convert the ASCII number to its integer value
        CMP.B #7, D1                    ; Check if the number is within the range 0-7
        BHI .invnum                     ; If the number is higher than 7, jump to error handling
        add.b D2,D1
        LSL.W #2, D1                    ; Multiply the index by 4 (size of longword) to get the offset
        move.l D1,D0                    ; Save offset to run get_str_hex
        lea REG_VAL_REQ(PC),A1
        bsr PRINT_STR
        BSR GET_STR_HEX                 ; Call GET_STR_HEX to get the value from user
        LEA REG_VALUES+RAM+RAMOFFSET, A0 ; Load the address of REG_VALUES
        ADDA.L D0, A0                   ; Add the offset to A0
        MOVE.L D1, (A0)                 ; Store the user's value in the register buffer
        RTS                             ; Return from subroutine

.invtype:
        lea REG_TYPE_BAD(PC),A1
        bsr PRINT_STR
        RTS                           ; Return from subroutine
.invnum:
        lea REG_NUM_BAD(PC),A1
        bsr PRINT_STR
        RTS                           ; Return from subroutine


* --------------- *
*    LOAD SREC    *
* --------------- *
* Load an assembled s-record formatted program into RAM storage
* While executing: D3 - Checksum accumulator, D2 - Byte count to read
*                  D1 - Current byte read,    A2 - Destination address
LOAD:
        lea     RAM+RAMOFFSET,A6
        clr.b   ECHO(A6)
        bsr     NEW_LINE
        lea     READY_LOAD(PC),A1
        bsr     PRINT_STR
        bsr     NEW_LINE
        clr.l   FIRST_REC(A6)

.check_next   bsr     GET_CHAR
        cmp.b   #CTRL_C,D1
        beq     .exit_load
        cmp.b   #'S',D1
        bne     .check_next
        bsr     GET_CHAR
        cmp.b   #'9',D1
        beq     .terminate
        cmp.b   #'8',D1
        bne     .process_data

.terminate bsr     .read_length
        move.b  D1,D6
.read_remaining_bytes_loop
        bsr     .read_length
        sub.b   #1,D6
        bne     .read_remaining_bytes_loop

.exit_load move.b   #1,ECHO(A6)
        btst    #0,D7
        beq     .check_input_error
        lea     ER_HEX(PC),A1
        bsr     PRINT_STR

.check_input_error btst    #2,D7
        beq     .return_load
        lea     ER_LOD(PC),A1
        bsr     PRINT_STR

.return_load rts

.process_data cmp.b   #'1',D1
        beq     .process_s1
        cmp.b   #'2',D1
        bne     .check_next

.process_s2 clr.b   D3
        bsr     .read_length
        sub.b   #4,D1
        move.b  D1,D2
        clr.l   D1
        bsr     .read_length
        asl.l   #8,D1
        bsr     .read_length
        asl.l   #8,D1
        bsr     .read_length
        move.l  D1,A2
        bra     .store_data

.process_s1 clr.b   D3
        bsr     .read_length
        sub.b   #3,D1
        move.b  D1,D2
        clr.l   D1
        bsr     .read_length
        asl.l   #8,D1
        bsr     .read_length
        move.l  D1,A2

.store_data tst.b   FIRST_REC(A6)
        bne     .skip_store_first
        move.l  A2,FIRST_REC(A6)
        move.b  #1,FIRST_REC(A6)

.skip_store_first
        bsr     .read_length
        move.b  D1,(A2)+
        sub.b   #1,D2
        bne     .store_data

        bsr     .read_length
        add.b   #1,D3
        beq     .check_next
        or.b    #%0000100,D7
        bra     .exit_load

.read_length bsr     GRAB_BYTE
        add.b   D1,D3
        rts


* ---------------- *
*    GO ROUTINE    *
* ---------------- *
* Jump to the address stored in FIRST_REC
GO:
        move.l  FIRST_REC(A6),A0    ; Load the address from FIRST_REC into A0
        move.l  A0,D0               ; Move the contents of A0 to D0
        tst.l   D0                  ; Test if D0 (A0) contains a valid address (non-zero)
        beq     .no_address         ; If zero, no valid address is present, skip the jump
        jmp     (A0)                ; Jump to the address in A0
.no_address:
        lea     NO_ADDR(PC),A1      ; Load the "No Address" error message
        bsr     PRINT_STR           ; Print the error message
        rts                         ; Return from GO

; result is stored in D1
GET_STR_HEX:
    lea     RAM+RAMOFFSET,A6
    move.l  D0, -(SP)
    move.l  D2, -(SP)
    move.l  D3, -(SP)
    move.l  D4, -(SP)
    clr.l   D7          ; Reset D7, the error flag
    bsr     GET_LINE    ; Run GET_LINE subroutine to receive a line buffer from the user
    lea     LNBUFF(A6), A1   ; Line buffer location
    clr.l   D1          ; Clear D1 for accumulating the hex value
    move.l  D3, D2      ; Move the amount of digits into D2
    subi.l  #2, D2       ; Subtract 2 from D2 to adjust for 0-based indexing

loop:
    clr.l   D0
    move.b  (A1)+, D0   ; Get a character from the buffer into D0 and increment A1
    cmp.b   #'0', D0    ; Compare D0 with '0'
    blt     error       ; If D0 < '0', set error flag and exit
    cmp.b   #'9', D0    ; Compare D0 with '9'
    ble     valid_digit ; If D0 <= '9', go to valid_digit
    cmp.b   #'A', D0    ; Compare D0 with 'A'
    blt     error       ; If D0 < 'A', set error flag and exit
    cmp.b   #'F', D0    ; Compare D0 with 'F'
    bgt     error       ; If D0 > 'F', set error flag and exit

valid_digit:
    cmp.b   #'9', D0    ; Compare D0 with '9'
    ble     skip        ; If D0 <= '9', skip subtraction
    subi.b  #7, D0       ; Subtract 7 from D0 to align A-F with their hex values

skip:
    subi.b  #'0', D0    ; Subtract '0' from D0 to align 0-9 with their hex values
    move.l  D2, D4      ; Move the exponent value into D4

exp_loop:
    cmpi.l  #0, D4      ; Compare D4 with 0
    beq     end_exp_loop ; If D4 == 0, exit the loop
    LSL.L   #4, D0
    subi.l  #1, D4       ; Decrement D4
    bra     exp_loop     ; Continue loop

end_exp_loop:
    add.l   D0, D1       ; Add D0 to D1
    subi.l  #1, D2       ; Decrement D2
    bge     loop         ; If D2 >= 0, continue loop
    move.l  (SP)+, D4
    move.l  (SP)+, D3
    move.l  (SP)+, D2
    move.l  (SP)+, D0
    rts                  ; Return from subroutine

error:
    moveq   #1, D7       ; Set D7 (error flag)
    move.l  (SP)+, D4
    move.l  (SP)+, D3
    move.l  (SP)+, D2
    move.l  (SP)+, D0
    rts                  ; Return from subroutine



HELP:
        lea     HLP(PC),A1
        bra     PRINT_STR
        rts

CLEAR:
        move.b  #CTRL_L,D0
        bra     PUT_CHAR
        rts

CLEARALL:
        clr.l   D0
        clr.l   D1
        clr.l   D2
        clr.l   D3
        clr.l   D4
        clr.l   D5
        clr.l   D6
        clr.l   D7
        move.l   D0,A0
        move.l   D0,A1
        move.l   D0,A2
        move.l   D0,A3
        move.l   D0,A4
        move.l   D0,A5
        move.l   D0,A6
        rts


SAVE_REG:
        MOVEM.L         D0-D7/A0-A7,REG_VALUES+RAM+RAMOFFSET
        RTS


; Save the original values of the data and address registers on the stack
REGISTER_TEST:
    MOVEM.L D0-D7/A0-A7, -(SP)

    ; Save register values to memory locations
    ;MOVEM.L D0-D7/A0-A7, REG_VALUES+RAM+RAMOFFSET

    ; Load register values from memory locations and display them
    lea     RAM+RAMOFFSET,A6
    LEA REG_VALUES(A6), A0
    LEA D0_STR(PC), A1
    MOVEQ #0, D7
    MOVEQ #8, D6

LOOP_DATA:
    MOVE.L (A0)+, D0
    BSR PRINT_STR
    ADDA.L #6, A1
    BSR OUT_D0
    BSR P_CLRF
    ADDQ.L #1, D7
    CMP.L D6, D7
    BNE LOOP_DATA

    MOVEQ #0, D7
    LEA A0_STR(PC), A1

LOOP_ADDRESS:
    MOVE.L (A0)+, D0
    BSR PRINT_STR
    ADDA.L #6, A1
    BSR OUT_D0
    BSR P_CLRF
    ADDQ.L #1, D7
    CMP.L D6, D7
    BNE LOOP_ADDRESS

    ; Restore the original values of the data and address registers from the stack
    MOVEM.L (SP)+, D0-D7/A0-A7

    RTS  ; Return from subroutine

; String constants for display
D0_STR: DC.B 'D0: $', 0
D1_STR: DC.B 'D1: $', 0
D2_STR: DC.B 'D2: $', 0
D3_STR: DC.B 'D3: $', 0
D4_STR: DC.B 'D4: $', 0
D5_STR: DC.B 'D5: $', 0
D6_STR: DC.B 'D6: $', 0
D7_STR: DC.B 'D7: $', 0

A0_STR: DC.B 'A0: $', 0
A1_STR: DC.B 'A1: $', 0
A2_STR: DC.B 'A2: $', 0
A3_STR: DC.B 'A3: $', 0
A4_STR: DC.B 'A4: $', 0
A5_STR: DC.B 'A5: $', 0
A6_STR: DC.B 'A6: $', 0
A7_STR: DC.B 'A7: $', 0

; Add your PRINT_STR, OUT_D0, and P_CLRF subroutines here.

; Make sure your command table has word-aligned subroutine addresses
COMTAB:

        DC.B    4,1
        DC.B    'HELP'
        DC.L    HELP-COMTAB

        DC.B    6,1
        DC.B    'CLEAR '
        DC.L    CLEAR-COMTAB
        
        DC.B    4,1
        DC.B    'BYTE'
        DC.L    BYTE_OP-COMTAB
        
        DC.B    4,1
        DC.B    'DUMP'
        DC.L    DUMP-COMTAB

        DC.B    8,1
        DC.B    'REGISTER'
        DC.L    REGISTER_TEST-COMTAB

        DC.B    6,1
        DC.B    'MODREG'
        DC.L    EDIT_REGISTER-COMTAB

        DC.B    2,1
        DC.B    'GO'
        DC.L    GO-COMTAB

        DC.B    4,1
        DC.B    'LOAD'
        DC.L    LOAD-COMTAB

        DC.B    4,1
        DC.B    'TEST'
        DC.L    REGISTER_TEST-COMTAB

        DC.B    4,1
        DC.B    'SAVE'
        DC.L    SAVE_REG-COMTAB

        DC.B    4,1
        DC.B    'EDIT'
        DC.L    EDIT_REGISTER-COMTAB
        
        DC.B    0,0

BS:         EQU     $7F     ; Destructive Backspace (CTRL-?)/DEL
LA:         EQU     $08     ; Putty non-destructive backspace (left arrow)
CR:         EQU     $0D     ; Carriage Return
LF:         EQU     $0A     ; Line Feed
ESC:        EQU     $1B     ; Escape
SPACE:      EQU     $20     ; Space
NUL:        EQU     $00     ; Null
CTRL_C:     EQU     $03     ; Quits line editing (ETX)
CTRL_L:     EQU     $0C     ; Clear console
EOT:        EQU     $04     ; End-of-Transmission ctrl^d

WELCOME:
        DC.B    CR,LF,'             ________  ______  ____  __.       __         .__                  __   '
        DC.B    CR,LF,'            /  _____/ /  __  \|    |/ _|____ _/  |______  |  | ___.__. _______/  |_ '
        DC.B    CR,LF,'           /   __  \  >      <|      < \__  \\   __\__  \ |  |<   |  |/  ___/\   __\'
        DC.B    CR,LF,'           \  |__\  \/   --   \    |  \ / __ \|  |  / __ \|  |_\___  |\___ \  |  |  '
        DC.B    CR,LF,'            \_____  /\______  /____|__ (____  /__| (____  /____/ ____/____  > |__|  '
        DC.B    CR,LF,'                  \/        \/        \/    \/          \/     \/         \/        '
        DC.B    CR,LF,0

HLP:
        DC.B    CR,LF,'Welcome to Katalyst Monitor Program for M68k'
        DC.B    CR,LF
        DC.B    CR,LF,'COMMANDS:'
        DC.B    CR,LF,'-> REGISTER              - Display status of registers D0-D7, A0-A7'
        DC.B    CR,LF,'-> MODREG                - Modify a register value'
        DC.B    CR,LF,'-> BYTE                  - Modify some location in memory'
        DC.B    CR,LF,'-> LOAD                  - Load S-Record into RAM to run later'
        DC.B    CR,LF,'-> GO                    - Run imported S-Record program'
        DC.B    CR,LF,'-> CLEAR                 - Clear the screen'
        DC.B    CR,LF,'-> HELP                  - Display this message'
        DC.B    CR,LF
        DC.B    CR,LF,'Designed with love and pain for Microcomputer Design, 2023'
        DC.B    CR,LF,'jaedynchilton.com'
        DC.B    CR,LF,0
REG_REQ:
        DC.B    'Enter Register to change (D0-D7, A0-A7)',0
REG_REQ_FAIL:
        DC.B    'Invalid',0
REG_TYPE_BAD:
        DC.B    CR,LF,'Data or Address register only (D0-D7, A0-A7)',0
REG_NUM_BAD:
        DC.B    CR,LF,'0-7 Index only (D0-D7, A0-A7)',0
REG_VAL_REQ:
        DC.B    CR,LF,'Input a hex value to store ($00000000-$FFFFFFFF)',0
ER_HEX:
        DC.B    'Hex Invalid',0
ER_LOD:
        DC.B    'Load Error',0
CRLF:
        DC.B    CR,LF,0
NO_READ:
        DC.B    CR,LF,'Only valid addresses to read are $000000 - $01FFFF.',0
PROMPT:
        DC.B    CR,LF,'>',0
INVALID:
        DC.B    'Command not found',CR,LF,0
READY_LOAD:
        DC.B    'Enter S-Record (Echo turned off)',0
BYTE_MOD_MSG:
        DC.B    'Enter starting address ($010000-$01FFFF)'
        DC.B    CR,LF,'Use ESC or CTRL_C to quit',0
NO_ADDR: 
        DC.B    'S-Record not yet imported. Run LOAD first.',0
PROMPT_LOC_TXT:
        dc.b    'Enter a memory location (RAM: $010000-$01FFFF):',0
PROMPT_VALUE_TXT:
        dc.b    CR,LF,'Enter a new 2-digit hex value: ',0
VALUE_TXT:
        dc.b    'Value at memory location: ',0
CONFIRM_CHANGE_TXT:
        dc.b    'New value confirmed: ',0
ERROR_INVALID_ADDRESS_TXT:
        dc.b    'Error: Invalid memory address.',0
ERROR_INVALID_VALUE_TXT:
        dc.b    'Error: Invalid hex value.',0
PROMPT_LOC: 
        DC.B 'Enter register location (e.g., D0, A0, SR, PC): ', 0
DISP_MSG:
        DC.B    CR,LF,'  Data          Address',CR,LF,0
SP_10:  DC.B    '        ',0
D_MSG1: DC.B    'SSP =',0
D_MSG2: DC.B    'SR  =',0
D_MSG3: DC.B    'PC  =',0


    END RESET









*~Font name~Courier New~
*~Font size~10~
*~Tab type~1~
*~Tab size~4~
