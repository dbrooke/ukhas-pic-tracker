; asmsyntax=pic

;**********************************************************************
; tracker.asm
;
; A simple HAB tracker developed as an exercise in learning PIC
; assembler.
;
; Interfaces to the data line of an NTX2B transmitter to send minimal
; UKHAS format RTTY telemetry strings at 50 baud 7N2 using an interrupt
; generated at bit rate by timer 0.
;
; Interfaces to a UBLOX 7 GPS at 9600 baud 8N1 using the EUSART
;
;**********************************************************************


    list        p=16f690        ; list directive to define processor
    #include    <p16f690.inc>       ; processor specific variable definitions

    __CONFIG    _CP_OFF & _CPD_OFF & _BOR_OFF & _PWRTE_ON & _WDT_OFF & _INTRC_OSC_NOCLKOUT & _MCLRE_OFF & _FCMEN_OFF & _IESO_OFF

;**********************************************************************

; constants for timer 0 (RTTY transmit bit timing)
; for initial testing these are for 1200 baud to allow use of a serial connection to a PC

#define tmr0_reload     d'230'
#define tmr0_options    b'11010100' ; internal clock source, prescaler 1:32

RTTY_BUF_LEN    EQU .40 ;length of RTTY buffer

;----------------------------------------------------------------------------
;Constants

SPBRG_VAL   EQU .25     ;set baud rate 9600 for 4Mhz clock
TX_BUF_LEN  EQU .80     ;length of transmit circular buffer
RX_BUF_LEN  EQU TX_BUF_LEN  ;length of receive circular buffer

;----------------------------------------------------------------------------
;Bit Definitions

TxBufFull   EQU 0       ;bit indicates Tx buffer is full
TxBufEmpty  EQU 1       ;bit indicates Tx buffer is empty
RxBufFull   EQU 2       ;bit indicates Rx buffer is full
RxBufEmpty  EQU 3       ;bit indicates Rx buffer is empty
ReceivedCR  EQU 4       ;bit indicates <CR> character received

;----------------------------------------------------------------------------
;Variables

        CBLOCK  0x70
        WREG_TEMP       ;storage for WREG during interrupt
        STATUS_TEMP     ;storage for STATUS during interrupt
        PCLATH_TEMP     ;storage for PCLATH during interrupt
        FSR_TEMP        ;storage for FSR during interrupt
        ENDC

        CBLOCK  0x20
        RTTYBuffer:RTTY_BUF_LEN ;RTTY transmit buffer
        tx_char         ;RTTY transmit character
        bit_count       ;count remaining bits in the transmit character
        tx_ptr          ;pointer to the character in transmit string
        index           ;index for copying strings
        Flags           ;byte to store indicator flags
        TempData        ;temporary data in main routines 
        BufferData      ;temporary data in buffer routines 
        TxStartPtr      ;pointer to start of data in TX buffer
        TxEndPtr        ;pointer to end of data in TX buffer
        RxStartPtr      ;pointer to start of data in RX buffer
        RxEndPtr        ;pointer to end of data in RX buffer
        ENDC

        CBLOCK  0xA0
        TxBuffer:TX_BUF_LEN ;transmit data buffer
        ENDC

        CBLOCK  0x120
        RxBuffer:RX_BUF_LEN ;receive data buffer
        ENDC

;-----------------------------------------------------------------------------
;Macros to select the register bank
; Many bank changes can be optimized when only one STATUS bit changes

Bank0       MACRO           ;macro to select data RAM bank 0
        bcf STATUS,RP0
        bcf STATUS,RP1
        ENDM

Bank1       MACRO           ;macro to select data RAM bank 1
        bsf STATUS,RP0
        bcf STATUS,RP1
        ENDM

Bank2       MACRO           ;macro to select data RAM bank 2
        bcf STATUS,RP0
        bsf STATUS,RP1
        ENDM

Bank3       MACRO           ;macro to select data RAM bank 3
        bsf STATUS,RP0
        bsf STATUS,RP1
        ENDM

;----------------------------------------------------------------------------
;This code executes when a reset occurs.

        ORG     0x0000      ;place code at reset vector

ResetCode:  clrf    PCLATH      ;select program memory page 0
        goto    Main        ;go to beginning of program

;----------------------------------------------------------------------------
;This code executes when an interrupt occurs.

        ORG 0x0004      ;place code at interrupt vector

InterruptCode:  movwf   WREG_TEMP   ;save WREG
        movf    STATUS,W    ;store STATUS in WREG
        clrf    STATUS      ;select file register bank0
        movwf   STATUS_TEMP ;save STATUS value
        movf    PCLATH,W    ;store PCLATH in WREG
        movwf   PCLATH_TEMP ;save PCLATH value
        clrf    PCLATH      ;select program memory page0
        movf    FSR,W       ;store FSR in WREG
        movwf   FSR_TEMP    ;save FSR value

        btfsc   INTCON,T0IF ;test timer 0 interrupt
        goto    isr_RTTY_bit

        Bank0           ;select bank0
        btfsc   PIR1,RCIF   ;test RCIF receive interrupt
        bsf STATUS,RP0  ;change to bank1 if RCIF set
        btfsc   PIE1,RCIE   ;test if interrupt enabled if RCIF set
        goto    GetData     ;if RCIF and RCIE set, do receive

        Bank0           ;select bank0
        btfsc   PIR1,TXIF   ;test for TXIF transmit interrupt
        bsf STATUS,RP0  ;change to bank1 if TXIF set
        btfsc   PIE1,TXIE   ;test if interrupt enabled if TXIF set
        goto    PutData     ;if TXIF and TCIE set, do transmit

;can do special error handling here - an unexpected interrupt occurred 

        goto    EndInt

;------------------------------------
;Get received data and write into receive buffer.

GetData:    Bank0
        btfsc   RCSTA,OERR  ;test overrun error flag
        goto    ErrOERR     ;handle it if error
        btfsc   RCSTA,FERR  ;test framing error flag
        goto    ErrFERR     ;handle it if error

        btfsc   Flags,RxBufFull ;check if buffer full
        goto    ErrRxOver   ;handle it if full

        movf    RCREG,W     ;get received data
        xorlw   0x0d        ;compare with <CR>      
        btfsc   STATUS,Z    ;check if the same
        bsf Flags,ReceivedCR ;indicate <CR> character received
        xorlw   0x0d        ;change back to valid data
        call    PutRxBuffer ;and put in buffer
        goto    EndInt

;error because OERR overrun error bit is set
;can do special error handling here - this code simply clears and continues

ErrOERR:    bcf RCSTA,CREN  ;reset the receiver logic
        bsf RCSTA,CREN  ;enable reception again
        goto    EndInt

;error because FERR framing error bit is set
;can do special error handling here - this code simply clears and continues

ErrFERR:    movf    RCREG,W     ;discard received data that has error
        goto    EndInt

;error because receive buffer is full and new data has been received
;can do special error handling here - this code simply clears and continues

ErrRxOver:  movf    RCREG,W     ;discard received data
        xorlw   0x0d        ;but compare with <CR>      
        btfsc   STATUS,Z    ;check if the same
        bsf Flags,ReceivedCR ;indicate <CR> character received
        goto    EndInt

;------------------------------------
;Read data from the transmit buffer and transmit the data.

PutData:    Bank0           ;select bank 0
        btfss   Flags,TxBufEmpty ;check if transmit buffer empty
        goto    PutDat1     ;if not then go get data and transmit
        Bank1           ;select bank1
        bcf PIE1,TXIE   ;disable TX interrupt because all done
        goto    EndInt

PutDat1:    call    GetTxBuffer ;get data to transmit
        movwf   TXREG       ;and transmit

;------------------------------------
;End of interrupt routine restores context

EndInt:     Bank0           ;select bank 0
        movf    FSR_TEMP,W  ;get saved FSR value
        movwf   FSR     ;restore FSR
        movf    PCLATH_TEMP,W   ;get saved PCLATH value
        movwf   PCLATH      ;restore PCLATH
        movf    STATUS_TEMP,W   ;get saved STATUS value
        movwf   STATUS      ;restore STATUS
        swapf   WREG_TEMP,F ;prepare WREG to be restored
        swapf   WREG_TEMP,W ;restore WREG without affecting STATUS
        retfie          ;return from interrupt

;----------------------------------------------------------------------------

isr_RTTY_bit:   Bank0   ;select bank 0

    ; reload timer 0
    movlw   tmr0_reload
    movwf   TMR0

    ; clear timer 0 interrupt flag
    bcf INTCON,T0IF

    ; any more bits in current char ?
    decfsz  bit_count,f
    goto    next_bit

    ; finished current char so get the next char
    movf    tx_ptr,w
    movwf   FSR
    movf    INDF,w

    ; check if it is a NULL char
    btfsc   STATUS,Z
    goto    null_char

    ; not a NULL char so start to send it
    bcf     PORTA,5     ; start bit

    ; next char now in w, so set the stop bit and save it
    iorlw   0x80
    movwf   tx_char

    ; start bit + 7 data + stop bit is 9
    movlw   9
    movwf   bit_count

    ; advance pointer for next time
    incf    tx_ptr,f

    goto    EndInt

null_char
    ; a NULL char terminates the string so disable interrupt
    bcf     INTCON,T0IE
    ; and set the bit count to 1 for next time
    movlw   1
    movwf   bit_count

    goto    EndInt

next_bit
    ; rotate bit into carry and set port pin accordingly
    rrf     tx_char,f
    btfsc   STATUS,C
    bsf     PORTA,5
    btfss   STATUS,C
    bcf     PORTA,5

    goto    EndInt


;**********************************************************************
; constant data and tables

; null terminated callsign string
callsign
    addwf   PCL,f
    dt  "DBTEST",0

;**********************************************************************
Main
    ; set I/O pin RA5 (RTTY Tx) as output
    banksel TRISA   ; not bank 0
    bcf     TRISA,5

    ; configure timer 0 options
    ; same bank as TRISA so no banksel
    movlw   tmr0_options
    movwf   OPTION_REG

    ; set up serial port and buffers
    call    SetupSerial

    banksel TMR0    ; restore bank 0

main_loop
    ; wait while RTTY transmission is in progress
    btfsc   INTCON,T0IE
    goto    main_loop

    ; start building a test message in RTTY buffer
    movlw   RTTYBuffer
    movwf   FSR

    ; start with two $ characters
    movlw   a'$'
    movwf   INDF
    incf    FSR,f
    movwf   INDF
    incf    FSR,f

    ; copy the callsign
    clrf    index
copy_callsign
    movf    index,w
    call    callsign
    movwf   INDF
    addlw   0
    btfsc   STATUS,Z
    goto    copied_callsign
    incf    FSR,f
    incf    index,f
    goto    copy_callsign

copied_callsign

    ; CR for testing
    movlw   0x0D
    movwf   INDF
    incf    FSR,f

    ; newline and zero terminator
    movlw   0x0A
    movwf   INDF
    incf    FSR,f
    movlw   0x0
    movwf   INDF

    ; set tx pointer to start of RTTY buffer
    movlw   RTTYBuffer
    movwf   tx_ptr

    ; reload timer 0
    movlw   tmr0_reload
    movwf   TMR0

    ; enable timer 0 interrupt
    bcf     INTCON,T0IF
    bsf     INTCON,T0IE

    goto    main_loop

;----------------------------------------------------------------------------
;Set up serial port and buffers.

SetupSerial:    Bank2           ;select bank 2
        bcf     ANSELH,ANS11    ; make RX pin digital
        Bank1
        movlw   SPBRG_VAL   ;set baud rate
        movwf   SPBRG
        movlw   0x24        ;enable transmission and high baud rate
        movwf   TXSTA
        Bank0           ;select bank0
        movlw   0x90        ;enable serial port and reception
        movwf   RCSTA
        clrf    Flags       ;clear all flag bits

        call    InitTxBuffer    ;initialize transmit buffer
        call    InitRxBuffer    ;initialize receive buffer

        movlw   0xc0        ;enable global and peripheral ints
        movwf   INTCON
        Bank1           ;select bank1
        movlw   0x30        ;enable TX and RX interrupts
        movwf   PIE1
        return

;----------------------------------------------------------------------------
;Circular buffer routines.

;----------------------------------------------------------------------------
;Initialize transmit buffer

InitTxBuffer:   Bank0
        movlw   LOW TxBuffer    ;take address of transmit buffer
        movwf   TxStartPtr  ;and place in transmit start pointer
        movwf   TxEndPtr    ;and place in transmit end pointer
        bcf Flags,TxBufFull ;indicate Tx buffer is not full
        bsf Flags,TxBufEmpty ;indicate Tx buffer is empty
        return

;----------------------------------------------
;Initialize receive buffer

InitRxBuffer:   Bank0
        movlw   LOW RxBuffer    ;take address of receive buffer
        movwf   RxStartPtr  ;and place in receive start pointer
        movwf   RxEndPtr    ;and place in receive end pointer
        bcf Flags,RxBufFull ;indicate Rx buffer is not full
        bsf Flags,RxBufEmpty ;indicate Rx buffer is empty
        return

;----------------------------------------------
;Add a byte (from WREG) to the end of the transmit buffer

PutTxBuffer:    Bank1           ;select bank 1
        bcf PIE1,TXIE   ;disable transmit interrupt
        Bank0           ;select bank 0
        btfsc   Flags,TxBufFull ;check if buffer full
        goto    ErrTxBufFull    ;and go handle error if full

        BANKISEL TxBuffer   ;bank bit for indirect addressing
        movwf   BufferData  ;save WREG data into BufferData
        movf    TxEndPtr,W  ;get EndPointer
        movwf   FSR     ;and place into FSR
        movf    BufferData,W    ;get BufferData
        movwf   INDF        ;and store in buffer

;test if buffer pointer needs to wrap around to beginning of buffer memory

        movlw   LOW TxBuffer+TX_BUF_LEN-1 ;get last address of buffer
        xorwf   TxEndPtr,W  ;and compare with end pointer
        movlw   LOW TxBuffer    ;load first address of buffer
        btfss   STATUS,Z    ;check if pointer is at last address
        incf    TxEndPtr,W  ;if not then increment pointer
        movwf   TxEndPtr    ;store new end pointer value

;test if buffer is full

        subwf   TxStartPtr,W    ;compare with start pointer
        btfsc   STATUS,Z    ;check if the same
        bsf Flags,TxBufFull ;if so then indicate buffer full
        bcf Flags,TxBufEmpty ;buffer cannot be empty
        Bank1           ;select bank 1
        bsf PIE1,TXIE   ;enable transmit interrupt
        Bank0           ;select bank 0
        return

;error because attempting to store new data and the buffer is full
;can do special error handling here - this code simply ignores the byte

ErrTxBufFull:   Bank1           ;select bank 1
        bsf PIE1,TXIE   ;enable transmit interrupt
        Bank0           ;select bank 0
        return          ;no save of data because buffer full

;----------------------------------------------
;Add a byte (from WREG) to the end of the receive buffer

PutRxBuffer:    Bank0           ;select bank 0
        btfsc   Flags,RxBufFull ;check if buffer full
        goto    ErrRxBufFull    ;and go handle error if full

        BANKISEL RxBuffer   ;bank bit for indirect addressing
        movwf   BufferData  ;save WREG into BufferData
        movf    RxEndPtr,W  ;get EndPointer
        movwf   FSR     ;and place into FSR
        movf    BufferData,W    ;get BufferData
        movwf   INDF        ;store in buffer

;test if buffer pointer needs to wrap around to beginning of buffer memory

        movlw   LOW RxBuffer+RX_BUF_LEN-1 ;get last address of buffer
        xorwf   RxEndPtr,W  ;and compare with end pointer
        movlw   LOW RxBuffer    ;load first address of buffer
        btfss   STATUS,Z    ;check if pointer is at last address
        incf    RxEndPtr,W  ;if not then increment pointer
        movwf   RxEndPtr    ;store new end pointer value

;test if buffer is full

        subwf   RxStartPtr,W    ;compare with start pointer
        btfsc   STATUS,Z    ;check if the same
        bsf Flags,RxBufFull ;if so then indicate buffer full
        bcf Flags,RxBufEmpty ;buffer cannot be empty
        return

;error because attempting to store new data and the buffer is full
;can do special error handling here - this code simply ignores the byte

ErrRxBufFull:   return          ;no save of data because buffer full

;----------------------------------------------
;Remove and return (in WREG) the byte at the start of the transmit buffer

GetTxBuffer:    Bank0           ;select bank 0
        btfsc   Flags,TxBufEmpty ;check if transmit buffer empty
        goto    ErrTxBufEmpty   ;and go handle error if empty

        BANKISEL TxBuffer   ;bank bit for indirect addressing
        movf    TxStartPtr,W    ;get StartPointer
        movwf   FSR     ;and place into FSR

;test if buffer pointer needs to wrap around to beginning of buffer memory

        movlw   LOW TxBuffer+TX_BUF_LEN-1 ;get last address of buffer
        xorwf   TxStartPtr,W    ;and compare with start pointer
        movlw   LOW TxBuffer    ;load first address of buffer
        btfss   STATUS,Z    ;check if pointer is at last address
        incf    TxStartPtr,W    ;if not then increment pointer
        movwf   TxStartPtr  ;store new pointer value
        bcf Flags,TxBufFull ;buffer cannot be full

;test if buffer is now empty

        xorwf   TxEndPtr,W  ;compare start to end   
        btfsc   STATUS,Z    ;check if the same
        bsf Flags,TxBufEmpty ;if same then buffer will be empty
        movf    INDF,W      ;get data from buffer

        return

;error because attempting to read data from an empty buffer
;can do special error handling here - this code simply returns zero

ErrTxBufEmpty:  retlw   0       ;tried to read empty buffer

;----------------------------------------------
;Remove and return (in WREG) the byte at the start of the receive buffer

GetRxBuffer:    Bank0           ;select bank 0
        btfsc   Flags,RxBufEmpty ;check if receive buffer empty
        goto    ErrRxBufEmpty   ;and go handle error if empty

        Bank1           ;select bank 1
        bcf PIE1,RCIE   ;disable receive interrupt
        Bank0           ;select bank 0

        BANKISEL RxBuffer   ;bank bit for indirect addressing
        movf    RxStartPtr,W    ;get StartPointer
        movwf   FSR     ;and place into FSR

;test if buffer pointer needs to wrap around to beginning of buffer memory

        movlw   LOW RxBuffer+RX_BUF_LEN-1 ;get last address of buffer
        xorwf   RxStartPtr,W    ; and compare with start pointer
        movlw   LOW RxBuffer    ;load first address of buffer
        btfss   STATUS,Z    ;check if pointer is at last address
        incf    RxStartPtr,W    ;if not then increment pointer
        movwf   RxStartPtr  ;store new pointer value
        bcf Flags,RxBufFull ;buffer cannot be full

;test if buffer is now empty

        xorwf   RxEndPtr,W  ;compare start to end   
        btfsc   STATUS,Z    ;check if the same
        bsf Flags,RxBufEmpty ;if same then buffer will be empty
        movf    INDF,W      ;get data from buffer

        Bank1           ;select bank 1
        bsf PIE1,RCIE   ;enable receive interrupt
        Bank0           ;select bank 0
        return

;error because attempting to read data from an empty buffer
;can do special error handling here - this code simply returns zero

ErrRxBufEmpty:  Bank1           ;select bank 1
        bsf PIE1,RCIE   ;enable receive interrupt
        Bank0           ;select bank 0
        retlw   0       ;tried to read empty buffer

;----------------------------------------------------------------------------

    END                       ; directive 'end of program'

