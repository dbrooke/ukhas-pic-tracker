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

;***** VARIABLE DEFINITIONS

; common locations to all banks 0x70 to 0x7F

cblock 0x70
tx_char         ; RTTY transmit character
bit_count       ; count remaining bits in the transmit character
tx_ptr          ; pointer to the character in transmit string
index           ; index for copying strings
w_temp          ; variable used for context saving
status_temp     ; variable used for context saving
pclath_temp     ; variable used for context saving
endc


;**********************************************************************
    ORG         0x000           ; processor reset vector
    goto        main            ; go to beginning of program


    ORG         0x004           ; interrupt vector location
    movwf       w_temp          ; save off current W register contents
    movf        STATUS,w        ; move status register into W register
    movwf       status_temp     ; save off contents of STATUS register
    movf        PCLATH,w        ; move pclath register into W register
    movwf       pclath_temp     ; save off contents of PCLATH register

    ; is it a timer 0 interrupt ?
    btfss   INTCON,T0IF
    goto    isr_end

    ; reload timer 0
    banksel TMR0
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

    goto    isr_end

null_char
    ; a NULL char terminates the string so disable interrupt
    bcf     INTCON,T0IE
    ; and set the bit count to 1 for next time
    movlw   1
    movwf   bit_count

    goto    isr_end

next_bit
    ; rotate bit into carry and set port pin accordingly
    rrf     tx_char,f
    btfsc   STATUS,C
    bsf     PORTA,5
    btfss   STATUS,C
    bcf     PORTA,5

    goto    isr_end

; insert next handler here

isr_end
    movf        pclath_temp,w   ; retrieve copy of PCLATH register
    movwf       PCLATH          ; restore pre-isr PCLATH register contents
    movf        status_temp,w   ; retrieve copy of STATUS register
    movwf       STATUS          ; restore pre-isr STATUS register contents
    swapf       w_temp,f
    swapf       w_temp,w        ; restore pre-isr W register contents
    retfie                      ; return from interrupt

;**********************************************************************
; constant data and tables

; null terminated callsign string
callsign
    addwf   PCL,f
    dt  "DBTEST",0

;**********************************************************************
main
    ; set I/O pin RA5 (RTTY Tx) as output
    banksel TRISA
    bcf     TRISA,5

    ; configure timer 0 options
    banksel OPTION_REG
    movlw   tmr0_options
    movwf   OPTION_REG

    ; global interrupt enable
    banksel INTCON
    bsf     INTCON,GIE

main_loop
    ; wait while RTTY transmission is in progress
    banksel INTCON
    btfsc   INTCON,T0IE
    goto    main_loop

    ; start building a test message in RAM at 0xA0
    movlw   0xA0
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

    ; set tx pointer to start of message
    movlw   0xA0
    movwf   tx_ptr

    ; reload timer 0
    movlw   tmr0_reload
    movwf   TMR0

    ; enable timer 0 interrupt
    banksel INTCON
    bcf     INTCON,T0IF
    bsf     INTCON,T0IE

    goto    main_loop

    END                       ; directive 'end of program'

