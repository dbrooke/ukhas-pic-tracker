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


;***** VARIABLE DEFINITIONS

; common locations to all banks 0x70 to 0x7F

cblock 0x70
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


; isr code can go here or be located as a call subroutine elsewhere

    movf        pclath_temp,w   ; retrieve copy of PCLATH register
    movwf       PCLATH          ; restore pre-isr PCLATH register contents
    movf        status_temp,w   ; retrieve copy of STATUS register
    movwf       STATUS          ; restore pre-isr STATUS register contents
    swapf       w_temp,f
    swapf       w_temp,w        ; restore pre-isr W register contents
    retfie                      ; return from interrupt


main

; remaining code goes here

    goto        $

    END                       ; directive 'end of program'

