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

; below are the values for 50 baud

;#define tmr0_reload     d'100'
;#define tmr0_options    b'11010110' ; internal clock source, prescaler 1:128

RTTY_BUF_LEN    EQU .80 ;length of RTTY buffer

;----------------------------------------------------------------------------
;Constants

SPBRG_VAL   EQU .25     ;set baud rate 9600 for 4Mhz clock
TX_BUF_LEN  EQU .40     ;length of transmit circular buffer
RX_BUF_LEN  EQU TX_BUF_LEN  ;length of receive circular buffer

;----------------------------------------------------------------------------
;Bit Definitions

TxBufFull   EQU 0       ;bit indicates Tx buffer is full
TxBufEmpty  EQU 1       ;bit indicates Tx buffer is empty
RxBufFull   EQU 2       ;bit indicates Rx buffer is full
RxBufEmpty  EQU 3       ;bit indicates Rx buffer is empty
ReceivedCR  EQU 4       ;bit indicates <CR> character received
RxInGGA     EQU 5       ;bit indicates GGA message is being extracted
RxGoodGGA   EQU 6       ;bit indicates good GGA message received
ZS          EQU 7       ;bit indicates leading zero suppression

;----------------------------------------------------------------------------
;Variables

        CBLOCK  0x70
        WREG_TEMP       ;storage for WREG during interrupt
        STATUS_TEMP     ;storage for STATUS during interrupt
        PCLATH_TEMP     ;storage for PCLATH during interrupt
        FSR_TEMP        ;storage for FSR during interrupt
        ENDC

        CBLOCK  0x20
        tx_char         ;RTTY transmit character
        RTTY_ptr        ;pointer for writes to RTTY string
        bit_count       ;count remaining bits in the transmit character
        tx_ptr          ;pointer to the character in transmit string
        index           ;index for copying strings
        GGA_field       ;field counter for GPGGA message
        field_ptr       ;write pointer for saved fields
        field_r_ptr     ;read pointer for saved fields
        GGA_time_field:.10      ;time field from GPGGA message
        GGA_lat_field:.11       ;lat field from GPGGA message
        GGA_NS_field:.2         ;NS field from GPGGA message
        GGA_long_field:.12      ;long field from GPGGA message
        GGA_EW_field:.2         ;EW field from GPGGA message
        GGA_quality_field:.2    ;quality field from GPGGA message
        GGA_numSV_field:.4      ;numSV field from GPGGA message
        GGA_alt_field:.8        ;alt field from GPGGA message
        GGA_cs_field:.3         ;cs field from GPGGA message
        NMEA_cs         ;NMEA message rolling checksum
        seq_hi          ;sequence ID high byte
        seq_lo          ;sequence ID low byte
        hiB
        lowB
        dec_no
        CRC16_high
        CRC16_low
        CRC16_index
        Flags           ;byte to store indicator flags
        tempH           ;temporary data in main routines
        tempL           ;temporary data in main routines
        BufferData      ;temporary data in buffer routines 
        TxStartPtr      ;pointer to start of data in TX buffer
        TxEndPtr        ;pointer to end of data in TX buffer
        RxStartPtr      ;pointer to start of data in RX buffer
        RxEndPtr        ;pointer to end of data in RX buffer
        ENDC

        CBLOCK  0xA0
        RTTYBuffer:RTTY_BUF_LEN ;RTTY transmit buffer
        ENDC

        CBLOCK  0x120
        TxBuffer:TX_BUF_LEN ;transmit data buffer
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
    BANKISEL RTTYBuffer
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

; Table consisting of values 10,000, 1000, 100, 10, and 1.
decade
    addwf   PCL,f
    retlw   d'39'   ; 10,000
    retlw   d'16'
    retlw   d'3'    ; 1,000
    retlw   d'232'
    retlw   d'0'    ; 100
    retlw   d'100'
    retlw   d'0'    ; 10
    retlw   d'10'
    retlw   d'0'    ; 1
    retlw   d'1'

;**********************************************************************
Main
    ; set I/O pin RA5 (RTTY Tx) as output
    banksel TRISA   ; not bank 0
    bcf     TRISA,5
    clrf    TRISC

    ; configure timer 0 options
    ; same bank as TRISA so no banksel
    movlw   tmr0_options
    movwf   OPTION_REG

    ; set up serial port and buffers
    call    SetupSerial

    banksel TMR0    ; restore bank 0

    clrf    seq_hi
    clrf    seq_lo
    clrf    GGA_field

    ;initialise fields as empty
    movlw   A','
    movwf   GGA_time_field
    movwf   GGA_lat_field
    movwf   GGA_NS_field
    movwf   GGA_long_field
    movwf   GGA_EW_field
    movwf   GGA_quality_field
    movwf   GGA_numSV_field
    movwf   GGA_alt_field

main_loop

    ; if there is any Rx character then process it
    btfsc   Flags,RxBufEmpty
    goto    after_rx_char

    movfw   GGA_field

    addwf   PCL,F
    goto    GGA_hdr_dollar
    goto    GGA_hdr_G
    goto    GGA_hdr_P
    goto    GGA_hdr_G
    goto    GGA_hdr_G
    goto    GGA_hdr_A
    goto    GGA_hdr_comma
    goto    GGA_time
    goto    GGA_lat
    goto    GGA_NS
    goto    GGA_long
    goto    GGA_EW
    goto    GGA_quality
    goto    GGA_numSV
    goto    GGA_HDOP
    goto    GGA_alt
    goto    GGA_uAlt
    goto    GGA_sep
    goto    GGA_uSep
    goto    GGA_diffAge
    goto    GGA_diffStation
    goto    GGA_cs

GGA_hdr_dollar:
    call    GetRxBuffer     ;get data from receive buffer
    xorlw   A'$'            ;compare with '$'
    skpz
    goto    GGA_hdr_reset   ;mismatch so reset
    incf    GGA_field,F     ;match so move on
    clrf    NMEA_cs         ;clear the checksum
    goto    after_rx_char

GGA_hdr_G:
    call    GetRxBuffer     ;get data from receive buffer
    xorwf   NMEA_cs,F       ;update checksum
    xorlw   A'G'            ;compare with 'G'
    skpz
    goto    GGA_hdr_reset   ;mismatch so reset
    incf    GGA_field,F     ;match so move on
    goto    after_rx_char

GGA_hdr_P:
    call    GetRxBuffer     ;get data from receive buffer
    xorwf   NMEA_cs,F       ;update checksum
    xorlw   A'P'            ;compare with 'P'
    skpz
    goto    GGA_hdr_reset   ;mismatch so reset
    incf    GGA_field,F     ;match so move on
    goto    after_rx_char

GGA_hdr_A:
    call    GetRxBuffer     ;get data from receive buffer
    xorwf   NMEA_cs,F       ;update checksum
    xorlw   A'A'            ;compare with 'A'
    skpz
    goto    GGA_hdr_reset   ;mismatch so reset
    incf    GGA_field,F     ;match so move on
    goto    after_rx_char

GGA_hdr_comma:
    call    GetRxBuffer     ;get data from receive buffer
    xorwf   NMEA_cs,F       ;update checksum
    xorlw   A','            ;compare with ','
    skpz
    goto    GGA_hdr_reset   ;mismatch so reset
    incf    GGA_field,F     ;match so move on
    movlw   GGA_time_field  ;point to start of ...
    movwf   field_ptr       ;next field
    bsf     PORTC,3
    bsf     Flags,RxInGGA   ;indicate that GGA fields are being extracted
    goto    after_rx_char

GGA_time:
    call    GetRxBuffer     ;get data from receive buffer
    xorwf   NMEA_cs,F       ;update checksum
    movwf   tempL           ;save data
    BANKISEL GGA_time_field
    movfw   field_ptr       ;retrieve the destination pointer
    movwf   FSR             ;and set for writing
    movf    tempL,W         ;restore data
    movwf   INDF            ;write data
    incf    field_ptr,f     ;advance the destination pointer
    xorlw   A','            ;compare with ','
    skpz
    goto    after_rx_char   ;mismatch so just continue
    incf    GGA_field,F     ;match so move on
    movlw   GGA_lat_field   ;point to start of ...
    movwf   field_ptr       ;next field
    goto    after_rx_char

GGA_lat:
    call    GetRxBuffer     ;get data from receive buffer
    xorwf   NMEA_cs,F       ;update checksum
    movwf   tempL           ;save data
    BANKISEL GGA_lat_field
    movfw   field_ptr       ;retrieve the destination pointer
    movwf   FSR             ;and set for writing
    movf    tempL,W         ;restore data
    movwf   INDF            ;write data
    incf    field_ptr,f     ;advance the destination pointer
    xorlw   A','            ;compare with ','
    skpz
    goto    after_rx_char   ;mismatch so just continue
    incf    GGA_field,F     ;match so move on
    movlw   GGA_NS_field    ;point to start of ...
    movwf   field_ptr       ;next field
    goto    after_rx_char

GGA_NS:
    call    GetRxBuffer     ;get data from receive buffer
    xorwf   NMEA_cs,F       ;update checksum
    movwf   tempL           ;save data
    BANKISEL GGA_NS_field
    movfw   field_ptr       ;retrieve the destination pointer
    movwf   FSR             ;and set for writing
    movf    tempL,W         ;restore data
    movwf   INDF            ;write data
    incf    field_ptr,f     ;advance the destination pointer
    xorlw   A','            ;compare with ','
    skpz
    goto    after_rx_char   ;mismatch so just continue
    incf    GGA_field,F     ;match so move on
    movlw   GGA_long_field  ;point to start of ...
    movwf   field_ptr       ;next field
    goto    after_rx_char

GGA_long:
    call    GetRxBuffer     ;get data from receive buffer
    xorwf   NMEA_cs,F       ;update checksum
    movwf   tempL           ;save data
    BANKISEL GGA_long_field
    movfw   field_ptr       ;retrieve the destination pointer
    movwf   FSR             ;and set for writing
    movf    tempL,W         ;restore data
    movwf   INDF            ;write data
    incf    field_ptr,f     ;advance the destination pointer
    xorlw   A','            ;compare with ','
    skpz
    goto    after_rx_char   ;mismatch so just continue
    incf    GGA_field,F     ;match so move on
    movlw   GGA_EW_field    ;point to start of ...
    movwf   field_ptr       ;next field
    goto    after_rx_char

GGA_EW:
    call    GetRxBuffer     ;get data from receive buffer
    xorwf   NMEA_cs,F       ;update checksum
    movwf   tempL           ;save data
    BANKISEL GGA_EW_field
    movfw   field_ptr       ;retrieve the destination pointer
    movwf   FSR             ;and set for writing
    movf    tempL,W         ;restore data
    movwf   INDF            ;write data
    incf    field_ptr,f     ;advance the destination pointer
    xorlw   A','            ;compare with ','
    skpz
    goto    after_rx_char   ;mismatch so just continue
    incf    GGA_field,F     ;match so move on
    movlw   GGA_quality_field ;point to start of ...
    movwf   field_ptr       ;next field
    goto    after_rx_char

GGA_quality:
    call    GetRxBuffer     ;get data from receive buffer
    xorwf   NMEA_cs,F       ;update checksum
    movwf   tempL           ;save data
    BANKISEL GGA_quality_field
    movfw   field_ptr       ;retrieve the destination pointer
    movwf   FSR             ;and set for writing
    movf    tempL,W         ;restore data
    movwf   INDF            ;write data
    incf    field_ptr,f     ;advance the destination pointer
    xorlw   A','            ;compare with ','
    skpz
    goto    after_rx_char   ;mismatch so just continue
    incf    GGA_field,F     ;match so move on
    movlw   GGA_numSV_field ;point to start of ...
    movwf   field_ptr       ;next field
    goto    after_rx_char

GGA_numSV:
    call    GetRxBuffer     ;get data from receive buffer
    xorwf   NMEA_cs,F       ;update checksum
    movwf   tempL           ;save data
    BANKISEL GGA_numSV_field
    movfw   field_ptr       ;retrieve the destination pointer
    movwf   FSR             ;and set for writing
    movf    tempL,W         ;restore data
    movwf   INDF            ;write data
    incf    field_ptr,f     ;advance the destination pointer
    xorlw   A','            ;compare with ','
    skpz
    goto    after_rx_char   ;mismatch so just continue
    incf    GGA_field,F     ;match so move on
    movlw   GGA_alt_field   ;point to start of ...
    movwf   field_ptr       ;next field
    goto    after_rx_char

GGA_alt:
    call    GetRxBuffer     ;get data from receive buffer
    xorwf   NMEA_cs,F       ;update checksum
    movwf   tempL           ;save data
    BANKISEL GGA_alt_field
    movfw   field_ptr       ;retrieve the destination pointer
    movwf   FSR             ;and set for writing
    movf    tempL,W         ;restore data
    movwf   INDF            ;write data
    incf    field_ptr,f     ;advance the destination pointer
    xorlw   A','            ;compare with ','
    skpz
    goto    after_rx_char   ;mismatch so just continue
    incf    GGA_field,F     ;match so move on
    movlw   GGA_cs_field    ;point to start of ...
    movwf   field_ptr       ;next field
    goto    after_rx_char

GGA_HDOP:
GGA_uAlt:
GGA_sep:
GGA_uSep:
GGA_diffAge:
    call    GetRxBuffer     ;get data from receive buffer
    xorwf   NMEA_cs,F       ;update checksum
    xorlw   A','            ;compare with ','
    skpz
    goto    after_rx_char   ;mismatch so just continue
    incf    GGA_field,F     ;match so move on
    goto    after_rx_char

GGA_diffStation:
    call    GetRxBuffer     ;get data from receive buffer
    xorlw   A'*'            ;compare with '*'
    skpz
    goto    after_rx_char   ;mismatch so just continue
    incf    GGA_field,F     ;match so move on
    goto    after_rx_char

GGA_cs:
    call    GetRxBuffer     ;get data from receive buffer
    movwf   tempL           ;save data
    BANKISEL GGA_cs_field
    movfw   field_ptr       ;retrieve the destination pointer
    movwf   FSR             ;and set for writing
    movf    tempL,W         ;restore data
    movwf   INDF            ;write data
    incf    field_ptr,f     ;advance the destination pointer
    xorlw   0x0D            ;compare with <CR>
    skpz
    goto    after_rx_char   ;mismatch so just continue
    ; FIXME should validate checksum
    bsf     Flags,RxGoodGGA ;indicate that a good GGA has been received
    goto    GGA_hdr_reset

GGA_hdr_reset:
    clrf    GGA_field
    bcf     PORTC,3
    bcf     Flags,RxInGGA   ;indicate that GGA fields are not being extracted
    goto    after_rx_char

after_rx_char
    ; loop while RTTY transmission is in progress
    btfsc   INTCON,T0IE
    goto    main_loop

    ; loop while GGA reception is in progress
    btfsc   Flags,RxInGGA
    goto    main_loop

    ; loop until a good GGA has been received
    btfss   Flags,RxGoodGGA
    goto    main_loop

    ; clear flag now that it is being processed
    bcf     Flags,RxGoodGGA

    ; increment sequence ID
    incfsz  seq_lo,F
    goto    done_sequence
    incf    seq_hi,F
done_sequence

    ; start building a test message in RTTY buffer
    movlw   RTTYBuffer
    movwf   RTTY_ptr    ; set pointer to start of buffer

    ; start with two $ characters
    movlw   a'$'
    call    PutRTTYChar
    call    PutRTTYChar

    ; the CRC excludes the leading '$' so initialise now
    call    CRC_init

    ; copy the callsign
    clrf    index       ;start at beginning
copy_callsign
    movfw   index
    call    callsign
    addlw   0
    skpnz
    goto    copied_callsign
    call    PutRTTYChar
    incf    index,f
    goto    copy_callsign

copied_callsign

    ; add a comma separator
    movlw   a','
    call    PutRTTYChar

    ; add the sequence ID as ASCII decimal
    movfw   RTTY_ptr
    movwf   FSR
    movfw   seq_hi
    movwf   hiB
    movfw   seq_lo
    movwf   lowB
    call    PRDEC
    movfw   FSR
    movwf   RTTY_ptr
    incf    RTTY_ptr,f

    ; add a comma separator
    movlw   a','
    call    PutRTTYChar

    ; copy the time field up to decimal or comma (in case empty)
    movlw   GGA_time_field
    movwf   field_r_ptr
copy_time
    call    GetFieldChar
    movwf   tempL
    xorlw   A','    ;compare with ','
    skpnz
    goto    copied_time
    movfw   tempL
    xorlw   A'.'    ;compare with '.'
    skpnz
    goto    copied_time
    movfw   tempL
    call    PutRTTYChar
    goto    copy_time

copied_time

    ; add a comma separator
    movlw   a','
    call    PutRTTYChar

    ; if NS is 'S' then insert '-'
    movfw   GGA_NS_field
    xorlw   A'S'    ;compare with 'S'
    skpz
    goto    done_NS
    movlw   a'-'
    call    PutRTTYChar

done_NS

    ; copy the lat field up to comma
    movlw   GGA_lat_field
    movwf   field_r_ptr
copy_lat
    call    GetFieldChar
    movwf   tempL
    xorlw   A','    ;compare with ','
    skpnz
    goto    copied_lat
    movfw   tempL
    call    PutRTTYChar
    goto    copy_lat

copied_lat

    ; add a comma separator
    movlw   a','
    call    PutRTTYChar

    ; if EW is 'W' then insert '-'
    movfw   GGA_EW_field
    xorlw   A'W'    ;compare with 'W'
    skpz
    goto    done_EW
    movlw   a'-'
    call    PutRTTYChar

done_EW

    ; copy the long field up to comma
    movlw   GGA_long_field
    movwf   field_r_ptr
copy_long
    call    GetFieldChar
    movwf   tempL
    xorlw   A','    ;compare with ','
    skpnz
    goto    copied_long
    movfw   tempL
    call    PutRTTYChar
    goto    copy_long

copied_long

    ; add a comma separator
    movlw   a','
    call    PutRTTYChar

    ; copy the alt field up to decimal or comma (in case empty)
    movlw   GGA_alt_field
    movwf   field_r_ptr
copy_alt
    call    GetFieldChar
    movwf   tempL
    xorlw   A','    ;compare with ','
    skpnz
    goto    copied_alt
    movfw   tempL
    xorlw   A'.'    ;compare with '.'
    skpnz
    goto    copied_alt
    movfw   tempL
    call    PutRTTYChar
    goto    copy_alt

copied_alt

    ; add a comma separator
    movlw   a','
    call    PutRTTYChar

    ; copy the quality field up to comma
    movlw   GGA_quality_field
    movwf   field_r_ptr
copy_quality
    call    GetFieldChar
    movwf   tempL
    xorlw   A','    ;compare with ','
    skpnz
    goto    copied_quality
    movfw   tempL
    call    PutRTTYChar
    goto    copy_quality

copied_quality

    ; add a comma separator
    movlw   a','
    call    PutRTTYChar

    ; copy the numSV field up to comma
    movlw   GGA_numSV_field
    movwf   field_r_ptr
copy_numSV
    call    GetFieldChar
    movwf   tempL
    xorlw   A','    ;compare with ','
    skpnz
    goto    copied_numSV
    movfw   tempL
    call    PutRTTYChar
    goto    copy_numSV

copied_numSV

    movfw   RTTY_ptr
    movwf   FSR
    ; checksum delimiter
    ; this is not included in CRC so write it directly
    movlw   A'*'
    movwf   INDF
    ; print CRC as ASCII HEX
    movfw   CRC16_high
    call    PRHEX
    movfw   CRC16_low
    call    PRHEX
    movfw   FSR
    movwf   RTTY_ptr
    incf    RTTY_ptr,f

    ; CR for testing
    movlw   0x0D
    call    PutRTTYChar

    ; newline and zero terminator
    movlw   0x0A
    call    PutRTTYChar
    movlw   0x0
    call    PutRTTYChar

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

GetFieldChar
    BANKISEL GGA_time_field ;all the fields are in the same bank
    movfw   field_r_ptr
    movwf   FSR
    movfw   INDF
    incf    field_r_ptr,f
    return

PutRTTYChar
    ;movwf   tempL        ;save data
    ;call    PutTxBuffer     ;put data in transmit buffer
    ;movf    tempL,W      ;restore data
    BANKISEL RTTYBuffer
    movwf   tempL
    movfw   RTTY_ptr
    movwf   FSR
    movfw   tempL
    movwf   INDF
    call    CRC_update
    incf    RTTY_ptr,f
    return

; This routine converts the value in W to 2 ASCII hex digits beginning
; at the address after FSR.
PRHEX:
    movwf   tempL       ; save byte temporarily
    swapf   tempL,w
    ; convert top nibble
    call    convert_nibble
    ; get back bottom nibble
    movf    tempL,w
    ; and convert low nibble
convert_nibble:
    andlw   0x0F
    addlw   0xF6
    btfsc   STATUS,C
    addlw   0x07
    addlw   0x3A
    incf    FSR,f
    movwf   INDF
    return

; This routine accepts a 16-bit number in hiB, lowB and converts it into
; an ASCII text string beginning at the address of FSR.
; The routine destroys the contents of hiB, lowB.
PRDEC:
    bcf     Flags,ZS                ; Clear zs flag.
    clrf    dec_no                  ; Clear decade no.
prdeclp:
    movf    dec_no,w
    call    decade                  ; table lookup
    movwf   tempH                   ; save it
    incf    dec_no,f                ; Get 2nd digit of decade no.
    movf    dec_no,w                ; incremented decade counter
    call    decade                  ; table lookup
    movwf   tempL                   ; save it
    call    sub_it                  ; Divide hiB,lowB by tempH,tempL
    movf    INDF,w                  ; get the result
    btfsc   Flags,ZS                ; If zs = 0 AND digit = 0 then
    goto    prdecnzs
    btfsc   STATUS,2                ; digit is a leading zero to be
    goto    prdecnz
prdecnzs:       ;no leading 0 supression
    movwf   INDF
    movlw   a'0'
    addwf   INDF,f                  ; add ascii '0'
    movf    INDF,w
    call    CRC_update
    incf    FSR,f                   ; Point to next memory location.
    bsf     Flags,ZS                ; First non-zero digit sets zs bit.
prdecnz:
    incf    dec_no,f                ; Next decade.
    movlw   0x08                    ; If dec_no = 8, we're down to ones.
    subwf   dec_no,w
    btfss   STATUS,2
    goto    prdeclp                 ; Otherwise, do next decade.
    incf    dec_no,f                ; Update decade number for d_point.
    movf    lowB,w
    movwf   INDF
    movlw   a'0'
    addwf   INDF,f                  ; add ascii '0'
    movf    INDF,w
    call    CRC_update
    retlw   0x00

; This routine performs division by iterated subtraction. It is efficient
; in this application because the dividend and divisor keep getting smaller
; as BIN_ASC runs, so the quotient is never larger than nine. A general-
; purpose division routine would be slower (and longer).
sub_it:
        clrf    INDF                    ; Clear to track no. of subtractions.
sub_it_loop:
        movf    tempL,w                 ; Subtract LSB.
        subwf   lowB,f
        btfsc   STATUS,0                ; If no borrow, continue w/MSB.
        goto    sub_it_skip
        movlw   0x01                    ; Otherwise borrow from MSB.
        subwf   hiB,f
        btfsc   STATUS,0                ; If borrow causes a carry, then
        goto    sub_it_skip
        incf    hiB,f                   ; add numbers back and return.
        movf    tempL,w
        addwf   lowB,f
        retlw   0x00
sub_it_skip:
        movf    tempH,w                 ; Subtract MSB.
        subwf   hiB,f
        btfsc   STATUS,0                ; If no borrow, subtract again.
        goto    sub_it_skip2
        movf    tempL,w
        addwf   lowB,f                  ; Otherwise, undo the subtraction
        btfsc   STATUS,0                ; by adding entire 16-bit no.
        incf    hiB,f                   ; back together and return.
        movf    tempH,w
        addwf   hiB,f
        retlw   0x00
sub_it_skip2
        incf    INDF,f                  ; No borrow, so do it again.
        goto    sub_it_loop

CRC_init:
    movlw   0xFF
    movwf   CRC16_high
    movwf   CRC16_low
    retlw   0

CRC_update:
    movwf   tempL
    xorwf   CRC16_high,w    ; (a^x):(b^y)
    movwf   CRC16_index       ;
    andlw   0xf0        ; W = (a^x):0
    swapf   CRC16_index,f     ; CRC16_index = (b^y):(a^x)
    xorwf   CRC16_index,f     ; CRC16_index = (a^b^x^y):(a^x) = i2:i1


  ; High byte
    movf    CRC16_index,W
    andlw   0xf0
    xorwf   CRC16_low,W
    movwf   CRC16_high

    rlf CRC16_index,W     ;
    rlf CRC16_index,W     ;
    xorwf   CRC16_high,f
    andlw   0xe0
    xorwf   CRC16_high,f

    swapf   CRC16_index,F
    xorwf   CRC16_index,W
    movwf   CRC16_low

    movf    tempL,W
    return

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

