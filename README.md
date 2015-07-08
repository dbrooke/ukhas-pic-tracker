# ukhas-pic-tracker

WARNING! May contain bank switching.

A simple HAB tracker developed as an exercise in learning PIC
assembler.

Interfaces to the data line of an NTX2B transmitter to send minimal
UKHAS format RTTY telemetry strings at 50 baud 7N2 using an interrupt
generated at bit rate by timer 0.

Interfaces to a UBLOX 7 GPS at 9600 baud 8N1 using the EUSART
