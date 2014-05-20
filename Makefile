# Makefile for PIC microcontrollers using gpasm and pk2cmd

# Specific microcontroller type:
DEVICE=16f690
FAMILY=pic14
PRJ=tracker
# C compiler, assembler, device programmer:
CC=sdcc --use-non-free -m$(FAMILY)
ASM=gpasm
SIM=gpsim
PK2=pk2cmd -B/usr/share/pk2 -Ppic$(DEVICE) -A3.3

All: $(PRJ).hex

$(PRJ).hex: $(PRJ).asm
	$(ASM) -p$(DEVICE) $(PRJ).asm

sim: $(PRJ).hex
	$(SIM) -L . -p $(DEVICE) -s $(PRJ).cod $(PRJ).hex

write: $(PRJ).hex
	$(PK2) -M -F$(PRJ).hex

on:
	$(PK2) -T -R

off:
	$(PK2) -W

erase:
	$(PK2) -E

clean:
	rm -f *.o *.cod *.hex *.lst *.err
