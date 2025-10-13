#!/bin/sh

# note about debugging 16 bit mode: pay attention to the segment registers!
# In particular: x/i $rip will give nonsense. Use this instead:
#   x/i $cs*16+$rip
# It will attempt to disassemble in 64-bit mode however. Switch architecture with:
#   set architecture i8086
# ... or at least, that ought to work but it doesn't ...

qemu-system-x86_64 \
	-machine q35 \
	-bios /tmp/qemubios.bin \
	-fw_cfg opt/lemmings/kernel.elf,file=/tmp/kernel.elf \
	-device isa-debug-exit \
	-debugcon file:/dev/stdout \
	-global isa-debugcon.iobase=0x402 \
	-nographic \
	-S -s \
	&
gdb -ex 'target remote localhost:1234'
wait
exit

	-d int,cpu,exec \
	--trace 'fw_cfg*' \
	--trace 'memory_region_*' \
	-monitor stdio \
