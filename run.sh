#!/bin/sh

set -xe
exec qemu-system-x86_64 \
	-machine q35 \
	-bios /tmp/qemubios.bin \
	-fw_cfg opt/lemmings/boot,file=run.sh \
	-monitor stdio \
	-device isa-debug-exit \
	-debugcon file:/dev/stdout \
	-global isa-debugcon.iobase=0x402 \
	--trace 'fw_cfg*' \
	"$@"

	-nographic \
	--trace 'memory_region_*' \
