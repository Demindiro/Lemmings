#!/bin/sh

. ./config.env

set -xe
exec qemu-system-x86_64 \
	-machine q35 \
	-bios /tmp/qemubios.bin \
	-fw_cfg opt/lemmings/kernel.elf,file="$RUST_TARGET"/release/kernel \
	-monitor stdio \
	-device isa-debug-exit \
	-debugcon file:/dev/stdout \
	-global isa-debugcon.iobase=0x402 \
	-no-reboot \
	-s \
	--trace 'fw_cfg*' \
	--trace 'pci_cfg_*' \
	--trace '*pci*' \
	--trace '*vbe*' \
	--trace '*vga*' \
	-d int \
	"$@"

	-nographic \
	--trace 'memory_region_*' \
