OUTPUT_FORMAT("elf64-x86-64")
ENTRY(_start)

KERNEL_BASE = 0xffffffffffff0000;

SECTIONS {
	/DISCARD/ : { *(.note .note* .eh_frame) }
	.text : AT(KERNEL_BASE) {
		*(.text .text*)
		*(.rodata .rodata.*)
	} =0
}
