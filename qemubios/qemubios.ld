OUTPUT_FORMAT("binary")

MEMORY {
	rom    (rx) : org = 0xffff0000, l = 0x10000
	init32 (x)  : org = 0xfffffef8, l = 0xd8
	gdt    (r)  : org = 0xffffffd0, l = 8*3
	gdtr   (r)  : org = 0xffffffe8, l = 8
	init   (x)  : org = 0xfffffff0, l = 16
}

LDFIX_main32 = 0xfef8;
LDFIX_gdt = 0xfffd0;
LDFIX_gdtr = 0xffe8;

SECTIONS {
	/DISCARD/ : { *(.note .note* .eh_frame) }
	.text : AT(0xffff0000) {
		*(.text .text*)
		*(.rodata .rodata.*)
	} > rom =0
	.init32 : AT(0xfffffef8) {
		KEEP(*(.init32))
	} > init32 =0
	.gdt : AT(0xffffffd0) {
		KEEP(*(.gdt))
	} > gdt =0
	.gdtr : AT(0xffffffe8) {
		KEEP(*(.gdtr))
	} > gdtr =0
	.init : AT(0xfffffff0) {
		KEEP(*(.init))
	} > init =0
}
