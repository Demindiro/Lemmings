OUTPUT_FORMAT("binary")

MEMORY {
	rom    (rx) : org = 0xffff0000, l = 0x10000
	gdtr   (r)  : org = 0xfffffef8, l = 8
	gdt    (r)  : org = 0xffffff00, l = 8*4
	init32 (x)  : org = 0xffffff20, l = 0xd0
	init   (x)  : org = 0xfffffff0, l = 16
}

LDFIX_main32 = 0xff20;
LDFIX_gdt = 0xfff00;
LDFIX_gdtr = 0xfef8;

SECTIONS {
	/DISCARD/ : { *(.note .note* .eh_frame) }
	.text : AT(0xffff0000) {
		*(.text .text*)
		*(.rodata .rodata.*)
	} > rom =0
	.gdtr : AT(0xfffffef8) {
		KEEP(*(.gdtr))
	} > gdtr =0
	.gdt : AT(0xffffff00) {
		KEEP(*(.gdt))
	} > gdt =0
	.init32 : AT(0xffffff20) {
		KEEP(*(.init32))
	} > init32 =0
	.init : AT(0xfffffff0) {
		KEEP(*(.init))
	} > init =0
}
