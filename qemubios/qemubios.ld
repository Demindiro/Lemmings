OUTPUT_FORMAT("binary")

MEMORY {
	rom    (rx) : org = 0xf0000, l = 0x10000
	init32 (x)  : org = 0xfff00, l = 0xd0
	gdt    (r)  : org = 0xfffd0, l = 8*3
	gdtr   (r)  : org = 0xfffe8, l = 8
	init   (x)  : org = 0xffff0, l = 16
}

LDFIX_main32 = 0xff00;
LDFIX_gdtr = 0xffe8;

SECTIONS {
	/DISCARD/ : { *(.note .note* .eh_frame) }
	.text : AT(0xf0000) {
		*(.text .text*)
		*(.rodata .rodata.*)
	} > rom =0
	.init32 : AT(0xfff00) {
		KEEP(*(.init32))
	} > init32 =0
	.gdt : AT(0xfffd0) {
		KEEP(*(.gdt))
	} > gdt =0
	.gdtr : AT(0xfffe8) {
		KEEP(*(.gdtr))
	} > gdtr =0
	.init : AT(0xffff0) {
		KEEP(*(.init))
	} > init =0
}
