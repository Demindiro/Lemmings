OUTPUT_FORMAT("binary")

MEMORY {
	rom    (rx) : org = 0xffff0000, l = 0x10000
	gdtr   (r)  : org = 0xffffff88, l = 8
	gdt    (r)  : org = 0xffffff90, l = 8*6
	init32 (x)  : org = 0xffffffc0, l = 0x30
	init   (x)  : org = 0xfffffff0, l = 16
}

LDFIX_main32 = 0xffc0;
LDFIX_gdt = 0xfff90;
LDFIX_gdtr = 0xff88;

SECTIONS {
	/DISCARD/ : { *(.note .note* .eh_frame) } /* FUCK OFF */
	.text : AT(0xffff0000) {
		*(.text .text*)
		*(.rodata .rodata.*)
	} > rom =0
	.gdtr : AT(0xffffff88) {
		KEEP(*(.gdtr))
	} > gdtr =0
	.gdt : AT(0xffffff90) {
		KEEP(*(.gdt))
	} > gdt =0
	.init32 : AT(0xffffffc0) {
		KEEP(*(.init32))
	} > init32 =0
	.init : AT(0xfffffff0) {
		KEEP(*(.init))
	} > init =0
}
