OUTPUT_FORMAT("binary")

MEMORY {
	bss    (rw) : org = 0x10, l = 512-16
	rom    (rx) : org = 0xf0000, l = 0x10000
	init32 (x)  : org = 0xfff00, l = 0xd0
	gdt    (r)  : org = 0xfffd0, l = 8*3
	gdtr   (r)  : org = 0xfffe8, l = 8
	init   (x)  : org = 0xffff0, l = 16
}

pages_base = 0xf0000;
pages_count = 6;
LDFIX_main32 = 0xff00;
LDFIX_gdtr = 0xffe8;

SECTIONS {
	/DISCARD/ : { *(.note .note* .eh_frame) }
	.bss : AT(0x10) {
		*(.bss .bss*)
	} > bss =0
	.pages : AT(pages_base) {
		*(.pages)
	} > rom =0
	.text : AT(pages_base + 0x1000 * pages_count) {
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
