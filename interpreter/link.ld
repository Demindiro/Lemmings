OUTPUT_FORMAT("elf64-x86-64")

ENTRY(_start)

PHDRS {
    headers PT_PHDR FILEHDR PHDRS;
	text PT_LOAD FILEHDR PHDRS;
	data PT_LOAD;
	dynamic PT_DYNAMIC;
}

SECTIONS {
	. = SIZEOF_HEADERS ;
	/DISCARD/ : { *(.note* .comment* .gnu.hash* .dynstr* .dynsym* .symtab* .strtab* .shstrtab* .hash* .eh_frame* .interp*) }
	.text    : { *(.text .text.*)     } :text =0xcc
	.rodata  : { *(.rodata .rodata.*) } :text =0
	. = ALIGN(0x1000);
	.data    : { *(.data .data.*)     } :data =0
	.bss     : { *(.bss .bss.*)       } :data =0
	.dynamic : { *(.dynamic)          } :data :dynamic
}
