OUTPUT_FORMAT("binary")

MEMORY {
	rom (rx) : org = 0x0000, l = 0x10000
}

SECTIONS {
	/DISCARD/ : { *(.note .note*) } /* FUCK OFF */
	.text : { *(.text .text*) . = 0x10000; } > rom
}
