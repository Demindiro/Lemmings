# While it is possible to enter long mode directly from real mode,
# QEMU doesn't identity map the CS segment which greatly complicates things.
# Hence, we will first enter protected mode using a segment with corresponding
# offset, switch to identity segment then enter long mode directly at the proper address.
.intel_syntax noprefix
.globl _start

.equ PML4_BASE, 0x0000
.equ PDPT_BASE, 0x1000
.equ PD_BASE  , 0x2000
.equ PT_BASE  , 0x3000
.equ GDT_BASE , 0x4000
.equ CODE64_BASE, 0x5000
.equ CODE32_BASE, 0xf000

.equ REAL_BIOS_BASE, 0xffff0000

.macro segm base:req, limit:req, access:req, flags:req
 .word \limit & 0xffff
 .word \base & 0xffff
 .byte (\base >> 16) & 0xff
 .byte \access
 .byte ((\limit >> 16) & 0xf) | (\flags << 4)
 .byte (\base >> 24) & 0xff
.endm


.section .text
. = PML4_BASE
.quad (REAL_BIOS_BASE + PDPT_BASE) | 0b00100011
.zero 511*8

. = PDPT_BASE
.quad (REAL_BIOS_BASE + PD_BASE) | 0b00100011   # 0x00000000 = 0<<21
.quad 0
.quad 0
.quad (REAL_BIOS_BASE + PD_BASE) | 0b00100011   # 0xc0000000 = 3<<21
.zero (512-4)*8

. = PD_BASE
.quad 0 | 0b11100011     # 2MiB hugepage to physaddr 0
.zero (512-2)*8
.quad (REAL_BIOS_BASE + PT_BASE) | 0b00100011

. = PT_BASE
.zero (512-16)*8
.rept 16
 .quad (REAL_BIOS_BASE | \+*0x1000) | 0b11100011    # 4KiB page to physaddr 0xfffx000
.endr


. = GDT_BASE
gdt:
segm 0, 0, 0, 0 # null
segm 0, 0xfffff, 0b10011011, 0b1010 # code64 (access: P, S, E, RW, A) (flags: G, L)
segm 0, 0xfffff, 0b10010011, 0b1010 # data64 (access: P, S, RW, A) (flags: G, L)
segm 0, 0xfffff, 0b10011011, 0b1100 # code32 (access: P, S, E, RW, A) (flags: G, DS)
segm 0, 0xfffff, 0b10010011, 0b1100 # data32 (access: P, S, RW, A) (flags: G, DS)
segm 0xffff0000, 16, 0b10011011, 0b1100 # code32 (access: P, S, E, RW, A) (flags: G, DS)
segm (GDT_BASE + (tss - gdt)), 0, 0b10001001, 0b0010 # TSS (access: P, avail) (flags: L) 
.quad (GDT_BASE + (tss - gdt)) >> 32             # high part of TSS
tss:
.zero 4
.zero (4 * 2) * 3 # rsp
.zero 4 * 2
.zero (4 * 2) * 7 # ist
.zero 4 * 2 + 2
.zero . - tss   # iomap base (ALWAYS AT END OF TSS!!)
gdtr:
.word tss - gdt - 1
.long 0xf0000 + GDT_BASE  # I hate segments!


. = CODE64_BASE
.code64
main64:
hlt


. = CODE32_BASE
.code32
main32:
jmp (3*8):REAL_BIOS_BASE + CODE32_BASE + (main32_s - main32)
main32_s:
# https://wiki.osdev.org/Entering_Long_Mode_Directly
mov eax, REAL_BIOS_BASE + PML4_BASE
mov cr3, eax
mov eax, 0b10100000 # PAE, PGE
mov cr4, eax
mov ecx, 0xc0000080 # IA32_EFER
rdmsr
or eax, (1 << 11) | (1 << 8) | (1 << 0) # NXE, LME, SCE
wrmsr
mov eax, 0x80000001 # PG, PE
mov cr0, eax
#mov eax, REAL_BIOS_BASE + CODE16_BASE # tss64
#ltr ax
jmp (1*8):REAL_BIOS_BASE + CODE64_BASE


. = 0xfff0
.code16
_start:
lgdt cs:[gdtr]
mov al, 1 # PE
mov cr0, eax
jmp (5*8):CODE32_BASE
