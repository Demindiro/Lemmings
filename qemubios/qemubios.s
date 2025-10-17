# While it is possible to enter long mode directly from real mode,
# QEMU doesn't identity map the CS segment which greatly complicates things.
# Hence, we will first enter protected mode using a segment with corresponding
# offset, switch to identity segment then enter long mode directly at the proper address.
.intel_syntax noprefix
.globl sys_exit
.globl sys_open
.globl sys_print
.globl sys_read

.equ SYS_PRINT, 0
.equ SYS_EXIT, 1
.equ SYS_OPEN, 2
.equ SYS_READ, 3

.macro segm base:req, limit:req, access:req, flags:req
 .word \limit & 0xffff
 .word \base & 0xffff
 .byte (\base >> 16) & 0xff
 .byte \access
 .byte ((\limit >> 16) & 0xf) | (\flags << 4)
 .byte (\base >> 24) & 0xff
.endm

.equ FW_CFG_IOBASE, 0x510
.equ FW_CFG.RAM_SIZE, 0x0003
.equ FW_CFG.NB_CPUS , 0x0005
.equ FW_CFG.MAX_CPUS, 0x000f
.equ FW_CFG.FILE_DIR, 0x0019

.equ FW_CFG_DMA.ERROR , 1 << 0
.equ FW_CFG_DMA.READ  , 1 << 1
.equ FW_CFG_DMA.SKIP  , 1 << 2
.equ FW_CFG_DMA.SELECT, 1 << 3
.equ FW_CFG_DMA.WRITE , 1 << 4

.equ PORT_PCI_CMD, 0xcf8
.equ PORT_PCI_DATA, 0xcfc


.macro movimm reg:req, imm:req
 .if \imm < 128
	push \imm
	pop \reg
 .else
  .assert 0, "todo"
 .endif
.endm

.macro movbe32 loc:req, imm:req
 .set .L\@, \imm
	mov dword ptr \loc, ((.L\@ & 0xff) << 24) | ((.L\@ & 0xff00) << 8) | ((.L\@ >> 8) & 0xff00) | ((.L\@ >> 24) & 0xff)
.endm

.macro string ptr:req, len:req, string:req
 .pushsection .rodata
	.L\@: .ascii "\string"
	.L\@.end:
 .popsection
	lea \ptr, [rip + .L\@]
	movimm \len, (.L\@.end - .L\@)
.endm


.section .text
.code64
main64:
mov esp, 4096 - 128 # red zone
mov edx, FW_CFG_IOBASE
xor eax, eax
out dx, ax
inc edx
# for sanity, ensure the machine is QEMU
	movimm rcx, 4
2:	shl eax, 8
	in al, dx
	loop 2b
cmp eax, ('Q'<<24) | ('E'<<16) | ('M'<<8) | 'U'
je is_qemu
hlt # just halt if not, we can't do anything else
is_qemu:
# ensure DMA is available
mov eax, 1
dec edx
out dx, ax
inc edx
in al, dx
test al, 1 << 1
jnz fwcfg_has_dma
hlt
fwcfg_has_dma:
	call boot
	string rsi, rcx, "[QEMUBIOS] Entering kernel\n"
	call sys_print
	lea rdi, [rip + boot_entry_info]
	lea rsi, [rip + sys]
	movabs rdx, 'L' | ('e' << 8) | ('m' << 16) | ('m' << 24) | ('i' << 32) | ('n' << 40) | ('g' << 48) | ('s' << 56)
	jmp rax

# IF=0, DF=0
sys:
	cmp eax, SYS_PRINT
	je sys_print
	cmp eax, SYS_EXIT
	je sys_exit
	cmp eax, SYS_OPEN
	je sys_open
	cmp eax, SYS_READ
	je sys_read
	string rsi, rcx, "[QEMUBIOS] Invalid system call! Halting...\n"
	call sys_print
	hlt

# rsi: string base
# rcx: string length
sys_print:
	mov dx, 0x402
	rep outsb
	ret

# edx: exit status
sys_exit:
	test edx, edx
	jnz 2f
	mov dx, 0x604
	mov ax, 0x2000
	jmp 3f
2:	mov dx, 0x501
	mov ax, 1
3:	out dx, ax
	hlt

# rsi: filename base
# ecx: filename length
# 256 bytes of stack space available during call
#
# eax: size if found, -1 if not found
sys_open:
	push rbp
	push rbx
	# name[56] is null-terminated ASCII...
	# I hate null-terminated strings!
	cmp ecx, 56
	jae 4f
	sub rsp, 56
	mov rdi, rsp
	push rcx
	xor eax, eax
	movimm rcx, 56/8
	rep stosq
	pop rcx
	mov rdi, rsp
	rep movsb
	mov rdi, rsp
	# prepare DMA
	sub rsp, 64
	mov rsi, rsp
	bswap rsi # I hate big endian!
	push rsi
	bswap rsi
	sub rsp, 8
	mov rbp, rsp
	# get count
	movbe32 [rbp+4], 4
	movbe32 [rbp+0], (FW_CFG.FILE_DIR << 16) | FW_CFG_DMA.READ | FW_CFG_DMA.SELECT
	call fw_cfg_dma
	mov ebx, [rsi]
	bswap ebx
	test ebx, ebx
	jz 4f
	# iterate entries
	add rsi, 8 # we only care about the name
2:	movbe32 [rbp+4], 64
	movbe32 [rbp+0], FW_CFG_DMA.READ
	call fw_cfg_dma
	mov ecx, 56/8
	push rsi
	push rdi
	rep cmpsq
	pop rdi
	pop rsi
	je 3f
	dec ebx
	jnz 2b
4:	movimm rax, -1
	jmp 5f
3:	mov ax, [rsi-8+4]
	xchg al, ah
	mov dx, FW_CFG_IOBASE
	out dx, ax
	mov eax, [rsi-8+0]
	bswap eax
5:	add rsp, 16 + 64 + 56
	pop rbx
	pop rbp
	ret

# rdi: buffer base
# ecx: buffer size
# 32 bytes of stack space available during call
sys_read:
	push rbp
	bswap rdi
	push rdi
	sub rsp, 8
	mov rbp, rsp
	bswap ecx
	mov [rbp+4], ecx
	movbe32 [rbp+0], FW_CFG_DMA.READ
	call fw_cfg_dma
	add rsp, 16
	pop rbp
	ret


# rbp: base
fw_cfg_dma:
	# high
	mov dx, FW_CFG_IOBASE + 4
	mov rax, rbp
	bswap rax
	out dx, eax
	# low
	add edx, 4
	mov eax, ebp
	bswap eax
	out dx, eax
	# wait until complete
2:	test dword ptr [rbp], ~(1 << 24)
	jnz 2b
	ret


.section .init32, "ax"
.equ PML4_BASE, 0x1000
.equ PDPT_BASE, 0x2000
.equ PD_BASE  , 0x3000
.code32
main32:
mov eax, PD_BASE
mov dword ptr [eax + 8*0], 0 | 0b11100011 # PS, A, R/W, P
#mov dword ptr [eax + 8*0+4], 1 << 31 # XD
mov dword ptr [PDPT_BASE + 8*0], PD_BASE | 0b00100011 # A, R/W, P
mov eax, PML4_BASE
mov dword ptr [eax + 8*0], PDPT_BASE | 0b00100011 # A, R/W, P
mov cr3, eax
mov eax, 0b10100000 # PAE, PGE
mov cr4, eax
mov ecx, 0xc0000080 # IA32_EFER
rdmsr
or eax, (1 << 11) | (1 << 8) | (1 << 0) # NXE, LME, SCE
wrmsr
mov eax, 0x80000001 # PG, PE
mov cr0, eax
jmp (1*8):main64


.section .gdt, "a"
gdt:
segm 0, 0, 0, 0 # null
segm 0, 0xfffff, 0b10011011, 0b1010 # code64 (access: P, S, E, RW, A) (flags: G, L)
segm 0xf0000, 15, 0b10011011, 0b1100 # code32 (access: P, S, E, RW, A) (flags: G, DS)
gdt_end:

.section .gdtr, "a"
gdtr:
.word gdt_end - gdt - 1
.long gdt

.section .init, "ax"
.code16
_start:
lgdt cs:[LDFIX_gdtr]
mov al, 1 # PE
mov cr0, eax
jmp (2*8):LDFIX_main32
