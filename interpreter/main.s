# Written in assembly as the interpreter is explicitly designed for doing
# sketchy things and compiler UB breaking things is the last thing I want
# to deal with.
#
# == Interpreter details ==
#
# - Two stacks: one for 64-bit integers, another for objects.
#   Both stacks can contain a maximum of 512 entries.
#   They are mapped such that out-of-bounds accesses will trigger a page fault,
#   avoiding the need for explicit bound checks.
# - Objects consist of plain bytes and references.
# - Objects are wholly immutable, as this trivializes GC and compilation.
#   It also improves reasoning over program behavior.
# - Object references are 4 bytes and objects themselves are aligned on a
#   8-byte boundary, enabling up to 32GiB of objects to exist.
# - The interpreter emits machine code directly, as this avoids the need for
#   a separate bytecode interpreter.
#   (why implement an interpreter when the hardware already has one?)
#
# The word dictionary is a very simple zero-terminated list.
#
# The word parser has special handling for quote characters (", ', and `):
# It will include whitespace until a matching quote is found.
# If a matching quote is preceded by an escape character (\), it is ignored.
# The quotes and escape characters are included in the final word.
#
# If a word begins with a digit (0-9), then it is parsed as an integer.
# If a word begins (and ends) with a ", then it is parsed as a string.
# If a word begins (and ends) with a ', then it is parsed as a Unicode character.
#
# It is possible to shrink an object but only if done before allocating another object.
#
# Object references are 32 bits. Object pointers are 64 bits. ptr = base + ref*8
#
# References to objects can be stored in globals.
# Up to 1024 references can be stored.
#
# The stacks, heaps and dictionary are all allocated at runtime.
.intel_syntax noprefix
.globl _start


.set NUM_STACK_HEAD, r15
.set OBJ_STACK_HEAD, r14
.set OBJ_HEAP_HEAD,  r13
.set GLOBALS_HEAD,   r12

.section .bss
rsp_start: .quad 0
obj_heap_base: .quad 0
dict_base: .quad 0
dict_head: .quad 0
fn_read: .quad 0


.macro f name:req, cc:req
 .macro if\name x:req, y:req, target:req
	cmp \x, \y
	j\cc \target
 .endm
 .macro if\name\()z x:req, target:req
	test \x, \x
	j\cc \target
 .endm
.endm
f eq e
f lt b
f gt a
.purgem f

.macro routine name:req
\name:
.endm

.macro string ptr:req, len:req, string:req
 .pushsection .rodata
	.L\@: .ascii "\string"
	.equ .L\@.len, . - .L\@
 .popsection
	lea \ptr, [rip+.L\@]
	mov \len, .L\@.len
.endm

.macro f id:req name:req
 .macro syscall_\name
	call gs:[\id * 8]
 .endm
 .macro sysjmp_\name
	jmp gs:[\id * 8]
 .endm
.endm
f 0 log
f 1 panic
f 2 door_list
f 3 door_register
.purgem f

.macro find_door name:req, api_h:req, api_l:req
 .macro find_door_\name
	movabs rdi, \api_l
	movabs rsi, \api_h
	xor edx, edx
	xor ecx, ecx
	syscall_door_list
 .endm
.endm
find_door framebuffer, 0xd8112085698f85f2, 0xdceefb6d4758a59f

.section .text
# rdi: pointer to syscall routine
#
# This routine does not return
routine _start
	string rdi, rsi, "Greetings from INTERPRETER"
	syscall_log

	find_door_framebuffer
	ifeqz rax, 4f
	lea rdi, [rip + disconnect_framebuffer]
	call [rax]

	mov [rip+rsp_start], rsp
	mov ebx, eax
	mov ecx, 200
2:	mov edx, 200
	mov edi, ecx
	shl edi, 12
	add rdi, rbx
3:	mov eax, -1
	sub al, dl
	sub ah, cl
	mov [rdi], eax
	add rdi, 4
	dec edx
	jnz 3b
	loop 2b
4:	hlt
	jmp parse_input


routine parse_input
.Lparse_input.loop:
	call read_word
	mov ecx, [rsi]
	ifeqz ecx, .Lparse_input.end
	call find_word
	ifeqz rax, word_not_found
	jmp .Lparse_input.loop
.Lparse_input.end:
	ret


# rsi: word
# ecx: word length
routine word_not_found
	push rsi
	push rcx
	#string rsi, ecx, "word not found: "
	#call write
	pop rcx
	pop rsi
	#call writeln
	jmp exit


routine write
	mov rdi, rsi
	mov rsi, rcx
	sysjmp_log

routine writeln
	syscall_log
	string rdi, rsi, "\n"
	sysjmp_log


# Read a word and store it on the heap
#
# rsi: pointer to object
routine read_word
.equ .Lread_word.BUFLEN, 64
	mov ecx, .Lread_word.BUFLEN
	call alloc
	lea rbp, [rsi + .Lread_word.BUFLEN]
.Lread_word.loop:
	ifeq rbx, rbp, .Lread_word.realloc
.Lread_word.loop.postcheck:
	hlt
	#sys_readbyte
	ifltz eax, .Lread_word.endword
	mov [rbx], al
	inc rbx
	jmp .Lread_word.loop
.Lread_word.realloc:
	int3
	ud2
	jmp .Lread_word.loop.postcheck
.Lread_word.endword:
	sub rbp, .Lread_word.BUFLEN
	mov rsi, rbx
	ret


routine find_word
	2: hlt
	jmp 2b


routine alloc
	2: hlt
	jmp 2b


routine disconnect_framebuffer
	ud2


.macro def name:req
routine \name
.endm
.macro enddef
	ret
.endm

def exit
	mov rsp, [rip+rsp_start]
	xor eax, eax
enddef


.macro f name:req size:req
 .section .\name, "a", @nobits
	\name: .zero \size
	\name\().end:
.endm
	f num_stack (1 << 12)
	f obj_stack (1 << 12)
	f obj_heap  (1 << 16)
	f word_dict (1 << 12)
.purgem f
