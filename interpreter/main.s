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

# set to 127, which is a sensible line length limit
# while also corresponding to exactly 2 cache lines
# minus one byte for word separators (whitespace).
#
# if extra long strings are desired: use concatenation.
.equ MAX_WORD_LEN, 127


.set NUM_STACK_HEAD, r15
.set OBJ_STACK_HEAD, r14
.set OBJ_HEAP_HEAD,  r13

.section .bss
rsp_start: .quad 0
obj_heap_base: .quad 0
dict_base: .quad 0
dict_head: .quad 0
fn_read: .quad 0

.macro defpanic label:req, reason:req
 .pushsection .rodata.panic
	.byte .L\@.end - .L\@
	.L\@: .ascii "\reason"
	.L\@.end:
 .popsection
 .pushsection .text.panic
\label:
	call _panic
 .popsection
.endm

.macro panic reason:req
	defpanic .L\@, "\reason"
	jmp .L\@
.endm

.macro g name:req cc:req
 .macro if\name x:req, y:req, target:req
	cmp \x, \y
	j\cc \target
 .endm
 .macro if\name\()z x:req, target:req
	test \x, \x
	j\cc \target
 .endm
.endm
# \@ doesn't work properly in nested macros :(
# https://lists.gnu.org/archive/html/bug-binutils/2024-11/msg00160.html
.macro h name:req rev_name:req
 .macro assert\name x:req, y:req, reason:req
	defpanic .L_\@, "\reason"
	if\rev_name \x, \y, .L\@
 .endm
 .macro assert\name\()z x:req, reason:req
	defpanic .L\@, "\reason"
	if\rev_name\()z \x, .L\@
 .endm
.endm
.macro f name:req cc:req rev_name:req rev_cc:req
	g \name \cc
	g \rev_name \rev_cc
	#h \name \rev_name
	#h \rev_name \name
.endm
	f eq e ne ne
	f lt l ge ge
	f gt g le le
	f ltu b geu ae
	f gtu a leu be

 # sad!
 .macro _assertcc ifcc:req, x:req, y:req, reason:req
	defpanic .L\@, "\reason"
	\ifcc \x, \y, .L\@
 .endm
 .macro _assertccz ifccz:req, x:req, reason:req
	defpanic .L\@, "\reason"
	\ifccz \x, .L\@
 .endm
 .macro assertge x:req, y:req, reason:req
	_assertcc iflt, \x, \y, "\reason"
 .endm
 .macro assertne x:req, y:req, reason:req
	_assertcc ifeq, \x, \y, "\reason"
 .endm
 .macro asserteq x:req, y:req, reason:req
	_assertcc ifne, \x, \y, "\reason"
 .endm
 .macro assertgez x:req, reason:req
	_assertccz ifltz, \x, "\reason"
 .endm
 .macro assertnez x:req, reason:req
	_assertccz ifeqz, \x, "\reason"
 .endm
 .macro asserteqz x:req, reason:req
	_assertccz ifnez, \x, "\reason"
 .endm

.purgem f
.purgem h
.purgem g

.macro f name:req revcond:req
 .macro panic_\name x:req, y:req, target:req
 .endm
 .macro panic_\name\()z x:req, target:req
	if\name\()z \x, \target
 .endm
.endm
f ne eq
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
find_door archive    , 0x5238e0fc4d60503d, 0x7b357037d5319ae5

.equ door.archive.root, 8 * 0
.equ door.archive.dir_iter, 8 * 1
.equ door.archive.dir_find, 8 * 2
.equ door.archive.file_read, 8 * 3

.macro f prefix:req PREFIX:req
 .macro \prefix\()_push x:req
	sub \PREFIX\()_STACK_HEAD, 8
	mov qword ptr [\PREFIX\()_STACK_HEAD], \x
 .endm
 .macro \prefix\()_pop x:req
	mov \x, qword ptr [\PREFIX\()_STACK_HEAD]
	add \PREFIX\()_STACK_HEAD, 8
 .endm
.endm
	f num NUM
	f obj OBJ
.purgem f


.section .rodata.panic
_panic_reasons:


.section .text.panic
routine _panic
	# determine panic ID
	pop rax
	lea rdx, [rip + _panic_start + 5]
	sub rax, rdx
	# divide by 5: x * 1/5 = x * 204.8/1024
	imul eax, 205
	shr eax, 10
	# find message
	lea rdi, [rip + _panic_reasons]
	movzx esi, byte ptr [rdi]
	inc rdi
	jmp 3f
2:	add rdi, rsi
	movzx esi, byte ptr [rdi]
	inc rdi
	dec eax
3:	ifnez eax, 2b
	syscall_panic
	ud2
_panic_start:



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
