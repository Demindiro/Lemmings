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
code_head: .quad 0

.section .bss.read
fn_read_word: .quad 0

.section .bss.read_archive
read_archive.door: .quad 0
read_archive.file: .quad 0
read_archive.offset: .quad 0

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


.macro _start_enter
	push rbp
	push rbx
	mov [rip + rsp_start], rsp
.endm
.macro _start_exit
	mov rsp, [rip + rsp_start]
	pop rbx
	pop rbp
.endm


.section .text
# rdi: pointer to syscall routine
routine _start
	_start_enter

	lea NUM_STACK_HEAD, [rip + num_stack.end]
	lea OBJ_STACK_HEAD, [rip + obj_stack.end]
	lea OBJ_HEAP_HEAD, [rip + obj_heap]
	lea rax, [rip + code_heap]
	mov [rip + code_head], rax

	find_door_archive
	assertnez rax, "Failed to find door archive"
	mov [rip + read_archive.door], rax
	mov rbx, rax
	call [rbx + door.archive.root]
	mov rdi, rax
	string rsi, edx, "interpreter.init"
	call [rbx + door.archive.dir_find]
	assertgez rax, "Failed to find interpreter.init"
	mov [rip + read_archive.file], rdx
	lea rax, [rip + read_archive.read_word]
	mov [rip + fn_read_word], rax

.L_start.init_dict:
	lea rsi, [rip + builtins_dict]
	lea rdi, [rip + word_dict]
	lea rbx, [rip + _builtins]
2:	movzx edx, word ptr [rsi]
	ifeq dx, -1, parse_input
	add rsi, 2
	lea rax, [rbx + rdx]
	mov [rdi], rax
	add rdi, 8
	movzx ecx, byte ptr [rsi]
	inc ecx
	rep movsb
	jmp 2b


routine parse_input
.Lparse_input.loop:
	call [rip + fn_read_word]
	ifeqz ecx, .Lparse_input.end
	movzx eax, byte ptr [rsi]
	ifeq al, '"', .Lparse_input.string
	movzx eax, byte ptr [rsi]
	ifeq al, '\'', .Lparse_input.char
	call find_word
	assertnez rax, "Word not found :("
	call rax
	jmp .Lparse_input.loop
.Lparse_input.end:
	ret
.Lparse_input.string:
	sub ecx, 2
	inc rsi
	call str_reserve
	obj_push rdi
	rep movsb
	jmp .Lparse_input.loop
.Lparse_input.char:
	panic "TODO char"


# Allocates on the string heap
#
# rsi: word string
# ecx: word length
routine read_archive.read_word
	mov ecx, MAX_WORD_LEN + 1
	call str_reserve
	mov rdx, rdi
	mov rax, [rip + read_archive.door]
	mov rdi, [rip + read_archive.file]
	mov rsi, [rip + read_archive.offset]
	mov ecx, MAX_WORD_LEN + 1
	call [rax + door.archive.file_read]
	mov rdi, rdx
	mov rsi, rdx
	lea rbp, [rdx + rax]
	ifeq rdi, rbp, .Lread_archive.read_word.eof
.Lread_archive.read_word.loop:
2:	movzx eax, byte ptr [rdi]
	ifeq al, ' ', .Lread_archive.read_word.end
	lea ecx, [eax - '\t']  # [0x9;0xd] => \t \n \v \f \r
	ifleu cl, ('\r' - '\t'), .Lread_archive.read_word.end
	ifeq al, '"', .Lread_archive.read_word.string
	ifeq al, ''', .Lread_archive.read_word.string
	ifeq al, '`', .Lread_archive.read_word.string
	inc rdi
	ifne rdi, rbp, 2b
.Lread_archive.read_word.end:
	# TODO check if at 128 byte limit
	mov ecx, edi
	sub ecx, esi
	lea edx, [ecx + 1]
	add [rip + read_archive.offset], rdx
	mov [rsi - 8], ecx
	ret
.Lread_archive.read_word.string:
	assertne rdi, rbp, "TODO unterminated string"
	inc rdi
2:	movzx edx, byte ptr [rdi]
	inc rdi
	ifne al, dl, 2b
	ifeq rdi, rbp, .Lread_archive.read_word.end
	jmp .Lread_archive.read_word.loop
.Lread_archive.read_word.eof:
	panic "TODO read word eof"

# rsi: string
# ecx: string length
#
# rax: routine
routine find_word
	lea rdi, [rip + word_dict]
2:	mov rax, [rdi]
	add rdi, 8
	ifeqz rax, .Lfind_word.found
	movzx edx, byte ptr [rdi]
	inc rdi
	ifne ecx, edx, 3f
	push rdi
	push rsi
	rep cmpsb
	pop rsi
	pop rdi
	je .Lfind_word.found
3:	add rdi, rdx
	jmp 2b
.Lfind_word.found:
	ret

# ecx: byte count
#
# rdi: ptr
routine str_reserve
	mov rdi, OBJ_HEAP_HEAD
	mov [rdi], rcx
	add rdi, 8
	lea OBJ_HEAP_HEAD, [rdi + rcx]
	ret


.section .rodata.builtins_dict
builtins_dict:

.macro def name:req
 .pushsection .rodata.builtins_dict
	.word \name - _builtins
	.byte .L\@.end - .L\@
	.L\@: .ascii "\name"
	.L\@.end:
 .popsection
	routine \name
.endm
.macro enddef
	ret
.endm

.section .text.builtins

_builtins:

def exit
	_start_exit
	xor eax, eax
enddef

def syslog
	obj_pop rdi
	mov esi, [rdi - 8]
	syscall_log
enddef

.purgem def
.purgem enddef

.section .rodata.builtins_dict
	.word -1
builtins_dict.end:


.macro f name:req size:req
 .section .\name, "a", @nobits
	\name: .zero \size
	\name\().end:
.endm
	f num_stack (1 << 12)
	f obj_stack (1 << 12)
	f obj_heap  (1 << 16)
	f word_dict (1 << 12)
	f code_heap (1 << 16)
.purgem f
