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
.set FLAGS,          r12

.equ FLAG.COMPILE_MODE, 1

.section .bss
rsp_start: .quad 0
obj_heap_base: .quad 0
dict_base: .quad 0
dict_head: .quad 0
code_head: .quad 0

.section .bss.read
fn_read_word: .quad 0
fn_read_byte: .quad 0

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
 .macro assertne x:req, y:req, reason:req
	_assertcc ifeq, \x, \y, "\reason"
 .endm
 .macro asserteq x:req, y:req, reason:req
	_assertcc ifne, \x, \y, "\reason"
 .endm
 .macro assertgt x:req, y:req, reason:req
	_assertcc ifle, \x, \y, "\reason"
 .endm
 .macro assertlt x:req, y:req, reason:req
	_assertcc ifge, \x, \y, "\reason"
 .endm
 .macro assertge x:req, y:req, reason:req
	_assertcc iflt, \x, \y, "\reason"
 .endm
 .macro assertle x:req, y:req, reason:req
	_assertcc ifgt, \x, \y, "\reason"
 .endm
 .macro assertltu x:req, y:req, reason:req
	_assertcc ifgeu, \x, \y, "\reason"
 .endm
 .macro assertgez x:req, reason:req
	_assertccz ifltz, \x, "\reason"
 .endm
 .macro assertlez x:req, reason:req
	_assertccz ifgtz, \x, "\reason"
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


.macro set_bit reg:req, x:req
 .if \x < 8
	or \reg, 1 << (\x)
 .else
	bts \reg, \x
 .endif
.endm
.macro clear_bit reg:req, x:req
 .if \x < 8
	and \reg, ~(1 << (\x))
 .else
	btc \reg, \x
 .endif
.endm


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
	lea rax, [rip + read_archive.read_byte]
	mov [rip + fn_read_byte], rax

.L_start.init_dict:
	lea rsi, [rip + builtins_dict._]
	lea rdi, [rip + word_dict]
	lea rbx, [rip + builtins._]
2:	movzx edx, word ptr [rsi]
	ifeq dx, -1, parse_input
	add rsi, 2
	lea rax, [rbx + rdx]
	mov [rdi], rax
	add rdi, 8
	movzx ecx, byte ptr [rsi]
	and ecx, 0x7f
	inc ecx
	rep movsb
	jmp 2b


routine parse_input
.Lparse_input.loop:
	call read_word
	ifeqz ecx, .Lparse_input.end
	movzx eax, byte ptr [rsi]
	ifeq al, '"', .Lparse_input.string
	ifeq al, '\'', .Lparse_input.char
	sub eax, '0'
	ifltu al, 10, .Lparse_input.number
	push rsi
	call find_word
	pop rsi
	ifeqz rax, .Lparse_input.word_not_found
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
	call parse_char
	num_push rax
	jmp .Lparse_input.loop
.Lparse_input.number:
	call parse_number
	num_push rax
	jmp .Lparse_input.loop
.Lparse_input.word_not_found:
	string rdi, edx, "undefined word: "


# rdi: prefix ptr
# edx: prefix len
# rsi: postfix ptr
# ecx: postfix len
routine panic_prefix
	sub rsp, 256
	push rsi
	push rcx
	mov rsi, rdi
	mov ecx, edx
	lea rdi, [rsp + 16]
	rep movsb
	pop rcx
	pop rsi
	rep movsb
	mov rsi, rdi
	mov rdi, rsp
	sub esi, edi
	syscall_panic


# rsi: string (must start with a "'" !)
# ecx: len
#
# rax: char
routine parse_char
	asserteq (byte ptr [rsi + rcx - 1]), '\'', "char doesn't end with a '"
	asserteq cl, 3, "TODO: non-ascii characters"
	movzx eax, byte ptr [rsi + 1]
	assertle al, 127, "Invalid UTF-8"
	ret

# rsi: string
# ecx: len
#
# rax: number
routine parse_number
	mov rdi, rsi
	add rsi, rcx
	movzx eax, byte ptr [rdi]
	inc rdi
	ifeq rdi, rsi, .Lparse_number.singledigit
	mov edx, eax
	xor eax, eax
	mov ecx, 10
	ifne dl, '0', .Lparse_number.loop
	movzx edx, byte ptr [rdi]
	mov ecx, 16
	ifeq dl, 'x', .Lparse_number.skipprefix
	mov ecx, 2
	ifeq dl, 'b', .Lparse_number.skipprefix
	mov ecx, 8
	ifeq dl, 'o', .Lparse_number.skipprefix
	jmp .Lparse_number.loop
.Lparse_number.singledigit:
	sub eax, '0'
	assertltu al, 10, "decimal out of range"
	ret
.Lparse_number.skipprefix:
	inc rdi
	assertne rdi, rsi, "expected digits after prefix"
.Lparse_number.loop:
	movzx edx, byte ptr [rdi]
	lea ebx, [edx - '0']
	ifltu bl, 10, 2f
	or edx, 040
	lea ebx, [edx - 'a' + 10]
2:	assertlt bl, dl, "digit out of range for base"
	mul ecx
	add rax, rbx
	inc rdi
	ifne rdi, rsi .Lparse_number.loop
.Lparse_number.end:
	ret

# Allocates on the string heap
#
# Never returns an empty string.
#
# rsi: word string (0 if EOF)
# ecx: word length
routine read_word
3:	call [rip + fn_read_word]
	ifeqz rsi, 2f
	ifeqz ecx, 3b
2:	ret

routine read_byte
	jmp [rip + fn_read_byte]

# Allocates on the string heap
#
# May return empty strings.
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
	ifeqz rax, .Lread_archive.eof
	mov rdi, rdx
	mov rsi, rdx
	lea rbp, [rdx + rax]
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
.Lread_archive.eof:
	xor esi, esi
	ret

# eax: character (-1 if EOF)
routine read_archive.read_byte
	push -1
	mov rax, [rip + read_archive.door]
	mov rdi, [rip + read_archive.file]
	mov rsi, [rip + read_archive.offset]
	mov ecx, 1
	mov rdx, rsp
	call [rax + door.archive.file_read]
	pop rax
	ifeqz rdx, 2f
	inc qword ptr [rip + read_archive.offset]
	movzx eax, al
2:	ret

# rsi: string
# ecx: string length
#
# rax: routine (0 if not found)
# edx: 1<<7 if immediate, 0 if not
routine find_word
	lea rdi, [rip + word_dict]
2:	mov rax, [rdi]
	add rdi, 8
	ifeqz rax, .Lfind_word.found # ... or not
	movzx edx, byte ptr [rdi]
	mov ebx, edx
	and ebx, 0x7f
	inc rdi
	ifne ecx, ebx, 3f
	push rdi
	push rsi
	rep cmpsb
	pop rsi
	pop rdi
	mov ecx, ebx
	je .Lfind_word.found
3:	add rdi, rbx
	jmp 2b
.Lfind_word.found:
	and edx, 1<<7
	ret

# rax: routine
# rsi: name
# ecx: name len (< 127!!)
# edx: 1<<7 if immediate, 0 if not
routine set_word
	push rdx
	lea rdi, [rip + word_dict]
	ifeq (qword ptr [rdi]), 0, .Lset_word.new # technically not possible, but be defensive
2:	add rdi, 8
	movzx edx, byte ptr [rdi]
	and edx, 0x7f
	inc rdi
	ifne ecx, edx, 3f
	push rdi
	push rsi
	rep cmpsb
	pop rsi
	pop rdi
	mov ecx, edx
	je .Lset_word.update
3:	add rdi, rdx
	ifne (qword ptr [rdi]), 0, 2b
.Lset_word.new:
	pop rdx
	mov [rdi], rax
	add rdi, 8
	or edx, ecx
	mov [rdi], dl
	inc rdi
	rep movsb
	ret
.Lset_word.update:
	pop rdx
	mov [rdi - 8], rax
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


.macro dict_begin namespace:req
 .section .rodata.builtins_dict.\namespace
builtins_dict.\namespace:

 .macro _def_as name:req, label:req, imm:req
  .pushsection .rodata.builtins_dict.\namespace
	.word \label - builtins.\namespace
	.byte (1001f - 1000f) | (\imm << 7)
	1000: .ascii "\name"
	1001:
  .popsection
	routine \label
 .endm
 .macro def_as name:req, label:req
	_def_as "\name", \label, 0
 .endm
 .macro defimm_as name:req, label:req
	_def_as "\name", \label, 1
 .endm
 .macro def name:req
	def_as \name \name
 .endm
 .macro defimm name:req
	defimm_as \name \name
 .endm
 .macro enddef
	ret
 .endm

 .section .text.builtins.\namespace
builtins.\namespace:
.endm

.macro dict_end namespace:req
 .purgem def
 .purgem defimm
 .purgem def_as
 .purgem defimm_as
 .purgem _def_as
 .purgem enddef

 .section .rodata.builtins_dict.\namespace
	.word -1
builtins_dict.\namespace\().end:
.endm






dict_begin _
	def exit
		_start_exit
		xor eax, eax
	enddef

	def syslog
		obj_pop rdi
		mov esi, [rdi - 8]
		syscall_log
	enddef
dict_end _


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
