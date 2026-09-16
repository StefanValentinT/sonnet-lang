.text
.balign 4
.globl _get_unit
_get_unit:
	stp	x29, x30, [sp, -16]!
	mov	x29, sp
	ldp	x29, x30, [sp], 16
	ret
/* end function get_unit */

.text
.balign 4
.globl _main
_main:
	stp	x29, x30, [sp, -16]!
	mov	x29, sp
	bl	_get_unit
	mov	w0, #10
	bl	_print_i32
	ldp	x29, x30, [sp], 16
	ret
/* end function main */

