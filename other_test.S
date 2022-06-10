.bss
.text
	._s0: .ascii "-"
.align 4
.global .entry
.entry:
	mov x28, x0
	mov x27, x1
	mov x26, 0
	mov x0, 0
	mov x1, 0
	mov x2, 0
	mov x3, 0
	mov x4, 0
	mov x5, 0
	mov x6, 0
	mov x7, 0
	bl main
	mov x0, 0
	mov x16, 1
	svc 0
// test_io.br:0
// test_io.br:6
putd:
	sub sp, sp, 16
	str x0, [sp, 8]
// test_io.br:7
	sub sp, sp, 32
// test_io.br:8
	strb w0, [sp, 7]
// test_io.br:9
	ldr x0, [sp, 40]
	cmp x0, xzr
	bge . +40
// test_io.br:10
	mov x0, 1
	adr x1, ._s0
	mov x2, 1
	mov x16, 4
	svc 0
	bcc . + 12
	mov x26, x0
	mvn x0, xzr
// test_io.br:11
	ldr x0, [sp, 40]
	mov x8, -1
	mvn x8, x8
	mul x0, x0, x8
	str x0, [sp, 40]
	nop
// test_io.br:13
	ldr x0, [sp, 40]
	cmp x0, xzr
	beq . +164
// test_io.br:14
	nop
	ldr x0, [sp, 40]
	cmp x0, xzr
	ble . +104
// test_io.br:15
	ldr x0, [sp, 40]
	mov x8, 10
	mov x9, x0
	sdiv x0, x0, x8
	mul x0, x0, x8
	sub x0, x9, x0
	add x0, x0, 48
	and x0, x0, 255
	add x1, sp, 8
	ldrsb x2, [sp, 7]
	mvn x2, x2
	add x2, x2, 33
	sub x2, x2, 1
	add x1, x1, x2
	strb w0, [x1]
	sxtb x0, w0
// test_io.br:16
	mov x0, 10
	ldr x1, [sp, 40]
	sdiv x0, x0, x1
	str x0, [sp, 40]
// test_io.br:17
	ldrsb x0, [sp, 7]
	add x0, x0, 1
	and x0, x0, 255
	strb w0, [sp, 7]
	sxtb x0, w0
	b . -112
	nop
// test_io.br:20
	mov x0, 1
	add x1, sp, 8
	add x1, x1, 32
	ldrsb x2, [sp, 7]
	sub x1, x1, x2
	ldrsb x2, [sp, 7]
	mov x16, 4
	svc 0
	bcc . + 12
	mov x26, x0
	mvn x0, xzr
	b . +52
	nop
// test_io.br:21
// test_io.br:22
	mov x0, 48
	add x1, sp, 8
	strb w0, [x1]
	sxtb x0, w0
// test_io.br:23
	mov x0, 1
	add x1, sp, 8
	mov x2, 1
	mov x16, 4
	svc 0
	bcc . + 12
	mov x26, x0
	mvn x0, xzr
	nop
	add sp, sp, 48
	ret
// test_io.br:27
putchar:
	sub sp, sp, 16
	strb w0, [sp, 15]
	mov x0, 1
	add x1, sp, 15
	mov x2, 1
	mov x16, 4
	svc 0
	bcc . + 12
	mov x26, x0
	mvn x0, xzr
	add sp, sp, 16
	ret
// test_io.br:29
strlen:
	sub sp, sp, 16
	str x0, [sp, 8]
// test_io.br:30
	ldr x0, [sp, 8]
	str x0, [sp, 0]
// test_io.br:31
	nop
	ldr x0, [sp, 8]
	ldrsb x0, [x0]
	cmp x0, xzr
	beq . +20
	ldr x0, [sp, 8]
	add x0, x0, 1
	str x0, [sp, 8]
	b . -32
	nop
// test_io.br:32
	ldr x0, [sp, 8]
	ldr x1, [sp, 0]
	sub x0, x0, x1
	add sp, sp, 16
	ret
// test_io.br:35
puts:
	stp fp, lr, [sp, -16]!
	mov fp, sp
	sub sp, sp, 16
	str x0, [sp, 8]
// test_io.br:36
	mov x0, 1
	ldr x1, [sp, 8]
	str x0, [sp, 0]
	ldr x0, [sp, 8]
	bl strlen
	mov x2, x0
	ldr x0, [sp, 0]
	mov x16, 4
	svc 0
	bcc . + 12
	mov x26, x0
	mvn x0, xzr
// test_io.br:37
	mov x0, 10
	bl putchar
	mov sp, fp
	ldp fp, lr, [sp], 16
	ret
// other_test.br:0
// other_test.br:3
main:
	stp fp, lr, [sp, -16]!
	mov fp, sp
// other_test.br:4
	sub sp, sp, 16
	str w0, [sp, 12]
	nop
	ldrsw x0, [sp, 12]
	cmp x0, 100
	bge . +44
// other_test.br:5
	mov x0, 65
	bl putchar
// other_test.br:4
	ldrsw x0, [sp, 12]
	add x0, x0, 1
	mov x8, 65535
	movk x8, 65535, lsl 16
	and x0, x0, x8
	str w0, [sp, 12]
	sxtw x0, w0
	b . -52
	nop
	add sp, sp, 16
// other_test.br:7
	mov x0, 10
	bl putchar
	mov sp, fp
	ldp fp, lr, [sp], 16
	ret
