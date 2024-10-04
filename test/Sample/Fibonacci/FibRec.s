.section .data
simp_3: .dword 0
.section .text
fib_1:
    addi sp, sp, -152
    sd a0, 8(sp)
    li t0, 2
    sd t0, 16(sp)
    ld t0, 8(sp)
    ld t1, 16(sp)
    slt t0, t0, t1
    sd t0, 24(sp)
    ld t1, 24(sp)
    beqz t1, else_0
    ld t0, 8(sp)
    sd t0, 32(sp)
    j end_1
else_0:
    li t0, 1
    sd t0, 40(sp)
    ld t0, 8(sp)
    ld t1, 40(sp)
    sub t0, t0, t1
    sd t0, 48(sp)
    li t0, 1
    sd t0, 56(sp)
    la a0, fib_1
    ld a1, 56(sp)
    sd ra, 72(sp)
    jal miniml_fun_to_paf
    ld ra, 72(sp)
    sd a0, 64(sp)
    ld a0, 64(sp)
    ld a1, 48(sp)
    sd ra, 88(sp)
    jal miniml_apply
    ld ra, 88(sp)
    sd a0, 80(sp)
    li t0, 2
    sd t0, 96(sp)
    ld t0, 8(sp)
    ld t1, 96(sp)
    sub t0, t0, t1
    sd t0, 104(sp)
    li t0, 1
    sd t0, 112(sp)
    la a0, fib_1
    ld a1, 112(sp)
    sd ra, 128(sp)
    jal miniml_fun_to_paf
    ld ra, 128(sp)
    sd a0, 120(sp)
    ld a0, 120(sp)
    ld a1, 104(sp)
    sd ra, 144(sp)
    jal miniml_apply
    ld ra, 144(sp)
    sd a0, 136(sp)
    ld t0, 80(sp)
    ld t1, 136(sp)
    add t0, t0, t1
    sd t0, 152(sp)
    ld t0, 152(sp)
    sd t0, 32(sp)
end_1:
    ld a0, 32(sp)
    addi sp, sp, 152
    ret
.globl _start
_start:
    addi sp, sp, -88
    li t0, 1
    sd t0, 8(sp)
    la a0, fib_1
    ld a1, 8(sp)
    sd ra, 24(sp)
    jal miniml_fun_to_paf
    ld ra, 24(sp)
    sd a0, 16(sp)
    li t0, 10
    sd t0, 32(sp)
    ld a0, 16(sp)
    ld a1, 32(sp)
    sd ra, 48(sp)
    jal miniml_apply
    ld ra, 48(sp)
    sd a0, 40(sp)
    li t0, 1
    sd t0, 56(sp)
    la a0, print_int
    ld a1, 56(sp)
    sd ra, 72(sp)
    jal miniml_fun_to_paf
    ld ra, 72(sp)
    sd a0, 64(sp)
    ld a0, 64(sp)
    ld a1, 40(sp)
    sd ra, 88(sp)
    jal miniml_apply
    ld ra, 88(sp)
    sd a0, 80(sp)
    la t0, simp_3
    ld t1, 80(sp)
    sd t1, 0(t0)
    li a0, 0
    addi sp, sp, 88
    li a0, 0
    jal exit
