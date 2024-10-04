.section .data
simp_7: .dword 0
.section .text
loop_2:
    addi sp, sp, -144
    sd a0, 8(sp)
    sd a1, 16(sp)
    sd a2, 24(sp)
    li t0, 0
    sd t0, 32(sp)
    ld t0, 8(sp)
    ld t1, 32(sp)
    sub t0, t0, t1
    seqz t0, t0
    sd t0, 40(sp)
    ld t1, 40(sp)
    beqz t1, else_0
    ld t0, 16(sp)
    sd t0, 48(sp)
    j end_1
else_0:
    li t0, 1
    sd t0, 56(sp)
    ld t0, 8(sp)
    ld t1, 56(sp)
    sub t0, t0, t1
    sd t0, 64(sp)
    li t0, 3
    sd t0, 72(sp)
    la a0, loop_2
    ld a1, 72(sp)
    sd ra, 88(sp)
    jal miniml_fun_to_paf
    ld ra, 88(sp)
    sd a0, 80(sp)
    ld a0, 80(sp)
    ld a1, 64(sp)
    sd ra, 104(sp)
    jal miniml_apply
    ld ra, 104(sp)
    sd a0, 96(sp)
    ld a0, 96(sp)
    ld a1, 24(sp)
    sd ra, 120(sp)
    jal miniml_apply
    ld ra, 120(sp)
    sd a0, 112(sp)
    ld t0, 16(sp)
    ld t1, 24(sp)
    add t0, t0, t1
    sd t0, 128(sp)
    ld a0, 112(sp)
    ld a1, 128(sp)
    sd ra, 144(sp)
    jal miniml_apply
    ld ra, 144(sp)
    sd a0, 136(sp)
    ld t0, 136(sp)
    sd t0, 48(sp)
end_1:
    ld a0, 48(sp)
    addi sp, sp, 144
    ret
fib_6:
    addi sp, sp, -96
    sd a0, 8(sp)
    li t0, 3
    sd t0, 16(sp)
    la a0, loop_2
    ld a1, 16(sp)
    sd ra, 32(sp)
    jal miniml_fun_to_paf
    ld ra, 32(sp)
    sd a0, 24(sp)
    ld a0, 24(sp)
    ld a1, 8(sp)
    sd ra, 48(sp)
    jal miniml_apply
    ld ra, 48(sp)
    sd a0, 40(sp)
    li t0, 0
    sd t0, 56(sp)
    ld a0, 40(sp)
    ld a1, 56(sp)
    sd ra, 72(sp)
    jal miniml_apply
    ld ra, 72(sp)
    sd a0, 64(sp)
    li t0, 1
    sd t0, 80(sp)
    ld a0, 64(sp)
    ld a1, 80(sp)
    sd ra, 96(sp)
    jal miniml_apply
    ld ra, 96(sp)
    sd a0, 88(sp)
    ld a0, 88(sp)
    addi sp, sp, 96
    ret
.globl _start
_start:
    addi sp, sp, -88
    li t0, 1
    sd t0, 8(sp)
    la a0, fib_6
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
    la t0, simp_7
    ld t1, 80(sp)
    sd t1, 0(t0)
    li a0, 0
    addi sp, sp, 88
    li a0, 0
    jal exit