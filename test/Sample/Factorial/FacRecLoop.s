.section .data
simp_6: .dword 0
.section .text
loop_2:
    addi sp, sp, -136
    sd a0, 8(sp)
    sd a1, 16(sp)
    sd a2, 24(sp)
    ld t0, 8(sp)
    ld t1, 16(sp)
    slt t0, t0, t1
    sd t0, 32(sp)
    ld t1, 32(sp)
    beqz t1, else_0
    ld t0, 24(sp)
    sd t0, 40(sp)
    j end_1
else_0:
    li t0, 3
    sd t0, 48(sp)
    la a0, loop_2
    ld a1, 48(sp)
    sd ra, 64(sp)
    jal miniml_fun_to_paf
    ld ra, 64(sp)
    sd a0, 56(sp)
    ld a0, 56(sp)
    ld a1, 8(sp)
    sd ra, 80(sp)
    jal miniml_apply
    ld ra, 80(sp)
    sd a0, 72(sp)
    li t0, 1
    sd t0, 88(sp)
    ld t0, 16(sp)
    ld t1, 88(sp)
    add t0, t0, t1
    sd t0, 96(sp)
    ld a0, 72(sp)
    ld a1, 96(sp)
    sd ra, 112(sp)
    jal miniml_apply
    ld ra, 112(sp)
    sd a0, 104(sp)
    ld t0, 24(sp)
    ld t1, 16(sp)
    mul t0, t0, t1
    sd t0, 120(sp)
    ld a0, 104(sp)
    ld a1, 120(sp)
    sd ra, 136(sp)
    jal miniml_apply
    ld ra, 136(sp)
    sd a0, 128(sp)
    ld t0, 128(sp)
    sd t0, 40(sp)
end_1:
    ld a0, 40(sp)
    addi sp, sp, 136
    ret
factorial_5:
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
    li t0, 1
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
    la a0, factorial_5
    ld a1, 8(sp)
    sd ra, 24(sp)
    jal miniml_fun_to_paf
    ld ra, 24(sp)
    sd a0, 16(sp)
    li t0, 5
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
    la t0, simp_6
    ld t1, 80(sp)
    sd t1, 0(t0)
    li a0, 0
    addi sp, sp, 88
    li a0, 0
    jal exit