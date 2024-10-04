.section .data
simp_3: .dword 0
.section .text
factorial_1:
    addi sp, sp, -104
    sd a0, 8(sp)
    li t0, 0
    sd t0, 16(sp)
    ld t0, 16(sp)
    ld t1, 8(sp)
    slt t0, t0, t1
    seqz t0, t0
    sd t0, 24(sp)
    ld t1, 24(sp)
    beqz t1, else_0
    li t0, 1
    sd t0, 40(sp)
    ld t0, 40(sp)
    sd t0, 32(sp)
    j end_1
else_0:
    li t0, 1
    sd t0, 48(sp)
    ld t0, 8(sp)
    ld t1, 48(sp)
    sub t0, t0, t1
    sd t0, 56(sp)
    li t0, 1
    sd t0, 64(sp)
    la a0, factorial_1
    ld a1, 64(sp)
    sd ra, 80(sp)
    jal miniml_fun_to_paf
    ld ra, 80(sp)
    sd a0, 72(sp)
    ld a0, 72(sp)
    ld a1, 56(sp)
    sd ra, 96(sp)
    jal miniml_apply
    ld ra, 96(sp)
    sd a0, 88(sp)
    ld t0, 8(sp)
    ld t1, 88(sp)
    mul t0, t0, t1
    sd t0, 104(sp)
    ld t0, 104(sp)
    sd t0, 32(sp)
end_1:
    ld a0, 32(sp)
    addi sp, sp, 104
    ret
.globl _start
_start:
    addi sp, sp, -88
    li t0, 1
    sd t0, 8(sp)
    la a0, factorial_1
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
    la t0, simp_3
    ld t1, 80(sp)
    sd t1, 0(t0)
    li a0, 0
    addi sp, sp, 88
    li a0, 0
    jal exit
