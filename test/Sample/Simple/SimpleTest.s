.section .data
simp_5: .dword 0
.section .text
id_2:
    addi sp, sp, -8
    sd a0, 8(sp)
    ld a0, 8(sp)
    addi sp, sp, 8
    ret
k_4:
    addi sp, sp, -32
    sd a0, 8(sp)
    li t0, 42
    sd t0, 16(sp)
    ld a0, 8(sp)
    ld a1, 16(sp)
    sd ra, 32(sp)
    jal miniml_apply
    ld ra, 32(sp)
    sd a0, 24(sp)
    ld a0, 24(sp)
    addi sp, sp, 32
    ret
.globl _start
_start:
    addi sp, sp, -104
    li t0, 1
    sd t0, 8(sp)
    la a0, k_4
    ld a1, 8(sp)
    sd ra, 24(sp)
    jal miniml_fun_to_paf
    ld ra, 24(sp)
    sd a0, 16(sp)
    li t0, 1
    sd t0, 32(sp)
    la a0, id_2
    ld a1, 32(sp)
    sd ra, 48(sp)
    jal miniml_fun_to_paf
    ld ra, 48(sp)
    sd a0, 40(sp)
    ld a0, 16(sp)
    ld a1, 40(sp)
    sd ra, 64(sp)
    jal miniml_apply
    ld ra, 64(sp)
    sd a0, 56(sp)
    li t0, 1
    sd t0, 72(sp)
    la a0, print_int
    ld a1, 72(sp)
    sd ra, 88(sp)
    jal miniml_fun_to_paf
    ld ra, 88(sp)
    sd a0, 80(sp)
    ld a0, 80(sp)
    ld a1, 56(sp)
    sd ra, 104(sp)
    jal miniml_apply
    ld ra, 104(sp)
    sd a0, 96(sp)
    la t0, simp_5
    ld t1, 96(sp)
    sd t1, 0(t0)
    li a0, 0
    addi sp, sp, 104
    li a0, 0
    jal exit
