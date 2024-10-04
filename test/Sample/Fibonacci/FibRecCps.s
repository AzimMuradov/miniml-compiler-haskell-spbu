.section .data
simp_10: .dword 0
.section .text
id_2:
    addi sp, sp, -8
    sd a0, 8(sp)
    ld a0, 8(sp)
    addi sp, sp, 8
    ret
ll_12:
    addi sp, sp, -48
    sd a0, 8(sp)
    sd a1, 16(sp)
    sd a2, 24(sp)
    ld t0, 16(sp)
    ld t1, 24(sp)
    add t0, t0, t1
    sd t0, 32(sp)
    ld a0, 8(sp)
    ld a1, 32(sp)
    sd ra, 48(sp)
    call miniml_apply
    ld ra, 48(sp)
    sd a0, 40(sp)
    ld a0, 40(sp)
    addi sp, sp, 48
    ret
ll_11:
    addi sp, sp, -136
    sd a0, 8(sp)
    sd a1, 16(sp)
    sd a2, 24(sp)
    sd a3, 32(sp)
    li t0, 2
    sd t0, 40(sp)
    ld t0, 16(sp)
    ld t1, 40(sp)
    sub t0, t0, t1
    sd t0, 48(sp)
    ld a0, 8(sp)
    ld a1, 48(sp)
    sd ra, 64(sp)
    call miniml_apply
    ld ra, 64(sp)
    sd a0, 56(sp)
    li t0, 3
    sd t0, 72(sp)
    la a0, ll_12
    ld a1, 72(sp)
    sd ra, 88(sp)
    call miniml_fun_to_paf
    ld ra, 88(sp)
    sd a0, 80(sp)
    ld a0, 80(sp)
    ld a1, 24(sp)
    sd ra, 104(sp)
    call miniml_apply
    ld ra, 104(sp)
    sd a0, 96(sp)
    ld a0, 96(sp)
    ld a1, 32(sp)
    sd ra, 120(sp)
    call miniml_apply
    ld ra, 120(sp)
    sd a0, 112(sp)
    ld a0, 56(sp)
    ld a1, 112(sp)
    sd ra, 136(sp)
    call miniml_apply
    ld ra, 136(sp)
    sd a0, 128(sp)
    ld a0, 128(sp)
    addi sp, sp, 136
    ret
fib_cps_4:
    addi sp, sp, -232
    sd a0, 8(sp)
    sd a1, 16(sp)
    li t0, 3
    sd t0, 24(sp)
    ld t0, 8(sp)
    ld t1, 24(sp)
    slt t0, t0, t1
    sd t0, 32(sp)
    ld t1, 32(sp)
    beqz t1, else_0
    li t0, 1
    sd t0, 48(sp)
    ld a0, 16(sp)
    ld a1, 48(sp)
    sd ra, 64(sp)
    call miniml_apply
    ld ra, 64(sp)
    sd a0, 56(sp)
    ld t0, 56(sp)
    sd t0, 40(sp)
    j end_1
else_0:
    li t0, 1
    sd t0, 72(sp)
    ld t0, 8(sp)
    ld t1, 72(sp)
    sub t0, t0, t1
    sd t0, 80(sp)
    li t0, 2
    sd t0, 88(sp)
    la a0, fib_cps_4
    ld a1, 88(sp)
    sd ra, 104(sp)
    call miniml_fun_to_paf
    ld ra, 104(sp)
    sd a0, 96(sp)
    ld a0, 96(sp)
    ld a1, 80(sp)
    sd ra, 120(sp)
    call miniml_apply
    ld ra, 120(sp)
    sd a0, 112(sp)
    li t0, 4
    sd t0, 128(sp)
    la a0, ll_11
    ld a1, 128(sp)
    sd ra, 144(sp)
    call miniml_fun_to_paf
    ld ra, 144(sp)
    sd a0, 136(sp)
    li t0, 2
    sd t0, 152(sp)
    la a0, fib_cps_4
    ld a1, 152(sp)
    sd ra, 168(sp)
    call miniml_fun_to_paf
    ld ra, 168(sp)
    sd a0, 160(sp)
    ld a0, 136(sp)
    ld a1, 160(sp)
    sd ra, 184(sp)
    call miniml_apply
    ld ra, 184(sp)
    sd a0, 176(sp)
    ld a0, 176(sp)
    ld a1, 8(sp)
    sd ra, 200(sp)
    call miniml_apply
    ld ra, 200(sp)
    sd a0, 192(sp)
    ld a0, 192(sp)
    ld a1, 16(sp)
    sd ra, 216(sp)
    call miniml_apply
    ld ra, 216(sp)
    sd a0, 208(sp)
    ld a0, 112(sp)
    ld a1, 208(sp)
    sd ra, 232(sp)
    call miniml_apply
    ld ra, 232(sp)
    sd a0, 224(sp)
    ld t0, 224(sp)
    sd t0, 40(sp)
end_1:
    ld a0, 40(sp)
    addi sp, sp, 232
    ret
fib_9:
    addi sp, sp, -88
    sd a0, 8(sp)
    li t0, 2
    sd t0, 16(sp)
    la a0, fib_cps_4
    ld a1, 16(sp)
    sd ra, 32(sp)
    call miniml_fun_to_paf
    ld ra, 32(sp)
    sd a0, 24(sp)
    ld a0, 24(sp)
    ld a1, 8(sp)
    sd ra, 48(sp)
    call miniml_apply
    ld ra, 48(sp)
    sd a0, 40(sp)
    li t0, 1
    sd t0, 56(sp)
    la a0, id_2
    ld a1, 56(sp)
    sd ra, 72(sp)
    call miniml_fun_to_paf
    ld ra, 72(sp)
    sd a0, 64(sp)
    ld a0, 40(sp)
    ld a1, 64(sp)
    sd ra, 88(sp)
    call miniml_apply
    ld ra, 88(sp)
    sd a0, 80(sp)
    ld a0, 80(sp)
    addi sp, sp, 88
    ret
.globl _start
_start:
    addi sp, sp, -88
    li t0, 1
    sd t0, 8(sp)
    la a0, fib_9
    ld a1, 8(sp)
    sd ra, 24(sp)
    call miniml_fun_to_paf
    ld ra, 24(sp)
    sd a0, 16(sp)
    li t0, 10
    sd t0, 32(sp)
    ld a0, 16(sp)
    ld a1, 32(sp)
    sd ra, 48(sp)
    call miniml_apply
    ld ra, 48(sp)
    sd a0, 40(sp)
    li t0, 1
    sd t0, 56(sp)
    la a0, print_int
    ld a1, 56(sp)
    sd ra, 72(sp)
    call miniml_fun_to_paf
    ld ra, 72(sp)
    sd a0, 64(sp)
    ld a0, 64(sp)
    ld a1, 40(sp)
    sd ra, 88(sp)
    call miniml_apply
    ld ra, 88(sp)
    sd a0, 80(sp)
    la t0, simp_10
    ld t1, 80(sp)
    sd t1, 0(t0)
    li a0, 0
    addi sp, sp, 88
    li a0, 0
    call exit