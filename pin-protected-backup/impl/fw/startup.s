.section .vectors,"ax",%progbits

.global _reset
.type _reset, %function
_reset:
    la sp, _stack_top
    call _preinit
    call _init
    call main
_fell_through:
    j _fell_through
.size _reset, .-_reset

/* stuff we want to do as soon as possible after boot */
.type _preinit, %function
_preinit:
    ret
.size _preinit, .-_preinit

.type _init, %function
_init:
_copy_data:
    la t1, _sidata
    la t2, _sdata
    la t3, _edata
    beq t2, t3, _zero_bss /* no data to copy, proceed to zero bss */
_copy_data_loop:
    lw t0, 0(t1)
    sw t0, 0(t2)
    addi t1, t1, 4
    addi t2, t2, 4
    bne t2, t3, _copy_data_loop
_zero_bss:
    la t1, _sbss
    la t2, _ebss
    beq t1, t2, _init_done
_zero_bss_loop:
    sw zero, 0(t1)
    addi t1, t1, 4
    bne t1, t2, _zero_bss_loop
_init_done:
    ret
.size _init, .-_init
