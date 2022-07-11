#define SUB_IF_GE(n) \
    li t0, n; \
    sltu t1, a0, t0; \
    addi t1, t1, -1; \
    and t0, t0, t1; \
    sub a0, a0, t0

.globl ct_mod1000000
.type ct_mod1000000, %function
ct_mod1000000:
    SUB_IF_GE(4096000000)
    SUB_IF_GE(2048000000)
    SUB_IF_GE(1024000000)
    SUB_IF_GE( 512000000)
    SUB_IF_GE( 256000000)
    SUB_IF_GE( 128000000)
    SUB_IF_GE(  64000000)
    SUB_IF_GE(  32000000)
    SUB_IF_GE(  16000000)
    SUB_IF_GE(   8000000)
    SUB_IF_GE(   4000000)
    SUB_IF_GE(   2000000)
    SUB_IF_GE(   1000000)
    ret
.size ct_mod1000000, .-ct_mod1000000
