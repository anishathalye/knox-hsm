MEMORY
{
    ROM (rx) : ORIGIN = 0x00000000, LENGTH = 4K,
    RAM (rwx) : ORIGIN = 0x20000000, LENGTH = 4K
}

_stacksz = 0x800;

SECTIONS
{
    /* stack at bottom of ram */
    .stack (NOLOAD) : {
        . = ALIGN(8);
        _sstack = .;
        . = . + _stacksz;
        . = ALIGN(8);
        _estack = .;
        _stack_top = .;
    } > RAM

    /* code */
    .text : {
        . = ALIGN(4);
        _stext = .;

        KEEP(*(.vectors))

        *(.text .text.* .gnu.linkonce.t.*)

        . = ALIGN(4);
        _etext = .;
    } > ROM

    /* constants */
    .rodata : {
        . = ALIGN(4);
        _srodata = .;

        *(.rodata .rodata.* .gnu.linkonce.r.*)

        . = ALIGN(4);
        _erodata = .;
    } > ROM

    /* data */
    _sidata = LOADADDR(.data);
    .data : {
        . = ALIGN(4);
        _sdata = .;

        *(.sdata .sdata.*)
        *(.data .data.*)

        . = ALIGN(4);

        _edata = .;
    } > RAM AT > ROM

    /* bss */
    .bss (NOLOAD) : {
        . = ALIGN(4);
        _sbss = .;

        *(.sbss .sbss.*)
        *(.bss .bss.*)
        *(COMMON)

        . = ALIGN(4);
        _ebss = .;
    } > RAM

    /DISCARD/ : {
        *(.comment)
        *(.riscv.attributes)
        *(.debug_*)
    }
}
