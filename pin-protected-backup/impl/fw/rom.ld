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

        /* constructors / destructors. */

        . = ALIGN(4);
        PROVIDE_HIDDEN(_preinit_array_start = .);
        KEEP(*(.preinit_array))
        PROVIDE_HIDDEN(_preinit_array_end = .);

        . = ALIGN(4);
        PROVIDE_HIDDEN(_init_array_start = .);
        KEEP(*(SORT_BY_INIT_PRIORITY(.init_array.*)))
        KEEP(*(.init_array))
        PROVIDE_HIDDEN(_init_array_end = .);

        . = ALIGN(4);
        PROVIDE_HIDDEN(_fini_array_start = .);
        KEEP(*(.fini_array))
        KEEP(*(SORT_BY_INIT_PRIORITY(.fini_array.*)))
        PROVIDE_HIDDEN(_fini_array_end = .);

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

    _sram = ORIGIN(RAM);
    ASSERT((_sram % 64 == 0), "must be 64 byte aligned for optimized clearing")
    _eram = ORIGIN(RAM) + LENGTH(RAM);
    ASSERT((_eram % 64 == 0), "must be 64 byte aligned for optimized clearing")

    _gpio = 0x40000000;

    /DISCARD/ : {
        *(.comment)
    }
}
