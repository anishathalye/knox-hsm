#ifndef DRIVERS_H
#define DRIVERS_H

#include <stdint.h>
#include <stddef.h>

struct uart {
    volatile uint32_t div;
    volatile uint32_t data;
    volatile uint32_t cts;
    volatile uint32_t status;
};

#define UART1 ((struct uart *) 0x40001000)
#define UART_STATUS_SEND_BUSY (1 << 0)

void uart_init(struct uart *uart, uint32_t divisor);

uint8_t uart_read(struct uart *uart);

void uart_write(struct uart *uart, uint8_t data);

#define PWR ((volatile uint8_t *) 0x40000000)

void poweroff();

#define FRAM_BASE ((volatile uint8_t *) 0x10000000)

struct sha1 {
    volatile uint32_t name0; // 0x0
    volatile uint32_t name1; // 0x1
    volatile uint32_t version; // 0x2
    volatile uint32_t _pad0[5]; // 0x3..0x7
    volatile uint32_t ctrl; // 0x8
    volatile uint32_t status; // 0x9
    volatile uint32_t _pad1[6]; // 0xa..0xf
    volatile uint32_t blocks[16]; // 0x10..0x1f
    volatile uint32_t digest[5]; // 0x20..0x24
    volatile uint32_t _pad2[219]; // 0x25..0xff
};

#define SHA1_CTRL_INIT (1 << 0)
#define SHA1_CTRL_NEXT (1 << 1)
#define SHA1_CTRL_MODE (1 << 2)
#define SHA1_STATUS_READY (1 << 0)
#define SHA1_STATUS_VALID (1 << 1)
#define SHA1_DIGEST_SIZE 20
#define SHA1_BLOCK_SIZE 64

#define SHA1 ((struct sha1 *) 0x40006000)

void sha1_digest(struct sha1 *sha1, const uint8_t *buf, size_t n, uint8_t *digest);

#endif // DRIVERS_H
