#ifndef DRIVERS_H
#define DRIVERS_H

#include <stdint.h>

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

#define FRAM_BASE ((volatile uint8_t *) 0x10000000)

#define PWR ((volatile uint8_t *) 0x40000000)

void poweroff();

#endif // DRIVERS_H
