#include "drivers.h"

void uart_init(struct uart *uart, uint32_t divisor) {
    uart->div = divisor;
}

uint8_t uart_read(struct uart *uart) {
    uint32_t data;
    do {
        data = uart->data;
    } while (data == 0xffffffff);
    return data & 0xff;
}

void uart_write(struct uart *uart, uint8_t data) {
    while (uart->cts) {
        // wait
    }
    uart->data = data;
    while (uart->status & UART_STATUS_SEND_BUSY) {
        // wait
    }
}

void poweroff() {
    *PWR = 1;
}
