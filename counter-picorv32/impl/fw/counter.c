#include "drivers.h"
#include <stddef.h>

#define DIVISOR 6

uint32_t *state = (uint32_t *) FRAM_BASE;

// API
#define CMD_ADD (0x01) // (4 bytes in little-endian encoding) -> ()
#define CMD_GET (0x02) // () -> (4 bytes in little-endian encoding)

void do_add();
void do_get();

void main() {
    uart_init(UART1, DIVISOR);
    uint8_t cmd = uart_read(UART1);
    switch (cmd) {
    case CMD_ADD:
      do_add();
      break;
    case CMD_GET:
      do_get();
      break;
    default:
      break;
    }
    poweroff(); // done, don't interact with world any more until next reset
}

void do_add() {
  uint32_t n = 0;
  for (int i = 0; i < 4; i++) {
    n |= ((uint32_t) uart_read(UART1)) << (8 * i);
  }
  uint32_t value = *state;
  uint32_t next = value + n;
  uint32_t not_overflow = next >= value;
  uint32_t mask = (not_overflow - 1);
  next |= mask;
  *state = next;
  uart_write(UART1, 0x01);
}

void do_get() {
  uint32_t n = *state;
  for (int i = 0; i < 4; i++) {
    uint32_t v = (n >> (8 * i)) & 0xff;
    uart_write(UART1, (uint8_t) v);
  }
}
