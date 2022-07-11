#include "drivers.h"
#include <stddef.h>

#define PIN_LENGTH 4
#define DATA_SIZE 16 // for AES-128 keys
#define NENTRY 4
#define GUESS_LIMIT 10

#define DIVISOR 6

struct __attribute__((packed)) entry {
    volatile uint8_t valid;
    volatile uint8_t bad_guesses;
    uint8_t _pad[2];
    volatile uint8_t pin[PIN_LENGTH];
    volatile uint8_t data[DATA_SIZE];
};

struct entry *entries = (struct entry *) FRAM_BASE;

// API
#define CMD_STATUS (0x01) // (slot) -> bool | is empty?
#define CMD_DELETE (0x02) // (slot) -> ()
#define CMD_STORE (0x03) // (slot, pin, data) -> () | store in slot if empty
#define CMD_RETRIEVE (0x04) // (slot, pin) -> [u8] | get if password matches and there are guesses left

void do_status(uint8_t slot);
void do_delete(uint8_t slot);
void do_store(uint8_t slot);
void do_retrieve(uint8_t slot);

void main() {
    uart_init(UART1, DIVISOR);
    uint8_t cmd = uart_read(UART1);
    uint8_t slot = uart_read(UART1);
    if (slot < NENTRY) {
        switch (cmd) {
            case CMD_STATUS:
                do_status(slot);
                break;
            case CMD_DELETE:
                do_delete(slot);
                break;
            case CMD_STORE:
                do_store(slot);
                break;
            case CMD_RETRIEVE:
                do_retrieve(slot);
                break;
            default:
                break;
        }
    }
    poweroff(); // done, don't interact with world any more until next reset
}

void do_status(uint8_t slot) {
    struct entry *entry = &entries[slot];
    uart_write(UART1, !!entry->valid);
}

void do_delete(uint8_t slot) {
    struct entry *entry = &entries[slot];
    uint8_t was_valid = !!entry->valid;
    entry->valid = 0;
    uart_write(UART1, was_valid);
}

void vmemcpy(volatile uint8_t *dest, const uint8_t *src, size_t sz) {
    for (int i = 0; i < sz; i++) {
        dest[i] = src[i];
    }
}

void do_store(uint8_t slot) {
    uint8_t pin[PIN_LENGTH];
    for (int i = 0; i < PIN_LENGTH; i++) {
        uint8_t digit = uart_read(UART1);
        pin[i] = digit;
    }
    uint8_t data[DATA_SIZE];
    for (int i = 0; i < DATA_SIZE; i++) {
        data[i] = uart_read(UART1);
    }
    // calculate this after reading from UART, to reduce number of branches to explore
    struct entry *entry = &entries[slot];
    // we don't do this check at the start of this function to simplify the
    // driver: this way, the amount that's read from UART is the same
    // regardless of the HSM's state; plus, if we did this check earlier, we'd
    // need to signal to the driver whether the entry was valid, so it could
    // decide whether to continue or not
    if (entry->valid) {
        uart_write(UART1, 0x00);
        return;
    }
    vmemcpy((volatile uint8_t *) &entry->data, data, DATA_SIZE);
    vmemcpy((volatile uint8_t *) &entry->pin, pin, PIN_LENGTH);
    entry->bad_guesses = 0;
    entry->valid = 1;
    uart_write(UART1, 0x01);
}

uint8_t pin_equal(const uint8_t *a, const uint8_t *b) {
    uint8_t eq = 1;
    for (int i = 0; i < PIN_LENGTH; i++) {
        eq = eq & (a[i] == b[i]);
    }
    return eq;
}

void do_retrieve(uint8_t slot) {
    uint8_t pin[PIN_LENGTH];
    for (int i = 0; i < PIN_LENGTH; i++) {
        pin[i] = uart_read(UART1);
    }
    // calculate after reading from UART
    struct entry *entry = &entries[slot];
    // similar to store, we do this check after reading the pin to simplify the
    // driver
    if (!entry->valid || entry->bad_guesses >= GUESS_LIMIT) {
        uart_write(UART1, 0x01);
        return;
    }
    uint8_t guess_correct = pin_equal((const uint8_t *) &entry->pin, pin);
    entry->bad_guesses = (guess_correct - 1) & (entry->bad_guesses + 1); // commit point
    if (!guess_correct) {
        uart_write(UART1, 0x02);
    } else {
        uart_write(UART1, 0x03);
        for (int i = 0; i < DATA_SIZE; i++) {
            uart_write(UART1, entry->data[i]);
        }
    }
}
