#include "drivers.h"
#include "lib.h"

#define BAUD_RATE 6

struct state {
    volatile uint32_t secret[SECRET_SIZE/4];
    uint8_t _pad[4];
    volatile uint64_t max_ctr;
};

struct pmem {
    volatile uint32_t active;
    uint8_t _pad[4];
    struct state state0;
    struct state state1;
} *pmem = (struct pmem *) FRAM_BASE;;

typedef char assert_pmem_packed[(sizeof(struct pmem)==72)*2-1];

#define CMD_SET_SECRET (0x01)
#define CMD_GET_OTP (0x02)
#define CMD_AUDIT (0x03)

void do_set_secret();
void do_get_otp();
void do_audit();

void main() {
    uart_init(UART1, BAUD_RATE);
    uint8_t cmd = uart_read(UART1);
    switch (cmd) {
        case CMD_SET_SECRET:
            do_set_secret();
            break;
        case CMD_GET_OTP:
            do_get_otp();
            break;
        case CMD_AUDIT:
            do_audit();
            break;
        default:
            break;
    }
    poweroff(); // done, don't interact with world any more until next reset
}

void memcpy(volatile uint32_t *dest, volatile uint32_t *src, size_t count) {
    for (int i = 0; i < count; i++) {
        dest[i] = src[i];
    }
}

void do_set_secret() {
    uint32_t wbuf[SECRET_SIZE/4];
    uint8_t *buf = (uint8_t *) wbuf;
    for (int i = 0; i < SECRET_SIZE; i++) {
        buf[i] = uart_read(UART1);
    }
    // get pointers to active and inactive regions
    struct state *base = &pmem->state0;
    uint32_t zlt = 0 < pmem->active;
    struct state *active = base + zlt;
    struct state *inactive = base + (1 - zlt);
    // copy counter
    inactive->max_ctr = active->max_ctr;
    // copy secret
    memcpy(inactive->secret, wbuf, SECRET_SIZE/4);
    // commit
    pmem->active = !pmem->active;
    uart_write(UART1, 0x01);
}

// returns value in big-endian order
void do_audit() {
    struct state *base = &pmem->state0;
    uint32_t zlt = 0 < pmem->active;
    struct state *active = base + zlt;
    for (int shift = 56; shift >= 0; shift -= 8) {
        uart_write(UART1, active->max_ctr >> shift);
    }
}

uint8_t scratch[SHA1_DIGEST_SIZE];

#define SWAP32(x) \
    ((x >> 24) & 0xff) | \
    ((x << 8) & 0xff0000) | \
    ((x >> 8) & 0xff00) | \
    ((x << 24) & 0xff000000)

void do_get_otp() {
    uint8_t c[CTR_SIZE];
    for (int i = 0; i < CTR_SIZE; i++) {
        c[i] = uart_read(UART1);
    }
    // get pointers to active and inactive regions
    struct state *base = &pmem->state0;
    uint32_t zlt = 0 < pmem->active;
    struct state *active = base + zlt;
    struct state *inactive = base + (1 - zlt);
    // c is unsigned big-endian; bail out if it's less than max_ctr
    uint64_t cint = SWAP32(*(((uint32_t *) c) + 1));
    cint |= ((uint64_t) SWAP32(*((uint32_t *) c))) << 32;
    if (cint < active->max_ctr) {
        uart_write(UART1, 0x01);
        return;
    }
    // update state
    inactive->max_ctr = cint;
    memcpy(inactive->secret, active->secret, SECRET_SIZE/4);
    // commit
    pmem->active = !pmem->active;
    active->max_ctr = cint; // line up active=0 and active=1 branches persistent memory for easier merge
    // compute hash and return
    uint32_t secret[SECRET_SIZE/4];
    memcpy(secret, active->secret, SECRET_SIZE/4);
    // we do the memcpy above so that after the memcpy is done, we can explore
    // just one branch (for the HOTP computation)
    uint32_t result = hotp((const uint8_t *) secret, c, scratch);
    uart_write(UART1, 0x02);
    // note: 0 <= result < 1000000, so it fits in 24 bits
    // writing out result in big-endian order
    uart_write(UART1, (result >> 24) & 0xff);
    uart_write(UART1, (result >> 16) & 0xff);
    uart_write(UART1, (result >> 8) & 0xff);
    uart_write(UART1, result & 0xff);
}
