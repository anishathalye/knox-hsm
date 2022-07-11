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

#define swap64(val) \
    ( (((val) >> 56) & 0x00000000000000FF) | (((val) >> 40) & 0x000000000000FF00) | \
      (((val) >> 24) & 0x0000000000FF0000) | (((val) >>  8) & 0x00000000FF000000) | \
      (((val) <<  8) & 0x000000FF00000000) | (((val) << 24) & 0x0000FF0000000000) | \
      (((val) << 40) & 0x00FF000000000000) | (((val) << 56) & 0xFF00000000000000) )

#define swap32(val) \
    ( (((val) >> 24) & 0x000000FF) | (((val) >>  8) & 0x0000FF00) | \
      (((val) <<  8) & 0x00FF0000) | (((val) << 24) & 0xFF000000) )

void sha1_digest(struct sha1 *sha1, const uint8_t *buf, size_t n, uint8_t *digest) {
    // pad the message as (m || 1 || 0...0 | size)
    // where size is encoded as 64 bits, and the number of 0s added is the
    // minimum possible to make the padded message be a multiple of 512 bits

    uint64_t bits64 = n << 3;
    bits64 = swap64(bits64);

    uint32_t ctrl_go = SHA1_CTRL_INIT;

    int i;

    // feed full blocks first, directly
    while (n >= SHA1_BLOCK_SIZE) {
        // wait until ready
        while (!(sha1->status & SHA1_STATUS_READY)) {
            // wait
        }
        // fill data
        for (i = 0; i < SHA1_BLOCK_SIZE/4; i++) {
            sha1->blocks[i] = swap32(*((const uint32_t *) buf));
            buf += 4;
        }
        // issue write
        sha1->ctrl = ctrl_go;
        ctrl_go = SHA1_CTRL_NEXT;
        n -= SHA1_BLOCK_SIZE;
    }

    // wait until ready
    while (!(sha1->status & SHA1_STATUS_READY)) {
        // wait
    }
    // write partial block
    i = 0;
    while ((n >> 2) > 0) {
        sha1->blocks[i++] = swap32(*((const uint32_t *) buf));
        buf += 4;
        n -= 4;
    }
    // write partial word; there must be space for this because we wrote all
    // full blocks earlier
    uint32_t w;
    switch (n) {
        case 3:
            w = (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | 0x80;
            break;
        case 2:
            w = (buf[0] << 24) | (buf[1] << 16) | (  0x80 << 8) | 0x00;
            break;
        case 1:
            w = (buf[0] << 24) | (  0x80 << 16) | (  0x00 << 8) | 0x00;
            break;
        case 0:
            w = (  0x80 << 24) | (  0x00 << 16) | (  0x00 << 8) | 0x00;
    }
    sha1->blocks[i++] = w;

    // now, we have either 1 or 2 more blocks left, depending on whether adding
    // the 8-byte encoding of the size overflows the block boundary
    if (i + 2 > 16) {
        // need another block

        // fill this block with 0s
        while (i < 16) {
            sha1->blocks[i++] = 0x00000000;
        }
        // issue write
        sha1->ctrl = ctrl_go;
        ctrl_go = SHA1_CTRL_NEXT;
        i = 0;
        // wait until ready
        while (!(sha1->status & SHA1_STATUS_READY)) {
            // wait
        }
    }

    // fill with 0s
    while (i + 2 < 16) {
        sha1->blocks[i++] = 0x00000000;
    }

    // write size
    sha1->blocks[i++] = swap32(*((uint32_t *) &bits64));
    sha1->blocks[i] = swap32(*(((uint32_t *) &bits64) + 1));

    // wait until ready
    while (!(sha1->status & SHA1_STATUS_READY)) {
        // wait
    }
    // issue write
    sha1->ctrl = ctrl_go;

    // wait until digest is ready
    while (!(sha1->status & SHA1_STATUS_READY)) {
        // wait
    }
    // read digest
    uint32_t *digest_w = (uint32_t *) digest;
    for (i = 0; i < SHA1_DIGEST_SIZE/4; i++) {
        digest_w[i] = swap32(sha1->digest[i]);
    }
}
