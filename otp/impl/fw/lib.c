#include "drivers.h"
#include "lib.h"
#include "mod.h"

#define MAX(a, b) (a > b ? a : b)
#define IPAD 0x36363636
#define OPAD 0x5c5c5c5c

void hmac_sha1(const uint8_t *key, const uint8_t *msg, uint8_t *buf) {
    uint8_t ibuf[SHA1_BLOCK_SIZE + MAX(CTR_SIZE, SHA1_DIGEST_SIZE)];
    // so we can do word-level copies
    uint32_t *wibuf = (uint32_t *) ibuf;
    const uint32_t *wkey = (const uint32_t *) key;
    const uint32_t *wmsg = (const uint32_t *) msg;
    uint32_t *wbuf = (uint32_t *) buf;
    // first hash: SHA1(ipad xor key || m)
    for (int i = 0; i < SECRET_SIZE/4; i++) {
        wibuf[i] = wkey[i] ^ IPAD;
    }
    // key is zeroes from here to end of block size
    for (int i = SECRET_SIZE/4; i < SHA1_BLOCK_SIZE/4; i++) {
        wibuf[i] = IPAD;
    }
    for (int i = 0; i < CTR_SIZE/4; i++) {
        wibuf[SHA1_BLOCK_SIZE/4+i] = wmsg[i];
    }
    sha1_digest(SHA1, ibuf, SHA1_BLOCK_SIZE+CTR_SIZE, buf);
    // second hash: SHA1(opad xor key || previous hash)
    for (int i = 0; i < SECRET_SIZE/4; i++) {
        wibuf[i] = wkey[i] ^ OPAD;
    }
    for (int i = SECRET_SIZE/4; i < SHA1_BLOCK_SIZE/4; i++) {
        wibuf[i] = OPAD;
    }
    for (int i = 0; i < SHA1_DIGEST_SIZE/4; i++) {
        wibuf[SHA1_BLOCK_SIZE/4+i] = wbuf[i];
    }
    sha1_digest(SHA1, ibuf, SHA1_BLOCK_SIZE+SHA1_DIGEST_SIZE, buf);
}

uint32_t hotp(const uint8_t *key, const uint8_t *c, uint8_t *buf) {
    hmac_sha1(key, c, buf);
    int offset = buf[SHA1_DIGEST_SIZE-1] & 0xf;
    // whole-circuit symbolic execution doesn't handle this symbolic memory address gracefully...
    /*
    uint32_t s = (buf[offset] & 0x7f) << 24
        | (buf[offset+1] & 0xff) << 16
        | (buf[offset+2] & 0xff) << 8
        | (buf[offset+3] & 0xff);
    */
    uint32_t s = 0;
    uint32_t matched_mask;
    for (int i = 0; i < 0x10; i++) {
        matched_mask = ((i != offset) - 1);
        s += ((buf[i] & 0x7f) & matched_mask) << 24;
        s += ((buf[i+1] & 0xff) & matched_mask) << 16;
        s += ((buf[i+2] & 0xff) & matched_mask) << 8;
        s += ((buf[i+3] & 0xff) & matched_mask);
    }
    // note: just a x % 1000000 would not be constant-time
    //
    // given our compiler/hardware, even this is not constant-time, because it
    // branches:
    //     x -= (x >= 4096000000) ? 4096000000 : 0;
    return ct_mod1000000(s);
}
