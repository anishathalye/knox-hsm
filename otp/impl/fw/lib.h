#ifndef LIB_H
#define LIB_H

#include <stdint.h>

#define SECRET_SIZE 20 // 32 characters base32-encoded, 32*5/8 = 20
#define CTR_SIZE 8 // 8-byte counter

// note: specialized to key length of SECRET_SIZE, message length of CTR_SIZE
// return buffer is re-used
void hmac_sha1(const uint8_t *key, const uint8_t *msg, uint8_t *buf);

// note: specialized to returning result of 6 digits; c is big-endian
uint32_t hotp(const uint8_t *key, const uint8_t *c, uint8_t *buf);

#endif // LIB_H
