/*
 * $HEADER$
 */

/** @file 32-bit cyclic redundancy check support */

#include <stdlib.h>

#include "lam_config.h"
#include "lam/stdint.h"
#include "datatype.h"

#define CRC_POLYNOMIAL       ((uint32_t) 0x04c11db7)
#define CRC_INITIAL_REGISTER ((uint32_t) 0xffffffff)
#define IS_32BIT_ALIGNED(X) \
    ((uint32_t)(X) & (uint32_t) 3 == (uint32_t) 0 ? 1 : 0)

/*
 * Look-up table for CRC32 generation
 */
static bool crc_table_initialized = false;
static uint32_t crc_table[256];

/**
 * CRC32 table generation
 *
 * One time initializtion of CRC32 look-up table.  Thanks to Charles
 * Michael Heard for his optimized CRC32 code.
 */
static void initialize_crc_table(void)
{
    register int i, j;
    register uint32_t crc_accum;

    for (i = 0; i < 256; i++) {
        crc_accum = (i << 24);
        for (j = 0; j < 8; j++) {
            if (crc_accum & 0x80000000) {
                crc_accum = (crc_accum << 1) ^ CRC_POLYNOMIAL;
            } else {
                crc_accum = (crc_accum << 1);
            }
        }
        crc_table[i] = crc_accum;
    }

    crc_table_initialized = 1;
}


/**
 * Generate a 32-bit CRC for a buffer
 *
 * @param buffer      Data buffer
 * @param size        Size of buffer
 * @param initial_crc Initial value of the CRC register
 * @return            The CRC
 *
 * Generate a 32-bit for a data buffer starting from a given CRC
 * value.
 */
uint32_t lam_crc32(const void *restrict buffer, size_t size, uint32_t initial_crc)
{
    register int i, j;
    register unsigned char *t;
    uint32_t tmp;
    uint32_t crc = initial_crc;

    if (!crc_table_initialized) {
        initialize_crc_table();
    }

    if (IS_32BIT_ALIGNED(buffer)) {
        register uint32_t *restrict src = (uint32_t *) buffer;
        while (size >= sizeof(uint32_t)) {
            tmp = *src++;
            t = (unsigned char *) &tmp;
            for (j = 0; j < (int) sizeof(uint32_t); j++) {
                i = ((crc >> 24) ^ *t++) & 0xff;
                crc = (crc << 8) ^ crc_table[i];
            }
            size -= sizeof(uint32_t);
        }
        t = (unsigned char *) src;
        while (size--) {
            i = ((crc >> 24) ^ *t++) & 0xff;
            crc = (crc << 8) ^ crc_table[i];
        }
    } else {
        register unsigned char *restrict src = (unsigned char *) buffer;
        while (size--) {
            i = ((crc >> 24) ^ *src++) & 0xff;
            crc = (crc << 8) ^ crc_table[i];
        }
    }

    return crc;
}


/**
 * Copy data from one buffer to another and calculate a 32-bit CRC
 *
 * @param dst      pointer to the destination buffer
 * @param src      pointer to the source buffer
 * @param size     size of the buffer
 * @param state    pointer to a memcpy with checksum/CRC state structure
 * @return         the original value of dst
 *
 * This handles cumulative CRCs for for arbitrary lengths and address
 * alignments as best as it can. The initial contents of state->sum is
 * used as the starting value of the CRC.  The final CRC is placed
 * back in state->sum.
 */
void *lam_memcpy_crc32(void *restrict dst,
                       const void *restrict src,
                       size_t size,
                       lam_memcpy_state_t *state)
{
    size_t crclenresidue = (state->size > size) ? (state->size - size) : 0;
    register int i, j;
    uint32_t tmp;
    register unsigned char t;
    uint32_t crc = state->sum;

    if (!crc_table_initialized) {
        initialize_crc_table();
    }

    if (state->first_call) {
        state->first_call = false;
        state->sum = CRC_INITIAL_REGISTER;
    }

    if (IS_32BIT_ALIGNED(src) && IS_32BIT_ALIGNED(dst)) {
        register uint32_t *restrict p = (uint32_t *) dst;
        register uint32_t *restrict q = (uint32_t *) src;
        register unsigned char *ts, *td;
        /* copy whole integers */
        while (size >= sizeof(uint32_t)) {
            tmp = *q++;
            *p++ = tmp;
            ts = (unsigned char *) &tmp;
            for (j = 0; j < (int) sizeof(uint32_t); j++) {
                i = ((crc >> 24) ^ *ts++) & 0xff;
                crc = (crc << 8) ^ crc_table[i];
            }
            size -= sizeof(uint32_t);
        }
        ts = (unsigned char *) q;
        td = (unsigned char *) p;
        /* copy partial integer */
        while (size--) {
            t = *ts++;
            *td++ = t;
            i = ((crc >> 24) ^ t) & 0xff;
            crc = (crc << 8) ^ crc_table[i];
        }
        /* calculate CRC over remaining bytes... */
        while (crclenresidue--) {
            i = ((crc >> 24) ^ *ts++) & 0xff;
            crc = (crc << 8) ^ crc_table[i];
        }
    } else {
        register unsigned char *restrict q = (unsigned char *) src;
        register unsigned char *restrict p = (unsigned char *) dst;
        while (size--) {
            t = *q++;
            *p++ = t;
            i = ((crc >> 24) ^ t) & 0xff;
            crc = (crc << 8) ^ crc_table[i];
        }
        while (crclenresidue--) {
            i = ((crc >> 24) ^ *q++) & 0xff;
            crc = (crc << 8) ^ crc_table[i];
        }
    }

    state->sum = crc;

    return dst;
}
