/*
 * $HEADER$
 */

/** @file alternative memcpy function */

#include <stdlib.h>
#include <string.h>

#include "lam_config.h"
#include "lam/stdint.h"


#define ALIGNED32(X) (((uint32_t)(X) & (uint32_t) 3) == (uint32_t) 0 ? 1 : 0)

/**
 * Alternative memcpy function
 *
 * @param dst   destination buffer
 * @param src   source buffer
 * @param size  size of buffer
 * @param dummy unused variable
 * @return      the original value of dst
 *
 * On some systems, this performs better than the system memcpy.
 */
void *lam_memcpy_alt(void *dst, const void *src, size_t size, void *dummy)
{
    if (ALIGNED32(src) && ALIGNED32(dst)) {
        uint32_t *restrict p = (uint32_t *) dst;
        uint32_t *restrict q = (uint32_t *) src;
        uint32_t i;
        uint32_t n = size >> 2;
        for (i = 0; i < n; i++) {
            *p++ = *q++;
        }
        size -= n * sizeof(size_t);
        if (size != 0) {
            while (size--) {
                *p++ = *q++;
            }
        }
    } else {
        memcpy(dst, src, size);
    }

    return dst;
}
