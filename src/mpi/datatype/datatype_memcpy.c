/*
 * $HEADER$
 */

/* alternative memcpy function */

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "lam_config.h"
#include "datatype.h"

/*
 * Alternative memcpy function: On some systems, this performs better
 * than the system memcpy.
 */
void *lam_memcpy_alt(void *dst, const void *src, size_t size,
                     lam_memcpy_state_t *dummy)
{
    assert(dst);
    assert(src);

    if (lam_aligned32((void *) src) && lam_aligned32(dst)) {
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
