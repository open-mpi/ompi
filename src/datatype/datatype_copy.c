/*
 * $HEADER$
 */

/* lam_dataype_t copy function */

#include <stdlib.h>

#include "datatype.h"

/*
 * Copy (the contents of) an array of data types
 */
int lam_datatype_copy(void *dst,
                      const void *src,
                      size_t count,
                      lam_datatype_t *d,
                      lam_memcpy_fn_t *memcpy_fn,
                      lam_memcpy_state_t *memcpy_state)
{
    int status;

    status = LAM_SUCCESS;

    if (NULL == src || NULL == dst) {
        status = LAM_ERROR;
    }

    if (LAM_SUCCESS == status) {
        if (NULL == d) {
            (*memcpy_fn)(dst, src, count, memcpy_state);
        } else if (LAM_DATATYPE_STATE_CONTIGUOUS & d->flags) {
            (*memcpy_fn)(dst, src, count * d->extent, memcpy_state);
        } else {
            lam_datavec_t *dv = d->datavec;
            unsigned char *p = (unsigned char *) dst;
            unsigned char *q = (unsigned char *) src;
            size_t i, j;

            while (count--) {
                for (i = 0; i < d->datavec_size; i++) {
                    for (j = 0; j < dv->nrepeat; j++) {
                        (*memcpy_fn)(p + dv->element[i].offset,
                                     q + dv->element[i].offset,
                                     dv->element[i].size,
                                     memcpy_state);
                    }
                    p += dv->repeat_offset;
                    q += dv->repeat_offset;
                }
                p += d->extent;
                q += d->extent;
            }
        }
    }

    return status;
}
