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
                      lam_memcpy_state_t *csum)
{
    int status;

    status = LAM_SUCCESS;

    if (NULL == src || NULL == dst) {
        status = LAM_ERROR;
    }

    if (LAM_SUCCESS == status) {
        if (NULL == d) {
            (*memcpy_fn)(dst, src, count, csum);
        } else if (LAM_DATATYPE_STATE_CONTIGUOUS & d->flags) {
            (*memcpy_fn)(dst, src, count * d->extent, csum);
        } else {
            lam_datavec_t *dv = d->datavec;
            unsigned char *p = (unsigned char *) dst;
            unsigned char *q = (unsigned char *) src;
            size_t i, j;

            while (count--) {
                for (j = 0; j < dv->nrepeat; j++) {
                    for (i = 0; i < d->datavec_size; i++) {
                        (*memcpy_fn)(p + dv->element[i].offset,
                                     q + dv->element[i].offset,
                                     dv->element[i].size,
                                     csum);
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
