/*
 * $HEADER$
 */

/** @file dataype copy function */

#include <stdlib.h>

#include "datatype.h"

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
        } else if (LAM_DATATYPE_STATE_CONTIGUOUS & d->d_flags) {
            (*memcpy_fn)(dst, src, count * d->d_extent, csum);
        } else {
            lam_datavec_t *dv = d->d_datavec;
            unsigned char *p = ((unsigned char *) dst);
            unsigned char *q = ((unsigned char *) src);
            size_t i, j;

            while (count--) {
                for (i = 0; i < d->d_datavec_size; i++) {
                    for (j = 0; j < dv->dv_nrepeat; i++) {
                        (*memcpy_fn)(p + dv->dv_element[i].dve_offset,
                                     q + dv->dv_element[i].dve_offset,
                                     dv->dv_element[i].dve_size,
                                     csum);
                    }
                    p += dv->dv_repeat_offset;
                    q += dv->dv_repeat_offset;
                }
                p += d->d_extent;
                q += d->d_extent;
            }
        }
    }

    return status;
}
