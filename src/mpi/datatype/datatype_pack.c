/*
 * $HEADER$
 */

/* lam_dataype_t pack function(s) */

#include <stdlib.h>

#include "datatype.h"

/*
 * Incrementally pack or unpack an array of datatypes to/from a buffer
 */
int lam_datatype_packer(lam_pack_state_t *state,
                        void *buf,
                        size_t bufsize,
                        void *typebuf,
                        size_t ntype,
                        lam_datatype_t *d,
                        lam_memcpy_fn_t *memcpy_fn,
                        lam_memcpy_state_t *check,
                        int pack_direction)
{
    if (LAM_DATATYPE_STATE_CONTIGUOUS & d->flags) {

        unsigned char *b;
        unsigned char *t;
        size_t copied_so_far;
        size_t left_to_copy;
        size_t size;

        b = (unsigned char *) buf + state->packed_offset;
        t = (unsigned char *) typebuf
            + state->type_index * d->extent
            + state->repeat_index * d->datavec->repeat_offset
            + state->datavec_offset;
        bufsize -= state->packed_offset;

        copied_so_far =
            state->type_index * d->extent + state->datavec_offset;
        size = bufsize;
        left_to_copy = ntype * d->extent - copied_so_far;
        if (size > left_to_copy) {
            size = left_to_copy;
        }
        if (LAM_DATATYPE_PACK == pack_direction) {
            memcpy(b, t, size);
        } else {
            memcpy(t, b, size);
        }
        copied_so_far += size;
        state->packed_offset += size;
        state->type_index = copied_so_far / d->extent;
        state->datavec_offset =
            copied_so_far - state->type_index * d->extent;
        if (copied_so_far != (ntype * d->extent)) {
            return LAM_DATATYPE_PACK_INCOMPLETE;
        }

    } else {

        unsigned char *ptr;
        unsigned char *b;
        unsigned char *t;
        size_t size;
        lam_datavec_t *dv;

        ptr = (unsigned char *) typebuf
            + state->type_index * d->extent,
            + state->repeat_index * dv->repeat_offset;
        b = (unsigned char *) buf + state->packed_offset;
        bufsize -= state->packed_offset;
        dv = d->datavec;

        while (state->type_index < ntype) {
            while (state->repeat_index < dv->nrepeat) {
                while (state->element_index < dv->nelement) {
                    t = ptr + dv->element[state->element_index].offset;
                    size = dv->element[state->element_index].size;
                    if (state->datavec_offset > 0) {
                        t += state->datavec_offset;
                        size -= state->datavec_offset;
                        if (size <= bufsize) {
                            state->datavec_offset = 0;
                        }
                    }
                    if (size > bufsize) {
                        size = bufsize;
                        state->datavec_offset += size;
                    }
                    if (LAM_DATATYPE_PACK == pack_direction) {
                        memcpy(b, t, size);
                    } else {
                        memcpy(t, b, size);
                    }
                    state->packed_offset += size;
                    if (state->datavec_offset > 0) {
                        return LAM_DATATYPE_PACK_INCOMPLETE;
                    }
                    bufsize -= size;
                    b += size;
                    state->element_index += 1;
                    if (bufsize == 0
                        && state->element_index < (size_t) dv->nelement) {
                        return LAM_DATATYPE_PACK_INCOMPLETE;
                    }
                }
                ptr += dv->repeat_offset;
                state->repeat_index += 1;
            }
            ptr += d->extent;
            state->type_index += 1;
            state->element_index = 0;
            if (bufsize == 0 && state->type_index < ntype) {
                return LAM_DATATYPE_PACK_INCOMPLETE;
            }
        }
    }

    return LAM_DATATYPE_PACK_COMPLETE;
}
