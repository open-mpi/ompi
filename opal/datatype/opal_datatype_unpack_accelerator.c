/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2019 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <assert.h>
#include <stddef.h>

#include "opal/datatype/opal_convertor_internal.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/datatype/opal_datatype_prototypes.h"
#include "opal/datatype/opal_datatype_accelerator_copy.h"

/**
 * Unpack only the remainder of a partially-unpacked block, so that the rest of the
 * unpacking process operates on whole blocks (plus a possible trailing partial block).
 * A basic type is never split. Returns 1 once we are aligned on a block boundary, 0 otherwise.
 */
static inline int opal_unpack_accelerator_partial_blocklen(opal_convertor_t *pConvertor,
                                                           const dt_elem_desc_t *pElem,
                                                           size_t *count,
                                                           unsigned char **packed,
                                                           unsigned char **memory,
                                                           size_t *space)
{
    const ddt_elem_desc_t *elem = &pElem->elem;
    const size_t elem_size = opal_datatype_basicDatatypes[elem->common.type]->size;
    unsigned char *from = *packed;
    unsigned char *to = (*memory) + elem->disp;
    size_t do_now, left_in_block, do_now_bytes;

    assert(0 != elem_size);
    assert(0 != elem->blocklen);
    assert(*count <= ((size_t) elem->count * elem->blocklen));

    /* Nothing to do if we are already aligned on a block boundary. */
    if (0 == (do_now = (*count) % elem->blocklen)) {
        return 1;
    }
    left_in_block = do_now;

    /* Never split a basic type: clamp to an integral number of elements. */
    if ((do_now * elem_size) > *space) {
        do_now = *space / elem_size;
    }
    if (0 == do_now) {
        return 0;
    }
    do_now_bytes = do_now * elem_size;

    pConvertor->cbmemcpy(to, from, do_now_bytes, pConvertor);
    *memory += (ptrdiff_t) do_now_bytes;
    if (do_now == left_in_block) {
        /* Completed the block; skip the gap to the start of the next one. */
        *memory += elem->extent - (ptrdiff_t) (elem->blocklen * elem_size);
    }
    *count -= do_now;
    *space -= do_now_bytes;
    *packed += do_now_bytes;
    return (do_now == left_in_block);
}

/**
 * Finish a predefined element that a fragment boundary split. The accelerator unpacker is only
 * selected for homogeneous convertors, so a partial element is a plain byte copy: place the
 * available sub-element bytes at their offset inside the destination element (through cbmemcpy so
 * it stays device-safe) and record progress in pConvertor->partial_length.
 *
 * partial_length is the datatype engine's shared mechanism for a mid-basic-element position:
 * opal_convertor_set_position() (via opal_convertor_generic_simple_position()) stores the
 * sub-element offset there for a repositioned receive (e.g. the per-fragment OB1 copy-in/out path),
 * and this mover stores it there itself for an in-order streaming receive, so both resume on the
 * next call. The stack (PUSH_STACK, below) only carries the whole-element resume state; the
 * intra-element offset is not derivable from it.
 *
 * When the element completes, consume it from the count and step past the block's trailing gap,
 * mirroring opal_unpack_partial_predefined() in opal_datatype_unpack.h.
 */
static inline void opal_unpack_accelerator_partial_predefined(opal_convertor_t *pConvertor,
                                                              const dt_elem_desc_t *pElem,
                                                              size_t *count,
                                                              unsigned char **packed,
                                                              unsigned char **memory,
                                                              size_t *space)
{
    const ddt_elem_desc_t *elem = &pElem->elem;
    const size_t elem_size = opal_datatype_basicDatatypes[elem->common.type]->size;
    const size_t start = pConvertor->partial_length;
    size_t length = elem_size - start;
    unsigned char *user_data = *memory + elem->disp;

    assert(0 != elem_size);
    assert(start < elem_size);
    if (*space < length) {
        length = *space;
    }

    pConvertor->cbmemcpy(user_data + start, *packed, length, pConvertor);

    pConvertor->partial_length = (start + length) % elem_size;
    *space -= length;
    *packed += length;
    if (0 == pConvertor->partial_length) {
        (*count)--; /* one full predefined element completed */
        *memory += elem_size;
        if (0 == (*count % elem->blocklen)) {
            *memory += elem->extent - (ptrdiff_t) (elem->blocklen * elem_size);
        }
    }
}

static inline size_t opal_unpack_accelerator_predefined_data(opal_convertor_t *pConvertor,
                                                             const dt_elem_desc_t *pElem,
                                                             size_t *count,
                                                             unsigned char **packed,
                                                             unsigned char **memory,
                                                             size_t *space)
{
    const ddt_elem_desc_t *elem = &pElem->elem;
    const size_t elem_size = opal_datatype_basicDatatypes[elem->common.type]->size;
    const size_t block_bytes = elem_size * elem->blocklen;
    size_t copy_count = *count, copied;
    char *from, *to;

    assert(0 != elem_size);
    assert(0 != elem->blocklen);
    /* Entry is always block-aligned; a mid-block resume is handled beforehand by
     * opal_unpack_accelerator_partial_blocklen(). */
    assert(0 == (copy_count % elem->blocklen));

    /* Never split a basic type, but a trailing partial block is allowed so that we always
     * make progress as long as at least one element fits in the unpack buffer. */
    if (copy_count > (*space / elem_size)) {
        copy_count = *space / elem_size;
    }
    if (0 == copy_count) {
        return 0;
    }

    from = (char *) *packed;
    to = (char *) (*memory + elem->disp);
    copied = opal_datatype_accelerator_copy_by_size(pConvertor, elem_size, copy_count,
                                                    elem->blocklen, elem->count, &from, *space,
                                                    block_bytes, &to, *space, elem->extent);

    assert(copied == copy_count);
    *count -= copied;
    *packed = (unsigned char *) from;
    *space -= copied * elem_size;
    *memory = (unsigned char *) to - elem->disp;
    return copied * elem_size;
}

/**
 * Unpack a contiguous loop: its body is a single contiguous run of end_loop->size bytes, repeated
 * loop->loops times with stride loop->extent. Only whole iterations are copied here; a body that
 * does not fit the remaining packed input is left untouched so the caller descends into the
 * element path, which still makes element-granular progress. When the iterations are back-to-back
 * (extent == size) the whole run collapses into a single device copy, which is the point of this
 * fast path: on accelerator memory every cbmemcpy is a device transfer with high fixed latency.
 */
static inline void opal_unpack_accelerator_contiguous_loop(opal_convertor_t *pConvertor,
                                                           const dt_elem_desc_t *pElem,
                                                           size_t *count,
                                                           unsigned char **packed,
                                                           unsigned char **memory,
                                                           size_t *space)
{
    const ddt_loop_desc_t *loop = &pElem->loop;
    const ddt_endloop_desc_t *end_loop = (const ddt_endloop_desc_t *) (pElem + loop->items);
    unsigned char *to = *memory + end_loop->first_elem_disp;
    unsigned char *from = *packed;
    size_t copy_loops = *count;

    /* Whole iterations only: never split the contiguous body across a fragment boundary here. */
    if ((copy_loops * end_loop->size) > *space) {
        copy_loops = *space / end_loop->size;
    }
    if (0 == copy_loops) {
        return;
    }

    if (loop->extent == (ptrdiff_t) end_loop->size) {
        /* Iterations are contiguous in device memory: one transfer for the whole run. */
        pConvertor->cbmemcpy(to, from, copy_loops * end_loop->size, pConvertor);
    } else {
        for (size_t i = 0; i < copy_loops; i++) {
            pConvertor->cbmemcpy(to, from, end_loop->size, pConvertor);
            to += loop->extent;
            from += end_loop->size;
        }
    }

    *packed += copy_loops * end_loop->size;
    *memory += (ptrdiff_t) copy_loops * loop->extent; /* base advances by the whole iterations copied */
    *space -= copy_loops * end_loop->size;
    *count -= copy_loops;
}

int32_t opal_unpack_accelerator_simple(opal_convertor_t *pConvertor, struct iovec *iov,
                                       uint32_t *out_size, size_t *max_data)
{
    dt_stack_t *pStack;
    uint32_t pos_desc;
    size_t count_desc;
    size_t total_unpacked = 0;
    dt_elem_desc_t *description;
    dt_elem_desc_t *pElem;
    const opal_datatype_t *pData = pConvertor->pDesc;
    unsigned char *conv_ptr, *iov_ptr;
    size_t iov_len_local;
    ptrdiff_t local_disp;
    uint32_t iov_count;

    /* Accelerator movers only ever run on homogeneous convertors: a device copy moves raw
     * bytes and can neither byte-swap nor resize a predefined type, so the completion test
     * below (bConverted == remote_size) is only meaningful when the local and remote sizes
     * agree. A heterogeneous convertor must take the generic host path. */
    assert(pConvertor->flags & CONVERTOR_HOMOGENEOUS);

    description = pConvertor->use_desc->desc;
    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc = pStack->index;
    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
    count_desc = pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &(description[pos_desc]);

    for (iov_count = 0; iov_count < (*out_size); iov_count++) {
        iov_ptr = (unsigned char *) iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;

        if (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            if (0 != pConvertor->partial_length) {
                /* A previous call (or a set_position landing mid-element) left us inside a
                 * predefined element; finish it before resuming the block. */
                opal_unpack_accelerator_partial_predefined(pConvertor, pElem, &count_desc,
                                                           &iov_ptr, &conv_ptr, &iov_len_local);
                if (0 == count_desc) {
                    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                    pos_desc++;
                    UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                             process_loop, process_end_loop);
                    goto process_data;
                }
                if (0 == iov_len_local) {
                    goto complete_loop;
                }
            }
            if (((size_t) pElem->elem.count * pElem->elem.blocklen) != count_desc) {
                /* Resume a datatype block that a previous call left partially unpacked. */
                if (0 == opal_unpack_accelerator_partial_blocklen(pConvertor, pElem, &count_desc,
                                                                  &iov_ptr, &conv_ptr,
                                                                  &iov_len_local)) {
                    goto complete_loop;
                }
                if (0 == count_desc) {
                    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                    pos_desc++;
                    UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                             process_loop, process_end_loop);
                    goto process_data;
                }
            }
        }

        while (1) {
            while (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            process_data:
                if (0 == opal_unpack_accelerator_predefined_data(pConvertor, pElem, &count_desc,
                                                                 &iov_ptr, &conv_ptr,
                                                                 &iov_len_local)) {
                    goto complete_loop;
                }
                if (0 != count_desc) {
                    goto complete_loop;
                }
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                pos_desc++;
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                         process_loop, process_end_loop);
                goto process_data;
            }
            if (OPAL_DATATYPE_END_LOOP == pElem->elem.common.type) {
            process_end_loop:
                if (0 == --(pStack->count)) {
                    if (0 == pConvertor->stack_pos) {
                        *out_size = iov_count;
                        goto complete_loop;
                    }
                    pConvertor->stack_pos--;
                    pStack--;
                    pos_desc++;
                } else {
                    pos_desc = pStack->index + 1;
                    if (-1 == pStack->index) {
                        pStack->disp += (pData->ub - pData->lb);
                    } else {
                        assert(OPAL_DATATYPE_LOOP == description[pStack->index].loop.common.type);
                        pStack->disp += description[pStack->index].loop.extent;
                    }
                }
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                         process_loop, process_end_loop);
                goto process_data;
            }
            if (OPAL_DATATYPE_LOOP == pElem->elem.common.type) {
            process_loop:
                local_disp = (ptrdiff_t) conv_ptr;
                if (pElem->loop.common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS) {
                    opal_unpack_accelerator_contiguous_loop(pConvertor, pElem, &count_desc,
                                                            &iov_ptr, &conv_ptr, &iov_len_local);
                    if (0 == count_desc) { /* whole loop unpacked: skip past its descriptor entries */
                        pos_desc += pElem->loop.items + 1;
                        goto update_loop_description;
                    }
                    /* count_desc != 0: the input drained or the body did not fit; descend so the
                     * element path makes element-granular progress (never a zero-progress spin). */
                }
                local_disp = (ptrdiff_t) conv_ptr - local_disp;
                PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, OPAL_DATATYPE_LOOP,
                           count_desc, pStack->disp + local_disp);
                pos_desc++;
            update_loop_description:
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                         process_loop, process_end_loop);
                goto process_data;
            }
        }

    complete_loop:
        if ((pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) && (0 != iov_len_local)) {
            /* A fragment boundary fell inside a predefined element (e.g. a host sender packed a
             * partial element that this device receiver must finish across fragments). Stash the
             * sub-element remainder and record partial_length so the next call resumes. */
            assert(iov_len_local < opal_datatype_basicDatatypes[pElem->elem.common.type]->size);
            opal_unpack_accelerator_partial_predefined(pConvertor, pElem, &count_desc, &iov_ptr,
                                                       &conv_ptr, &iov_len_local);
        }
        iov[iov_count].iov_len -= iov_len_local;
        total_unpacked += iov[iov_count].iov_len;
    }

    *max_data = total_unpacked;
    pConvertor->bConverted += total_unpacked;
    *out_size = iov_count;
    if (pConvertor->bConverted == pConvertor->local_size) {
        pConvertor->flags |= CONVERTOR_COMPLETED;
        return 1;
    }

    PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, pElem->elem.common.type, count_desc,
               conv_ptr - pConvertor->pBaseBuf);
    return 0;
}
