/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2019 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stddef.h>
#include <stdlib.h>

#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_datatype_internal.h"

#if OPAL_ENABLE_DEBUG
#    include "opal/util/output.h"

#    define DO_DEBUG(INST)             \
        if (opal_ddt_position_debug) { \
            INST                       \
        }
#else
#    define DO_DEBUG(INST)
#endif /* OPAL_ENABLE_DEBUG */

/* The pack/unpack functions need a cleanup. I have to create a proper interface to access
 * all basic functionalities, hence using them as basic blocks for all conversion functions.
 *
 * But first let's make some global assumptions:
 * - a datatype (with the flag DT_DATA set) will have the contiguous flags set if and only if
 *   the data is really contiguous (extent equal with size)
 * - for the OPAL_DATATYPE_LOOP type the DT_CONTIGUOUS flag set means that the content of the loop
 * is contiguous but with a gap in the beginning or at the end.
 * - the DT_CONTIGUOUS flag for the type OPAL_DATATYPE_END_LOOP is meaningless.
 */

static inline void position_single_block(opal_convertor_t *CONVERTOR, unsigned char **mem,
                                         ptrdiff_t mem_update, size_t *space, size_t space_update,
                                         size_t *cnt, size_t cnt_update)
{
    OPAL_DATATYPE_SAFEGUARD_POINTER(*mem, mem_update, (CONVERTOR)->pBaseBuf, (CONVERTOR)->pDesc,
                                    (CONVERTOR)->count);
    DO_DEBUG(opal_output(0, "position( %p, %lu ) => space %lu [prolog]\n", (void *) *mem,
                         (unsigned long) space_update, (unsigned long) (*space)););
    *mem += mem_update;
    *space -= space_update;
    *cnt -= cnt_update;
}

/**
 * Advance the convertors' position according to account for *COUNT elements. Update
 * the pointer and the remaining space accordingly.
 */
static inline void position_predefined_data(opal_convertor_t *CONVERTOR, dt_elem_desc_t *ELEM,
                                            size_t *COUNT, unsigned char **POINTER, size_t *SPACE)
{
    const ddt_elem_desc_t *_elem = &((ELEM)->elem);
    size_t total_count = (size_t) _elem->count * _elem->blocklen;
    /*
     * Two element sizes are in play. The packed stream (SPACE, the byte budget we are walking
     * over) is denominated in the convertor's stream sizes -- remote for a receive or a
     * converting send, local otherwise. The in-memory layout (the extent stride and the gap
     * between blocks) always uses the local basic-type size. They are equal for a homogeneous
     * convertor, so this reduces to the original single-size arithmetic.
     */
    const size_t stream_elem_size = CONVERTOR->sizes[_elem->common.type];
    const size_t local_elem_size = opal_datatype_basicDatatypes[_elem->common.type]->size;
    size_t cando_count = (*SPACE) / stream_elem_size;
    size_t do_now;
    unsigned char *_memory = (*POINTER) + _elem->disp;

    assert(*(COUNT) <= ((size_t) _elem->count * _elem->blocklen));

    if (cando_count > *(COUNT)) {
        cando_count = *(COUNT);
    } else if( 0 == cando_count )
        return;

    if (1 == _elem->blocklen) {
        DO_DEBUG(opal_output(0,
                             "position( %p, %" PRIsize_t " ) x (count %" PRIsize_t
                             ", extent %ld) => space %lu [prolog]\n",
                             (void *) _memory, (unsigned long) stream_elem_size, cando_count,
                             _elem->extent, (unsigned long) (*SPACE)););
        _memory += cando_count * _elem->extent;
        *SPACE -= cando_count * stream_elem_size;
        *COUNT -= cando_count;
        goto update_and_return;
    }

    /**
     * First check if we already did something on this element ?
     */
    do_now = (total_count - *(COUNT)); /* done elements */
    if (0 != do_now) {
        do_now = do_now % _elem->blocklen; /* partial blocklen? */

        if (0 != do_now) {
            size_t left_in_block = _elem->blocklen - do_now; /* left in the current blocklen */
            do_now = (left_in_block > cando_count) ? cando_count : left_in_block;

            /* Elements inside a block are contiguous in memory (local size) but stride the
             * packed stream by the stream size. */
            position_single_block(CONVERTOR, &_memory, do_now * local_elem_size, SPACE,
                                  do_now * stream_elem_size, COUNT, do_now);

            /* compensate if we just completed a blocklen */
            if (do_now == left_in_block) {
                _memory += _elem->extent - (_elem->blocklen * local_elem_size);
            }
            cando_count -= do_now;
        }
    }

    /**
     * Compute how many full blocklen we need to do and do them.
     */
    do_now = cando_count / _elem->blocklen;
    if (0 != do_now) {
        size_t block_stream_bytes = _elem->blocklen * stream_elem_size;
#if OPAL_ENABLE_DEBUG
        for (size_t _i = 0; _i < do_now; _i++) {
            position_single_block(CONVERTOR, &_memory, _elem->extent, SPACE, block_stream_bytes,
                                  COUNT, _elem->blocklen);
            cando_count -= _elem->blocklen;
        }
#else
        _memory += do_now * _elem->extent;
        *SPACE -= do_now * block_stream_bytes;
        *COUNT -= do_now * _elem->blocklen;
        cando_count -= do_now * _elem->blocklen;
#endif /* OPAL_ENABLE_DEBUG */
    }

    /**
     * As an epilog do anything left from the last blocklen.
     */
    do_now = cando_count;
    if (0 != do_now) {
        position_single_block(CONVERTOR, &_memory, do_now * local_elem_size, SPACE,
                              do_now * stream_elem_size, COUNT, do_now);
    }

update_and_return:
    *(POINTER) = _memory - _elem->disp;
}

int opal_convertor_generic_simple_position(opal_convertor_t *pConvertor, size_t *position)
{
    dt_stack_t *pStack; /* pointer to the position on the stack */
    uint32_t pos_desc;  /* actual position in the description of the derived datatype */
    size_t count_desc;  /* the number of items already done in the actual pos_desc */
    size_t iov_len_local;
    dt_elem_desc_t *description = pConvertor->use_desc->desc;
    dt_elem_desc_t *pElem; /* current position */
    unsigned char *base_pointer = pConvertor->pBaseBuf;
    ptrdiff_t extent = pConvertor->pDesc->ub - pConvertor->pDesc->lb;
    ptrdiff_t local_disp;

    DUMP("opal_convertor_generic_simple_position( %p, &%ld )\n", (void *) pConvertor,
         (long) *position);
    assert(*position > pConvertor->bConverted);

    /* We dont want to have to parse the datatype multiple times. What we are interested in
     * here is to compute the number of completed datatypes that we can move forward, update
     * the counters and compute the position taking in account only the remaining elements.
     * The only problem is that we have to modify all the elements on the stack.
     *
     * *position and bConverted are packed-stream byte offsets, so whole datatype instances are
     * skipped using the packed per-instance size (stream_size). For a homogeneous or plain-send
     * convertor that equals pDesc->size; for a receive (or converting send) it is the remote
     * footprint. The in-memory stack displacements still advance by the local extent.
     */
    iov_len_local = *position - pConvertor->bConverted;
    const size_t stream_size = pConvertor->remote_size / pConvertor->count;
    if (iov_len_local > stream_size) {
        pStack = pConvertor->pStack; /* we're working with the full stack */
        count_desc = iov_len_local / stream_size;
        DO_DEBUG(opal_output(0,
                             "position before %lu asked %lu data size %lu"
                             " iov_len_local %lu count_desc %" PRIsize_t "\n",
                             (unsigned long) pConvertor->bConverted, (unsigned long) *position,
                             (unsigned long) stream_size, (unsigned long) iov_len_local,
                             count_desc););
        /* Update all the stack including the last one */
        for (pos_desc = 0; pos_desc <= pConvertor->stack_pos; pos_desc++) {
            pStack[pos_desc].disp += count_desc * extent;
        }
        pConvertor->bConverted += count_desc * stream_size;
        iov_len_local = *position - pConvertor->bConverted;
        pStack[0].count -= count_desc;
        DO_DEBUG(opal_output(0, "after bConverted %lu remaining count %lu iov_len_local %lu\n",
                             (unsigned long) pConvertor->bConverted,
                             (unsigned long) pStack[0].count, (unsigned long) iov_len_local););
    }

    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc = pStack->index;
    base_pointer += pStack->disp;
    count_desc = pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &(description[pos_desc]);

    DO_DEBUG(opal_output(0,
                         "position start pos_desc %d count_desc %" PRIsize_t " disp %llx\n"
                         "stack_pos %d pos_desc %d count_desc %" PRIsize_t " disp %llx\n",
                         pos_desc, count_desc,
                         (unsigned long long) (base_pointer - pConvertor->pBaseBuf),
                         pConvertor->stack_pos, pStack->index, pStack->count,
                         (unsigned long long) pStack->disp););
    /* Last data has been only partially converted. Compute the relative position. partial_length
     * counts packed-stream bytes of a predefined element not yet consumed, so measure the element
     * in stream units too. */
    if (0 != pConvertor->partial_length) {
        size_t element_length = pConvertor->sizes[pElem->elem.common.type];
        size_t missing_length = element_length - pConvertor->partial_length;
        if (missing_length >= iov_len_local) {
            pConvertor->partial_length = (pConvertor->partial_length + iov_len_local)
                                         % element_length;
            pConvertor->bConverted += iov_len_local;
            assert(pConvertor->partial_length < element_length);
            return 0;
        }
        pConvertor->partial_length = 0;
        pConvertor->bConverted += missing_length;
        iov_len_local -= missing_length;
        count_desc--;
    }
    while (1) {
        if (OPAL_DATATYPE_END_LOOP
            == pElem->elem.common.type) { /* end of the entire datatype */
        process_end_loop:
            DO_DEBUG(opal_output(0,
                                 "position end_loop count %" PRIsize_t
                                 " stack_pos %d pos_desc %d disp %lx space %lu\n",
                                 pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp,
                                 (unsigned long) iov_len_local););
            if (--(pStack->count) == 0) { /* end of loop */
                if (pConvertor->stack_pos == 0) {
                    pConvertor->flags |= CONVERTOR_COMPLETED;
                    goto complete_loop; /* completed */
                }
                pConvertor->stack_pos--;
                pStack--;
                pos_desc++;
            } else {
                if (pStack->index == -1) {
                    pStack->disp += extent;
                    pos_desc = 0; /* back to the first element */
                } else {
                    assert(OPAL_DATATYPE_LOOP == description[pStack->index].loop.common.type);
                    pStack->disp += description[pStack->index].loop.extent;
                    pos_desc = pStack->index; /* go back to the loop start itself to give a chance
                                               * to move forward by entire loops */
                }
            }
            base_pointer = pConvertor->pBaseBuf + pStack->disp;
            DO_DEBUG(opal_output(0,
                                 "position new_loop count %" PRIsize_t
                                 " stack_pos %d pos_desc %d disp %lx space %lu\n",
                                 pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp,
                                 (unsigned long) iov_len_local););
            UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                     process_loop, process_end_loop);
            goto process_data;
        }
        if (OPAL_DATATYPE_LOOP == pElem->elem.common.type) {
        process_loop:
            local_disp = (ptrdiff_t) base_pointer;
            ddt_endloop_desc_t *end_loop = (ddt_endloop_desc_t *) (pElem + pElem->loop.items);
            /* end_loop->size is the loop body's LOCAL byte size; it matches the packed-stream
             * footprint only when no predefined type in this datatype changes size on the wire.
             * When one does (CONVERTOR_UNSAFE_SPLIT) the whole-iteration shortcut would consume the
             * wrong number of stream bytes, so disable it and let the per-element walk below --
             * which accounts in convertor->sizes -- advance one element at a time. */
            size_t full_loops = (pConvertor->flags & CONVERTOR_UNSAFE_SPLIT)
                                    ? 0
                                    : iov_len_local / end_loop->size;
            full_loops = count_desc <= full_loops ? count_desc : full_loops;
            if (full_loops) {
                base_pointer += full_loops * pElem->loop.extent;
                iov_len_local -= full_loops * end_loop->size;
                count_desc -= full_loops;

                if (0 == count_desc) { /* completed */
                    pos_desc += pElem->loop.items + 1;
                    goto update_loop_description;
                }
                /* Save the stack with the correct last_count value. */
            }
            local_disp = (ptrdiff_t) base_pointer - local_disp;
            PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, OPAL_DATATYPE_LOOP, count_desc,
                       pStack->disp + local_disp);
            pos_desc++;
        update_loop_description: /* update the current state */
            base_pointer = pConvertor->pBaseBuf + pStack->disp;
            DDT_DUMP_STACK(pConvertor->pStack, pConvertor->stack_pos, &description[pos_desc],
                           "advance loop");
            DO_DEBUG(opal_output(0,
                                 "position set loop count %" PRIsize_t
                                 " stack_pos %d pos_desc %d disp %lx space %lu\n",
                                 pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp,
                                 (unsigned long) iov_len_local););
            UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                     process_loop, process_end_loop);
            goto process_data;
        }
        while (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
        process_data:
            /* now here we have a basic datatype */
            position_predefined_data(pConvertor, pElem, &count_desc, &base_pointer, &iov_len_local);
            if (0 != count_desc) { /* completed */
                pConvertor->partial_length = iov_len_local;
                goto complete_loop;
            }
            base_pointer = pConvertor->pBaseBuf + pStack->disp;
            pos_desc++; /* advance to the next data */
            DO_DEBUG(opal_output(0,
                                 "position set loop count %" PRIsize_t
                                 " stack_pos %d pos_desc %d disp %lx space %lu\n",
                                 pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp,
                                 (unsigned long) iov_len_local););
            UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                     process_loop, process_end_loop);
            goto process_data;
        }
    }
complete_loop:
    pConvertor->bConverted = *position; /* update the already converted bytes */

    if (!(pConvertor->flags & CONVERTOR_COMPLETED)) {
        /* I complete an element, next step I should go to the next one */
        PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, pElem->elem.common.type, count_desc,
                   base_pointer - pConvertor->pBaseBuf);
        DO_DEBUG(opal_output(0,
                             "position save stack stack_pos %d pos_desc %d count_desc %" PRIsize_t
                             " disp %llx\n",
                             pConvertor->stack_pos, pStack->index, pStack->count,
                             (unsigned long long) pStack->disp););
        return 0;
    }
    return 1;
}
