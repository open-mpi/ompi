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
 * Copyright (c) 2017-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stddef.h>
#include <stdint.h>

#include "opal/datatype/opal_convertor_internal.h"
#include "opal/datatype/opal_datatype_internal.h"

#if OPAL_ENABLE_DEBUG
#    include "opal/util/output.h"

#    define DO_DEBUG(INST)         \
        if (opal_ddt_pack_debug) { \
            INST                   \
        }
#else
#    define DO_DEBUG(INST)
#endif /* OPAL_ENABLE_DEBUG */

#include "opal/datatype/opal_datatype_pack.h"
#include "opal/datatype/opal_datatype_prototypes.h"

/* the contig versions does not use the stack. They can easily retrieve
 * the status with just the information from pConvertor->bConverted.
 */
int32_t opal_pack_homogeneous_contig(opal_convertor_t *pConv, struct iovec *iov,
                                     uint32_t *out_size, size_t *max_data)
{
    dt_stack_t *pStack = pConv->pStack;
    unsigned char *source_base = NULL;
    uint32_t iov_count;
    size_t length = pConv->local_size - pConv->bConverted, initial_amount = pConv->bConverted;

    source_base = (pConv->pBaseBuf + pConv->pDesc->true_lb + pStack[0].disp + pStack[1].disp);

    /* There are some optimizations that can be done if the upper level
     * does not provide a buffer.
     */
    for (iov_count = 0; iov_count < (*out_size); iov_count++) {
        if (0 == length) {
            break;
        }
        if ((size_t) iov[iov_count].iov_len > length) {
            iov[iov_count].iov_len = length;
        }
        if (iov[iov_count].iov_base == NULL) {
            iov[iov_count].iov_base = (IOVBASE_TYPE *) source_base;
        } else {
            /* contiguous data just memcpy the smallest data in the user buffer */
            OPAL_DATATYPE_SAFEGUARD_POINTER(source_base, iov[iov_count].iov_len, pConv->pBaseBuf,
                                            pConv->pDesc, pConv->count);
            pConv->cbmemcpy(iov[iov_count].iov_base, source_base, iov[iov_count].iov_len, pConv);
        }
        length -= iov[iov_count].iov_len;
        pConv->bConverted += iov[iov_count].iov_len;
        pStack[0].disp += iov[iov_count].iov_len;
        source_base += iov[iov_count].iov_len;
    }

    /* update the return value */
    *max_data = pConv->bConverted - initial_amount;
    *out_size = iov_count;
    if (pConv->bConverted == pConv->local_size) {
        pConv->flags |= CONVERTOR_COMPLETED;
        return 1;
    }
    return 0;
}

int32_t opal_pack_homogeneous_contig_with_gaps(opal_convertor_t *pConv, struct iovec *iov,
                                               uint32_t *out_size, size_t *max_data)
{
    size_t remaining, length, initial_bytes_converted = pConv->bConverted;
    const opal_datatype_t *pData = pConv->pDesc;
    const ddt_elem_desc_t *model = NULL;
    dt_stack_t *stack = pConv->pStack;
    ptrdiff_t extent = pData->ub - pData->lb;
    dt_elem_desc_t elem;
    unsigned char *user_memory, *packed_buffer;
    uint32_t idx;
    size_t i;

    /* The memory layout is contiguous with gaps in the beginning and at the end. The datatype
     * true_lb is the initial displacement, the size the length of the contiguous area and the
     * extent represent how much we should jump between elements.
     */
    assert((pData->flags & OPAL_DATATYPE_FLAG_CONTIGUOUS) && ((ptrdiff_t) pData->size != extent));
    assert(pData->opt_desc.used <= 1);
    DO_DEBUG(opal_output(0, "pack_homogeneous_contig( pBaseBuf %p, iov_count %d )\n",
                         (void *) pConv->pBaseBuf, *out_size););

    if (NULL == iov[0].iov_base) {
        if (stack[1].type != opal_datatype_uint1.id) {
            stack[1].count = pData->size;
            stack[1].type = opal_datatype_uint1.id;
        }
        user_memory = pConv->pBaseBuf + pData->true_lb;

        for (idx = 0; (idx < (*out_size)) && stack[0].count; idx++) {
            iov[idx].iov_base = user_memory + stack[0].disp + stack[1].disp;
            iov[idx].iov_len = stack[1].count;

            pConv->bConverted += stack[1].count;

            stack[0].disp += extent;
            stack[0].count--;
            stack[1].disp = 0;
            stack[1].count = pData->size; /* we might need this to update the partial
                                           * length for the first iteration */
        }
        goto update_status_and_return;
    }

    /*
     * Select the typed path once. Ineligible datatypes must enter the original byte-copy loop
     * without carrying adaptive checks through every iovec and complete datatype repetition.
     */
    if (1 != pData->opt_desc.used) {  /* the optimizer did not reduce contiguous data to one entry */
        goto memcpy_path;
    }
    model = &pData->opt_desc.desc[0].elem;

    /* A counted entry or a large block cannot use the predefined inline mover efficiently. */
    if ((1 != model->count) || (OPAL_DATATYPE_PREDEFINED_MAX_INLINE_BLOCKLEN < model->blocklen)) {
        goto memcpy_path;
    }

    /* Replay datatype repetitions as one strided DATA entry through the predefined mover. */
    elem.elem.common.flags = OPAL_DATATYPE_FLAG_DATA;
    elem.elem.common.type = model->common.type;
    elem.elem.blocklen = model->blocklen;
    elem.elem.extent = extent;
    elem.elem.disp = pData->true_lb;

    for (idx = 0; idx < (*out_size); idx++) {
        remaining = pConv->local_size - pConv->bConverted;
        if (0 == remaining) {
            break; /* we're done this time */
        }
        if (remaining > iov[idx].iov_len) {
            remaining = iov[idx].iov_len;
        }
        packed_buffer = (unsigned char *) iov[idx].iov_base;
        pConv->bConverted += remaining;
        user_memory = pConv->pBaseBuf + pData->true_lb + stack[0].disp + stack[1].disp;

        DO_DEBUG(opal_output(0, "pack_homogeneous_contig( user_memory %p, packed_buffer %p length %" PRIsize_t "\n",
                             (void *) user_memory, (void *) packed_buffer, remaining););

        /* disp is a byte offset, so finish a fragmented datatype instance before full instances. */
        length = pData->size - stack[1].disp;
        if ((0 != stack[1].disp) && (length <= remaining)) {
            OPAL_DATATYPE_SAFEGUARD_POINTER(user_memory, length, pConv->pBaseBuf, pData,
                                            pConv->count);
            DO_DEBUG(opal_output(0, "pack dest %p src %p length %" PRIsize_t " [prologue]\n",
                                 (void *) user_memory, (void *) packed_buffer, length););
            pConv->cbmemcpy(packed_buffer, user_memory, length, pConv);
            packed_buffer += length;
            remaining -= length;
            stack[0].count--;
            stack[0].disp += extent;
            stack[1].disp = 0;
            if (0 == stack[0].count) {
                break;
            }
            user_memory = pConv->pBaseBuf + pData->true_lb + stack[0].disp;
        }

        /* The prologue either reached an instance boundary or left less than one instance. */
        if (pData->size <= remaining) {
            unsigned char *memory = pConv->pBaseBuf + stack[0].disp;
            size_t full_length = remaining - (remaining % pData->size);
            size_t full_count = full_length / pData->size;
            size_t count_left = full_count;

            /* DATA descriptor block counts are 32-bit; split larger convertor counts into chunks. */
            while (0 < count_left) {
                uint32_t chunk_count = (UINT32_MAX < count_left) ? UINT32_MAX : (uint32_t) count_left;
                size_t count_desc = (size_t) chunk_count * model->blocklen;
                size_t space = (size_t) chunk_count * pData->size;

                elem.elem.count = chunk_count;
                PACK_PREDEFINED_DATATYPE(pConv, &elem, count_desc, memory, packed_buffer, space);
                count_left -= chunk_count;
            }

            remaining -= full_length;
            stack[0].count -= full_count;
            stack[0].disp += full_count * extent;
            user_memory = pConv->pBaseBuf + pData->true_lb + stack[0].disp;
        }

        if (0 != remaining) {
            /* A fragment may split a predefined element; retain its byte displacement. */
            OPAL_DATATYPE_SAFEGUARD_POINTER(user_memory, remaining, pConv->pBaseBuf, pData,
                                            pConv->count);
            DO_DEBUG(opal_output(0, "4. pack dest %p src %p length %" PRIsize_t "\n",
                                 (void *) user_memory, (void *) packed_buffer, remaining););
            pConv->cbmemcpy(packed_buffer, user_memory, remaining, pConv);
            stack[1].disp += remaining;
        }
    }

    /* Materialize the stack state once when another pack call must resume this conversion. */
    if (pConv->bConverted < pConv->local_size) {
        if (0 == stack[1].disp) {
            stack[1].type = model->common.type;
            stack[1].count = model->blocklen;
        } else {
            stack[1].type = opal_datatype_uint1.id;
            stack[1].count = pData->size - stack[1].disp;
        }
    }
    goto update_status_and_return;

memcpy_path:
    for (idx = 0; idx < (*out_size); idx++) {
        remaining = pConv->local_size - pConv->bConverted;
        if (0 == remaining) {
            break;
        }
        if (remaining > iov[idx].iov_len) {
            remaining = iov[idx].iov_len;
        }
        packed_buffer = (unsigned char *) iov[idx].iov_base;
        pConv->bConverted += remaining;
        user_memory = pConv->pBaseBuf + pData->true_lb + stack[0].disp + stack[1].disp;

        DO_DEBUG(opal_output(0,
                             "pack_homogeneous_contig( user_memory %p, packed_buffer %p length %"
                             PRIsize_t "\n",
                             (void *) user_memory, (void *) packed_buffer, remaining););

        length = pData->size - stack[1].disp;
        if ((0 != stack[1].disp) && (length <= remaining)) {
            OPAL_DATATYPE_SAFEGUARD_POINTER(user_memory, length, pConv->pBaseBuf, pData,
                                            pConv->count);
            DO_DEBUG(opal_output(0, "pack dest %p src %p length %" PRIsize_t " [prologue]\n",
                                 (void *) user_memory, (void *) packed_buffer, length););
            pConv->cbmemcpy(packed_buffer, user_memory, length, pConv);
            packed_buffer += length;
            remaining -= length;
            stack[0].count--;
            stack[0].disp += extent;
            stack[1].disp = 0;
            if (0 == stack[0].count) {
                break;
            }
            user_memory = pConv->pBaseBuf + pData->true_lb + stack[0].disp;
        }

        for (i = 0; pData->size <= remaining; i++) {
            OPAL_DATATYPE_SAFEGUARD_POINTER(user_memory, pData->size, pConv->pBaseBuf, pData,
                                            pConv->count);
            DO_DEBUG(opal_output(0,
                                 "pack dest %p src %p length %" PRIsize_t " [%" PRIsize_t
                                 "/%" PRIsize_t "\n",
                                 (void *) user_memory, (void *) packed_buffer, pData->size,
                                 remaining, iov[idx].iov_len););
            pConv->cbmemcpy(packed_buffer, user_memory, pData->size, pConv);
            packed_buffer += pData->size;
            user_memory += extent;
            remaining -= pData->size;
        }
        stack[0].count -= i;
        stack[0].disp += i * extent;

        if (0 != remaining) {
            OPAL_DATATYPE_SAFEGUARD_POINTER(user_memory, remaining, pConv->pBaseBuf, pData,
                                            pConv->count);
            DO_DEBUG(opal_output(0, "4. pack dest %p src %p length %" PRIsize_t "\n",
                                 (void *) user_memory, (void *) packed_buffer, remaining););
            pConv->cbmemcpy(packed_buffer, user_memory, remaining, pConv);
            stack[1].disp += remaining;
        }
    }

    /* The byte mover resumes from the number of bytes left in the current datatype instance. */
    if (pConv->bConverted < pConv->local_size) {
        stack[1].type = opal_datatype_uint1.id;
        stack[1].count = pData->size - stack[1].disp;
    }

update_status_and_return:
    *out_size = idx;
    *max_data = pConv->bConverted - initial_bytes_converted;
    if (pConv->bConverted == pConv->local_size) {
        pConv->flags |= CONVERTOR_COMPLETED;
        return 1;
    }
    return 0;
}

/*
 * Performance-critical homogeneous pack/unpack backend.
 *
 * The inlined pack and unpack functions deliberately keep descriptor traversal, partial-element
 * handling, loop-stack management, and predefined data movement in one large function. The
 * PACK_PREDEFINED_DATATYPE and UNPACK_PREDEFINED_DATATYPE macros reach header-resident inline
 * helpers, allowing the compiler to fold the homogeneous path into the interpreter. Small
 * blocklengths then expand into type-aware scalar movers, while larger blocks retain the inline
 * control path around the convertor memcpy callback. UPDATE_INTERNAL_COUNTERS similarly expands
 * direct jumps to the DATA, LOOP, and END_LOOP handlers without a separate dispatcher call.
 *
 * This structure is intentionally sensitive to compiler decisions, code layout, and architecture.
 * Every change must be evaluated for correctness with full and fragmented buffers and benchmarked
 * across representative datatypes, message sizes, compilers, and architectures. Generated code
 * should also be inspected whenever control flow or inlining changes.
 *
 * The implementation relies on these descriptor invariants:
 * - a datatype (with the flag DT_DATA set) will have the contiguous flags set if and only if
 *   the data is really contiguous (extent equal with size)
 * - for the OPAL_DATATYPE_LOOP type the DT_CONTIGUOUS flag set means that the content of the loop
 * is contiguous but with a gap in the beginning or at the end.
 * - the DT_CONTIGUOUS flag for the type OPAL_DATATYPE_END_LOOP is meaningless.
 *
 * The descriptor interpreter uses direct jumps between DATA, LOOP, and END_LOOP handlers. Each
 * transition keeps its small dispatch sequence local because routing through a shared dispatcher
 * adds an unconditional branch to the critical path.
 */
int32_t opal_generic_inlined_pack(opal_convertor_t *pConvertor, struct iovec *iov,
                                  uint32_t *out_size, size_t *max_data)
{
    dt_stack_t *pStack;      /* pointer to the position on the stack */
    uint32_t pos_desc;       /* actual position in the description of the derived datatype */
    size_t count_desc;       /* the number of items already done in the actual pos_desc */
    size_t total_packed = 0; /* total amount packed this time */
    dt_elem_desc_t *description;
    dt_elem_desc_t *pElem;
    const opal_datatype_t *pData = pConvertor->pDesc;
    unsigned char *conv_ptr, *iov_ptr;
    size_t iov_len_local;
    ptrdiff_t local_disp;
    uint32_t iov_count;

    DO_DEBUG(opal_output(0, "opal_convertor_generic_inlined_pack( %p:%p, {%p, %lu}, %d )\n",
                         (void *) pConvertor, (void *) pConvertor->pBaseBuf,
                         (void *) iov[0].iov_base, (unsigned long) iov[0].iov_len, *out_size););

    description = pConvertor->use_desc->desc;

    /* The first step adds both displacements to the source. Subsequent descriptor transitions
     * restore conv_ptr from the stack because conversion can stop in the middle of a DATA entry. */
    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc = pStack->index;
    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
    count_desc = pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &(description[pos_desc]);

    DO_DEBUG(opal_output(0,
                         "pack start pos_desc %d count_desc %" PRIsize_t " disp %ld\n"
                         "stack_pos %d pos_desc %d count_desc %" PRIsize_t " disp %ld\n",
                         pos_desc, count_desc, (long) (conv_ptr - pConvertor->pBaseBuf),
                         pConvertor->stack_pos, pStack->index, pStack->count, pStack->disp););

    for (iov_count = 0; iov_count < (*out_size); iov_count++) {
        iov_ptr = (unsigned char *) iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;

        if (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            if (((size_t) pElem->elem.count * pElem->elem.blocklen) != count_desc) {
                /* we have a partial (less than blocklen) basic datatype */
                int rc = PACK_PARTIAL_BLOCKLEN(pConvertor, pElem, count_desc, conv_ptr, iov_ptr,
                                               iov_len_local);
                if (0 == rc) { /* not done */
                    goto complete_loop;
                }
                if (0 == count_desc) {
                    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                    pos_desc++; /* advance to the next data */
                    UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc);
                }
            }
        }

        /* Keep the per-iovec entry structured. Direct jumps here cause the compiler to reshape
         * the complete interpreter and regress otherwise unrelated datatype layouts. */
        while (1) {
            if (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            process_data:
                /* Pack one DATA descriptor. A partial output buffer exits with count_desc
                 * preserving the exact position. */
                PACK_PREDEFINED_DATATYPE(pConvertor, pElem, count_desc, conv_ptr, iov_ptr,
                                         iov_len_local);
                if (0 != count_desc) {
                    goto complete_loop;
                }
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                pos_desc++;
                /* Keep dispatch local to each transition. Sharing it through another label would
                 * add an unconditional branch before the descriptor-type branch. */
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc);
            }
            if (OPAL_DATATYPE_END_LOOP == pElem->elem.common.type) {
            process_end_loop:
                DO_DEBUG(opal_output(0,
                                     "pack end_loop count %" PRIsize_t " stack_pos %d"
                                     " pos_desc %d disp %ld space %lu\n",
                                     pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp,
                                     (unsigned long) iov_len_local););
                if (--(pStack->count) == 0) { /* end of loop */
                    if (0 == pConvertor->stack_pos) {
                        /* we're done. Force the exit of the main for loop (around iovec) */
                        *out_size = iov_count;
                        goto complete_loop;
                    }
                    pConvertor->stack_pos--; /* go one position up on the stack */
                    pStack--;
                    pos_desc++; /* and move to the next element */
                } else {
                    pos_desc = pStack->index + 1; /* jump back to the beginning of the loop */
                    if (pStack->index == -1) {    /* If it's the datatype count loop */
                        pStack->disp += (pData->ub - pData->lb); /* jump by the datatype extent */
                    } else {
                        assert(OPAL_DATATYPE_LOOP == description[pStack->index].loop.common.type);
                        pStack->disp += description[pStack->index].loop.extent; /* jump by the loop extent */
                    }
                }
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                DO_DEBUG(opal_output(0,
                                     "pack new_loop count %" PRIsize_t " stack_pos %d pos_desc %d"
                                     " disp %ld space %lu\n",
                                     pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp,
                                     (unsigned long) iov_len_local););
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc);
            }
            if (OPAL_DATATYPE_LOOP == pElem->elem.common.type) {
            process_loop:
                local_disp = (ptrdiff_t) conv_ptr;
                if (pElem->loop.common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS) {
                    PACK_CONTIGUOUS_LOOP(pConvertor, pElem, count_desc, conv_ptr, iov_ptr,
                                         iov_len_local);
                    if (0 == count_desc) { /* completed */
                        pos_desc += pElem->loop.items + 1;
                        goto update_loop_description;
                    }
                    /* Save the stack with the correct last_count value. */
                }
                local_disp = (ptrdiff_t) conv_ptr - local_disp;
                PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, OPAL_DATATYPE_LOOP, count_desc,
                           pStack->disp + local_disp);
                pos_desc++;
            update_loop_description:
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                DDT_DUMP_STACK(pConvertor->pStack, pConvertor->stack_pos, &description[pos_desc],
                               "advance loop");
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc);
            }
        }

    complete_loop:
        iov[iov_count].iov_len -= iov_len_local; /* update the amount of valid data */
        total_packed += iov[iov_count].iov_len;
    }
    *max_data = total_packed;
    pConvertor->bConverted += total_packed; /* update the already converted bytes */
    *out_size = iov_count;
    if (pConvertor->bConverted == pConvertor->remote_size) {
        pConvertor->flags |= CONVERTOR_COMPLETED;
        return 1;
    }
    /* Save the global position for the next round */
    PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, pElem->elem.common.type, count_desc,
               conv_ptr - pConvertor->pBaseBuf);
    DO_DEBUG(opal_output(0,
                         "pack save stack stack_pos %d pos_desc %d count_desc %" PRIsize_t " disp %ld\n",
                         pConvertor->stack_pos, pStack->index, pStack->count, pStack->disp););
    return 0;
}

/*
 * Remember that the first item in the stack (ie. position 0) is the number
 * of times the datatype is involved in the operation (ie. the count argument
 * in the MPI_ call).
 */
/* Convert data from multiple input buffers (as received from the network layer)
 * to a contiguous output buffer with a predefined size.
 * return OPAL_SUCCESS if everything went OK and if there is still room before the complete
 *          conversion of the data (need additional call with others input buffers )
 *        1 if everything went fine and the data was completely converted
 *       -1 something wrong occurs.
 */

static inline void
pack_predefined_heterogeneous(opal_convertor_t *CONVERTOR,
                              const dt_elem_desc_t *ELEM, size_t *COUNT,
                              unsigned char **memory,
                              unsigned char **packed, size_t *SPACE)
{
    const opal_convertor_master_t *master = (CONVERTOR)->master;
    const ddt_elem_desc_t *_elem = &((ELEM)->elem);
    size_t cando_count = *(COUNT);
    size_t remote_elem_size = master->remote_sizes[_elem->common.type];
    size_t blocklen_bytes = remote_elem_size;
    unsigned char *_memory = (*memory) + _elem->disp;
    unsigned char *_packed = *packed;
    char *from, *to;
    size_t copied;

    assert(0 == (cando_count % _elem->blocklen)); /* no partials here */
    assert(*(COUNT) <= ((size_t) _elem->count * _elem->blocklen));

    if ((remote_elem_size * cando_count) > *(SPACE))
        cando_count = (*SPACE) / blocklen_bytes;

    from = (char *) _memory;
    to = (char *) _packed;
    copied = master->pFunctions[_elem->common.type](
        CONVERTOR, cando_count, _elem->blocklen, _elem->count, &from, *SPACE, _elem->extent, &to,
        *SPACE, _elem->blocklen * remote_elem_size);
    *(COUNT) -= copied;
    _memory = (unsigned char *) from;
    _packed = (unsigned char *) to;

    *(memory) = _memory - _elem->disp;
    *(SPACE) -= (_packed - *packed);
    *(packed) = _packed;
}

int32_t opal_pack_general(opal_convertor_t *pConvertor, struct iovec *iov,
                          uint32_t *out_size, size_t *max_data)
{
    dt_stack_t *pStack;      /* pointer to the position on the stack */
    uint32_t pos_desc;       /* actual position in the description of the derived datatype */
    size_t count_desc;       /* the number of items already done in the actual pos_desc */
    size_t total_packed = 0; /* total amount packed this time */
    dt_elem_desc_t *description;
    dt_elem_desc_t *pElem;
    const opal_datatype_t *pData = pConvertor->pDesc;
    unsigned char *conv_ptr, *iov_ptr;
    size_t iov_len_local;
    ptrdiff_t local_disp;
    uint32_t iov_count;

    DO_DEBUG(opal_output(0, "opal_convertor_general_pack( %p:%p, {%p, %lu}, %d )\n",
                         (void *) pConvertor, (void *) pConvertor->pBaseBuf,
                         (void *) iov[0].iov_base, (unsigned long) iov[0].iov_len, *out_size););

    description = pConvertor->use_desc->desc;

    /* For the first step we have to add both displacement to the source. After in the
     * main while loop we will set back the conv_ptr to the correct value. This is
     * due to the fact that the convertor can stop in the middle of a data with a count
     */
    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc = pStack->index;
    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
    count_desc = pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &(description[pos_desc]);

    DO_DEBUG(opal_output(0,
                         "pack start pos_desc %d count_desc %" PRIsize_t " disp %ld\n"
                         "stack_pos %d pos_desc %d count_desc %" PRIsize_t " disp %ld\n",
                         pos_desc, count_desc, (long) (conv_ptr - pConvertor->pBaseBuf),
                         pConvertor->stack_pos, pStack->index, pStack->count, pStack->disp););

    for (iov_count = 0; iov_count < (*out_size); iov_count++) {
        iov_ptr = (unsigned char *) iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;
        while (1) {
            while (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            process_data:
                /* now here we have a basic datatype */
                DO_DEBUG(opal_output(0, "pack (%p:%ld, %" PRIsize_t ", %ld) -> (%p, %ld) type %s\n",
                                     (void *) pConvertor->pBaseBuf,
                                     conv_ptr + pElem->elem.disp - pConvertor->pBaseBuf, count_desc,
                                     description[pos_desc].elem.extent, (void *) iov_ptr, iov_len_local,
                                     opal_datatype_basicDatatypes[pElem->elem.common.type]->name););

                pack_predefined_heterogeneous(pConvertor, pElem, &count_desc, &conv_ptr, &iov_ptr,
                                              &iov_len_local);
#if 0
                PACK_PREDEFINED_DATATYPE( pConvertor, pElem, count_desc,
                                          conv_ptr, iov_ptr, iov_len_local );
#endif
                if (0 == count_desc) { /* completed */
                    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                    pos_desc++; /* advance to the next data */
                    UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc);
                }
                goto complete_loop;
            }
            if (OPAL_DATATYPE_END_LOOP == pElem->elem.common.type) { /* end of the current loop */
            process_end_loop:
                DO_DEBUG(opal_output(0,
                                     "pack end_loop count %" PRIsize_t " stack_pos %d pos_desc %d disp %ld space %lu\n",
                                     pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp,
                                     (unsigned long) iov_len_local););
                if (--(pStack->count) == 0) { /* end of loop */
                    if (0 == pConvertor->stack_pos) {
                        /* we lie about the size of the next element in order to
                         * make sure we exit the main loop.
                         */
                        *out_size = iov_count;
                        goto complete_loop; /* completed */
                    }
                    pConvertor->stack_pos--;
                    pStack--;
                    pos_desc++;
                } else {
                    pos_desc = pStack->index + 1;
                    if (pStack->index == -1) {
                        pStack->disp += (pData->ub - pData->lb);
                    } else {
                        assert(OPAL_DATATYPE_LOOP == description[pStack->index].loop.common.type);
                        pStack->disp += description[pStack->index].loop.extent;
                    }
                }
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                DO_DEBUG(opal_output(0,
                                     "pack new_loop count %" PRIsize_t " stack_pos %d pos_desc %d"
                                     " disp %ld space %lu\n",
                                     pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp,
                                     (unsigned long) iov_len_local););
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc);
            }
            if (OPAL_DATATYPE_LOOP == pElem->elem.common.type) {
            process_loop:
                local_disp = (ptrdiff_t) conv_ptr;
#if 0
                if( pElem->loop.common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS ) {
                    PACK_CONTIGUOUS_LOOP( pConvertor, pElem, count_desc,
                                          conv_ptr, iov_ptr, iov_len_local );
                    if( 0 == count_desc ) {  /* completed */
                        pos_desc += pElem->loop.items + 1;
                        goto update_loop_description;
                    }
                    /* Save the stack with the correct last_count value. */
                }
#endif /* in a heterogeneous environment we can't handle the contiguous loops */
                local_disp = (ptrdiff_t) conv_ptr - local_disp;
                PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, OPAL_DATATYPE_LOOP, count_desc,
                           pStack->disp + local_disp);
                pos_desc++;
#if 0
            update_loop_description:  /* update the current state */
#endif /* in a heterogeneous environment we can't handle the contiguous loops */
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                DDT_DUMP_STACK(pConvertor->pStack, pConvertor->stack_pos,
                               &description[pos_desc], "advance loop");
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc);
            }
        }
    complete_loop:
        iov[iov_count].iov_len -= iov_len_local; /* update the amount of valid data */
        total_packed += iov[iov_count].iov_len;
    }
    *max_data = total_packed;
    pConvertor->bConverted += total_packed; /* update the already converted bytes */
    *out_size = iov_count;
    size_t expected_packed_size;
    opal_convertor_get_packed_size(pConvertor, &expected_packed_size);
    if (pConvertor->bConverted == expected_packed_size) {
        pConvertor->flags |= CONVERTOR_COMPLETED;
        return 1;
    }
    /* Save the global position for the next round */
    PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, pElem->elem.common.type, count_desc,
               conv_ptr - pConvertor->pBaseBuf);
    DO_DEBUG(opal_output(0,
                         "pack save stack stack_pos %d pos_desc %d count_desc %" PRIsize_t " disp %ld\n",
                         pConvertor->stack_pos, pStack->index, pStack->count, pStack->disp););
    return 0;
}
