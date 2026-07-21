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
 * Copyright (c) 2008-2009 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2026 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stddef.h>
#include <stdio.h>

#include "opal/datatype/opal_convertor_internal.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/mca/accelerator/accelerator.h"
#include "opal/mca/accelerator/base/base.h"

#if OPAL_ENABLE_DEBUG
#    include "opal/util/output.h"

#    define DO_DEBUG(INST)           \
        if (opal_ddt_unpack_debug) { \
            INST                     \
        }
#else
#    define DO_DEBUG(INST)
#endif /* OPAL_ENABLE_DEBUG */

#include "opal/datatype/opal_datatype_memcpy.h"
#include "opal/datatype/opal_datatype_prototypes.h"
#include "opal/datatype/opal_datatype_unpack.h"

/**
 * This function will be used to unpack all datatypes that have the contiguous flag set.
 * Several types of datatypes match this criterion, not only the contiguous one, but
 * the ones that have gaps in the beginning and/or at the end but where the data to
 * be unpacked is contiguous. However, this function only work for homogeneous cases
 * and the datatype that are contiguous and where the extent is equal to the size are
 * taken in account directly in the opal_convertor_unpack function (in convertor.c) for
 * the homogeneous case.
 */
int32_t opal_unpack_homogeneous_contig(opal_convertor_t *pConv, struct iovec *iov,
                                       uint32_t *out_size, size_t *max_data)
{
    const opal_datatype_t *pData = pConv->pDesc;
    unsigned char *user_memory, *packed_buffer;
    uint32_t iov_idx;
    uint32_t __opal_attribute_unused__ i;
    size_t remaining, initial_bytes_converted = pConv->bConverted;
    dt_stack_t *stack = pConv->pStack;
    ptrdiff_t extent = pData->ub - pData->lb;

    DO_DEBUG(opal_output(0, "unpack_homogeneous_contig( pBaseBuf %p, iov count %d )\n",
                         (void *) pConv->pBaseBuf, *out_size););

    /*
     * Raw byte copier; the incoming stream is consumed at arbitrary byte boundaries. That is only
     * valid when the convertor is safe to split (homogeneous or same-size byte-swap). A size-changing
     * conversion must unpack through opal_unpack_general. See CONVERTOR_UNSAFE_SPLIT and the matching
     * assert in opal_unpack_partial_predefined.
     */
    assert(!(pConv->flags & CONVERTOR_UNSAFE_SPLIT));

    if (stack[1].type != opal_datatype_uint1.id) {
        stack[1].count *= opal_datatype_basicDatatypes[stack[1].type]->size;
        stack[1].type = opal_datatype_uint1.id;
    }

    if ((ptrdiff_t) pData->size == extent) {
        for (iov_idx = 0; iov_idx < (*out_size); iov_idx++) {
            remaining = pConv->local_size - pConv->bConverted;
            if (0 == remaining) {
                break; /* we're done this time */
            }
            if (remaining > iov[iov_idx].iov_len) {
                remaining = iov[iov_idx].iov_len;
            }

            packed_buffer = (unsigned char *) iov[iov_idx].iov_base;
            user_memory = pConv->pBaseBuf + pData->true_lb + pConv->bConverted;

            /* contiguous data or basic datatype with count */
            OPAL_DATATYPE_SAFEGUARD_POINTER(user_memory, remaining, pConv->pBaseBuf, pData,
                                            pConv->count);
            DO_DEBUG(opal_output(0, "unpack contig [%d] dest %p src %p length %" PRIsize_t "\n",
                                 iov_idx, (void *) user_memory, (void *) packed_buffer,
                                 remaining););
            pConv->cbmemcpy(user_memory, packed_buffer, remaining, pConv);
            pConv->bConverted += remaining; /* how much will get unpacked this time */
        }
    } else {
        for (iov_idx = 0; iov_idx < (*out_size); iov_idx++) {
            remaining = pConv->local_size - pConv->bConverted;
            if (0 == remaining) {
                break; /* we're done this time */
            }
            if (remaining > iov[iov_idx].iov_len) {
                remaining = iov[iov_idx].iov_len;
            }

            packed_buffer = (unsigned char *) iov[iov_idx].iov_base;
            user_memory = pConv->pBaseBuf + pData->true_lb + stack[0].disp + stack[1].disp;
            pConv->bConverted += remaining; /* how much will get unpacked this time */

            for (i = 0; stack[1].count <= remaining; i++) { /* partial or full data */
                OPAL_DATATYPE_SAFEGUARD_POINTER(user_memory, stack[1].count, pConv->pBaseBuf, pData,
                                                pConv->count);
                DO_DEBUG(opal_output(0,
                                     "unpack gaps [%d] dest %p src %p length %" PRIsize_t " [%d]\n",
                                     iov_idx, (void *) user_memory, (void *) packed_buffer,
                                     stack[1].count, i););
                pConv->cbmemcpy(user_memory, packed_buffer, stack[1].count, pConv);

                packed_buffer += stack[1].count;
                remaining -= stack[1].count;

                stack[0].count--;
                stack[0].disp += extent;
                stack[1].count = pData->size;
                stack[1].disp = 0;

                user_memory = pConv->pBaseBuf + pData->true_lb + stack[0].disp;
            }

            /* Copy the last bits */
            if (0 != remaining) {
                OPAL_DATATYPE_SAFEGUARD_POINTER(user_memory, remaining, pConv->pBaseBuf, pData,
                                                pConv->count);
                DO_DEBUG(
                    opal_output(0,
                                "unpack gaps [%d] dest %p src %p length %" PRIsize_t " [epilog]\n",
                                iov_idx, (void *) user_memory, (void *) packed_buffer, remaining););
                pConv->cbmemcpy(user_memory, packed_buffer, remaining, pConv);
                stack[1].count -= remaining;
                stack[1].disp += remaining; /* keep the += in case we are copying less that the
                                               datatype size */
                assert(stack[1].count);
            }
        }
    }
    *out_size = iov_idx; /* we only reach this line after the for loop successfully complete */
    *max_data = pConv->bConverted - initial_bytes_converted;
    if (pConv->bConverted == pConv->local_size) {
        pConv->flags |= CONVERTOR_COMPLETED;
    }
    return !!(pConv->flags & CONVERTOR_COMPLETED); /* done or not */
}

/*
 * Production homogeneous unpack interpreter using in-function typed movers: the default unpack
 * fAdvance for non-contiguous derived datatypes.
 *
 * Complete DATA entries jump to a mover specialized for their predefined C type, and consecutive
 * entries of the same type remain in that mover. Partial predefined elements, descriptor loops,
 * stack state, and fragmented iovecs use the shared handlers. The medium-block byte limit is
 * deliberately unpack-specific because unpack scatters into datatype memory instead of gathering
 * from it.
 */
int32_t opal_generic_inlined_unpack(opal_convertor_t *pConvertor, struct iovec *iov,
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

    DO_DEBUG(opal_output(0, "opal_convertor_generic_inlined_unpack( %p, iov[%u] = {%p, %lu} )\n",
                         (void *) pConvertor, *out_size, (void *) iov[0].iov_base,
                         (unsigned long) iov[0].iov_len););

    /*
     * The inlined homogeneous mover can consume the incoming stream mid predefined element at a
     * fragment boundary, so it is only valid for a convertor that is safe to split. A size-changing
     * conversion must go through opal_unpack_general instead. See CONVERTOR_UNSAFE_SPLIT.
     */
    assert(!(pConvertor->flags & CONVERTOR_UNSAFE_SPLIT));

    description = pConvertor->use_desc->desc;

    /* Restore the exact DATA-entry position because conversion can stop within an entry. */
    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc = pStack->index;
    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
    count_desc = pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &description[pos_desc];

    DO_DEBUG(opal_output(0,
                         "unpack start pos_desc %d count_desc %" PRIsize_t " disp %ld\n"
                         "stack_pos %d pos_desc %d count_desc %" PRIsize_t " disp %ld\n",
                         pos_desc, count_desc, (long) (conv_ptr - pConvertor->pBaseBuf),
                         pConvertor->stack_pos, pStack->index, pStack->count,
                         (long) pStack->disp););

    for (iov_count = 0; iov_count < *out_size; iov_count++) {
        iov_ptr = (unsigned char *) iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;

        /* Finish any predefined element or block that was split by the previous iovec. */
        if (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            if (0 != pConvertor->partial_length) {
                opal_unpack_partial_predefined(pConvertor, pElem, &count_desc, &iov_ptr, &conv_ptr,
                                               &iov_len_local);
                if (0 == count_desc) {
                    assert(0 == pConvertor->partial_length);
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
                int rc = UNPACK_PARTIAL_BLOCKLEN(pConvertor, pElem, count_desc, iov_ptr, conv_ptr,
                                                 iov_len_local);

                if (0 == rc) {
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
            if (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
                goto process_data;

            process_data_complete:
                if (0 != count_desc) {
                    goto complete_loop;
                }
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                pos_desc++;
                goto process_next_element;
            }
            if (OPAL_DATATYPE_END_LOOP == pElem->elem.common.type) {
            process_end_loop:
                DO_DEBUG(opal_output(0,
                                     "unpack end_loop count %" PRIsize_t
                                     " stack_pos %d pos_desc %d disp %ld space %" PRIsize_t "\n",
                                     pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp,
                                     iov_len_local););
                if (0 == --pStack->count) {
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
                        pStack->disp += pData->ub - pData->lb;
                    } else {
                        assert(OPAL_DATATYPE_LOOP == description[pStack->index].loop.common.type);
                        pStack->disp += description[pStack->index].loop.extent;
                    }
                }
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                DO_DEBUG(opal_output(0,
                                     "unpack new_loop count %" PRIsize_t
                                     " stack_pos %d pos_desc %d disp %ld space %" PRIsize_t "\n",
                                     pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp,
                                     iov_len_local););
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc, process_loop,
                                         process_end_loop);
                goto process_data;
            }
            if (OPAL_DATATYPE_LOOP == pElem->elem.common.type) {
            process_loop:
                local_disp = (ptrdiff_t) conv_ptr;
                if (pElem->loop.common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS) {
                    UNPACK_CONTIGUOUS_LOOP(pConvertor, pElem, count_desc, iov_ptr, conv_ptr,
                                           iov_len_local);
                    if (0 == count_desc) {
                        pos_desc += pElem->loop.items + 1;
                        goto update_loop_description;
                    }
                }
                local_disp = (ptrdiff_t) conv_ptr - local_disp;
                PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, OPAL_DATATYPE_LOOP, count_desc,
                           pStack->disp + local_disp);
                pos_desc++;
            update_loop_description:
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                DDT_DUMP_STACK(pConvertor->pStack, pConvertor->stack_pos, &description[pos_desc],
                               "advance loop");
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc, process_loop,
                                         process_end_loop);
                goto process_data;
            }
        }

    complete_loop:
        assert(pElem->elem.common.type < OPAL_DATATYPE_MAX_PREDEFINED);
        if ((pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) && (0 != iov_len_local)) {
            unsigned char *temporary = conv_ptr;

            /* Keep an incomplete predefined element in the convertor until the next call. */
            assert(iov_len_local
                   < opal_datatype_basicDatatypes[pElem->elem.common.type]->size);
            opal_unpack_partial_predefined(pConvertor, pElem, &count_desc, &iov_ptr, &temporary,
                                           &iov_len_local);
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
    DO_DEBUG(opal_output(0,
                         "unpack save stack stack_pos %d pos_desc %d count_desc %" PRIsize_t
                         " disp %ld\n",
                         pConvertor->stack_pos, pStack->index, pStack->count,
                         (long) pStack->disp););
    return 0;

process_next_element:
    UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc, process_loop,
                             process_end_loop);
    goto process_data;

process_data:
    /* Select a typed mover when entering or resuming a DATA entry. */
#define OPAL_DATATYPE_UNPACK_DISPATCH_AVAILABLE(TYPE, ALIGN, NAME, FLAGS) goto unpack_##NAME
#define OPAL_DATATYPE_UNPACK_DISPATCH_NOT_AVAILABLE(NAME, FLAGS) goto unpack_predefined

    switch (pElem->elem.common.type) {
    case OPAL_DATATYPE_INT1:
    case OPAL_DATATYPE_UINT1:
        OPAL_DATATYPE_HANDLE_UINT1(OPAL_DATATYPE_UNPACK_DISPATCH_AVAILABLE,
                                   OPAL_DATATYPE_UNPACK_DISPATCH_NOT_AVAILABLE, 0);
    case OPAL_DATATYPE_INT2:
    case OPAL_DATATYPE_UINT2:
        OPAL_DATATYPE_HANDLE_UINT2(OPAL_DATATYPE_UNPACK_DISPATCH_AVAILABLE,
                                   OPAL_DATATYPE_UNPACK_DISPATCH_NOT_AVAILABLE, 0);
    case OPAL_DATATYPE_INT4:
    case OPAL_DATATYPE_UINT4:
        OPAL_DATATYPE_HANDLE_UINT4(OPAL_DATATYPE_UNPACK_DISPATCH_AVAILABLE,
                                   OPAL_DATATYPE_UNPACK_DISPATCH_NOT_AVAILABLE, 0);
    case OPAL_DATATYPE_INT8:
    case OPAL_DATATYPE_UINT8:
        OPAL_DATATYPE_HANDLE_UINT8(OPAL_DATATYPE_UNPACK_DISPATCH_AVAILABLE,
                                   OPAL_DATATYPE_UNPACK_DISPATCH_NOT_AVAILABLE, 0);
    case OPAL_DATATYPE_INT16:
    case OPAL_DATATYPE_UINT16:
        OPAL_DATATYPE_HANDLE_UINT16(OPAL_DATATYPE_UNPACK_DISPATCH_AVAILABLE,
                                    OPAL_DATATYPE_UNPACK_DISPATCH_NOT_AVAILABLE, 0);
    case OPAL_DATATYPE_FLOAT2:
        OPAL_DATATYPE_HANDLE_FLOAT2(OPAL_DATATYPE_UNPACK_DISPATCH_AVAILABLE,
                                    OPAL_DATATYPE_UNPACK_DISPATCH_NOT_AVAILABLE, 0);
    case OPAL_DATATYPE_FLOAT4:
        OPAL_DATATYPE_HANDLE_FLOAT4(OPAL_DATATYPE_UNPACK_DISPATCH_AVAILABLE,
                                    OPAL_DATATYPE_UNPACK_DISPATCH_NOT_AVAILABLE, 0);
    case OPAL_DATATYPE_FLOAT8:
        OPAL_DATATYPE_HANDLE_FLOAT8(OPAL_DATATYPE_UNPACK_DISPATCH_AVAILABLE,
                                    OPAL_DATATYPE_UNPACK_DISPATCH_NOT_AVAILABLE, 0);
    case OPAL_DATATYPE_FLOAT16:
        OPAL_DATATYPE_HANDLE_FLOAT16(OPAL_DATATYPE_UNPACK_DISPATCH_AVAILABLE,
                                     OPAL_DATATYPE_UNPACK_DISPATCH_NOT_AVAILABLE, 0);
    case OPAL_DATATYPE_SHORT_FLOAT_COMPLEX:
        OPAL_DATATYPE_HANDLE_SHORT_FLOAT_COMPLEX(OPAL_DATATYPE_UNPACK_DISPATCH_AVAILABLE,
                                                 OPAL_DATATYPE_UNPACK_DISPATCH_NOT_AVAILABLE, 0);
    case OPAL_DATATYPE_FLOAT_COMPLEX:
        OPAL_DATATYPE_HANDLE_FLOAT_COMPLEX(OPAL_DATATYPE_UNPACK_DISPATCH_AVAILABLE,
                                           OPAL_DATATYPE_UNPACK_DISPATCH_NOT_AVAILABLE, 0);
    case OPAL_DATATYPE_DOUBLE_COMPLEX:
        OPAL_DATATYPE_HANDLE_DOUBLE_COMPLEX(OPAL_DATATYPE_UNPACK_DISPATCH_AVAILABLE,
                                            OPAL_DATATYPE_UNPACK_DISPATCH_NOT_AVAILABLE, 0);
    case OPAL_DATATYPE_LONG_DOUBLE_COMPLEX:
        OPAL_DATATYPE_HANDLE_LONG_DOUBLE_COMPLEX(OPAL_DATATYPE_UNPACK_DISPATCH_AVAILABLE,
                                                 OPAL_DATATYPE_UNPACK_DISPATCH_NOT_AVAILABLE, 0);
    case OPAL_DATATYPE_BOOL:
        OPAL_DATATYPE_HANDLE_BOOL(OPAL_DATATYPE_UNPACK_DISPATCH_AVAILABLE,
                                  OPAL_DATATYPE_UNPACK_DISPATCH_NOT_AVAILABLE, 0);
    case OPAL_DATATYPE_WCHAR:
        OPAL_DATATYPE_HANDLE_WCHAR(OPAL_DATATYPE_UNPACK_DISPATCH_AVAILABLE,
                                   OPAL_DATATYPE_UNPACK_DISPATCH_NOT_AVAILABLE, 0);
    case OPAL_DATATYPE_FLOAT12:
    case OPAL_DATATYPE_FLOAT128_COMPLEX:
    default:
        goto unpack_predefined;
    }

#undef OPAL_DATATYPE_UNPACK_DISPATCH_NOT_AVAILABLE
#undef OPAL_DATATYPE_UNPACK_DISPATCH_AVAILABLE

    /* Generate one typed scatter mover and same-type continuation per available predefined type. */
#define OPAL_DATATYPE_UNPACK_TYPE_AVAILABLE(TYPE, ALIGN, NAME, ALTERNATE_TYPE)                    \
    unpack_##NAME : {                                                                             \
        const ddt_elem_desc_t *elem = &pElem->elem;                                               \
        size_t blocklen = elem->blocklen;                                                         \
        size_t cando_count = count_desc;                                                          \
        unsigned char *src = iov_ptr;                                                             \
        unsigned char *dest = conv_ptr + elem->disp;                                              \
        uintptr_t alignment_mask = (uintptr_t) (ALIGN) - 1;                                       \
        if (cando_count > iov_len_local / sizeof(TYPE)) {                                         \
            cando_count = iov_len_local / sizeof(TYPE);                                           \
        }                                                                                         \
        if ((0 != (((uintptr_t) src | (uintptr_t) dest) & alignment_mask))                        \
            || ((0 != ((uintptr_t) elem->extent & alignment_mask))                                \
                && (cando_count > blocklen))) {                                                   \
            goto unpack_predefined;                                                               \
        }                                                                                         \
        if (OPAL_DATATYPE_PREDEFINED_MAX_INLINE_BLOCKLEN >= blocklen) {                           \
            OPAL_DATATYPE_UNPACK_PREDEFINED_ELEMENT_UNCHECKED(src, dest, cando_count, blocklen,   \
                                                               TYPE);                             \
        } else {                                                                                  \
            const size_t block_bytes = blocklen * sizeof(TYPE);                                   \
            const ptrdiff_t block_gap = elem->extent - (ptrdiff_t) block_bytes;                   \
            size_t remaining = cando_count;                                                       \
            if ((opal_datatype_config.unpack.max_vectorized_block_bytes < block_bytes)           \
                || (cando_count < (blocklen << 1))) {                                             \
                goto unpack_predefined;                                                           \
            }                                                                                     \
            /* Compact medium scatters use the callback in the measured middle-length region.     \
             * block_gap is signed (a negative-stride element yields a negative gap), so compare  \
             * it against the threshold as ptrdiff_t rather than letting it wrap to size_t. */    \
            if ((opal_datatype_config.unpack.always_typed_block_bytes < block_bytes)             \
                && (block_bytes <= opal_datatype_config.unpack.compact_memcpy_max_bytes)         \
                && (block_gap < (ptrdiff_t) opal_datatype_config.unpack.min_scatter_gap_bytes)   \
                && (opal_datatype_config.unpack.small_fragment_bytes < iov_len_local)            \
                && (iov_len_local < opal_datatype_config.unpack.large_fragment_bytes)) {         \
                goto unpack_predefined;                                                           \
            }                                                                                     \
            TYPE *source = (TYPE *) (void *) src;                                                 \
            TYPE *destination = (TYPE *) (void *) dest;                                           \
            while (blocklen <= remaining) {                                                       \
                OPAL_DATATYPE_SAFEGUARD_POINTER((unsigned char *) destination,                    \
                                                blocklen * sizeof(TYPE), pConvertor->pBaseBuf,    \
                                                pConvertor->pDesc, pConvertor->count);            \
                for (size_t index = 0; index < blocklen; index++) {                               \
                    destination[index] = source[index];                                           \
                }                                                                                 \
                source += blocklen;                                                               \
                destination = (TYPE *) ((unsigned char *) destination + elem->extent);            \
                remaining -= blocklen;                                                            \
            }                                                                                     \
            if (0 != remaining) {                                                                 \
                OPAL_DATATYPE_SAFEGUARD_POINTER((unsigned char *) destination,                    \
                                                remaining * sizeof(TYPE), pConvertor->pBaseBuf,   \
                                                pConvertor->pDesc, pConvertor->count);            \
                for (size_t index = 0; index < remaining; index++) {                              \
                    destination[index] = source[index];                                           \
                }                                                                                 \
                source += remaining;                                                              \
                destination += remaining;                                                         \
            }                                                                                     \
            src = (unsigned char *) source;                                                       \
            dest = (unsigned char *) destination;                                                 \
        }                                                                                         \
        count_desc -= cando_count;                                                                \
        iov_len_local -= cando_count * sizeof(TYPE);                                              \
        iov_ptr = src;                                                                            \
        conv_ptr = dest - elem->disp;                                                             \
        goto unpack_##NAME##_complete;                                                            \
    }                                                                                             \
    unpack_##NAME##_complete :                                                                    \
        if (0 != count_desc) {                                                                    \
            goto complete_loop;                                                                   \
        }                                                                                         \
        conv_ptr = pConvertor->pBaseBuf + pStack->disp;                                           \
        pos_desc++;                                                                               \
        /* Fall through to this type's next-element dispatch. */                                  \
        UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc, process_loop,          \
                                 process_end_loop);                                               \
        if ((OPAL_DATATYPE_##NAME == pElem->elem.common.type)                                     \
            || ((ALTERNATE_TYPE) == pElem->elem.common.type)) {                                   \
            goto unpack_##NAME;                                                                   \
        }                                                                                         \
        goto process_data;

#define OPAL_DATATYPE_UNPACK_TYPE_NOT_AVAILABLE(NAME, ALTERNATE_TYPE)

    OPAL_DATATYPE_HANDLE_UINT1(OPAL_DATATYPE_UNPACK_TYPE_AVAILABLE,
                               OPAL_DATATYPE_UNPACK_TYPE_NOT_AVAILABLE, OPAL_DATATYPE_INT1);
    OPAL_DATATYPE_HANDLE_UINT2(OPAL_DATATYPE_UNPACK_TYPE_AVAILABLE,
                               OPAL_DATATYPE_UNPACK_TYPE_NOT_AVAILABLE, OPAL_DATATYPE_INT2);
    OPAL_DATATYPE_HANDLE_UINT4(OPAL_DATATYPE_UNPACK_TYPE_AVAILABLE,
                               OPAL_DATATYPE_UNPACK_TYPE_NOT_AVAILABLE, OPAL_DATATYPE_INT4);
    OPAL_DATATYPE_HANDLE_UINT8(OPAL_DATATYPE_UNPACK_TYPE_AVAILABLE,
                               OPAL_DATATYPE_UNPACK_TYPE_NOT_AVAILABLE, OPAL_DATATYPE_INT8);
    OPAL_DATATYPE_HANDLE_UINT16(OPAL_DATATYPE_UNPACK_TYPE_AVAILABLE,
                                OPAL_DATATYPE_UNPACK_TYPE_NOT_AVAILABLE, OPAL_DATATYPE_INT16);
    OPAL_DATATYPE_HANDLE_FLOAT2(OPAL_DATATYPE_UNPACK_TYPE_AVAILABLE,
                                OPAL_DATATYPE_UNPACK_TYPE_NOT_AVAILABLE,
                                OPAL_DATATYPE_MAX_PREDEFINED);
    OPAL_DATATYPE_HANDLE_FLOAT4(OPAL_DATATYPE_UNPACK_TYPE_AVAILABLE,
                                OPAL_DATATYPE_UNPACK_TYPE_NOT_AVAILABLE,
                                OPAL_DATATYPE_MAX_PREDEFINED);
    OPAL_DATATYPE_HANDLE_FLOAT8(OPAL_DATATYPE_UNPACK_TYPE_AVAILABLE,
                                OPAL_DATATYPE_UNPACK_TYPE_NOT_AVAILABLE,
                                OPAL_DATATYPE_MAX_PREDEFINED);
    OPAL_DATATYPE_HANDLE_FLOAT16(OPAL_DATATYPE_UNPACK_TYPE_AVAILABLE,
                                 OPAL_DATATYPE_UNPACK_TYPE_NOT_AVAILABLE,
                                 OPAL_DATATYPE_MAX_PREDEFINED);
    OPAL_DATATYPE_HANDLE_SHORT_FLOAT_COMPLEX(OPAL_DATATYPE_UNPACK_TYPE_AVAILABLE,
                                             OPAL_DATATYPE_UNPACK_TYPE_NOT_AVAILABLE,
                                             OPAL_DATATYPE_MAX_PREDEFINED);
    OPAL_DATATYPE_HANDLE_FLOAT_COMPLEX(OPAL_DATATYPE_UNPACK_TYPE_AVAILABLE,
                                       OPAL_DATATYPE_UNPACK_TYPE_NOT_AVAILABLE,
                                       OPAL_DATATYPE_MAX_PREDEFINED);
    OPAL_DATATYPE_HANDLE_DOUBLE_COMPLEX(OPAL_DATATYPE_UNPACK_TYPE_AVAILABLE,
                                        OPAL_DATATYPE_UNPACK_TYPE_NOT_AVAILABLE,
                                        OPAL_DATATYPE_MAX_PREDEFINED);
    OPAL_DATATYPE_HANDLE_LONG_DOUBLE_COMPLEX(OPAL_DATATYPE_UNPACK_TYPE_AVAILABLE,
                                             OPAL_DATATYPE_UNPACK_TYPE_NOT_AVAILABLE,
                                             OPAL_DATATYPE_MAX_PREDEFINED);
    OPAL_DATATYPE_HANDLE_BOOL(OPAL_DATATYPE_UNPACK_TYPE_AVAILABLE,
                              OPAL_DATATYPE_UNPACK_TYPE_NOT_AVAILABLE,
                              OPAL_DATATYPE_MAX_PREDEFINED);
    OPAL_DATATYPE_HANDLE_WCHAR(OPAL_DATATYPE_UNPACK_TYPE_AVAILABLE,
                               OPAL_DATATYPE_UNPACK_TYPE_NOT_AVAILABLE,
                               OPAL_DATATYPE_MAX_PREDEFINED);

#undef OPAL_DATATYPE_UNPACK_TYPE_NOT_AVAILABLE
#undef OPAL_DATATYPE_UNPACK_TYPE_AVAILABLE

unpack_predefined:
    UNPACK_PREDEFINED_DATATYPE(pConvertor, pElem, count_desc, iov_ptr, conv_ptr, iov_len_local);
    goto process_data_complete;
}

/*
 *  Remember that the first item in the stack (ie. position 0) is the number
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
/*
 * Heterogeneous counterpart of unpack_partial_blocklen(). When a previous fragment stopped in the
 * middle of a repeated block (a whole number of predefined elements, but not a whole block), the
 * block movers below cannot resume: they assume entry on a block boundary and would advance the
 * gappy destination by the full extent from a mid-block position. Convert just the elements left in
 * the current block (COUNT % blocklen) through the same per-type conversion function used for full
 * blocks, then skip the inter-block gap once the block is complete, so
 * unpack_predefined_heterogeneous always sees a block-aligned COUNT. Only whole predefined elements
 * are moved -- a basic type is never split (that case is handled by opal_unpack_partial_predefined).
 *
 * Return 1 if we are now aligned on a block, 0 otherwise.
 */
static inline int
unpack_partial_blocklen_heterogeneous(opal_convertor_t *CONVERTOR,
                                      const dt_elem_desc_t *ELEM, size_t *COUNT,
                                      unsigned char **packed,
                                      unsigned char **memory, size_t *SPACE)
{
    const opal_convertor_master_t *master = (CONVERTOR)->master;
    const ddt_elem_desc_t *_elem = &((ELEM)->elem);
    size_t local_elem_size = opal_datatype_basicDatatypes[_elem->common.type]->size;
    size_t remote_elem_size = master->remote_sizes[_elem->common.type];
    unsigned char *_memory = (*memory) + _elem->disp;
    unsigned char *_packed = *packed;
    char *from, *to;
    size_t do_now;

    assert(*(COUNT) <= ((size_t) _elem->count * _elem->blocklen));

    /* How many predefined elements remain in the current (partial) block? */
    if (0 == (do_now = (*COUNT) % _elem->blocklen)) {
        return 1; /* already aligned on a block boundary */
    }
    size_t left_in_block = do_now;

    /* Never split a predefined element: cap to the whole elements available in the source. */
    if ((remote_elem_size * do_now) > *(SPACE)) {
        do_now = (*SPACE) / remote_elem_size;
    }

    from = (char *) _packed;
    to = (char *) _memory;
    /* do_now < blocklen, so the conversion runs its contiguous "leftover" path (no extent jump).
     * Trust the mover's returned element count rather than the requested do_now: the pointer and
     * SPACE bookkeeping below already follow the mover's actual advance (from/to), so COUNT and the
     * block-completion test must too. They agree today (do_now is pre-capped to fit SPACE), but
     * keying off the return keeps the three in lockstep if a mover ever converts fewer elements. */
    size_t copied = master->pFunctions[_elem->common.type](CONVERTOR, do_now, _elem->blocklen,
                                                           _elem->count, &from, *SPACE,
                                                           _elem->blocklen * remote_elem_size, &to,
                                                           *SPACE, _elem->extent);
    *(COUNT) -= copied;
    _packed = (unsigned char *) from;
    _memory = (unsigned char *) to;
    if (copied == left_in_block) { /* compensate if we completed a blocklen */
        _memory += _elem->extent - (ptrdiff_t) (_elem->blocklen * local_elem_size);
    }

    *(memory) = _memory - _elem->disp;
    *(SPACE) -= (_packed - *packed);
    *(packed) = _packed;
    return (copied == left_in_block);
}

static inline void
unpack_predefined_heterogeneous(opal_convertor_t *CONVERTOR,
                                const dt_elem_desc_t *ELEM, size_t *COUNT,
                                unsigned char **memory,
                                unsigned char **packed, size_t *SPACE)
{
    const opal_convertor_master_t *master = (CONVERTOR)->master;
    const ddt_elem_desc_t *_elem = &((ELEM)->elem);
    size_t cando_count = *(COUNT);
    size_t remote_elem_size = master->remote_sizes[_elem->common.type];
    unsigned char *_memory = (*memory) + _elem->disp;
    unsigned char *_packed = *packed;
    char *from, *to;
    size_t copied;

    assert(0 == (cando_count % _elem->blocklen)); /* no partials here */
    assert(*(COUNT) <= ((size_t) _elem->count * _elem->blocklen));

    /* Cap to whole predefined elements (single-element size, not the block size). */
    if ((remote_elem_size * cando_count) > *(SPACE))
        cando_count = (*SPACE) / remote_elem_size;

    from = (char *) _packed;
    to = (char *) _memory;
    copied = master->pFunctions[_elem->common.type](
        CONVERTOR, cando_count, _elem->blocklen, _elem->count, &from, *SPACE,
        _elem->blocklen * remote_elem_size, &to, *SPACE, _elem->extent);
    *(COUNT) -= copied;
    _packed = (unsigned char *) from;
    _memory = (unsigned char *) to;

    *(memory) = _memory - _elem->disp;
    *(SPACE) -= (_packed - *packed);
    *(packed) = _packed;
}

int32_t opal_unpack_general(opal_convertor_t *pConvertor, struct iovec *iov,
                            uint32_t *out_size, size_t *max_data)
{
    dt_stack_t *pStack; /* pointer to the position on the stack */
    uint32_t pos_desc;  /* actual position in the description of the derived datatype */
    size_t count_desc;  /* the number of items already done in the actual pos_desc */
    size_t total_unpacked = 0;                    /* total size unpacked this time */
    dt_elem_desc_t *description;
    dt_elem_desc_t *pElem;
    const opal_datatype_t *pData = pConvertor->pDesc;
    unsigned char *conv_ptr, *iov_ptr;
    uint32_t iov_count;
    size_t iov_len_local;

    DO_DEBUG(opal_output(0, "opal_convertor_general_unpack( %p, {%p, %lu}, %d )\n",
                         (void *) pConvertor, (void *) iov[0].iov_base,
                         (unsigned long) iov[0].iov_len, *out_size););

    description = pConvertor->use_desc->desc;

    /* For the first step we have to add both displacement to the source. After in the
     * main while loop we will set back the source_base to the correct value. This is
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
                         "unpack start pos_desc %d count_desc %" PRIsize_t " disp %ld\n"
                         "stack_pos %d pos_desc %d count_desc %" PRIsize_t " disp %ld\n",
                         pos_desc, count_desc, (long) (conv_ptr - pConvertor->pBaseBuf),
                         pConvertor->stack_pos, pStack->index, pStack->count, (long) (pStack->disp)););

    for (iov_count = 0; iov_count < (*out_size); iov_count++) {
        iov_ptr = (unsigned char *) iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;

        /* Complete an element split by the preceding input fragment before converting more data. */
        if (0 != pConvertor->partial_length) {
            assert(pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA);
            opal_unpack_partial_predefined(pConvertor, pElem, &count_desc, &iov_ptr, &conv_ptr,
                                           &iov_len_local);
            if (0 == count_desc) {
                assert(0 == pConvertor->partial_length);
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                pos_desc++;
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc, process_loop,
                                         process_end_loop);
                goto process_data;
            }
            if (0 == iov_len_local) {
                goto complete_loop;
            }
        }

        /* Re-align a block split by the preceding input fragment before converting more data. The
         * block movers below require entry on a block boundary. */
        if (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            if (((size_t) pElem->elem.count * pElem->elem.blocklen) != count_desc) {
                int rc = unpack_partial_blocklen_heterogeneous(pConvertor, pElem, &count_desc,
                                                               &iov_ptr, &conv_ptr, &iov_len_local);
                if (0 == rc) { /* still mid-block: the block was not completed */
                    /* Any tail shorter than one predefined element is the leading bytes of the next
                     * element (a basic type split across this fragment boundary). Stash it in the
                     * convertor so the following fragment can finish it, exactly as the main mover
                     * does below; otherwise the fragment is spent on a whole-element boundary and
                     * there is nothing to keep. Without this the heterogeneous complete_loop would
                     * silently drop the partial element. */
                    if (0 != iov_len_local) {
                        assert(iov_len_local
                               < opal_datatype_basicDatatypes[pElem->elem.common.type]->size);
                        opal_unpack_partial_predefined(pConvertor, pElem, &count_desc, &iov_ptr,
                                                       &conv_ptr, &iov_len_local);
                        assert(0 == iov_len_local);
                    }
                    goto complete_loop;
                }
                if (0 == count_desc) {
                    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                    pos_desc++;
                    UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc, process_loop,
                                             process_end_loop);
                    goto process_data;
                }
            }
        }
        while (1) {
            while (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            process_data:
                /* now here we have a basic datatype */
                OPAL_DATATYPE_SAFEGUARD_POINTER(conv_ptr + pElem->elem.disp, pData->size,
                                                pConvertor->pBaseBuf, pData, pConvertor->count);
                DO_DEBUG(opal_output(0, "unpack (%p, %ld) -> (%p:%ld, %" PRIsize_t ", %ld) type %s\n",
                                     (void *) iov_ptr, iov_len_local, (void *) pConvertor->pBaseBuf,
                                     conv_ptr + pElem->elem.disp - pConvertor->pBaseBuf, count_desc,
                                     pElem->elem.extent,
                                     opal_datatype_basicDatatypes[pElem->elem.common.type]->name););
                unpack_predefined_heterogeneous(pConvertor, pElem, &count_desc, &conv_ptr, &iov_ptr,
                                                &iov_len_local);
                if (0 == count_desc) {    /* completed */
                    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                    pos_desc++; /* advance to the next data */
                    /* Refresh pElem/count_desc for the new pos_desc before any escape: do not
                     * shortcut to complete_loop on a spent iovec (0 == iov_len_local) while pElem
                     * still names the finished entry and count_desc is 0. Doing so would save a
                     * stale (pos_desc, count_desc==0) resume state and, if another iovec follows,
                     * re-enter the DATA loop on the old element and increment pos_desc a second
                     * time, skipping an entry. When iov_len_local is 0 the refreshed DATA entry
                     * simply copies nothing at process_data and falls through to complete_loop with
                     * the correct saved state. */
                    UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                             process_loop, process_end_loop);
                    goto process_data;
                }
                assert(pElem->elem.common.type < OPAL_DATATYPE_MAX_PREDEFINED);
                if (0 != iov_len_local) {
                    /* We have some partial data here. Let's copy it into the convertor
                     * and keep it hot until the next round.
                     */
                    assert(iov_len_local < opal_datatype_basicDatatypes[pElem->elem.common.type]->size);
                    opal_unpack_partial_predefined(pConvertor, pElem, &count_desc, &iov_ptr,
                                                   &conv_ptr, &iov_len_local);
                    assert( 0 == iov_len_local );
                }
                goto complete_loop;
            }
            if (OPAL_DATATYPE_END_LOOP == pElem->elem.common.type) { /* end of the current loop */
            process_end_loop:
                DO_DEBUG(opal_output(0,
                                     "unpack end_loop count %" PRIsize_t " stack_pos %d pos_desc %d disp %ld space %" PRIsize_t "\n",
                                     pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp, iov_len_local););
                if (--(pStack->count) == 0) { /* end of loop */
                    if (0 == pConvertor->stack_pos) {
                        /* we're done. Force the exit of the main for loop (around iovec) */
                        *out_size = iov_count;
                        goto complete_loop;
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
                                     "unpack new_loop count %" PRIsize_t " stack_pos %d pos_desc %d disp %ld space %" PRIsize_t "\n",
                                     pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp, iov_len_local););
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                         process_loop, process_end_loop);
                goto process_data;
            }
            if (OPAL_DATATYPE_LOOP == pElem->elem.common.type) {
            process_loop:
                PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, OPAL_DATATYPE_LOOP, count_desc,
                           pStack->disp);
                pos_desc++;
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                DDT_DUMP_STACK(pConvertor->pStack, pConvertor->stack_pos,
                               &description[pos_desc], "advance loop");
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc,
                                         process_loop, process_end_loop);
                goto process_data;
            }
        }
    complete_loop:
        iov[iov_count].iov_len -= iov_len_local; /* update the amount of valid data */
        total_unpacked += iov[iov_count].iov_len;
    }
    *max_data = total_unpacked;
    pConvertor->bConverted += total_unpacked; /* update the already converted bytes */
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
    DO_DEBUG(opal_output(0, "unpack save stack stack_pos %d pos_desc %d count_desc %" PRIsize_t " disp %ld\n",
                         pConvertor->stack_pos, pStack->index, pStack->count, (long) pStack->disp););
    return 0;
}
