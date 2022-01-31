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
 * Copyright (c) 2011      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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

#if OPAL_ENABLE_DEBUG
#    include "opal/util/output.h"

#    define DO_DEBUG(INST)           \
        if (opal_ddt_unpack_debug) { \
            INST                     \
        }
#else
#    define DO_DEBUG(INST)
#endif /* OPAL_ENABLE_DEBUG */

#include "opal/datatype/opal_datatype_checksum.h"
#include "opal/datatype/opal_datatype_prototypes.h"
#include "opal/datatype/opal_datatype_unpack.h"

#if defined(CHECKSUM)
#    define opal_unpack_general_function            opal_unpack_general_checksum
#    define opal_unpack_homogeneous_contig_function opal_unpack_homogeneous_contig_checksum
#    define opal_generic_simple_unpack_function     opal_generic_simple_unpack_checksum
#else
#    define opal_unpack_general_function            opal_unpack_general
#    define opal_unpack_homogeneous_contig_function opal_unpack_homogeneous_contig
#    define opal_generic_simple_unpack_function     opal_generic_simple_unpack
#endif /* defined(CHECKSUM) */

/**
 * This function will be used to unpack all datatypes that have the contiguous flag set.
 * Several types of datatypes match this criterion, not only the contiguous one, but
 * the ones that have gaps in the beginning and/or at the end but where the data to
 * be unpacked is contiguous. However, this function only work for homogeneous cases
 * and the datatype that are contiguous and where the extent is equal to the size are
 * taken in account directly in the opal_convertor_unpack function (in convertor.c) for
 * the homogeneous case.
 */
int32_t opal_unpack_homogeneous_contig_function(opal_convertor_t *pConv, struct iovec *iov,
                                                uint32_t *out_size, size_t *max_data)
{
    const opal_datatype_t *pData = pConv->pDesc;
    unsigned char *user_memory, *packed_buffer;
    uint32_t iov_idx, i;
    size_t remaining, initial_bytes_converted = pConv->bConverted;
    dt_stack_t *stack = pConv->pStack;
    ptrdiff_t extent = pData->ub - pData->lb;

    DO_DEBUG(opal_output(0, "unpack_homogeneous_contig( pBaseBuf %p, iov count %d )\n",
                         (void *) pConv->pBaseBuf, *out_size););
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
            MEMCPY_CSUM(user_memory, packed_buffer, remaining, pConv);
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
                MEMCPY_CSUM(user_memory, packed_buffer, stack[1].count, pConv);

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
                MEMCPY_CSUM(user_memory, packed_buffer, remaining, pConv);
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

/**
 * This function handle partial types. Depending on the send operation it might happens
 * that we receive only a partial type (always predefined type). In fact the outcome is
 * that the unpack has to be done in 2 steps. As there is no way to know if the other
 * part of the datatype is already received, we need to use a trick to handle this special
 * case. The trick is to fill the missing part with some well known value, unpack the data
 * as if it was completely received, and then move into the user memory only the bytes
 * that don't match the well known value. This approach work as long as there is no need
 * for more than structural changes. They will not work for cases where we will have to
 * change the content of the data (as in all conversions that require changing the size
 * of the exponent or mantissa).
 */
static inline void
opal_unpack_partial_predefined(opal_convertor_t *pConvertor, const dt_elem_desc_t *pElem,
                               size_t *COUNT, unsigned char **packed,
                               unsigned char **memory, size_t *SPACE)
{
    char unused_byte = 0x7F, saved_data[16];
    unsigned char temporary[16], *temporary_buffer = temporary;
    unsigned char *user_data = *memory + pElem->elem.disp;
    size_t data_length = opal_datatype_basicDatatypes[pElem->elem.common.type]->size;
    unsigned char *partial_data = *packed;
    ptrdiff_t start_position = pConvertor->partial_length;
    size_t length = data_length - start_position;
    size_t count_desc = 1;
    dt_elem_desc_t single_elem = { .elem = { .common = pElem->elem.common, .count = 1, .blocklen = 1,
                                   .extent = data_length,  /* advance by a full data element */
                                   .disp = 0  /* right where the pointer is */ } };
    if( *SPACE < length ) {
        length = *SPACE;
    }

    DO_DEBUG( opal_output( 0, "unpack partial data start %lu end %lu data_length %lu user %p\n"
                           "\tbConverted %lu total_length %lu count %ld\n",
                           (unsigned long)start_position, (unsigned long)start_position + length,
                           (unsigned long)data_length, (void*)*memory,
                           (unsigned long)pConvertor->bConverted,
                           (unsigned long)pConvertor->local_size, pConvertor->count ); );
    COMPUTE_CSUM( partial_data, length, pConvertor );

    /* Find a byte value that is not used in the partial buffer. We use it as a marker
     * to identify what has not been modified by the unpack call. */
 find_unused_byte:
    for (size_t i = 0; i < length; i++ ) {
        if( unused_byte == partial_data[i] ) {
            unused_byte--;
            goto find_unused_byte;
        }
    }

    /* Prepare an full element of the predefined type, by populating an entire type
     * with the unused byte and then put the partial data at the right position. */
    memset( temporary, unused_byte, data_length );
    MEMCPY( temporary + start_position, partial_data, length );

    /* Save the original content of the user memory */
#if OPAL_CUDA_SUPPORT
    /* In the case where the data is being unpacked from device memory, need to
     * use the special host to device memory copy. */
    pConvertor->cbmemcpy(saved_data, user_data, data_length, pConvertor );
#else
    MEMCPY( saved_data, user_data, data_length );
#endif

    /* Then unpack the data into the user memory */
    UNPACK_PREDEFINED_DATATYPE(pConvertor, &single_elem, count_desc, temporary_buffer, user_data,
                               data_length);

    /* reload the length and user buffer as they have been updated by the macro */
    data_length = opal_datatype_basicDatatypes[pElem->elem.common.type]->size;
    user_data = *memory + pElem->elem.disp;

    /* Rebuild the data by pulling back the unmodified bytes from the original
     * content in the user memory. */
#if OPAL_CUDA_SUPPORT
    /* Need to copy the modified user_data again so we can see which
     * bytes need to be converted back to their original values. */
    {
        char resaved_data[16];
        pConvertor->cbmemcpy(resaved_data, user_data, data_length, pConvertor);
        for (size_t i = 0; i < data_length; i++) {
            if (unused_byte == resaved_data[i])
                pConvertor->cbmemcpy(&user_data[i], &saved_data[i], 1, pConvertor);
        }
    }
#else
    for (size_t i = 0; i < data_length; i++) {
        if (unused_byte == user_data[i]) {
            user_data[i] = saved_data[i];
        }
    }
#endif
    pConvertor->partial_length = (pConvertor->partial_length + length) % data_length;
    *SPACE  -= length;
    *packed += length;
    if (0 == pConvertor->partial_length) {
        (*COUNT)--;  /* we have enough to complete one full predefined type */
        *memory += data_length;
        if (0 == (*COUNT % pElem->elem.blocklen)) {
            *memory += pElem->elem.extent - (pElem->elem.blocklen * data_length);
        }
    }
}

/* The pack/unpack functions need a cleanup. I have to create a proper interface to access
 * all basic functionalities, hence using them as basic blocks for all conversion functions.
 *
 * But first let's make some global assumptions:
 * - a datatype (with the flag DT_DATA set) will have the contiguous flags set if and only if
 *   the data is really contiguous (extent equal with size)
 * - for the OPAL_DATATYPE_LOOP type the DT_CONTIGUOUS flag set means that the content of the loop
 * is contiguous but with a gap in the begining or at the end.
 * - the DT_CONTIGUOUS flag for the type OPAL_DATATYPE_END_LOOP is meaningless.
 */
int32_t opal_generic_simple_unpack_function(opal_convertor_t *pConvertor, struct iovec *iov,
                                            uint32_t *out_size, size_t *max_data)
{
    dt_stack_t *pStack;        /* pointer to the position on the stack */
    uint32_t pos_desc;         /* actual position in the description of the derived datatype */
    size_t count_desc;         /* the number of items already done in the actual pos_desc */
    size_t total_unpacked = 0; /* total size unpacked this time */
    dt_elem_desc_t *description;
    dt_elem_desc_t *pElem;
    const opal_datatype_t *pData = pConvertor->pDesc;
    unsigned char *conv_ptr, *iov_ptr;
    size_t iov_len_local;
    uint32_t iov_count;

    DO_DEBUG( opal_output( 0, "opal_convertor_generic_simple_unpack( %p, iov[%u] = {%p, %lu} )\n",
                           (void*)pConvertor, *out_size, (void*)iov[0].iov_base, (unsigned long)iov[0].iov_len ); );

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
                         pConvertor->stack_pos, pStack->index, pStack->count,
                         (long) (pStack->disp)););

    for (iov_count = 0; iov_count < (*out_size); iov_count++) {
        iov_ptr = (unsigned char *) iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;

        /* Deal with all types of partial predefined datatype unpacking, including when
         * unpacking a partial predefined element and when unpacking a part smaller than
         * the blocklen.
         */
        if (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            if (0 != pConvertor->partial_length) {  /* partial predefined element */
                assert( pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA );
                opal_unpack_partial_predefined( pConvertor, pElem, &count_desc,
                                                &iov_ptr, &conv_ptr, &iov_len_local );
                if (0 == count_desc) {  /* the end of the vector ? */
                    assert( 0 == pConvertor->partial_length );
                    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                    pos_desc++; /* advance to the next data */
                    UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc);
                    goto next_vector;
                }
                if( 0 == iov_len_local )
                    goto complete_loop;
            }
            if (((size_t) pElem->elem.count * pElem->elem.blocklen) != count_desc) {
                /* we have a partial (less than blocklen) basic datatype */
                int rc = UNPACK_PARTIAL_BLOCKLEN(pConvertor, pElem, count_desc, iov_ptr, conv_ptr,
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

        while (1) {
          next_vector:
            while (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
                /* we have a basic datatype (working on full blocks) */
                UNPACK_PREDEFINED_DATATYPE(pConvertor, pElem, count_desc, iov_ptr, conv_ptr,
                                           iov_len_local);
                if (0 != count_desc) { /* completed? */
                    goto complete_loop;
                }
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                pos_desc++; /* advance to the next data */
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc);
            }
            if (OPAL_DATATYPE_END_LOOP == pElem->elem.common.type) { /* end of the current loop */
                DO_DEBUG(opal_output(0,
                                     "unpack end_loop count %" PRIsize_t
                                     " stack_pos %d pos_desc %d disp %ld space %lu\n",
                                     pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp,
                                     (unsigned long) iov_len_local););
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
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc);
                DO_DEBUG(opal_output(0,
                                     "unpack new_loop count %" PRIsize_t
                                     " stack_pos %d pos_desc %d disp %ld space %lu\n",
                                     pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp,
                                     (unsigned long) iov_len_local););
            }
            if (OPAL_DATATYPE_LOOP == pElem->elem.common.type) {
                ptrdiff_t local_disp = (ptrdiff_t) conv_ptr;
                if (pElem->loop.common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS) {
                    UNPACK_CONTIGUOUS_LOOP(pConvertor, pElem, count_desc, iov_ptr, conv_ptr,
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
            update_loop_description: /* update the current state */
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc);
                DDT_DUMP_STACK(pConvertor->pStack, pConvertor->stack_pos, pElem, "advance loop");
            }
        }
    complete_loop:
        assert( pElem->elem.common.type < OPAL_DATATYPE_MAX_PREDEFINED );
        if( (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) && (0 != iov_len_local) ) {
            unsigned char* temp = conv_ptr;
            /* We have some partial data here. Let's copy it into the convertor
             * and keep it hot until the next round.
             */
            assert( iov_len_local < opal_datatype_basicDatatypes[pElem->elem.common.type]->size );
            opal_unpack_partial_predefined(pConvertor, pElem, &count_desc, &iov_ptr, &temp, &iov_len_local);
        }

        iov[iov_count].iov_len -= iov_len_local; /* update the amount of valid data */
        total_unpacked += iov[iov_count].iov_len;
    }
    *max_data = total_unpacked;
    pConvertor->bConverted += total_unpacked; /* update the already converted bytes */
    *out_size = iov_count;
    if (pConvertor->bConverted == pConvertor->local_size) {
        pConvertor->flags |= CONVERTOR_COMPLETED;
        return 1;
    }
    /* Save the global position for the next round */
    PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, pElem->elem.common.type, count_desc,
               conv_ptr - pConvertor->pBaseBuf);
    DO_DEBUG(opal_output(0,
                         "unpack save stack stack_pos %d pos_desc %d count_desc %" PRIsize_t
                         " disp %ld\n",
                         pConvertor->stack_pos, pStack->index, pStack->count,
                         (long) pStack->disp););
    return 0;
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
static inline void
unpack_predefined_heterogeneous(opal_convertor_t *CONVERTOR,
                                const dt_elem_desc_t *ELEM, size_t *COUNT,
                                unsigned char **memory,
                                unsigned char **packed, size_t *SPACE)
{
    const opal_convertor_master_t *master = (CONVERTOR)->master;
    const ddt_elem_desc_t *_elem = &((ELEM)->elem);
    size_t cando_count = *(COUNT), do_now_bytes;
    size_t local_elem_size = opal_datatype_basicDatatypes[_elem->common.type]->size;
    size_t remote_elem_size = master->remote_sizes[_elem->common.type];
    size_t blocklen_bytes = remote_elem_size;
    unsigned char *_memory = (*memory) + _elem->disp;
    unsigned char *_packed = *packed;
    ptrdiff_t advance = 0;

    assert(0 == (cando_count % _elem->blocklen)); /* no partials here */
    assert(*(COUNT) <= ((size_t) _elem->count * _elem->blocklen));

    if ((remote_elem_size * cando_count) > *(SPACE))
        cando_count = (*SPACE) / blocklen_bytes;

    /* preemptively update the number of COUNT we will return. */
    *(COUNT) -= cando_count;

    if (_elem->blocklen == 1) {
        master->pFunctions[_elem->common.type](CONVERTOR, cando_count,
                                               _packed, *SPACE, remote_elem_size,
                                               _memory, *SPACE, _elem->extent,
                                               &advance);
        _memory += cando_count * _elem->extent;
        _packed += cando_count * remote_elem_size;
        goto update_and_return;
    }

    if ((1 < _elem->count) && (_elem->blocklen <= cando_count)) {
        blocklen_bytes = remote_elem_size * _elem->blocklen;

        do { /* Do as many full blocklen as possible */
            OPAL_DATATYPE_SAFEGUARD_POINTER(_memory, blocklen_bytes, (CONVERTOR)->pBaseBuf,
                                            (CONVERTOR)->pDesc, (CONVERTOR)->count);
            DO_DEBUG(opal_output(0, "pack 2. memcpy( %p, %p, %lu ) => space %lu\n",
                                 (void *) _packed, (void *) _memory, (unsigned long) blocklen_bytes,
                                 (unsigned long) (*(SPACE) - (_packed - *(packed)))););
            master->pFunctions[_elem->common.type](CONVERTOR, _elem->blocklen,
                                                   _packed, *SPACE, remote_elem_size,
                                                   _memory, *SPACE, local_elem_size,
                                                   &advance);
            _packed += blocklen_bytes;
            _memory += _elem->extent;
            cando_count -= _elem->blocklen;
        } while (_elem->blocklen <= cando_count);
    }

    /**
     * As an epilog do anything left from the last blocklen.
     */
    if (0 != cando_count) {
        assert((cando_count < _elem->blocklen)
               || ((1 == _elem->count) && (cando_count <= _elem->blocklen)));
        do_now_bytes = cando_count * remote_elem_size;
        OPAL_DATATYPE_SAFEGUARD_POINTER(_memory, do_now_bytes, (CONVERTOR)->pBaseBuf,
                                        (CONVERTOR)->pDesc, (CONVERTOR)->count);
        DO_DEBUG(opal_output(0, "pack 3. memcpy( %p, %p, %lu ) => space %lu [epilog]\n",
                             (void *) _packed, (void *) _memory, (unsigned long) do_now_bytes,
                             (unsigned long) (*(SPACE) - (_packed - *(packed)))););
        master->pFunctions[_elem->common.type](CONVERTOR, cando_count,
                                               _packed, *SPACE, remote_elem_size,
                                               _memory, *SPACE, local_elem_size,
                                               &advance);
        _memory += cando_count * local_elem_size;
        _packed += do_now_bytes;
    }

update_and_return:
    *(memory) = _memory - _elem->disp;
    *(SPACE) -= (_packed - *packed);
    *(packed) = _packed;
}

int32_t opal_unpack_general_function(opal_convertor_t *pConvertor, struct iovec *iov,
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
                         pConvertor->stack_pos, pStack->index, pStack->count,
                         (long) (pStack->disp)););

    for (iov_count = 0; iov_count < (*out_size); iov_count++) {
        iov_ptr = (unsigned char *) iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;
        assert(0 == pConvertor->partial_length);
        while (1) {
            while (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
                /* now here we have a basic datatype */
                OPAL_DATATYPE_SAFEGUARD_POINTER(conv_ptr + pElem->elem.disp, pData->size,
                                                pConvertor->pBaseBuf, pData, pConvertor->count);
                DO_DEBUG(opal_output(0,
                                     "unpack (%p, %ld) -> (%p:%ld, %" PRIsize_t ", %ld) type %s\n",
                                     (void *) iov_ptr, iov_len_local, (void *) pConvertor->pBaseBuf,
                                     conv_ptr + pElem->elem.disp - pConvertor->pBaseBuf, count_desc,
                                     description[pos_desc].elem.extent,
                                     opal_datatype_basicDatatypes[description[pos_desc].elem.common.type]->name););
                unpack_predefined_heterogeneous(pConvertor, pElem, &count_desc, &conv_ptr, &iov_ptr,
                                                &iov_len_local);
                if (0 == count_desc) {    /* completed */
                    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                    pos_desc++; /* advance to the next data */
                    UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc);
                    if (0 == iov_len_local) {
                        goto complete_loop; /* escape if we're done */
                    }
                    continue;
                }
                assert(pElem->elem.common.type < OPAL_DATATYPE_MAX_PREDEFINED);
                assert(0 == iov_len_local);
                if (0 != iov_len_local) {
                    unsigned char *temp = conv_ptr;
                    /* We have some partial data here. Let's copy it into the convertor
                     * and keep it hot until the next round.
                     */
                    assert(iov_len_local < opal_datatype_basicDatatypes[pElem->elem.common.type]->size);
                    opal_unpack_partial_predefined(pConvertor, pElem, &count_desc, &iov_ptr,
                                                   &temp, &iov_len_local);
                    assert( 0 == iov_len_local );
                }
                goto complete_loop;
            }
            if (OPAL_DATATYPE_END_LOOP == pElem->elem.common.type) { /* end of the current loop */
                DO_DEBUG(opal_output(0,
                                     "unpack end_loop count %" PRIsize_t
                                     " stack_pos %d pos_desc %d disp %ld space %lu\n",
                                     pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp,
                                     (unsigned long) iov_len_local););
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
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc);
                DO_DEBUG(opal_output(0,
                                     "unpack new_loop count %" PRIsize_t
                                     " stack_pos %d pos_desc %d disp %ld space %lu\n",
                                     pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp,
                                     (unsigned long) iov_len_local););
            }
            if (OPAL_DATATYPE_LOOP == pElem->elem.common.type) {
                PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, OPAL_DATATYPE_LOOP, count_desc,
                           pStack->disp);
                pos_desc++;
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc);
                DDT_DUMP_STACK(pConvertor->pStack, pConvertor->stack_pos, pElem, "advance loop");
                continue;
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
    DO_DEBUG(opal_output(0,
                         "unpack save stack stack_pos %d pos_desc %d count_desc %" PRIsize_t
                         " disp %ld\n",
                         pConvertor->stack_pos, pStack->index, pStack->count,
                         (long) pStack->disp););
    return 0;
}
