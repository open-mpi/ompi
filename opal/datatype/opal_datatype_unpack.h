/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2019 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2026 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2017-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020-2021 IBM Corporation. All rights reserved.
 * Copyright (c) 2022      Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_DATATYPE_UNPACK_H_HAS_BEEN_INCLUDED
#define OPAL_DATATYPE_UNPACK_H_HAS_BEEN_INCLUDED

#include "opal_config.h"
#include "opal/datatype/opal_datatype_pack_unpack_predefined.h"

/**
 * This function deals only with partial elements. The COUNT points however to
 * the whole leftover count, but this function is only expected to operate on
 * an amount less than blength, that would allow the rest of the pack process
 * to handle only entire blength blocks (plus the left over).
 *
 * Return 1 if we are now aligned on a block, 0 otherwise.
 */
static inline int unpack_partial_blocklen(opal_convertor_t *CONVERTOR, const dt_elem_desc_t *ELEM,
                                          size_t *COUNT, unsigned char **packed,
                                          unsigned char **memory, size_t *SPACE)
{
    const ddt_elem_desc_t *_elem = &((ELEM)->elem);
    size_t do_now_bytes = opal_datatype_basicDatatypes[_elem->common.type]->size;
    size_t do_now = (*COUNT);
    unsigned char *_memory = (*memory) + _elem->disp;
    unsigned char *_packed = *packed;

    assert(*(COUNT) <= ((size_t)(_elem->count * _elem->blocklen)));

    if( (*SPACE) < do_now_bytes ) /* Can we do anything ? */
        return 0;
    /**
     * First check if we already did something on this element ? The COUNT is the number
     * of remaining predefined types in the current elem, not how many predefined types
     * should be manipulated in the current call (this number is instead reflected on the
     * SPACE).
     */
    if (0 == (do_now = (*COUNT) % _elem->blocklen))
        return 1;

    size_t left_in_block = do_now; /* left in the current blocklen */

    if ((do_now_bytes * do_now) > *(SPACE))
        do_now = (*SPACE) / do_now_bytes;

    do_now_bytes *= do_now;

    OPAL_DATATYPE_SAFEGUARD_POINTER( _memory, do_now_bytes, (CONVERTOR)->pBaseBuf,
                                     (CONVERTOR)->pDesc, (CONVERTOR)->count );
    DO_DEBUG( opal_output( 0, "unpack memcpy( %p [%ld], %p, %lu ) => space %lu [prolog]\n",
                           (void*)_memory, _memory - CONVERTOR->pBaseBuf,
                           (void*)_packed, (unsigned long)do_now_bytes, (unsigned long)(*(SPACE)) ); );
    (CONVERTOR)->cbmemcpy(_memory, _packed, do_now_bytes, (CONVERTOR));
    *(memory)     += (ptrdiff_t)do_now_bytes;
    if( do_now == left_in_block )  /* compensate if completed a blocklen */
        *(memory) += _elem->extent - (_elem->blocklen * opal_datatype_basicDatatypes[_elem->common.type]->size);

    *(COUNT)  -= do_now;
    *(SPACE)  -= do_now_bytes;
    *(packed) += do_now_bytes;
    return (do_now == left_in_block);
}

static inline void unpack_predefined_data(opal_convertor_t *CONVERTOR, const dt_elem_desc_t *ELEM,
                                          size_t *COUNT, unsigned char **packed,
                                          unsigned char **memory, size_t *SPACE)
{
    const ddt_elem_desc_t *_elem = &((ELEM)->elem);
    size_t blocklen_bytes = opal_datatype_basicDatatypes[_elem->common.type]->size;
    size_t cando_count = (*COUNT), do_now_bytes;
    unsigned char *_memory = (*memory) + _elem->disp;
    unsigned char *_packed = *packed;

    assert(0 == (cando_count % _elem->blocklen)); /* no partials here */
    assert(*(COUNT) <= ((size_t) _elem->count * _elem->blocklen));

    if ((blocklen_bytes * cando_count) > *(SPACE))
        cando_count = (*SPACE) / blocklen_bytes;

    if (_elem->blocklen <= OPAL_DATATYPE_PREDEFINED_MAX_INLINE_BLOCKLEN) {
        if (OPAL_LIKELY(OPAL_SUCCESS
                        == opal_datatype_unpack_predefined_element(&_packed, &_memory,
                                                                   cando_count, _elem))) {
            *(COUNT) -= cando_count;
            goto update_and_return;
        }
        /* else unrecognized _elem->common.type, use the memcpy path */
    }

    /* preemptively update the number of COUNT we will return. */
    *(COUNT) -= cando_count;

    if (1 == _elem->blocklen) {  /* Do as many full blocklen as possible */
        for (; cando_count > 0; cando_count--) {
            OPAL_DATATYPE_SAFEGUARD_POINTER( _memory, blocklen_bytes, (CONVERTOR)->pBaseBuf,
                                             (CONVERTOR)->pDesc, (CONVERTOR)->count );
            DO_DEBUG( opal_output( 0, "unpack memcpy( %p [%ld], %p [%ld], %lu ) => space %lu [blen = 1]\n",
                                   (void*)_memory, _memory - CONVERTOR->pBaseBuf,
                                   (void*)_packed, _packed - *packed,
                                   (unsigned long)blocklen_bytes, (unsigned long)(*(SPACE) - (_packed - *(packed))) ); );
            (CONVERTOR)->cbmemcpy(_memory, _packed, blocklen_bytes, (CONVERTOR));
            _packed     += blocklen_bytes;
            _memory     += _elem->extent;
        }
        goto update_and_return;
    }

    if ((1 < _elem->count) && (_elem->blocklen <= cando_count)) {
        blocklen_bytes *= _elem->blocklen;

        do { /* Do as many full blocklen as possible */
            OPAL_DATATYPE_SAFEGUARD_POINTER( _memory, blocklen_bytes, (CONVERTOR)->pBaseBuf,
                                             (CONVERTOR)->pDesc, (CONVERTOR)->count );
            DO_DEBUG( opal_output( 0, "unpack 2. memcpy( %p [%ld], %p [%ld], %lu ) => space %lu\n",
                                   (void*)_memory, _memory - CONVERTOR->pBaseBuf,
                                   (void*)_packed, _packed - *packed,
                                   (unsigned long)blocklen_bytes, (unsigned long)(*(SPACE) - (_packed - *(packed))) ); );
            (CONVERTOR)->cbmemcpy(_memory, _packed, blocklen_bytes, (CONVERTOR));
            _packed     += blocklen_bytes;
            _memory     += _elem->extent;
            cando_count -= _elem->blocklen;
        } while (_elem->blocklen <= cando_count);
    }

    /**
     * As an epilog do anything left from the last blocklen.
     */
    if (0 != cando_count) {
        assert((cando_count < _elem->blocklen)
               || ((1 == _elem->count) && (cando_count <= _elem->blocklen)));
        do_now_bytes = cando_count * opal_datatype_basicDatatypes[_elem->common.type]->size;
        OPAL_DATATYPE_SAFEGUARD_POINTER( _memory, do_now_bytes, (CONVERTOR)->pBaseBuf,
                                         (CONVERTOR)->pDesc, (CONVERTOR)->count );
        DO_DEBUG( opal_output( 0, "unpack 3. memcpy( %p [%ld], %p [%ld], %lu ) => space %lu [epilog]\n",
                               (void*)_memory, _memory - CONVERTOR->pBaseBuf,
                               (void*)_packed, _packed - *packed,
                               (unsigned long)do_now_bytes, (unsigned long)(*(SPACE) - (_packed - *(packed))) ); );
        (CONVERTOR)->cbmemcpy(_memory, _packed, do_now_bytes, (CONVERTOR));
        _memory   += do_now_bytes;
        _packed   += do_now_bytes;
    }

update_and_return:
    *(memory) = _memory - _elem->disp;
    *(SPACE) -= (_packed - *packed);
    *(packed) = _packed;
}

static inline void unpack_contiguous_loop(opal_convertor_t *CONVERTOR, const dt_elem_desc_t *ELEM,
                                          size_t *COUNT, unsigned char **packed,
                                          unsigned char **memory, size_t *SPACE)
{
    const ddt_loop_desc_t *_loop = (ddt_loop_desc_t *) (ELEM);
    const ddt_endloop_desc_t *_end_loop = (ddt_endloop_desc_t *) ((ELEM) + _loop->items);
    unsigned char *_memory = (*memory) + _end_loop->first_elem_disp;
    size_t _copy_loops = *(COUNT);

    if ((_copy_loops * _end_loop->size) > *(SPACE))
        _copy_loops = (*(SPACE) / _end_loop->size);
    for (size_t _i = 0; _i < _copy_loops; _i++) {
        OPAL_DATATYPE_SAFEGUARD_POINTER(_memory, _end_loop->size, (CONVERTOR)->pBaseBuf,
                                        (CONVERTOR)->pDesc, (CONVERTOR)->count);
        DO_DEBUG(opal_output(0, "unpack 3. memcpy( %p, %p, %lu ) => space %lu\n", (void *) _memory,
                             (void *) *(packed), (unsigned long) _end_loop->size,
                             (unsigned long) (*(SPACE) -_i * _end_loop->size)););
        (CONVERTOR)->cbmemcpy(_memory, *(packed), _end_loop->size, (CONVERTOR));
        *(packed) += _end_loop->size;
        _memory += _loop->extent;
    }
    *(memory) = _memory - _end_loop->first_elem_disp;
    *(SPACE) -= _copy_loops * _end_loop->size;
    *(COUNT) -= _copy_loops;
}

#define UNPACK_PARTIAL_BLOCKLEN(CONVERTOR, /* the convertor */                       \
                                ELEM,      /* the basic element to be packed */      \
                                COUNT,     /* the number of elements */              \
                                PACKED,    /* the destination pointer (char*) */     \
                                MEMORY,    /* the source pointer (char*) */          \
                                SPACE)     /* the space in the destination buffer */ \
    unpack_partial_blocklen((CONVERTOR), (ELEM), &(COUNT), &(PACKED), &(MEMORY), &(SPACE))

#define UNPACK_PREDEFINED_DATATYPE(CONVERTOR, /* the convertor */                       \
                                   ELEM,      /* the basic element to be packed */      \
                                   COUNT,     /* the number of elements */              \
                                   PACKED,    /* the destination pointer (char*) */     \
                                   MEMORY,    /* the source pointer (char*) */          \
                                   SPACE)     /* the space in the destination buffer */ \
    unpack_predefined_data((CONVERTOR), (ELEM), &(COUNT), &(PACKED), &(MEMORY), &(SPACE))

#define UNPACK_CONTIGUOUS_LOOP(CONVERTOR, ELEM, COUNT, PACKED, MEMORY, SPACE) \
    unpack_contiguous_loop((CONVERTOR), (ELEM), &(COUNT), &(PACKED), &(MEMORY), &(SPACE))

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
 *
 * This helper is shared between the production unpacker in opal_datatype_unpack.c and the
 * reference interpreter used by the pack_description_sweep benchmark; both include this
 * header after the datatype, convertor, memcpy, and accelerator base headers that provide
 * the symbols referenced below.
 */
static inline void
opal_unpack_partial_predefined(opal_convertor_t *pConvertor, const dt_elem_desc_t *pElem,
                               size_t *COUNT, unsigned char **packed,
                               unsigned char **memory, size_t *SPACE)
{
    char unused_byte = 0x7F, saved_data[OPAL_DATATYPE_MAX_PREDEFINED_SIZE];
    unsigned char temporary[OPAL_DATATYPE_MAX_PREDEFINED_SIZE], *temporary_buffer = temporary;
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
    /* In the case where the data is being unpacked from device memory, need to
     * use the special host to device memory copy. */
    pConvertor->cbmemcpy(saved_data, user_data, data_length, pConvertor );

    /* Then unpack the data into the user memory. Heterogeneous conversion must use the same
     * predefined-type callback as the general unpacker; the homogeneous mover only copies bytes. */
    if (pConvertor->flags & CONVERTOR_HOMOGENEOUS) {
        UNPACK_PREDEFINED_DATATYPE(pConvertor, &single_elem, count_desc, temporary_buffer, user_data,
                                   data_length);
    } else {
        const size_t remote_length = pConvertor->master->remote_sizes[pElem->elem.common.type];
        char *from = (char *) temporary_buffer;
        char *to = (char *) user_data;
        size_t copied;

        /*
         * A predefined element is only ever split across a fragment boundary when the packer was
         * allowed to stop mid-element, which it is not for a size-changing (CONVERTOR_UNSAFE_SPLIT)
         * convertor. So the only heterogeneous conversions that can reach this partial path are
         * same-size structural transforms (e.g. byte swapping), for which remote and local widths
         * are equal. Assert the invariant rather than attempting a size-changing partial reassembly
         * the marker trick below cannot express.
         */
        assert(!(pConvertor->flags & CONVERTOR_UNSAFE_SPLIT));
        assert(remote_length == data_length);
        copied = pConvertor->master->pFunctions[pElem->elem.common.type](
            pConvertor, 1, 1, 1, &from, remote_length, remote_length, &to, data_length, data_length);
        assert(1 == copied);
        (void) copied;
    }

    /* reload the length and user buffer as they have been updated by the macro */
    data_length = opal_datatype_basicDatatypes[pElem->elem.common.type]->size;
    user_data = *memory + pElem->elem.disp;

    /* Rebuild the data by pulling back the unmodified bytes from the original
     * content in the user memory. */
    /* Need to copy the modified user_data again so we can see which
     * bytes need to be converted back to their original values. */
    if (0 != strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name, "null")) {
        char resaved_data[OPAL_DATATYPE_MAX_PREDEFINED_SIZE];
        pConvertor->cbmemcpy(resaved_data, user_data, data_length, pConvertor);
        for (size_t i = 0; i < data_length; i++) {
            if (unused_byte == resaved_data[i])
                pConvertor->cbmemcpy(&user_data[i], &saved_data[i], 1, pConvertor);
        }
    } else {
        for (size_t i = 0; i < data_length; i++) {
            if (unused_byte == user_data[i]) {
                user_data[i] = saved_data[i];
            }
        }
    }

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

#endif /* OPAL_DATATYPE_UNPACK_H_HAS_BEEN_INCLUDED */
