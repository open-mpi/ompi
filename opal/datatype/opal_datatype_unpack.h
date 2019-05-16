/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2019 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2017-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_DATATYPE_UNPACK_H_HAS_BEEN_INCLUDED
#define OPAL_DATATYPE_UNPACK_H_HAS_BEEN_INCLUDED

#include "opal_config.h"

#if !defined(CHECKSUM) && OPAL_CUDA_SUPPORT
/* Make use of existing macro to do CUDA style memcpy */
#undef MEMCPY_CSUM
#define MEMCPY_CSUM( DST, SRC, BLENGTH, CONVERTOR ) \
    CONVERTOR->cbmemcpy( (DST), (SRC), (BLENGTH), (CONVERTOR) )
#endif

static inline void
unpack_predefined_data( opal_convertor_t* CONVERTOR,
                        const dt_elem_desc_t* ELEM,
                        size_t* COUNT,
                        unsigned char** packed,
                        unsigned char** memory,
                        size_t* SPACE )
{
    const ddt_elem_desc_t* _elem = &((ELEM)->elem);
    size_t total_count = _elem->count * _elem->blocklen;
    size_t cando_count = (*SPACE) / opal_datatype_basicDatatypes[_elem->common.type]->size;
    size_t do_now, do_now_bytes;
    unsigned char* _memory = (*memory) + _elem->disp;

    assert( *(COUNT) <= _elem->count * _elem->blocklen);

    if( cando_count > *(COUNT) )
        cando_count = *(COUNT);

    /**
     * First check if we already did something on this element ?
     */
    do_now = (total_count - *(COUNT));  /* done elements */
    if( 0 != do_now ) {
        _memory += (do_now / _elem->blocklen) * _elem->extent;  /* account for all the full blocklen */
        do_now = do_now % _elem->blocklen;  /* partial blocklen? */

        if( 0 != do_now ) {
            _memory  += (do_now * opal_datatype_basicDatatypes[_elem->common.type]->size);

            do_now       = _elem->blocklen - do_now;  /* left in the current blocklen */
            do_now_bytes = do_now * opal_datatype_basicDatatypes[_elem->common.type]->size;

            OPAL_DATATYPE_SAFEGUARD_POINTER( _memory, do_now_bytes, (CONVERTOR)->pBaseBuf,
                                            (CONVERTOR)->pDesc, (CONVERTOR)->count );
            DO_DEBUG( opal_output( 0, "unpack 1. memcpy( %p, %p, %lu ) => space %lu [prolog]\n",
                                   (void*)_memory, (void*)*(packed), (unsigned long)do_now_bytes, (unsigned long)(*(SPACE)) ); );
            MEMCPY_CSUM( _memory, *(packed), do_now_bytes, (CONVERTOR) );
            _memory      = (*memory) + _elem->disp + _elem->extent;
            *(packed)   += do_now_bytes;
            *(SPACE)    -= do_now_bytes;
            *(COUNT)    -= do_now;
            cando_count -= do_now;
        }
    }

    /**
     * Compute how many full blocklen we need to do and do them.
     */
    do_now = cando_count / _elem->blocklen;
    if( 0 != do_now ) {
        do_now_bytes = _elem->blocklen * opal_datatype_basicDatatypes[_elem->common.type]->size;
        for(size_t _i = 0; _i < do_now; _i++ ) {
            OPAL_DATATYPE_SAFEGUARD_POINTER( _memory, do_now_bytes, (CONVERTOR)->pBaseBuf,
                                            (CONVERTOR)->pDesc, (CONVERTOR)->count );
            DO_DEBUG( opal_output( 0, "pack 2. memcpy( %p, %p, %lu ) => space %lu\n",
                                   (void*)_memory, (void*)*(packed), (unsigned long)do_now_bytes, (unsigned long)*(SPACE) ); );
            MEMCPY_CSUM( _memory, *(packed), do_now_bytes, (CONVERTOR) );
            *(packed)   += do_now_bytes;
            _memory     += _elem->extent;
            *(SPACE)    -= do_now_bytes;
            *(COUNT)    -= _elem->blocklen;
            cando_count -= _elem->blocklen;
        }
    }

    /**
     * As an epilog do anything left from the last blocklen.
     */
    do_now = cando_count;
    if( 0 != do_now ) {
        do_now_bytes = do_now * opal_datatype_basicDatatypes[_elem->common.type]->size;
        OPAL_DATATYPE_SAFEGUARD_POINTER( _memory, do_now_bytes, (CONVERTOR)->pBaseBuf,
                                        (CONVERTOR)->pDesc, (CONVERTOR)->count );
        DO_DEBUG( opal_output( 0, "pack 3. memcpy( %p, %p, %lu ) => space %lu [epilog]\n",
                               (void*)_memory, (void*)*(packed), (unsigned long)do_now_bytes, (unsigned long)(*(SPACE)) ); );
        MEMCPY_CSUM( _memory, *(packed), do_now_bytes, (CONVERTOR) );
        *(packed) += do_now_bytes;
        *(SPACE)  -= do_now_bytes;
        *(COUNT)  -= do_now;
    }

    *(memory)  = _memory - _elem->disp;
}

static inline void unpack_contiguous_loop( opal_convertor_t* CONVERTOR,
                                           const dt_elem_desc_t* ELEM,
                                           size_t* COUNT,
                                           unsigned char** packed,
                                           unsigned char** memory,
                                           size_t* SPACE )
{
    const ddt_loop_desc_t *_loop = (ddt_loop_desc_t*)(ELEM);
    const ddt_endloop_desc_t* _end_loop = (ddt_endloop_desc_t*)((ELEM) + _loop->items);
    unsigned char* _memory = (*memory) + _end_loop->first_elem_disp;
    size_t _copy_loops = *(COUNT);

    if( (_copy_loops * _end_loop->size) > *(SPACE) )
        _copy_loops = (*(SPACE) / _end_loop->size);
    for(size_t _i = 0; _i < _copy_loops; _i++ ) {
        OPAL_DATATYPE_SAFEGUARD_POINTER( _memory, _end_loop->size, (CONVERTOR)->pBaseBuf,
                                    (CONVERTOR)->pDesc, (CONVERTOR)->count );
        DO_DEBUG( opal_output( 0, "unpack 3. memcpy( %p, %p, %lu ) => space %lu\n",
                               (void*)_memory, (void*)*(packed), (unsigned long)_end_loop->size, (unsigned long)(*(SPACE) - _i * _end_loop->size) ); );
        MEMCPY_CSUM( _memory, *(packed), _end_loop->size, (CONVERTOR) );
        *(packed) += _end_loop->size;
        _memory   += _loop->extent;
    }
    *(memory)  = _memory - _end_loop->first_elem_disp;
    *(SPACE)  -= _copy_loops * _end_loop->size;
    *(COUNT)  -= _copy_loops;
}

#define UNPACK_PREDEFINED_DATATYPE( CONVERTOR, ELEM, COUNT, PACKED, MEMORY, SPACE ) \
    unpack_predefined_data( (CONVERTOR), (ELEM), &(COUNT), &(PACKED), &(MEMORY), &(SPACE) )

#define UNPACK_CONTIGUOUS_LOOP( CONVERTOR, ELEM, COUNT, PACKED, MEMORY, SPACE ) \
    unpack_contiguous_loop( (CONVERTOR), (ELEM), &(COUNT), &(PACKED), &(MEMORY), &(SPACE) )

#endif  /* OPAL_DATATYPE_UNPACK_H_HAS_BEEN_INCLUDED */
