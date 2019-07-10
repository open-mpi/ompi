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
    size_t blocklen_bytes = opal_datatype_basicDatatypes[_elem->common.type]->size;
    size_t cando_count = (*COUNT), do_now, do_now_bytes;
    unsigned char* _memory = (*memory) + _elem->disp;
    unsigned char* _packed = *packed;

    assert( *(COUNT) <= (_elem->count * _elem->blocklen));

    if( (blocklen_bytes * cando_count) > *(SPACE) )
        cando_count = (*SPACE) / blocklen_bytes;

    do_now = *(COUNT);  /* save the COUNT for later */
    /* premptively update the number of COUNT we will return. */
    *(COUNT) -= cando_count;

    if( 1 == _elem->count ) {  /* Everything is contiguous, handle it as a prologue */
        goto do_epilog;
    }
    if( 1 == _elem->blocklen ) {  /* Do as many full blocklen as possible */
        for(; cando_count > 0; cando_count--) {
            OPAL_DATATYPE_SAFEGUARD_POINTER( _memory, blocklen_bytes, (CONVERTOR)->pBaseBuf,
                                             (CONVERTOR)->pDesc, (CONVERTOR)->count );
            DO_DEBUG( opal_output( 0, "unpack 2. memcpy( %p, %p, %lu ) => space %lu\n",
                                   (void*)_memory, (void*)_packed, (unsigned long)blocklen_bytes, (unsigned long)(*(SPACE) - (_packed - *(packed))) ); );
            MEMCPY_CSUM( _memory, _packed, blocklen_bytes, (CONVERTOR) );
            _packed     += blocklen_bytes;
            _memory     += _elem->extent;
        }
        goto update_and_return;
    }

    blocklen_bytes *= _elem->blocklen;
    if( (_elem->count * _elem->blocklen) == cando_count ) {
        goto skip_prolog;
    }

    /**
     * First check if we already did something on this element ? The COUNT is the number
     * of remaining predefined types in the current elem, not how many predefined types
     * should be manipulated in the current call (this number is instead reflected on the
     * SPACE).
     */
    do_now = do_now % _elem->blocklen;  /* any partial elements ? */

    if( 0 != do_now ) {
        size_t left_in_block = do_now;  /* left in the current blocklen */
        do_now = (do_now > cando_count ) ? cando_count : do_now;
        do_now_bytes = do_now * opal_datatype_basicDatatypes[_elem->common.type]->size;

        OPAL_DATATYPE_SAFEGUARD_POINTER( _memory, do_now_bytes, (CONVERTOR)->pBaseBuf,
                                         (CONVERTOR)->pDesc, (CONVERTOR)->count );
        DO_DEBUG( opal_output( 0, "unpack 1. memcpy( %p, %p, %lu ) => space %lu [prolog]\n",
                               (void*)_memory, (void*)_packed, (unsigned long)do_now_bytes, (unsigned long)(*(SPACE)) ); );
        MEMCPY_CSUM( _memory, _packed, do_now_bytes, (CONVERTOR) );
        _memory      += (ptrdiff_t)do_now_bytes;
        /* compensate if we just completed a blocklen */
        if( do_now == left_in_block )
            _memory += _elem->extent - blocklen_bytes;
        _packed     += do_now_bytes;
        cando_count -= do_now;
    }

 skip_prolog:
    /* Do as many full blocklen as possible */
    for(size_t _i = 0; _elem->blocklen <= cando_count; _i++ ) {
        OPAL_DATATYPE_SAFEGUARD_POINTER( _memory, blocklen_bytes, (CONVERTOR)->pBaseBuf,
                                         (CONVERTOR)->pDesc, (CONVERTOR)->count );
        DO_DEBUG( opal_output( 0, "unpack 2. memcpy( %p, %p, %lu ) => space %lu\n",
                               (void*)_memory, (void*)_packed, (unsigned long)blocklen_bytes, (unsigned long)(*(SPACE) - (_packed - *(packed))) ); );
        MEMCPY_CSUM( _memory, _packed, blocklen_bytes, (CONVERTOR) );
        _packed     += blocklen_bytes;
        _memory     += _elem->extent;
        cando_count -= _elem->blocklen;
    }

    /**
     * As an epilog do anything left from the last blocklen.
     */
    if( 0 != cando_count ) {

    do_epilog:
        assert( cando_count < _elem->blocklen );
        do_now_bytes = cando_count * opal_datatype_basicDatatypes[_elem->common.type]->size;
        OPAL_DATATYPE_SAFEGUARD_POINTER( _memory, do_now_bytes, (CONVERTOR)->pBaseBuf,
                                         (CONVERTOR)->pDesc, (CONVERTOR)->count );
        DO_DEBUG( opal_output( 0, "unpack 3. memcpy( %p, %p, %lu ) => space %lu [epilog]\n",
                               (void*)_memory, (void*)_packed, (unsigned long)do_now_bytes, (unsigned long)(*(SPACE) - (_packed - *(packed))) ); );
        MEMCPY_CSUM( _memory, _packed, do_now_bytes, (CONVERTOR) );
        _memory   += do_now_bytes;
        _packed   += do_now_bytes;
    }

 update_and_return:
    *(memory)  = _memory - _elem->disp;
    *(SPACE)  -= (_packed - *packed);
    *(packed)  = _packed;
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
