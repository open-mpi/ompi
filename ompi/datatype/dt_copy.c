/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/prefetch.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/convertor.h"
#include "ompi/datatype/datatype_internal.h"
#include "ompi/datatype/datatype_checksum.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

#if OMPI_ENABLE_DEBUG
extern int ompi_copy_debug;
#define DO_DEBUG(INST)  if( ompi_copy_debug ) { INST }
#else
#define DO_DEBUG(INST)
#endif  /* OMPI_ENABLE_DEBUG */

size_t ompi_datatype_memcpy_block_size = 128 * 1024;

static inline void copy_predefined_data( const dt_elem_desc_t* ELEM,
                                         const ompi_datatype_t* DATATYPE,
                                         char* SOURCE_BASE,
                                         int32_t TOTAL_COUNT,
                                         uint32_t COUNT,
                                         char* SOURCE,
                                         char* DESTINATION,
                                         size_t* SPACE )
{
    uint32_t _copy_count = (COUNT);
	size_t _copy_blength;
    const ddt_elem_desc_t* _elem = &((ELEM)->elem);
    char* _source = (SOURCE) + _elem->disp;
    char* _destination = (DESTINATION) + _elem->disp;

    _copy_blength = ompi_ddt_basicDatatypes[_elem->common.type]->size;

    if( _copy_blength == (uint32_t)_elem->extent ) {
        _copy_blength *= _copy_count;
        OMPI_DDT_SAFEGUARD_POINTER( _source, _copy_blength, (SOURCE_BASE),
                                    (DATATYPE), (TOTAL_COUNT) );
        /* the extent and the size of the basic datatype are equals */
        DO_DEBUG( opal_output( 0, "copy 1. memcpy( %p, %p, %ld ) => space %lu\n",
                               _destination, _source, _copy_blength, (unsigned long)(*(SPACE)) ); );
        MEMCPY( _destination, _source, _copy_blength );
        _source      += _copy_blength;
        _destination += _copy_blength;
    } else {
        uint32_t _i;
        for( _i = 0; _i < _copy_count; _i++ ) {
            OMPI_DDT_SAFEGUARD_POINTER( _source, _copy_blength, (SOURCE_BASE),
                                        (DATATYPE), (TOTAL_COUNT) );
            DO_DEBUG( opal_output( 0, "copy 2. memcpy( %p, %p, %ld ) => space %lu\n",
                                   _destination, _source, _copy_blength, (unsigned long)(*(SPACE) - (_i * _copy_blength)) ); );
            MEMCPY( _destination, _source, _copy_blength );
            _source      += _elem->extent;
            _destination += _elem->extent;
        }
        _copy_blength *= _copy_count;
    }
    *(SPACE)      -= _copy_blength;
}

static inline void copy_contiguous_loop( const dt_elem_desc_t* ELEM,
                                         const ompi_datatype_t* DATATYPE,
                                         char* SOURCE_BASE,
                                         int32_t TOTAL_COUNT,
                                         uint32_t COUNT,
                                         char* SOURCE,
                                         char* DESTINATION,
                                         size_t* SPACE )
{
    ddt_loop_desc_t *_loop = (ddt_loop_desc_t*)(ELEM);
    ddt_endloop_desc_t* _end_loop = (ddt_endloop_desc_t*)((ELEM) + _loop->items);
    char* _source = (SOURCE) + _end_loop->first_elem_disp;
    char* _destination = (DESTINATION) + _end_loop->first_elem_disp;
    size_t _copy_loops = (COUNT);
    uint32_t _i;

    if( _loop->extent == (ptrdiff_t)_end_loop->size ) {  /* the loop is contiguous */
        _copy_loops *= _end_loop->size;
        OMPI_DDT_SAFEGUARD_POINTER( _source, _copy_loops, (SOURCE_BASE),
                                    (DATATYPE), (TOTAL_COUNT) );
        MEMCPY( _destination, _source, _copy_loops );
    } else {
        for( _i = 0; _i < _copy_loops; _i++ ) {
            OMPI_DDT_SAFEGUARD_POINTER( _source, _end_loop->size, (SOURCE_BASE),
                                        (DATATYPE), (TOTAL_COUNT) );
            DO_DEBUG( opal_output( 0, "copy 3. memcpy( %p, %p, %ld ) => space %ld\n",
                                   _destination, _source, _end_loop->size, *(SPACE) - _i * _end_loop->size ); );
            MEMCPY( _destination, _source, _end_loop->size );
            _source      += _loop->extent;
            _destination += _loop->extent;
        }
        _copy_loops *= _end_loop->size;
    }
    *(SPACE)      -= _copy_loops;
}

#define COPY_PREDEFINED_DATATYPE( ELEM, DATATYPE, SOURCE_BASE, TOTAL_COUNT, COUNT, SOURCE, DESTINATION, SPACE ) \
    copy_predefined_data( (ELEM), (DATATYPE), (SOURCE_BASE), (TOTAL_COUNT), \
                          (COUNT), (SOURCE), (DESTINATION), &(SPACE) )

#define COPY_CONTIGUOUS_LOOP( ELEM, DATATYPE, SOURCE_BASE, TOTAL_COUNT, COUNT, SOURCE, DESTINATION, SPACE ) \
    copy_contiguous_loop( (ELEM), (DATATYPE), (SOURCE_BASE), (TOTAL_COUNT), \
                          (COUNT), (SOURCE), (DESTINATION), &(SPACE) )

int32_t ompi_ddt_copy_content_same_ddt( const ompi_datatype_t* datatype, int32_t count,
                                        char* destination_base, char* source_base )
{
    dt_stack_t* pStack;       /* pointer to the position on the stack */
    int32_t stack_pos;        /* index of the stack level */
    uint32_t pos_desc;        /* actual position in the description of the derived datatype */
    uint32_t count_desc;      /* the number of items already done in the actual pos_desc */
    dt_elem_desc_t* description;
    dt_elem_desc_t* pElem;
    size_t iov_len_local;
    char *source = source_base, *destination = destination_base;

    DO_DEBUG( opal_output( 0, "ompi_ddt_copy_content_same_ddt( %p, %d, dst %p, src %p )\n",
                           (void*)datatype, count, destination_base, source_base ); );
    /* empty data ? then do nothing. This should normally be trapped
     * at a higher level.
     */
    if( 0 == count ) return 1;

    iov_len_local = count * datatype->size;

    /* If we have to copy a contiguous datatype then simply
     * do a memcpy.
     */
    if( (datatype->flags & DT_FLAG_CONTIGUOUS) == DT_FLAG_CONTIGUOUS ) {
        ptrdiff_t extent = (datatype->ub - datatype->lb);
        /* Now that we know the datatype is contiguous, we should move the 2 pointers
         * source and destination to the correct displacement.
         */
        destination += datatype->lb;
        source      += datatype->lb;
        if( (ptrdiff_t)datatype->size == extent ) {  /* all contiguous == no gaps around */
            size_t total_length = datatype->size * count;
            size_t memcpy_chunk = ompi_datatype_memcpy_block_size;
            while( total_length > 0 ) {
                if( memcpy_chunk > total_length ) memcpy_chunk = total_length;
                OMPI_DDT_SAFEGUARD_POINTER( destination, memcpy_chunk,
                                            destination, datatype, count );
                DO_DEBUG( opal_output( 0, "copy c1. memcpy( %p, %p, %ld ) => space %lu\n",
                                       destination, source, memcpy_chunk, (unsigned long)iov_len_local ); );
                MEMCPY( destination, source, memcpy_chunk );
                destination   += memcpy_chunk;
                source        += memcpy_chunk;
                total_length  -= memcpy_chunk;
                iov_len_local -= memcpy_chunk;
            }
        } else {
            for( pos_desc = 0; (int32_t)pos_desc < count; pos_desc++ ) {
                OMPI_DDT_SAFEGUARD_POINTER( destination, datatype->size,
                                            destination, datatype, count );
                DO_DEBUG( opal_output( 0, "copy c2. memcpy( %p, %p, %ld ) => space %lu\n",
                                       destination, source, datatype->size,
                                       (unsigned long)(iov_len_local - (pos_desc * datatype->size)) ); );
                MEMCPY( destination, source, datatype->size );
                destination += extent;
                source += extent;
            }
        }
        return 0;
    }

    pStack = (dt_stack_t*)alloca( sizeof(dt_stack_t) * (datatype->btypes[DT_LOOP] + 1) );
    pStack->count = count;
    pStack->index   = -1;
    pStack->disp    = 0;
    pos_desc = 0;
    stack_pos = 0;

    if( datatype->opt_desc.desc != NULL ) {
        description = datatype->opt_desc.desc;
    } else {
        description = datatype->desc.desc;
    }

    if( description[0].elem.common.type == DT_LOOP )
        count_desc = description[0].loop.loops;
    else
        count_desc = description[0].elem.count;
    pElem = &(description[pos_desc]);

    while( 1 ) {
        while( OPAL_LIKELY(pElem->elem.common.flags & DT_FLAG_DATA) ) {
            /* now here we have a basic datatype */
            COPY_PREDEFINED_DATATYPE( pElem, datatype, source_base, count, count_desc,
                                      source, destination, iov_len_local );
            pos_desc++;  /* advance to the next data */
            UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
        }
        if( DT_END_LOOP == pElem->elem.common.type ) { /* end of the current loop */
            DO_DEBUG( opal_output( 0, "copy end_loop count %d stack_pos %d pos_desc %d disp %ld space %lu\n",
                                   (int)pStack->count, stack_pos, pos_desc, (long)pStack->disp, (unsigned long)iov_len_local ); );
            if( --(pStack->count) == 0 ) { /* end of loop */
                if( stack_pos == 0 ) {
                    assert( iov_len_local == 0 );
                    return 0;  /* completed */
                }
                stack_pos--;
                pStack--;
                pos_desc++;
            } else {
                pos_desc = pStack->index + 1;
                if( pStack->index == -1 ) {
                    pStack->disp += (datatype->ub - datatype->lb);
                } else {
                    assert( DT_LOOP == description[pStack->index].loop.common.type );
                    pStack->disp += description[pStack->index].loop.extent;
                }
            }
            source      = source_base + pStack->disp;
            destination = destination_base + pStack->disp;
            UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
            DO_DEBUG( opal_output( 0, "copy new_loop count %d stack_pos %d pos_desc %d disp %ld space %lu\n",
                                   (int)pStack->count, stack_pos, pos_desc, (long)pStack->disp, (unsigned long)iov_len_local ); );
        }
        if( DT_LOOP == pElem->elem.common.type ) {
            ptrdiff_t local_disp = (ptrdiff_t)source;
            if( pElem->loop.common.flags & DT_FLAG_CONTIGUOUS ) {
                COPY_CONTIGUOUS_LOOP( pElem, datatype, source_base, count, count_desc,
                                      source, destination, iov_len_local );
                pos_desc += pElem->loop.items + 1;
                goto update_loop_description;
            }
            local_disp = (ptrdiff_t)source - local_disp;
            PUSH_STACK( pStack, stack_pos, pos_desc, DT_LOOP, count_desc,
                        pStack->disp + local_disp);
            pos_desc++;
        update_loop_description:  /* update the current state */
            source      = source_base + pStack->disp;
            destination = destination_base + pStack->disp;
            UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
            DDT_DUMP_STACK( pStack, stack_pos, pElem, "advance loop" );
            continue;
        }
    }
}
