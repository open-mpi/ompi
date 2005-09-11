/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "datatype/datatype.h"
#include "datatype/convertor.h"
#include "datatype/datatype_internal.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

static int ompi_unpack_debug = 0;

#define DO_DEBUG(INST)  if( ompi_unpack_debug ) { INST }

/* The pack/unpack functions need a cleanup. I have to create a proper interface to access
 * all basic functionalities, hence using them as basic blocks for all conversion functions.
 *
 * But first let's make some global assumptions:
 * - a datatype (with the flag DT_DATA set) will have the contiguous flags set if and only if
 *   the data is really contiguous (extent equal with size)
 * - for the DT_LOOP type the DT_CONTIGUOUS flag set means that the content of the loop is
 *   contiguous but with a gap in the begining or at the end.
 * - the DT_CONTIGUOUS flag for the type DT_END_LOOP is meaningless.
 */

static inline void unpack_predefined_data( ompi_convertor_t* CONVERTOR, /* the convertor */
                                           uint16_t TYPE,               /* the basic type to be packed */
                                           uint32_t* COUNT,             /* the number of elements */
                                           long EXTENT,                 /* the extent in bytes of each element */
                                           char** SOURCE,               /* the source pointer */
                                           char** DESTINATION,          /* the destination pointer */
                                           uint32_t* SPACE )            /* the space in the destination buffer */
{
    uint32_t _copy_count = *(COUNT), _copy_blength;

    if( (_copy_count * ompi_ddt_basicDatatypes[(TYPE)]->size) > *(SPACE) ) {
        _copy_count = *(SPACE) / ompi_ddt_basicDatatypes[(TYPE)]->size;
        if( 0 == _copy_count ) return;  /* nothing to do */
    }
    _copy_blength = _copy_count * ompi_ddt_basicDatatypes[(TYPE)]->size;

    if( ompi_ddt_basicDatatypes[(TYPE)]->size == (uint32_t)(EXTENT) ) {
        /* the extent and the size of the basic datatype are equals */
        OMPI_DDT_SAFEGUARD_POINTER( *(DESTINATION), _copy_blength, (CONVERTOR)->pBaseBuf,
                                    (CONVERTOR)->pDesc, (CONVERTOR)->count );
        DO_DEBUG( opal_output( 0, "unpack 1. memcpy( %p, %p, %ld ) => space %d\n",
                               *(DESTINATION), *(SOURCE), _copy_blength, *(SPACE) ); );
        MEMCPY( *(DESTINATION), *(SOURCE), _copy_blength );
        *(SOURCE) += _copy_blength;
        *(DESTINATION) += _copy_blength;
    } else {
        uint32_t _i;
        for( _i = 0; _i < _copy_count; _i++ ) {
            OMPI_DDT_SAFEGUARD_POINTER( *(DESTINATION), _copy_blength, (CONVERTOR)->pBaseBuf,
                                        (CONVERTOR)->pDesc, (CONVERTOR)->count );
            DO_DEBUG( opal_output( 0, "unpack 2. memcpy( %p, %p, %ld ) => space %d\n",
                                   *(DESTINATION), *(SOURCE), _copy_blength,
                                   *(SPACE) - (_i * ompi_ddt_basicDatatypes[(TYPE)]->size) ); );
            MEMCPY( *(DESTINATION), *(SOURCE), ompi_ddt_basicDatatypes[(TYPE)]->size );
            *(SOURCE) += ompi_ddt_basicDatatypes[(TYPE)]->size;
            *(DESTINATION) += (EXTENT);
        }
    }
    *(SPACE) -= _copy_blength;
    *(COUNT) -= _copy_count;
}

static inline void unpack_contiguous_loop( ompi_convertor_t* CONVERTOR,
                                           dt_elem_desc_t* ELEM,
                                           uint32_t* COUNT,
                                           char** SOURCE,
                                           char** DESTINATION,
                                           uint32_t* SPACE )
{
    ddt_loop_desc_t *loop = (ddt_loop_desc_t*)(ELEM);
    ddt_endloop_desc_t* end_loop = (ddt_endloop_desc_t*)((ELEM) + (ELEM)->loop.items);
    size_t _copy_loops = *(COUNT);
    uint32_t _i;

    if( (_copy_loops * end_loop->size) > *(SPACE) )
        _copy_loops = *(SPACE) / end_loop->size;
    for( _i = 0; _i < _copy_loops; _i++ ) {
        OMPI_DDT_SAFEGUARD_POINTER( *(DESTINATION),
                                    end_loop->size, (CONVERTOR)->pBaseBuf,
                                    (CONVERTOR)->pDesc, (CONVERTOR)->count );
        DO_DEBUG( opal_output( 0, "unpack 3. memcpy( %p, %p, %ld )\n",
                               *(DESTINATION), *(SOURCE), end_loop->size ); );
        MEMCPY( *(DESTINATION), *(SOURCE), end_loop->size );
        *(SOURCE) += end_loop->size;
        *(DESTINATION) += loop->extent;
    }
    *(SPACE) -= _copy_loops * end_loop->size;
    *(COUNT) -= _copy_loops;
}

#define UNPACK_PREDEFINED_DATATYPE( CONVERTOR, TYPE, COUNT, EXTENT, SOURCE, DESTINATION, SPACE ) \
    unpack_predefined_data( (CONVERTOR), (TYPE), &(COUNT), (EXTENT), &(SOURCE), &(DESTINATION), &(SPACE) )

#define UNPACK_CONTIGUOUS_LOOP( CONVERTOR, ELEM, COUNT, SOURCE, DESTINATION, SPACE ) \
    unpack_contiguous_loop( (CONVERTOR), (ELEM), &(COUNT), &(SOURCE), &(DESTINATION), &(SPACE) )

#define UPDATE_INTERNAL_COUNTERS( DESCRIPTION, POSITION, ELEMENT, COUNTER, DISPLACEMENT ) \
    do {                                                                \
        (ELEMENT) = &((DESCRIPTION)[(POSITION)]);                       \
        (COUNTER) = (ELEMENT)->elem.count;                              \
        (DISPLACEMENT)  = (ELEMENT)->elem.disp;                         \
    } while (0)

int ompi_convertor_generic_simple_unpack( ompi_convertor_t* pConvertor,
                                          struct iovec* iov, uint32_t* out_size,
                                          size_t* max_data,
                                          int32_t* freeAfter )
{
    dt_stack_t* pStack;       /* pointer to the position on the stack */
    uint32_t pos_desc;        /* actual position in the description of the derived datatype */
    uint32_t count_desc;      /* the number of items already done in the actual pos_desc */
    uint16_t type;            /* type at current position */
    long disp_desc = 0;       /* compute displacement for truncated data */
    size_t total_unpack = 0;  /* total size unpacked this time */
    dt_elem_desc_t* description;
    dt_elem_desc_t* pElem;
    const ompi_datatype_t *pData = pConvertor->pDesc;
    char *user_memory_base, *user_memory, *packed_buffer;
    uint32_t iov_len_local, iov_count;

    DUMP( "ompi_convertor_generic_simple_unpack( %p, {%p, %d}, %d )\n", (void*)pConvertor,
          iov[0].iov_base, iov[0].iov_len, *out_size );

    description = pConvertor->use_desc->desc;

    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc   = pStack->index;
    disp_desc  = pStack->disp;
    count_desc = pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &(description[pos_desc]); 
    user_memory_base = pConvertor->pBaseBuf + pStack->disp;
    user_memory = user_memory_base + disp_desc;

    DO_DEBUG( opal_output( 0, "unpack start pos_desc %d count_desc %d disp %ld\n"
                           "stack_pos %d pos_desc %d count_desc %d disp %ld\n",
                           pos_desc, count_desc, disp_desc,
                           pConvertor->stack_pos, pStack->index, pStack->count, pStack->disp ); );

    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {
        if( pConvertor->flags & CONVERTOR_COMPLETED )
            break;  /* do not unpack over the boundaries even if there are more iovecs */

        packed_buffer = iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;
        while( 1 ) {
            if( DT_END_LOOP == pElem->elem.common.type ) { /* end of the current loop */
                DO_DEBUG( opal_output( 0, "unpack end_loop count %d stack_pos %d pos_desc %d disp %ld space %d\n",
                                       pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp, iov_len_local ); );
                if( --(pStack->count) == 0 ) { /* end of loop */
                    if( pConvertor->stack_pos == 0 ) {
                        pConvertor->flags |= CONVERTOR_COMPLETED;
                        goto complete_loop;  /* completed */
                    }
                    pConvertor->stack_pos--;
                    pStack--;
                    pos_desc++;
                } else {
                    pos_desc = pStack->index + 1;
                    if( pStack->index == -1 ) {
                        pStack->disp += (pData->ub - pData->lb);
                    } else {
                        assert( DT_LOOP == description[pStack->index].loop.common.type );
                        pStack->disp += description[pStack->index].loop.extent;
                    }
                }
                user_memory_base = pConvertor->pBaseBuf + pStack->disp;
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc, disp_desc );
                DO_DEBUG( opal_output( 0, "unpack new_loop count %d stack_pos %d pos_desc %d disp %ld space %d\n",
                                       pStack->count, pConvertor->stack_pos, pos_desc, disp_desc, iov_len_local ); );
            }
            if( DT_LOOP == pElem->elem.common.type ) {
                if( pElem->loop.common.flags & DT_FLAG_CONTIGUOUS ) {
                    user_memory = user_memory_base + disp_desc;
                    UNPACK_CONTIGUOUS_LOOP( pConvertor, pElem, count_desc, 
                                            packed_buffer, user_memory, iov_len_local );
                    if( 0 == count_desc ) {  /* completed */
                        pos_desc += pElem->loop.items + 1;
                        goto update_loop_description;
                    }
                    /* Save the stack with the correct last_count value. */
                }
                PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, DT_LOOP, count_desc,
                            pStack->disp, pos_desc + pElem->elem.disp + 1);
                pos_desc++;
            update_loop_description:
                /* update the current state */
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc, disp_desc );
                DDT_DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElem, "advance loop" );
                continue;
            }
            while( pElem->elem.common.flags & DT_FLAG_DATA ) {
                /* now here we have a basic datatype */
                type = pElem->elem.common.type;
                user_memory = user_memory_base + disp_desc;
                UNPACK_PREDEFINED_DATATYPE( pConvertor, type, count_desc, pElem->elem.extent,
                                            packed_buffer, user_memory, iov_len_local );
                if( 0 != count_desc ) {  /* completed */
                    goto complete_loop;
                }
                pos_desc++;  /* advance to the next data */
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc, disp_desc );
            }
        }
    complete_loop:
        iov[iov_count].iov_len -= iov_len_local;  /* update the amount of valid data */
        total_unpack += iov[iov_count].iov_len;
        pConvertor->bConverted += iov[iov_count].iov_len;  /* update the already converted bytes */
        assert( iov_len_local >= 0 );
    }
    *max_data = total_unpack;
    if( !(pConvertor->flags & CONVERTOR_COMPLETED) ) {
        /* I complete an element, next step I should go to the next one */
        PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, DT_BYTE, count_desc,
                    user_memory - user_memory_base, pos_desc );
        DO_DEBUG( opal_output( 0, "unpack save stack stack_pos %d pos_desc %d count_desc %d disp %ld\n",
                               pConvertor->stack_pos, pStack->index, pStack->count, pStack->disp ); );
        return 0;
    }
    return 1;
}

