/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "ompi/datatype/datatype.h"
#include "ompi/datatype/convertor.h"
#include "ompi/datatype/datatype_internal.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

#if OMPI_ENABLE_DEBUG
int ompi_unpack_debug = 0;
#define DO_DEBUG(INST)  if( ompi_unpack_debug ) { INST }
#else
#define DO_DEBUG(INST)
#endif  /* OMPI_ENABLE_DEBUG */

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
                                           dt_elem_desc_t* ELEM,         /* the element description */
                                           uint32_t* COUNT,              /* the number of elements */
                                           char** SOURCE,                /* the source pointer */
                                           char** DESTINATION,           /* the destination pointer */
                                           uint32_t* SPACE )             /* the space in the destination buffer */
{
    uint32_t _copy_count = *(COUNT), _copy_blength;
    ddt_elem_desc_t* _elem = &((ELEM)->elem);
    char* _destination = (*DESTINATION) + _elem->disp;

    _copy_blength = ompi_ddt_basicDatatypes[_elem->common.type]->size;
    if( (_copy_count * _copy_blength) > *(SPACE) ) {
        _copy_count = *(SPACE) / _copy_blength;
        if( 0 == _copy_count ) return;  /* nothing to do */
    }

    if( _copy_blength == (uint32_t)_elem->extent ) {
        _copy_blength *= _copy_count;
        /* the extent and the size of the basic datatype are equals */
        OMPI_DDT_SAFEGUARD_POINTER( _destination, _copy_blength, (CONVERTOR)->pBaseBuf,
                                    (CONVERTOR)->pDesc, (CONVERTOR)->count );
        DO_DEBUG( opal_output( 0, "unpack 1. memcpy( %p, %p, %ld ) => space %d\n",
                               _destination, *(SOURCE), _copy_blength, *(SPACE) ); );
        MEMCPY( _destination, *(SOURCE), _copy_blength );
        *(SOURCE)    += _copy_blength;
        _destination += _copy_blength;
    } else {
        uint32_t _i;
        for( _i = 0; _i < _copy_count; _i++ ) {
            OMPI_DDT_SAFEGUARD_POINTER( _destination, _copy_blength, (CONVERTOR)->pBaseBuf,
                                        (CONVERTOR)->pDesc, (CONVERTOR)->count );
            DO_DEBUG( opal_output( 0, "unpack 2. memcpy( %p, %p, %ld ) => space %d\n",
                                   _destination, *(SOURCE), _copy_blength, *(SPACE) - (_i * _copy_blength) ); );
            MEMCPY( _destination, *(SOURCE), _copy_blength );
            *(SOURCE)    += _copy_blength;
            _destination += _elem->extent;
        }
        _copy_blength *= _copy_count;
    }
    (*DESTINATION)  = _destination - _elem->disp;
    *(SPACE)       -= _copy_blength;
    *(COUNT)       -= _copy_count;
}

static inline void unpack_contiguous_loop( ompi_convertor_t* CONVERTOR,
                                           dt_elem_desc_t* ELEM,
                                           uint32_t* COUNT,
                                           char** SOURCE,
                                           char** DESTINATION,
                                           uint32_t* SPACE )
{
    ddt_loop_desc_t *_loop = (ddt_loop_desc_t*)(ELEM);
    ddt_endloop_desc_t* _end_loop = (ddt_endloop_desc_t*)((ELEM) + _loop->items);
    char* _destination = (*DESTINATION) + _end_loop->first_elem_disp;
    size_t _copy_loops = *(COUNT);
    uint32_t _i;

    if( (_copy_loops * _end_loop->size) > *(SPACE) )
        _copy_loops = *(SPACE) / _end_loop->size;
    for( _i = 0; _i < _copy_loops; _i++ ) {
        OMPI_DDT_SAFEGUARD_POINTER( _destination, _end_loop->size, (CONVERTOR)->pBaseBuf,
                                    (CONVERTOR)->pDesc, (CONVERTOR)->count );
        DO_DEBUG( opal_output( 0, "unpack 3. memcpy( %p, %p, %ld ) => space %ld\n",
                               _destination, *(SOURCE), _end_loop->size, *(SPACE) - _i * _end_loop->size ); );
        MEMCPY( _destination, *(SOURCE), _end_loop->size );
        *(SOURCE)    += _end_loop->size;
        _destination += _loop->extent;
    }
    *(DESTINATION) = _destination - _end_loop->first_elem_disp;
    *(SPACE)      -= _copy_loops * _end_loop->size;
    *(COUNT)      -= _copy_loops;
}

#define UNPACK_PREDEFINED_DATATYPE( CONVERTOR, ELEM, COUNT, SOURCE, DESTINATION, SPACE ) \
    unpack_predefined_data( (CONVERTOR), (ELEM), &(COUNT), &(SOURCE), &(DESTINATION), &(SPACE) )

#define UNPACK_CONTIGUOUS_LOOP( CONVERTOR, ELEM, COUNT, SOURCE, DESTINATION, SPACE ) \
    unpack_contiguous_loop( (CONVERTOR), (ELEM), &(COUNT), &(SOURCE), &(DESTINATION), &(SPACE) )

#define UPDATE_INTERNAL_COUNTERS( DESCRIPTION, POSITION, ELEMENT, COUNTER ) \
    do {                                                                \
        (ELEMENT) = &((DESCRIPTION)[(POSITION)]);                       \
        (COUNTER) = (ELEMENT)->elem.count;                              \
    } while (0)

int ompi_convertor_generic_simple_unpack( ompi_convertor_t* pConvertor,
                                          struct iovec* iov, uint32_t* out_size,
                                          size_t* max_data,
                                          int32_t* freeAfter )
{
    dt_stack_t* pStack;         /* pointer to the position on the stack */
    uint32_t pos_desc;          /* actual position in the description of the derived datatype */
    uint32_t count_desc;        /* the number of items already done in the actual pos_desc */
    uint16_t type;              /* type at current position */
    size_t total_unpacked = 0;  /* total size unpacked this time */
    dt_elem_desc_t* description;
    dt_elem_desc_t* pElem;
    const ompi_datatype_t *pData = pConvertor->pDesc;
    char *user_memory_base, *packed_buffer;
    uint32_t iov_len_local, iov_count, required_space = 0;

    DO_DEBUG( opal_output( 0, "ompi_convertor_generic_simple_unpack( %p, {%p, %lu}, %u )\n",
                           (void*)pConvertor, iov[0].iov_base, (size_t)iov[0].iov_len, *out_size ); );

    description = pConvertor->use_desc->desc;

    /* For the first step we have to add both displacement to the source. After in the
     * main while loop we will set back the source_base to the correct value. This is
     * due to the fact that the convertor can stop in the middle of a data with a count
     */
    user_memory_base = pConvertor->pBaseBuf;
    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc          = pStack->index;
    user_memory_base += pStack->disp;
    count_desc        = pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &(description[pos_desc]); 
    user_memory_base += pStack->disp;

    DO_DEBUG( opal_output( 0, "unpack start pos_desc %d count_desc %d disp %ld\n"
                           "stack_pos %d pos_desc %d count_desc %d disp %ld\n",
                           pos_desc, count_desc, user_memory_base - pConvertor->pBaseBuf,
                           pConvertor->stack_pos, pStack->index, pStack->count, pStack->disp ); );

    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {
        if( required_space > ((*max_data) - total_unpacked) )
            break;  /* do not pack over the boundaries even if there are more iovecs */

        packed_buffer = iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;
        if( 0 != pConvertor->pending_length ) {
            uint32_t element_length = ompi_ddt_basicDatatypes[pElem->elem.common.type]->size;
            uint32_t missing_length = element_length - pConvertor->pending_length;

            assert( pElem->elem.common.flags & DT_FLAG_DATA );
            memcpy( pConvertor->pending + pConvertor->pending_length, packed_buffer, missing_length );
            packed_buffer = pConvertor->pending;
            DO_DEBUG( opal_output( 0, "unpack pending from the last unpack %d out of %d bytes\n",
                                   pConvertor->pending_length, ompi_ddt_basicDatatypes[pElem->elem.common.type]->size ); );
            UNPACK_PREDEFINED_DATATYPE( pConvertor, pElem, count_desc,
                                        packed_buffer, user_memory_base, element_length );
            if( 0 == count_desc ) {
                user_memory_base = pConvertor->pBaseBuf + pStack->disp;
                pos_desc++;  /* advance to the next data */
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
            }
            assert( 0 == element_length );
            packed_buffer = (char*)iov[iov_count].iov_base + missing_length;
            iov_len_local -= missing_length;
            pConvertor->pending_length = 0;  /* nothing more inside */
        }
        while( 1 ) {
            if( DT_END_LOOP == pElem->elem.common.type ) { /* end of the current loop */
                DO_DEBUG( opal_output( 0, "unpack end_loop count %d stack_pos %d pos_desc %d disp %ld space %d\n",
                                       pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp, iov_len_local ); );
                if( --(pStack->count) == 0 ) { /* end of loop */
                    if( pConvertor->stack_pos == 0 ) {
                        /* we lie about the size of the next element in order to
                         * make sure we exit the main loop.
                         */
                        required_space = 0xffffffff;
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
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
                DO_DEBUG( opal_output( 0, "unpack new_loop count %d stack_pos %d pos_desc %d disp %ld space %d\n",
                                       pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp, iov_len_local ); );
            }
            if( DT_LOOP == pElem->elem.common.type ) {
                long local_disp = (long)user_memory_base;
                if( pElem->loop.common.flags & DT_FLAG_CONTIGUOUS ) {
                    UNPACK_CONTIGUOUS_LOOP( pConvertor, pElem, count_desc, 
                                            packed_buffer, user_memory_base, iov_len_local );
                    if( 0 == count_desc ) {  /* completed */
                        pos_desc += pElem->loop.items + 1;
                        goto update_loop_description;
                    }
                    /* Save the stack with the correct last_count value. */
                }
                local_disp = (long)user_memory_base - local_disp;
                PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, DT_LOOP, count_desc,
                            pStack->disp + local_disp, pos_desc + pElem->elem.disp + 1);
                pos_desc++;
            update_loop_description:  /* update the current state */
                user_memory_base = pConvertor->pBaseBuf + pStack->disp;
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
                DDT_DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElem, "advance loop" );
                continue;
            }
            while( pElem->elem.common.flags & DT_FLAG_DATA ) {
                /* now here we have a basic datatype */
                UNPACK_PREDEFINED_DATATYPE( pConvertor, pElem, count_desc,
                                            packed_buffer, user_memory_base, iov_len_local );
                if( 0 != count_desc ) {  /* completed */
                    type = pElem->elem.common.type;
                    required_space = ompi_ddt_basicDatatypes[type]->size;
                    goto complete_loop;
                }
                user_memory_base = pConvertor->pBaseBuf + pStack->disp;
                pos_desc++;  /* advance to the next data */
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
            }
        }
    complete_loop:
        if( !(pConvertor->flags & CONVERTOR_COMPLETED) && (0 != iov_len_local) ) {
            /* We have some partial data here. Let's copy it into the convertor
             * and keep it hot until the next round.
             */
            assert( iov_len_local < ompi_ddt_basicDatatypes[type]->size );
            memcpy( pConvertor->pending, packed_buffer, iov_len_local );
            DO_DEBUG( opal_output( 0, "Saving %d bytes for the next call\n", iov_len_local ); );
            pConvertor->pending_length = iov_len_local;
            iov_len_local = 0;
        }
        iov[iov_count].iov_len -= iov_len_local;  /* update the amount of valid data */
        total_unpacked += iov[iov_count].iov_len;
        pConvertor->bConverted += iov[iov_count].iov_len;  /* update the already converted bytes */
        assert( iov_len_local >= 0 );
        /* We compute the checksum if we have to. But we will store the temporary
         * values in the a and b variables, and only at the end take in account the
         * value of the convertor checksum.
         */
        COMPUTE_SPECIFIC_CHECKSUM( iov[iov_count].iov_base, iov[iov_count].iov_len,
                                   pConvertor->checksum );
    }
    *max_data = total_unpacked;
    *out_size = iov_count;
    if( !(pConvertor->flags & CONVERTOR_COMPLETED) ) {
        /* I complete an element, next step I should go to the next one */
        PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, DT_BYTE, count_desc,
                    user_memory_base - pStack->disp - pConvertor->pBaseBuf, pos_desc );
        DO_DEBUG( opal_output( 0, "unpack save stack stack_pos %d pos_desc %d count_desc %d disp %ld\n",
                               pConvertor->stack_pos, pStack->index, pStack->count, pStack->disp ); );
        return 0;
    }
    return 1;
}

