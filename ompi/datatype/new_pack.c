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

#define DO_DEBUG(INST)  /* output disabled */

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

#define PACK_PREDEFINED_DATATYPE( CONVERTOR,    /* the convertor */     \
                                  TYPE,         /* the basic type to be packed */ \
                                  COUNT,        /* the number of elements */ \
                                  EXTENT,       /* the extent in bytes of each element */ \
                                  SOURCE,       /* the source pointer (char*) */ \
                                  DESTINATION,  /* the destination pointer (char*) */ \
                                  SPACE )       /* the space in the destination buffer */ \
     do {                             \
         uint32_t _copy_count = (COUNT), _copy_blength;                 \
                                                                        \
         if( (_copy_count * ompi_ddt_basicDatatypes[(TYPE)]->size) > (SPACE) ) \
             _copy_count = (SPACE) / ompi_ddt_basicDatatypes[type]->size; \
         _copy_blength = _copy_count * ompi_ddt_basicDatatypes[type]->size; \
                                                                        \
         if( ompi_ddt_basicDatatypes[type]->size == (uint32_t)(EXTENT) ) { \
             /* the extent and the size of the basic datatype are equals */ \
             OMPI_DDT_SAFEGUARD_POINTER( (SOURCE), _copy_blength, (CONVERTOR)->pBaseBuf, \
                                         (CONVERTOR)->pDesc, (CONVERTOR)->count ); \
             DO_DEBUG( opal_output( 0, "1. memcpy( %p, %p, %ld )\n",    \
                                    (DESTINATION), (SOURCE),            \
                                    _copy_blength ); );                 \
             MEMCPY( (DESTINATION), (SOURCE), _copy_blength );          \
             (SOURCE) += _copy_blength;                                 \
             (DESTINATION) += _copy_blength;                            \
         } else {                                                       \
             uint32_t _i;                                               \
             for( _i = 0; _i < _copy_count; _i++ ) {                    \
                 OMPI_DDT_SAFEGUARD_POINTER( (SOURCE), _copy_blength, (CONVERTOR)->pBaseBuf, \
                                             (CONVERTOR)->pDesc, (CONVERTOR)->count ); \
                 DO_DEBUG( opal_output( 0, "2. memcpy( %p, %p, %ld )\n", \
                                        (DESTINATION), (SOURCE), _copy_blength ); ); \
                 MEMCPY( (DESTINATION), (SOURCE), ompi_ddt_basicDatatypes[type]->size ); \
                 (DESTINATION) += ompi_ddt_basicDatatypes[type]->size;  \
                 (SOURCE) += (EXTENT);                                  \
             }                                                          \
         }                                                              \
         (SPACE) -= _copy_blength;                                      \
         (COUNT) -= _copy_count;                                        \
     } while (0)

#define PACK_CONTIGUOUS_LOOP( CONVERTOR,    /*   */ \
                              ELEM,         /*   */ \
                              COUNT,        /*   */ \
                              SOURCE,       /*   */ \
                              DESTINATION,  /*   */ \
                              SPACE )       /*   */ \
    do {                                                                \
        ddt_loop_desc_t *loop = (ddt_loop_desc_t*)(ELEM);               \
        ddt_endloop_desc_t* end_loop = (ddt_endloop_desc_t*)((ELEM) + (ELEM)->loop.items); \
        size_t _copy_loops = (COUNT);                                   \
        uint32_t _i;                                                    \
                                                                        \
        if( (_copy_loops * end_loop->size) > (SPACE) )                  \
            _copy_loops = (SPACE) / end_loop->size;                     \
        for( _i = 0; _i < _copy_loops; _i++ ) {                         \
            OMPI_DDT_SAFEGUARD_POINTER( (SOURCE),                       \
                                        end_loop->size, (CONVERTOR)->pBaseBuf, \
                                        (CONVERTOR)->pDesc, (CONVERTOR)->count ); \
            DO_DEBUG( opal_output( 0, "3. memcpy( %p, %p, %ld )\n",     \
                                   (DESTINATION), (SOURCE),             \
                                   end_loop->size ); );                 \
            MEMCPY( (DESTINATION), (SOURCE), end_loop->size );          \
            (DESTINATION) += end_loop->size;                            \
            (SOURCE) += loop->extent;                                   \
        }                                                               \
        (SPACE) -= _copy_loops * end_loop->size;                        \
        (COUNT) -= _copy_loops;                                         \
    } while (0)

#define UPDATE_INTERNAL_COUNTERS( DESCRIPTION, POSITION, ELEMENT, COUNTER, DISPLACEMENT ) \
    do {                                                                \
        (ELEMENT) = &((DESCRIPTION)[(POSITION)]);                       \
        (COUNTER) = (ELEMENT)->elem.count;                              \
        (DISPLACEMENT)  = (ELEMENT)->elem.disp;                         \
    } while (0)

int ompi_convertor_generic_simple_pack( ompi_convertor_t* pConvertor,
                                        struct iovec* iov, uint32_t* out_size,
                                        size_t* max_data,
                                        int32_t* freeAfter )
{
    dt_stack_t* pStack;       /* pointer to the position on the stack */
    uint32_t pos_desc;        /* actual position in the description of the derived datatype */
    uint32_t count_desc;      /* the number of items already done in the actual pos_desc */
    uint16_t type;            /* type at current position */
    long disp_desc = 0;       /* compute displacement for truncated data */
    dt_elem_desc_t* description;
    dt_elem_desc_t* pElem;
    const ompi_datatype_t *pData = pConvertor->pDesc;
    char *source_base, *source, *destination;
    uint32_t iov_len_local, iov_count;

    DUMP( "ompi_convertor_generic_simple_pack( %p, {%p, %d}, %d )\n", (void*)pConvertor,
          iov[0].iov_base, iov[0].iov_len, *out_size );

    description = pConvertor->use_desc->desc;

    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc   = pStack->index;
    disp_desc  = pStack->disp;
    count_desc = pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &(description[pos_desc]); 
    source_base = pConvertor->pBaseBuf;

    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {
        if( iov[iov_count].iov_base == NULL ) {
            /*
             *  ALLOCATE SOME MEMORY ...
             */
            uint32_t length = iov[iov_count].iov_len;
            if( length <= 0 )
                length = pConvertor->count * pData->size - pConvertor->bConverted;
            if( (*max_data) < length )
                length = *max_data;
            iov[iov_count].iov_len = length;
            iov[iov_count].iov_base = pConvertor->memAlloc_fn( &(iov[iov_count].iov_len),
                                                               pConvertor->memAlloc_userdata );
            *freeAfter = (*freeAfter) | (1 << iov_count);
        }
        destination = iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;
        while( 1 ) {
            if( DT_END_LOOP == pElem->elem.common.type ) { /* end of the current loop */
                if( --(pStack->count) == 0 ) { /* end of loop */
                    if( pConvertor->stack_pos == 0 )
                        goto complete_loop;  /* completed */
                    pConvertor->stack_pos--;
                    pStack--;
                    pos_desc++;
                } else {
                    pos_desc = pStack->index + 1;
                    if( pStack->index == -1 ) {
                        pStack->disp += (pData->ub - pData->lb);
                    } else {
                        assert( DT_LOOP == description[pStack->index].elem.common.type );
                        pStack->disp += description[pStack->index].loop.extent;
                    }
                }
                source_base = pConvertor->pBaseBuf + pStack->disp;
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc, disp_desc );
            }
            if( DT_LOOP == pElem->elem.common.type ) {
                if( pElem->loop.common.flags & DT_FLAG_CONTIGUOUS ) {
                    PACK_CONTIGUOUS_LOOP( pConvertor, pElem, count_desc, 
                                          source, destination, iov_len_local );
                    if( 0 == count_desc ) {  /* completed */
                        pos_desc += pElem->loop.items + 1;
                        goto update_loop_description;
                    }
                    /* Save the stack with the correct last_count value. */
                }
                PUSH_STACK( pStack, pConvertor->stack_pos,
                            pos_desc, DT_LOOP, count_desc,
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
                if( 0 == iov_len_local ) goto complete_loop;
                type = pElem->elem.common.type;
                source = source_base + disp_desc;
                PACK_PREDEFINED_DATATYPE( pConvertor, type, count_desc, pElem->elem.extent,
                                          source, destination, iov_len_local );
                if( 0 != count_desc ) {  /* completed */
                    goto complete_loop;
                }
                pos_desc++;  /* advance to the next data */
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc, disp_desc );
            }
        }
    complete_loop:
        iov[iov_count].iov_len -= iov_len_local;  /* update the amount of valid data */
        pConvertor->bConverted += iov[iov_count].iov_len;  /* update the already converted bytes */
        assert( iov_len_local >= 0 );
    }
    if( pConvertor->bConverted != (pData->size * pConvertor->count) ) {
        /* I complete an element, next step I should go to the next one */
        PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, DT_BYTE, count_desc,
                    source_base - pConvertor->pBaseBuf, pos_desc );
        return 0;
    }
    return 1;
}

