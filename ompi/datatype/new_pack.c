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

#define PACK_PREDEFINED_DATATYPE( TYPE,         /* the basic type to be packed */          \
                                  COUNT,        /* the number of elements */               \
                                  EXTENT,       /* the extent in bytes of each element */  \
                                  SOURCE,       /* the source pointer (char*) */           \
                                  DESTINATION,  /* the destination pointer (char*) */      \
                                  SPACE )       /* the space in the destination buffer */  \
do {                                                                                       \
    int copy_count = (COUNT), copy_blength;                                                \
                                                                                           \
    if( (copy_count * ompi_ddt_basicDatatypes[(TYPE)]->size) > (SPACE) )                   \
        copy_count = (SPACE) / ompi_ddt_basicDatatypes[type]->size;                        \
    copy_blength = copy_count * ompi_ddt_basicDatatypes[type]->size;                       \
                                                                                           \
    if( ompi_ddt_basicDatatypes[type]->size == (EXTENT) ) {                                \
        memcpy( (DESTINATION), (SOURCE), copy_blength );                                   \
        (SOURCE) += copy_blength;                                                          \
        (DESTINATION) += copy_blength;                                                     \
    } else {                                                                               \
        int i;                                                                             \
        for( i = 0; i < copy_count; i++ ) {                                                \
            memcpy( (DESTINATION), (SOURCE), ompi_ddt_basicDatatypes[type]->size );        \
            (DESTINATION) += ompi_ddt_basicDatatypes[type]->size;                          \
            (SOURCE) += (EXTENT);                                                          \
        }                                                                                  \
    }                                                                                      \
    (SPACE) -= copy_blength;                                                               \
    (COUNT) -= copy_count;                                                                 \
} while (0)

#define PACK_CONTIGUOUS_LOOP( CONVERTOR,    /*   */ \
                              ELEM,         /*   */ \
                              COUNT,        /*   */ \
                              SOURCE,       /*   */ \
                              DESTINATION,  /*   */ \
                              SPACE )       /*   */ \
do {                                                \
    ddt_loop_desc_t *loop = (ddt_loop_desc_t*)(ELEM);                                     \
    ddt_endloop_desc_t* end_loop = (ddt_endloop_desc_t*)((ELEM) + (ELEM).loop.items);     \
    size_t copy_loops = (COUNT); \
    int i; \
\
    if( (copy_loops * end_loop->size) > (SPACE) ) \
         copy_loops = (SPACE) / end_loop->size; \
    assert( loop->extent != end_loop->size ); \
    for( i = 0; i < copy_loops; i++ ) { \
        OMPI_DDT_SAFEGUARD_POINTER( (CONVERTOR)->pBaseBuf + lastDisp, end_loop->size, \
                                    (CONVERTOR)->pBaseBuf, (CONVERTOR)->pDesc, (CONVERTOR)->count ); \
        DO_DEBUG (ompi_output( 0, "2. memcpy( %p, %p, %ld )\n", pDestBuf, (CONVERTOR)->pBaseBuf + lastDisp,  \
                               end_loop->size ); ); \
        MEMCPY( pDestBuf, (CONVERTOR)->pBaseBuf + lastDisp, end_loop->size ); \
        lastDisp += loop->extent;  \
        (DESTINATION) += end_loop->size;  \
        (SOURCE) += loop->extent; \
    } \
    (SPACE) -= copy_count * end_loop->size; \
} while (0)

#define UPDATE_INTERNAL_COUNTERS( DESCRIPTION, POSITION, ELEMENT, COUNTER, DISPLACEMENT ) \
do { \
   (ELEMENT) = &((DESCRIPTION)[(POSITION)]); \
   (COUNTER) = (ELEMENT)->elem.count;        \
   (DISPLACEMENT)  = (ELEMENT)->elem.disp;   \
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
    uint32_t bConverted = 0;  /* number of bytes converted this time */
    dt_elem_desc_t* description;
    dt_elem_desc_t* pElem;
    const ompi_datatype_t *pData = pConvertor->pDesc;
    char* iov_base_local;
    uint32_t iov_len_local, i, iov_count;

    DUMP( "ompi_convertor_generic_simple_pack( %p, {%p, %d}, %d )\n", (void*)pConvertor,
          iov[0].iov_base, iov[0].iov_len, *out_size );

    description = pConvertor->use_desc->desc;

    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc   = pStack->index;
    disp_desc  = pStack->disp;
    count_desc = pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem =&(description[pos_desc]); 

    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {
        if( iov[iov_count].iov_base == NULL ) {
            /*
             *  ALLOCATE SOME MEMORY ...
             */
            *freeAfter = (*freeAfter) | (1 << iov_count);
        }
        iov_base_local = iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;
        bConverted = 0;
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
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc, disp_desc );
            }
            if( DT_LOOP == pElem->elem.common.type ) {
                int stop_in_loop = 0;
                if( pElem->loop.common.flags & DT_FLAG_CONTIGUOUS ) {
                    ddt_endloop_desc_t* end_loop = &(description[pos_desc + pElem->loop.items].end_loop);
                    if( (end_loop->size * count_desc) > iov_len_local ) {
                        stop_in_loop = count_desc;
                        count_desc = iov_len_local / end_loop->size;
                    }
                    for( i = 0; i < count_desc; i++ ) {
                        /*
                         *  DO SOMETHING USEFULL ...
                         */
                        iov_base_local += end_loop->size;  /* size of the contiguous data */
                        disp_desc += pElem->loop.extent;
                    }
                    iov_len_local -= (end_loop->size * count_desc);
                    bConverted += (end_loop->size * count_desc);
                    if( stop_in_loop == 0 ) {
                        pos_desc += pElem->loop.items + 1;
                        goto update_loop_description;
                    }
                    /* mark some of the iterations as completed */
                    count_desc = stop_in_loop - count_desc;
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
                type = pElem->elem.common.type;
                if( pElem->elem.common.flags & DT_FLAG_CONTIGUOUS ) {
                    /* the extent and the size of the basic datatype are equals */
                    /*
                     *  DO SOMETHING USEFULL ...
                     */
                } else {
                    /* the extent and the size of the basic datatype are differents */
                    /*
                     *  DO SOMETHING USEFULL ...
                     */
                }
                pos_desc++;  /* advance to the next data */
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc, disp_desc );
            }
        }
    complete_loop:
        pConvertor->bConverted += bConverted;  /* update the already converted bytes */
        assert( bConverted <= iov[iov_count].iov_len );
        iov[iov_count].iov_len = bConverted;   /* update the length in the iovec */
    }
    if( pConvertor->bConverted != (pData->size * pConvertor->count) ) {
        /* I complete an element, next step I should go to the next one */
        PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, DT_BYTE, count_desc,
                    disp_desc, pos_desc );
        return 0;
    }
    return 1;
}

