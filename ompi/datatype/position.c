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

#include "ompi/datatype/datatype.h"
#include "ompi/datatype/convertor.h"
#include "ompi/datatype/datatype_internal.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

#if OMPI_ENABLE_DEBUG
extern int ompi_position_debug;
#define DO_DEBUG(INST)  if( ompi_position_debug ) { INST }
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

static inline void position_predefined_data( ompi_convertor_t* CONVERTOR,
                                             dt_elem_desc_t* ELEM,
                                             uint32_t* COUNT,
                                             char** POINTER,
                                             uint32_t* SPACE )
{
    uint32_t _copy_count = *(COUNT), _copy_blength;
    ddt_elem_desc_t* _elem = &((ELEM)->elem);
    
    _copy_blength =  ompi_ddt_basicDatatypes[_elem->common.type]->size;
    if( (_copy_count * _copy_blength) > *(SPACE) ) {
        _copy_count = *(SPACE) / _copy_blength;
        if( 0 == _copy_count ) return;  /* nothing to do */
    }
    _copy_blength *= _copy_count;
    
    OMPI_DDT_SAFEGUARD_POINTER( *(POINTER) + _elem->disp, _copy_blength, (CONVERTOR)->pBaseBuf,
                                (CONVERTOR)->pDesc, (CONVERTOR)->count );
    *(POINTER) += (_copy_count * _elem->extent);
    *(SPACE)   -= _copy_blength;
    *(COUNT)   -= _copy_count;
}

static inline void position_contiguous_loop( ompi_convertor_t* CONVERTOR,
                                             dt_elem_desc_t* ELEM,
                                             uint32_t* COUNT,
                                             char** POINTER,
                                             uint32_t* SPACE )
{
    ddt_loop_desc_t *_loop = (ddt_loop_desc_t*)(ELEM);
    ddt_endloop_desc_t* _end_loop = (ddt_endloop_desc_t*)((ELEM) + (ELEM)->loop.items);
    size_t _copy_loops = *(COUNT);
    
    if( (_copy_loops * _end_loop->size) > *(SPACE) )
        _copy_loops = *(SPACE) / _end_loop->size;
    OMPI_DDT_SAFEGUARD_POINTER( *(POINTER) + _end_loop->first_elem_disp,
                                (_copy_loops - 1) * _loop->extent + _end_loop->size,
                                (CONVERTOR)->pBaseBuf, (CONVERTOR)->pDesc, (CONVERTOR)->count );
    *(POINTER) += _copy_loops * _loop->extent;
    *(SPACE)   -= _copy_loops * _end_loop->size;
    *(COUNT)   -= _copy_loops;
}

#define POSITION_PREDEFINED_DATATYPE( CONVERTOR, ELEM, COUNT, POSITION, SPACE ) \
    position_predefined_data( (CONVERTOR), (ELEM), &(COUNT), &(POSITION), &(SPACE) )

#define POSITION_CONTIGUOUS_LOOP( CONVERTOR, ELEM, COUNT, POSITION, SPACE ) \
    position_contiguous_loop( (CONVERTOR), (ELEM), &(COUNT), &(POSITION), &(SPACE) )

#define UPDATE_INTERNAL_COUNTERS( DESCRIPTION, POSITION, ELEMENT, COUNTER ) \
    do {                                                                \
        (ELEMENT) = &((DESCRIPTION)[(POSITION)]);                       \
        (COUNTER) = (ELEMENT)->elem.count;                              \
    } while (0)

int ompi_convertor_generic_simple_position( ompi_convertor_t* pConvertor,
                                            size_t* position )
{
    dt_stack_t* pStack;       /* pointer to the position on the stack */
    uint32_t pos_desc;        /* actual position in the description of the derived datatype */
    uint32_t count_desc;      /* the number of items already done in the actual pos_desc */
    uint16_t type;            /* type at current position */
    dt_elem_desc_t* description;
    dt_elem_desc_t* pElem;
    char *base_pointer;
    uint32_t iov_len_local, required_space = 0;


    DUMP( "ompi_convertor_generic_simple_pack( %p, &%ld )\n",
          (void*)pConvertor, (long)*position );

    description = pConvertor->use_desc->desc;

    base_pointer = pConvertor->pBaseBuf;

    /* We dont want to have to parse the datatype multiple times. What we are interested in
     * here is to compute the number of completed datatypes that we can move forward, update
     * the the counters and finally compute the position taking in account only the remaining
     * elements. The only problem is that we have to modify all the elements on the stack.
     */
    iov_len_local = *position - pConvertor->bConverted;
    if( iov_len_local > pConvertor->pDesc->size ) {
        long extent = pConvertor->pDesc->ub - pConvertor->pDesc->lb;

        count_desc = iov_len_local / pConvertor->pDesc->size;
        for( type = 0; type <= pConvertor->stack_pos; type++ )
            pConvertor->pStack[type].disp += extent;
        pConvertor->bConverted += count_desc * pConvertor->pDesc->size;
        iov_len_local = *position - pConvertor->bConverted;
        pConvertor->pStack[0].count -= count_desc;
    }

    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc      = pStack->index;
    base_pointer += pStack->disp;
    count_desc    = pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &(description[pos_desc]); 
    base_pointer += pStack->disp;
    
    DO_DEBUG( opal_output( 0, "unpack start pos_desc %d count_desc %d disp %ld\n"
                           "stack_pos %d pos_desc %d count_desc %d disp %ld\n",
                           pos_desc, count_desc, base_pointer - pConvertor->pBaseBuf,
                           pConvertor->stack_pos, pStack->index, pStack->count, pStack->disp ); );

    while( 1 ) {
        if( DT_END_LOOP == pElem->elem.common.type ) { /* end of the current loop */
            DO_DEBUG( opal_output( 0, "pack end_loop count %d stack_pos %d pos_desc %d disp %ld space %d\n",
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
                if( pStack->index == -1 ) {
                    pStack->disp += (pConvertor->pDesc->ub - pConvertor->pDesc->lb);
                } else {
                    assert( DT_LOOP == description[pStack->index].loop.common.type );
                    pStack->disp += description[pStack->index].loop.extent;
                }
            }
            base_pointer = pConvertor->pBaseBuf + pStack->disp;
            UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
            DO_DEBUG( opal_output( 0, "pack new_loop count %d stack_pos %d pos_desc %d disp %ld space %d\n",
                                   pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp, iov_len_local ); );
        }
        if( DT_LOOP == pElem->elem.common.type ) {
            long local_disp = (long)base_pointer;
            if( pElem->loop.common.flags & DT_FLAG_CONTIGUOUS ) {
                POSITION_CONTIGUOUS_LOOP( pConvertor, pElem, count_desc,
                                          base_pointer, iov_len_local );
                if( 0 == count_desc ) {  /* completed */
                    pos_desc += pElem->loop.items + 1;
                    goto update_loop_description;
                }
                /* Save the stack with the correct last_count value. */
            }
            local_disp = (long)base_pointer - local_disp;
            PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, DT_LOOP, count_desc,
                        pStack->disp + local_disp, pos_desc + pElem->elem.disp + 1);
            pos_desc++;
        update_loop_description:  /* update the current state */
            base_pointer = pConvertor->pBaseBuf + pStack->disp;
            UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
            DDT_DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElem, "advance loop" );
            continue;
        }
        while( pElem->elem.common.flags & DT_FLAG_DATA ) {
            /* now here we have a basic datatype */
            POSITION_PREDEFINED_DATATYPE( pConvertor, pElem, count_desc,
                                          base_pointer, iov_len_local );
            if( 0 != count_desc ) {  /* completed */
                type = pElem->elem.common.type;
                required_space = ompi_ddt_basicDatatypes[type]->size;
                goto complete_loop;
            }
            base_pointer = pConvertor->pBaseBuf + pStack->disp;
            pos_desc++;  /* advance to the next data */
            UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
        }
    }
 complete_loop:
    (*position) -= iov_len_local;
    pConvertor->bConverted = *position;  /* update the already converted bytes */
    if( (pConvertor->pending_length != iov_len_local) &&
        (pConvertor->flags & CONVERTOR_RECV) ) {
        opal_output( 0, "Missing some data ?" );
    }
    if( !(pConvertor->flags & CONVERTOR_COMPLETED) ) {
        /* I complete an element, next step I should go to the next one */
        PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, DT_BYTE, count_desc,
                    base_pointer - pStack->disp - pConvertor->pBaseBuf, pos_desc );
        DO_DEBUG( opal_output( 0, "pack save stack stack_pos %d pos_desc %d count_desc %d disp %ld\n",
                               pConvertor->stack_pos, pStack->index, pStack->count, pStack->disp ); );
        return 0;
    }
    return 1;
}

