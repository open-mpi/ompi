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

int ompi_convertor_create_stack_with_pos_general( ompi_convertor_t* pConvertor,
                                                  size_t starting_point,
                                                  const size_t* sizes );

static inline size_t
ompi_convertor_compute_remote_size( const ompi_datatype_t* pData, const size_t* sizes )
{
    uint32_t i;
    size_t length = 0;

    for( i = DT_CHAR; i < DT_MAX_PREDEFINED; i++ ) {
        length += (pData->btypes[i] * sizes[i]);
    }
    return length;
}

int ompi_convertor_create_stack_with_pos_general( ompi_convertor_t* pConvertor,
                                                  size_t starting_point, const size_t* sizes )
{
    dt_stack_t* pStack;   /* pointer to the position on the stack */
    int pos_desc;         /* actual position in the description of the derived datatype */
    size_t lastLength = 0;
    const ompi_datatype_t* pData = pConvertor->pDesc;
	size_t loop_length, *remoteLength, remote_size;
    size_t resting_place = starting_point;
    dt_elem_desc_t* pElems;
    uint32_t count;

    assert( 0 != starting_point );
    assert( pConvertor->bConverted != starting_point );
    assert( starting_point <=(pConvertor->count * pData->size) );

    /*opal_output( 0, "Data extent %d size %d count %d total_size %d starting_point %d\n",
                 pData->ub - pData->lb, pData->size, pConvertor->count,
                 pConvertor->local_size, starting_point );*/
    pConvertor->stack_pos = 0;
    pStack = pConvertor->pStack;
    /* Fill the first position on the stack. This one correspond to the
     * last fake DT_END_LOOP that we add to the data representation and
     * allow us to move quickly inside the datatype when we have a count.
     */
    pElems = pConvertor->use_desc->desc;

    if( (pConvertor->flags & CONVERTOR_HOMOGENEOUS) && (pData->flags & DT_FLAG_CONTIGUOUS) ) {
        /* Special case for contiguous datatypes */
        int32_t cnt = (int32_t)(starting_point / pData->size);
        ptrdiff_t extent = pData->ub - pData->lb;

        loop_length = GET_FIRST_NON_LOOP( pElems );
        pStack[0].disp  = pElems[loop_length].elem.disp;
        pStack[0].type  = DT_LOOP;
        pStack[0].count = pConvertor->count - cnt;
        cnt = (int32_t)(starting_point - cnt * pData->size);  /* number of bytes after the loop */
        pStack[1].index    = 0;
        pStack[1].type     = DT_BYTE;
        pStack[1].disp     = pStack[0].disp;
        pStack[1].count    = pData->size - cnt;

        if( (ptrdiff_t)pData->size == extent ) { /* all elements are contiguous */
            pStack[1].disp += starting_point;
        } else {  /* each is contiguous but there are gaps inbetween */
            pStack[1].disp += (pConvertor->count - pStack[0].count) * extent + cnt;
        }

        pConvertor->bConverted = starting_point;
        pConvertor->stack_pos = 1;
        return OMPI_SUCCESS;
    }

    /* remove from the main loop all the complete datatypes */
    remote_size    = ompi_convertor_compute_remote_size( pData, sizes );
    count          = (int32_t)(starting_point / remote_size);
    resting_place -= (remote_size * count);
    pStack->count  = pConvertor->count - count;
    pStack->index  = -1;

    loop_length = GET_FIRST_NON_LOOP( pElems );
    pStack->disp = count * (pData->ub - pData->lb) + pElems[loop_length].elem.disp;

    pos_desc  = 0;
    remoteLength = (size_t*)alloca( sizeof(size_t) * (pConvertor->pDesc->btypes[DT_LOOP] + 1));
    remoteLength[0] = 0;  /* initial value set to ZERO */
    loop_length = 0;

    /* The only way to get out of this loop is when we reach the desired position or
     * when we finish the whole datatype.
     */
    while( pos_desc < (int32_t)pConvertor->use_desc->used ) {
        if( DT_END_LOOP == pElems->elem.common.type ) { /* end of the current loop */
            ddt_endloop_desc_t* end_loop = (ddt_endloop_desc_t*)pElems;
            ptrdiff_t extent;

            if( (loop_length * pStack->count) > resting_place ) {
                /* We will stop somewhere on this loop. To avoid moving inside the loop
                 * multiple times, we can compute the index of the loop where we will
                 * stop. Once this index is computed we can then reparse the loop once
                 * until we find the correct position.
                 */
                int32_t cnt = (int32_t)(resting_place / loop_length);
                if( pStack->index == -1 ) {
                    extent = pData->ub - pData->lb;
                } else {
                    assert( DT_LOOP == (pElems - end_loop->items)->loop.common.type );
                    extent = ((ddt_loop_desc_t*)(pElems - end_loop->items))->extent;
                }
                pStack->count -= (cnt + 1);
                resting_place -= cnt * loop_length;
                pStack->disp += (cnt + 1) * extent;
                /* reset the remoteLength as we act as restarting the last loop */
                pos_desc -= (end_loop->items - 1);  /* go back to the first element in the loop */
                pElems -= (end_loop->items - 1);
                remoteLength[pConvertor->stack_pos] = 0;
                loop_length = 0;
                continue;
            }
            /* Not in this loop. Cleanup the stack and advance to the
             * next data description.
             */
            resting_place -= (loop_length * (pStack->count - 1));  /* update the resting place */
            pStack--;
            pConvertor->stack_pos--;
            pos_desc++;
            pElems++;
            remoteLength[pConvertor->stack_pos] += (loop_length * pStack->count);
            loop_length = remoteLength[pConvertor->stack_pos];
            continue;
        }
        if( DT_LOOP == pElems->elem.common.type ) {
            remoteLength[pConvertor->stack_pos] += loop_length;
            PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, DT_LOOP,
				        pElems->loop.loops, pStack->disp );
            pos_desc++;
            pElems++;
            remoteLength[pConvertor->stack_pos] = 0;
            loop_length = 0;  /* starting a new loop */
        }
        while( pElems->elem.common.flags & DT_FLAG_DATA ) {
            /* now here we have a basic datatype */
            const ompi_datatype_t* basic_type = BASIC_DDT_FROM_ELEM( (*pElems) );
            lastLength = pElems->elem.count * basic_type->size;
            if( resting_place < lastLength ) {
                int32_t cnt = (int32_t)(resting_place / basic_type->size);
                loop_length += (cnt * basic_type->size);
                resting_place -= (cnt * basic_type->size);
                PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, pElems->elem.common.type, 
                            pElems->elem.count - cnt,
                            pElems->elem.disp + cnt * pElems->elem.extent );
                pConvertor->bConverted = starting_point - resting_place;
                DDT_DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos,
                                pConvertor->pDesc->desc.desc, pConvertor->pDesc->name );
                return OMPI_SUCCESS;
            }
            loop_length += lastLength;
            resting_place -= lastLength;
            pos_desc++;  /* advance to the next data */
            pElems++;
        }
    }

    /* Correctly update the bConverted field */
    pConvertor->flags |= CONVERTOR_COMPLETED;
    pConvertor->bConverted = pConvertor->local_size;
    return OMPI_SUCCESS;
}
