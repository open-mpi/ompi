/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "datatype.h"
#include "datatype_internal.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

int ompi_convertor_create_stack_with_pos( ompi_convertor_t* pConvertor,
                                          int starting_point, const int* sizes );

static inline size_t 
ompi_convertor_compute_remote_size( const ompi_datatype_t* pData, const int* sizes )
{
    uint32_t i;
    size_t length = 0;

    for( i = DT_CHAR; i < DT_MAX_PREDEFINED; i++ ) {
        length += (pData->btypes[i] * sizes[i]);
    }
    return length;    
}

int ompi_convertor_create_stack_with_pos_general( ompi_convertor_t* pConvertor,
                                                  int starting_point, const int* sizes )
{
    dt_stack_t* pStack;   /* pointer to the position on the stack */
    int pos_desc;         /* actual position in the description of the derived datatype */
    int type, lastLength = 0;
    ompi_datatype_t* pData = pConvertor->pDesc;
    int* remoteLength;
    int loop_length;
    int resting_place = starting_point;
    dt_elem_desc_t* pElems;
    size_t remote_size;
    uint32_t count;

    if( starting_point == 0 ) {
        return ompi_convertor_create_stack_at_begining( pConvertor, sizes );
    }
    /* if the convertor continue from the last position there is nothing to do. */
    if( pConvertor->bConverted == (unsigned long)starting_point ) return OMPI_SUCCESS;

    /* do we start after the end of the data ? */
    if( starting_point >= (int)(pConvertor->count * pData->size) ) {
        pConvertor->bConverted = pConvertor->count * pData->size;
        return OMPI_SUCCESS;
    }

    pConvertor->stack_pos = 0;
    pStack = pConvertor->pStack;
    /* Fill the first position on the stack. This one correspond to the
     * last fake DT_END_LOOP that we add to the data representation and
     * allow us to move quickly inside the datatype when we have a count.
     */
    if( pData->opt_desc.desc != NULL ) {
        pElems = pData->opt_desc.desc;
        pStack->end_loop = pData->opt_desc.used;
    } else {
        pElems = pData->desc.desc;
        pStack->end_loop = pData->desc.used;
    }
    pStack->count    = pConvertor->count;
    pStack->index    = -1;
    if( pConvertor->flags & CONVERTOR_HOMOGENEOUS ) {
        if( pData->opt_desc.desc != NULL ) {
            pElems = pData->opt_desc.desc;
            pStack->end_loop = pData->opt_desc.used;
        }

        loop_length = GET_FIRST_NON_LOOP( pElems );
        pStack->disp = pElems[loop_length].disp;
        
        /* Special case for contiguous datatypes */
        if( pData->flags & DT_FLAG_CONTIGUOUS ) {
            int cnt = starting_point / pData->size;
            long extent = pData->ub - pData->lb;
            
            pStack->count -= cnt;
            cnt = starting_point - cnt * pData->size;  /* number of bytes after the loop */
            pStack[1].index    = 0;
            pStack[1].count    = pElems[loop_length].count - cnt;
            pStack[1].end_loop = pStack->end_loop;
            
            if( (long)pData->size == extent ) { /* all elements are contiguous */
                pStack[1].disp     = pStack->disp + starting_point;
            } else {  /* each is contiguous but there are gaps inbetween */
                pStack[1].disp = pStack->disp /* original place */
                    + pStack->count * extent  /* the completed elements with their extent */ /* TODO check */
                    + pStack[1].count;        /* what we complete from the last begining of the data */
            }
            pConvertor->bConverted = starting_point;
            pConvertor->stack_pos = 1;
            return OMPI_SUCCESS;
        }
    }

    /* remove from the main loop all the complete datatypes */
    remote_size = ompi_convertor_compute_remote_size( pData, sizes );
    count = resting_place / remote_size;
    resting_place -= (remote_size * count);
    pStack->count -= count;
    pConvertor->bConverted += (remote_size * count);

    loop_length = GET_FIRST_NON_LOOP( pElems );
    pStack->disp = count * (pData->ub - pData->lb) + pElems[loop_length].disp;

    pos_desc  = 0;
    remoteLength = (int*)alloca( sizeof(int) * (pConvertor->pDesc->btypes[DT_LOOP] + 1));
    remoteLength[0] = 0;  /* initial value set to ZERO */

    /* The only way to get out of this loop is when we reach the desired position or
     * when we finish the whole datatype.
     */
  next_loop:
    loop_length = remoteLength[pConvertor->stack_pos];
    while( pos_desc >= 0 ) {
        if( pElems->type == DT_END_LOOP ) { /* end of the current loop */
            dt_endloop_desc_t* end_loop = (dt_endloop_desc_t*)pElems;
            long extent;

            pStack->count--;

            if( (loop_length * pStack->count) > resting_place ) {
                /* We will stop somewhere on this loop. To avoid moving inside the loop
                 * multiple times, we can compute the index of the loop where we will
                 * stop. Once this index is computed we can then reparse the loop once
                 * until we find the correct position.
                 */
                int cnt = resting_place / loop_length;
                if( pStack->index == -1 ) {
                    extent = pData->ub - pData->lb;
                } else {
                    extent = ((dt_loop_desc_t*)(pElems - end_loop->items + 1))->extent;
                }
                pStack->count -= cnt;
                resting_place -= cnt * loop_length;
                pStack->disp += cnt * extent;
                /* reset the remoteLength as we act as restarting the last loop */
                remoteLength[pConvertor->stack_pos] = 0;
                pos_desc -= (end_loop->items - 1);  /* go back to the first element in the loop */
                pElems = &(pData->desc.desc[pos_desc]);
                goto next_loop;
            }
            /* Not in this loop. Cleanup the stack and advance to the
             * next data description.
             */
            loop_length *= pStack->count;  /* without the initial loop */
            remoteLength[pConvertor->stack_pos] += loop_length;
            resting_place -= loop_length;  /* update the resting place */
            /* if we are embedded in another loop we should update it's length too */
            pStack--;
            pConvertor->stack_pos--;
            if( pConvertor->stack_pos >= 0 ) {
                remoteLength[pConvertor->stack_pos] += remoteLength[pConvertor->stack_pos + 1];
            }
            pos_desc++;
            pElems++;
            goto next_loop;
        }
        if( pElems->type == DT_LOOP ) {
            remoteLength[pConvertor->stack_pos] += loop_length;
            PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc,
                        pData->desc.desc[pos_desc].count,
                        pStack->disp, pos_desc + pElems->disp );
            remoteLength[pConvertor->stack_pos] = 0;
            pos_desc++;
            pElems++;
            loop_length = 0;  /* starting a new loop */
        }
        while( pElems->flags & DT_FLAG_DATA ) {
            /* now here we have a basic datatype */
            type = pElems->type;
            lastLength = pElems->count * ompi_ddt_basicDatatypes[type]->size;
            if( resting_place < lastLength ) {
                int cnt = resting_place / ompi_ddt_basicDatatypes[type]->size;
                loop_length += cnt * ompi_ddt_basicDatatypes[type]->size;
                resting_place -= (cnt * ompi_ddt_basicDatatypes[type]->size);
                PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, 
                            pElems->count - cnt,
                            pStack->disp + pElems->disp + cnt * pElems->extent,
                            pos_desc );
                pConvertor->bConverted = starting_point - resting_place;
                return OMPI_SUCCESS;
            }
            loop_length += lastLength;
            resting_place -= lastLength;
            pos_desc++;  /* advance to the next data */
            pElems++;
        }
    }

    /* Correctly update the bConverted field */
    pConvertor->bConverted = pData->size * pConvertor->count;
    return OMPI_SUCCESS;
}
