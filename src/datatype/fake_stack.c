/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "ompi_config.h"

#include "datatype.h"
#include "datatype_internal.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

int ompi_convertor_create_stack_with_pos( ompi_convertor_t* pConvertor,
                                          int starting_point, int* sizes );

int ompi_convertor_create_stack_with_pos( ompi_convertor_t* pConvertor,
                                          int starting_point, int* sizes )
{
    dt_stack_t* pStack;   /* pointer to the position on the stack */
    int pos_desc;         /* actual position in the description of the derived datatype */
    int type, lastLength = 0;
    long totalDisp;
    ompi_datatype_t* pData = pConvertor->pDesc;
    int* remoteLength;
    int loop_length;
    int resting_place = starting_point;
    dt_elem_desc_t* pElems;

    if( starting_point == 0 ) {
        return ompi_convertor_create_stack_at_begining( pConvertor, sizes );
    }
    /* if the convertor continue from the last position
     * there is nothing to do.
     */
    if( pConvertor->bConverted == (unsigned long)starting_point ) return OMPI_SUCCESS;

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
    loop_length = GET_FIRST_NON_LOOP( pElems );
    pStack->disp = pElems[loop_length].disp;
    pStack->count = pConvertor->count;
    pStack->index = -1;

    /* Special case for contiguous datatypes */
    if( pData->flags & DT_FLAG_CONTIGUOUS ) {
        int cnt = starting_point / pData->size;
        long extent = pData->ub - pData->lb;

        pStack->count -= cnt;
        pStack[1].index    = 0;
        pStack[1].count    = starting_point - cnt * pData->size;
        pStack[1].end_loop = pStack->end_loop;

        if( (long)pData->size == extent ) { /* all elements are contiguous */
            pStack[1].disp     = pStack->disp + starting_point;
        } else {  /* each is contiguous but there are gaps inbetween */
            pStack[1].disp = pStack->disp /* original place */
                + cnt * extent            /* the completed elements with their extent */
                + pStack[1].count;        /* what we complete from the last begining of the data */
        }
	pConvertor->bConverted = starting_point;
        pConvertor->stack_pos = 1;
        return OMPI_SUCCESS;
    }

    pos_desc  = 0;
    remoteLength = (int*)alloca( sizeof(int) * pConvertor->pDesc->btypes[DT_LOOP] );
    remoteLength[0] = 0;  /* initial value set to ZERO */

  next_loop:
    totalDisp = pStack->disp;
    loop_length = remoteLength[pConvertor->stack_pos];
    while( pos_desc >= 0 ) {
        if( pElems->type == DT_END_LOOP ) { /* end of the current loop */
            dt_endloop_desc_t* end_loop = (dt_endloop_desc_t*)pElems;
            /* now we know the length of the loop. We can compute
             * if the the starting_position will happend in one of the
             * iterations of this loop.
             */
            remoteLength[pConvertor->stack_pos] = loop_length;
            if( (loop_length * pStack->count) > resting_place ) {
                /* OK here we stop in this loop. First save the loop
                 * on the stack, then save the position of the last data
                 */
                int cnt = resting_place / loop_length;
                dt_loop_desc_t* loop = (dt_loop_desc_t*)(pElems - end_loop->items - 1);
                pStack->count -= cnt;
                resting_place -= cnt * loop_length;
                pStack->disp += cnt * loop->extent;
                pos_desc -= end_loop->items;  /* go back to the first element in the loop */
                goto next_loop;
            }
            /* Not in this loop. Cleanup the stack and advance to the
             * next data description.
             */
            pConvertor->stack_pos--;
            pStack--;
            pos_desc++;
            pElems++;
            goto next_loop;
        }
        if( pElems->type == DT_LOOP ) {
            remoteLength[pConvertor->stack_pos + 1] = 0;
            totalDisp = pElems->disp;
            PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc,
                        pData->desc.desc[pos_desc].count,
                        totalDisp, pos_desc + pElems->disp );
            pos_desc++;
            pElems++;
            loop_length = 0;  /* starting a new loop */
            goto next_loop;
        }
        /* now here we have a basic datatype */
        type = pElems->type;
        lastLength = pElems->count * ompi_ddt_basicDatatypes[type]->size;
        if( resting_place > lastLength ) {
            resting_place -= lastLength;
            loop_length += lastLength;
        } else {
            int cnt = resting_place / ompi_ddt_basicDatatypes[type]->size;
            resting_place -= cnt * ompi_ddt_basicDatatypes[type]->size;
            PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, 
                        pElems->count - cnt,
                        totalDisp + pElems->disp + cnt * pElems->extent,
                        pos_desc );
            pConvertor->bConverted += (starting_point - resting_place);
            PUSH_STACK( pStack, pConvertor->stack_pos, 0, 0, 0, 0 );
            return OMPI_SUCCESS;
        }
        pos_desc++;  /* advance to the next data */
        pElems++;
    }
    PUSH_STACK( pStack, pConvertor->stack_pos, 0, 0, 0, 0 );

    /* Correctly update the bConverted field */
    pConvertor->bConverted = starting_point - resting_place;
    return OMPI_SUCCESS;
}
