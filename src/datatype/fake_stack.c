/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "ompi_config.h"

#include "datatype.h"
#include "datatype_internal.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

int ompi_convertor_create_stack_with_pos_general( ompi_convertor_t* pConvertor,
                                                  int starting_point, int* sizes );

static inline long GET_LOOP_DISP( dt_elem_desc_t* _pElem )
{
   while( _pElem->type == DT_LOOP ) ++_pElem;
   return _pElem->disp;
}

int ompi_convertor_create_stack_with_pos_general( ompi_convertor_t* pConvertor,
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
    if( pConvertor->bConverted == starting_point ) return OMPI_SUCCESS;

    remoteLength = (int*)alloca( sizeof(int) * pConvertor->pDesc->btypes[DT_LOOP] );
    pStack = pConvertor->pStack;
    pStack->count = pConvertor->count;
    pStack->index = -1;
    pStack->end_loop = pData->desc.used;
    pStack->disp = 0;
    pos_desc  = 0;
    remoteLength[0] = 0;  /* initial value set to ZERO */
    pConvertor->stack_pos = 0;
    pElems = &(pData->desc.desc[pos_desc]);

  next_loop:
    totalDisp = pStack->disp;
    loop_length = remoteLength[pConvertor->stack_pos];
    while( pos_desc >= 0 ) {
        if( pElems->type == DT_END_LOOP ) { /* end of the current loop */
            /* now we know the length of the loop. We can compute
             * if the the starting_position will happend in one of the
             * iterations of this loop.
             */
            remoteLength[pConvertor->stack_pos] = loop_length;
            if( (loop_length * pStack->count) > resting_place ) {
                /* OK here we stop in this loop. First save the loop
                 * on the stack, then save the position of the last 
                 * data */
                int cnt = resting_place / loop_length;
                pStack->count -= cnt;
                resting_place -= cnt * loop_length;
                pStack->disp += cnt * pElems->extent;
                pConvertor->bConverted += (cnt * loop_length);
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

    return OMPI_SUCCESS;
}

/* This function works for homogeneous architectures. As we keep
 * trace of the size inside the loop in the END_LOOP element
 * we can easily jump directly where we need. It works only
 * because we can split a basic data in the middle if we
 * have a optimized representation.
 */
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
    if( pConvertor->bConverted == starting_point ) return OMPI_SUCCESS;
    if( pConvertor->flags & DT_FLAG_CONTIGUOUS ) {
        int cnt;

        cnt = starting_point / pData->size;
        pConvertor->stack_pos = 1;
        pConvertor->pStack[0].index = 0;
        pConvertor->pStack[0].count = pConvertor->count - cnt;
        pConvertor->pStack[0].disp  = 0;
        /* first here we should select which data representation will be used for
         * this operation: normal one or the optimized version ? */
        if( pData->opt_desc.used > 0 ) {
            pElems = pData->opt_desc.desc;
            pConvertor->pStack[0].end_loop = pData->opt_desc.used;
        } else {
            pElems = pData->desc.desc;
            pConvertor->pStack[0].end_loop = pData->desc.used;
        }
        cnt = starting_point - cnt * pData->size;
        pConvertor->pStack[1].index = 0;
        pConvertor->pStack[1].count = pElems->count - cnt;
        pConvertor->pStack[1].disp = pElems->disp + cnt;
        pConvertor->pStack[1].end_loop = pConvertor->pStack[0].end_loop;
	pConvertor->bConverted = starting_point;
        return OMPI_SUCCESS;
    }
    remoteLength = (int*)alloca( sizeof(int) * pConvertor->pDesc->btypes[DT_LOOP] );
    pStack = pConvertor->pStack;
    pStack->count = pConvertor->count;
    pStack->index = -1;
    pStack->end_loop = pData->desc.used;
    pStack->disp = 0;
    pos_desc  = 0;
    remoteLength[0] = 0;  /* initial value set to ZERO */
    pConvertor->stack_pos = 0;
    pElems = &(pData->desc.desc[pos_desc]);

  next_loop:
    totalDisp = pStack->disp;
    loop_length = remoteLength[pConvertor->stack_pos];
    while( pos_desc < pStack->end_loop ) {
        if( pElems->type == DT_END_LOOP ) { /* end of the current loop */
            /* now we know the length of the loop. We can compute
             * if the the starting_position will happend in one of the
             * iterations of this loop.
             */
            remoteLength[pConvertor->stack_pos] = loop_length;
            if( (loop_length * pStack->count) > resting_place ) {
                /* OK here we stop in this loop. First save the loop
                 * on the stack, then save the position of the last 
                 * data */
                int cnt = resting_place / loop_length;
                pStack->count -= cnt;
                resting_place -= cnt * loop_length;
                pStack->disp += cnt * pElems->extent;
                pConvertor->bConverted += (cnt * loop_length);
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

    return OMPI_SUCCESS;
}
