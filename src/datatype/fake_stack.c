/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "lam_config.h"

#include "datatype.h"
#include "datatype_internal.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

static inline long GET_LOOP_DISP( dt_elem_desc_t* _pElem )
{
   while( _pElem->type == DT_LOOP ) ++_pElem;
   return _pElem->disp;
}

int lam_create_stack_with_pos( lam_convertor_t* pConvertor,
                               int starting_point,
                               int* sizes )
{
    long lastDisp = 0;
    dt_stack_t* pStack;   /* pointer to the position on the stack */
    int pos_desc;         /* actual position in the description of the derived datatype */
    int end_loop;         /* last element in the actual loop */
    int stack_pos = 0;
    int type, lastLength = 0, nbElems = 0, changes = 0;
    long totalDisp;
    lam_datatype_t* pData = pConvertor->pDesc;
    int* remoteLength;
    int loop_length;

    if( starting_point == 0 ) {
        dt_elem_desc_t* pElems;
        
        pConvertor->stack_pos = 1;
        pConvertor->pStack[0].index = 0;
        pConvertor->pStack[0].count = pConvertor->count;
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
        pConvertor->pStack[1].index = 0;
        pConvertor->pStack[1].count = pElems->count;
        pConvertor->pStack[1].disp = pElems->disp;
        pConvertor->pStack[1].end_loop = pConvertor->pStack[0].end_loop;
        return 0;
    }
    /* if the convertor continue from the last position
     * there is nothing to do.
     */
    if( pConvertor->bConverted != starting_point ) return 0;

    remoteLength = (int*)alloca( sizeof(int) * pConvertor->pDesc->btypes[DT_LOOP] );
    pStack = pConvertor->pStack;
    pStack->count = pConvertor->count;
    pStack->index = -1;
    pStack->end_loop = pData->desc.used - 1;
    pStack->disp = 0;
    pos_desc  = 0;
   
  next_loop:
    end_loop = pStack->end_loop;
    totalDisp = pStack->disp;
    loop_length = remoteLength[stack_pos];
    while( pos_desc <= end_loop ) {
        if( pData->desc.desc[pos_desc].type == DT_END_LOOP ) { /* end of the current loop */
            /* now we know the length of the loop. We can compute if the the starting_position
             * will happend in this loop.
             */
            remoteLength[stack_pos] = loop_length;
            stack_pos--;
            pStack--;
            pos_desc++;
            goto next_loop;
        }
        if( pData->desc.desc[pos_desc].type == DT_LOOP ) {
            dt_elem_desc_t* pEndLoop = &(pData->desc.desc[pos_desc + pData->desc.desc[pos_desc].disp]);
            long loop_disp = GET_LOOP_DISP( &(pData->desc.desc[pos_desc]) );
            remoteLength [stack_pos] = 0;
            if( pData->desc.desc[pos_desc].flags & DT_FLAG_CONTIGUOUS ) {
                /* the loop is contiguous or composed by contiguous elements with a gap */
                if( pData->desc.desc[pos_desc].extent == pEndLoop->extent ) {
                    /* the whole loop is contiguous */
                    if( (lastDisp + lastLength) != (totalDisp + loop_disp) ) {
/*                   SAVE_DESC( pElemDesc, lastDisp, lastLength ); */
                        lastLength = 0;
                        lastDisp = totalDisp + loop_disp;
                    }
                    lastLength += pData->desc.desc[pos_desc].count * pEndLoop->extent;
                } else {
                    int counter = pData->desc.desc[pos_desc].count;
                    if( (lastDisp + lastLength) == (totalDisp + loop_disp) ) {
                        lastLength += pEndLoop->extent;
                        counter--;
                    }
                    if( lastLength != 0 ) {
/*                   SAVE_DESC( pElemDesc, lastDisp, lastLength ); */
                        lastDisp += lastLength;
                        lastLength = 0;
                    }                  
                    /* we have a gap in the begining or the end of the loop but the whole
                     * loop can be merged in just one memcpy.
                     */
/*                SAVE_ELEM( pElemDesc, DT_LOOP, pData->desc.desc[pos_desc].flags, */
/*                           counter, (long)2, pData->desc.desc[pos_desc].extent ); */
/*                SAVE_DESC( pElemDesc, loop_disp, pEndLoop->extent ); */
/*                SAVE_ELEM( pElemDesc, DT_END_LOOP, pEndLoop->flags, */
/*                           2, pEndLoop->disp, pEndLoop->extent ); */
                }
                pos_desc += pData->desc.desc[pos_desc].disp + 1;
                changes++;
            } else {
                if( lastLength != 0 ) {
/*                SAVE_DESC( pElemDesc, lastDisp, lastLength ); */
                    lastDisp += lastLength;
                    lastLength = 0;
                }                  
/*             SAVE_ELEM( pElemDesc, DT_LOOP, pData->desc.desc[pos_desc].flags, */
/*                        pData->desc.desc[pos_desc].count, (long)nbElems, */
/*                        pData->desc.desc[pos_desc].extent ); */
                nbElems = 1;
                PUSH_STACK( pStack, stack_pos, pos_desc, pData->desc.desc[pos_desc].count,
                            totalDisp, pos_desc + pData->desc.desc[pos_desc].disp );
                pos_desc++;
            }
            goto next_loop;
        }
        /* now here we have a basic datatype */
        type = pData->desc.desc[pos_desc].type;
        if( (lastDisp + lastLength) == (totalDisp + pData->desc.desc[pos_desc].disp) ) {
            lastLength += pData->desc.desc[pos_desc].count * basicDatatypes[type].size;
        } else {
/*          if( lastLength != 0 ) */
/*             SAVE_DESC( pElemDesc, lastDisp, lastLength ); */
            lastDisp = totalDisp + pData->desc.desc[pos_desc].disp;
            lastLength = pData->desc.desc[pos_desc].count * basicDatatypes[type].size;
        }
        pos_desc++;  /* advance to the next data */
    }

/*    if( lastLength != 0 ) */
/*       SAVE_DESC( pElemDesc, lastDisp, lastLength ); */
    return 0;
}
