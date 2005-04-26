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
#include "datatype/datatype_internal.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

static int ompi_convertor_generic_parse( ompi_convertor_t* pConvertor, 
                                         struct iovec* iov, uint32_t* out_size, 
                                         uint32_t* max_data, 
                                         int32_t* freeAfter ) 
{
    dt_stack_t* pStack;    /* pointer to the position on the stack */ 
    uint32_t pos_desc;     /* actual position in the description of the derived datatype */ 
    int count_desc;        /* the number of items already done in the actual pos_desc */ 
    int type;              /* type at current position */ 
    long disp_desc = 0;    /* compute displacement for truncated data */ 
    int bConverted = 0;    /* number of bytes converted this time */ 
    ompi_datatype_t *pData = pConvertor->pDesc; 
    dt_elem_desc_t* pElem; 
    char* iov_base; 
    int iov_len; 
    uint32_t iov_count, total_bytes_converted = 0; 
 
    DUMP( "convertor_decode( %p, {%p, %d}, %d )\n", (void*)pConvertor, 
          iov[0].iov_base, iov[0].iov_len, *out_size ); 
 
    pElem = pConvertor->use_desc->desc;
 
    pStack = pConvertor->pStack + pConvertor->stack_pos; 
    pos_desc   = pStack->index; 
    disp_desc  = pStack->disp; 
    count_desc = pStack->count; 
    pStack--; 
    pConvertor->stack_pos--; 

    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) { 
        if( iov[iov_count].iov_base == NULL ) {
            /*
             *  ALLOCATE SOME MEMORY ...
             */
            *freeAfter = (*freeAfter) | (1 << iov_count);
        }
        iov_base = iov[iov_count].iov_base; 
        iov_len = iov[iov_count].iov_len; 
        bConverted = 0;
        while( 1 ) { 
            if( DT_END_LOOP == pElem[pos_desc].elem.common.type ) { /* end of the current loop */
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
                        assert( DT_LOOP == pElem[pStack->index].elem.common.type ); 
                        pStack->disp += pElem[pStack->index].loop.extent; 
                    } 
                } 
                count_desc = pElem[pos_desc].elem.count; 
                disp_desc = pElem[pos_desc].elem.disp; 
            } 
            if( DT_LOOP == pElem[pos_desc].elem.common.type ) { 
                do { 
                    PUSH_STACK( pStack, pConvertor->stack_pos, 
                                pos_desc, pElem[pos_desc].elem.count, 
                                pStack->disp, pos_desc + pElem[pos_desc].elem.disp + 1); 
                    pos_desc++; 
                } while( DT_LOOP == pElem[pos_desc].elem.common.type ); /* let's start another loop */
                DDT_DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElem, "advance loop" );
                /* update the current state */ 
                count_desc = pElem[pos_desc].elem.count; 
                disp_desc = pElem[pos_desc].elem.disp; 
                continue; 
            } 
            while( pElem[pos_desc].elem.common.flags & DT_FLAG_DATA ) { 
                /* now here we have a basic datatype */ 
                type = pElem[pos_desc].elem.common.type; 
                /*
                 *  DO SOMETHING USEFULL ...
                 */
                pos_desc++;  /* advance to the next data */ 
                count_desc = pElem[pos_desc].elem.count; 
                disp_desc = pElem[pos_desc].elem.disp; 
            } 
        } 
    complete_loop: 
        pConvertor->bConverted += bConverted;  /* update the already converted bytes */ 
        iov[iov_count].iov_len = bConverted;   /* update the length in the iovec */ 
        total_bytes_converted += bConverted; 
    } 
    bConverted = pConvertor->bConverted - (pData->size * pConvertor->count);
    if( bConverted ) {
        /* I complete an element, next step I should go to the next one */ 
        PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, count_desc, 
                    disp_desc, pos_desc ); 
    }
    return (bConverted == 0);
}

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
    int lastLength = 0, loop_length;
    ompi_datatype_t* pData = pConvertor->pDesc;
    int* remoteLength;
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

        loop_length = GET_FIRST_NON_LOOP( pElems );
        pStack->disp = pElems[loop_length].elem.disp;
        
        /* Special case for contiguous datatypes */
        if( pData->flags & DT_FLAG_CONTIGUOUS ) {
            int cnt = starting_point / pData->size;
            long extent = pData->ub - pData->lb;
            
            pStack->count -= cnt;
            cnt = starting_point - cnt * pData->size;  /* number of bytes after the loop */
            pStack[1].index    = 0;
            pStack[1].count    = (pElems[loop_length].elem.count * 
                                  ompi_ddt_basicDatatypes[pElems[loop_length].elem.common.type]->size) - cnt;
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
    pStack->disp = count * (pData->ub - pData->lb) + pElems[loop_length].elem.disp;

    pos_desc  = 0;
    remoteLength = (int*)alloca( sizeof(int) * (pConvertor->pDesc->btypes[DT_LOOP] + 1));
    remoteLength[0] = 0;  /* initial value set to ZERO */

    /* The only way to get out of this loop is when we reach the desired position or
     * when we finish the whole datatype.
     */
  next_loop:
    loop_length = remoteLength[pConvertor->stack_pos];
    while( pos_desc < pConvertor->pStack[0].end_loop ) {  /* protect in case when the starting_pos is bigger than the total size */
        if( DT_END_LOOP == pElems->elem.common.type ) { /* end of the current loop */
            ddt_endloop_desc_t* end_loop = (ddt_endloop_desc_t*)pElems;
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
                    assert( DT_LOOP == pElems[1 - end_loop->items].loop.common.type );
                    extent = ((ddt_loop_desc_t*)(pElems - end_loop->items + 1))->extent;
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
            if( pConvertor->stack_pos > 0 ) {
                remoteLength[pConvertor->stack_pos] += remoteLength[pConvertor->stack_pos + 1];
            }
            pos_desc++;
            pElems++;
            goto next_loop;
        }
        if( DT_LOOP == pElems->elem.common.type ) {
            remoteLength[pConvertor->stack_pos] += loop_length;
            PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc,
                        pElems->loop.loops,  /*pData->desc.desc[pos_desc].loop.loops,*/
                        pStack->disp, pos_desc + pElems->loop.items );
            remoteLength[pConvertor->stack_pos] = 0;
            pos_desc++;
            pElems++;
            loop_length = 0;  /* starting a new loop */
        }
        while( pElems->elem.common.flags & DT_FLAG_DATA ) {
            /* now here we have a basic datatype */
            const ompi_datatype_t* basic_type = BASIC_DDT_FROM_ELEM( (*pElems) );
            lastLength = pElems->elem.count * basic_type->size;
            if( resting_place < lastLength ) {
                int cnt = resting_place / basic_type->size;
                loop_length += cnt * basic_type->size;
                resting_place -= (cnt * basic_type->size);
                PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, 
                            pElems->elem.count - cnt,
                            pStack->disp + pElems->elem.disp + cnt * pElems->elem.extent,
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

void ompi_convertor_dump( ompi_convertor_t* convertor )
{
    printf( "Convertor %p count %d stack position %d bConverted %d\n", (void*)convertor,
            convertor->count, convertor->stack_pos, convertor->bConverted );
    ompi_ddt_dump( convertor->pDesc );
    printf( "Actual stack representation\n" );
    ompi_ddt_dump_stack( convertor->pStack, convertor->stack_pos,
                         convertor->pDesc->desc.desc, convertor->pDesc->name );
}
