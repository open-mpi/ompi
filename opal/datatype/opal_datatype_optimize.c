/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2019 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stddef.h>
#include <stdlib.h>

#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype_internal.h"

static int32_t
opal_datatype_optimize_short( opal_datatype_t* pData,
                              size_t count,
                              dt_type_desc_t* pTypeDesc )
{
    dt_elem_desc_t* pElemDesc;
    dt_stack_t *pOrigStack, *pStack; /* pointer to the position on the stack */
    int32_t pos_desc = 0;            /* actual position in the description of the derived datatype */
    int32_t stack_pos = 0;
    int32_t nbElems = 0;
    ptrdiff_t total_disp = 0;
    ddt_elem_desc_t last = {.common.flags = 0xFFFF /* all on */, .count = 0, .disp = 0}, compress;
    ddt_elem_desc_t* current;

    pOrigStack = pStack = (dt_stack_t*)malloc( sizeof(dt_stack_t) * (pData->loops+2) );
    SAVE_STACK( pStack, -1, 0, count, 0 );

    pTypeDesc->length = 2 * pData->desc.used + 1 /* for the fake OPAL_DATATYPE_END_LOOP at the end */;
    pTypeDesc->desc = pElemDesc = (dt_elem_desc_t*)malloc( sizeof(dt_elem_desc_t) * pTypeDesc->length );
    pTypeDesc->used = 0;

    assert( OPAL_DATATYPE_END_LOOP == pData->desc.desc[pData->desc.used].elem.common.type );

    while( stack_pos >= 0 ) {
        if( OPAL_DATATYPE_END_LOOP == pData->desc.desc[pos_desc].elem.common.type ) { /* end of the current loop */
            ddt_endloop_desc_t* end_loop = &(pData->desc.desc[pos_desc].end_loop);
            if( 0 != last.count ) {
                CREATE_ELEM( pElemDesc, last.common.type, OPAL_DATATYPE_FLAG_BASIC,
                             last.blocklen, last.count, last.disp, last.extent );
                pElemDesc++; nbElems++;
                last.count= 0;
            }
            CREATE_LOOP_END( pElemDesc, nbElems - pStack->index + 1,  /* # of elems in this loop */
                             end_loop->first_elem_disp, end_loop->size, end_loop->common.flags );
            if( --stack_pos >= 0 ) {  /* still something to do ? */
                ddt_loop_desc_t* pStartLoop = &(pTypeDesc->desc[pStack->index - 1].loop);
                pStartLoop->items = pElemDesc->end_loop.items;
                total_disp = pStack->disp;  /* update the displacement position */
            }
            pElemDesc++; nbElems++;
            pStack--;  /* go down one position on the stack */
            pos_desc++;
            continue;
        }
        if( OPAL_DATATYPE_LOOP == pData->desc.desc[pos_desc].elem.common.type ) {
            ddt_loop_desc_t* loop = (ddt_loop_desc_t*)&(pData->desc.desc[pos_desc]);
            int index = GET_FIRST_NON_LOOP( &(pData->desc.desc[pos_desc]) );

            if( loop->common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS ) {
                ddt_endloop_desc_t* end_loop = (ddt_endloop_desc_t*)&(pData->desc.desc[pos_desc + loop->items]);

                assert(pData->desc.desc[pos_desc + index].elem.disp == end_loop->first_elem_disp);
                compress.common.flags = loop->common.flags;
                compress.common.type =  pData->desc.desc[pos_desc + index].elem.common.type;
                compress.blocklen = pData->desc.desc[pos_desc + index].elem.blocklen;
                for( uint32_t i = index+1; i < loop->items; i++ ) {
                    current = &pData->desc.desc[pos_desc + i].elem;
                    assert(1 ==  current->count);
                    if( (current->common.type == OPAL_DATATYPE_LOOP) ||
                        compress.common.type != current->common.type ) {
                        compress.common.type = OPAL_DATATYPE_UINT1;
                        compress.blocklen = end_loop->size;
                        break;
                    }
                    compress.blocklen += current->blocklen;
                }
                compress.count = loop->loops;
                compress.extent = loop->extent;
                compress.disp = end_loop->first_elem_disp;
                if( compress.extent == (ptrdiff_t)(compress.blocklen * opal_datatype_basicDatatypes[compress.common.type]->size) ) {
                    /* The compressed element is contiguous: collapse it into a single large blocklen */
                    compress.blocklen *= compress.count;
                    compress.extent   *= compress.count;
                    compress.count     = 1;
                }
                /**
                 * The current loop has been compressed and can now be treated as if it
                 * was a data element. We can now look if it can be fused with last,
                 * as done in the fusion of 2 elements below. Let's use the same code.
                 */
                pos_desc += loop->items + 1;
                current = &compress;
                goto fuse_loops;
            }

            /**
             * If the content of the loop is not contiguous there is little we can do
             * that would not incur significant optimization cost and still be beneficial
             * in reducing the number of memcpy during pack/unpack.
             */

            if( 0 != last.count ) {  /* Generate the pending element */
                CREATE_ELEM( pElemDesc, last.common.type, OPAL_DATATYPE_FLAG_BASIC,
                             last.blocklen, last.count, last.disp, last.extent );
                pElemDesc++; nbElems++;
                last.count       = 0;
                last.common.type = OPAL_DATATYPE_LOOP;
            }

            /* Can we unroll the loop? */
            if( (loop->items <= 3) && (loop->loops <= 2) ) {
                ptrdiff_t elem_displ = 0;
                for( uint32_t i = 0; i < loop->loops; i++ ) {
                    for( uint32_t j = 0; j < (loop->items - 1); j++ ) {
                        current = &pData->desc.desc[pos_desc + index + j].elem;
                        CREATE_ELEM( pElemDesc, current->common.type, current->common.flags,
                                     current->blocklen, current->count, current->disp + elem_displ, current->extent );
                        pElemDesc++; nbElems++;
                    }
                    elem_displ += loop->extent;
                }
                pos_desc += loop->items + 1;
                goto complete_loop;
            }

            CREATE_LOOP_START( pElemDesc, loop->loops, loop->items, loop->extent, loop->common.flags );
            pElemDesc++; nbElems++;
            PUSH_STACK( pStack, stack_pos, nbElems, OPAL_DATATYPE_LOOP, loop->loops, total_disp );
            pos_desc++;
            DDT_DUMP_STACK( pStack, stack_pos, pData->desc.desc, "advance loops" );

        complete_loop:
            total_disp = pStack->disp;  /* update the displacement */
            continue;
        }
        while( pData->desc.desc[pos_desc].elem.common.flags & OPAL_DATATYPE_FLAG_DATA ) {  /* go over all basic datatype elements */
            current = &pData->desc.desc[pos_desc].elem;
            pos_desc++;  /* point to the next element as current points to the current one */

          fuse_loops:
            if( 0 == last.count ) {  /* first data of the datatype */
                last = *current;
                continue;  /* next data */
            } else {  /* can we merge it in order to decrease count */
                if( (ptrdiff_t)last.blocklen * (ptrdiff_t)opal_datatype_basicDatatypes[last.common.type]->size == last.extent ) {
                    last.extent *= last.count;
                    last.blocklen *= last.count;
                    last.count = 1;
                }
            }

            /* are the two elements compatible: aka they have very similar values and they
             * can be merged together by increasing the count, and/or changing the extent.
             */
            if( (last.blocklen * opal_datatype_basicDatatypes[last.common.type]->size) ==
                (current->blocklen * opal_datatype_basicDatatypes[current->common.type]->size) ) {
                ddt_elem_desc_t save = last;  /* safekeep the type and blocklen */
                if( last.common.type != current->common.type ) {
                    last.blocklen    *= opal_datatype_basicDatatypes[last.common.type]->size;
                    last.common.type  = OPAL_DATATYPE_UINT1;
                }

                if( (last.extent * (ptrdiff_t)last.count + last.disp) == current->disp ) {
                    if( 1 == current->count ) {
                        last.count++;
                        continue;
                    }
                    if( last.extent == current->extent ) {
                        last.count += current->count;
                        continue;
                    }
                }
                if( 1 == last.count ) {
                    /* we can ignore the extent of the element with count == 1 and merge them together if their displacements match */
                    if( 1 == current->count ) {
                        last.extent = current->disp - last.disp;
                        last.count++;
                        continue;
                    }
                    /* can we compute a matching displacement ? */
                    if( (last.disp + current->extent) == current->disp ) {
                        last.extent = current->extent;
                        last.count = current->count + last.count;
                        continue;
                    }
                }
                last.blocklen = save.blocklen;
                last.common.type = save.common.type;
                /* try other optimizations */
            }
            /* are the elements fusionable such that we can fusion the last blocklen of one with the first
             * blocklen of the other.
             */
            if( (ptrdiff_t)(last.disp + (last.count - 1) * last.extent + last.blocklen * opal_datatype_basicDatatypes[last.common.type]->size) ==
                current->disp ) {
                if( last.count != 1 ) {
                    CREATE_ELEM( pElemDesc, last.common.type, OPAL_DATATYPE_FLAG_BASIC,
                                 last.blocklen, last.count - 1, last.disp, last.extent );
                    pElemDesc++; nbElems++;
                    last.disp += (last.count - 1) * last.extent;
                    last.count = 1;
                }
                if( last.common.type == current->common.type ) {
                    last.blocklen += current->blocklen;
                } else {
                    last.blocklen = ((last.blocklen * opal_datatype_basicDatatypes[last.common.type]->size) +
                                     (current->blocklen * opal_datatype_basicDatatypes[current->common.type]->size));
                    last.common.type = OPAL_DATATYPE_UINT1;
                }
                last.extent += current->extent;
                if( current->count != 1 ) {
                    CREATE_ELEM( pElemDesc, last.common.type, OPAL_DATATYPE_FLAG_BASIC,
                                 last.blocklen, last.count, last.disp, last.extent );
                    pElemDesc++; nbElems++;
                    last = *current;
                    last.count -= 1;
                    last.disp += last.extent;
                }
                continue;
            }
            CREATE_ELEM( pElemDesc, last.common.type, OPAL_DATATYPE_FLAG_BASIC,
                         last.blocklen, last.count, last.disp, last.extent );
            pElemDesc++; nbElems++;
            last = *current;
        }
    }

    if( 0 != last.count ) {
        CREATE_ELEM( pElemDesc, last.common.type, OPAL_DATATYPE_FLAG_BASIC,
                     last.blocklen, last.count, last.disp, last.extent );
        pElemDesc++; nbElems++;
    }
    /* cleanup the stack */
    pTypeDesc->used = nbElems - 1;  /* except the last fake END_LOOP */
    free(pOrigStack);
    return OPAL_SUCCESS;
}

int32_t opal_datatype_commit( opal_datatype_t * pData )
{
    ddt_endloop_desc_t* pLast = &(pData->desc.desc[pData->desc.used].end_loop);
    ptrdiff_t first_elem_disp = 0;

    if( pData->flags & OPAL_DATATYPE_FLAG_COMMITTED ) return OPAL_SUCCESS;
    pData->flags |= OPAL_DATATYPE_FLAG_COMMITTED;

    /* We have to compute the displacement of the first non loop item in the
     * description.
     */
    if( 0 != pData->size ) {
        int index;
        dt_elem_desc_t* pElem = pData->desc.desc;

        index = GET_FIRST_NON_LOOP( pElem );
        assert( pElem[index].elem.common.flags & OPAL_DATATYPE_FLAG_DATA );
        first_elem_disp = pElem[index].elem.disp;
    }

    /* let's add a fake element at the end just to avoid useless comparaisons
     * in pack/unpack functions.
     */
    pLast->common.type     = OPAL_DATATYPE_END_LOOP;
    pLast->common.flags    = 0;
    pLast->items           = pData->desc.used;
    pLast->first_elem_disp = first_elem_disp;
    pLast->size            = pData->size;

    /* If there is no datatype description how can we have an optimized description ? */
    if( 0 == pData->desc.used ) {
        pData->opt_desc.length = 0;
        pData->opt_desc.desc   = NULL;
        pData->opt_desc.used   = 0;
        return OPAL_SUCCESS;
    }

    /* If the data is contiguous is useless to generate an optimized version. */
    /*if( pData->size == (pData->true_ub - pData->true_lb) ) return OPAL_SUCCESS; */

    (void)opal_datatype_optimize_short( pData, 1, &(pData->opt_desc) );
    if( 0 != pData->opt_desc.used ) {
        /* let's add a fake element at the end just to avoid useless comparaisons
         * in pack/unpack functions.
         */
        pLast = &(pData->opt_desc.desc[pData->opt_desc.used].end_loop);
        pLast->common.type     = OPAL_DATATYPE_END_LOOP;
        pLast->common.flags    = 0;
        pLast->items           = pData->opt_desc.used;
        pLast->first_elem_disp = first_elem_disp;
        pLast->size            = pData->size;
    }
    return OPAL_SUCCESS;
}
