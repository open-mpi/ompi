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
    int32_t nbElems = 0, continuity;
    ptrdiff_t total_disp = 0;
    uint32_t i;
    ddt_elem_desc_t last = {.common.flags = 0xFFFF /* all on */, .count = 0, .disp = 0};

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
                last.disp += last.count;
                last.count= 0;
            }
            CREATE_LOOP_END( pElemDesc, nbElems - pStack->index + 1,  /* # of elems in this loop */
                             end_loop->first_elem_disp, end_loop->size, end_loop->common.flags );
            pElemDesc++; nbElems++;
            if( --stack_pos >= 0 ) {  /* still something to do ? */
                ddt_loop_desc_t* pStartLoop = &(pTypeDesc->desc[pStack->index - 1].loop);
                pStartLoop->items = end_loop->items;
                total_disp = pStack->disp;  /* update the displacement position */
            }
            pStack--;  /* go down one position on the stack */
            pos_desc++;
            continue;
        }
        if( OPAL_DATATYPE_LOOP == pData->desc.desc[pos_desc].elem.common.type ) {
            ddt_loop_desc_t* loop = (ddt_loop_desc_t*)&(pData->desc.desc[pos_desc]);
            ddt_endloop_desc_t* end_loop = (ddt_endloop_desc_t*)&(pData->desc.desc[pos_desc + loop->items]);
            int index = GET_FIRST_NON_LOOP( &(pData->desc.desc[pos_desc]) );
            ptrdiff_t loop_disp = pData->desc.desc[pos_desc + index].elem.disp;

            continuity = ((last.disp + (ptrdiff_t)last.count * (ptrdiff_t)opal_datatype_basicDatatypes[last.common.type]->size)
                          == (total_disp + loop_disp));
            if( loop->common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS ) {
                /* the loop is contiguous or composed by contiguous elements with a gap */
                if( loop->extent == (ptrdiff_t)end_loop->size ) {
                    /* the whole loop is contiguous */
                    if( !continuity ) {
                        if( 0 != last.count ) {
                            CREATE_ELEM( pElemDesc, last.common.type, OPAL_DATATYPE_FLAG_BASIC,
                                         last.blocklen, last.count, last.disp, last.extent );
                            pElemDesc++; nbElems++;
                            last.count = 0;
                        }
                        last.disp = total_disp + loop_disp;
                    }
                    last.count  = (last.count * opal_datatype_basicDatatypes[last.common.type]->size
                                   + loop->loops * end_loop->size);
                    last.common.type = OPAL_DATATYPE_UINT1;
                    last.extent      = 1;
                } else {
                    int counter = loop->loops;
                    ptrdiff_t merged_disp = 0;
                    /* if the previous data is contiguous with this piece and it has a length not ZERO */
                    if( last.count != 0 ) {
                        if( continuity ) {
                            last.count       *= opal_datatype_basicDatatypes[last.common.type]->size;
                            last.count       += end_loop->size;
                            last.common.type  = OPAL_DATATYPE_UINT1;
                            last.extent       = 1;
                            counter--;
                            merged_disp = loop->extent;  /* merged loop, update the disp of the remaining elems */
                        }
                        CREATE_ELEM( pElemDesc, last.common.type, OPAL_DATATYPE_FLAG_BASIC,
                                     last.blocklen, last.count, last.disp, last.extent );
                        pElemDesc++; nbElems++;
                        last.disp        += last.count;
                        last.count        = 0;
                        last.common.type  = OPAL_DATATYPE_LOOP;
                    }
                    /**
                     * The content of the loop is contiguous (maybe with a gap before or after).
                     *
                     * If any of the loops have been merged with the previous element, then the
                     * displacement of the first element (or the displacement of all elements if the
                     * loop will be removed) must be updated accordingly.
                     */
                    if( counter <= 2 ) {
                        merged_disp += end_loop->first_elem_disp;
                        while( counter > 0 ) {
                            CREATE_ELEM( pElemDesc, OPAL_DATATYPE_UINT1, OPAL_DATATYPE_FLAG_BASIC,
                                         1, end_loop->size, merged_disp, 1 );
                            pElemDesc++; nbElems++; counter--;
                            merged_disp += loop->extent;
                        }
                    } else {
                        CREATE_LOOP_START( pElemDesc, counter, 2, loop->extent, loop->common.flags );
                        pElemDesc++; nbElems++;
                        CREATE_ELEM( pElemDesc, OPAL_DATATYPE_UINT1, OPAL_DATATYPE_FLAG_BASIC,
                                     1, end_loop->size, loop_disp, 1);
                        pElemDesc++; nbElems++;
                        CREATE_LOOP_END( pElemDesc, 2, end_loop->first_elem_disp + merged_disp,
                                         end_loop->size, end_loop->common.flags );
                        pElemDesc++; nbElems++;
                    }
                }
                pos_desc += loop->items + 1;
            } else {
                ddt_elem_desc_t* elem = (ddt_elem_desc_t*)&(pData->desc.desc[pos_desc+1]);
                if( 0 != last.count ) {
                    CREATE_ELEM( pElemDesc, last.common.type, OPAL_DATATYPE_FLAG_BASIC,
                                 last.blocklen, last.count, last.disp, last.extent );
                    pElemDesc++; nbElems++;
                    last.count       = 0;
                    last.common.type = OPAL_DATATYPE_LOOP;
                }
                if( 2 == loop->items ) { /* small loop */
                    if( (1 == elem->count) && (1 == elem->blocklen)
                        && (elem->extent == (ptrdiff_t)opal_datatype_basicDatatypes[elem->common.type]->size) ) {
                        CREATE_ELEM( pElemDesc, elem->common.type, elem->common.flags & ~OPAL_DATATYPE_FLAG_CONTIGUOUS,
                                     1, loop->loops, elem->disp, loop->extent );
                        pElemDesc++; nbElems++;
                        pos_desc += loop->items + 1;  /* jump the entire loop */
                        goto complete_loop;
                    } else if( loop->loops < 3 ) {  /* unfold the loop */
                        ptrdiff_t elem_displ = elem->disp;
                        for( i = 0; i < loop->loops; i++ ) {
                            CREATE_ELEM( pElemDesc, elem->common.type, elem->common.flags,
                                         elem->blocklen, elem->count, elem_displ, elem->extent );
                            elem_displ += loop->extent;
                            pElemDesc++; nbElems++;
                        }
                        pos_desc += loop->items + 1;
                        goto complete_loop;
                    }
                }
                CREATE_LOOP_START( pElemDesc, loop->loops, loop->items, loop->extent, loop->common.flags );
                pElemDesc++; nbElems++;
                PUSH_STACK( pStack, stack_pos, nbElems, OPAL_DATATYPE_LOOP, loop->loops, total_disp );
                pos_desc++;
                DDT_DUMP_STACK( pStack, stack_pos, pData->desc.desc, "advance loops" );
            }
        complete_loop:
            total_disp = pStack->disp;  /* update the displacement */
            continue;
        }
        while( pData->desc.desc[pos_desc].elem.common.flags & OPAL_DATATYPE_FLAG_DATA ) {  /* go over all basic datatype elements */
            ddt_elem_desc_t* current = &pData->desc.desc[pos_desc].elem;
            pos_desc++;  /* point to the next element as current points to the current one */

            if( 0 == last.count ) {  /* first data of the datatype */
                last = *current;
                continue;  /* next data */
            }

            /* are the two elements compatible: aka they have very similar values and they
             * can be merged together by increasing the count. This optimizes the memory
             * required for storing the datatype description.
             */
            if( ((last.blocklen * opal_datatype_basicDatatypes[last.common.type]->size) ==
                  (current->blocklen * opal_datatype_basicDatatypes[current->common.type]->size)) &&
                (current->disp == (last.disp + last.count * last.extent)) &&
                ((last.count == 1) || (current->count == 1) || (last.extent == current->extent)) ) {
                last.count += current->count;
                if( last.count == 1 ) {
                    last.extent = current->extent;
                }  /* otherwise keep the last.extent */
                /* find the lowest common denomitaor type */
                if( last.common.type != current->common.type ) {
                    last.common.type  = OPAL_DATATYPE_UINT1;
                    last.blocklen    *= opal_datatype_basicDatatypes[last.common.type]->size;
                }
                continue;  /* next data */
            }
            /* are the elements fusionable such that we can fusion the last blocklen of one with the first
             * blocklen of the other.
             */
            if( (last.disp + (last.count - 1) * last.extent + last.blocklen * opal_datatype_basicDatatypes[last.common.type]->size) ==
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
