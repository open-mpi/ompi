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

#define SET_EMPTY_ELEMENT( ELEM )             \
    do {                                      \
        ddt_elem_desc_t* _elem = (ELEM);      \
        _elem->common.flags = DT_FLAG_BASIC;  \
        _elem->common.type  = DT_LOOP;        \
        _elem->count        = 0;              \
        _elem->disp         = 0;              \
        _elem->extent       = 0;              \
    } while (0)

static inline int SAVE_OPTIMIZED_ELEMENT( dt_elem_desc_t* pElemDesc,
                                          ddt_elem_desc_t* opt_elem )
{
    if( 0 != opt_elem->count ) {
        pElemDesc->elem = *opt_elem;
        SET_EMPTY_ELEMENT( opt_elem );
    }
    return 0;
}

static inline int ADD_ELEMENT( dt_elem_desc_t* pElemDesc,
                               ddt_elem_desc_t* opt_elem,
                               uint16_t type, uint32_t count,
                               ptrdiff_t disp, int32_t extent )
{
    if( 0 == opt_elem->count ) {
        opt_elem->common.flags = DT_FLAG_BASIC;
        opt_elem->common.type  = type;
        opt_elem->count        = count;
        opt_elem->disp         = disp;
        opt_elem->extent       = extent;
        return 0;
    }
    return 1;
}

static int32_t
ompi_ddt_optimize_short( ompi_datatype_t* pData,
                         int32_t count,
                         dt_type_desc_t* pTypeDesc )
{
    dt_elem_desc_t* pElemDesc;
    ddt_elem_desc_t opt_elem;
    ptrdiff_t last_disp = 0;
    dt_stack_t* pStack;            /* pointer to the position on the stack */
    int32_t pos_desc = 0;          /* actual position in the description of the derived datatype */
    int32_t stack_pos = 0, last_type = DT_BYTE;
    int32_t type = DT_LOOP, nbElems = 0, changes = 0;
    int32_t optimized = 0, continuity;
    uint16_t last_flags = 0xFFFF;  /* keep all for the first datatype */
    ptrdiff_t total_disp = 0, last_extent = 1;
	int32_t last_length = 0;
    uint32_t i;

    pStack = (dt_stack_t*)alloca( sizeof(dt_stack_t) * (pData->btypes[DT_LOOP]+2) );
    SAVE_STACK( pStack, -1, 0, count, 0 );

    pTypeDesc->length = 2 * pData->desc.used + 1 /* for the fake DT_END_LOOP at the end */;
    pTypeDesc->desc = pElemDesc = (dt_elem_desc_t*)malloc( sizeof(dt_elem_desc_t) * pTypeDesc->length );
    pTypeDesc->used = 0;

    SET_EMPTY_ELEMENT( &opt_elem );
    assert( DT_END_LOOP == pData->desc.desc[pData->desc.used].elem.common.type );
    opt_elem.common.type = DT_LOOP;
    opt_elem.common.flags = 0xFFFF;  /* keep all for the first datatype */
    opt_elem.count = 0;
    opt_elem.disp = pData->desc.desc[pData->desc.used].end_loop.first_elem_disp;
    opt_elem.extent = 0;

    while( stack_pos >= 0 ) {
        if( DT_END_LOOP == pData->desc.desc[pos_desc].elem.common.type ) { /* end of the current loop */
            ddt_endloop_desc_t* end_loop = &(pData->desc.desc[pos_desc].end_loop);
            if( last_length != 0 ) {
                CREATE_ELEM( pElemDesc, last_type, DT_FLAG_BASIC, last_length, last_disp, last_extent );
                pElemDesc++; nbElems++;
                last_disp += last_length;
                last_length = 0;
            }
            CREATE_LOOP_END( pElemDesc, nbElems - pStack->index + 1,  /* # of elems in this loop */
                             end_loop->first_elem_disp, end_loop->size, end_loop->common.flags );
            pElemDesc++; nbElems++;
            if( --stack_pos >= 0 ) {  /* still something to do ? */
                ddt_loop_desc_t* pStartLoop = &(pTypeDesc->desc[pStack->index - 1].loop);
                pStartLoop->items = (pElemDesc - 1)->elem.count;
                total_disp = pStack->disp;  /* update the displacement position */
            }
            pStack--;  /* go down one position on the stack */
            pos_desc++;
            continue;
        }
        if( DT_LOOP == pData->desc.desc[pos_desc].elem.common.type ) {
            ddt_loop_desc_t* loop = (ddt_loop_desc_t*)&(pData->desc.desc[pos_desc]);
            ddt_endloop_desc_t* end_loop = (ddt_endloop_desc_t*)&(pData->desc.desc[pos_desc + loop->items]);
            int index = GET_FIRST_NON_LOOP( &(pData->desc.desc[pos_desc]) );
            ptrdiff_t loop_disp = pData->desc.desc[pos_desc + index].elem.disp;

            continuity = ((last_disp + last_length * (ptrdiff_t)ompi_ddt_basicDatatypes[last_type]->size)
                              == (total_disp + loop_disp));
            if( loop->common.flags & DT_FLAG_CONTIGUOUS ) {
                /* the loop is contiguous or composed by contiguous elements with a gap */
                if( loop->extent == (ptrdiff_t)end_loop->size ) {
                    /* the whole loop is contiguous */
                    if( !continuity ) {
                        if( 0 != last_length ) {
                            CREATE_ELEM( pElemDesc, last_type, DT_FLAG_BASIC, last_length, last_disp, last_extent );
                            pElemDesc++; nbElems++;
                            last_length = 0;
                        }
                        last_disp = total_disp + loop_disp;
                    }
                    last_length = (last_length * ompi_ddt_basicDatatypes[last_type]->size
                                   + loop->loops * end_loop->size);
                    last_type   = DT_BYTE;
                    last_extent = 1;
                    optimized++;
                } else {
                    int counter = loop->loops;
                    /* if the previous data is contiguous with this piece and it has a length not ZERO */
                    if( last_length != 0 ) {
                        if( continuity ) {
                            last_length *= ompi_ddt_basicDatatypes[last_type]->size;
                            last_length += end_loop->size;
                            last_type    = DT_BYTE;
                            last_extent  = 1;
                            counter--;
                        }
                        CREATE_ELEM( pElemDesc, last_type, DT_FLAG_BASIC, last_length, last_disp, last_extent );
                        pElemDesc++; nbElems++;
                        last_disp += last_length;
                        last_length = 0;
                        last_type = DT_LOOP;
                    }
                    /* we have a gap in the begining or the end of the loop but the whole
                     * loop can be merged in just one memcpy.
                     */
                    CREATE_LOOP_START( pElemDesc, counter, 2, loop->extent, loop->common.flags );
                    pElemDesc++; nbElems++;
                    CREATE_ELEM( pElemDesc, DT_BYTE, DT_FLAG_BASIC, end_loop->size, loop_disp, 1);
                    pElemDesc++; nbElems++;
                    CREATE_LOOP_END( pElemDesc, 2, end_loop->first_elem_disp, end_loop->size,
                                     end_loop->common.flags );
                    pElemDesc++; nbElems++;
                    if( loop->items > 2 ) optimized++;
                }
                pos_desc += loop->items + 1;
                changes++;
            } else {
                ddt_elem_desc_t* elem = (ddt_elem_desc_t*)&(pData->desc.desc[pos_desc+1]);
                if( last_length != 0 ) {
                    CREATE_ELEM( pElemDesc, last_type, DT_FLAG_BASIC, last_length, last_disp, last_extent );
                    pElemDesc++; nbElems++;
                    last_disp  += last_length;
                    last_length = 0;
                    last_type   = DT_LOOP;
                }
                if( 2 == loop->items ) { /* small loop */
                    if( (1 == elem->count)
                        && (elem->extent == (ptrdiff_t)ompi_ddt_basicDatatypes[elem->common.type]->size) ) {
                        CREATE_ELEM( pElemDesc, elem->common.type, elem->common.flags & ~DT_FLAG_CONTIGUOUS,
                                     loop->loops, elem->disp, loop->extent );
                        pElemDesc++; nbElems++;
                        pos_desc += loop->items + 1;
                        changes++; optimized++;
                        goto complete_loop;
                    } else if( loop->loops < 3 ) {
                        ptrdiff_t elem_displ = elem->disp;
                        for( i = 0; i < loop->loops; i++ ) {
                            CREATE_ELEM( pElemDesc, elem->common.type, elem->common.flags,
                                         elem->count, elem_displ, elem->extent );
                            elem_displ += loop->extent;
                            pElemDesc++; nbElems++;
                        }
                        pos_desc += loop->items + 1;
                        changes += loop->loops; optimized += loop->loops;
                        goto complete_loop;
                    }
                }
                CREATE_LOOP_START( pElemDesc, loop->loops, loop->items, loop->extent, loop->common.flags );
                pElemDesc++; nbElems++;
                PUSH_STACK( pStack, stack_pos, nbElems, DT_LOOP, loop->loops, total_disp );
                pos_desc++;
                DDT_DUMP_STACK( pStack, stack_pos, pData->desc.desc, "advance loops" );
            }
        complete_loop:
            total_disp = pStack->disp;  /* update the displacement */
            continue;
        }
        while( pData->desc.desc[pos_desc].elem.common.flags & DT_FLAG_DATA ) {  /* keep doing it until we reach a non datatype element */
            /* now here we have a basic datatype */
            type = pData->desc.desc[pos_desc].elem.common.type;
            continuity = ((last_disp + last_length * (ptrdiff_t)ompi_ddt_basicDatatypes[last_type]->size)
                          == (total_disp + pData->desc.desc[pos_desc].elem.disp));

            if( (pData->desc.desc[pos_desc].elem.common.flags & DT_FLAG_CONTIGUOUS) && continuity &&
                (pData->desc.desc[pos_desc].elem.extent == (int32_t)ompi_ddt_basicDatatypes[type]->size) ) {
                if( type == last_type ) {
                    last_length += pData->desc.desc[pos_desc].elem.count;
                    last_extent = pData->desc.desc[pos_desc].elem.extent;
                } else {
                    if( last_length == 0 ) {
                        last_type = type;
                        last_length = pData->desc.desc[pos_desc].elem.count;
                        last_extent = pData->desc.desc[pos_desc].elem.extent;
                    } else {
                        last_length = last_length * ompi_ddt_basicDatatypes[last_type]->size +
                            pData->desc.desc[pos_desc].elem.count * ompi_ddt_basicDatatypes[type]->size;
                        last_type = DT_BYTE;
                        last_extent = 1;
                        optimized++;
                    }
                }
                last_flags &= pData->desc.desc[pos_desc].elem.common.flags;
            } else {
                if( last_length != 0 ) {
                    CREATE_ELEM( pElemDesc, last_type, DT_FLAG_BASIC, last_length, last_disp, last_extent );
                    pElemDesc++; nbElems++;
                }
                last_disp = total_disp + pData->desc.desc[pos_desc].elem.disp;
                last_length = pData->desc.desc[pos_desc].elem.count;
                last_extent = pData->desc.desc[pos_desc].elem.extent;
                last_type = type;
            }
            pos_desc++;  /* advance to the next data */
        }
    }

    if( last_length != 0 ) {
        CREATE_ELEM( pElemDesc, last_type, DT_FLAG_BASIC, last_length, last_disp, last_extent );
        pElemDesc++; nbElems++;
    }
    /* cleanup the stack */
    pTypeDesc->used = nbElems - 1;  /* except the last fake END_LOOP */
    return OMPI_SUCCESS;
}

int32_t ompi_ddt_commit( ompi_datatype_t** data )
{
    ompi_datatype_t* pData = *data;
    ddt_endloop_desc_t* pLast = &(pData->desc.desc[pData->desc.used].end_loop);
    ptrdiff_t first_elem_disp = 0;

    if( pData->flags & DT_FLAG_COMMITED ) return OMPI_SUCCESS;
    pData->flags |= DT_FLAG_COMMITED;

    /* We have to compute the displacement of the first non loop item in the
     * description.
     */
    if( 0 != pData->size ) {
        int index;
        dt_elem_desc_t* pElem = pData->desc.desc;

        index = GET_FIRST_NON_LOOP( pElem );
        assert( pData->desc.desc[index].elem.common.flags & DT_FLAG_DATA );
        first_elem_disp = pData->desc.desc[index].elem.disp;
    }

    /* let's add a fake element at the end just to avoid useless comparaisons
     * in pack/unpack functions.
     */
    pLast->common.type     = DT_END_LOOP;
    pLast->common.flags    = 0;
    pLast->items           = pData->desc.used;
    pLast->first_elem_disp = first_elem_disp;
    pLast->size            = pData->size;

    /* If there is no datatype description how can we have an optimized description ? */
    if( 0 == pData->desc.used ) {
        pData->opt_desc.length = 0;
        pData->opt_desc.desc   = NULL;
        pData->opt_desc.used   = 0;
        return OMPI_SUCCESS;
    }

    /* If the data is contiguous is useless to generate an optimized version. */
    /*if( pData->size == (pData->true_ub - pData->true_lb) ) return OMPI_SUCCESS; */

    (void)ompi_ddt_optimize_short( pData, 1, &(pData->opt_desc) );
    if( 0 != pData->opt_desc.used ) {
        /* let's add a fake element at the end just to avoid useless comparaisons
         * in pack/unpack functions.
         */
        pLast = &(pData->opt_desc.desc[pData->opt_desc.used].end_loop);
        pLast->common.type     = DT_END_LOOP;
        pLast->common.flags    = 0;
        pLast->items           = pData->opt_desc.used;
        pLast->first_elem_disp = first_elem_disp;
        pLast->size            = pData->size;
    }
    return OMPI_SUCCESS;
}
