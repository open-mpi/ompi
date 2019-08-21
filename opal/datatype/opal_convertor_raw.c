/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2019 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stddef.h>

#include "opal/datatype/opal_convertor_internal.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal_stdint.h"

#if OPAL_ENABLE_DEBUG
#include "opal/util/output.h"

#define DO_DEBUG(INST)  if( opal_ddt_raw_debug ) { INST }
#else
#define DO_DEBUG(INST)
#endif /* OPAL_ENABLE_DEBUG */

/* Take a new iovec (base + len) and try to merge it with what we already
 * have. If we succeed return 0 and move forward, otherwise save it into a new
 * iovec location. If we need to advance position and we reach the end
 * of the iovec array, return 1 to signal we did not saved the last iovec.
 */
static inline int
opal_convertor_merge_iov( struct iovec* iov, uint32_t* iov_count,
                          IOVBASE_TYPE* base, size_t len,
                          uint32_t* idx )
{
    if( 0 != iov[*idx].iov_len ) {
        if( (base == ((char*)iov[*idx].iov_base + iov[*idx].iov_len)) ) {
            iov[*idx].iov_len += len;  /* merge with previous iovec */
            return 0;
        }  /* cannot merge, move to the next position */
        *idx = *idx + 1;
        if( *idx == *iov_count ) return 1;  /* do not overwrite outside the iovec array boundaries */
    }
    iov[*idx].iov_base = base;
    iov[*idx].iov_len = len;
    return 0;
}

/**
 * This function always work in local representation. This means no representation
 * conversion (i.e. no heterogeneity) is taken into account, and that all
 * length we're working on are local.
 */
int32_t
opal_convertor_raw( opal_convertor_t* pConvertor,
                    struct iovec* iov, uint32_t* iov_count,
                    size_t* length )
{
    const opal_datatype_t *pData = pConvertor->pDesc;
    dt_stack_t* pStack;       /* pointer to the position on the stack */
    uint32_t pos_desc;        /* actual position in the description of the derived datatype */
    size_t count_desc;        /* the number of items already done in the actual pos_desc */
    size_t do_now, blength;
    dt_elem_desc_t* description, *pElem;
    unsigned char *source_base;  /* origin of the data */
    size_t sum_iov_len = 0;      /* sum of raw data lengths in the iov_len fields */
    uint32_t index = 0;          /* the iov index and a simple counter */

    assert( (*iov_count) > 0 );
    if( OPAL_LIKELY(pConvertor->flags & CONVERTOR_COMPLETED) ) {
        iov[0].iov_base = NULL;
        iov[0].iov_len  = 0;
        *iov_count      = 0;
        *length         = iov[0].iov_len;
        return 1;  /* We're still done */
    }
    if( OPAL_LIKELY(pConvertor->flags & CONVERTOR_NO_OP) ) {
        /* The convertor contain minimal informations, we only use the bConverted
         * to manage the conversion. This function work even after the convertor
         * was moved to a specific position.
         */
        opal_convertor_get_current_pointer( pConvertor, (void**)&iov[0].iov_base );
        iov[0].iov_len = pConvertor->local_size - pConvertor->bConverted;
        *length = iov[0].iov_len;
        pConvertor->bConverted = pConvertor->local_size;
        pConvertor->flags |= CONVERTOR_COMPLETED;
        *iov_count = 1;
        return 1;  /* we're done */
    }

    DO_DEBUG( opal_output( 0, "opal_convertor_raw( %p, {%p, %" PRIu32 "}, %"PRIsize_t " )\n", (void*)pConvertor,
                           (void*)iov, *iov_count, *length ); );

    description = pConvertor->use_desc->desc;

    /* For the first step we have to add both displacement to the source. After in the
     * main while loop we will set back the source_base to the correct value. This is
     * due to the fact that the convertor can stop in the middle of a data with a count
     */
    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc     = pStack->index;
    source_base  = pConvertor->pBaseBuf + pStack->disp;
    count_desc   = pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &(description[pos_desc]);

    DO_DEBUG( opal_output( 0, "raw start pos_desc %d count_desc %" PRIsize_t " disp %ld\n"
                           "stack_pos %d pos_desc %d count_desc %" PRIsize_t " disp %ld\n",
                           pos_desc, count_desc, (long)(source_base - pConvertor->pBaseBuf),
                           pConvertor->stack_pos, pStack->index, pStack->count, (long)pStack->disp ); );

    iov[index].iov_len = 0;
    /* Special case if we start from a position that is in the middle of a data element blocklen.
     * We can treat this outside the loop as it is an exception that can only happen once,
     * and will simplify the loop handling.
     */
    if( pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA ) {
        const ddt_elem_desc_t* current = &(pElem->elem);

        if( count_desc != (current->count * current->blocklen) ) {  /* Not the full element description */
            if( (do_now = count_desc % current->blocklen) ) {
                do_now = current->blocklen - do_now;  /* how much left in the block */
                source_base += current->disp;
                blength = do_now * opal_datatype_basicDatatypes[current->common.type]->size;
                OPAL_DATATYPE_SAFEGUARD_POINTER( source_base, blength, pConvertor->pBaseBuf,
                                                 pConvertor->pDesc, pConvertor->count );
                DO_DEBUG( opal_output( 0, "raw 1. iov[%d] = {base %p, length %" PRIsize_t "}\n",
                                       index, (void*)source_base, blength ); );
                opal_convertor_merge_iov( iov, iov_count,
                                          (IOVBASE_TYPE *) source_base, blength, &index );
                /* ignore the return value, we know there was at least one element in the iovec */
                sum_iov_len += blength;
                count_desc -= do_now;

                source_base += (blength - current->blocklen * opal_datatype_basicDatatypes[current->common.type]->size +
                                current->extent - current->disp);
            }
        }
    }

    while( 1 ) {
        while( pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA ) {
            const ddt_elem_desc_t* current = &(pElem->elem);
            source_base += current->disp;

            do_now = current->count;
            if( count_desc != (current->count * current->blocklen) ) {
                do_now = count_desc / current->blocklen;
                assert( 0 == (count_desc % current->blocklen) );
            }

            blength = current->blocklen * opal_datatype_basicDatatypes[current->common.type]->size;
            for(size_t _i = 0; _i < do_now; _i++ ) {
                OPAL_DATATYPE_SAFEGUARD_POINTER( source_base, blength, pConvertor->pBaseBuf,
                                                 pConvertor->pDesc, pConvertor->count );
                DO_DEBUG( opal_output( 0, "raw 2. iov[%d] = {base %p, length %" PRIsize_t "}\n",
                                       index, (void*)source_base, blength ); );
                if( opal_convertor_merge_iov( iov, iov_count,
                                              (IOVBASE_TYPE *) source_base, blength, &index ) )
                    break;  /* no more iovec available, bail out */

                source_base += current->extent;
                sum_iov_len += blength;
                count_desc -= current->blocklen;
            }

            if( 0 == count_desc ) {  /* completed */
                source_base = pConvertor->pBaseBuf + pStack->disp;
                pos_desc++;  /* advance to the next data */
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
                continue;
            }
            source_base -= current->disp;
            goto complete_loop;
        }
        if( OPAL_DATATYPE_END_LOOP == pElem->elem.common.type ) { /* end of the current loop */
            DO_DEBUG( opal_output( 0, "raw end_loop count %" PRIsize_t " stack_pos %d"
                                   " pos_desc %d disp %ld space %" PRIsize_t "\n",
                                   pStack->count, pConvertor->stack_pos,
                                   pos_desc, (long)pStack->disp, sum_iov_len ); );
            if( --(pStack->count) == 0 ) { /* end of loop */
                if( 0 == pConvertor->stack_pos ) {
                    /* we're done. Force the exit of the main for loop (around iovec) */
                    index++;  /* account for the currently updating iovec */
                    goto complete_loop;
                }
                pConvertor->stack_pos--;
                pStack--;
                pos_desc++;
            } else {
                pos_desc = pStack->index + 1;
                if( pStack->index == -1 ) {
                    pStack->disp += (pData->ub - pData->lb);
                } else {
                    assert( OPAL_DATATYPE_LOOP == description[pStack->index].loop.common.type );
                    pStack->disp += description[pStack->index].loop.extent;  /* jump by the loop extent */
                }
            }
            source_base = pConvertor->pBaseBuf + pStack->disp;
            UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
            DO_DEBUG( opal_output( 0, "raw new_loop count %" PRIsize_t " stack_pos %d "
                                   "pos_desc %d disp %ld space %" PRIsize_t "\n",
                                   pStack->count, pConvertor->stack_pos,
                                   pos_desc, (long)pStack->disp, sum_iov_len ); );
        }
        if( OPAL_DATATYPE_LOOP == pElem->elem.common.type ) {
            ptrdiff_t local_disp = (ptrdiff_t)source_base;
            ddt_endloop_desc_t* end_loop = (ddt_endloop_desc_t*)(pElem + pElem->loop.items);

            if( pElem->loop.common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS ) {
                ptrdiff_t offset = end_loop->first_elem_disp;
                source_base += offset;
                for(; count_desc > 0; ) {
                    OPAL_DATATYPE_SAFEGUARD_POINTER( source_base, end_loop->size, pConvertor->pBaseBuf,
                                                     pConvertor->pDesc, pConvertor->count );
                    if( opal_convertor_merge_iov( iov, iov_count,
                                                  (IOVBASE_TYPE *) source_base, end_loop->size, &index ) ) {
                        source_base -= offset;
                        goto complete_loop;
                    }

                    source_base += pElem->loop.extent;
                    sum_iov_len += end_loop->size;
                    count_desc--;
                    DO_DEBUG( opal_output( 0, "raw contig loop generate iov[%d] = {base %p, length %" PRIsize_t "}"
                                           "space %" PRIsize_t " [pos_desc %d]\n",
                                           index, iov[index].iov_base, iov[index].iov_len,
                                           sum_iov_len, pos_desc ); );
                }
                source_base -= offset;
                pos_desc += pElem->loop.items + 1;
            } else {
                local_disp = (ptrdiff_t)source_base - local_disp;
                PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, OPAL_DATATYPE_LOOP, count_desc,
                            pStack->disp + local_disp);
                pos_desc++;
            }
            source_base = pConvertor->pBaseBuf + pStack->disp;
            UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
            DDT_DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElem, "advance loop" );
        }
    }
 complete_loop:
    pConvertor->bConverted += sum_iov_len;  /* update the already converted bytes */
    *length = sum_iov_len;
    *iov_count = index;
    if( pConvertor->bConverted == pConvertor->local_size ) {
        pConvertor->flags |= CONVERTOR_COMPLETED;
        return 1;
    }
    /* I complete an element, next step I should go to the next one */
    PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, OPAL_DATATYPE_UINT1, count_desc,
                source_base - pConvertor->pBaseBuf );
    DO_DEBUG( opal_output( 0, "raw save stack stack_pos %d pos_desc %d count_desc %" PRIsize_t " disp %ld\n",
                           pConvertor->stack_pos, pStack->index, pStack->count, (long)pStack->disp ); );
    return 0;
}
