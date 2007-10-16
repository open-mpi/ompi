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
#include "ompi/datatype/convertor_internal.h"
#include "ompi/datatype/datatype_internal.h"

#if OMPI_ENABLE_DEBUG
extern int ompi_pack_debug;
#define DO_DEBUG(INST)  if( ompi_pack_debug ) { INST }
#else
#define DO_DEBUG(INST)
#endif  /* OMPI_ENABLE_DEBUG */

#include "ompi/datatype/datatype_checksum.h"
#include "ompi/datatype/datatype_pack.h"
#include "ompi/datatype/datatype_prototypes.h"

#if defined(CHECKSUM)
#define ompi_pack_homogeneous_contig_function           ompi_pack_homogeneous_contig_checksum
#define ompi_pack_homogeneous_contig_with_gaps_function ompi_pack_homogeneous_contig_with_gaps_checksum
#define ompi_generic_simple_pack_function               ompi_generic_simple_pack_checksum
#else
#define ompi_pack_homogeneous_contig_function           ompi_pack_homogeneous_contig
#define ompi_pack_homogeneous_contig_with_gaps_function ompi_pack_homogeneous_contig_with_gaps
#define ompi_generic_simple_pack_function               ompi_generic_simple_pack
#endif  /* defined(CHECKSUM) */


#define IOVEC_MEM_LIMIT 8192


/* the contig versions does not use the stack. They can easily retrieve
 * the status with just the informations from pConvertor->bConverted.
 */
int32_t
ompi_pack_homogeneous_contig_function( ompi_convertor_t* pConv,
                                       struct iovec* iov,
                                       uint32_t* out_size,
                                       size_t* max_data )
{
    dt_stack_t* pStack = pConv->pStack;
    char *source_base = NULL;
    uint32_t iov_count;
    size_t length = pConv->local_size - pConv->bConverted, initial_amount = pConv->bConverted;
    ptrdiff_t initial_displ = pConv->use_desc->desc[pConv->use_desc->used].end_loop.first_elem_disp;

    source_base = (pConv->pBaseBuf + initial_displ + pStack[0].disp + pStack[1].disp);

    /* There are some optimizations that can be done if the upper level
     * does not provide a buffer.
     */
    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {
        if( 0 == length ) break;
        if( (size_t)iov[iov_count].iov_len > length )
            iov[iov_count].iov_len = length;
        if( iov[iov_count].iov_base == NULL ) {
            iov[iov_count].iov_base = (IOVBASE_TYPE *) source_base;
            COMPUTE_CSUM( iov[iov_count].iov_base, iov[iov_count].iov_len, pConv );
        } else {
            /* contiguous data just memcpy the smallest data in the user buffer */
            OMPI_DDT_SAFEGUARD_POINTER( source_base, iov[iov_count].iov_len,
                                        pConv->pBaseBuf, pConv->pDesc, pConv->count );
            MEMCPY_CSUM( iov[iov_count].iov_base, source_base, iov[iov_count].iov_len, pConv );
        }
        length -= iov[iov_count].iov_len;
        pConv->bConverted += iov[iov_count].iov_len;
        pStack[0].disp += iov[iov_count].iov_len;
        source_base += iov[iov_count].iov_len;
    }

    /* update the return value */
    *max_data = pConv->bConverted - initial_amount;
    *out_size = iov_count;
    if( pConv->bConverted == pConv->local_size ) {
        pConv->flags |= CONVERTOR_COMPLETED;
        return 1;
    }
    return 0;
}

int32_t
ompi_pack_homogeneous_contig_with_gaps_function( ompi_convertor_t* pConv,
                                                 struct iovec* iov,
                                                 uint32_t* out_size,
                                                 size_t* max_data )
{
    const ompi_datatype_t* pData = pConv->pDesc;
    dt_stack_t* pStack = pConv->pStack;
    char *user_memory, *packed_buffer;
    uint32_t i, index, iov_count;
    size_t max_allowed, total_bytes_converted = 0;
    ptrdiff_t extent;
    ptrdiff_t initial_displ = pConv->use_desc->desc[pConv->use_desc->used].end_loop.first_elem_disp;

    extent = pData->ub - pData->lb;
    assert( (pData->flags & DT_FLAG_CONTIGUOUS) && ((ptrdiff_t)pData->size != extent) );

    /* Limit the amount of packed data to the data left over on this convertor */
    max_allowed = pConv->local_size - pConv->bConverted;
    if( max_allowed > (*max_data) )
        max_allowed = (*max_data);

    i = (uint32_t)(pConv->bConverted / pData->size);  /* how many we already pack */

    /* There are some optimizations that can be done if the upper level
     * does not provide a buffer.
     */
    user_memory = pConv->pBaseBuf + initial_displ + pStack[0].disp + pStack[1].disp;
    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {
        if( 0 == max_allowed ) break;  /* we're done this time */
        if( iov[iov_count].iov_base == NULL ) {
            /* special case for small data. We avoid allocating memory if we
             * can fill the iovec directly with the address of the remaining
             * data.
             */
            if( (uint32_t)pStack->count < ((*out_size) - iov_count) ) {
                pStack[1].count = pData->size - (pConv->bConverted % pData->size);
                for( index = iov_count; i < pConv->count; i++, index++ ) {
                    iov[index].iov_base = (IOVBASE_TYPE *) user_memory;
                    iov[index].iov_len = pStack[1].count;
                    pStack[0].disp += extent;
                    total_bytes_converted += pStack[1].count;
                    pStack[1].disp  = 0;  /* reset it for the next round */
                    pStack[1].count = pData->size;
                    user_memory = pConv->pBaseBuf + initial_displ + pStack[0].disp;
                    COMPUTE_CSUM( iov[index].iov_base, iov[index].iov_len, pConv );
                }
                *out_size = iov_count + index;
                pConv->bConverted += total_bytes_converted;
                *max_data = total_bytes_converted;
                pConv->flags |= CONVERTOR_COMPLETED;
                return 1;  /* we're done */
            }
            /* now special case for big contiguous data with gaps around */
            if( pData->size >= IOVEC_MEM_LIMIT ) {
                /* as we dont have to copy any data, we can simply fill the iovecs
                 * with data from the user data description.
                 */
                for( index = iov_count; (i < pConv->count) && (index < (*out_size));
                     i++, index++ ) {
                    if( max_allowed < pData->size ) {
                        iov[index].iov_base = (IOVBASE_TYPE *) user_memory;
                        iov[index].iov_len = max_allowed;
                        max_allowed = 0;
                        COMPUTE_CSUM( iov[index].iov_base, iov[index].iov_len, pConv );
                        break;
                    } else {
                        iov[index].iov_base = (IOVBASE_TYPE *) user_memory;
                        iov[index].iov_len = pData->size;
                        user_memory += extent;
                        COMPUTE_CSUM( iov[index].iov_base, (size_t)iov[index].iov_len, pConv );
                    }
                    max_allowed -= iov[index].iov_len;
                    total_bytes_converted += iov[index].iov_len;
                }
                *out_size = index;
                *max_data = total_bytes_converted;
                pConv->bConverted += total_bytes_converted;
                if( pConv->bConverted == pConv->local_size ) {
                    pConv->flags |= CONVERTOR_COMPLETED;
                    return 1;
                }
                return 0;
            }
        }

        {
            uint32_t counter;
            size_t done;

            packed_buffer = iov[iov_count].iov_base;
            done = pConv->bConverted - i * pData->size;  /* partial data from last pack */
            if( done != 0 ) {  /* still some data to copy from the last time */
                done = pData->size - done;
                OMPI_DDT_SAFEGUARD_POINTER( user_memory, done, pConv->pBaseBuf, pData, pConv->count );
                MEMCPY_CSUM( packed_buffer, user_memory, done, pConv );
                packed_buffer += done;
                max_allowed -= done;
                total_bytes_converted += done;
                user_memory += (extent - pData->size + done);
            }
            counter = (uint32_t)(max_allowed / pData->size);
            if( counter > pConv->count ) counter = pConv->count;
            for( i = 0; i < counter; i++ ) {
                OMPI_DDT_SAFEGUARD_POINTER( user_memory, pData->size, pConv->pBaseBuf, pData, pConv->count );
                MEMCPY_CSUM( packed_buffer, user_memory, pData->size, pConv );
                packed_buffer+= pData->size;
                user_memory += extent;
            }
            done = (counter * pData->size);
            max_allowed -= done;
            total_bytes_converted += done;
            /* If there is anything pending ... */
            if( 0 != max_allowed ) {
                done = max_allowed;
                OMPI_DDT_SAFEGUARD_POINTER( user_memory, done, pConv->pBaseBuf, pData, pConv->count );
                MEMCPY_CSUM( packed_buffer, user_memory, done, pConv );
                packed_buffer += done;
                max_allowed = 0;
                total_bytes_converted += done;
                user_memory += done;
            }
        }
    }
    pStack[0].disp = (intptr_t)user_memory - (intptr_t)pConv->pBaseBuf - initial_displ;
    pStack[1].disp = max_allowed;
    *max_data = total_bytes_converted;
    pConv->bConverted += total_bytes_converted;
    *out_size = iov_count;
    if( pConv->bConverted == pConv->local_size ) {
        pConv->flags |= CONVERTOR_COMPLETED;
        return 1;
    }
    return 0;
}

/* The pack/unpack functions need a cleanup. I have to create a proper interface to access
 * all basic functionalities, hence using them as basic blocks for all conversion functions.
 *
 * But first let's make some global assumptions:
 * - a datatype (with the flag DT_DATA set) will have the contiguous flags set if and only if
 *   the data is really contiguous (extent equal with size)
 * - for the DT_LOOP type the DT_CONTIGUOUS flag set means that the content of the loop is
 *   contiguous but with a gap in the begining or at the end.
 * - the DT_CONTIGUOUS flag for the type DT_END_LOOP is meaningless.
 */
int32_t
ompi_generic_simple_pack_function( ompi_convertor_t* pConvertor,
                                   struct iovec* iov, uint32_t* out_size,
                                   size_t* max_data )
{
    dt_stack_t* pStack;       /* pointer to the position on the stack */
    uint32_t pos_desc;        /* actual position in the description of the derived datatype */
    uint32_t count_desc;      /* the number of items already done in the actual pos_desc */
    size_t total_packed = 0;  /* total amount packed this time */
    dt_elem_desc_t* description;
    dt_elem_desc_t* pElem;
    const ompi_datatype_t *pData = pConvertor->pDesc;
    char *source_base, *destination;
    size_t iov_len_local;
    uint32_t iov_count;

    DO_DEBUG( opal_output( 0, "ompi_convertor_generic_simple_pack( %p, {%p, %lu}, %d )\n", (void*)pConvertor,
                           iov[0].iov_base, (unsigned long)iov[0].iov_len, *out_size ); );

    description = pConvertor->use_desc->desc;

    /* For the first step we have to add both displacement to the source. After in the
     * main while loop we will set back the source_base to the correct value. This is
     * due to the fact that the convertor can stop in the middle of a data with a count
     */
    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc     = pStack->index;
    source_base  = pConvertor->pBaseBuf + pStack->disp;
    count_desc   = (uint32_t)pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &(description[pos_desc]);
    source_base += pStack->disp;

    DO_DEBUG( opal_output( 0, "unpack start pos_desc %d count_desc %d disp %ld\n"
                           "stack_pos %d pos_desc %d count_desc %d disp %ld\n",
                           pos_desc, count_desc, (long)(source_base - pConvertor->pBaseBuf),
                           pConvertor->stack_pos, pStack->index, (int)pStack->count, (long)pStack->disp ); );

    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {
        destination = iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;
        while( 1 ) {
            while( pElem->elem.common.flags & DT_FLAG_DATA ) {
                /* now here we have a basic datatype */
                PACK_PREDEFINED_DATATYPE( pConvertor, pElem, count_desc,
                                          source_base, destination, iov_len_local );
                if( 0 == count_desc ) {  /* completed */
                    source_base = pConvertor->pBaseBuf + pStack->disp;
                    pos_desc++;  /* advance to the next data */
                    UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
                    continue;
                }
                goto complete_loop;
            }
            if( DT_END_LOOP == pElem->elem.common.type ) { /* end of the current loop */
                DO_DEBUG( opal_output( 0, "pack end_loop count %d stack_pos %d pos_desc %d disp %ld space %lu\n",
                                       (int)pStack->count, pConvertor->stack_pos, pos_desc, (long)pStack->disp, (unsigned long)iov_len_local ); );
                if( --(pStack->count) == 0 ) { /* end of loop */
                    if( pConvertor->stack_pos == 0 ) {
                        /* we lie about the size of the next element in order to
                         * make sure we exit the main loop.
                         */
                        *out_size = iov_count;
                        goto complete_loop;  /* completed */
                    }
                    pConvertor->stack_pos--;
                    pStack--;
                    pos_desc++;
                } else {
                    pos_desc = pStack->index + 1;
                    if( pStack->index == -1 ) {
                        pStack->disp += (pData->ub - pData->lb);
                    } else {
                        assert( DT_LOOP == description[pStack->index].loop.common.type );
                        pStack->disp += description[pStack->index].loop.extent;
                    }
                }
                source_base = pConvertor->pBaseBuf + pStack->disp;
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
                DO_DEBUG( opal_output( 0, "pack new_loop count %d stack_pos %d pos_desc %d disp %ld space %lu\n",
                                       (int)pStack->count, pConvertor->stack_pos, pos_desc, (long)pStack->disp, (unsigned long)iov_len_local ); );
            }
            if( DT_LOOP == pElem->elem.common.type ) {
                ptrdiff_t local_disp = (ptrdiff_t)source_base;
                if( pElem->loop.common.flags & DT_FLAG_CONTIGUOUS ) {
                    PACK_CONTIGUOUS_LOOP( pConvertor, pElem, count_desc,
                                          source_base, destination, iov_len_local );
                    if( 0 == count_desc ) {  /* completed */
                        pos_desc += pElem->loop.items + 1;
                        goto update_loop_description;
                    }
                    /* Save the stack with the correct last_count value. */
                }
                local_disp = (ptrdiff_t)source_base - local_disp;
                PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, DT_LOOP, count_desc,
                            pStack->disp + local_disp);
                pos_desc++;
            update_loop_description:  /* update the current state */
                source_base = pConvertor->pBaseBuf + pStack->disp;
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
                DDT_DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElem, "advance loop" );
                continue;
            }
        }
    complete_loop:
        iov[iov_count].iov_len -= iov_len_local;  /* update the amount of valid data */
        total_packed += iov[iov_count].iov_len;
        pConvertor->bConverted += iov[iov_count].iov_len;  /* update the already converted bytes */
    }
    *max_data = total_packed;
    *out_size = iov_count;

    if( pConvertor->bConverted == pConvertor->local_size ) {
        pConvertor->flags |= CONVERTOR_COMPLETED;
        return 1;
    }
    /* I complete an element, next step I should go to the next one */
    PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, DT_BYTE, count_desc,
                source_base - pStack->disp - pConvertor->pBaseBuf );
    DO_DEBUG( opal_output( 0, "pack save stack stack_pos %d pos_desc %d count_desc %d disp %ld\n",
                           pConvertor->stack_pos, pStack->index, (int)pStack->count, (long)pStack->disp ); );
    return 0;
}
