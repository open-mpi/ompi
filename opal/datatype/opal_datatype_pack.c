/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
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

#if OPAL_ENABLE_DEBUG
#include "opal/util/output.h"

#define DO_DEBUG(INST)  if( opal_pack_debug ) { INST }
#else
#define DO_DEBUG(INST)
#endif  /* OPAL_ENABLE_DEBUG */

#include "opal/datatype/opal_datatype_checksum.h"
#include "opal/datatype/opal_datatype_pack.h"
#include "opal/datatype/opal_datatype_prototypes.h"

#if defined(CHECKSUM)
#define opal_pack_homogeneous_contig_function           opal_pack_homogeneous_contig_checksum
#define opal_pack_homogeneous_contig_with_gaps_function opal_pack_homogeneous_contig_with_gaps_checksum
#define opal_generic_simple_pack_function               opal_generic_simple_pack_checksum
#else
#define opal_pack_homogeneous_contig_function           opal_pack_homogeneous_contig
#define opal_pack_homogeneous_contig_with_gaps_function opal_pack_homogeneous_contig_with_gaps
#define opal_generic_simple_pack_function               opal_generic_simple_pack
#endif  /* defined(CHECKSUM) */


#define IOVEC_MEM_LIMIT 8192


/* the contig versions does not use the stack. They can easily retrieve
 * the status with just the informations from pConvertor->bConverted.
 */
int32_t
opal_pack_homogeneous_contig_function( opal_convertor_t* pConv,
                                       struct iovec* iov,
                                       uint32_t* out_size,
                                       size_t* max_data )
{
    dt_stack_t* pStack = pConv->pStack;
    unsigned char *source_base = NULL;
    uint32_t iov_count;
    size_t length = pConv->local_size - pConv->bConverted, initial_amount = pConv->bConverted;
    OPAL_PTRDIFF_TYPE initial_displ = pConv->use_desc->desc[pConv->use_desc->used].end_loop.first_elem_disp;

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
            OPAL_DATATYPE_SAFEGUARD_POINTER( source_base, iov[iov_count].iov_len,
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
opal_pack_homogeneous_contig_with_gaps_function( opal_convertor_t* pConv,
                                                 struct iovec* iov,
                                                 uint32_t* out_size,
                                                 size_t* max_data )
{
    const opal_datatype_t* pData = pConv->pDesc;
    dt_stack_t* stack = pConv->pStack;
    unsigned char *user_memory, *packed_buffer;
    uint32_t i, index, iov_count;
    size_t bConverted, remaining, length, initial_bytes_converted = pConv->bConverted;
    OPAL_PTRDIFF_TYPE extent= pData->ub - pData->lb;
    OPAL_PTRDIFF_TYPE initial_displ = pConv->use_desc->desc[pConv->use_desc->used].end_loop.first_elem_disp;

    assert( (pData->flags & OPAL_DATATYPE_FLAG_CONTIGUOUS) && ((OPAL_PTRDIFF_TYPE)pData->size != extent) );
    DO_DEBUG( opal_output( 0, "pack_homogeneous_contig( pBaseBuf %p, iov_count %d )\n",
                           pConv->pBaseBuf, *out_size ); );
    if( stack[1].type != opal_datatype_uint1.id ) {
        stack[1].count *= opal_datatype_basicDatatypes[stack[1].type]->size;
        stack[1].type = opal_datatype_uint1.id;
    }

    /* There are some optimizations that can be done if the upper level
     * does not provide a buffer.
     */
    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {
        /* Limit the amount of packed data to the data left over on this convertor */
        remaining = pConv->local_size - pConv->bConverted;
        if( 0 == remaining ) break;  /* we're done this time */
        if( remaining > (uint32_t)iov[iov_count].iov_len )
            remaining = iov[iov_count].iov_len;
        packed_buffer = (unsigned char *)iov[iov_count].iov_base;
        bConverted = remaining; /* how much will get unpacked this time */
        user_memory = pConv->pBaseBuf + initial_displ + stack[0].disp + stack[1].disp;
        i = pConv->count - stack[0].count;  /* how many we already packed */
        assert(i == ((uint32_t)(pConv->bConverted / pData->size)));

        if( packed_buffer == NULL ) {
            /* special case for small data. We avoid allocating memory if we
             * can fill the iovec directly with the address of the remaining
             * data.
             */
            if( (uint32_t)stack->count < ((*out_size) - iov_count) ) {
                stack[1].count = pData->size - (pConv->bConverted % pData->size);
                for( index = iov_count; i < pConv->count; i++, index++ ) {
                    iov[index].iov_base = (IOVBASE_TYPE *) user_memory;
                    iov[index].iov_len = stack[1].count;
                    stack[0].disp += extent;
                    pConv->bConverted += stack[1].count;
                    stack[1].disp  = 0;  /* reset it for the next round */
                    stack[1].count = pData->size;
                    user_memory = pConv->pBaseBuf + initial_displ + stack[0].disp;
                    COMPUTE_CSUM( iov[index].iov_base, iov[index].iov_len, pConv );
                }
                *out_size = iov_count + index;
                *max_data = (pConv->bConverted - initial_bytes_converted);
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
                    if( remaining < pData->size ) {
                        iov[index].iov_base = (IOVBASE_TYPE *) user_memory;
                        iov[index].iov_len = remaining;
                        remaining = 0;
                        COMPUTE_CSUM( iov[index].iov_base, iov[index].iov_len, pConv );
                        break;
                    } else {
                        iov[index].iov_base = (IOVBASE_TYPE *) user_memory;
                        iov[index].iov_len = pData->size;
                        user_memory += extent;
                        COMPUTE_CSUM( iov[index].iov_base, (size_t)iov[index].iov_len, pConv );
                    }
                    remaining -= iov[index].iov_len;
                    pConv->bConverted += iov[index].iov_len;
                }
                *out_size = index;
                *max_data = (pConv->bConverted - initial_bytes_converted);
                if( pConv->bConverted == pConv->local_size ) {
                    pConv->flags |= CONVERTOR_COMPLETED;
                    return 1;
                }
                return 0;
            }
        }

        {
            DO_DEBUG( opal_output( 0, "pack_homogeneous_contig( user_memory %p, packed_buffer %p length %lu\n",
                                   user_memory, packed_buffer, (unsigned long)remaining ); );

            length = (0 == pConv->stack_pos ? 0 : stack[1].count);  /* left over from the last pack */
            /* data left from last round and enough space in the buffer */
            if( (0 != length) && (length <= remaining)) {
                /* copy the partial left-over from the previous round */
                OPAL_DATATYPE_SAFEGUARD_POINTER( user_memory, length, pConv->pBaseBuf,
                                                 pData, pConv->count );
                DO_DEBUG( opal_output( 0, "2. pack dest %p src %p length %lu\n",
                                       user_memory, packed_buffer, (unsigned long)length ); );
                MEMCPY_CSUM( packed_buffer, user_memory, length, pConv );
                packed_buffer  += length;
                user_memory    += (extent - pData->size + length);
                remaining      -= length;
                stack[1].count -= length;
                if( 0 == stack[1].count) { /* one completed element */
                    stack[0].count--;
                    stack[0].disp += extent;
                    if( 0 != stack[0].count ) {  /* not yet done */
                        stack[1].count = pData->size;
                        stack[1].disp = 0;
                    }
                }
            }
            for( i = 0;  pData->size <= remaining; i++ ) {
                OPAL_DATATYPE_SAFEGUARD_POINTER( user_memory, pData->size, pConv->pBaseBuf,
                                                 pData, pConv->count );
                DO_DEBUG( opal_output( 0, "3. pack dest %p src %p length %lu\n",
                                       user_memory, packed_buffer, (unsigned long)pData->size ); );
                MEMCPY_CSUM( packed_buffer, user_memory, pData->size, pConv );
                packed_buffer += pData->size;
                user_memory   += extent;
                remaining   -= pData->size;
            }
            stack[0].count -= i;  /* the filled up and the entire types */
            stack[0].disp  += (i * extent);
            stack[1].disp  += remaining;
            /* Copy the last bits */
            if( 0 != remaining ) {
                OPAL_DATATYPE_SAFEGUARD_POINTER( user_memory, remaining, pConv->pBaseBuf,
                                                 pData, pConv->count );
                DO_DEBUG( opal_output( 0, "4. pack dest %p src %p length %lu\n",
                                       user_memory, packed_buffer, (unsigned long)remaining ); );
                MEMCPY_CSUM( packed_buffer, user_memory, remaining, pConv );
                user_memory += remaining;
                stack[1].count -= remaining;
            }
            if( 0 == stack[1].count ) {  /* prepare for the next element */
                stack[1].count = pData->size;
                stack[1].disp  = 0;
            }
        }
        pConv->bConverted += bConverted;
    }
    *out_size = iov_count;
    *max_data = (pConv->bConverted - initial_bytes_converted);
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
 * - for the OPAL_DATATYPE_LOOP type the DT_CONTIGUOUS flag set means that the content of the loop is
 *   contiguous but with a gap in the begining or at the end.
 * - the DT_CONTIGUOUS flag for the type OPAL_DATATYPE_END_LOOP is meaningless.
 */
int32_t
opal_generic_simple_pack_function( opal_convertor_t* pConvertor,
                                   struct iovec* iov, uint32_t* out_size,
                                   size_t* max_data )
{
    dt_stack_t* pStack;       /* pointer to the position on the stack */
    uint32_t pos_desc;        /* actual position in the description of the derived datatype */
    uint32_t count_desc;      /* the number of items already done in the actual pos_desc */
    size_t total_packed = 0;  /* total amount packed this time */
    dt_elem_desc_t* description;
    dt_elem_desc_t* pElem;
    const opal_datatype_t *pData = pConvertor->pDesc;
    unsigned char *conv_ptr, *iov_ptr;
    size_t iov_len_local;
    uint32_t iov_count;

    DO_DEBUG( opal_output( 0, "opal_convertor_generic_simple_pack( %p:%p, {%p, %lu}, %d )\n",
                           (void*)pConvertor, (void*)pConvertor->pBaseBuf,
                           iov[0].iov_base, (unsigned long)iov[0].iov_len, *out_size ); );

    description = pConvertor->use_desc->desc;

    /* For the first step we have to add both displacement to the source. After in the
     * main while loop we will set back the conv_ptr to the correct value. This is
     * due to the fact that the convertor can stop in the middle of a data with a count
     */
    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc   = pStack->index;
    conv_ptr   = pConvertor->pBaseBuf + pStack->disp;
    count_desc = (uint32_t)pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &(description[pos_desc]);

    DO_DEBUG( opal_output( 0, "pack start pos_desc %d count_desc %d disp %ld\n"
                           "stack_pos %d pos_desc %d count_desc %d disp %ld\n",
                           pos_desc, count_desc, (long)(conv_ptr - pConvertor->pBaseBuf),
                           pConvertor->stack_pos, pStack->index, (int)pStack->count, (long)pStack->disp ); );

    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {
        iov_ptr = (unsigned char *) iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;
        while( 1 ) {
            while( pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA ) {
                /* now here we have a basic datatype */
                PACK_PREDEFINED_DATATYPE( pConvertor, pElem, count_desc,
                                          conv_ptr, iov_ptr, iov_len_local );
                if( 0 == count_desc ) {  /* completed */
                    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                    pos_desc++;  /* advance to the next data */
                    UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
                    continue;
                }
                goto complete_loop;
            }
            if( OPAL_DATATYPE_END_LOOP == pElem->elem.common.type ) { /* end of the current loop */
                DO_DEBUG( opal_output( 0, "pack end_loop count %d stack_pos %d"
                                       " pos_desc %d disp %ld space %lu\n",
                                       (int)pStack->count, pConvertor->stack_pos,
                                       pos_desc, (long)pStack->disp, (unsigned long)iov_len_local ); );
                if( --(pStack->count) == 0 ) { /* end of loop */
                    if( 0 == pConvertor->stack_pos ) {
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
                        assert( OPAL_DATATYPE_LOOP == description[pStack->index].loop.common.type );
                        pStack->disp += description[pStack->index].loop.extent;
                    }
                }
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
                DO_DEBUG( opal_output( 0, "pack new_loop count %d stack_pos %d pos_desc %d count_desc %d disp %ld space %lu\n",
                                       (int)pStack->count, pConvertor->stack_pos, pos_desc,
                                       count_desc, (long)pStack->disp, (unsigned long)iov_len_local ); );
            }
            if( OPAL_DATATYPE_LOOP == pElem->elem.common.type ) {
                OPAL_PTRDIFF_TYPE local_disp = (OPAL_PTRDIFF_TYPE)conv_ptr;
                if( pElem->loop.common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS ) {
                    PACK_CONTIGUOUS_LOOP( pConvertor, pElem, count_desc,
                                          conv_ptr, iov_ptr, iov_len_local );
                    if( 0 == count_desc ) {  /* completed */
                        pos_desc += pElem->loop.items + 1;
                        goto update_loop_description;
                    }
                    /* Save the stack with the correct last_count value. */
                }
                local_disp = (OPAL_PTRDIFF_TYPE)conv_ptr - local_disp;
                PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, OPAL_DATATYPE_LOOP, count_desc,
                            pStack->disp + local_disp);
                pos_desc++;
            update_loop_description:  /* update the current state */
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
                DDT_DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElem, "advance loop" );
                continue;
            }
        }
    complete_loop:
        iov[iov_count].iov_len -= iov_len_local;  /* update the amount of valid data */
        total_packed += iov[iov_count].iov_len;
    }
    *max_data = total_packed;
    pConvertor->bConverted += total_packed;  /* update the already converted bytes */
    *out_size = iov_count;
    if( pConvertor->bConverted == pConvertor->local_size ) {
        pConvertor->flags |= CONVERTOR_COMPLETED;
        return 1;
    }
    /* Save the global position for the next round */
    PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, pElem->elem.common.type, count_desc,
                conv_ptr - pConvertor->pBaseBuf );
    DO_DEBUG( opal_output( 0, "pack save stack stack_pos %d pos_desc %d count_desc %d disp %ld\n",
                           pConvertor->stack_pos, pStack->index, (int)pStack->count, (long)pStack->disp ); );
    return 0;
}
