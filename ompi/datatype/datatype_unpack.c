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
extern int ompi_unpack_debug;
#define DO_DEBUG(INST)  if( ompi_unpack_debug ) { INST }
#else
#define DO_DEBUG(INST)
#endif  /* OMPI_ENABLE_DEBUG */

#include "ompi/datatype/datatype_checksum.h"
#include "ompi/datatype/datatype_unpack.h"
#include "ompi/datatype/datatype_prototypes.h"

#if defined(CHECKSUM)
#define ompi_unpack_general_function            ompi_unpack_general_checksum
#define ompi_unpack_homogeneous_contig_function ompi_unpack_homogeneous_contig_checksum
#define ompi_generic_simple_unpack_function     ompi_generic_simple_unpack_checksum
#else
#define ompi_unpack_general_function            ompi_unpack_general
#define ompi_unpack_homogeneous_contig_function ompi_unpack_homogeneous_contig
#define ompi_generic_simple_unpack_function     ompi_generic_simple_unpack
#endif  /* defined(CHECKSUM) */

/*
 *  Remember that the first item in the stack (ie. position 0) is the number
 * of times the datatype is involved in the operation (ie. the count argument
 * in the MPI_ call).
 */
/* Convert data from multiple input buffers (as received from the network layer)
 * to a contiguous output buffer with a predefined size.
 * return OMPI_SUCCESS if everything went OK and if there is still room before the complete
 *          conversion of the data (need additional call with others input buffers )
 *        1 if everything went fine and the data was completly converted
 *       -1 something wrong occurs.
 */
int32_t
ompi_unpack_general_function( ompi_convertor_t* pConvertor,
                              struct iovec* iov,
                              uint32_t* out_size,
                              size_t* max_data )
{
    dt_stack_t* pStack;      /* pointer to the position on the stack */
    uint32_t pos_desc;       /* actual position in the description of the derived datatype */
    int32_t count_desc;      /* the number of items already done in the actual pos_desc */
    int type = DT_CHAR;      /* type at current position */
    ptrdiff_t advance;       /* number of bytes that we should advance the buffer */
    ptrdiff_t disp_desc = 0; /* compute displacement for truncated data */
    size_t bConverted = 0;   /* number of bytes converted this time */
    const ompi_convertor_master_t* master = pConvertor->master;
    dt_elem_desc_t* description;
    ptrdiff_t extent = pConvertor->pDesc->ub - pConvertor->pDesc->lb;
    size_t oCount = extent * pConvertor->count;
    size_t iCount, total_bytes_converted = 0;
    char* pInput;
    int32_t rc;
    uint32_t iov_count;

    /* For the general case always use the user data description */
    description = pConvertor->use_desc->desc;

    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc   = pStack->index;
    count_desc = (int32_t)pStack->count;
    disp_desc  = pStack->disp;
    pStack--;
    pConvertor->stack_pos--;

    DDT_DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, description, "starting" );

    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {
        bConverted = 0;
        pInput = iov[iov_count].iov_base;
        iCount = iov[iov_count].iov_len;
        while( 1 ) {
            if( DT_END_LOOP == description[pos_desc].elem.common.type ) { /* end of the current loop */
                if( --(pStack->count) == 0 ) { /* end of loop */
                    if( pConvertor->stack_pos == 0 ) {
                        goto save_and_return;  /* completed */
                    }
                    pConvertor->stack_pos--;
                    pStack--;
                    pos_desc++;
                } else {
                    pos_desc = pStack->index + 1;
                    if( pStack->index == -1 ) {
                        pStack->disp += extent;
                    } else {
                        assert( DT_LOOP == description[pStack->index].elem.common.type );
                        pStack->disp += description[pStack->index].loop.extent;
                    }
                }
                count_desc = description[pos_desc].elem.count;
                disp_desc = description[pos_desc].elem.disp;
            }
            if( DT_LOOP == description[pos_desc].elem.common.type ) {
                do {
                    PUSH_STACK( pStack, pConvertor->stack_pos,
                                pos_desc, DT_LOOP, description[pos_desc].loop.loops, pStack->disp );
                    pos_desc++;
                } while( DT_LOOP == description[pos_desc].loop.common.type ); /* let's start another loop */
                DDT_DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, description, "advance loops" );
                /* update the current state */
                count_desc = description[pos_desc].elem.count;
                disp_desc = description[pos_desc].elem.disp;
            }
            while( description[pos_desc].elem.common.flags & DT_FLAG_DATA ) {
                /* now here we have a basic datatype */
                type = description[pos_desc].elem.common.type;
                rc = master->pFunctions[type]( pConvertor, count_desc,
                                               pInput, iCount, ompi_ddt_basicDatatypes[type]->size,
                                               pConvertor->pBaseBuf + pStack->disp + disp_desc,
                                               oCount, description[pos_desc].elem.extent, &advance );
                iCount -= advance;      /* decrease the available space in the buffer */
                pInput += advance;      /* increase the pointer to the buffer */
                bConverted += advance;
                if( rc != count_desc ) {
                    /* not all data has been converted. Keep the state */
                    count_desc -= rc;
                    disp_desc += rc * description[pos_desc].elem.extent;
                    if( iCount != 0 )
                        printf( "unpack there is still room in the input buffer %ld bytes\n", (long)iCount );
                    goto save_and_return;
                }
                pos_desc++;  /* advance to the next data */
                count_desc = description[pos_desc].elem.count;
                disp_desc = description[pos_desc].elem.disp;
                if( iCount == 0 )
                    goto save_and_return;  /* break if there is no more data in the buffer */
            }
        }
    save_and_return:
        pConvertor->bConverted += bConverted;  /* update the # of bytes already converted */
        iov[iov_count].iov_len = bConverted;           /* update the iovec length */
        total_bytes_converted += bConverted;
    }
    *max_data = total_bytes_converted;
    /* out of the loop: we have complete the data conversion or no more space
     * in the buffer.
     */
    if( pConvertor->remote_size == pConvertor->bConverted ) {
        pConvertor->flags |= CONVERTOR_COMPLETED;
        return 1;  /* I'm done */
    }

    /* I complete an element, next step I should go to the next one */
    PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, type,
                count_desc, disp_desc );

    return 0;
}

/**
 * This function will be used to unpack all datatypes that have the contiguous flag set.
 * Several types of datatypes match this criterion, not only the contiguous one, but
 * the ones that have gaps in the beginning and/or at the end but where the data to
 * be unpacked is contiguous. However, this function only work for homogeneous cases
 * and the datatype that are contiguous and where the extent is equal to the size are
 * taken in account directly in the ompi_convertor_unpack function (in convertor.c) for
 * the homogeneous case.
 */
int32_t
ompi_unpack_homogeneous_contig_function( ompi_convertor_t* pConv,
                                         struct iovec* iov,
                                         uint32_t* out_size,
                                         size_t* max_data )
{
    const ompi_datatype_t *pData = pConv->pDesc;
    char *user_memory, *packed_buffer;
    uint32_t iov_count, i;
    size_t bConverted, remaining, length, initial_bytes_converted = pConv->bConverted;
    dt_stack_t* stack = pConv->pStack;
    ptrdiff_t extent = pData->ub - pData->lb;
    ptrdiff_t initial_displ = pConv->use_desc->desc[pConv->use_desc->used].end_loop.first_elem_disp;

    DO_DEBUG( opal_output( 0, "unpack_homogeneous_contig( pBaseBuf %p, iov_count %d )\n",
                           pConv->pBaseBuf, *out_size ); );
    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {
        packed_buffer = (char*)iov[iov_count].iov_base;
        remaining = pConv->local_size - pConv->bConverted;
        if( remaining > (uint32_t)iov[iov_count].iov_len )
            remaining = iov[iov_count].iov_len;
        bConverted = remaining; /* how much will get unpacked this time */
        user_memory = pConv->pBaseBuf + initial_displ;

        if( (ptrdiff_t)pData->size == extent ) {
            user_memory += pConv->bConverted;
            DO_DEBUG( opal_output( 0, "unpack_homogeneous_contig( user_memory %p, packed_buffer %p length %lu\n",
                                   user_memory, packed_buffer, (unsigned long)remaining ); );

            /* contiguous data or basic datatype with count */
            OMPI_DDT_SAFEGUARD_POINTER( user_memory, remaining,
                                        pConv->pBaseBuf, pData, pConv->count );
            DO_DEBUG( opal_output( 0, "1. unpack contig dest %p src %p length %lu\n",
                                   user_memory, packed_buffer, (unsigned long)remaining ); );
            MEMCPY_CSUM( user_memory, packed_buffer, remaining, pConv );
        } else {
            user_memory += stack[0].disp + stack[1].disp;

            DO_DEBUG( opal_output( 0, "unpack_homogeneous_contig( user_memory %p, packed_buffer %p length %lu\n",
                                   user_memory, packed_buffer, (unsigned long)remaining ); );

            length = pConv->bConverted / pData->size;  /* already done */
            length = pConv->bConverted - length * pData->size;  /* how much of the last data we convert */

            /* complete the last copy */
            if( length != 0 ) {
                length = pData->size - length;
                if( length <= remaining ) {
                    OMPI_DDT_SAFEGUARD_POINTER( user_memory, length, pConv->pBaseBuf,
                                                pData, pConv->count );
                    DO_DEBUG( opal_output( 0, "2. unpack dest %p src %p length %lu\n",
                                           user_memory, packed_buffer, (unsigned long)length ); );
                    MEMCPY_CSUM( user_memory, packed_buffer, length, pConv );
                    packed_buffer += length;
                    user_memory   += (extent - (pData->size - length));
                    remaining     -= length;
                }
            }
            for( i = 0; pData->size <= remaining; i++ ) {
                OMPI_DDT_SAFEGUARD_POINTER( user_memory, pData->size, pConv->pBaseBuf,
                                            pData, pConv->count );
                DO_DEBUG( opal_output( 0, "3. unpack dest %p src %p length %lu\n",
                                       user_memory, packed_buffer, (unsigned long)pData->size ); );
                MEMCPY_CSUM( user_memory, packed_buffer, pData->size, pConv );
                packed_buffer += pData->size;
                user_memory   += extent;
                remaining     -= pData->size;
            }
            stack[0].disp = (intptr_t)user_memory - (intptr_t)pConv->pBaseBuf - initial_displ;
            stack[1].disp = remaining;
            /* copy the last bits */
            if( remaining != 0 ) {
                OMPI_DDT_SAFEGUARD_POINTER( user_memory, remaining, pConv->pBaseBuf,
                                            pData, pConv->count );
                DO_DEBUG( opal_output( 0, "4. unpack dest %p src %p length %lu\n",
                                       user_memory, packed_buffer, (unsigned long)remaining ); );
                MEMCPY_CSUM( user_memory, packed_buffer, remaining, pConv );
                user_memory += remaining;
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

/**
 * This function handle partial types. Depending on the send operation it might happens
 * that we receive only a partial type (always predefined type). In fact the outcome is
 * that the unpack has to be done in 2 steps. As there is no way to know if the other
 * part of the datatype is already received, we need to use a trick to handle this special
 * case. The trick is to fill the missing part with some well known value, unpack the data
 * as if it was completely received, and then move into the user memory only the bytes
 * that don't match th wekk known value. This approach work as long as there is no need
 * for more than structural changes. They will not work for cases where we will have to
 * change the content of the data (as in all conversions that require changing the size
 * of the exponent or mantissa).
 */
static inline uint32_t
ompi_unpack_partial_datatype( ompi_convertor_t* pConvertor, dt_elem_desc_t* pElem,
                              char* partial_data,
                              ptrdiff_t start_position, ptrdiff_t end_position,
                              char** user_buffer )
{
    char unused_byte = 0x7F, saved_data[16];
    char temporary[16], *temporary_buffer = temporary;
    char* real_data = *user_buffer + pElem->elem.disp;
    uint32_t i, length, count_desc = 1;
    size_t data_length = ompi_ddt_basicDatatypes[pElem->elem.common.type]->size;

    DO_DEBUG( opal_output( 0, "unpack partial data start %d end %d data_length %lu user %p\n"
                           "\tbConverted %lu total_length %lu count %d\n",
                           start_position, end_position, (unsigned long)data_length, *user_buffer,
                           (unsigned long)pConvertor->bConverted, (unsigned long)pConvertor->local_size, pConvertor->count ); );

    /* Find a byte that is not used in the partial buffer */
 find_unused_byte:
    length = (uint32_t)(end_position - start_position);
    for( i = 0; i < length; i++ ) {
        if( unused_byte == partial_data[i] ) {
            unused_byte--;
            goto find_unused_byte;
        }
    }

    /* Copy and fill the rest of the buffer with the unused byte */
    memset( temporary, unused_byte, data_length );
    MEMCPY( temporary + start_position, partial_data, (end_position - start_position) );

    /* Save the content of the user memory */
    MEMCPY( saved_data, real_data, data_length );

    /* Then unpack the data into the user memory */
    UNPACK_PREDEFINED_DATATYPE( pConvertor, pElem, count_desc,
                                temporary_buffer, *user_buffer, data_length );

    /* reload the length as it is reset by the macro */
    data_length = ompi_ddt_basicDatatypes[pElem->elem.common.type]->size;

    /* For every occurence of the unused byte move data from the saved
     * buffer back into the user memory.
     */
    for( i = 0; i < data_length; i++ ) {
        if( unused_byte == real_data[i] )
            real_data[i] = saved_data[i];
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
ompi_generic_simple_unpack_function( ompi_convertor_t* pConvertor,
                                     struct iovec* iov, uint32_t* out_size,
                                     size_t* max_data )
{
    dt_stack_t* pStack;                /* pointer to the position on the stack */
    uint32_t pos_desc;                 /* actual position in the description of the derived datatype */
    uint32_t count_desc;               /* the number of items already done in the actual pos_desc */
    uint16_t type = DT_MAX_PREDEFINED; /* type at current position */
    size_t total_unpacked = 0;         /* total size unpacked this time */
    dt_elem_desc_t* description;
    dt_elem_desc_t* pElem;
    const ompi_datatype_t *pData = pConvertor->pDesc;
    char *user_memory_base, *packed_buffer;
    size_t iov_len_local;
    uint32_t iov_count;

    DO_DEBUG( opal_output( 0, "ompi_convertor_generic_simple_unpack( %p, {%p, %lu}, %u )\n",
                           (void*)pConvertor, iov[0].iov_base, (unsigned long)iov[0].iov_len, *out_size ); );

    description = pConvertor->use_desc->desc;

    /* For the first step we have to add both displacement to the source. After in the
     * main while loop we will set back the source_base to the correct value. This is
     * due to the fact that the convertor can stop in the middle of a data with a count
     */
    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc          = pStack->index;
    user_memory_base  = pConvertor->pBaseBuf + pStack->disp;
    count_desc        = (uint32_t)pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &(description[pos_desc]); 
    user_memory_base += pStack->disp;

    DO_DEBUG( opal_output( 0, "unpack start pos_desc %d count_desc %d disp %ld\n"
                           "stack_pos %d pos_desc %d count_desc %d disp %ld\n",
                           pos_desc, count_desc, (long)(user_memory_base - pConvertor->pBaseBuf),
                           pConvertor->stack_pos, pStack->index, (int)pStack->count, (long)(pStack->disp) ); );

    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {

        packed_buffer = iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;
        if( 0 != pConvertor->partial_length ) {
            size_t element_length = ompi_ddt_basicDatatypes[pElem->elem.common.type]->size;
            size_t missing_length = element_length - pConvertor->partial_length;

            assert( pElem->elem.common.flags & DT_FLAG_DATA );
            COMPUTE_CSUM( packed_buffer, missing_length, pConvertor );
            ompi_unpack_partial_datatype( pConvertor, pElem,
                                          packed_buffer,
                                          pConvertor->partial_length, element_length,
                                          &user_memory_base );
            --count_desc;
            if( 0 == count_desc ) {
                user_memory_base = pConvertor->pBaseBuf + pStack->disp;
                pos_desc++;  /* advance to the next data */
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
            }
            packed_buffer += missing_length;
            iov_len_local -= missing_length;
            pConvertor->partial_length = 0;  /* nothing more inside */
        }
        while( 1 ) {
            while( pElem->elem.common.flags & DT_FLAG_DATA ) {
                /* now here we have a basic datatype */
                UNPACK_PREDEFINED_DATATYPE( pConvertor, pElem, count_desc,
                                            packed_buffer, user_memory_base, iov_len_local );
                if( 0 == count_desc ) {  /* completed */
                    user_memory_base = pConvertor->pBaseBuf + pStack->disp;
                    pos_desc++;  /* advance to the next data */
                    UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
                    continue;
                }
                type = pElem->elem.common.type;
                assert( type < DT_MAX_PREDEFINED );
                if( 0 != iov_len_local ) {
                    char* temp = user_memory_base;
                    /* We have some partial data here. Let's copy it into the convertor
                     * and keep it hot until the next round.
                     */
                    assert( iov_len_local < ompi_ddt_basicDatatypes[type]->size );
                    COMPUTE_CSUM( packed_buffer, iov_len_local, pConvertor );

                    ompi_unpack_partial_datatype( pConvertor, pElem,
                                                  packed_buffer, 0, iov_len_local,
                                                  &temp );

                    pConvertor->partial_length = (uint32_t)iov_len_local;
                    iov_len_local = 0;
                }
                goto complete_loop;
            }
            if( DT_END_LOOP == pElem->elem.common.type ) { /* end of the current loop */
                DO_DEBUG( opal_output( 0, "unpack end_loop count %d stack_pos %d pos_desc %d disp %ld space %lu\n",
                                       (int)pStack->count, pConvertor->stack_pos, pos_desc,
                                      (long)pStack->disp, (unsigned long)iov_len_local ); );
                if( --(pStack->count) == 0 ) { /* end of loop */
                    if( pConvertor->stack_pos == 0 ) {
                        /* Force the conversion to stop by lowering the number of iovecs. */
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
                user_memory_base = pConvertor->pBaseBuf + pStack->disp;
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
                DO_DEBUG( opal_output( 0, "unpack new_loop count %d stack_pos %d pos_desc %d disp %ld space %lu\n",
                                       (int)pStack->count, pConvertor->stack_pos, pos_desc,
                                       (long)pStack->disp, (unsigned long)iov_len_local ); );
            }
            if( DT_LOOP == pElem->elem.common.type ) {
                ptrdiff_t local_disp = (ptrdiff_t)user_memory_base;
                if( pElem->loop.common.flags & DT_FLAG_CONTIGUOUS ) {
                    UNPACK_CONTIGUOUS_LOOP( pConvertor, pElem, count_desc, 
                                            packed_buffer, user_memory_base, iov_len_local );
                    if( 0 == count_desc ) {  /* completed */
                        pos_desc += pElem->loop.items + 1;
                        goto update_loop_description;
                    }
                    /* Save the stack with the correct last_count value. */
                }
                local_disp = (ptrdiff_t)user_memory_base - local_disp;
                PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, DT_LOOP, count_desc,
                            pStack->disp + local_disp);
                pos_desc++;
            update_loop_description:  /* update the current state */
                user_memory_base = pConvertor->pBaseBuf + pStack->disp;
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
                DDT_DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElem, "advance loop" );
                continue;
            }
        }
    complete_loop:
        iov[iov_count].iov_len -= iov_len_local;  /* update the amount of valid data */
        total_unpacked += iov[iov_count].iov_len;
    }
    *max_data = total_unpacked;
    pConvertor->bConverted += total_unpacked;  /* update the already converted bytes */
    *out_size = iov_count;
    if( pConvertor->bConverted == pConvertor->remote_size ) {
        pConvertor->flags |= CONVERTOR_COMPLETED;
        return 1;
    }
    /* I complete an element, next step I should go to the next one */
    PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, DT_BYTE, count_desc,
                user_memory_base - pStack->disp - pConvertor->pBaseBuf );
    DO_DEBUG( opal_output( 0, "unpack save stack stack_pos %d pos_desc %d count_desc %d disp %ld\n",
                           pConvertor->stack_pos, pStack->index, (int)pStack->count, (long)pStack->disp ); );
    return 0;
}
