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
                              size_t* max_data,
                              int32_t* freeAfter )
{
    dt_stack_t* pStack;    /* pointer to the position on the stack */
    uint32_t pos_desc;     /* actual position in the description of the derived datatype */
    int count_desc;        /* the number of items already done in the actual pos_desc */
    int type = DT_CHAR;    /* type at current position */
    uint32_t advance;      /* number of bytes that we should advance the buffer */
    long disp_desc = 0;    /* compute displacement for truncated data */
    int bConverted = 0;    /* number of bytes converted this time */
    const ompi_convertor_master_t* master = pConvertor->master;
    dt_elem_desc_t* pElems;
    int oCount = (pConvertor->pDesc->ub - pConvertor->pDesc->lb) * pConvertor->count;
    char* pInput;
    int iCount, rc;
    uint32_t iov_count, total_bytes_converted = 0;

    /* For the general case always use the user data description */
    pElems = pConvertor->pDesc->desc.desc;

    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc   = pStack->index;
    count_desc = pStack->count;
    disp_desc  = pStack->disp;
    pStack--;
    pConvertor->stack_pos--;

    DDT_DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElems, "starting" );
    DUMP( "remember position on stack %d last_elem at %d\n", pConvertor->stack_pos, pos_desc );
    DUMP( "top stack info {index = %d, count = %d}\n", pStack->index, pStack->count );

    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {
        bConverted = 0;
        pInput = iov[iov_count].iov_base;
        iCount = iov[iov_count].iov_len;
        while( 1 ) {
            if( DT_END_LOOP == pElems[pos_desc].elem.common.type ) { /* end of the current loop */
                if( --(pStack->count) == 0 ) { /* end of loop */
                    if( pConvertor->stack_pos == 0 ) {
                        goto save_and_return;  /* completed */
                    }
                    pConvertor->stack_pos--;
                    pStack--;
                }

                if( pStack->index == -1 ) {
                    pStack->disp += (pConvertor->pDesc->ub - pConvertor->pDesc->lb);
                } else {
                    assert( DT_LOOP == pElems[pStack->index].elem.common.type );
                    pStack->disp += pElems[pStack->index].loop.extent;
                }
                pos_desc = pStack->index + 1;
                count_desc = pElems[pos_desc].elem.count;
                disp_desc = pElems[pos_desc].elem.disp;
            }
            if( DT_LOOP == pElems[pos_desc].elem.common.type ) {
                do {
                    PUSH_STACK( pStack, pConvertor->stack_pos,
                                pos_desc, DT_LOOP, pElems[pos_desc].loop.loops,
                                pStack->disp, pos_desc + pElems[pos_desc].loop.items + 1 );
                    pos_desc++;
                } while( DT_LOOP == pElems[pos_desc].loop.common.type ); /* let's start another loop */
                DDT_DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElems, "advance loops" );
                /* update the current state */
                count_desc = pElems[pos_desc].elem.count;
                disp_desc = pElems[pos_desc].elem.disp;
            }
            while( pElems[pos_desc].elem.common.flags & DT_FLAG_DATA ) {
                /* now here we have a basic datatype */
                type = pElems[pos_desc].elem.common.type;
                rc = master->pFunctions[type]( pConvertor, count_desc,
                                               pInput, iCount, ompi_ddt_basicDatatypes[type]->size,
                                               pConvertor->pBaseBuf + pStack->disp + disp_desc,
                                               oCount, pElems[pos_desc].elem.extent, &advance );
                iCount -= advance;      /* decrease the available space in the buffer */
                pInput += advance;      /* increase the pointer to the buffer */
                bConverted += advance;
                if( rc != count_desc ) {
                    /* not all data has been converted. Keep the state */
                    count_desc -= rc;
                    disp_desc += rc * pElems[pos_desc].elem.extent;
                    if( iCount != 0 )
                        printf( "unpack there is still room in the input buffer %d bytes\n", iCount );
                    goto save_and_return;
                }
                pos_desc++;  /* advance to the next data */
                count_desc = pElems[pos_desc].elem.count;
                disp_desc = pElems[pos_desc].elem.disp;
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
                count_desc, disp_desc, pos_desc );

    return 0;
}

int32_t
ompi_unpack_homogeneous_contig_function( ompi_convertor_t* pConv,
                                         struct iovec* iov,
                                         uint32_t* out_size,
                                         size_t* max_data,
                                         int32_t* freeAfter )
{
    const ompi_datatype_t *pData = pConv->pDesc;
    char *user_memory, *packed_buffer;
    uint32_t iov_count, initial_bytes_converted = pConv->bConverted;
    long extent = pData->ub - pData->lb;
    uint32_t bConverted, length, remaining, i;
    dt_stack_t* stack = &(pConv->pStack[1]);

    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {
        packed_buffer = (char*)iov[iov_count].iov_base;
        remaining = pConv->local_size - pConv->bConverted;
        if( remaining > (uint32_t)iov[iov_count].iov_len )
            remaining = iov[iov_count].iov_len;
        bConverted = remaining; /* how much will get unpacked this time */
        user_memory = pConv->pBaseBuf + pData->true_lb;

        /*opal_output( 0, "unpack_homogeneous_contig( user_memory %p, packed_buffer %p length %d\n",
          user_memory, packed_buffer, remaining );*/

        if( (long)pData->size == extent ) {
            user_memory += pConv->bConverted;

            /* contiguous data or basic datatype with count */
            OMPI_DDT_SAFEGUARD_POINTER( user_memory, remaining,
                                        pConv->pBaseBuf, pData, pConv->count );
            /*opal_output( 0, "1. unpack contig dest %p src %p length %d\n",
              user_memory, packed_buffer, remaining );*/
            MEMCPY_CSUM( user_memory, packed_buffer, remaining, pConv );
        } else {
            user_memory += stack->disp;

            length = pConv->bConverted / pData->size;  /* already done */
            length = pConv->bConverted - length * pData->size;  /* still left on the last element */
            /* complete the last copy */
            if( length != 0 ) {
                OMPI_DDT_SAFEGUARD_POINTER( user_memory, length, pConv->pBaseBuf,
                                            pData, pConv->count );
                /*opal_output( 0, "1. unpack dest %p src %p length %d\n",
                  user_memory, packed_buffer, length );*/
                MEMCPY_CSUM( user_memory, packed_buffer, length, pConv );
                packed_buffer += length;
                user_memory   += (extent - (pData->size - length));
                remaining     -= length;
            }
            for( i = 0; pData->size <= remaining; i++ ) {
                OMPI_DDT_SAFEGUARD_POINTER( user_memory, pData->size, pConv->pBaseBuf,
                                            pData, pConv->count );
                /*opal_output( 0, "2. unpack dest %p src %p length %d\n",
                  user_memory, packed_buffer, pData->size );*/
                MEMCPY_CSUM( user_memory, packed_buffer, pData->size, pConv );
                packed_buffer += pData->size;
                user_memory   += extent;
                remaining     -= pData->size;
            }
            /* copy the last bits */
            if( remaining != 0 ) {
                OMPI_DDT_SAFEGUARD_POINTER( user_memory, remaining, pConv->pBaseBuf,
                                            pData, pConv->count );
                /*opal_output( 0, "3. unpack dest %p src %p length %d\n",
                  user_memory, packed_buffer, remaining );*/
                MEMCPY_CSUM( user_memory, packed_buffer, remaining, pConv );
                user_memory += remaining;
            }
            stack->disp = user_memory - pData->true_lb - pConv->pBaseBuf;  /* save the position */
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
 * - for the DT_LOOP type the DT_CONTIGUOUS flag set means that the content of the loop is
 *   contiguous but with a gap in the begining or at the end.
 * - the DT_CONTIGUOUS flag for the type DT_END_LOOP is meaningless.
 */
int32_t
ompi_generic_simple_unpack_function( ompi_convertor_t* pConvertor,
                                     struct iovec* iov, uint32_t* out_size,
                                     size_t* max_data,
                                     int32_t* freeAfter )
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
    uint32_t iov_len_local, iov_count;

    DO_DEBUG( opal_output( 0, "ompi_convertor_generic_simple_unpack( %p, {%p, %lu}, %u )\n",
                           (void*)pConvertor, iov[0].iov_base, (size_t)iov[0].iov_len, *out_size ); );

    description = pConvertor->use_desc->desc;

    /* For the first step we have to add both displacement to the source. After in the
     * main while loop we will set back the source_base to the correct value. This is
     * due to the fact that the convertor can stop in the middle of a data with a count
     */
    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc          = pStack->index;
    user_memory_base += pStack->disp;
    count_desc        = pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &(description[pos_desc]); 
    user_memory_base = pConvertor->pBaseBuf + pStack->disp;

    DO_DEBUG( opal_output( 0, "unpack start pos_desc %d count_desc %d disp %ld\n"
                           "stack_pos %d pos_desc %d count_desc %d disp %ld\n",
                           pos_desc, count_desc, user_memory_base - pConvertor->pBaseBuf,
                           pConvertor->stack_pos, pStack->index, pStack->count, pStack->disp ); );

    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {

        packed_buffer = iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;
        if( 0 != pConvertor->storage.length ) {
            uint32_t element_length = ompi_ddt_basicDatatypes[pElem->elem.common.type]->size;
            uint32_t missing_length = element_length - pConvertor->storage.length;

            assert( pElem->elem.common.flags & DT_FLAG_DATA );
#if defined(CHECKSUM)
            pConvertor->checksum -= OPAL_CSUM(pConvertor->storage.data, pConvertor->storage.length);
#endif
            memcpy(pConvertor->storage.data + pConvertor->storage.length, packed_buffer, missing_length);
            packed_buffer = pConvertor->storage.data;
            DO_DEBUG( opal_output( 0, "unpack pending from the last unpack %d out of %d bytes\n",
                                   pConvertor->storage.length, ompi_ddt_basicDatatypes[pElem->elem.common.type]->size ); );
            UNPACK_PREDEFINED_DATATYPE( pConvertor, pElem, count_desc,
                                        packed_buffer, user_memory_base, element_length );
            if( 0 == count_desc ) {
                user_memory_base = pConvertor->pBaseBuf + pStack->disp;
                pos_desc++;  /* advance to the next data */
                UPDATE_INTERNAL_COUNTERS( description, pos_desc, pElem, count_desc );
            }
            assert( 0 == element_length );
            packed_buffer = (char*)iov[iov_count].iov_base + missing_length;
            iov_len_local -= missing_length;
            pConvertor->bConverted += element_length;
            pConvertor->storage.length = 0;  /* nothing more inside */
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
                    /* We have some partial data here. Let's copy it into the convertor
                     * and keep it hot until the next round.
                     */
                    assert( iov_len_local < ompi_ddt_basicDatatypes[type]->size );
                    MEMCPY_CSUM( pConvertor->storage.data, packed_buffer, iov_len_local, pConvertor );
                    DO_DEBUG( opal_output( 0, "Saving %d bytes for the next call\n", iov_len_local ); );
                    pConvertor->storage.length = iov_len_local;
                    iov_len_local = 0;
                }
                goto complete_loop;
            }
            if( DT_END_LOOP == pElem->elem.common.type ) { /* end of the current loop */
                DO_DEBUG( opal_output( 0, "unpack end_loop count %d stack_pos %d pos_desc %d disp %ld space %d\n",
                                       pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp, iov_len_local ); );
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
                DO_DEBUG( opal_output( 0, "unpack new_loop count %d stack_pos %d pos_desc %d disp %ld space %d\n",
                                       pStack->count, pConvertor->stack_pos, pos_desc, pStack->disp, iov_len_local ); );
            }
            if( DT_LOOP == pElem->elem.common.type ) {
                long local_disp = (long)user_memory_base;
                if( pElem->loop.common.flags & DT_FLAG_CONTIGUOUS ) {
                    UNPACK_CONTIGUOUS_LOOP( pConvertor, pElem, count_desc, 
                                            packed_buffer, user_memory_base, iov_len_local );
                    if( 0 == count_desc ) {  /* completed */
                        pos_desc += pElem->loop.items + 1;
                        goto update_loop_description;
                    }
                    /* Save the stack with the correct last_count value. */
                }
                local_disp = (long)user_memory_base - local_disp;
                PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, DT_LOOP, count_desc,
                            pStack->disp + local_disp, pos_desc + pElem->elem.disp + 1);
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
        pConvertor->bConverted += iov[iov_count].iov_len;  /* update the already converted bytes */
    }
    *max_data = total_unpacked;
    *out_size = iov_count;
    if( pConvertor->bConverted == pConvertor->remote_size ) {
        pConvertor->flags |= CONVERTOR_COMPLETED;
        return 1;
    }
    /* I complete an element, next step I should go to the next one */
    PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, DT_BYTE, count_desc,
                user_memory_base - pStack->disp - pConvertor->pBaseBuf, pos_desc );
    DO_DEBUG( opal_output( 0, "unpack save stack stack_pos %d pos_desc %d count_desc %d disp %ld\n",
                           pConvertor->stack_pos, pStack->index, pStack->count, pStack->disp ); );
    return 0;
}
