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

#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif

#include "ompi/datatype/datatype.h"
#include "ompi/datatype/convertor.h"
#include "ompi/datatype/datatype_internal.h"
#include "ompi/datatype/datatype_checksum.h"
#include "ompi/datatype/datatype_prototypes.h"
#include "ompi/datatype/convertor_internal.h"
#include "ompi/datatype/dt_arch.h"

extern size_t ompi_ddt_local_sizes[DT_MAX_PREDEFINED];
extern int ompi_convertor_create_stack_with_pos_general( ompi_convertor_t* convertor,
                                                         int starting_point, const int* sizes );

static void ompi_convertor_construct( ompi_convertor_t* convertor )
{
    convertor->pStack         = convertor->static_stack;
    convertor->stack_size     = DT_STATIC_STACK_SIZE;
    convertor->partial_length = 0;
    convertor->remoteArch     = ompi_mpi_local_arch;
}

static void ompi_convertor_destruct( ompi_convertor_t* convertor )
{
    ompi_convertor_cleanup( convertor );
}

OBJ_CLASS_INSTANCE(ompi_convertor_t, opal_object_t, ompi_convertor_construct, ompi_convertor_destruct );

static ompi_convertor_master_t* ompi_convertor_master_list = NULL;

extern conversion_fct_t ompi_ddt_heterogeneous_copy_functions[DT_MAX_PREDEFINED];
extern conversion_fct_t ompi_ddt_copy_functions[DT_MAX_PREDEFINED];

void ompi_convertor_destroy_masters( void )
{
    ompi_convertor_master_t* master = ompi_convertor_master_list;

    while( NULL != master ) {
        ompi_convertor_master_list = master->next;
        master->next = NULL;
        /* Cleanup the conversion function if not one of the defaults */
        if( (master->pFunctions != ompi_ddt_heterogeneous_copy_functions) &&
            (master->pFunctions != ompi_ddt_copy_functions) )
            free( master->pFunctions );

        free( master );
        master = ompi_convertor_master_list;
    }
}

/**
 * Find or create a convertor suitable for the remote architecture. If there
 * is already a master convertor for this architecture then return it.
 * Otherwise, create and initialize a full featured master convertor.
 */
ompi_convertor_master_t*
ompi_convertor_find_or_create_master( uint32_t remote_arch )
{
    ompi_convertor_master_t* master = ompi_convertor_master_list;
    int i;
    size_t* remote_sizes;

    while( NULL != master ) {
        if( master->remote_arch == remote_arch )
            return master;
        master = master->next;
    }
    /* Create a new convertor matching the specified architecture and add it to the
     * master convertor list.
     */
    master = (ompi_convertor_master_t*)malloc( sizeof(ompi_convertor_master_t) );
    master->next = ompi_convertor_master_list;
    ompi_convertor_master_list = master;
    master->remote_arch = remote_arch;
    master->flags       = 0;
    master->hetero_mask = 0;
    /* Most of the sizes will be identical, so for now just make a copy of
     * the local ones. As master->remote_sizes is defined as being an array of
     * consts we have to manually cast it before using it for writing purposes.
     */
    remote_sizes = (size_t*)master->remote_sizes;
    for( i = DT_CHAR; i < DT_MAX_PREDEFINED; i++ ) {
        remote_sizes[i] = ompi_ddt_local_sizes[i];
    }
    /**
     * If the local and remote architecture are the same there is no need
     * to check for the remote data sizes. They will always be the same as
     * the local ones.
     */
    if( master->remote_arch == ompi_mpi_local_arch ) {
        master->pFunctions = ompi_ddt_copy_functions;
        master->flags |= CONVERTOR_HOMOGENEOUS;
        return master;
    }

    /* Find out the remote bool size */
    if( ompi_arch_checkmask( &master->remote_arch, OMPI_ARCH_BOOLIS8 ) ) {
        remote_sizes[DT_CXX_BOOL] = 1;
    } else if( ompi_arch_checkmask( &master->remote_arch, OMPI_ARCH_BOOLIS16 ) ) {
        remote_sizes[DT_CXX_BOOL] = 2;
    } else if( ompi_arch_checkmask( &master->remote_arch, OMPI_ARCH_BOOLIS32 ) ) {
        remote_sizes[DT_CXX_BOOL] = 4;
    } else {
        opal_output( 0, "Unknown sizeof(bool) for the remote architecture\n" );
    }

    /* check the length of the long */
    if( ompi_arch_checkmask( &master->remote_arch, OMPI_ARCH_LONGIS64 ) ) {
        remote_sizes[DT_LONG]               = 8;
        remote_sizes[DT_UNSIGNED_LONG]      = 8;
        remote_sizes[DT_LONG_LONG_INT]      = 8;
        remote_sizes[DT_UNSIGNED_LONG_LONG] = 8;
    }
    /* find out the remote logical size. It can happens that the size will be
     * unknown (if Fortran is not supported on the remote library). If this is
     * the case, just let the remote logical size to match the local size.
     */
    if( ompi_arch_checkmask( &master->remote_arch, OMPI_ARCH_LOGICALIS8 ) ) {
        remote_sizes[DT_LOGIC] = 1;
    } else if( ompi_arch_checkmask( &master->remote_arch, OMPI_ARCH_LOGICALIS16 ) ) {
        remote_sizes[DT_LOGIC] = 2;
    } else if( ompi_arch_checkmask( &master->remote_arch, OMPI_ARCH_LOGICALIS32 ) ) {
        remote_sizes[DT_LOGIC] = 4;
    } else {
        opal_output( 0, "Unknown sizeof(fortran logical) for the remote architecture\n" );
    }

    /**
     * Now we can compute the conversion mask. For all sizes where the remote
     * and local architecture differ a conversion is needed. Moreover, if the
     * 2 architectures don't have the same endianess all data with a length
     * over 2 bytes (with the exception of logicals) have to be byte-swapped.
     */
    for( i = DT_CHAR; i < DT_MAX_PREDEFINED; i++ ) {
        if( remote_sizes[i] != ompi_ddt_local_sizes[i] )
            master->hetero_mask |= (((uint64_t)1) << i);
    }
    if( ompi_arch_checkmask( &master->remote_arch, OMPI_ARCH_ISBIGENDIAN ) !=
        ompi_arch_checkmask( &ompi_mpi_local_arch, OMPI_ARCH_ISBIGENDIAN ) ) {
        uint64_t hetero_mask = 0;

        for( i = DT_CHAR; i < DT_MAX_PREDEFINED; i++ ) {
            if( remote_sizes[i] > 1 )
                hetero_mask |= (((uint64_t)1) << i);
        }
        hetero_mask &= ~((((uint64_t)1) << DT_LOGIC) | (((uint64_t)1) << DT_CXX_BOOL));
        master->hetero_mask |= hetero_mask;
    }
    master->pFunctions = (conversion_fct_t*)malloc( sizeof(ompi_ddt_heterogeneous_copy_functions) );
    /**
     * Usually the heterogeneous functions are slower than the copy ones. Let's
     * try to minimize the usage of the heterogeneous versions.
     */
    for( i = DT_CHAR; i < DT_MAX_PREDEFINED; i++ ) {
        if( master->hetero_mask & (((uint64_t)1) << i) )
            master->pFunctions[i] = ompi_ddt_heterogeneous_copy_functions[i];
        else
            master->pFunctions[i] = ompi_ddt_copy_functions[i];
    }
    /* We're done so far, return the mater convertor */
    return master;
}

ompi_convertor_t* ompi_convertor_create( int32_t remote_arch, int32_t mode )
{
    ompi_convertor_t* convertor = OBJ_NEW(ompi_convertor_t);
    ompi_convertor_master_t* master;

    master = ompi_convertor_find_or_create_master( remote_arch );

    convertor->remoteArch = remote_arch;
    convertor->stack_pos  = 0;
    convertor->flags      = master->flags;
    convertor->master     = master;

    return convertor;
}

#define OMPI_CONVERTOR_SET_STATUS_BEFORE_PACK_UNPACK( CONVERTOR, IOV, OUT, MAX_DATA ) \
    do {                                                                \
        (CONVERTOR)->checksum = OPAL_CSUM_ZERO;                         \
        (CONVERTOR)->csum_ui1 = 0;                                      \
        (CONVERTOR)->csum_ui2 = 0;                                      \
                                                                        \
        /* protect against over packing data */                         \
        if( (CONVERTOR)->flags & CONVERTOR_COMPLETED ) {                \
            (IOV)[0].iov_len = 0;                                       \
            *(OUT) = 0;                                                 \
            *(MAX_DATA) = 0;                                            \
            return 1;  /* nothing to do */                              \
        }                                                               \
        assert( (CONVERTOR)->bConverted < (CONVERTOR)->local_size );    \
    } while(0)

/* 
 * Return 0 if everything went OK and if there is still room before the complete
 *          conversion of the data (need additional call with others input buffers )
 *        1 if everything went fine and the data was completly converted
 *       -1 something wrong occurs.
 */
int32_t ompi_convertor_pack( ompi_convertor_t* pConv,
                             struct iovec* iov, uint32_t* out_size,
                             size_t* max_data )
{
    OMPI_CONVERTOR_SET_STATUS_BEFORE_PACK_UNPACK( pConv, iov, out_size, max_data );

    if( !(pConv->flags & CONVERTOR_WITH_CHECKSUM) &&
        (pConv->flags & DT_FLAG_NO_GAPS) ) {
        /* We are doing conversion on a predefined contiguous datatype. The
         * convertor contain minimal informations, we only use the bConverted
         * to manage the conversion.
         */
        uint32_t i;
        size_t initial_bConverted = pConv->bConverted;
        size_t pending_length = pConv->local_size - pConv->bConverted;
        char* base_pointer;

        if( (*max_data) < pending_length )
            pending_length = (*max_data);

        for( i = 0; (i < *out_size) && (0 != pending_length); i++ ) {
            base_pointer = pConv->pBaseBuf + pConv->bConverted + pConv->pDesc->true_lb;

            if( iov[i].iov_len > pending_length )
                iov[i].iov_len = pending_length;

            if( NULL == iov[i].iov_base ) {
                iov[i].iov_base = base_pointer;
            } else {
                MEMCPY( iov[i].iov_base, base_pointer, iov[i].iov_len );
            }
            pConv->bConverted += iov[i].iov_len;
            pending_length -= iov[i].iov_len;
        }
        *out_size = i;
        *max_data = pConv->bConverted - initial_bConverted;
        if( pConv->bConverted == pConv->local_size ) {
            pConv->flags |= CONVERTOR_COMPLETED;
            return 1;
        }
        return 0;
    }

    return pConv->fAdvance( pConv, iov, out_size, max_data );
}

int32_t ompi_convertor_unpack( ompi_convertor_t* pConv,
                               struct iovec* iov, uint32_t* out_size,
                               size_t* max_data )
{
    OMPI_CONVERTOR_SET_STATUS_BEFORE_PACK_UNPACK( pConv, iov, out_size, max_data );

    if( !(pConv->flags & CONVERTOR_WITH_CHECKSUM) &&
        ((pConv->flags & (CONVERTOR_HOMOGENEOUS | DT_FLAG_NO_GAPS)) ==
         (CONVERTOR_HOMOGENEOUS | DT_FLAG_NO_GAPS)) ) {
        /* We are doing conversion on a contiguous datatype on a homogeneous
         * environment. The convertor contain minimal informations, we only
         * use the bConverted to manage the conversion.
         */
        uint32_t i;
        char* base_pointer;

        *max_data = pConv->bConverted;
        base_pointer = pConv->pBaseBuf + pConv->bConverted + 
            pConv->use_desc->desc[pConv->use_desc->used].end_loop.first_elem_disp;
        for( i = 0; i < *out_size; i++ ) {
            if( (pConv->bConverted + iov[i].iov_len) >= pConv->local_size ) {
                goto predefined_data_unpack;
            }
            MEMCPY( base_pointer, iov[i].iov_base, iov[i].iov_len );
            /*opal_output( 0, "copy at %p %d bytes [initial ptr %p]\n", base_pointer,
              iov[i].iov_len, pConv->pBaseBuf );*/
            pConv->bConverted += iov[i].iov_len;
            base_pointer += iov[i].iov_len;
        }
        *max_data = pConv->bConverted - (*max_data);
        return 0;
    predefined_data_unpack:
        iov[i].iov_len = pConv->local_size - pConv->bConverted;
        MEMCPY( base_pointer, iov[i].iov_base, iov[i].iov_len );
        /*opal_output( 0, "copy at %p %d bytes [initial ptr %p] *last*\n", base_pointer,
          iov[i].iov_len, pConv->pBaseBuf );*/
        pConv->bConverted = pConv->local_size;
        *out_size = i + 1;
        *max_data = pConv->bConverted - (*max_data);
        pConv->flags |= CONVERTOR_COMPLETED;
        return 1;
    }

    return pConv->fAdvance( pConv, iov, out_size, max_data );
}

static inline
int ompi_convertor_create_stack_with_pos_contig( ompi_convertor_t* pConvertor,
                                                 size_t starting_point, const size_t* sizes )
{
    dt_stack_t* pStack;   /* pointer to the position on the stack */
    const ompi_datatype_t* pData = pConvertor->pDesc;
    dt_elem_desc_t* pElems;
    uint32_t count;
    ptrdiff_t extent;

    pStack = pConvertor->pStack;
    /* The prepare function already make the selection on which data representation
     * we have to use: normal one or the optimized version ?
     */
    pElems = pConvertor->use_desc->desc;

    count = (uint32_t)(starting_point / pData->size);
    extent = pData->ub - pData->lb;

    pStack[0].type     = DT_LOOP;  /* the first one is always the loop */
    pStack[0].count    = pConvertor->count - count;
    pStack[0].index    = -1;
    pStack[0].disp     = count * extent;

    /* now compute the number of pending bytes */
    count = (uint32_t)(starting_point - count * pData->size);
    /* we save the current displacement starting from the begining
     * of this data.
     */
    if( 0 == count ) {
        pStack[1].type     = pElems->elem.common.type;
        pStack[1].count    = pElems->elem.count;
        pStack[1].disp     = pElems->elem.disp;
    } else {
        pStack[1].type  = DT_BYTE;
        pStack[1].count = pData->size - count;
        pStack[1].disp  = pData->true_lb + count;
    }
    pStack[1].index    = 0;  /* useless */

    pConvertor->bConverted = starting_point;
    pConvertor->stack_pos = 1;
    assert( 0 == pConvertor->partial_length );
    return OMPI_SUCCESS;
}

static inline
int ompi_convertor_create_stack_at_begining( ompi_convertor_t* convertor,
                                             const size_t* sizes )
{
    dt_stack_t* pStack = convertor->pStack;
    dt_elem_desc_t* pElems;

    convertor->stack_pos      = 1;
    convertor->partial_length = 0;
    convertor->bConverted     = 0;
    /* Fill the first position on the stack. This one correspond to the
     * last fake DT_END_LOOP that we add to the data representation and
     * allow us to move quickly inside the datatype when we have a count.
     */
    pStack[0].index = -1;
    pStack[0].count = convertor->count;
    pStack[0].disp  = 0;
    /* The prepare function already make the selection on which data representation
     * we have to use: normal one or the optimized version ?
     */
    pElems = convertor->use_desc->desc;

    pStack[1].index = 0;
    pStack[1].disp = 0;
    if( pElems[0].elem.common.type == DT_LOOP ) {
        pStack[1].count = pElems[0].loop.loops;
    } else {
        pStack[1].count = pElems[0].elem.count;
    }
    return OMPI_SUCCESS;
}

int32_t ompi_convertor_set_position_nocheck( ompi_convertor_t* convertor,
                                             size_t* position )
{
    int32_t rc;

    /*
     * If we plan to rollback the convertor then first we have to set it
     * at the beginning.
     */
    if( (0 == (*position)) || ((*position) < convertor->bConverted) ) {
        rc = ompi_convertor_create_stack_at_begining( convertor, ompi_ddt_local_sizes );
        if( 0 == (*position) ) return rc;
    }
    if( convertor->flags & DT_FLAG_CONTIGUOUS ) {
        rc = ompi_convertor_create_stack_with_pos_contig( convertor, (*position),
                                                          ompi_ddt_local_sizes );
    } else {
        rc = ompi_convertor_generic_simple_position( convertor, position );
    }
    *position = convertor->bConverted;
    return rc;
}

/* This macro will initialize a convertor based on a previously created convertor. The idea
 * is the move outside these function the heavy selection of architecture features for the convertors.
 *
 * I consider here that the convertor is clean, either never initialized or already cleanup.
 */
#define OMPI_CONVERTOR_PREPARE( convertor, datatype, count, pUserBuf )  \
    {                                                                   \
        convertor->pBaseBuf        = (char*)pUserBuf;                   \
        convertor->count           = count;                             \
                                                                        \
        /* Compute the local and remote sizes */                        \
        convertor->local_size = convertor->count * datatype->size;      \
        /* Grab the datatype part of the flags */                       \
        convertor->flags         &= CONVERTOR_TYPE_MASK;                \
        convertor->flags         |= (CONVERTOR_DATATYPE_MASK & datatype->flags); \
        convertor->pDesc          = (ompi_datatype_t*)datatype;         \
                                                                        \
        /* If the data is empty we just mark the convertor as           \
         * completed. With this flag set the pack and unpack functions  \
         * will not do anything. In order to decrease the data          \
         * dependencies (and to speed-up this code) we will not test    \
         * the convertor->local_size but we can test the 2 components.  \
         */                                                             \
        if( (0 == convertor->count) || (0 == datatype->size) ) {        \
            convertor->flags |= CONVERTOR_COMPLETED;                    \
            convertor->local_size = convertor->remote_size = 0;         \
            return OMPI_SUCCESS;                                        \
        }                                                               \
                                                                        \
        convertor->flags |= CONVERTOR_HOMOGENEOUS;                      \
        if( convertor->remoteArch == ompi_mpi_local_arch ) {            \
            convertor->remote_size = convertor->local_size;             \
            convertor->use_desc = &(datatype->opt_desc);                \
        } else {                                                        \
            ompi_convertor_master_t* master;                            \
            int i;                                                      \
            uint64_t bdt_mask = datatype->bdt_used;                     \
            master = convertor->master;                                 \
            convertor->remote_size = 0;                                 \
            for( i = DT_CHAR; i < DT_MAX_PREDEFINED; i++ ) {            \
                if( bdt_mask & ((uint64_t)1 << i) ) {                   \
                    convertor->remote_size += (datatype->btypes[i] *    \
                                               master->remote_sizes[i]);\
                }                                                       \
            }                                                           \
            convertor->remote_size *= convertor->count;                 \
            convertor->use_desc = &(datatype->desc);                    \
            bdt_mask = datatype->bdt_used & master->hetero_mask;        \
            if( 0 != bdt_mask )                                         \
                convertor->flags ^= CONVERTOR_HOMOGENEOUS;              \
        }                                                               \
        assert( NULL != convertor->use_desc->desc );                    \
        /* For predefined datatypes (contiguous) do nothing more */     \
        /* if checksum is enabled then always continue */               \
        if( !(convertor->flags & CONVERTOR_WITH_CHECKSUM) &&            \
            (convertor->flags & DT_FLAG_NO_GAPS) &&                     \
            ((convertor->flags & CONVERTOR_SEND) ||                     \
             (convertor->flags & CONVERTOR_HOMOGENEOUS)) ) {            \
            convertor->bConverted = 0;                                  \
            return OMPI_SUCCESS;                                        \
        }                                                               \
        {                                                               \
            uint32_t required_stack_length = datatype->btypes[DT_LOOP] + 1; \
                                                                        \
            if( required_stack_length > convertor->stack_size ) {       \
                convertor->stack_size = required_stack_length;          \
                convertor->pStack     = (dt_stack_t*)malloc(sizeof(dt_stack_t) * \
                                                            convertor->stack_size ); \
            } else {                                                    \
                convertor->pStack = convertor->static_stack;            \
                convertor->stack_size = DT_STATIC_STACK_SIZE;           \
            }                                                           \
        }                                                               \
        ompi_convertor_create_stack_at_begining( convertor, ompi_ddt_local_sizes ); \
    }

int32_t
ompi_convertor_prepare_for_recv( ompi_convertor_t* convertor,
                                 const struct ompi_datatype_t* datatype,
                                 int32_t count,
                                 const void* pUserBuf )
{
    /* Here I should check that the data is not overlapping */

    convertor->flags |= CONVERTOR_RECV;

    OMPI_CONVERTOR_PREPARE( convertor, datatype, count, pUserBuf );

    if( convertor->flags & CONVERTOR_WITH_CHECKSUM ) {
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
        if( !(convertor->flags & CONVERTOR_HOMOGENEOUS) ) {
            convertor->fAdvance = ompi_unpack_general_checksum;
        } else
#endif
        if( convertor->pDesc->flags & DT_FLAG_CONTIGUOUS ) {
            convertor->fAdvance = ompi_unpack_homogeneous_contig_checksum;
        } else {
            convertor->fAdvance = ompi_generic_simple_unpack_checksum;
        }
    } else {
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
        if( !(convertor->flags & CONVERTOR_HOMOGENEOUS) ) {
            convertor->fAdvance = ompi_unpack_general;
        } else
#endif
        if( convertor->pDesc->flags & DT_FLAG_CONTIGUOUS ) {
            convertor->fAdvance = ompi_unpack_homogeneous_contig;
        } else {
            convertor->fAdvance = ompi_generic_simple_unpack;
        }
    }
    return OMPI_SUCCESS;
}

int32_t
ompi_convertor_prepare_for_send( ompi_convertor_t* convertor,
                                 const struct ompi_datatype_t* datatype,
                                 int32_t count,
                                 const void* pUserBuf )
{
    convertor->flags |= CONVERTOR_SEND;

    OMPI_CONVERTOR_PREPARE( convertor, datatype, count, pUserBuf );

    if( convertor->flags & CONVERTOR_WITH_CHECKSUM ) {
        if( datatype->flags & DT_FLAG_CONTIGUOUS ) {
            if( ((datatype->ub - datatype->lb) == (ptrdiff_t)datatype->size) 
                || (1 >= convertor->count) )
                convertor->fAdvance = ompi_pack_homogeneous_contig_checksum;
            else
                convertor->fAdvance = ompi_pack_homogeneous_contig_with_gaps_checksum;
        } else {
            convertor->fAdvance = ompi_generic_simple_pack_checksum;
        }
    } else {
        if( datatype->flags & DT_FLAG_CONTIGUOUS ) {
            if( ((datatype->ub - datatype->lb) == (ptrdiff_t)datatype->size) 
                || (1 >= convertor->count) )
                convertor->fAdvance = ompi_pack_homogeneous_contig;
            else
                convertor->fAdvance = ompi_pack_homogeneous_contig_with_gaps;
        } else {
            convertor->fAdvance = ompi_generic_simple_pack;
        }
    }
    return OMPI_SUCCESS;
}

/*
 * These functions can be used in order to create an IDENTICAL copy of one convertor. In this
 * context IDENTICAL means that the datatype and count and all other properties of the basic
 * convertor get replicated on this new convertor. However, the references to the datatype
 * are not increased. This function take special care about the stack. If all the cases the
 * stack is created with the correct number of entries but if the copy_stack is true (!= 0)
 * then the content of the old stack is copied on the new one. The result will be a convertor
 * ready to use starting from the old position. If copy_stack is false then the convertor
 * is created with a empty stack (you have to use ompi_convertor_set_position before using it).
 */
int ompi_convertor_clone( const ompi_convertor_t* source,
                          ompi_convertor_t* destination,
                          int32_t copy_stack )
{
    destination->remoteArch        = source->remoteArch;
    destination->flags             = source->flags | CONVERTOR_CLONE;
    destination->pDesc             = source->pDesc;
    destination->use_desc          = source->use_desc;
    destination->count             = source->count;
    destination->pBaseBuf          = source->pBaseBuf;
    destination->fAdvance          = source->fAdvance;
    destination->master            = source->master;
    destination->local_size        = source->local_size;
    destination->remote_size       = source->remote_size;
    /* create the stack */
    if( source->stack_size > DT_STATIC_STACK_SIZE ) {
        destination->pStack = (dt_stack_t*)malloc(sizeof(dt_stack_t) * source->stack_size );
    } else {
        destination->pStack = destination->static_stack;
    }
    destination->stack_size = source->stack_size;

    /* initialize the stack */
    if( 0 == copy_stack ) {
        destination->bConverted = -1;
        destination->stack_pos  = -1;
    } else {
        memcpy( destination->pStack, source->pStack, sizeof(dt_stack_t) * (source->stack_pos+1) );
        destination->bConverted = source->bConverted;
        destination->stack_pos  = source->stack_pos;
    }
    return OMPI_SUCCESS;
}

void ompi_convertor_dump( ompi_convertor_t* convertor )
{
    printf( "Convertor %p count %d stack position %d bConverted %ld\n", (void*)convertor,
            convertor->count, convertor->stack_pos, (unsigned long)convertor->bConverted );
    printf( "\tlocal_size %ld remote_size %ld flags %X stack_size %d pending_length %d\n",
            (unsigned long)convertor->local_size, (unsigned long)convertor->remote_size,
            convertor->flags, convertor->stack_size, convertor->partial_length );
    ompi_ddt_dump( convertor->pDesc );
    printf( "Actual stack representation\n" );
    ompi_ddt_dump_stack( convertor->pStack, convertor->stack_pos,
                         convertor->pDesc->desc.desc, convertor->pDesc->name );
}

void ompi_ddt_dump_stack( const dt_stack_t* pStack, int stack_pos,
                          const union dt_elem_desc* pDesc, const char* name )
{
    opal_output( 0, "\nStack %p stack_pos %d name %s\n", (void*)pStack, stack_pos, name );
    for( ; stack_pos >= 0; stack_pos-- ) {
        opal_output( 0, "%d: pos %d count %d disp %ld ", stack_pos, pStack[stack_pos].index,
                     (int)pStack[stack_pos].count, (long)pStack[stack_pos].disp );
        if( pStack->index != -1 )
            opal_output( 0, "\t[desc count %d disp %ld extent %ld]\n",
                         pDesc[pStack[stack_pos].index].elem.count,
                         (long)pDesc[pStack[stack_pos].index].elem.disp,
                         (long)pDesc[pStack[stack_pos].index].elem.extent );
        else
            opal_output( 0, "\n" );
    }
    opal_output( 0, "\n" );
}
