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

ompi_convertor_t* ompi_convertor_create( int32_t remote_arch, int32_t mode )
{
    ompi_convertor_t* convertor = OBJ_NEW(ompi_convertor_t);

    convertor->remoteArch = remote_arch;
    convertor->pFunctions = ompi_ddt_copy_functions;
    convertor->stack_pos  = 0;

    return convertor;
}

/* The cleanup function will release the reference to the datatype and put the convertor in
 * exactly the same state as after a call to ompi_convertor_construct. Therefore, all PML
 * can call OBJ_DESTRUCT on the request's convertors without having to call OBJ_CONSTRUCT
 * everytime they grab a new one from the cache. The OBJ_CONSTRUCT on the convertor should
 * be called only on the first creation of a request (not when extracted from the cache).
 */
inline int ompi_convertor_cleanup( ompi_convertor_t* convertor )
{
    if( convertor->stack_size > DT_STATIC_STACK_SIZE ) {
        free( convertor->pStack );
        convertor->pStack     = convertor->static_stack;
        convertor->stack_size = DT_STATIC_STACK_SIZE;
    }
    convertor->pDesc     = NULL;
    convertor->flags     = CONVERTOR_HOMOGENEOUS;
    convertor->stack_pos = 0;
    return OMPI_SUCCESS;
}

static void ompi_convertor_construct( ompi_convertor_t* convertor )
{
    convertor->flags             = CONVERTOR_HOMOGENEOUS;
    convertor->pStack            = convertor->static_stack;
    convertor->stack_size        = DT_STATIC_STACK_SIZE;
    convertor->remoteArch        = 0;
    convertor->pending_length    = 0;
}

static void ompi_convertor_destruct( ompi_convertor_t* convertor )
{
    ompi_convertor_cleanup( convertor );
}

OBJ_CLASS_INSTANCE(ompi_convertor_t, opal_object_t, ompi_convertor_construct, ompi_convertor_destruct );


/* 
 * Return 0 if everything went OK and if there is still room before the complete
 *          conversion of the data (need additional call with others input buffers )
 *        1 if everything went fine and the data was completly converted
 *       -1 something wrong occurs.
 */
#include "ompi/communicator/communicator.h"
inline int32_t ompi_convertor_pack( ompi_convertor_t* pConv,
                                    struct iovec* iov, uint32_t* out_size,
                                    size_t* max_data, int32_t* freeAfter )
{
    pConv->checksum = OMPI_CSUM_ZERO;
    pConv->csum_ui1 = 0;
    pConv->csum_ui2 = 0;

    /* protect against over packing data */
    if( pConv->flags & CONVERTOR_COMPLETED ) {
        iov[0].iov_len = 0;
        *out_size = 0;
        *max_data = 0;
        return 1;  /* nothing to do */
    }
    assert( pConv->bConverted < pConv->local_size );

    /* We dont allocate any memory. The packing function should allocate it
     * if it need. If it's possible to find iovec in the derived datatype
     * description then we dont have to allocate any memory.
     */
    return pConv->fAdvance( pConv, iov, out_size, max_data, freeAfter );
}

inline int32_t ompi_convertor_unpack( ompi_convertor_t* pConv,
                                      struct iovec* iov, uint32_t* out_size,
                                      size_t* max_data, int32_t* freeAfter )
{
    pConv->checksum = OMPI_CSUM_ZERO;
    pConv->csum_ui1 = 0;
    pConv->csum_ui2 = 0;

    /* protect against over unpacking data */
    if( pConv->flags & CONVERTOR_COMPLETED ) {
        iov[0].iov_len = 0;
        out_size = 0;
        *max_data = 0;
        return 1;  /* nothing to do */
    }

    assert( pConv->bConverted < pConv->local_size );
    return pConv->fAdvance( pConv, iov, out_size, max_data, freeAfter );
}

static inline
int ompi_convertor_create_stack_with_pos_contig( ompi_convertor_t* pConvertor,
                                                 int starting_point, const int* sizes )
{
    dt_stack_t* pStack;   /* pointer to the position on the stack */
    const ompi_datatype_t* pData = pConvertor->pDesc;
    dt_elem_desc_t* pElems;
    uint32_t count;
    long extent;

    pStack = pConvertor->pStack;
    /* The prepare function already make the selection on which data representation
     * we have to use: normal one or the optimized version ?
     */
    pElems = pConvertor->use_desc->desc;

    count = starting_point / pData->size;
    extent = pData->ub - pData->lb;

    pStack[0].type     = DT_LOOP;  /* the first one is always the loop */
    pStack[0].count    = pConvertor->count - count;
    pStack[0].index    = -1;
    pStack[0].end_loop = pConvertor->use_desc->used;
    pStack[0].disp     = count * extent;

    /* now compute the number of pending bytes */
    count = starting_point - count * pData->size;
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
    pStack[1].end_loop = 0;  /* useless */

    pConvertor->bConverted = starting_point;
    pConvertor->stack_pos = 1;
    return OMPI_SUCCESS;
}

static inline
int ompi_convertor_create_stack_at_begining( ompi_convertor_t* convertor,
                                             const int* sizes )
{
    dt_stack_t* pStack = convertor->pStack;
    dt_elem_desc_t* pElems;

    convertor->stack_pos      = 1;
    convertor->pending_length = 0;
    convertor->bConverted     = 0;
    /* Fill the first position on the stack. This one correspond to the
     * last fake DT_END_LOOP that we add to the data representation and
     * allow us to move quickly inside the datatype when we have a count.
     */
    pStack[0].index = -1;
    pStack[0].count = convertor->count;
    pStack[0].disp  = 0;
    pStack[0].end_loop = convertor->use_desc->used;
    /* The prepare function already make the selection on which data representation
     * we have to use: normal one or the optimized version ?
     */
    pElems = convertor->use_desc->desc;

    pStack[1].index = 0;
    pStack[1].disp = 0;
    pStack[1].end_loop = 0;
    if( pElems[0].elem.common.type == DT_LOOP ) {
        pStack[1].count = pElems[0].loop.loops;
    } else {
        pStack[1].count = pElems[0].elem.count;
    }
    return OMPI_SUCCESS;
}

extern int ompi_ddt_local_sizes[DT_MAX_PREDEFINED];
extern int ompi_convertor_create_stack_with_pos_general( ompi_convertor_t* convertor,
                                                         int starting_point, const int* sizes );

int32_t ompi_convertor_set_position_nocheck( ompi_convertor_t* convertor,
                                             size_t* position )
{
    int32_t rc;

    /*
     * Do not allow the convertor to go outside the data boundaries. This test include
     * the check for datatype with size zero as well as for convertors with a count of zero.
     */
    if( convertor->local_size <= *position) {
        convertor->flags |= CONVERTOR_COMPLETED;
        convertor->bConverted = convertor->local_size;
        *position = convertor->bConverted;
        return OMPI_SUCCESS;
    }
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

int32_t
ompi_convertor_personalize( ompi_convertor_t* convertor, uint32_t flags,
                            size_t* position, memalloc_fct_t allocfn, void* userdata )
{
    convertor->flags |= flags;
    convertor->memAlloc_fn = allocfn;
    convertor->memAlloc_userdata = userdata;

    return ompi_convertor_set_position( convertor, position );
}

/* This function will initialize a convertor based on a previously created convertor. The idea
 * is the move outside these function the heavy selection of architecture features for the convertors.
 *
 * I consider here that the convertor is clean, either never initialized or already cleanup.
 */
int ompi_convertor_prepare( ompi_convertor_t* convertor,
                            const ompi_datatype_t* datatype, int32_t count,
                            const void* pUserBuf )
{
    uint32_t required_stack_length = datatype->btypes[DT_LOOP] + 1;

    if( !(datatype->flags & DT_FLAG_COMMITED) ) {
        /* this datatype is improper for conversion. Commit it first */
        return OMPI_ERROR;
    }

    convertor->pBaseBuf        = (void*)pUserBuf;
    convertor->count           = count;

    assert( datatype != NULL );
    /* As we change (or set) the datatype on this convertor we should reset the datatype
     * part of the convertor flags to the default value.
     */
    convertor->flags         &= CONVERTOR_TYPE_MASK;
    convertor->flags         |= (CONVERTOR_DATATYPE_MASK & datatype->flags);
    convertor->pDesc          = (ompi_datatype_t*)datatype;

    /* Decide which data representation will be used for the conversion. */
    if( (NULL != datatype->opt_desc.desc) && (convertor->flags & CONVERTOR_HOMOGENEOUS) ) {
        convertor->use_desc = &(datatype->opt_desc);
    } else {
        convertor->use_desc = &(datatype->desc);
    }

    if( required_stack_length > convertor->stack_size ) {
        convertor->stack_size = required_stack_length;
        convertor->pStack     = (dt_stack_t*)malloc(sizeof(dt_stack_t) * convertor->stack_size );
    } else {
        convertor->pStack = convertor->static_stack;
        convertor->stack_size = DT_STATIC_STACK_SIZE;
    }

    /* Compute the local and remote sizes */
    convertor->local_size = convertor->count * datatype->size;
    /* If the data is empty we don't have to anything except mark the convertor as
     * completed. With this flag set the pack and unpack functions will not do
     * anything.
     */
    if( 0 == convertor->local_size ) {
        convertor->flags |= CONVERTOR_COMPLETED;
        convertor->remote_size = 0;
        return OMPI_SUCCESS;
    }

    if( convertor->remoteArch == ompi_mpi_local_arch ) {
        convertor->remote_size = convertor->local_size;
    } else {
        int i;
        uint64_t bdt_mask = datatype->bdt_used >> DT_CHAR;
        convertor->remote_size = 0;
        for( i = DT_CHAR; bdt_mask != 0; i++, bdt_mask >>= 1 ) { 
            if( bdt_mask & ((unsigned long long)1) ) {
                /* TODO replace with the remote size */
                convertor->remote_size += (datatype->btypes[i] * ompi_ddt_basicDatatypes[i]->size);
            }
        }   
        convertor->remote_size *= convertor->count;
    }

    return ompi_convertor_create_stack_at_begining( convertor, ompi_ddt_local_sizes );
}

/*
 * All the conversion functions (pack and unpack) we have. For each
 * function there are 2 versions: one without checksum (using memcpy)
 * and one with checksum.
 */
extern convertor_advance_fct_t ompi_unpack_general;
extern convertor_advance_fct_t ompi_unpack_general_checksum;
extern convertor_advance_fct_t ompi_unpack_homogeneous;
extern convertor_advance_fct_t ompi_unpack_homogeneous_checksum;
extern convertor_advance_fct_t ompi_generic_simple_unpack;
extern convertor_advance_fct_t ompi_generic_simple_unpack_checksum;
extern convertor_advance_fct_t ompi_unpack_homogeneous_contig;
extern convertor_advance_fct_t ompi_unpack_homogeneous_contig_checksum;
extern convertor_advance_fct_t ompi_pack_general;
extern convertor_advance_fct_t ompi_pack_general_checksum;
extern convertor_advance_fct_t ompi_pack_homogeneous_with_memcpy;
extern convertor_advance_fct_t ompi_pack_homogeneous_with_memcpy_checksum;
extern convertor_advance_fct_t ompi_pack_no_conversion;
extern convertor_advance_fct_t ompi_pack_no_conversion_checksum;
extern convertor_advance_fct_t ompi_generic_simple_pack;
extern convertor_advance_fct_t ompi_generic_simple_pack_checksum;
extern convertor_advance_fct_t ompi_pack_no_conv_contig;
extern convertor_advance_fct_t ompi_pack_no_conv_contig_checksum;
extern convertor_advance_fct_t ompi_pack_no_conv_contig_with_gaps;
extern convertor_advance_fct_t ompi_pack_no_conv_contig_with_gaps_checksum;

int32_t
ompi_convertor_prepare_for_recv( ompi_convertor_t* convertor,
                                 const struct ompi_datatype_t* datatype,
                                 int32_t count,
                                 const void* pUserBuf )
{
    /* Here I should check that the data is not overlapping */

    if( OMPI_SUCCESS != ompi_convertor_prepare( convertor, datatype,
                                                count, pUserBuf ) ) {
        return OMPI_ERROR;
    }

    convertor->flags      |= CONVERTOR_RECV;
    convertor->memAlloc_fn = NULL;

    if( convertor->flags & CONVERTOR_WITH_CHECKSUM ) {
        if( convertor->pDesc->flags & DT_FLAG_CONTIGUOUS ) {
            assert( convertor->flags & DT_FLAG_CONTIGUOUS );
            convertor->fAdvance = ompi_unpack_homogeneous_contig_checksum;
        } else {
            convertor->fAdvance = ompi_generic_simple_unpack_checksum;
        }
    } else {
        if( convertor->pDesc->flags & DT_FLAG_CONTIGUOUS ) {
            assert( convertor->flags & DT_FLAG_CONTIGUOUS );
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
    if( OMPI_SUCCESS != ompi_convertor_prepare( convertor, datatype,
                                                count, pUserBuf ) ) {
        return OMPI_ERROR;
    }

    convertor->flags            |= CONVERTOR_SEND;
    convertor->memAlloc_fn       = NULL;
    if( convertor->flags & CONVERTOR_WITH_CHECKSUM ) {
        if( datatype->flags & DT_FLAG_CONTIGUOUS ) {
            assert( convertor->flags & DT_FLAG_CONTIGUOUS );
            if( ((datatype->ub - datatype->lb) == (long)datatype->size) )
                convertor->fAdvance = ompi_pack_no_conv_contig_checksum;
            else if( 1 >= convertor->count )  /* gaps or no gaps */
                convertor->fAdvance = ompi_pack_no_conv_contig_checksum;
            else
                convertor->fAdvance = ompi_pack_no_conv_contig_with_gaps_checksum;
        } else {
            convertor->fAdvance = ompi_generic_simple_pack_checksum;
        }
    } else {
        if( datatype->flags & DT_FLAG_CONTIGUOUS ) {
            assert( convertor->flags & DT_FLAG_CONTIGUOUS );
            if( ((datatype->ub - datatype->lb) == (long)datatype->size) )
                convertor->fAdvance = ompi_pack_no_conv_contig;
            else if( 1 >= convertor->count )  /* gaps or no gaps */
                convertor->fAdvance = ompi_pack_no_conv_contig;
            else
                convertor->fAdvance = ompi_pack_no_conv_contig_with_gaps;
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
    destination->memAlloc_fn       = source->memAlloc_fn;
    destination->memAlloc_userdata = source->memAlloc_userdata;
    destination->pFunctions        = source->pFunctions;
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
            convertor->flags, convertor->stack_size, convertor->pending_length );
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
        opal_output( 0, "%d: pos %d count %d disp %ld end_loop %d ", stack_pos, pStack[stack_pos].index,
                     pStack[stack_pos].count, pStack[stack_pos].disp, pStack[stack_pos].end_loop );
        if( pStack->index != -1 )
            opal_output( 0, "\t[desc count %d disp %ld extent %d]\n",
                         pDesc[pStack[stack_pos].index].elem.count,
                         pDesc[pStack[stack_pos].index].elem.disp,
                         pDesc[pStack[stack_pos].index].elem.extent );
        else
            opal_output( 0, "\n" );
    }
    opal_output( 0, "\n" );
}
