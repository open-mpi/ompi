/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "datatype/datatype.h"
#include "datatype/convertor.h"
#include "datatype/datatype_internal.h"

ompi_convertor_t* ompi_convertor_create( int32_t remote_arch, int32_t mode )
{
    ompi_convertor_t* convertor = OBJ_NEW(ompi_convertor_t);

    convertor->remoteArch  = remote_arch;
    convertor->pFunctions  = ompi_ddt_copy_functions;
    convertor->flags       = 0;

    return convertor;
}

inline int ompi_convertor_cleanup( ompi_convertor_t* convertor )
{
    ompi_datatype_t* datatype = (ompi_datatype_t*)convertor->pDesc;

    if( convertor->stack_size > DT_STATIC_STACK_SIZE ) {
        free( convertor->pStack );
        convertor->pStack = NULL;
        convertor->stack_size = 0;
    }
    if( !(CONVERTOR_CLONE & convertor->flags) )
        OBJ_RELEASE( datatype );
    convertor->pDesc = NULL;
    return OMPI_SUCCESS;
}

static void ompi_convertor_construct( ompi_convertor_t* convertor )
{
    convertor->pDesc             = NULL;
    convertor->pStack            = convertor->static_stack;
    convertor->stack_size        = DT_STATIC_STACK_SIZE;
    convertor->fAdvance          = NULL;
    convertor->memAlloc_fn       = NULL;
    convertor->memAlloc_userdata = NULL;
}

static void ompi_convertor_destruct( ompi_convertor_t* convertor )
{
    ompi_convertor_cleanup( convertor );
}

OBJ_CLASS_INSTANCE(ompi_convertor_t, ompi_object_t, ompi_convertor_construct, ompi_convertor_destruct );


/* 
 * Return 0 if everything went OK and if there is still room before the complete
 *          conversion of the data (need additional call with others input buffers )
 *        1 if everything went fine and the data was completly converted
 *       -1 something wrong occurs.
 */
inline int32_t ompi_convertor_pack( ompi_convertor_t* pConv,
                                    struct iovec* iov, uint32_t* out_size,
                                    size_t* max_data, int32_t* freeAfter )
{
    /* protect against over packing data */
    if( pConv->bConverted == (pConv->pDesc->size * pConv->count) ) {
	iov[0].iov_len = 0;
	*out_size = 0;
        *max_data = 0;
	return 1;  /* nothing to do */
    }
    assert( pConv->bConverted < (pConv->pDesc->size * pConv->count) );
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
    const ompi_datatype_t *pData = pConv->pDesc;
   
    /* protect against over unpacking data */
    if( pConv->bConverted == (pData->size * pConv->count) ) {
        iov[0].iov_len = 0;
        out_size = 0;
        *max_data = 0;
        return 1;  /* nothing to do */
    }

    assert( pConv->bConverted < (pConv->pDesc->size * pConv->count) );
    return pConv->fAdvance( pConv, iov, out_size, max_data, freeAfter );
}

int ompi_convertor_create_stack_with_pos_general( ompi_convertor_t* pConvertor,
                                                  int starting_point, const int* sizes );
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

    pStack[0].count    = pConvertor->count;
    pStack[0].index    = -1;

    pElems = pConvertor->use_desc->desc;
    pStack[0].end_loop = pConvertor->use_desc->used;

    /* Special case for contiguous datatypes */
    if( pData->size == 0 ) {  /* special case for empty datatypes */
        count = pConvertor->count;
    } else {
        count = starting_point / pData->size;
    }
    extent = pData->ub - pData->lb;
    
    pStack[0].disp = count * extent;
    pStack[0].count -= count;

    /* now compute the number of pending bytes */
    count = starting_point - count * pData->size;
    pStack[1].index    = 0;  /* useless */
    pStack[1].count    = pData->size - count;
    pStack[1].end_loop = 0;  /* useless */
    /* we save the current displacement starting from the begining
     * of this data.
     */
    pStack[1].disp     = pData->true_lb + count;

    pConvertor->bConverted = starting_point;
    pConvertor->stack_pos = 1;
    return OMPI_SUCCESS;
}

static inline
int ompi_convertor_create_stack_at_begining( ompi_convertor_t* pConvertor, const int* sizes )
{
    dt_stack_t* pStack;
    dt_elem_desc_t* pElems;
    int index = 0;

    pConvertor->stack_pos = 0;
    pStack = pConvertor->pStack;
    /* Fill the first position on the stack. This one correspond to the
     * last fake DT_END_LOOP that we add to the data representation and
     * allow us to move quickly inside the datatype when we have a count.
     */
    pConvertor->pStack[0].index = -1;
    pConvertor->pStack[0].count = pConvertor->count;
    pConvertor->pStack[0].disp  = 0;
    /* first here we should select which data representation will be used for
     * this operation: normal one or the optimized version ? */
    pElems = pConvertor->use_desc->desc;
    pStack[0].end_loop = pConvertor->use_desc->used;

    /* In the case where the datatype start with loops, we should push them on the stack.
     * Otherwise when we reach the end_loop field we will pop too many entries and finish
     * by overriding other places in memory. Now the big question is when to stop creating
     * the entries on the stack ? Should I stop when I reach the first data element or
     * should I stop on the first contiguous loop ?
     */
    while( pElems[index].elem.common.type == DT_LOOP ) {
        PUSH_STACK( pStack, pConvertor->stack_pos, index, DT_LOOP,
                    pElems[index].loop.loops, 0, pElems[index].loop.items );
        index++;
    }
    if( pElems[index].elem.common.flags & DT_FLAG_DATA ) {  /* let's stop here */
        PUSH_STACK( pStack, pConvertor->stack_pos, index, pElems[index].elem.common.type,
                    pElems[index].elem.count, pElems[index].elem.disp, 0 );
    } else {
        ompi_output( 0, "Here we should have a data in the datatype description\n" );
	ompi_ddt_dump( pConvertor->pDesc );
    }
    pConvertor->bConverted = 0;
    return OMPI_SUCCESS;
}

extern int ompi_ddt_local_sizes[DT_MAX_PREDEFINED];
inline int32_t ompi_convertor_set_position( ompi_convertor_t* convertor, size_t* position )
{
    if( (*position) == convertor->bConverted ) return OMPI_SUCCESS;
    if( 0 == (*position) )
        return ompi_convertor_create_stack_at_begining( convertor, ompi_ddt_local_sizes );

    if( convertor->flags & DT_FLAG_CONTIGUOUS )
        return ompi_convertor_create_stack_with_pos_contig( convertor, (*position),
                                                            ompi_ddt_local_sizes );
    return ompi_convertor_create_stack_with_pos_general( convertor, (*position),
                                                         ompi_ddt_local_sizes );
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
inline int ompi_convertor_prepare( ompi_convertor_t* convertor,
                                   const ompi_datatype_t* datatype, int32_t count,
                                   const void* pUserBuf )
{
    uint32_t required_stack_length = datatype->btypes[0 /*DT_LOOP*/] + 3;

    if( !(datatype->flags & DT_FLAG_COMMITED) ) {
        /* this datatype is improper for conversion. Commit it first */
        return OMPI_ERROR;
    }

    convertor->pBaseBuf        = (void*)pUserBuf;
    convertor->count           = count;
    convertor->stack_pos       = 0;

    assert( datatype != NULL );
    OBJ_RETAIN( datatype );
    /* as we increase the reference count on the datatype we are not a clone anymore */
    convertor->flags &= ~CONVERTOR_CLONE;
    convertor->pDesc = (ompi_datatype_t*)datatype;
    convertor->bConverted = 0;

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

    return ompi_convertor_create_stack_at_begining( convertor, ompi_ddt_local_sizes );
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
inline int
ompi_convertor_clone( const ompi_convertor_t* source,
                      ompi_convertor_t* destination,
                      int32_t copy_stack )
{
    /* copy all properties */
    destination->remoteArch  = source->remoteArch;
    destination->flags       = source->flags | CONVERTOR_CLONE;
    destination->pDesc       = source->pDesc;
    destination->use_desc    = source->use_desc;
    destination->count       = source->count;
    destination->pBaseBuf    = source->pBaseBuf;
    destination->fAdvance    = source->fAdvance;
    destination->memAlloc_fn = source->memAlloc_fn;
    destination->pFunctions  = source->pFunctions;
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
        bcopy( source->pStack, destination->pStack, sizeof(dt_stack_t) * source->stack_size );
        destination->bConverted = source->bConverted;
        destination->stack_pos  = source->stack_pos;
    }
    return OMPI_SUCCESS;
}

inline int
ompi_convertor_clone_with_position( const ompi_convertor_t* source,
                                    ompi_convertor_t* destination,
                                    int32_t copy_stack,
                                    size_t* position )
{
    (void)ompi_convertor_clone( source, destination, copy_stack );
    return ompi_convertor_set_position( destination, position );
}

/* Actually we suppose that we can only do receiver side conversion */
int32_t ompi_convertor_get_packed_size( const ompi_convertor_t* pConv, size_t* pSize )
{
    int32_t ddt_size = 0;

    if( ompi_ddt_type_size( pConv->pDesc, &ddt_size ) != 0 )
        return OMPI_ERROR;
    /* actually *pSize contain the size of one instance of the data */
    *pSize = ddt_size * pConv->count;
    return OMPI_SUCCESS;
}

int32_t ompi_convertor_get_unpacked_size( const ompi_convertor_t* pConv, size_t* pSize )
{
    int i;
    const ompi_datatype_t* pData = pConv->pDesc;

    if( pConv->count == 0 ) {
        *pSize = 0;
        return OMPI_SUCCESS;
    }
    if( pConv->remoteArch == 0 ) {  /* same architecture */
        *pSize = pData->size * pConv->count;
        return OMPI_SUCCESS;
    }
    *pSize = 0;
    for( i = DT_CHAR; i < DT_MAX_PREDEFINED; i++ ) {
        if( pData->bdt_used & (((unsigned long long)1)<<i) ) {
            /* TODO replace with the remote size */
            *pSize += (pData->btypes[i] * ompi_ddt_basicDatatypes[i]->size);
        }
    }
    *pSize *= pConv->count;
    return OMPI_SUCCESS;
}

