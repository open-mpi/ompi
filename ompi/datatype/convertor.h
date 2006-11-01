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

#ifndef CONVERTOR_H_HAS_BEEN_INCLUDED
#define CONVERTOR_H_HAS_BEEN_INCLUDED

#include "ompi_config.h"
#include "ompi/constants.h"
#include "ompi/datatype/datatype.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * CONVERTOR SECTION
 */
/* keep the last 16 bits free for data flags */
#define CONVERTOR_DATATYPE_MASK    0x0000FFFF
#define CONVERTOR_USELESS          0x00010000
#define CONVERTOR_RECV             0x00020000
#define CONVERTOR_SEND             0x00040000
#define CONVERTOR_HOMOGENEOUS      0x00080000
#define CONVERTOR_CLONE            0x00100000
#define CONVERTOR_WITH_CHECKSUM    0x00200000
#define CONVERTOR_TYPE_MASK        0x00FF0000
#define CONVERTOR_STATE_START      0x01000000
#define CONVERTOR_STATE_COMPLETE   0x02000000
#define CONVERTOR_STATE_ALLOC      0x04000000
#define CONVERTOR_COMPLETED        0x08000000

typedef struct ompi_convertor_t ompi_convertor_t;

typedef int32_t (*convertor_advance_fct_t)( ompi_convertor_t* pConvertor,
                                            struct iovec* iov,
                                            uint32_t* out_size,
                                            size_t* max_data );
typedef void*(*memalloc_fct_t)( size_t* pLength, void* userdata );

/* The master convertor struct (defined in convertor_internal.h) */
struct ompi_convertor_master_t;

typedef struct dt_stack {
    int16_t   index;    /**< index in the element description */
    int16_t   type;     /**< the type used for the last pack/unpack (original or DT_BYTE) */
    size_t    count;    /**< number of times we still have to do it */
    ptrdiff_t disp;     /**< actual displacement depending on the count field */
} dt_stack_t;

/**
 *
 */

#define DT_STATIC_STACK_SIZE   5

struct ompi_convertor_t {
    opal_object_t                 super;        /**< basic superclass */
    uint32_t                      remoteArch;   /**< the remote architecture */
    uint32_t                      flags;        /**< the properties of this convertor */
    size_t                        local_size;
    size_t                        remote_size;
    const struct ompi_datatype_t* pDesc;        /**< the datatype description associated with the convertor */
    const struct dt_type_desc*    use_desc;     /**< the version used by the convertor (normal or optimized) */
    uint32_t                      count;        /**< the total number of full datatype elements */
    char*                         pBaseBuf;     /**< initial buffer as supplied by the user */
    dt_stack_t*                   pStack;       /**< the local stack for the actual conversion */
    uint32_t                      stack_size;   /**< size of the allocated stack */
    convertor_advance_fct_t       fAdvance;     /**< pointer to the pack/unpack functions */
    struct ompi_convertor_master_t* master;     /* the master convertor */
    /* All others fields get modified for every call to pack/unpack functions */
    uint32_t                      stack_pos;    /**< the actual position on the stack */
    size_t                        bConverted;   /**< # of bytes already converted */
    uint32_t                      partial_length;  /**< amount of data left over from the last unpack */
    uint32_t                      checksum;     /**< checksum computed by pack/unpack operation */
    uint32_t                      csum_ui1;     /**< partial checksum computed by pack/unpack operation */
    size_t                        csum_ui2;     /**< partial checksum computed by pack/unpack operation */
    dt_stack_t                    static_stack[DT_STATIC_STACK_SIZE];  /**< local stack for small datatypes */
};
OMPI_DECLSPEC OBJ_CLASS_DECLARATION( ompi_convertor_t );

/* Base convertor for all external32 operations */
OMPI_DECLSPEC extern ompi_convertor_t* ompi_mpi_external32_convertor;
OMPI_DECLSPEC extern ompi_convertor_t* ompi_mpi_local_convertor;
OMPI_DECLSPEC extern uint32_t          ompi_mpi_local_arch;

/*
 *
 */
static inline uint32_t
ompi_convertor_get_checksum( ompi_convertor_t* convertor )
{
    return convertor->checksum;
}

/*
 *
 */
OMPI_DECLSPEC int32_t
ompi_convertor_pack( ompi_convertor_t* pConv,
                     struct iovec* iov,
                     uint32_t* out_size,
                     size_t* max_data );

/*
 *
 */
OMPI_DECLSPEC int32_t
ompi_convertor_unpack( ompi_convertor_t* pConv,
                       struct iovec* iov,
                       uint32_t* out_size,
                       size_t* max_data );

/*
 *
 */
OMPI_DECLSPEC ompi_convertor_t* ompi_convertor_create( int32_t remote_arch, int32_t mode );

/**
 * The cleanup function will put the convertor in exactly the same state as after a call
 * to ompi_convertor_construct. Therefore, all PML can call OBJ_DESTRUCT on the request's
 * convertors without having to call OBJ_CONSTRUCT everytime they grab a new one from the
 * cache. The OBJ_CONSTRUCT on the convertor should be called only on the first creation
 * of a request (not when extracted from the cache).
 */
static inline int ompi_convertor_cleanup( ompi_convertor_t* convertor )
{
    if( convertor->stack_size > DT_STATIC_STACK_SIZE ) {
        free( convertor->pStack );
        convertor->pStack     = convertor->static_stack;
        convertor->stack_size = DT_STATIC_STACK_SIZE;
    }
    convertor->pDesc     = NULL;
    convertor->stack_pos = 0;
    return OMPI_SUCCESS;
}

/*
 *
 */
static inline int32_t
ompi_convertor_need_buffers( const ompi_convertor_t* pConvertor )
{
    return !ompi_ddt_is_contiguous_memory_layout( pConvertor->pDesc, pConvertor->count );
}

/*
 *
 */
static inline void
ompi_convertor_get_packed_size( const ompi_convertor_t* pConv,
                                size_t* pSize )
{
    *pSize = pConv->local_size;
}

/*
 *
 */
static inline void
ompi_convertor_get_unpacked_size( const ompi_convertor_t* pConv,
                                  size_t* pSize )
{
    *pSize = pConv->remote_size;
}

/*
 *
 */
OMPI_DECLSPEC int32_t
ompi_convertor_prepare_for_send( ompi_convertor_t* convertor,
                                 const struct ompi_datatype_t* datatype,
                                 int32_t count,
                                 const void* pUserBuf);
static inline int32_t
ompi_convertor_copy_and_prepare_for_send( const ompi_convertor_t* pSrcConv, 
                                          const struct ompi_datatype_t* datatype,
                                          int32_t count,
                                          const void* pUserBuf,
                                          int32_t flags,
                                          ompi_convertor_t* convertor )
{
    convertor->remoteArch = pSrcConv->remoteArch;
    convertor->flags      = (pSrcConv->flags | flags);
    convertor->master     = pSrcConv->master;
    return ompi_convertor_prepare_for_send( convertor, datatype, count, pUserBuf );
}

/*
 *
 */
OMPI_DECLSPEC int32_t
ompi_convertor_prepare_for_recv( ompi_convertor_t* convertor,
                                 const struct ompi_datatype_t* datatype,
                                 int32_t count,
                                 const void* pUserBuf );
static inline int32_t
ompi_convertor_copy_and_prepare_for_recv( const ompi_convertor_t* pSrcConv, 
                                          const struct ompi_datatype_t* datatype,
                                          int32_t count,
                                          const void* pUserBuf,
                                          int32_t flags,
                                          ompi_convertor_t* convertor )
{
    convertor->remoteArch = pSrcConv->remoteArch;
    convertor->flags      = (pSrcConv->flags | flags);
    convertor->master     = pSrcConv->master;

    return ompi_convertor_prepare_for_recv( convertor, datatype, count, pUserBuf );
}

/*
 * Upper level does not need to call the _nocheck function directly.
 */
OMPI_DECLSPEC int32_t
ompi_convertor_set_position_nocheck( ompi_convertor_t* convertor,
                                     size_t* position );
static inline int32_t
ompi_convertor_set_position( ompi_convertor_t* convertor,
                             size_t* position )
{
    /*
     * If the convertor is already at the correct position we are happy.
     */
    if( (*position) == convertor->bConverted ) return OMPI_SUCCESS;

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

    /* Remove the completed flag if it's already set */
    convertor->flags &= ~CONVERTOR_COMPLETED;

    if( !(convertor->flags & CONVERTOR_WITH_CHECKSUM) &&
        (convertor->flags & DT_FLAG_NO_GAPS) &&
        ((convertor->flags & CONVERTOR_SEND) ||
         (convertor->flags & CONVERTOR_HOMOGENEOUS)) ) {
        /* Contiguous and no checkpoint and no homogeneous unpack */
        convertor->bConverted = *position;
        return OMPI_SUCCESS;
    }
    return ompi_convertor_set_position_nocheck( convertor, position );
}

/*
 *
 */
static inline int32_t
ompi_convertor_personalize( ompi_convertor_t* convertor, uint32_t flags,
                            size_t* position )
{
    convertor->flags |= flags;

    if( NULL == position )
        return OMPI_SUCCESS;
    return ompi_convertor_set_position( convertor, position );
}

/*
 *
 */
OMPI_DECLSPEC int
ompi_convertor_clone( const ompi_convertor_t* source,
                      ompi_convertor_t* destination,
                      int32_t copy_stack );
static inline int
ompi_convertor_clone_with_position( const ompi_convertor_t* source,
                                    ompi_convertor_t* destination,
                                    int32_t copy_stack,
                                    size_t* position )
{
    (void)ompi_convertor_clone( source, destination, copy_stack );
    return ompi_convertor_set_position( destination, position );
}

/*
 *
 */
OMPI_DECLSPEC void ompi_convertor_dump( ompi_convertor_t* convertor );
OMPI_DECLSPEC void ompi_ddt_dump_stack( const dt_stack_t* pStack, int stack_pos,
                                        const union dt_elem_desc* pDesc, const char* name );

/*
 *
 */
OMPI_DECLSPEC int
ompi_convertor_generic_simple_position( ompi_convertor_t* pConvertor,
                                        size_t* position );

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif  /* CONVERTOR_H_HAS_BEEN_INCLUDED */
