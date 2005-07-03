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

#ifndef CONVERTOR_H_HAS_BEEN_INCLUDED
#define CONVERTOR_H_HAS_BEEN_INCLUDED

#include "ompi_config.h"
#include "include/constants.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

union dt_elem_desc;
struct ompi_datatype_t;

/*
 * CONVERTOR SECTION
 */
/* keep the last 16 bits free for data flags */
#define CONVERTOR_USELESS          0x00010000
#define CONVERTOR_RECV             0x00020000
#define CONVERTOR_SEND             0x00040000
#define CONVERTOR_HOMOGENEOUS      0x00080000
#define CONVERTOR_CLONE            0x00100000
#define CONVERTOR_STATE_MASK       0xFF000000
#define CONVERTOR_STATE_START      0x01000000
#define CONVERTOR_STATE_COMPLETE   0x02000000
#define CONVERTOR_STATE_ALLOC      0x04000000

typedef int32_t (*conversion_fct_t)( uint32_t count,
                                     const void* from, uint32_t from_len, long from_extent,
                                     void* to, uint32_t in_length, long to_extent );

typedef struct ompi_convertor_t ompi_convertor_t;
typedef int32_t (*convertor_advance_fct_t)( ompi_convertor_t* pConvertor,
                                            struct iovec* pInputv,
                                            uint32_t* inputCount,
                                            size_t* max_data,
                                            int32_t* freeAfter );
typedef void*(*memalloc_fct_t)( size_t* pLengthi, void* userdata );

typedef struct dt_stack {
    int16_t index;    /**< index in the element description */
    int16_t type;     /**< the type used for the last pack/unpack (original or DT_BYTE) */
    int32_t count;    /**< number of times we still have to do it */
    int32_t end_loop; /**< for loops the end of the loop, otherwise useless */
    long    disp;     /**< actual displacement depending on the count field */
} dt_stack_t;

#define DT_STATIC_STACK_SIZE   5

struct ompi_convertor_t {
    opal_object_t                 super;        /**< basic superclass */
    uint32_t                      remoteArch;   /**< the remote architecture */
    uint32_t                      flags;        /**< the properties of this convertor */
    const struct ompi_datatype_t* pDesc;        /**< the datatype description associated with the convertor */
    const struct dt_type_desc*    use_desc;     /**< the version used by the convertor (normal or optimized) */
    uint32_t                      count;        /**< the total number of full datatype elements */
    char*                         pBaseBuf;     /**< initial buffer as supplied by the user */
    dt_stack_t*                   pStack;       /**< the local stack for the actual conversion */
    uint32_t                      stack_size;   /**< size of the allocated stack */
    convertor_advance_fct_t       fAdvance;     /**< pointer to the pack/unpack functions */
    memalloc_fct_t                memAlloc_fn;  /**< pointer to the memory allocation function */
    void*                         memAlloc_userdata;  /**< user data for the malloc function */
    conversion_fct_t*             pFunctions;   /**< the convertor functions pointer */
    /* All others fields get modified for every call to pack/unpack functions */
    uint32_t                      stack_pos;    /**< the actual position on the stack */
    size_t                        bConverted;   /**< # of bytes already converted */
    dt_stack_t                    static_stack[DT_STATIC_STACK_SIZE];  /**< local stack for small datatypes */
};
OBJ_CLASS_DECLARATION( ompi_convertor_t );

/* Base convertor for all external32 operations */
extern ompi_convertor_t* ompi_mpi_external32_convertor;
extern conversion_fct_t ompi_ddt_copy_functions[];

/*
 *
 */
OMPI_DECLSPEC int32_t
ompi_convertor_pack( ompi_convertor_t* pConv,
                     struct iovec* iov,
                     uint32_t* out_size,
                     size_t* max_data,
                     int32_t* freeAfter );

/*
 *
 */
OMPI_DECLSPEC int32_t
ompi_convertor_unpack( ompi_convertor_t* pConv,
                       struct iovec* iov,
                       uint32_t* out_size,
                       size_t* max_data,
                       int32_t* freeAfter );

/*
 *
 */
OMPI_DECLSPEC ompi_convertor_t* ompi_convertor_create( int32_t remote_arch, int32_t mode );

/*
 *
 */
OMPI_DECLSPEC int32_t ompi_convertor_cleanup( ompi_convertor_t* convertor );

/*
 *
 */
OMPI_DECLSPEC int32_t
ompi_convertor_set_position( ompi_convertor_t* convertor,
                             size_t* position );

/*
 *
 */
OMPI_DECLSPEC int32_t
ompi_convertor_personalize( ompi_convertor_t* pConv, uint32_t flags,
                            size_t* starting_point,
                            memalloc_fct_t allocfn,
                            void* userdata );

/*
 *
 */
OMPI_DECLSPEC int32_t
ompi_convertor_need_buffers( ompi_convertor_t* pConvertor );

/*
 *
 */
OMPI_DECLSPEC int32_t
ompi_convertor_get_packed_size( const ompi_convertor_t* pConv,
                                size_t* pSize );

/*
 *
 */
OMPI_DECLSPEC int32_t
ompi_convertor_get_unpacked_size( const ompi_convertor_t* pConv,
                                  size_t* pSize );

/*
 * This function is internal to the data type engine. It should not be called from
 * outside. The data preparation should use the specialized prepare_for_send and
 * prepare_for_recv functions.
 */
#ifdef WIN32
OMPI_DECLSPEC
#endif
int ompi_convertor_prepare( ompi_convertor_t* convertor,
                            const struct ompi_datatype_t* datatype, int32_t count,
                            const void* pUserBuf );

/*
 *
 */
OMPI_DECLSPEC int32_t
ompi_convertor_copy_and_prepare_for_send( const ompi_convertor_t* pSrcConv, 
                                          const struct ompi_datatype_t* datatype,
                                          int32_t count,
                                          const void* pUserBuf,
                                          ompi_convertor_t* convertor );
OMPI_DECLSPEC int32_t
ompi_convertor_prepare_for_send( ompi_convertor_t* convertor,
                                 const struct ompi_datatype_t* datatype,
                                 int32_t count,
                                 const void* pUserBuf);

/*
 *
 */
OMPI_DECLSPEC int32_t
ompi_convertor_copy_and_prepare_for_recv( const ompi_convertor_t* pSrcConv, 
                                          const struct ompi_datatype_t* datatype,
                                          int32_t count,
                                          const void* pUserBuf,
                                          ompi_convertor_t* convertor );
OMPI_DECLSPEC int32_t
ompi_convertor_prepare_for_recv( ompi_convertor_t* convertor,
                                 const struct ompi_datatype_t* datatype,
                                 int32_t count,
                                 const void* pUserBuf );

/*
 *
 */
OMPI_DECLSPEC int32_t
ompi_convertor_clone( const ompi_convertor_t* source,
                      ompi_convertor_t* destination,
                      int32_t copy_stack );
OMPI_DECLSPEC int32_t
ompi_convertor_clone_with_position( const ompi_convertor_t* source,
                                    ompi_convertor_t* destination,
                                    int32_t copy_stack,
                                    size_t* position );

/*
 *
 */
OMPI_DECLSPEC void ompi_convertor_dump( ompi_convertor_t* convertor );
OMPI_DECLSPEC void ompi_ddt_dump_stack( const dt_stack_t* pStack, int stack_pos,
                                        const union dt_elem_desc* pDesc, const char* name );

/*
 *
 */
extern int ompi_convertor_generic_simple_pack( ompi_convertor_t* pConvertor,
                                               struct iovec* iov, uint32_t* out_size,
                                               size_t* max_data,
                                               int32_t* freeAfter );

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif  /* CONVERTOR_H_HAS_BEEN_INCLUDED */
