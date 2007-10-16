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
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * ompi_datatype_t interface for OMPI internal data type representation
 *
 * ompi_datatype_t is a class which represents contiguous or
 * non-contiguous data together with constituent type-related
 * information.
 */

#ifndef DATATYPE_H_HAS_BEEN_INCLUDED
#define DATATYPE_H_HAS_BEEN_INCLUDED

#include "ompi_config.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stddef.h>
#include <string.h>
#include "ompi/constants.h"
#include "opal/class/opal_object.h"
#include "opal/class/opal_hash_table.h"
#include "ompi/class/ompi_pointer_array.h"
#include "mpi.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

extern ompi_pointer_array_t *ompi_datatype_f_to_c_table;

/* if there are more basic datatypes than the number of bytes in the int type
 * the bdt_used field of the data description struct should be changed to long.
 */
#define DT_MAX_PREDEFINED  0x2A

/* flags for the datatypes. */
#define DT_FLAG_DESTROYED     0x0001  /**< user destroyed but some other layers still have a reference */
#define DT_FLAG_COMMITED      0x0002  /**< ready to be used for a send/recv operation */
#define DT_FLAG_CONTIGUOUS    0x0004  /**< contiguous datatype */
#define DT_FLAG_OVERLAP       0x0008  /**< datatype is unpropper for a recv operation */
#define DT_FLAG_USER_LB       0x0010  /**< has a user defined LB */
#define DT_FLAG_USER_UB       0x0020  /**< has a user defined UB */
#define DT_FLAG_PREDEFINED    0x0040  /**< cannot be removed: initial and predefined datatypes */
#define DT_FLAG_NO_GAPS       0x0080  /**< no gaps around the datatype */
#define DT_FLAG_DATA          0x0100  /**< data or control structure */
#define DT_FLAG_ONE_SIDED     0x0200  /**< datatype can be used for one sided operations */
#define DT_FLAG_UNAVAILABLE   0x0400  /**< datatypes unavailable on the build (OS or compiler dependant) */
#define DT_FLAG_VECTOR        0x0800  /**< valid only for loops. The loop contain only one element
                                       **< without extent. It correspond to the vector type. */
/* Keep trace of the type of the predefined datatypes */
#define DT_FLAG_DATA_INT      0x1000
#define DT_FLAG_DATA_FLOAT    0x2000
#define DT_FLAG_DATA_COMPLEX  0x3000
#define DT_FLAG_DATA_TYPE     0x3000
/* In which language the datatype is intended for to be used */
#define DT_FLAG_DATA_C        0x4000
#define DT_FLAG_DATA_CPP      0x8000
#define DT_FLAG_DATA_FORTRAN  0xC000
#define DT_FLAG_DATA_LANGUAGE 0xC000

/* 
 * We should make the difference here between the predefined contiguous and non contiguous
 * datatypes. The DT_FLAG_BASIC is held by all predefined contiguous datatypes.
 */
#define DT_FLAG_BASIC         (DT_FLAG_PREDEFINED | DT_FLAG_CONTIGUOUS | DT_FLAG_NO_GAPS | DT_FLAG_DATA | DT_FLAG_COMMITED)

typedef union dt_elem_desc dt_elem_desc_t;

/**
 * The number of supported entries in the data-type definition and the
 * associated type.
 */
#define MAX_DT_COMPONENT_COUNT UINT_MAX
typedef uint32_t opal_ddt_count_t;

typedef struct dt_type_desc {
    opal_ddt_count_t  length;  /* the maximum number of elements in the description array */
    opal_ddt_count_t  used;    /* the number of used elements in the description array */
    dt_elem_desc_t*   desc;
} dt_type_desc_t;

/* the data description.
 */
typedef struct ompi_datatype_t {
    opal_object_t      super;    /**< basic superclass */
    size_t             size;     /**< total size in bytes of the memory used by the data if
                                  * the data is put on a contiguous buffer */
    uint32_t           align;    /**< data should be aligned to */
    ptrdiff_t          true_lb;
    ptrdiff_t          true_ub;  /**< the true ub of the data without user defined lb and ub */
    ptrdiff_t          lb;       /**< lower bound in memory */
    ptrdiff_t          ub;       /**< upper bound in memory */
    uint16_t           flags;    /**< the flags */
    uint16_t           id;       /**< data id, normally the index in the data array. */
    uint32_t           nbElems;  /**< total number of elements inside the datatype */
    uint64_t           bdt_used; /**< which basic datatypes are used in the data description */

    /* Attribute fields */
    opal_hash_table_t *d_keyhash;
    int32_t            d_f_to_c_index;
    char               name[MPI_MAX_OBJECT_NAME];
    dt_type_desc_t     desc;     /**< the data description */
    dt_type_desc_t     opt_desc; /**< short description of the data used when conversion is useless
                                  * or in the send case (without conversion) */
    void*              args;     /**< data description for the user */
    void*              packed_description;  /**< the packed description of the datatype */

    /* basic elements count used to compute the size of the datatype for
     * remote nodes */
    uint32_t           btypes[DT_MAX_PREDEFINED];
} ompi_datatype_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION( ompi_datatype_t );

int ompi_ddt_register_params(void);
OMPI_DECLSPEC int32_t ompi_ddt_init( void );
OMPI_DECLSPEC int32_t ompi_ddt_finalize( void );
ompi_datatype_t* ompi_ddt_create( int32_t expectedSize );
OMPI_DECLSPEC int32_t ompi_ddt_commit( ompi_datatype_t** );
OMPI_DECLSPEC int32_t ompi_ddt_destroy( ompi_datatype_t** );
static inline int32_t ompi_ddt_is_committed( const ompi_datatype_t* type )
{ return ((type->flags & DT_FLAG_COMMITED) == DT_FLAG_COMMITED); }
static inline int32_t ompi_ddt_is_overlapped( const ompi_datatype_t* type )
{ return ((type->flags & DT_FLAG_OVERLAP) == DT_FLAG_OVERLAP); }
static inline int32_t ompi_ddt_is_acceptable_for_one_sided( const ompi_datatype_t* type )
{ return true; }
static inline int32_t ompi_ddt_is_valid( const ompi_datatype_t* type )
{ return !((type->flags & DT_FLAG_UNAVAILABLE) == DT_FLAG_UNAVAILABLE); }
static inline int32_t ompi_ddt_is_predefined( const ompi_datatype_t* type )
{ return (type->flags & DT_FLAG_PREDEFINED); }

void ompi_ddt_dump( const ompi_datatype_t* pData );
/* data creation functions */
OMPI_DECLSPEC int32_t ompi_ddt_duplicate( const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_contiguous( int count, const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_vector( int count, int bLength, int stride,
                                              const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_hvector( int count, int bLength, MPI_Aint stride,
                                               const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_indexed( int count, const int* pBlockLength, const int* pDisp,
                                               const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_hindexed( int count, const int* pBlockLength, const MPI_Aint* pDisp,
                                                const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_indexed_block( int count, int bLength, const int* pDisp,
                                                     const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_struct( int count, const int* pBlockLength, const MPI_Aint* pDisp,
                                              ompi_datatype_t* const* pTypes, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_resized( const ompi_datatype_t* oldType, MPI_Aint lb, MPI_Aint extent, ompi_datatype_t** newType );

OMPI_DECLSPEC int32_t ompi_ddt_add( ompi_datatype_t* pdtBase, const ompi_datatype_t* pdtAdd, uint32_t count,
                                    ptrdiff_t disp, ptrdiff_t extent );

static inline int32_t ompi_ddt_type_lb( const ompi_datatype_t* pData, ptrdiff_t* disp )
{ *disp = pData->lb; return 0; }
static inline int32_t ompi_ddt_type_ub( const ompi_datatype_t* pData, ptrdiff_t* disp )
{ *disp = pData->ub; return 0; }
static inline int32_t ompi_ddt_type_size ( const ompi_datatype_t* pData, size_t *size )
{ *size = pData->size; return 0; }
static inline int32_t ompi_ddt_type_extent( const ompi_datatype_t* pData, ptrdiff_t* extent )
{ *extent = pData->ub - pData->lb; return 0; }

static inline int32_t ompi_ddt_get_extent( const ompi_datatype_t* pData, ptrdiff_t* lb, ptrdiff_t* extent)
{ *lb = pData->lb; *extent = pData->ub - pData->lb; return 0; }
static inline int32_t ompi_ddt_get_true_extent( const ompi_datatype_t* pData, ptrdiff_t* true_lb, ptrdiff_t* true_extent)
{ *true_lb = pData->true_lb; *true_extent = (pData->true_ub - pData->true_lb); return 0; }
/*
 * This function return true (1) if the datatype representation depending on the count
 * is contiguous in the memory. And false (0) otherwise.
 */
static inline int32_t ompi_ddt_is_contiguous_memory_layout( const ompi_datatype_t* datatype, int32_t count )
{
   if( !(datatype->flags & DT_FLAG_CONTIGUOUS) ) return 0;
   if( (count == 1) || (datatype->flags & DT_FLAG_NO_GAPS) ) return 1;
   assert( (ptrdiff_t)datatype->size != (datatype->ub - datatype->lb) );
   return 0;
}

OMPI_DECLSPEC int32_t ompi_ddt_get_element_count( const ompi_datatype_t* pData, size_t iSize );
OMPI_DECLSPEC int32_t ompi_ddt_set_element_count( const ompi_datatype_t* pData, uint32_t count, size_t* length );
OMPI_DECLSPEC int32_t ompi_ddt_copy_content_same_ddt( const ompi_datatype_t* pData, int32_t count,
                                                      char* pDestBuf, char* pSrcBuf );

OMPI_DECLSPEC const ompi_datatype_t* ompi_ddt_match_size( int size, uint16_t datakind, uint16_t datalang );

/*
 *
 */
OMPI_DECLSPEC int32_t ompi_ddt_sndrcv( void *sbuf, int32_t scount, const ompi_datatype_t* sdtype, void *rbuf,
                                       int32_t rcount, const ompi_datatype_t* rdtype);

/*
 *
 */
OMPI_DECLSPEC int32_t ompi_ddt_get_args( const ompi_datatype_t* pData, int32_t which,
                                         int32_t * ci, int32_t * i,
                                         int32_t * ca, MPI_Aint* a,
                                         int32_t * cd, ompi_datatype_t** d, int32_t * type);
OMPI_DECLSPEC int32_t ompi_ddt_set_args( ompi_datatype_t* pData,
                                         int32_t ci, int32_t ** i,
                                         int32_t ca, MPI_Aint* a,
                                         int32_t cd, ompi_datatype_t** d,int32_t type);
OMPI_DECLSPEC int32_t ompi_ddt_copy_args( const ompi_datatype_t* source_data,
                                          ompi_datatype_t* dest_data );
OMPI_DECLSPEC int32_t ompi_ddt_release_args( ompi_datatype_t* pData );

/*
 *
 */
OMPI_DECLSPEC size_t ompi_ddt_pack_description_length( const ompi_datatype_t* datatype );

/*
 *
 */
OMPI_DECLSPEC int ompi_ddt_get_pack_description( ompi_datatype_t* datatype,
                                                 const void** packed_buffer );

/*
 *
 */
struct ompi_proc_t;
OMPI_DECLSPEC ompi_datatype_t*
ompi_ddt_create_from_packed_description( void** packed_buffer,
                                         struct ompi_proc_t* remote_processor );

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif  /* DATATYPE_H_HAS_BEEN_INCLUDED */
