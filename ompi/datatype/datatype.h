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
#include <string.h>
#include "include/constants.h"
#include "class/ompi_object.h"
#include "class/ompi_hash_table.h"
#include "class/ompi_pointer_array.h"
#include "mpi.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OMPI_DECLSPEC extern ompi_pointer_array_t *ompi_datatype_f_to_c_table;

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
#define DT_FLAG_IN_LOOP       0x0080  /**< we are inside a loop */
#define DT_FLAG_DATA          0x0100  /**< data or control structure */
#define DT_FLAG_ONE_SIDED     0x0200  /**< datatype can be used for one sided operations */
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
#define DT_FLAG_BASIC         (DT_FLAG_PREDEFINED | DT_FLAG_CONTIGUOUS | DT_FLAG_DATA | DT_FLAG_COMMITED)

typedef union dt_elem_desc dt_elem_desc_t;

typedef struct dt_type_desc {
    uint32_t          length;  /* the maximum number of elements in the description array */
    uint32_t          used;    /* the number of used elements in the description array */
    dt_elem_desc_t*   desc;
} dt_type_desc_t;

/* the data description.
 */
typedef struct ompi_datatype_t {
   ompi_object_t      super;    /**< basic superclass */
   unsigned long      size;     /**< total size in bytes of the memory used by the data if
                                 * the data is put on a contiguous buffer */
   uint32_t           align;    /**< data should be aligned to */
   long               true_lb;
   long               true_ub;  /**< the true ub of the data without user defined lb and ub */
   long               lb;       /**< lower bound in memory */
   long               ub;       /**< upper bound in memory */
   uint16_t           flags;    /**< the flags */
   uint16_t           id;       /**< data id, normally the index in the data array. */
   uint32_t           nbElems;  /**< total number of elements inside the datatype */
   uint64_t           bdt_used; /**< which basic datatypes are used in the data description */

   /* Attribute fields */
   ompi_hash_table_t *d_keyhash;
   int32_t            d_f_to_c_index;
   char               name[MPI_MAX_OBJECT_NAME];
   dt_type_desc_t     desc;     /**< the data description */
   dt_type_desc_t     opt_desc; /**< short description of the data used when conversion is useless
                                 * or in the send case (without conversion) */
   void*              args;     /**< data description for the user */

   /* basic elements count used to compute the size of the datatype for
    * remote nodes */
   uint32_t           btypes[DT_MAX_PREDEFINED];
} ompi_datatype_t;

OBJ_CLASS_DECLARATION( ompi_datatype_t );

int32_t ompi_ddt_init( void );
int32_t ompi_ddt_finalize( void );
ompi_datatype_t* ompi_ddt_create( int32_t expectedSize );
int32_t ompi_ddt_commit( ompi_datatype_t** );
int32_t ompi_ddt_destroy( ompi_datatype_t** );
static inline int32_t ompi_ddt_is_committed( const ompi_datatype_t* type ) 
{ return ((type->flags & DT_FLAG_COMMITED) == DT_FLAG_COMMITED); }
static inline int32_t ompi_ddt_is_overlapped( const ompi_datatype_t* type )
{ return ((type->flags & DT_FLAG_OVERLAP) == DT_FLAG_OVERLAP); }
static inline int32_t ompi_ddt_is_acceptable_for_one_sided( const ompi_datatype_t* type )
{ return ((type->flags & DT_FLAG_ONE_SIDED) == DT_FLAG_ONE_SIDED); }
void ompi_ddt_dump( const ompi_datatype_t* pData );
/* data creation functions */
OMPI_DECLSPEC int32_t ompi_ddt_duplicate( const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_contiguous( int count, const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_vector( int count, int bLength, long stride,
                                              const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_hvector( int count, int bLength, long stride,
                                               const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_indexed( int count, const int* pBlockLength, const int* pDisp,
                                               const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_hindexed( int count, const int* pBlockLength, const long* pDisp,
                                                const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_indexed_block( int count, int bLength, const int* pDisp,
                                                     const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_struct( int count, const int* pBlockLength, const long* pDisp,
                                              ompi_datatype_t* const* pTypes, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_resized( const ompi_datatype_t* oldType, long lb, long extent, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_subarray( int ndims, const int* pSizes,
                                                const int* pSubSizes, const int* pStarts,
                                                int order, const ompi_datatype_t* oldType, ompi_datatype_t** newType );
OMPI_DECLSPEC int32_t ompi_ddt_create_darray( int size, int rank, int ndims,
                                              const int* pGSizes, const int *pDistrib,
                                              const int* pDArgs, const int* pPSizes,
                                              int order, const ompi_datatype_t* oldType,
                                              ompi_datatype_t** newType );

OMPI_DECLSPEC int32_t ompi_ddt_add( ompi_datatype_t* pdtBase, const ompi_datatype_t* pdtAdd, uint32_t count,
                                    long disp, long extent );

static inline int32_t ompi_ddt_type_lb( const ompi_datatype_t* pData, long* disp )
{ *disp = pData->lb; return 0; }
static inline int32_t ompi_ddt_type_ub( const ompi_datatype_t* pData, long* disp )
{ *disp = pData->ub; return 0; }
static inline int32_t ompi_ddt_type_size ( const ompi_datatype_t* pData, int32_t *size )
{ *size = pData->size; return 0; }
static inline int32_t ompi_ddt_type_extent( const ompi_datatype_t* pData, long* extent )
{ *extent = pData->ub - pData->lb; return 0; }

static inline int32_t ompi_ddt_get_extent( const ompi_datatype_t* pData, long* lb, long* extent)
{ *lb = pData->lb; *extent = pData->ub - pData->lb; return 0; }
static inline int32_t ompi_ddt_get_true_extent( const ompi_datatype_t* pData, long* true_lb, long* true_extent)
{ *true_lb = pData->true_lb; *true_extent = (pData->true_ub - pData->true_lb); return 0; }

OMPI_DECLSPEC int32_t ompi_ddt_get_element_count( const ompi_datatype_t* pData, int32_t iSize );
OMPI_DECLSPEC int32_t ompi_ddt_copy_content_same_ddt( const ompi_datatype_t* pData, int32_t count,
                                                      char* pDestBuf, const char* pSrcBuf );

OMPI_DECLSPEC int32_t ompi_ddt_optimize_short( ompi_datatype_t* pData, int32_t count, dt_type_desc_t* pTypeDesc );
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
                                         int32_t * ca, long* a,
                                         int32_t * cd, ompi_datatype_t** d, int32_t * type);
OMPI_DECLSPEC int32_t ompi_ddt_set_args( ompi_datatype_t* pData,
                                         int32_t ci, int32_t ** i, 
                                         int32_t ca, long* a,
                                         int32_t cd, ompi_datatype_t** d,int32_t type);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif  /* DATATYPE_H_HAS_BEEN_INCLUDED */
