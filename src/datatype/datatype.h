/* -*- Mode: C; c-basic-offset:4 ; -*- */

/**
 * lam_datatype_t interface for LAM internal data type representation
 *
 * lam_datatype_t is a class which represents contiguous or
 * non-contiguous data together with constituent type-related
 * information.
 */

#ifndef DATATYPE_H_HAS_BEEN_INCLUDED
#define DATATYPE_H_HAS_BEEN_INCLUDED

#include "lam_config.h"
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <string.h>
#include "include/constants.h"
#include "lfc/lam_object.h"
#include "lfc/lam_hash_table.h"
#include "mpi.h"

/* if there are more basic datatypes than the number of bytes in the int type
 * the bdt_used field of the data description struct should be changed to long.
 */
#define DT_MAX_PREDEFINED  0x25

#define DT_INCREASE_STACK  32

/* the basic element. A data description is composed
 * by a set of basic elements.
 */
typedef struct __dt_elem_desc {
      unsigned short flags;  /**< flags for the record */
      unsigned short type;   /**< the basic data type id */
      unsigned int   count;  /**< number of elements */
      long           disp;   /**< displacement of the first element */
      unsigned int   extent; /**< extent of each element */
} dt_elem_desc_t;

typedef struct __dt_struct_desc {
   int length;  /* the maximum number of elements in the description array */
   int used;    /* the number of used elements in the description array */
   dt_elem_desc_t* desc;
} dt_type_desc_t;

/* the data description.
 */
typedef struct lam_datatype_t {
   lam_object_t super;    /**< basic superclass */
   unsigned int size;     /**< total size in bytes of the memory used by the data if
                           * the data is put on a contiguous buffer */
   long true_lb;
   long true_ub;          /**< the true ub of the data without user defined lb and ub */
   unsigned int align;    /**< data should be aligned to */
   long lb;               /**< lower bound in memory */
   long ub;               /**< upper bound in memory */
   unsigned short flags;  /**< the flags */
   unsigned short id;     /**< data id, normally the index in the data array. */
   unsigned int nbElems;  /**< total number of elements inside the datatype */
   unsigned long long bdt_used; /**< which basic datatypes are used in the data description */

   /* Attribute fields */
   lam_hash_table_t *d_keyhash;
   char name[MPI_MAX_OBJECT_NAME];

   dt_type_desc_t desc;   /**< the data description */
   dt_type_desc_t opt_desc; /**< short description of the data used when conversion is useless
                             * or in the send case (without conversion) */
   void*        args;     /**< data description for the user */

   /* basic elements count used to compute the size of the datatype for
    * remote nodes */
   unsigned int btypes[DT_MAX_PREDEFINED];
} dt_desc_t, lam_datatype_t;

OBJ_CLASS_DECLARATION( lam_datatype_t );

int lam_ddt_init( void );
int lam_ddt_finalize( void );
dt_desc_t* lam_ddt_create( int expectedSize );
int lam_ddt_commit( dt_desc_t** );
int lam_ddt_destroy( dt_desc_t** );
void lam_ddt_dump( dt_desc_t* pData );
void lam_ddt_dump_complete( dt_desc_t* pData );
/* data creation functions */
int lam_ddt_duplicate( dt_desc_t* oldType, dt_desc_t** newType );
int lam_ddt_create_contiguous( int count, dt_desc_t* oldType, dt_desc_t** newType );
int lam_ddt_create_vector( int count, int bLength, long stride,
                           dt_desc_t* oldType, dt_desc_t** newType );
int lam_ddt_create_hvector( int count, int bLength, long stride,
                            dt_desc_t* oldType, dt_desc_t** newType );
int lam_ddt_create_indexed( int count, int* pBlockLength, int* pDisp,
                            dt_desc_t* oldType, dt_desc_t** newType );
int lam_ddt_create_hindexed( int count, int* pBlockLength, long* pDisp,
                             dt_desc_t* oldType, dt_desc_t** newType );
int lam_ddt_create_indexed_block( int count, int bLength, int* pDisp,
                                  dt_desc_t* oldType, dt_desc_t** newType );
int lam_ddt_create_struct( int count, int* pBlockLength, long* pDisp,
                           dt_desc_t** pTypes, dt_desc_t** newType );
int lam_ddt_create_resized( dt_desc_t* oldType, long lb, long extent, dt_desc_t** newType );
int lam_ddt_create_subarray( int ndims, int* pSizes, int* pSubSizes, int* pStarts,
                             int order, dt_desc_t* oldType, dt_desc_t** newType );
int lam_ddt_create_darray( int size, int rank, int ndims, int* pGSizes, int *pDistrib,
                           int* pDArgs, int* pPSizes, int order, dt_desc_t* oldType,
                           dt_desc_t** newType );

int lam_ddt_add( dt_desc_t* pdtBase, dt_desc_t* pdtNew, unsigned int count, long disp, long extent );

static inline int lam_ddt_type_lb( dt_desc_t* pData, long* disp )
{ *disp = pData->lb; return 0; }
static inline int lam_ddt_type_ub( dt_desc_t* pData, long* disp )
{ *disp = pData->ub; return 0; }
static inline int lam_ddt_type_size ( dt_desc_t* pData, int *size )
{ *size = pData->size; return 0; }
static inline int lam_ddt_type_extent( dt_desc_t* pData, long* extent )
{ *extent = (pData->ub - pData->lb); return 0; }

static inline int lam_ddt_type_resize( dt_desc_t* pOld, long lb, long extent, dt_desc_t** pNew )
{ /* empty function */ return -1; }
static inline int lam_ddt_get_extent( dt_desc_t* pData, long* lb, long* extent)
{ *lb = pData->lb; *extent = pData->ub - pData->lb; return 0; }
static inline int lam_ddt_get_true_extent( dt_desc_t* pData, long* true_lb, long* true_extent)
{ *true_lb = pData->true_lb; *true_extent = (pData->true_ub - pData->true_lb); return 0; }

int lam_ddt_get_element_count( dt_desc_t* pData, int iSize );
int lam_ddt_copy_content_same_ddt( dt_desc_t* pData, int count, char* pDestBuf, char* pSrcBuf );

int lam_ddt_optimize_short( dt_desc_t* pData, int count, dt_type_desc_t* pTypeDesc );

/* flags for the datatypes */

typedef int (*conversion_fct_t)( unsigned int count,
                                 void* from, unsigned int from_len, long from_extent,
                                 void* to, unsigned int in_length, long to_extent,
                                 unsigned int* used );

typedef struct __dt_stack dt_stack_t;
typedef struct lam_convertor_t lam_convertor_t;
typedef int (*convertor_advance_fct_t)( lam_convertor_t* pConvertor,
                                        struct iovec* pInputv,
                                        unsigned int inputCount );

/* and now the convertor stuff */
struct lam_convertor_t {
   lam_object_t super;    /**< basic superclass */
   dt_desc_t* pDesc;
   long remoteArch;
   dt_stack_t* pStack;
   /* the convertor functions pointer */
   /* the local stack for the actual conversion */
   int converted;   /* the number of already converted elements */
   int bConverted;  /* the size of already converted elements in bytes */
   unsigned int flags;
   unsigned int count;
   unsigned int stack_pos;
   char* pBaseBuf;
   unsigned int available_space;
   void* freebuf;
   convertor_advance_fct_t fAdvance;
   conversion_fct_t* pFunctions;
};
OBJ_CLASS_DECLARATION( lam_convertor_t );

/* some convertor flags */
#define lam_convertor_progress( PCONV, IOVEC, COUNT ) \
  (PCONV)->fAdvance( (PCONV), (IOVEC), (COUNT) );

/* and finally the convertor functions */
lam_convertor_t* lam_convertor_create( int remote_arch, int mode );
lam_convertor_t* lam_convertor_get_copy( lam_convertor_t* pConvertor );
int lam_convertor_copy( lam_convertor_t* pSrcConv, lam_convertor_t* pDestConv );
int lam_convertor_init_for_send( lam_convertor_t* pConv, unsigned int flags,
                                 dt_desc_t* pData, int count,
                                 void* pUserBuf, int local_starting_point );
int lam_convertor_init_for_recv( lam_convertor_t* pConv, unsigned int flags,
                                 dt_desc_t* pData, int count,
                                 void* pUserBuf, int remote_starting_point );
int lam_convertor_need_buffers( lam_convertor_t* pConvertor );
int lam_convertor_pack( lam_convertor_t* pConv, struct iovec* in, unsigned int in_size );
int lam_convertor_unpack( lam_convertor_t* pConv, struct iovec* out, unsigned int out_size );
int lam_convertor_get_packed_size( lam_convertor_t* pConv, int* pSize );
int lam_convertor_get_unpacked_size( lam_convertor_t* pConv, int* pSize );
int lam_create_stack_with_pos( lam_convertor_t* pConv,
                               int local_starting_point,
                               int* local_sizes );
#endif  /* DATATYPE_H_HAS_BEEN_INCLUDED */

