/* -*- Mode: C; c-basic-offset:4 ; -*- */

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
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <string.h>
#include "include/constants.h"
#include "errhandler/errhandler.h"
#include "class/ompi_object.h"
#include "class/ompi_hash_table.h"
#include "class/ompi_pointer_array.h"
#include "mpi.h"

extern ompi_pointer_array_t *ompi_datatype_f_to_c_table;

/* if there are more basic datatypes than the number of bytes in the int type
 * the bdt_used field of the data description struct should be changed to long.
 */
#define DT_MAX_PREDEFINED  0x2A

#define DT_INCREASE_STACK  32

/* the basic element. A data description is composed
 * by a set of basic elements.
 */
typedef struct __dt_elem_desc {
      u_int16_t flags;  /**< flags for the record */
      u_int16_t type;   /**< the basic data type id */
      u_int32_t count;  /**< number of elements */
      long      disp;   /**< displacement of the first element */
      u_int32_t extent; /**< extent of each element */
} dt_elem_desc_t;

typedef struct __dt_struct_desc {
   int32_t length;  /* the maximum number of elements in the description array */
   int32_t used;    /* the number of used elements in the description array */
   dt_elem_desc_t* desc;
} dt_type_desc_t;

/* the data description.
 */
typedef struct ompi_datatype_t {
   ompi_object_t super;    /**< basic superclass */
   u_int32_t    size;     /**< total size in bytes of the memory used by the data if
                           * the data is put on a contiguous buffer */
   long         true_lb;
   long         true_ub;  /**< the true ub of the data without user defined lb and ub */
   u_int32_t    align;    /**< data should be aligned to */
   long         lb;       /**< lower bound in memory */
   long         ub;       /**< upper bound in memory */
   u_int16_t    flags;    /**< the flags */
   u_int16_t    id;       /**< data id, normally the index in the data array. */
   u_int32_t    nbElems;  /**< total number of elements inside the datatype */
   u_int64_t    bdt_used; /**< which basic datatypes are used in the data description */

   /* Attribute fields */
   ompi_hash_table_t *d_keyhash;
   int d_f_to_c_index;
   char name[MPI_MAX_OBJECT_NAME];
   dt_type_desc_t desc;     /**< the data description */
   dt_type_desc_t opt_desc; /**< short description of the data used when conversion is useless
                             * or in the send case (without conversion) */
   void*          args;     /**< data description for the user */

   /* basic elements count used to compute the size of the datatype for
    * remote nodes */
   u_int32_t      btypes[DT_MAX_PREDEFINED];
} dt_desc_t, ompi_datatype_t;

OBJ_CLASS_DECLARATION( ompi_datatype_t );

int ompi_ddt_init( void );
int ompi_ddt_finalize( void );
dt_desc_t* ompi_ddt_create( int expectedSize );
int ompi_ddt_commit( dt_desc_t** );
int ompi_ddt_destroy( dt_desc_t** );
void ompi_ddt_dump( dt_desc_t* pData );
void ompi_ddt_dump_complete( dt_desc_t* pData );
/* data creation functions */
int ompi_ddt_duplicate( dt_desc_t* oldType, dt_desc_t** newType );
int ompi_ddt_create_contiguous( int count, dt_desc_t* oldType, dt_desc_t** newType );
int ompi_ddt_create_vector( int count, int bLength, long stride,
                           dt_desc_t* oldType, dt_desc_t** newType );
int ompi_ddt_create_hvector( int count, int bLength, long stride,
                            dt_desc_t* oldType, dt_desc_t** newType );
int ompi_ddt_create_indexed( int count, int* pBlockLength, int* pDisp,
                            dt_desc_t* oldType, dt_desc_t** newType );
int ompi_ddt_create_hindexed( int count, int* pBlockLength, long* pDisp,
                             dt_desc_t* oldType, dt_desc_t** newType );
int ompi_ddt_create_indexed_block( int count, int bLength, int* pDisp,
                                  dt_desc_t* oldType, dt_desc_t** newType );
int ompi_ddt_create_struct( int count, int* pBlockLength, long* pDisp,
                           dt_desc_t** pTypes, dt_desc_t** newType );
int ompi_ddt_create_resized( dt_desc_t* oldType, long lb, long extent, dt_desc_t** newType );
int ompi_ddt_create_subarray( int ndims, int* pSizes, int* pSubSizes, int* pStarts,
                             int order, dt_desc_t* oldType, dt_desc_t** newType );
int ompi_ddt_create_darray( int size, int rank, int ndims, int* pGSizes, int *pDistrib,
                           int* pDArgs, int* pPSizes, int order, dt_desc_t* oldType,
                           dt_desc_t** newType );

int ompi_ddt_add( dt_desc_t* pdtBase, dt_desc_t* pdtNew, unsigned int count, long disp, long extent );

static inline int ompi_ddt_type_lb( dt_desc_t* pData, long* disp )
{ *disp = pData->lb; return 0; }
static inline int ompi_ddt_type_ub( dt_desc_t* pData, long* disp )
{ *disp = pData->ub; return 0; }
static inline int ompi_ddt_type_size ( dt_desc_t* pData, int *size )
{ *size = pData->size; return 0; }
static inline int ompi_ddt_type_extent( dt_desc_t* pData, long* extent )
{ *extent = (pData->ub - pData->lb); return 0; }

static inline int ompi_ddt_type_resize( dt_desc_t* pOld, long lb, long extent, dt_desc_t** pNew )
{ /* empty function */ return -1; }
static inline int ompi_ddt_get_extent( dt_desc_t* pData, long* lb, long* extent)
{ *lb = pData->lb; *extent = pData->ub - pData->lb; return 0; }
static inline int ompi_ddt_get_true_extent( dt_desc_t* pData, long* true_lb, long* true_extent)
{ *true_lb = pData->true_lb; *true_extent = (pData->true_ub - pData->true_lb); return 0; }

int ompi_ddt_get_element_count( dt_desc_t* pData, int iSize );
int ompi_ddt_copy_content_same_ddt( dt_desc_t* pData, int count, char* pDestBuf, char* pSrcBuf );

int ompi_ddt_optimize_short( dt_desc_t* pData, int count, dt_type_desc_t* pTypeDesc );

/* flags for the datatypes */

typedef int (*conversion_fct_t)( unsigned int count,
                                 void* from, unsigned int from_len, long from_extent,
                                 void* to, unsigned int in_length, long to_extent,
                                 unsigned int* used );

typedef struct __dt_stack dt_stack_t;
typedef struct ompi_convertor_t ompi_convertor_t;
typedef int (*convertor_advance_fct_t)( ompi_convertor_t* pConvertor,
                                        struct iovec* pInputv,
                                        unsigned int inputCount );

/* and now the convertor stuff */
struct ompi_convertor_t {
    ompi_object_t super;    /**< basic superclass */
    dt_desc_t* pDesc;
    long remoteArch;
    dt_stack_t* pStack;
    /* the convertor functions pointer */
    /* the local stack for the actual conversion */
    int32_t   converted;   /* the number of already converted elements */
    int32_t   bConverted;  /* the size of already converted elements in bytes */
    u_int32_t flags;
    u_int32_t count;
    u_int32_t stack_pos;
    char*     pBaseBuf;
    u_int32_t available_space;
    void*     freebuf;
    convertor_advance_fct_t fAdvance;
    conversion_fct_t* pFunctions;
};
OBJ_CLASS_DECLARATION( ompi_convertor_t );

/* some convertor flags */
#define ompi_convertor_progress( PCONV, IOVEC, COUNT ) \
  (PCONV)->fAdvance( (PCONV), (IOVEC), (COUNT) );

/* and finally the convertor functions */
ompi_convertor_t* ompi_convertor_create( int remote_arch, int mode );
ompi_convertor_t* ompi_convertor_get_copy( ompi_convertor_t* pConvertor );
int ompi_convertor_copy( ompi_convertor_t* pSrcConv, ompi_convertor_t* pDestConv );
int ompi_convertor_init_for_send( ompi_convertor_t* pConv, unsigned int flags,
                                 dt_desc_t* pData, int count,
                                 void* pUserBuf, int local_starting_point );
int ompi_convertor_init_for_recv( ompi_convertor_t* pConv, unsigned int flags,
                                 dt_desc_t* pData, int count,
                                 void* pUserBuf, int remote_starting_point );
int ompi_convertor_need_buffers( ompi_convertor_t* pConvertor );
int ompi_convertor_pack( ompi_convertor_t* pConv, struct iovec* in, unsigned int in_size );
int ompi_convertor_unpack( ompi_convertor_t* pConv, struct iovec* out, unsigned int out_size );
int ompi_convertor_get_packed_size( ompi_convertor_t* pConv, int* pSize );
int ompi_convertor_get_unpacked_size( ompi_convertor_t* pConv, int* pSize );
int ompi_create_stack_with_pos( ompi_convertor_t* pConv,
                               int local_starting_point,
                               int* local_sizes );

/* temporary function prototypes. They should move in other place later. */
int ompi_ddt_get_args( dt_desc_t* pData, int which,
                      int * ci, int * i,
                      int * ca, MPI_Aint * a,
                      int * cd, MPI_Datatype * d, int * type);
int ompi_ddt_set_args( dt_desc_t* pData,
                      int ci, int ** i, 
                      int ca, MPI_Aint* a,
                      int cd, MPI_Datatype* d,int type);
/* VPS: Added  */
int ompi_ddt_sndrcv(void *sbuf, int scount, MPI_Datatype sdtype, void *rbuf,
		 int rcount, MPI_Datatype rdtype, int tag, MPI_Comm comm);

#endif  /* DATATYPE_H_HAS_BEEN_INCLUDED */

