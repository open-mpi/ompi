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
#include "constants.h"
#include "lfc/lam_object.h"
#include "lfc/lam_hash_table.h"
#include "types.h"
#include "mpi.h"

#define DT_LOOP            0x00
#define DT_LB              0x01
#define DT_UB              0x02
#define DT_SPACE           0x03
#define DT_CHAR            0x04
#define DT_BYTE            0x05
#define DT_SHORT           0x06
#define DT_INT             0x07
#define DT_FLOAT           0x08
#define DT_LONG            0x09
#define DT_DOUBLE          0x0A
#define DT_LONG_LONG       0x0B
#define DT_LONG_DOUBLE     0x0C
#define DT_COMPLEX_FLOAT   0x0D
#define DT_COMPLEX_DOUBLE  0x0E
#define DT_END_LOOP        0x0F
/* if there are more basic datatypes than the number of bytes in the int type
 * the bdt_used field of the data description struct should be changed to long.
 */
#define DT_MAX_PREDEFINED  0x10

/* flags for the datatypes. */
#define DT_FLAG_DESTROYED  0x0001  /**< user destroyed but some other layers still have a reference */
#define DT_FLAG_COMMITED   0x0002  /**< ready to be used for a send/recv operation */
#define DT_FLAG_CONTIGUOUS 0x0004  /**< contiguous datatype */
#define DT_FLAG_OVERLAP    0x0008  /**< datatype is unpropper for a recv operation */
#define DT_FLAG_USER_LB    0x0010  /**< has a user defined LB */
#define DT_FLAG_USER_UB    0x0020  /**< has a user defined UB */
#define DT_FLAG_FOREVER    0x0040  /**< cannot be removed: initial and predefined datatypes */
#define DT_FLAG_IN_LOOP    0x0080  /**< we are inside a loop */
#define DT_FLAG_INITIAL    0x0100  /**< one of the initial datatype */
#define DT_FLAG_DATA       0x0200  /**< data or control structure */
#define DT_FLAG_BASIC      (DT_FLAG_INITIAL | DT_FLAG_COMMITED | DT_FLAG_FOREVER | DT_FLAG_CONTIGUOUS)

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

typedef struct {
      float r;
      float i;
} complex_float_t;

typedef struct {
      double r;
      double i;
} complex_double_t;

/* The basic memory zone description. The idea is to be able to represent the
 * data as a array of zones, thus allowing us to simply find when concatenating
 * several data leads to merging contiguous zones of memory.
 */
typedef struct __dt_zone_desc {
  int useless;
} dt_zone_desc_t;

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
   unsigned int bdt_used; /**< which basic datatypes are used in the data description */

   /* Attribute fields */
   lam_hash_table_t *keyhash;
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

extern dt_desc_t basicDatatypes[];

#if defined(__GNUC__)
#define LMAX(A,B)  ({ long _a = (A), _b = (B); (_a < _b ? _b : _a); })
#define LMIN(A,B)  ({ long _a = (A), _b = (B); (_a < _b ? _a : _b); })
#define IMAX(A,B)  ({ int _a = (A), _b = (B); (_a < _b ? _b : _a); })
#define IMIN(A,B)  ({ int _a = (A), _b = (B); (_a < _b ? _a : _b); })
#else
static long LMAX( long a, long b ) { return ( a < b ? b : a ); }
static long LMIN( long a, long b ) { return ( a < b ? a : b ); }
static int  IMAX( int a, int b ) { return ( a < b ? b : a ); }
static int  IMIN( int a, int b ) { return ( a < b ? a : b ); }
#endif  /* __GNU__ */

typedef struct __dt_stack {
      int index;
      int count;
      int end_loop;
      long disp;
} dt_stack_t;

typedef struct __dt_convert {
  char* buf;
  unsigned int length;
  dt_stack_t* pStack;
  dt_desc_t* pDesc;
} dt_convert_t;

int dt_load( void );
int dt_unload( void );
dt_desc_t* dt_create( int expectedSize );
int dt_commit( dt_desc_t** );
#define dt_free dt_destroy
int dt_free( dt_desc_t** );
int dt_destroy( dt_desc_t** );
void dt_dump( dt_desc_t* pData );
void dt_dump_complete( dt_desc_t* pData );
/* data creation functions */
int dt_duplicate( dt_desc_t* oldType, dt_desc_t** newType );
int dt_create_contiguous( size_t count, dt_desc_t* oldType, dt_desc_t** newType );
int dt_create_vector( size_t count, int bLength, long stride,
                      dt_desc_t* oldType, dt_desc_t** newType );
int dt_create_hvector( size_t count, int bLength, long stride,
                       dt_desc_t* oldType, dt_desc_t** newType );
int dt_create_indexed( size_t count, int* pBlockLength, int* pDisp,
                       dt_desc_t* oldType, dt_desc_t** newType );
int dt_create_hindexed( size_t count, int* pBlockLength, long* pDisp,
                        dt_desc_t* oldType, dt_desc_t** newType );
int dt_create_indexed_block( size_t count, int bLength, int* pDisp,
                             dt_desc_t* oldType, dt_desc_t** newType );
int dt_create_struct( size_t count, size_t* pBlockLength, long* pDisp,
                      dt_desc_t** pTypes, dt_desc_t** newType );
int dt_create_resized( dt_desc_t* oldType, long lb, long extent, dt_desc_t** newType );
int dt_create_subarray( int ndims, int* pSizes, int* pSubSizes, int* pStarts,
                        int order, dt_desc_t* oldType, dt_desc_t** newType );
int dt_create_darray( int size, int rank, int ndims, int* pGSizes, int *pDistrib,
                      int* pDArgs, int* pPSizes, int order, dt_desc_t* oldType,
                      dt_desc_t** newType );

int dt_add( dt_desc_t* pdtBase, dt_desc_t* pdtNew, unsigned int count, long disp, long extent );

static inline int dt_type_lb( dt_desc_t* pData, long* disp )
{ *disp = pData->lb; return 0; };
static inline int dt_type_ub( dt_desc_t* pData, long* disp )
{ *disp = pData->ub; return 0; };
static inline int dt_type_size ( dt_desc_t* pData, int *size )
{ *size = pData->size; return 0; };
static inline int dt_type_extent( dt_desc_t* pData, long* extent )
{ *extent = (pData->ub - pData->lb); return 0; };

static inline int dt_type_resize( dt_desc_t* pOld, long lb, long extent, dt_desc_t** pNew )
{ /* empty function */ return -1; };
static inline int dt_get_extent( dt_desc_t* pData, long* lb, long* extent)
{ *lb = pData->lb; *extent = pData->ub - pData->lb; return 0; };
static inline int dt_get_true_extent( dt_desc_t* pData, long* true_lb, long* true_extent)
{ *true_lb = pData->true_lb; *true_extent = (pData->true_ub - pData->true_lb); return 0; };

int dt_get_element_count( dt_desc_t* pData, size_t iSize );
int dt_copy_content_same_dt( dt_desc_t* pData, int count, char* pDestBuf, char* pSrcBuf );

#define dt_increase_ref(PDT) OBJ_RETAIN( PDT )
#define dt_decrease_ref(PDT) OBJ_RELEASE( PDT )

int dt_optimize_short( dt_desc_t* pData, int count, dt_type_desc_t* pTypeDesc );

#define REMOVE_FLAG( INT_VALUE, FLAG )  (INT_VALUE) = (INT_VALUE) ^ (FLAG)
#define SET_FLAG( INT_VALUE, FLAG )     (INT_VALUE) = (INT_VALUE) | (FLAG)
#define UNSET_FLAG( INT_VALUE, FLAG)    (INT_VALUE) = (INT_VALUE) & (~(FLAG))

#define REMOVE_CONTIGUOUS_FLAG( INT_VALUE )  REMOVE_FLAG(INT_VALUE, DT_FLAG_CONTIGUOUS)
#define SET_CONTIGUOUS_FLAG( INT_VALUE )     SET_FLAG(INT_VALUE, DT_FLAG_CONTIGUOUS)
#define UNSET_CONTIGUOUS_FLAG( INT_VALUE )   UNSET_FLAG(INT_VALUE, DT_FLAG_CONTIGUOUS)

/* flags for the datatypes */

typedef int (*conversion_fct_t)( unsigned int count,
                                 void* from, unsigned int from_len, long from_extent,
                                 void* to, unsigned int in_length, long to_extent,
                                 unsigned int* used );

/* keep the last 16 bits free for data flags */
#define CONVERTOR_USELESS 0x00010000
#define CONVERTOR_RECV    0x00020000
#define CONVERTOR_SEND    0x00040000

#define CONVERTOR_STATE_MASK       0xFF000000
#define CONVERTOR_STATE_START      0x01000000
#define CONVEROTR_STATE_COMPLETE   0x02000000
#define CONVERTOR_STATE_ALLOC      0x04000000

typedef struct __struct_convertor convertor_t;
typedef int (*convertor_advance_fct_t)( convertor_t* pConvertor,
                                        struct iovec* pInputv,
                                        unsigned int inputCount );

/* and now the convertor stuff */
struct __struct_convertor {
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

extern conversion_fct_t copy_functions[DT_MAX_PREDEFINED];

/* some convertor flags */
#define convertor_progress( PCONV, IOVEC, COUNT ) \
  (PCONV)->fAdvance( (PCONV), (IOVEC), (COUNT) );

/* and finally the convertor functions */
convertor_t* convertor_create( int remote_arch, int mode );
int convertor_init_for_send( convertor_t* pConv, unsigned int flags,
                             dt_desc_t* pData, int count, void* pUserBuf );
int convertor_init_for_recv( convertor_t* pConv, unsigned int flags,
                             dt_desc_t* pData, int count, void* pUserBuf );
convertor_t* convertor_get_copy( convertor_t* pConvertor );
int convertor_need_buffers( convertor_t* pConvertor );
int convertor_pack( convertor_t* pConv, struct iovec* in, unsigned int in_size );
int convertor_unpack( convertor_t* pConv, struct iovec* out, unsigned int out_size );
int convertor_destroy( convertor_t** ppConv );
int convertor_get_packed_size( convertor_t* pConv, unsigned int* pSize );
int convertor_get_unpacked_size( convertor_t* pConv, unsigned int* pSize );

#endif  /* DATATYPE_H_HAS_BEEN_INCLUDED */

