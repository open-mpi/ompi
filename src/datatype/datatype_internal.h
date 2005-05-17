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

#ifndef DATATYPE_INTERNAL_H_HAS_BEEN_INCLUDED
#define DATATYPE_INTERNAL_H_HAS_BEEN_INCLUDED

#include "util/output.h"

extern int ompi_ddt_dfd;

#if defined(VERBOSE)
#  define DDT_DUMP_STACK( PSTACK, STACK_POS, PDESC, NAME ) \
     ompi_ddt_dump_stack( (PSTACK), (STACK_POS), (PDESC), (NAME) )
#  if defined(ACCEPT_C99)
#    define DUMP( ARGS... )          ompi_output(ompi_ddt_dfd, __VA_ARGS__)
#  else
#    if defined(__GNUC__) && !defined(__STDC__)
#      define DUMP(ARGS...)          ompi_output( ompi_ddt_dfd, ARGS)
#  else
static inline void DUMP( char* fmt, ... )
{
   va_list list;

   va_start( list, fmt );
   ompi_output( ompi_ddt_dfd, fmt, list );
   va_end( list );
}
#      define DUMP                   printf
#    endif  /* __GNUC__ && !__STDC__ */
#  endif  /* ACCEPT_C99 */
#else
#  define DDT_DUMP_STACK( PSTACK, STACK_POS, PDESC, NAME )
#  if defined(ACCEPT_C99)
#    define DUMP(ARGS...)
#  else
#    if defined(__GNUC__) && !defined(__STDC__)
#      define DUMP(ARGS...)
#    else
       static inline void DUMP( char* fmt, ...) { /* empty hopefully removed by the compiler */ }
#    endif  /* __GNUC__ && !__STDC__ */
#  endif  /* ACCEPT_C99 */
#endif  /* VERBOSE */

#define DT_LOOP                    0x00
#define DT_END_LOOP                0x01
#define DT_LB                      0x02
#define DT_UB                      0x03
#define DT_CHAR                    0x04
#define DT_CHARACTER               0x05
#define DT_UNSIGNED_CHAR           0x06
#define DT_BYTE                    0x07
#define DT_SHORT                   0x08
#define DT_UNSIGNED_SHORT          0x09
#define DT_INT                     0x0A
#define DT_UNSIGNED_INT            0x0B
#define DT_LONG                    0x0C
#define DT_UNSIGNED_LONG           0x0D
#define DT_LONG_LONG               0x0E
#define DT_LONG_LONG_INT           0x0F
#define DT_UNSIGNED_LONG_LONG      0x10
#define DT_FLOAT                   0x11
#define DT_DOUBLE                  0x12
#define DT_LONG_DOUBLE             0x13
#define DT_COMPLEX_FLOAT           0x14
#define DT_COMPLEX_DOUBLE          0x15
#define DT_COMPLEX_LONG_DOUBLE     0x16
#define DT_PACKED                  0x17
#define DT_LOGIC                   0x18
#define DT_FLOAT_INT               0x19
#define DT_DOUBLE_INT              0x1A
#define DT_LONG_DOUBLE_INT         0x1B
#define DT_LONG_INT                0x1C
#define DT_2INT                    0x1D
#define DT_SHORT_INT               0x1E
#define DT_INTEGER                 0x1F
#define DT_REAL                    0x20
#define DT_DBLPREC                 0x21
#define DT_2REAL                   0x22
#define DT_2DBLPREC                0x23
#define DT_2INTEGER                0x24
#define DT_WCHAR                   0x25
#define DT_2COMPLEX                0x26
#define DT_2DOUBLE_COMPLEX         0x27
#define DT_CXX_BOOL                0x28
#define DT_UNAVAILABLE             0x29
/* If the number of basic datatype should change update
 * DT_MAX_PREDEFINED in datatype.h
 */
#if DT_MAX_PREDEFINED <= DT_UNAVAILABLE
#error DT_MAX_PREDEFINED should be updated
#endif  /* safe check for max predefined datatypes. */

#define DT_INCREASE_STACK     32

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct ddt_elem_id_description {
    uint16_t   flags;  /**< flags for the record */
    uint16_t   type;   /**< the basic data type id */
};
typedef struct ddt_elem_id_description ddt_elem_id_description;

/* the basic element. A data description is composed
 * by a set of basic elements.
 */
struct ddt_elem_desc {
    ddt_elem_id_description common; /**< basic data description and flags */
    uint32_t                count;  /**< number of elements */
    long                    disp;   /**< displacement of the first element */
    int32_t                 extent; /**< extent of each element */
};
typedef struct ddt_elem_desc ddt_elem_desc_t;

struct ddt_loop_desc {
    ddt_elem_id_description common; /**< basic data description and flags */
    uint32_t                loops;  /**< number of elements */
    long                    extent; /**< extent of the whole loop */
    uint32_t                items;  /**< number of items in the loop */
};
typedef struct ddt_loop_desc ddt_loop_desc_t;

struct ddt_endloop_desc {
    ddt_elem_id_description common;       /**< basic data description and flags */
    uint32_t                items;        /**< number of elements */
    long                    total_extent; /**< total extent of the loop taking in account the repetitions */
    uint32_t                size;         /**< real size of the data in the loop */
};
typedef struct ddt_endloop_desc ddt_endloop_desc_t;

union dt_elem_desc {
    ddt_elem_desc_t    elem;
    ddt_loop_desc_t    loop;
    ddt_endloop_desc_t end_loop;
};

#define CREATE_LOOP_START( _place, _count, _items, _extent, _flags ) \
do { \
    (_place)->loop.common.type   = DT_LOOP; \
    (_place)->loop.common.flags  = (_flags) & ~DT_FLAG_DATA; \
    (_place)->loop.loops         = (_count); \
    (_place)->loop.items         = (_items); \
    (_place)->loop.extent        = (_extent); \
} while(0)

#define CREATE_LOOP_END( _place, _items, _total_extent, _size, _flags ) \
do { \
   (_place)->end_loop.common.type = DT_END_LOOP; \
   (_place)->end_loop.common.flags = (_flags) & ~DT_FLAG_DATA; \
   (_place)->end_loop.items = (_items); \
   (_place)->end_loop.total_extent = (_total_extent);  /* the final extent for the loop */ \
   (_place)->end_loop.size = (_size);                  /* the size inside the loop */ \
} while(0)

#define CREATE_ELEM( _place, _type, _flags, _count, _disp, _extent ) \
do { \
    (_place)->elem.common.flags = (_flags) | DT_FLAG_DATA; \
    (_place)->elem.common.type  = (_type); \
    (_place)->elem.count        = (_count); \
    (_place)->elem.disp         = (_disp); \
    (_place)->elem.extent       = (_extent); \
} while(0)

/* keep the last 16 bits free for data flags */
#define CONVERTOR_USELESS          0x00010000
#define CONVERTOR_RECV             0x00020000
#define CONVERTOR_SEND             0x00040000
#define CONVERTOR_HOMOGENEOUS      0x00080000
#define CONVERTOR_STATE_MASK       0xFF000000
#define CONVERTOR_STATE_START      0x01000000
#define CONVEROTR_STATE_COMPLETE   0x02000000
#define CONVERTOR_STATE_ALLOC      0x04000000

typedef struct {
    float r;
    float i;
} ompi_complex_float_t;

typedef struct {
    double r;
    double i;
} ompi_complex_double_t;

typedef struct {
    long double r;
    long double i;
} ompi_complex_long_double_t;

extern const ompi_datatype_t* ompi_ddt_basicDatatypes[];
#define BASIC_DDT_FROM_ELEM( ELEM ) (ompi_ddt_basicDatatypes[(ELEM).elem.common.type])

extern conversion_fct_t ompi_ddt_copy_functions[DT_MAX_PREDEFINED];
extern int32_t ompi_ddt_external32_init( void );
extern void ompi_ddt_dump_stack( const dt_stack_t* pStack, int stack_pos,
                                 const dt_elem_desc_t* pDesc, const char* name );
extern void ompi_convertor_dump( ompi_convertor_t* convertor );

#define SAVE_STACK( PSTACK, INDEX, COUNT, DISP, END_LOOP) \
do { \
   (PSTACK)->index    = (INDEX); \
   (PSTACK)->count    = (COUNT); \
   (PSTACK)->disp     = (DISP); \
   (PSTACK)->end_loop = (END_LOOP); \
} while(0)

#define PUSH_STACK( PSTACK, STACK_POS, INDEX, COUNT, DISP, END_LOOP) \
do { \
   dt_stack_t* pTempStack = (PSTACK) + 1; \
   SAVE_STACK( pTempStack, (INDEX), (COUNT), (DISP), (END_LOOP) );  \
   (STACK_POS)++; \
   (PSTACK) = pTempStack; \
} while(0)

#define MEMCPY( DST, SRC, BLENGTH ) \
do { \
    /*ompi_output( 0, "memcpy dest = %p src = %p length = %d\n", (void*)(DST), (void*)(SRC), (int)(BLENGTH) ); */\
    memcpy( (DST), (SRC), (BLENGTH) ); \
} while (0)

#if OMPI_ENABLE_DEBUG
void ompi_ddt_safeguard_pointer( const void* actual_ptr, int length, const void* initial_ptr,
                                 const ompi_datatype_t* pData, int count );
#define OMPI_DDT_SAFEGUARD_POINTER( ACTPTR, LENGTH, INITPTR, PDATA, COUNT ) \
    ompi_ddt_safeguard_pointer( (ACTPTR), (LENGTH), (INITPTR), (PDATA), (COUNT) )
#else
#define OMPI_DDT_SAFEGUARD_POINTER( ACTPTR, LENGTH, INITPTR, PDATA, COUNT )
#endif  /* OMPI_ENABLE_DEBUG */

#ifdef USELESS
#define MEMCPY_LIMIT 1

#define MEMCPY( DST, SRC, BLENGTH ) \
do { \
   if( (BLENGTH) < (MEMCPY_LIMIT) ) { \
      long mask = sizeof(int) - 1; \
      char *dst = (char*)(DST), *src = (char*)(SRC); \
      int i; \
      if( ((long)(DST) & mask) == ((long)(SRC) & mask) ) { \
         int *idst = (int*)((long)(DST) & (~mask)); \
         int *isrc = (int*)((long)(SRC) & (~mask)); \
         for( i = 0; i < ((long)(DST) & mask); i++ ) { \
            *dst = *src; dst++; src++; \
         } \
         if( ((char*)idst) != dst ) { \
            idst++; isrc++; \
         } \
         for( i = 0; i < ((BLENGTH) >> 2); i++ ) { \
            *idst = *isrc; idst++; isrc++; \
         } \
      } else { \
         for( i = 0; i < (BLENGTH); i++ ) { \
            *dst = *src; dst++; src++; \
         } \
      } \
   } else \
      memcpy( (DST), (SRC), (BLENGTH) ); \
} while(0)
#endif  /* USELESS */

static inline int GET_FIRST_NON_LOOP( const dt_elem_desc_t* _pElem )
{
    int index = 0;

    /* We dont have to check for the end as we always put an END_LOOP
     * at the end of all datatype descriptions.
     */
    while( _pElem->elem.common.type == DT_LOOP ) {
        ++_pElem; index++;
    }
    return index;
}

int ompi_convertor_create_stack_with_pos_general( ompi_convertor_t* pConvertor,
                                                  int starting_point, const int* sizes );
static inline
int ompi_convertor_create_stack_with_pos_contig( ompi_convertor_t* pConvertor,
                                                 int starting_point, const int* sizes )
{
    dt_stack_t* pStack;   /* pointer to the position on the stack */
    ompi_datatype_t* pData = pConvertor->pDesc;
    dt_elem_desc_t* pElems;
    uint32_t count;
    long extent;

    pStack = pConvertor->pStack;

    pStack[0].count    = pConvertor->count;
    pStack[0].index    = -1;
    if( pData->opt_desc.desc != NULL ) {
        pElems = pData->opt_desc.desc;
        pStack[0].end_loop = pData->opt_desc.used;
    } else {
        pElems = pData->desc.desc;
        pStack[0].end_loop = pData->desc.used;
    }

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
    ompi_datatype_t* pData = pConvertor->pDesc;
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
    pElems = pData->desc.desc;
    pStack[0].end_loop = pData->desc.used;
    if( pConvertor->flags & CONVERTOR_HOMOGENEOUS ) {
        if( pData->opt_desc.used > 0 ) {
            pElems = pData->opt_desc.desc;
            pConvertor->pStack[0].end_loop = pData->opt_desc.used;
        }
    }
    /* In the case where the datatype start with loops, we should push them on the stack.
     * Otherwise when we reach the end_loop field we will pop too many entries and finish
     * by overriding other places in memory. Now the big question is when to stop creating
     * the entries on the stack ? Should I stop when I reach the first data element or
     * should I stop on the first contiguous loop ?
     */
    while( pElems[index].elem.common.type == DT_LOOP ) {
        PUSH_STACK( pStack, pConvertor->stack_pos, index,
                    pElems[index].loop.loops, 0, pElems[index].loop.items );
        index++;
    }
    if( pElems[index].elem.common.flags & DT_FLAG_DATA ) {  /* let's stop here */
        PUSH_STACK( pStack, pConvertor->stack_pos, index,
                    pElems[index].elem.count, pElems[index].elem.disp, 0 );
    } else {
        ompi_output( 0, "Here we should have a data in the datatype description\n" );
    }
    pConvertor->bConverted = 0;
    return OMPI_SUCCESS;
}

static inline void
convertor_init_generic( ompi_convertor_t* pConv, const ompi_datatype_t* datatype, int count,
			const void* pUserBuf )
{
    uint32_t required_stack_length = datatype->btypes[DT_LOOP] + 3;

    OBJ_RETAIN( datatype );
    if( pConv->pDesc != datatype ) {
        pConv->pDesc = (ompi_datatype_t*)datatype;
        if( pConv->pStack != NULL ) {
            if( pConv->stack_size > DT_STATIC_STACK_SIZE )
                free( pConv->pStack );
        }
        pConv->pStack = pConv->static_stack;
	pConv->stack_size = DT_STATIC_STACK_SIZE;
    }
    if( required_stack_length > pConv->stack_size ) {
	pConv->stack_size = required_stack_length;
	pConv->pStack     = (dt_stack_t*)malloc(sizeof(dt_stack_t) * pConv->stack_size );
    }
    
    pConv->pBaseBuf = (void*)pUserBuf;
    pConv->count = count;
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif  /* DATATYPE_INTERNAL_H_HAS_BEEN_INCLUDED */
