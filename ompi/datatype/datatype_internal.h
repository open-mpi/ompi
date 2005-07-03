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

#include "opal/util/output.h"

extern int ompi_ddt_dfd;

#if defined(VERBOSE)
#  define DDT_DUMP_STACK( PSTACK, STACK_POS, PDESC, NAME ) \
     ompi_ddt_dump_stack( (PSTACK), (STACK_POS), (PDESC), (NAME) )
#  if defined(ACCEPT_C99)
#    define DUMP( ARGS... )          opal_output(ompi_ddt_dfd, __VA_ARGS__)
#  else
#    if defined(__GNUC__) && !defined(__STDC__)
#      define DUMP(ARGS...)          opal_output( ompi_ddt_dfd, ARGS)
#  else
static inline void DUMP( char* fmt, ... )
{
   va_list list;

   va_start( list, fmt );
   opal_output( ompi_ddt_dfd, fmt, list );
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

/* There 3 types of predefined data types. 
 * - the basic one composed by just one basic datatype which are definitively contiguous
 * - the derived ones where the same basic type is used multiple times. They should be most
 *   of the time contiguous.
 * - and finally the derived one where multiple basic types are used. Depending on the architecture
 *   they can be contiguous or not.
 *
 * At this level we do not care from which language the datatype came from (C, C++ or FORTRAN),
 * we only focus on their internal representation in the host memory.
 */
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
#define DT_PACKED                  0x14
#define DT_WCHAR                   0x15
#define DT_CXX_BOOL                0x16
#define DT_LOGIC                   0x17
#define DT_INTEGER                 0x18
#define DT_REAL                    0x19
#define DT_DBLPREC                 0x1A
/*
 * This is not a datatype. It contain the number of basic datatypes.
 */
#define DT_MAX_BASIC               0x1B
/*
 * Derived datatypes supposely contiguous
 */
#define DT_COMPLEX_FLOAT           0x1B
#define DT_COMPLEX_DOUBLE          0x1C
#define DT_COMPLEX_LONG_DOUBLE     0x1D
#define DT_2INT                    0x1E
#define DT_2INTEGER                0x1F
#define DT_2REAL                   0x20
#define DT_2DBLPREC                0x21
#define DT_2COMPLEX                0x22
#define DT_2DOUBLE_COMPLEX         0x23
/*
 * Derived datatypes which will definitively be non contiguous on some architectures.
 */
#define DT_FLOAT_INT               0x24
#define DT_DOUBLE_INT              0x25
#define DT_LONG_DOUBLE_INT         0x26
#define DT_LONG_INT                0x27
#define DT_SHORT_INT               0x28
#define DT_UNAVAILABLE             0x29
/* If the number of basic datatype should change update
 * DT_MAX_PREDEFINED in datatype.h
 */
#if DT_MAX_PREDEFINED <= DT_UNAVAILABLE
#error DT_MAX_PREDEFINED should be updated to the next value after the DT_UNAVAILABLE define
#endif  /* safe check for max predefined datatypes. */

#define DT_INCREASE_STACK     8

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

extern int32_t ompi_ddt_external32_init( void );

#define SAVE_STACK( PSTACK, INDEX, TYPE, COUNT, DISP, END_LOOP) \
do { \
   (PSTACK)->index    = (INDEX); \
   (PSTACK)->type     = (TYPE); \
   (PSTACK)->count    = (COUNT); \
   (PSTACK)->disp     = (DISP); \
   (PSTACK)->end_loop = (END_LOOP); \
} while(0)

#define PUSH_STACK( PSTACK, STACK_POS, INDEX, TYPE, COUNT, DISP, END_LOOP) \
do { \
   dt_stack_t* pTempStack = (PSTACK) + 1; \
   SAVE_STACK( pTempStack, (INDEX), (TYPE), (COUNT), (DISP), (END_LOOP) );  \
   (STACK_POS)++; \
   (PSTACK) = pTempStack; \
} while(0)

#define MEMCPY( DST, SRC, BLENGTH ) \
do { \
    /*opal_output( 0, "memcpy dest = %p src = %p length = %d\n", (void*)(DST), (void*)(SRC), (int)(BLENGTH) ); */\
    memcpy( (DST), (SRC), (BLENGTH) ); \
} while (0)

#if OMPI_ENABLE_DEBUG
OMPI_DECLSPEC int ompi_ddt_safeguard_pointer_debug_breakpoint( const void* actual_ptr, int length,
                                                               const void* initial_ptr,
                                                               const ompi_datatype_t* pData,
                                                               int count );
#define OMPI_DDT_SAFEGUARD_POINTER( ACTPTR, LENGTH, INITPTR, PDATA, COUNT ) \
{ \
    char *__lower_bound = (char*)(INITPTR), *__upper_bound; \
    assert( ((LENGTH) != 0) && ((COUNT) != 0) ); \
    __lower_bound += (PDATA)->true_lb; \
    __upper_bound = (INITPTR) + ((PDATA)->ub - (PDATA)->lb) * ((COUNT) - 1) + (PDATA)->true_ub; \
    if( ((ACTPTR) < __lower_bound) || ((ACTPTR) >= __upper_bound) ) { \
        opal_output( 0, "%s:%d\n\tPointer %p size %d is outside [%p,%p] for\n\tbase ptr %p count %d and data \n", \
                     __FILE__, __LINE__, (ACTPTR), (LENGTH), __lower_bound, __upper_bound, \
                     (INITPTR), (COUNT) ); \
        ompi_ddt_dump( (PDATA) ); \
        ompi_ddt_safeguard_pointer_debug_breakpoint( (ACTPTR), (LENGTH), (INITPTR), (PDATA), (COUNT) ); \
    } \
}

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

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif  /* DATATYPE_INTERNAL_H_HAS_BEEN_INCLUDED */
