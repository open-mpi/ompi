/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/convertor.h"
#include "ompi/datatype/datatype_internal.h"
#include "ompi/datatype/datatype_checksum.h"
#include "ompi/datatype/convertor_internal.h"

/* 
 * This function is used to copy data from one buffer to another.  The assumption
 *     is that the number of bytes per element to copy at the source and destination 
 *     are the same.
 *   count - number of instances of a given data-type to copy
 *   from - point to the source buffer
 *   to - pointer to the destination buffer
 *   from_len - length of source buffer (in bytes)
 *   to_len - length of destination buffer (in bytes)
 *   from_extent - extent of the source data type (in bytes)
 *   to_extent - extent of the destination data type (in bytes)
 *   
 * Return value: Number of elements of type TYPE copied
 */
#define COPY_TYPE( TYPENAME, TYPE, COUNT )                                      \
static int copy_##TYPENAME( ompi_convertor_t *pConvertor, uint32_t count,       \
                            char* from, size_t from_len, ptrdiff_t from_extent, \
                            char* to, size_t to_len, ptrdiff_t to_extent,       \
                            ptrdiff_t *advance)                         \
{                                                                       \
    uint32_t i;                                                         \
    size_t remote_TYPE_size = sizeof(TYPE) * (COUNT); /* TODO */        \
    size_t local_TYPE_size = (COUNT) * sizeof(TYPE);                    \
                                                                        \
    /* make sure the remote buffer is large enough to hold the data */  \
    if( (remote_TYPE_size * count) > from_len ) {                       \
        count = (uint32_t)(from_len / remote_TYPE_size);                \
        if( (count * remote_TYPE_size) != from_len ) {                  \
            DUMP( "oops should I keep this data somewhere (excedent %d bytes)?\n", \
                  from_len - (count * remote_TYPE_size) );              \
        }                                                               \
        DUMP( "correct: copy %s count %d from buffer %p with length %d to %p space %d\n", \
              #TYPE, count, from, from_len, to, to_len );               \
    } else                                                              \
        DUMP( "         copy %s count %d from buffer %p with length %d to %p space %d\n", \
              #TYPE, count, from, from_len, to, to_len );               \
                                                                        \
    if( (from_extent == (ptrdiff_t)local_TYPE_size) &&                  \
        (to_extent == (ptrdiff_t)remote_TYPE_size) ) {                  \
        /* copy of contigous data at both source and destination */     \
        MEMCPY( to, from, count * local_TYPE_size );                    \
    } else {                                                            \
        /* source or destination are non-contigous */                   \
        for( i = 0; i < count; i++ ) {                                  \
            MEMCPY( to, from, local_TYPE_size );                        \
            to += to_extent;                                            \
            from += from_extent;                                        \
        }                                                               \
    }                                                                   \
    *advance = count * from_extent;                                     \
    return count;                                                       \
}

/* 
 * This function is used to copy data from one buffer to another.  The assumption
 *     is that the number of bytes per element to copy at the source and destination 
 *     are the same.
 *   count - number of instances of a given data-type to copy
 *   from - point to the source buffer
 *   to - pointer to the destination buffer
 *   from_len - length of source buffer (in bytes)
 *   to_len - length of destination buffer (in bytes)
 *   from_extent - extent of the source data type (in bytes)
 *   to_extent - extent of the destination data type (in bytes)
 *   
 * Return value: Number of elements of type TYPE copied
 */
#define COPY_CONTIGUOUS_BYTES( TYPENAME, COUNT )                                          \
static int copy_##TYPENAME##_##COUNT( ompi_convertor_t *pConvertor, uint32_t count,       \
                                      char* from, size_t from_len, ptrdiff_t from_extent, \
                                      char* to, size_t to_len, ptrdiff_t to_extent,       \
                                      ptrdiff_t *advance )              \
{                                                                       \
    uint32_t i;                                                         \
    size_t remote_TYPE_size = (size_t)(COUNT); /* TODO */               \
    size_t local_TYPE_size = (size_t)(COUNT);                           \
                                                                        \
    if( (remote_TYPE_size * count) > from_len ) {                       \
        count = (uint32_t)(from_len / remote_TYPE_size);                \
        if( (count * remote_TYPE_size) != from_len ) {                  \
            DUMP( "oops should I keep this data somewhere (excedent %d bytes)?\n", \
                  from_len - (count * remote_TYPE_size) );              \
        }                                                               \
        DUMP( "correct: copy %s count %d from buffer %p with length %d to %p space %d\n", \
              #TYPENAME, count, from, from_len, to, to_len );           \
    } else                                                              \
        DUMP( "         copy %s count %d from buffer %p with length %d to %p space %d\n", \
              #TYPENAME, count, from, from_len, to, to_len );           \
                                                                        \
    if( (from_extent == (ptrdiff_t)local_TYPE_size) &&                  \
        (to_extent == (ptrdiff_t)remote_TYPE_size) ) {                  \
        MEMCPY( to, from, count * local_TYPE_size );                    \
    } else {                                                            \
        for( i = 0; i < count; i++ ) {                                  \
            MEMCPY( to, from, local_TYPE_size );                        \
            to += to_extent;                                            \
            from += from_extent;                                        \
        }                                                               \
    }                                                                   \
    *advance = count * from_extent;                                     \
    return count;                                                       \
}

/* set up copy functions for the basic C MPI data types */
COPY_TYPE( char, char, 1 )
COPY_TYPE( short, short, 1 )
COPY_TYPE( int, int, 1 )
COPY_TYPE( float, float, 1 )
COPY_TYPE( long, long, 1 )
COPY_TYPE( double, double, 1 )
COPY_TYPE( long_long, long long, 1 )
COPY_TYPE( long_double, long double, 1 )
COPY_TYPE( complex_float, ompi_complex_float_t, 1 )
COPY_TYPE( complex_double, ompi_complex_double_t, 1 )
COPY_TYPE( complex_long_double, ompi_complex_long_double_t, 1 )
COPY_TYPE( wchar, wchar_t, 1 )
COPY_TYPE( 2int, int, 2 )
COPY_TYPE( 2float, float, 2 )
COPY_TYPE( 2double, double, 2 )
COPY_TYPE( 2complex_float, ompi_complex_float_t, 2 )
COPY_TYPE( 2complex_double, ompi_complex_double_t, 2 )

#if OMPI_SIZEOF_FORTRAN_LOGICAL == 1 || SIZEOF_BOOL == 1
#define REQUIRE_COPY_BYTES_1 1
#else
#define REQUIRE_COPY_BYTES_1 0
#endif

#if OMPI_SIZEOF_FORTRAN_LOGICAL == 2 || SIZEOF_BOOL == 2
#define REQUIRE_COPY_BYTES_2 1
#else
#define REQUIRE_COPY_BYTES_2 0
#endif

#if OMPI_SIZEOF_FORTRAN_LOGICAL == 4 || SIZEOF_BOOL == 4
#define REQUIRE_COPY_BYTES_4 1
#else
#define REQUIRE_COPY_BYTES_4 0
#endif

#if (SIZEOF_FLOAT + SIZEOF_INT) == 8 || (SIZEOF_LONG + SIZEOF_INT) == 8 || SIZEOF_BOOL == 8
#define REQUIRE_COPY_BYTES_8 1
#else
#define REQUIRE_COPY_BYTES_8 0
#endif

#if (SIZEOF_DOUBLE + SIZEOF_INT) == 12 || (SIZEOF_LONG + SIZEOF_INT) == 12
#define REQUIRE_COPY_BYTES_12 1
#else
#define REQUIRE_COPY_BYTES_12 0
#endif

#if (SIZEOF_LONG_DOUBLE + SIZEOF_INT) == 16
#define REQUIRE_COPY_BYTES_16 1
#else
#define REQUIRE_COPY_BYTES_16 0
#endif

#if (SIZEOF_LONG_DOUBLE + SIZEOF_INT) == 20
#define REQUIRE_COPY_BYTES_20 1
#else
#define REQUIRE_COPY_BYTES_20 0
#endif

#if REQUIRE_COPY_BYTES_1
COPY_CONTIGUOUS_BYTES( bytes, 1 )
#endif  /* REQUIRE_COPY_BYTES_1 */
#if REQUIRE_COPY_BYTES_2
COPY_CONTIGUOUS_BYTES( bytes, 2 )
#endif  /* REQUIRE_COPY_BYTES_2 */
#if REQUIRE_COPY_BYTES_4
COPY_CONTIGUOUS_BYTES( bytes, 4 )
#endif  /* REQUIRE_COPY_BYTES_4 */
#if REQUIRE_COPY_BYTES_8
COPY_CONTIGUOUS_BYTES( bytes, 8 )
#endif  /* REQUIRE_COPY_BYTES_8 */
#if REQUIRE_COPY_BYTES_12
COPY_CONTIGUOUS_BYTES( bytes, 12 )
#endif  /* REQUIRE_COPY_BYTES_12 */
#if REQUIRE_COPY_BYTES_16
COPY_CONTIGUOUS_BYTES( bytes, 16 )
#endif  /* REQUIRE_COPY_BYTES_16 */
#if REQUIRE_COPY_BYTES_20
COPY_CONTIGUOUS_BYTES( bytes, 20 )
#endif  /* REQUIRE_COPY_BYTES_20 */

/* table of predefined copy functions - one for each MPI type */
conversion_fct_t ompi_ddt_copy_functions[DT_MAX_PREDEFINED] = {
   (conversion_fct_t)NULL,                      /* DT_LOOP                */
   (conversion_fct_t)NULL,                      /* DT_END_LOOP            */
   (conversion_fct_t)NULL,                      /* DT_LB                  */
   (conversion_fct_t)NULL,                      /* DT_UB                  */
   (conversion_fct_t)copy_char,                 /* DT_CHAR                */
   (conversion_fct_t)copy_char,                 /* DT_CHARACTER           */
   (conversion_fct_t)copy_char,                 /* DT_UNSIGNED_CHAR       */
   (conversion_fct_t)copy_char,                 /* DT_SIGNED_CHAR       */
   (conversion_fct_t)copy_char,                 /* DT_BYTE                */
   (conversion_fct_t)copy_short,                /* DT_SHORT               */
   (conversion_fct_t)copy_short,                /* DT_UNSIGNED_SHORT      */
   (conversion_fct_t)copy_int,                  /* DT_INT                 */
   (conversion_fct_t)copy_int,                  /* DT_UNSIGNED_INT        */
   (conversion_fct_t)copy_long,                 /* DT_LONG                */
   (conversion_fct_t)copy_long,                 /* DT_UNSIGNED_LONG       */
   (conversion_fct_t)copy_long_long,            /* DT_LONG_LONG_INT       */
   (conversion_fct_t)copy_long_long,            /* DT_UNSIGNED_LONG_LONG  */
   (conversion_fct_t)copy_float,                /* DT_FLOAT               */
   (conversion_fct_t)copy_double,               /* DT_DOUBLE              */
   (conversion_fct_t)copy_long_double,          /* DT_LONG_DOUBLE         */
   (conversion_fct_t)NULL,                      /* DT_PACKED              */
   (conversion_fct_t)copy_wchar,                /* DT_WCHAR               */
#if SIZEOF_BOOL == 1
   (conversion_fct_t)copy_bytes_1,              /* DT_CXX_BOOL            */
#elif SIZEOF_BOOL == 4
   (conversion_fct_t)copy_bytes_4,              /* DT_CXX_BOOL            */
#elif SIZEOF_BOOL == 8
   (conversion_fct_t)copy_bytes_8,              /* DT_CXX_BOOL            */
#else
#error Complete me please
#endif
#if OMPI_SIZEOF_FORTRAN_LOGICAL == 1
   (conversion_fct_t)copy_bytes_1,              /* DT_LOGIC               */
#elif OMPI_SIZEOF_FORTRAN_LOGICAL == 4
   (conversion_fct_t)copy_bytes_4,              /* DT_LOGIC               */
#elif 1 /* always, some compiler complain if there is not value */
   NULL,                                        /* DT_LOGIC               */
#endif
   (conversion_fct_t)copy_int,                  /* DT_INTEGER             */
   (conversion_fct_t)copy_float,                /* DT_REAL                */
   (conversion_fct_t)copy_double,               /* DT_DBLPREC             */
   (conversion_fct_t)copy_complex_float,        /* DT_COMPLEX_FLOAT       */
   (conversion_fct_t)copy_complex_double,       /* DT_COMPLEX_DOUBLE      */
   (conversion_fct_t)copy_complex_long_double,  /* DT_COMPLEX_LONG_DOUBLE */
   (conversion_fct_t)copy_2int,                 /* DT_2INT                */
   (conversion_fct_t)copy_2int,                 /* DT_2INTEGER            */
   (conversion_fct_t)copy_2float,               /* DT_2REAL               */
   (conversion_fct_t)copy_2double,              /* DT_2DBLPREC            */
   (conversion_fct_t)copy_2complex_float,       /* DT_2COMPLEX            */
   (conversion_fct_t)copy_2complex_double,      /* DT_2DOUBLE_COMPLEX     */
#if (SIZEOF_FLOAT + SIZEOF_INT) == 8
   (conversion_fct_t)copy_bytes_8,              /* DT_FLOAT_INT           */
#else
#error Complete me please
#endif
#if (SIZEOF_DOUBLE + SIZEOF_INT) == 12
   (conversion_fct_t)copy_bytes_12,             /* DT_DOUBLE_INT          */
#else
#error Complete me please
#endif
#if (SIZEOF_LONG_DOUBLE + SIZEOF_INT) == 12
   (conversion_fct_t)copy_bytes_12,             /* DT_LONG_DOUBLE_INT     */
#elif (SIZEOF_LONG_DOUBLE + SIZEOF_INT) == 16
   (conversion_fct_t)copy_bytes_16,             /* DT_LONG_DOUBLE_INT     */
#elif (SIZEOF_LONG_DOUBLE + SIZEOF_INT) == 20
   (conversion_fct_t)copy_bytes_20,             /* DT_LONG_DOUBLE_INT     */
#else
#error Complete me please
#endif
#if (SIZEOF_LONG + SIZEOF_INT) == 8
   (conversion_fct_t)copy_bytes_8,              /* DT_LONG_INT            */
#elif (SIZEOF_LONG + SIZEOF_INT) == 12
   (conversion_fct_t)copy_bytes_12,             /* DT_LONG_INT            */
#else
#error Complete me please
#endif
   (conversion_fct_t)NULL,                      /* DT_SHORT_INT           */
   (conversion_fct_t)NULL,                      /* DT_UNAVAILABLE         */
};
