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

#include "opal/util/output.h"

#include "opal/types.h"
#include "ompi/datatype/dt_arch.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/convertor.h"
#include "ompi/datatype/datatype_internal.h"
#include "ompi/datatype/datatype_checksum.h"
#include "ompi/datatype/convertor_internal.h"

static inline void
ompi_dt_swap_bytes(void *to_p, const void *from_p, const size_t size)
{
    size_t i, back_i;
    uint8_t *to = (uint8_t*) to_p, *from = (uint8_t*) from_p;
	back_i = size - 1;
    for (i = 0 ; i < size ; i++, back_i--) {
        to[back_i] = from[i];
    }
}


#define COPY_TYPE_HETEROGENEOUS( TYPENAME, TYPE )                                         \
static int32_t                                                                            \
copy_##TYPENAME##_heterogeneous(ompi_convertor_t *pConvertor, uint32_t count,             \
                                const char* from, size_t from_len, ptrdiff_t from_extent, \
                                char* to, size_t to_length, ptrdiff_t to_extent,          \
                                ptrdiff_t *advance)                     \
{                                                                       \
    uint32_t i;                                                         \
                                                                        \
    datatype_check( #TYPE, sizeof(TYPE), sizeof(TYPE), &count,          \
                   from, from_len, from_extent,                         \
                   to, to_length, to_extent);                           \
                                                                        \
    if ((pConvertor->remoteArch & OMPI_ARCH_ISBIGENDIAN) !=             \
        (ompi_mpi_local_arch & OMPI_ARCH_ISBIGENDIAN)) {                \
        for( i = 0; i < count; i++ ) {                                  \
            ompi_dt_swap_bytes(to, from, sizeof(TYPE));                 \
            to += to_extent;                                            \
            from += from_extent;                                        \
        }                                                               \
    } else if ((ptrdiff_t)sizeof(TYPE) == to_extent &&                  \
               (ptrdiff_t)sizeof(TYPE) == from_extent) {                \
         MEMCPY( to, from, count * sizeof(TYPE) );                      \
    } else {                                                            \
         /* source or destination are non-contigous */                  \
         for( i = 0; i < count; i++ ) {                                 \
             MEMCPY( to, from, sizeof(TYPE) );                          \
             to += to_extent;                                           \
             from += from_extent;                                       \
         }                                                              \
    }                                                                   \
    *advance = count * from_extent;                                     \
    return count;                                                       \
}


#define COPY_2TYPE_HETEROGENEOUS( TYPENAME, TYPE1, TYPE2 )              \
static int32_t                                                          \
copy_##TYPENAME##_heterogeneous(ompi_convertor_t *pConvertor, uint32_t count, \
                                const char* from, uint32_t from_len, ptrdiff_t from_extent, \
                                char* to, uint32_t to_length, ptrdiff_t to_extent, \
                                uint32_t *advance)                      \
{                                                                       \
    uint32_t i;                                                         \
                                                                        \
    datatype_check( #TYPENAME, sizeof(TYPE1) + sizeof(TYPE2),           \
                   sizeof(TYPE1) + sizeof(TYPE2), &count,               \
                   from, from_len, from_extent,                         \
                   to, to_length, to_extent);                           \
                                                                        \
    if ((pConvertor->remoteArch & OMPI_ARCH_ISBIGENDIAN) !=             \
        (ompi_mpi_local_arch & OMPI_ARCH_ISBIGENDIAN)) {                \
        /* source and destination are different endianness */           \
        for( i = 0; i < count; i++ ) {                                  \
            TYPE1* to_1, *from_1;                                       \
            TYPE2* to_2, *from_2;                                       \
            to_1 = (TYPE1*) to; from_1 = (TYPE1*) from;                 \
            ompi_dt_swap_bytes(to_1, from_1, sizeof(TYPE1));            \
            to_2 = (TYPE2*) (to_1 + 1); from_2 = (TYPE2*) (from_1 + 1); \
            ompi_dt_swap_bytes(to_2, from_2, sizeof(TYPE2));            \
            to += to_extent;                                            \
            from += from_extent;                                        \
        }                                                               \
    } else if ((ptrdiff_t)(sizeof(TYPE1) + sizeof(TYPE2)) == to_extent &&   \
               (ptrdiff_t)(sizeof(TYPE1) + sizeof(TYPE2)) == from_extent) { \
        /* source and destination are contigous */                      \
        MEMCPY( to, from, count * (sizeof(TYPE1) + sizeof(TYPE2)) );    \
    } else {                                                            \
        /* source or destination are non-contigous */                   \
        for( i = 0; i < count; i++ ) {                                  \
            MEMCPY( to, from, sizeof(TYPE1) + sizeof(TYPE2) );          \
            to += to_extent;                                            \
            from += from_extent;                                        \
        }                                                               \
    }                                                                   \
    *advance = count * from_extent;                                     \
    return count;                                                       \
}


#define COPY_COMPLEX_HETEROGENEOUS( TYPENAME, TYPE )                    \
    COPY_2TYPE_HETEROGENEOUS(complex_##TYPENAME, TYPE, TYPE)


#define COPY_2COMPLEX_HETEROGENEOUS( TYPENAME, TYPE )                   \
static int32_t                                                          \
copy_2complex_##TYPENAME##_heterogeneous(ompi_convertor_t *pConvertor, uint32_t count, \
                                         const char* from, uint32_t from_len, ptrdiff_t from_extent, \
                                         char* to, uint32_t to_length, ptrdiff_t to_extent, \
                                         uint32_t *advance)             \
{                                                                       \
    uint32_t i;                                                         \
                                                                        \
    datatype_check( #TYPENAME, sizeof(TYPE) * 2, sizeof(TYPE) * 2, &count, \
                   from, from_len, from_extent,                         \
                   to, to_length, to_extent);                           \
                                                                        \
    if ((pConvertor->remoteArch & OMPI_ARCH_ISBIGENDIAN) !=             \
        (ompi_mpi_local_arch & OMPI_ARCH_ISBIGENDIAN)) {                \
        /* source and destination are different endianness */           \
        for( i = 0; i < count; i++ ) {                                  \
            TYPE *to_p = (TYPE*) to, *from_p = (TYPE*) from;            \
            ompi_dt_swap_bytes(&(to_p->r), &(from_p->r), sizeof(to_p->r)); \
            ompi_dt_swap_bytes(&(to_p->i), &(from_p->i), sizeof(to_p->i)); \
            to_p++; from_p++;                                           \
            ompi_dt_swap_bytes(&(to_p->r), &(from_p->r), sizeof(to_p->r)); \
            ompi_dt_swap_bytes(&(to_p->i), &(from_p->i), sizeof(to_p->i)); \
            to += to_extent;                                            \
            from += from_extent;                                        \
        }                                                               \
    } else if (sizeof(TYPE) * 2 == to_extent &&                         \
               sizeof(TYPE) * 2 == from_extent) {                       \
        /* source and destination are contigous */                      \
        MEMCPY( to, from, count * (sizeof(TYPE) * 2) );                 \
    } else {                                                            \
        /* source or destination are non-contigous */                   \
        for( i = 0; i < count; i++ ) {                                  \
            MEMCPY( to, from, sizeof(TYPE) * 2 );                       \
            to += to_extent;                                            \
            from += from_extent;                                        \
        }                                                               \
    }                                                                   \
    *advance = count * from_extent;                                     \
    return count;                                                       \
}


static inline void
datatype_check(char *type, size_t local_size, size_t remote_size, uint32_t *count, 
               const char* from, size_t from_len, ptrdiff_t from_extent,
               char* to, size_t to_len, ptrdiff_t to_extent)
{
    /* make sure the remote buffer is large enough to hold the data */  
    if( (remote_size * *count) > from_len ) {                       
        *count = (uint32_t)(from_len / remote_size);
        if( (*count * remote_size) != from_len ) {                  
            DUMP( "oops should I keep this data somewhere (excedent %d bytes)?\n", 
                  from_len - (*count * remote_size) );              
        }                                                               
        DUMP( "correct: copy %s count %d from buffer %p with length %d to %p space %d\n", 
              "char", *count, from, from_len, to, to_len );               
    } else {
        DUMP( "         copy %s count %d from buffer %p with length %d to %p space %d\n", 
              "char", *count, from, from_len, to, to_len );               
    }    
}


/* char has no endian issues, so don't really worry about it */
static int32_t 
copy_char_heterogeneous(ompi_convertor_t *pConvertor, uint32_t count,
                        const char* from, uint32_t from_len, ptrdiff_t from_extent,
                        char* to, uint32_t to_length, ptrdiff_t to_extent,
                        uint32_t *advance)
{
    uint32_t i;

    datatype_check("char", sizeof(char), sizeof(char), &count, 
                   from, from_len, from_extent,
                   to, to_length, to_extent);

    if( (from_extent == sizeof(char)) &&                       
        (to_extent == sizeof(char)) ) {                       
        /* copy of contigous data at both source and destination */     
        MEMCPY( to, from, count * sizeof(char) );                    
    } else {                                                            
        /* source or destination are non-contigous */                   
        for( i = 0; i < count; i++ ) {                                  
            MEMCPY( to, from, sizeof(char) );                        
            to += to_extent;                                            
            from += from_extent;                                        
        }                                                               
    }                                                                   
    *advance = count * from_extent;
    return count;                                                       
}


#define CXX_BOOL_COPY_LOOP(TYPE)                        \
    for( i = 0; i < count; i++ ) {                      \
        bool *to_real = (bool*) to;                     \
        *to_real = *((TYPE*) from) == 0 ? false : true; \
        to += to_extent;                                \
        from += from_extent;                            \
    }
static int32_t
copy_cxx_bool_heterogeneous(ompi_convertor_t *pConvertor, uint32_t count,
                            const char* from, uint32_t from_len, ptrdiff_t from_extent,
                            char* to, uint32_t to_length, ptrdiff_t to_extent,
                            uint32_t *advance)
{
    uint32_t i;

    /* fix up the from extent */
    if ((pConvertor->remoteArch & OMPI_ARCH_BOOLISxx) != 
        (ompi_mpi_local_arch & OMPI_ARCH_BOOLISxx)) {
        switch (pConvertor->remoteArch & OMPI_ARCH_BOOLISxx) {
        case OMPI_ARCH_BOOLIS8:
            from_extent = 1;
            break;
        case OMPI_ARCH_BOOLIS16:
            from_extent = 2;
            break;
        case OMPI_ARCH_BOOLIS32:
            from_extent = 4;
            break;
        }
    }

    datatype_check( "bool", sizeof(bool), sizeof(bool), &count,
                   from, from_len, from_extent,
                   to, to_length, to_extent);

    if ((to_extent != sizeof(bool) || from_extent != sizeof(bool)) ||
        ((pConvertor->remoteArch & OMPI_ARCH_BOOLISxx) != 
         (ompi_mpi_local_arch & OMPI_ARCH_BOOLISxx))) {
        switch (pConvertor->remoteArch & OMPI_ARCH_BOOLISxx) {
        case OMPI_ARCH_BOOLIS8:
            CXX_BOOL_COPY_LOOP(int8_t);
            break;
        case OMPI_ARCH_BOOLIS16:
            CXX_BOOL_COPY_LOOP(int16_t);
            break;
        case OMPI_ARCH_BOOLIS32:
            CXX_BOOL_COPY_LOOP(int32_t);
            break;
        }
    } else {
        MEMCPY( to, from, count * sizeof(bool) );                    
    }

    *advance = count * from_extent;
    return count;
}

#define FORTRAN_LOGICAL_COPY_LOOP(TYPE)                                \
    for( i = 0; i < count; i++ ) {                                     \
        ompi_fortran_logical_t *to_real = (ompi_fortran_logical_t*) to; \
        *to_real = *((TYPE*) from) == 0 ? 0 : OMPI_FORTRAN_VALUE_TRUE; \
        to += to_extent;                                               \
        from += from_extent;                                           \
    }
static int32_t
copy_fortran_logical_heterogeneous(ompi_convertor_t *pConvertor, uint32_t count,
                            const char* from, uint32_t from_len, ptrdiff_t from_extent,
                            char* to, uint32_t to_length, ptrdiff_t to_extent,
                            uint32_t *advance)
{
    uint32_t i;

    /* fix up the from extent */
    if ((pConvertor->remoteArch & OMPI_ARCH_LOGICALISxx) != 
        (ompi_mpi_local_arch & OMPI_ARCH_LOGICALISxx)) {
        switch (pConvertor->remoteArch & OMPI_ARCH_LOGICALISxx) {
        case OMPI_ARCH_LOGICALIS8:
            from_extent = 1;
            break;
        case OMPI_ARCH_LOGICALIS16:
            from_extent = 2;
            break;
        case OMPI_ARCH_LOGICALIS32:
            from_extent = 4;
            break;
        }
    }

    datatype_check( "logical", sizeof(ompi_fortran_logical_t), 
                    sizeof(ompi_fortran_logical_t), &count,
                    from, from_len, from_extent,
                    to, to_length, to_extent);

    if ((to_extent != sizeof(ompi_fortran_logical_t) || 
         from_extent != sizeof(ompi_fortran_logical_t)) ||
        ((pConvertor->remoteArch & OMPI_ARCH_LOGICALISxx) != 
         (ompi_mpi_local_arch & OMPI_ARCH_LOGICALISxx))) {
        switch (pConvertor->remoteArch & OMPI_ARCH_LOGICALISxx) {
        case OMPI_ARCH_LOGICALIS8:
            FORTRAN_LOGICAL_COPY_LOOP(int8_t);
            break;
        case OMPI_ARCH_LOGICALIS16:
            FORTRAN_LOGICAL_COPY_LOOP(int16_t);
            break;
        case OMPI_ARCH_LOGICALIS32:
            FORTRAN_LOGICAL_COPY_LOOP(int32_t);
            break;
        }
    } else {
        MEMCPY( to, from, count * sizeof(ompi_fortran_logical_t) );                    
    }

    *advance = count * from_extent;
    return count;
}


COPY_TYPE_HETEROGENEOUS(short, short)
COPY_TYPE_HETEROGENEOUS(int, int)
COPY_TYPE_HETEROGENEOUS(long, long)
COPY_TYPE_HETEROGENEOUS(long_long, long long)
COPY_TYPE_HETEROGENEOUS(float, float)
COPY_TYPE_HETEROGENEOUS(double, double)
COPY_TYPE_HETEROGENEOUS(long_double, long double)
COPY_COMPLEX_HETEROGENEOUS(float, float)
COPY_COMPLEX_HETEROGENEOUS(double, double)
COPY_COMPLEX_HETEROGENEOUS(long_double, long double)
COPY_2TYPE_HETEROGENEOUS(float_int, float, int)
COPY_2TYPE_HETEROGENEOUS(double_int, double, int)
COPY_2TYPE_HETEROGENEOUS(long_double_int, long double, int)
COPY_2TYPE_HETEROGENEOUS(long_int, long, int)
COPY_2TYPE_HETEROGENEOUS(2int, int, int)
COPY_2TYPE_HETEROGENEOUS(2float, float, float)
COPY_2TYPE_HETEROGENEOUS(2double, double, double)
COPY_TYPE_HETEROGENEOUS(wchar, wchar_t)
COPY_2COMPLEX_HETEROGENEOUS(float, ompi_complex_float_t)
COPY_2COMPLEX_HETEROGENEOUS(double, ompi_complex_double_t)


/* table of predefined copy functions - one for each MPI type */
conversion_fct_t ompi_ddt_heterogeneous_copy_functions[DT_MAX_PREDEFINED] = {
   NULL,                      /* DT_LOOP                */
   NULL,                      /* DT_END_LOOP            */
   NULL,                      /* DT_LB                  */
   NULL,                      /* DT_UB                  */
   (conversion_fct_t) copy_char_heterogeneous,               /* DT_CHAR                */
   (conversion_fct_t) copy_char_heterogeneous,               /* DT_CHARACTER           */
   (conversion_fct_t) copy_char_heterogeneous,               /* DT_UNSIGNED_CHAR       */
   (conversion_fct_t) copy_char_heterogeneous,               /* DT_SIGNED_CHAR         */
   (conversion_fct_t) copy_char_heterogeneous,               /* DT_BYTE                */
   (conversion_fct_t) copy_short_heterogeneous,              /* DT_SHORT               */
   (conversion_fct_t) copy_short_heterogeneous,              /* DT_UNSIGNED_SHORT      */
   (conversion_fct_t) copy_int_heterogeneous,                /* DT_INT                 */
   (conversion_fct_t) copy_int_heterogeneous,                /* DT_UNSIGNED_INT        */
   (conversion_fct_t) copy_long_heterogeneous,               /* DT_LONG                */
   (conversion_fct_t) copy_long_heterogeneous,               /* DT_UNSIGNED_LONG       */
   (conversion_fct_t) copy_long_long_heterogeneous,          /* DT_LONG_LONG_INT       */
   (conversion_fct_t) copy_long_long_heterogeneous,          /* DT_UNSIGNED_LONG_LONG  */
   (conversion_fct_t) copy_float_heterogeneous,              /* DT_FLOAT               */
   (conversion_fct_t) copy_double_heterogeneous,             /* DT_DOUBLE              */
   (conversion_fct_t) copy_long_double_heterogeneous,        /* DT_LONG_DOUBLE         */
   NULL,                       /* DT_PACKED              */
   (conversion_fct_t) copy_wchar_heterogeneous,              /* DT_WCHAR               */
   (conversion_fct_t) copy_cxx_bool_heterogeneous,           /* DT_CXX_BOOL            */
   (conversion_fct_t) copy_fortran_logical_heterogeneous,    /* DT_LOGIC               */
   (conversion_fct_t) copy_int_heterogeneous,                /* DT_INTEGER             */
   (conversion_fct_t) copy_float_heterogeneous,              /* DT_REAL                */
   (conversion_fct_t) copy_double_heterogeneous,             /* DT_DBLPREC             */
   (conversion_fct_t) copy_complex_float_heterogeneous,      /* DT_COMPLEX_FLOAT       */
   (conversion_fct_t) copy_complex_double_heterogeneous,     /* DT_COMPLEX_DOUBLE      */
   (conversion_fct_t) copy_complex_long_double_heterogeneous,/* DT_COMPLEX_LONG_DOUBLE */
   (conversion_fct_t) copy_2int_heterogeneous,               /* DT_2INT                */
   (conversion_fct_t) copy_2int_heterogeneous,               /* DT_2INTEGER            */
   (conversion_fct_t) copy_2float_heterogeneous,             /* DT_2REAL               */
   (conversion_fct_t) copy_2double_heterogeneous,            /* DT_2DBLPREC            */
   (conversion_fct_t) copy_2complex_float_heterogeneous,     /* DT_2COMPLEX            */
   (conversion_fct_t) copy_2complex_double_heterogeneous,    /* DT_2DOUBLE_COMPLEX     */
   (conversion_fct_t) copy_float_int_heterogeneous,          /* DT_FLOAT_INT           */
   (conversion_fct_t) copy_double_int_heterogeneous,         /* DT_DOUBLE_INT          */
   (conversion_fct_t) copy_long_double_int_heterogeneous,    /* DT_LONG_DOUBLE_INT     */
   (conversion_fct_t) copy_long_int_heterogeneous,           /* DT_LONG_INT            */
   NULL,                       /* DT_SHORT_INT           */
   NULL,                       /* DT_UNAVAILABLE         */
};
