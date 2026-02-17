/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stddef.h>

#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_convertor_internal.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_datatype_checksum.h"
#include "opal/datatype/opal_datatype_constructors.h"

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
#define COPY_TYPE(TYPENAME, TYPE, COUNT)                                                        \
    static int copy_##TYPENAME(opal_convertor_t *pConvertor, size_t count, char *from,          \
                               size_t from_len, ptrdiff_t from_extent, char *to, size_t to_len, \
                               ptrdiff_t to_extent, ptrdiff_t *advance)                         \
    {                                                                                           \
        size_t remote_TYPE_size = sizeof(TYPE) * (COUNT); /* TODO */                            \
        size_t local_TYPE_size = (COUNT) * sizeof(TYPE);                                        \
                                                                                                \
        /* make sure the remote buffer is large enough to hold the data */                      \
        if ((remote_TYPE_size * count) > from_len) {                                            \
            count = from_len / remote_TYPE_size;                                                \
            if ((count * remote_TYPE_size) != from_len) {                                       \
                DUMP("oops should I keep this data somewhere (excedent %d bytes)?\n",           \
                     from_len - (count * remote_TYPE_size));                                    \
            }                                                                                   \
            DUMP("correct: copy %s count %d from buffer %p with length %d to %p space %d\n",    \
                 #TYPE, count, from, from_len, to, to_len);                                     \
        } else                                                                                  \
            DUMP("         copy %s count %d from buffer %p with length %d to %p space %d\n",    \
                 #TYPE, count, from, from_len, to, to_len);                                     \
                                                                                                \
        if ((from_extent == (ptrdiff_t) local_TYPE_size)                                        \
            && (to_extent == (ptrdiff_t) remote_TYPE_size)) {                                   \
            /* copy of contiguous data at both source and destination */                         \
            MEMCPY(to, from, count *local_TYPE_size);                                           \
        } else {                                                                                \
            /* source or destination are non-contiguous */                                       \
            for (size_t i = 0; i < count; i++) {                                                \
                MEMCPY(to, from, local_TYPE_size);                                              \
                to += to_extent;                                                                \
                from += from_extent;                                                            \
            }                                                                                   \
        }                                                                                       \
        *advance = count * from_extent;                                                         \
        return count;                                                                           \
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
#define COPY_CONTIGUOUS_BYTES(TYPENAME, COUNT)                                                  \
    static size_t copy_##TYPENAME##_##COUNT(opal_convertor_t *pConvertor, size_t count,         \
                                            char *from, size_t from_len, ptrdiff_t from_extent, \
                                            char *to, size_t to_len, ptrdiff_t to_extent,       \
                                            ptrdiff_t *advance)                                 \
    {                                                                                           \
        size_t remote_TYPE_size = (size_t)(COUNT); /* TODO */                                   \
        size_t local_TYPE_size = (size_t)(COUNT);                                               \
                                                                                                \
        if ((remote_TYPE_size * count) > from_len) {                                            \
            count = from_len / remote_TYPE_size;                                                \
            if ((count * remote_TYPE_size) != from_len) {                                       \
                DUMP("oops should I keep this data somewhere (excedent %d bytes)?\n",           \
                     from_len - (count * remote_TYPE_size));                                    \
            }                                                                                   \
            DUMP("correct: copy %s count %d from buffer %p with length %d to %p space %d\n",    \
                 #TYPENAME, count, from, from_len, to, to_len);                                 \
        } else                                                                                  \
            DUMP("         copy %s count %d from buffer %p with length %d to %p space %d\n",    \
                 #TYPENAME, count, from, from_len, to, to_len);                                 \
                                                                                                \
        if ((from_extent == (ptrdiff_t) local_TYPE_size)                                        \
            && (to_extent == (ptrdiff_t) remote_TYPE_size)) {                                   \
            MEMCPY(to, from, count *local_TYPE_size);                                           \
        } else {                                                                                \
            for (size_t i = 0; i < count; i++) {                                                \
                MEMCPY(to, from, local_TYPE_size);                                              \
                to += to_extent;                                                                \
                from += from_extent;                                                            \
            }                                                                                   \
        }                                                                                       \
        *advance = count * from_extent;                                                         \
        return count;                                                                           \
    }

/* set up copy functions for the basic C MPI data types */
/* per default, select all of them */
#define REQUIRE_COPY_BYTES_1  1
#define REQUIRE_COPY_BYTES_2  1
#define REQUIRE_COPY_BYTES_4  1
#define REQUIRE_COPY_BYTES_8  1
#define REQUIRE_COPY_BYTES_16 1

#if REQUIRE_COPY_BYTES_1
COPY_CONTIGUOUS_BYTES(bytes, 1)
#endif
#if REQUIRE_COPY_BYTES_2
COPY_CONTIGUOUS_BYTES(bytes, 2)
#endif
#if REQUIRE_COPY_BYTES_4
COPY_CONTIGUOUS_BYTES(bytes, 4)
#endif
#if REQUIRE_COPY_BYTES_8
COPY_CONTIGUOUS_BYTES(bytes, 8)
#endif
#if REQUIRE_COPY_BYTES_16
COPY_CONTIGUOUS_BYTES(bytes, 16)
#endif

#if defined(HAVE_SHORT_FLOAT) && SIZEOF_SHORT_FLOAT == 2
COPY_TYPE(float_2, short float, 1)
#elif SIZEOF_FLOAT == 2
COPY_TYPE(float_2, float, 1)
#elif SIZEOF_DOUBLE == 2
COPY_TYPE(float_2, double, 1)
#elif SIZEOF_LONG_DOUBLE == 2
COPY_TYPE(float_2, long double, 1)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T) && SIZEOF_OPAL_SHORT_FLOAT_T == 2
COPY_TYPE(float_2, opal_short_float_t, 1)
#else
/* #error No basic type for copy function for opal_datatype_float2 found */
#    define copy_float_2 NULL
#endif

#if defined(HAVE_SHORT_FLOAT) && SIZEOF_SHORT_FLOAT == 4
COPY_TYPE(float_4, short float, 1)
#elif SIZEOF_FLOAT == 4
COPY_TYPE(float_4, float, 1)
#elif SIZEOF_DOUBLE == 4
COPY_TYPE(float_4, double, 1)
#elif SIZEOF_LONG_DOUBLE == 4
COPY_TYPE(float_4, long double, 1)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T) && SIZEOF_OPAL_SHORT_FLOAT_T == 4
COPY_TYPE(float_4, opal_short_float_t, 1)
#else
#    error No basic type for copy function for opal_datatype_float4 found
#endif

#if defined(HAVE_SHORT_FLOAT) && SIZEOF_SHORT_FLOAT == 8
COPY_TYPE(float_8, short float, 1)
#elif SIZEOF_FLOAT == 8
COPY_TYPE(float_8, float, 1)
#elif SIZEOF_DOUBLE == 8
COPY_TYPE(float_8, double, 1)
#elif SIZEOF_LONG_DOUBLE == 8
COPY_TYPE(float_8, long double, 1)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T) && SIZEOF_OPAL_SHORT_FLOAT_T == 8
COPY_TYPE(float_8, opal_short_float_t, 1)
#else
#    error No basic type for copy function for opal_datatype_float8 found
#endif

#if SIZEOF_LONG_DOUBLE == OPAL_SIZEOF_FLOAT12
COPY_TYPE(float_12, long double, 1)
#elif SIZEOF_DOUBLE == OPAL_SIZEOF_FLOAT12
COPY_TYPE(float_12, double, 1)
#elif SIZEOF_FLOAT == OPAL_SIZEOF_FLOAT12
COPY_TYPE(float_12, float, 1)
#elif defined(HAVE_SHORT_FLOAT) && SIZEOF_SHORT_FLOAT == OPAL_SIZEOF_FLOAT12
COPY_TYPE(float_12, short float, 1)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T) && SIZEOF_OPAL_SHORT_FLOAT_T == 12
COPY_TYPE(float_12, opal_short_float_t, 1)
#else
/* #error No basic type for copy function for opal_datatype_float12 found */
#    define copy_float_12 NULL
#endif

#if defined(HAVE__FLOAT128) && SIZEOF__FLOAT128 == 16
COPY_TYPE(float_16, _Float128, 1)
#elif defined(HAVE___FLOAT128) && SIZEOF___FLOAT128 == 16
COPY_TYPE(float_16, __float128, 1)
#elif defined(HAVE_SHORT_FLOAT) && SIZEOF_SHORT_FLOAT == 16
COPY_TYPE(float_16, short float, 1)
#elif SIZEOF_FLOAT == 16
COPY_TYPE(float_16, float, 1)
#elif SIZEOF_DOUBLE == 16
COPY_TYPE(float_16, double, 1)
#elif SIZEOF_LONG_DOUBLE == 16
COPY_TYPE(float_16, long double, 1)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T) && SIZEOF_OPAL_SHORT_FLOAT_T == 16
COPY_TYPE(float_16, opal_short_float_t, 1)
#else
/* #error No basic type for copy function for opal_datatype_float16 found */
#    define copy_float_16 NULL
#endif

#if defined(HAVE_SHORT_FLOAT__COMPLEX)
COPY_TYPE(short_float_complex, short float _Complex, 1)
#elif defined(HAVE_OPAL_SHORT_FLOAT_COMPLEX_T)
COPY_TYPE(short_float_complex, opal_short_float_complex_t, 1)
#else
/* #error No basic type for copy function for opal_datatype_short_float_complex found */
#    define copy_short_float_complex NULL
#endif

COPY_TYPE(float_complex, float _Complex, 1)

COPY_TYPE(double_complex, double _Complex, 1)

COPY_TYPE(long_double_complex, long double _Complex, 1)

#if defined(HAVE__FLOAT128) && defined(HAVE__FLOAT128__COMPLEX)
COPY_TYPE(float128_complex, _Float128 _Complex, 1)
#elif defined(HAVE___FLOAT128) && defined(HAVE___FLOAT128__COMPLEX)
COPY_TYPE(float128_complex, __float128 _Complex, 1)
#else
/* #error No _Float128 _Complex support available */
#    define copy_float128_complex NULL
#endif

#if SIZEOF__BOOL == SIZEOF_CHAR
COPY_TYPE(bool, char, 1)
#elif SIZEOF__BOOL == SIZEOF_SHORT
COPY_TYPE(bool, short, 1)
#elif SIZEOF__BOOL == SIZEOF_INT
COPY_TYPE(bool, int, 1)
#elif SIZEOF__BOOL == SIZEOF_LONG
COPY_TYPE(bool, long, 1)
#else
#    error No basic type for copy function for opal_datatype_bool found
#endif

COPY_TYPE(wchar, wchar_t, 1)

/* Table of predefined copy functions - one for each OPAL type */
/* NOTE: The order of this array *MUST* match the order in opal_datatype_basicDatatypes */
conversion_fct_t opal_datatype_copy_functions[OPAL_DATATYPE_MAX_PREDEFINED] = {
    [OPAL_DATATYPE_LOOP]                =     (conversion_fct_t) NULL,
    [OPAL_DATATYPE_END_LOOP]            =     (conversion_fct_t) NULL,
    [OPAL_DATATYPE_LB]                  =     (conversion_fct_t) NULL,
    [OPAL_DATATYPE_UB]                  =     (conversion_fct_t) NULL,
    [OPAL_DATATYPE_INT1]                =     (conversion_fct_t) copy_bytes_1,
    [OPAL_DATATYPE_INT2]                =     (conversion_fct_t) copy_bytes_2,
    [OPAL_DATATYPE_INT4]                =     (conversion_fct_t) copy_bytes_4,
    [OPAL_DATATYPE_INT8]                =     (conversion_fct_t) copy_bytes_8,
    [OPAL_DATATYPE_INT16]               =     (conversion_fct_t) copy_bytes_16,
    [OPAL_DATATYPE_UINT1]               =     (conversion_fct_t) copy_bytes_1,
    [OPAL_DATATYPE_UINT2]               =     (conversion_fct_t) copy_bytes_2,
    [OPAL_DATATYPE_UINT4]               =     (conversion_fct_t) copy_bytes_4,
    [OPAL_DATATYPE_UINT8]               =     (conversion_fct_t) copy_bytes_8,
    [OPAL_DATATYPE_UINT16]              =     (conversion_fct_t) copy_bytes_16,
    [OPAL_DATATYPE_FLOAT2]              =     (conversion_fct_t) copy_float_2,
    [OPAL_DATATYPE_FLOAT4]              =     (conversion_fct_t) copy_float_4,
    [OPAL_DATATYPE_FLOAT8]              =     (conversion_fct_t) copy_float_8,
    [OPAL_DATATYPE_FLOAT12]             =     (conversion_fct_t) copy_float_12,
    [OPAL_DATATYPE_FLOAT16]             =     (conversion_fct_t) copy_float_16,
    [OPAL_DATATYPE_SHORT_FLOAT_COMPLEX] =     (conversion_fct_t) copy_short_float_complex,
    [OPAL_DATATYPE_FLOAT_COMPLEX]       =     (conversion_fct_t) copy_float_complex,
    [OPAL_DATATYPE_DOUBLE_COMPLEX]      =     (conversion_fct_t) copy_double_complex,
    [OPAL_DATATYPE_LONG_DOUBLE_COMPLEX] =     (conversion_fct_t) copy_long_double_complex,
    [OPAL_DATATYPE_BOOL]                =     (conversion_fct_t) copy_bool,
    [OPAL_DATATYPE_WCHAR]               =     (conversion_fct_t) copy_wchar,
#if SIZEOF_LONG == 4
    [OPAL_DATATYPE_LONG]                =     (conversion_fct_t)copy_bytes_4,
    [OPAL_DATATYPE_UNSIGNED_LONG]       =     (conversion_fct_t)copy_bytes_4,
#elif SIZEOF_LONG == 8
    [OPAL_DATATYPE_LONG]                =     (conversion_fct_t)copy_bytes_8,
    [OPAL_DATATYPE_UNSIGNED_LONG]       =     (conversion_fct_t)copy_bytes_8,
#endif
    [OPAL_DATATYPE_FLOAT128_COMPLEX]    =     (conversion_fct_t) copy_float128_complex,
    [OPAL_DATATYPE_UNAVAILABLE]         =     NULL,
};
