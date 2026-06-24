/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */

/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2019 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2021      IBM Corporation. All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_DATATYPE_CONSTRUCTORS_H_HAS_BEEN_INCLUDED
#define OPAL_DATATYPE_CONSTRUCTORS_H_HAS_BEEN_INCLUDED

#include "opal/datatype/opal_datatype_internal.h"

#define OPAL_DATATYPE_INIT_NAME(NAME) "OPAL_" #NAME
#define OPAL_DATATYPE_INIT_NAME_WITH_TYPE(NAME, TYPE) "OPAL_" #NAME "\0:" #TYPE

/*
 * Macro to initialize the main description for basic types, setting the pointer
 * into the array opal_datatype_predefined_type_desc, which is initialized at
 * runtime in opal_datatype_init(). Each basic type has two desc-elements....
 */
#define OPAL_DATATYPE_INIT_DESC_PREDEFINED(NAME)                                \
    {                                                                           \
        .length = 1, .used = 1,                                                 \
        .desc = &(opal_datatype_predefined_elem_desc[2 * OPAL_DATATYPE_##NAME]) \
    }
#define OPAL_DATATYPE_INIT_DESC_NULL         \
    {                                        \
        .length = 0, .used = 0, .desc = NULL \
    }

#define OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED(NAME, FLAGS)                                   \
    {                                                                                              \
        .super = OPAL_OBJ_STATIC_INIT(opal_datatype_t),                                            \
        .flags = OPAL_DATATYPE_FLAG_UNAVAILABLE | OPAL_DATATYPE_FLAG_PREDEFINED | (FLAGS),         \
        .id = OPAL_DATATYPE_##NAME, .bdt_used = 0, .size = 0, .true_lb = 0, .true_ub = 0, .lb = 0, \
        .ub = 0, .align = 0, .nbElems = 1, .name = OPAL_DATATYPE_INIT_NAME(NAME),                  \
        .desc = OPAL_DATATYPE_INIT_DESC_PREDEFINED(UNAVAILABLE),                                   \
        .opt_desc = OPAL_DATATYPE_INIT_DESC_PREDEFINED(UNAVAILABLE),                               \
        .ptypes = OPAL_DATATYPE_INIT_PTYPES_ARRAY_UNAVAILABLE                                      \
    }

#define OPAL_DATATYPE_INITIALIZER_UNAVAILABLE(FLAGS) \
    OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED(UNAVAILABLE, (FLAGS))

#define OPAL_DATATYPE_INITIALIZER_EMPTY(FLAGS)                                               \
    {                                                                                        \
        .super = OPAL_OBJ_STATIC_INIT(opal_datatype_t),                                      \
        .flags = OPAL_DATATYPE_FLAG_PREDEFINED | (FLAGS), .id = 0, .bdt_used = 0, .size = 0, \
        .true_lb = 0, .true_ub = 0, .lb = 0, .ub = 0, .align = 0, .nbElems = 1,              \
        .name = OPAL_DATATYPE_INIT_NAME(EMPTY), .desc = OPAL_DATATYPE_INIT_DESC_NULL,        \
        .opt_desc = OPAL_DATATYPE_INIT_DESC_NULL,                                            \
        .ptypes = OPAL_DATATYPE_INIT_PTYPES_ARRAY_UNAVAILABLE                                \
    }

#define OPAL_DATATYPE_INIT_BASIC_TYPE(TYPE, NAME, FLAGS)                                        \
    {                                                                                           \
        .super = OPAL_OBJ_STATIC_INIT(opal_datatype_t),                                         \
        .flags = OPAL_DATATYPE_FLAG_PREDEFINED | (FLAGS), .id = TYPE,                           \
        .bdt_used = (((uint32_t) 1) << (TYPE)), .size = 0, .true_lb = 0, .true_ub = 0, .lb = 0, \
        .ub = 0, .align = 0, .nbElems = 1, .name = OPAL_DATATYPE_INIT_NAME_WITH_TYPE(NAME, TYPE), \
        .desc = OPAL_DATATYPE_INIT_DESC_NULL, .opt_desc = OPAL_DATATYPE_INIT_DESC_NULL,         \
        .ptypes = OPAL_DATATYPE_INIT_PTYPES_ARRAY_UNAVAILABLE                                   \
    }

#define OPAL_DATATYPE_INIT_BASIC_DATATYPE(TYPE, ALIGN, NAME, FLAGS)                           \
    {                                                                                         \
        .super = OPAL_OBJ_STATIC_INIT(opal_datatype_t),                                       \
        .flags = OPAL_DATATYPE_FLAG_BASIC | (FLAGS), .id = OPAL_DATATYPE_##NAME,              \
        .bdt_used = (((uint32_t) 1) << (OPAL_DATATYPE_##NAME)), .size = sizeof(TYPE),         \
        .true_lb = 0, .true_ub = sizeof(TYPE), .lb = 0, .ub = sizeof(TYPE), .align = (ALIGN), \
        .nbElems = 1, .name = OPAL_DATATYPE_INIT_NAME_WITH_TYPE(NAME, TYPE),                  \
        .desc = OPAL_DATATYPE_INIT_DESC_PREDEFINED(NAME),                                     \
        .opt_desc = OPAL_DATATYPE_INIT_DESC_PREDEFINED(NAME),                                 \
        .ptypes = OPAL_DATATYPE_INIT_PTYPES_ARRAY_UNAVAILABLE                                 \
    }

#define OPAL_DATATYPE_INITIALIZER_INT1(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_INT1(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                              OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_INT2(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_INT2(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                              OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_INT4(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_INT4(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                              OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_INT8(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_INT8(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                              OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_INT16(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_INT16(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                               OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_UINT1(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_UINT1(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                               OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_UINT2(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_UINT2(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                               OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_UINT4(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_UINT4(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                               OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_UINT8(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_UINT8(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                               OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_UINT16(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_UINT16(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                                OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_FLOAT2(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_FLOAT2(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                                OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_FLOAT4(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_FLOAT4(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                                OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_FLOAT8(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_FLOAT8(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                                OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_FLOAT12(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_FLOAT12(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                                 OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_FLOAT16(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_FLOAT16(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                                 OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_SHORT_FLOAT_COMPLEX(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_SHORT_FLOAT_COMPLEX(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                                             OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_FLOAT_COMPLEX(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_FLOAT_COMPLEX(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                                       OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_DOUBLE_COMPLEX(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_DOUBLE_COMPLEX(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                                        OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_LONG_DOUBLE_COMPLEX(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_LONG_DOUBLE_COMPLEX(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                                             OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_FLOAT128_COMPLEX(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_FLOAT128_COMPLEX(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                                          OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_BOOL(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_BOOL(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                              OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_WCHAR(FLAGS)                    \
    OPAL_DATATYPE_HANDLE_WCHAR(OPAL_DATATYPE_INIT_BASIC_DATATYPE, \
                               OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_LOOP(FLAGS) \
    OPAL_DATATYPE_INIT_BASIC_TYPE(OPAL_DATATYPE_LOOP, LOOP_S, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_END_LOOP(FLAGS) \
    OPAL_DATATYPE_INIT_BASIC_TYPE(OPAL_DATATYPE_END_LOOP, LOOP_E, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_LB(FLAGS) \
    OPAL_DATATYPE_INIT_BASIC_TYPE(OPAL_DATATYPE_LB, LB, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_UB(FLAGS) \
    OPAL_DATATYPE_INIT_BASIC_TYPE(OPAL_DATATYPE_UB, UB, FLAGS)

#define OPAL_DATATYPE_HANDLE_INT1(AV, NOTAV, FLAGS) AV(int8_t, OPAL_ALIGNMENT_INT8, INT1, FLAGS)
#define OPAL_DATATYPE_HANDLE_INT2(AV, NOTAV, FLAGS) AV(int16_t, OPAL_ALIGNMENT_INT16, INT2, FLAGS)
#define OPAL_DATATYPE_HANDLE_INT4(AV, NOTAV, FLAGS) AV(int32_t, OPAL_ALIGNMENT_INT32, INT4, FLAGS)
#define OPAL_DATATYPE_HANDLE_INT8(AV, NOTAV, FLAGS) AV(int64_t, OPAL_ALIGNMENT_INT64, INT8, FLAGS)
#ifdef HAVE_INT128_T
#    define OPAL_DATATYPE_HANDLE_INT16(AV, NOTAV, FLAGS) \
        AV(int128_t, OPAL_ALIGNMENT_INT128, INT16, FLAGS)
#else
#    define OPAL_DATATYPE_HANDLE_INT16(AV, NOTAV, FLAGS) NOTAV(INT16, FLAGS)
#endif
#define OPAL_DATATYPE_HANDLE_UINT1(AV, NOTAV, FLAGS) AV(uint8_t, OPAL_ALIGNMENT_INT8, UINT1, FLAGS)
#define OPAL_DATATYPE_HANDLE_UINT2(AV, NOTAV, FLAGS) \
    AV(uint16_t, OPAL_ALIGNMENT_INT16, UINT2, FLAGS)
#define OPAL_DATATYPE_HANDLE_UINT4(AV, NOTAV, FLAGS) \
    AV(uint32_t, OPAL_ALIGNMENT_INT32, UINT4, FLAGS)
#define OPAL_DATATYPE_HANDLE_UINT8(AV, NOTAV, FLAGS) \
    AV(uint64_t, OPAL_ALIGNMENT_INT64, UINT8, FLAGS)
#ifdef HAVE_UINT128_T
#    define OPAL_DATATYPE_HANDLE_UINT16(AV, NOTAV, FLAGS) \
        AV(uint128_t, OPAL_ALIGNMENT_INT128, UINT16, FLAGS)
#else
#    define OPAL_DATATYPE_HANDLE_UINT16(AV, NOTAV, FLAGS) NOTAV(INT16, FLAGS)
#endif


#define OPAL_DATATYPE_INITIALIZER_LONG(FLAGS)  \
     OPAL_DATATYPE_INIT_BASIC_DATATYPE(long, OPAL_ALIGNMENT_LONG, LONG, FLAGS)
#define OPAL_DATATYPE_INITIALIZER_UNSIGNED_LONG(FLAGS)  \
     OPAL_DATATYPE_INIT_BASIC_DATATYPE(unsigned long, OPAL_ALIGNMENT_LONG, UNSIGNED_LONG, FLAGS)

#if defined(HAVE_SHORT_FLOAT) && SIZEOF_SHORT_FLOAT == 2
#    define OPAL_DATATYPE_HANDLE_FLOAT2(AV, NOTAV, FLAGS) \
        AV(short float, OPAL_ALIGNMENT_SHORT_FLOAT, FLOAT2, FLAGS)
#elif SIZEOF_FLOAT == 2
#    define OPAL_DATATYPE_HANDLE_FLOAT2(AV, NOTAV, FLAGS) \
        AV(float, OPAL_ALIGNMENT_FLOAT, FLOAT2, FLAGS)
#elif SIZEOF_DOUBLE == 2
#    define OPAL_DATATYPE_HANDLE_FLOAT2(AV, NOTAV, FLAGS) \
        AV(double, OPAL_ALIGNMENT_DOUBLE, FLOAT2, FLAGS)
#elif SIZEOF_LONG_DOUBLE == 2
#    define OPAL_DATATYPE_HANDLE_FLOAT2(AV, NOTAV, FLAGS) \
        AV(long double, OPAL_ALIGNMENT_LONG_DOUBLE, FLOAT2, FLAGS)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T) && SIZEOF_OPAL_SHORT_FLOAT_T == 2
#    define OPAL_DATATYPE_HANDLE_FLOAT2(AV, NOTAV, FLAGS) \
        AV(opal_short_float_t, OPAL_ALIGNMENT_OPAL_SHORT_FLOAT_T, FLOAT2, FLAGS)
#else
#    define OPAL_DATATYPE_HANDLE_FLOAT2(AV, NOTAV, FLAGS) NOTAV(FLOAT2, FLAGS)
#endif

#if defined(HAVE_SHORT_FLOAT) && SIZEOF_SHORT_FLOAT == 4
#    define OPAL_DATATYPE_HANDLE_FLOAT4(AV, NOTAV, FLAGS) \
        AV(short float, OPAL_ALIGNMENT_SHORT_FLOAT, FLOAT4, FLAGS)
#elif SIZEOF_FLOAT == 4
#    define OPAL_DATATYPE_HANDLE_FLOAT4(AV, NOTAV, FLAGS) \
        AV(float, OPAL_ALIGNMENT_FLOAT, FLOAT4, FLAGS)
#elif SIZEOF_DOUBLE == 4
#    define OPAL_DATATYPE_HANDLE_FLOAT4(AV, NOTAV, FLAGS) \
        AV(double, OPAL_ALIGNMENT_DOUBLE, FLOAT4, FLAGS)
#elif SIZEOF_LONG_DOUBLE == 4
#    define OPAL_DATATYPE_HANDLE_FLOAT4(AV, NOTAV, FLAGS) \
        AV(long double, OPAL_ALIGNMENT_LONG_DOUBLE, FLOAT4, FLAGS)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T) && SIZEOF_OPAL_SHORT_FLOAT_T == 4
#    define OPAL_DATATYPE_HANDLE_FLOAT4(AV, NOTAV, FLAGS) \
        AV(opal_short_float_t, OPAL_ALIGNMENT_OPAL_SHORT_FLOAT_T, FLOAT4, FLAGS)
#else
#    define OPAL_DATATYPE_HANDLE_FLOAT4(AV, NOTAV, FLAGS) NOTAV(FLOAT4, FLAGS)
#endif

#if defined(HAVE_SHORT_FLOAT) && SIZEOF_SHORT_FLOAT == 8
#    define OPAL_DATATYPE_HANDLE_FLOAT8(AV, NOTAV, FLAGS) \
        AV(short float, OPAL_ALIGNMENT_SHORT_FLOAT, FLOAT8, FLAGS)
#elif SIZEOF_FLOAT == 8
#    define OPAL_DATATYPE_HANDLE_FLOAT8(AV, NOTAV, FLAGS) \
        AV(float, OPAL_ALIGNMENT_FLOAT, FLOAT8, FLAGS)
#elif SIZEOF_DOUBLE == 8
#    define OPAL_DATATYPE_HANDLE_FLOAT8(AV, NOTAV, FLAGS) \
        AV(double, OPAL_ALIGNMENT_DOUBLE, FLOAT8, FLAGS)
#elif SIZEOF_LONG_DOUBLE == 8
#    define OPAL_DATATYPE_HANDLE_FLOAT8(AV, NOTAV, FLAGS) \
        AV(long double, OPAL_ALIGNMENT_LONG_DOUBLE, FLOAT8, FLAGS)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T) && SIZEOF_OPAL_SHORT_FLOAT_T == 8
#    define OPAL_DATATYPE_HANDLE_FLOAT8(AV, NOTAV, FLAGS) \
        AV(opal_short_float_t, OPAL_ALIGNMENT_OPAL_SHORT_FLOAT_T, FLOAT8, FLAGS)
#else
#    define OPAL_DATATYPE_HANDLE_FLOAT8(AV, NOTAV, FLAGS) NOTAV(FLOAT8, FLAGS)
#endif

/**
 * The Float12 is different from the others floating point types because the name is not indicating the
 * size of the type but the number of significant digits used when manipulating it. It is defined
 * to handle long double types on architectures where this type only has 80 useful bits (the rest, up to
 * 96 or 128 bits, are used for alignment purposes).
 */
#if SIZEOF_LONG_DOUBLE >= 12 && LDBL_MANT_DIG <= 64
#    define OPAL_DATATYPE_HANDLE_FLOAT12(AV, NOTAV, FLAGS) \
        AV(long double, OPAL_ALIGNMENT_LONG_DOUBLE, FLOAT12, FLAGS)
#    define OPAL_SIZEOF_FLOAT12 SIZEOF_LONG_DOUBLE
#elif SIZEOF_DOUBLE == 12 && DBL_MANT_DIG <= 64
#    define OPAL_DATATYPE_HANDLE_FLOAT12(AV, NOTAV, FLAGS) \
        AV(double, OPAL_ALIGNMENT_DOUBLE, FLOAT12, FLAGS)
#    define OPAL_SIZEOF_FLOAT12 SIZEOF_DOUBLE
#else
#    define OPAL_DATATYPE_HANDLE_FLOAT12(AV, NOTAV, FLAGS) NOTAV(FLOAT12, FLAGS)
#    define OPAL_SIZEOF_FLOAT12 0
#endif

#if defined(HAVE__FLOAT128) &&  SIZEOF__FLOAT128 == 16
#    define OPAL_DATATYPE_HANDLE_FLOAT16(AV, NOTAV, FLAGS) \
        AV(_Float128, OPAL_ALIGNMENT__FLOAT128, FLOAT16, FLAGS)
#    define OPAL_SIZEOF_FLOAT16 SIZEOF__FLOAT128
#elif defined(HAVE___FLOAT128) &&  SIZEOF___FLOAT128 == 16
#    define OPAL_DATATYPE_HANDLE_FLOAT16(AV, NOTAV, FLAGS) \
        AV(__float128, OPAL_ALIGNMENT___FLOAT128, FLOAT16, FLAGS)
#    define OPAL_SIZEOF_FLOAT16 SIZEOF___FLOAT128
#elif SIZEOF_LONG_DOUBLE == 16
#    define OPAL_DATATYPE_HANDLE_FLOAT16(AV, NOTAV, FLAGS) \
        AV(long double, OPAL_ALIGNMENT_LONG_DOUBLE, FLOAT16, FLAGS)
#    define OPAL_SIZEOF_FLOAT16 SIZEOF_LONG_DOUBLE
#elif SIZEOF_DOUBLE == 16
#    define OPAL_DATATYPE_HANDLE_FLOAT16(AV, NOTAV, FLAGS) \
        AV(double, OPAL_ALIGNMENT_DOUBLE, FLOAT16, FLAGS)
#    define OPAL_SIZEOF_FLOAT16 SIZEOF_DOUBLE
#elif SIZEOF_FLOAT == 16
#    define OPAL_DATATYPE_HANDLE_FLOAT16(AV, NOTAV, FLAGS) \
        AV(float, OPAL_ALIGNMENT_FLOAT, FLOAT16, FLAGS)
#    define OPAL_SIZEOF_FLOAT16 SIZEOF_FLOAT
#elif defined(HAVE_SHORT_FLOAT) && SIZEOF_SHORT_FLOAT == 16
#    define OPAL_DATATYPE_HANDLE_FLOAT16(AV, NOTAV, FLAGS) \
        AV(short float, OPAL_ALIGNMENT_SHORT_FLOAT, FLOAT16, FLAGS)
#    define OPAL_SIZEOF_FLOAT16 SIZEOF_SHORT_FLOAT
#elif defined(HAVE_OPAL_SHORT_FLOAT_T) && SIZEOF_OPAL_SHORT_FLOAT_T == 16
#    define OPAL_DATATYPE_HANDLE_FLOAT16(AV, NOTAV, FLAGS) \
        AV(opal_short_float_t, OPAL_ALIGNMENT_OPAL_SHORT_FLOAT_T, FLOAT16, FLAGS)
#    define OPAL_SIZEOF_FLOAT16 SIZEOF_OPAL_SHORT_FLOAT_T
#else
#    define OPAL_DATATYPE_HANDLE_FLOAT16(AV, NOTAV, FLAGS) NOTAV(FLOAT16, FLAGS)
#    define OPAL_SIZEOF_FLOAT16 0
#endif

#if defined(HAVE_SHORT_FLOAT__COMPLEX)
#    define OPAL_DATATYPE_HANDLE_SHORT_FLOAT_COMPLEX(AV, NOTAV, FLAGS) \
        AV(short float _Complex, OPAL_ALIGNMENT_SHORT_FLOAT_COMPLEX, SHORT_FLOAT_COMPLEX, FLAGS)
#elif defined(HAVE_OPAL_SHORT_FLOAT_COMPLEX_T)
#    define OPAL_DATATYPE_HANDLE_SHORT_FLOAT_COMPLEX(AV, NOTAV, FLAGS)                         \
        AV(opal_short_float_complex_t, OPAL_ALIGNMENT_OPAL_SHORT_FLOAT_T, SHORT_FLOAT_COMPLEX, \
           FLAGS)
#else
#    define OPAL_DATATYPE_HANDLE_SHORT_FLOAT_COMPLEX(AV, NOTAV, FLAGS) \
        NOTAV(SHORT_FLOAT_COMPLEX, FLAGS)
#endif

#define OPAL_DATATYPE_HANDLE_FLOAT_COMPLEX(AV, NOTAV, FLAGS) \
    AV(float _Complex, OPAL_ALIGNMENT_FLOAT_COMPLEX, FLOAT_COMPLEX, FLAGS)

#define OPAL_DATATYPE_HANDLE_DOUBLE_COMPLEX(AV, NOTAV, FLAGS) \
    AV(double _Complex, OPAL_ALIGNMENT_DOUBLE_COMPLEX, DOUBLE_COMPLEX, FLAGS)

#define OPAL_DATATYPE_HANDLE_LONG_DOUBLE_COMPLEX(AV, NOTAV, FLAGS) \
    AV(long double _Complex, OPAL_ALIGNMENT_LONG_DOUBLE_COMPLEX, LONG_DOUBLE_COMPLEX, FLAGS)

#if defined(HAVE__FLOAT128) && defined(HAVE__FLOAT128__COMPLEX)
#    define OPAL_DATATYPE_HANDLE_FLOAT128_COMPLEX(AV, NOTAV, FLAGS) \
        AV(_Float128 _Complex, OPAL_ALIGNMENT__FLOAT128_COMPLEX, FLOAT128_COMPLEX, FLAGS)
#elif defined(HAVE___FLOAT128) && defined(HAVE___FLOAT128__COMPLEX)
#    define OPAL_DATATYPE_HANDLE_FLOAT128_COMPLEX(AV, NOTAV, FLAGS) \
        AV(__float128 _Complex, OPAL_ALIGNMENT___FLOAT128_COMPLEX, FLOAT128_COMPLEX, FLAGS)
#else
#    define OPAL_DATATYPE_HANDLE_FLOAT128_COMPLEX(AV, NOTAV, FLAGS) \
        NOTAV(FLOAT128_COMPLEX, FLAGS)
#endif

#define OPAL_DATATYPE_HANDLE_BOOL(AV, NOTAV, FLAGS) \
    AV(_Bool, OPAL_ALIGNMENT_BOOL, BOOL, FLAGS)

#if OPAL_ALIGNMENT_WCHAR != 0
#    define OPAL_DATATYPE_HANDLE_WCHAR(AV, NOTAV, FLAGS) \
        AV(wchar_t, OPAL_ALIGNMENT_WCHAR, WCHAR, FLAGS)
#else
#    define OPAL_DATATYPE_HANDLE_WCHAR(AV, NOTAV, FLAGS) NOTAV(WCHAR, FLAGS)
#endif

#endif /* OPAL_DATATYPE_CONSTRUCTORS_H_HAS_BEEN_INCLUDED */
