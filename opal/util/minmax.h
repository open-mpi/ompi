/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * @file
 */

/**
 * @file Contains implementations of opal_min(a,b) and opal_max(a,b).
 */
#ifndef OPAL_MINMAX_H
#define OPAL_MINMAX_H

#include "opal_stdint.h"

#define OPAL_DEFINE_MINMAX(type, suffix)                                   \
    static inline const type opal_min_##suffix(const type a, const type b) \
    {                                                                      \
        return (a < b) ? a : b;                                            \
    }                                                                      \
    static inline const type opal_max_##suffix(const type a, const type b) \
    {                                                                      \
        return (a > b) ? a : b;                                            \
    }

OPAL_DEFINE_MINMAX(int8_t, 8)
OPAL_DEFINE_MINMAX(uint8_t, u8)
OPAL_DEFINE_MINMAX(int16_t, 16)
OPAL_DEFINE_MINMAX(uint16_t, u16)
OPAL_DEFINE_MINMAX(int32_t, 32)
OPAL_DEFINE_MINMAX(uint32_t, u32)
OPAL_DEFINE_MINMAX(int64_t, 64)
OPAL_DEFINE_MINMAX(uint64_t, u64)
OPAL_DEFINE_MINMAX(size_t, size_t)
OPAL_DEFINE_MINMAX(ssize_t, ssize_t)
OPAL_DEFINE_MINMAX(float, float)
OPAL_DEFINE_MINMAX(double, double)
OPAL_DEFINE_MINMAX(void *, ptr)

#if OPAL_C_HAVE__GENERIC
#    define opal_min(a, b) \
        (_Generic((a) + (b),                                                \
                                 int8_t: opal_min_8,                    \
                                 uint8_t: opal_min_u8,                  \
                                 int16_t: opal_min_16,                  \
                                 uint16_t: opal_min_u16,                \
                                 int32_t: opal_min_32,                  \
                                 uint32_t: opal_min_u32,                \
                                 int64_t: opal_min_64,                  \
                                 uint64_t: opal_min_u64,                \
                                 float: opal_min_float,                 \
                                 double: opal_min_double,               \
                                 void *: opal_min_ptr,                  \
                                 default: opal_min_64)((a), (b)))

#    define opal_max(a, b) \
        (_Generic((a) + (b),                                                \
                                 int8_t: opal_max_8,                    \
                                 uint8_t: opal_max_u8,                  \
                                 int16_t: opal_max_16,                  \
                                 uint16_t: opal_max_u16,                \
                                 int32_t: opal_max_32,                  \
                                 uint32_t: opal_max_u32,                \
                                 int64_t: opal_max_64,                  \
                                 uint64_t: opal_max_u64,                \
                                 float: opal_max_float,                 \
                                 double: opal_max_double,               \
                                 void *: opal_max_ptr,                  \
                                 default: opal_max_64)((a), (b)))
#else

/* these versions suffer from double-evaluation. please upgrade to a modern compiler */

#    define opal_min(a, b) (((a) < (b)) ? (a) : (b))
#    define opal_max(a, b) (((a) > (b)) ? (a) : (b))

#endif

#endif /* OPAL_MINMAX_H */
