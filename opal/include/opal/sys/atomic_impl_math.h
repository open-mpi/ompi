/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Implementation of the required atomic math functions in terms of
 * compare and swap operators.
 */

#ifndef ATOMIC_IMPL_MATH_H
#define ATOMIC_IMPL_MATH_H 1

#define OPAL_ATOMIC_DEFINE_OP(type, bits, operation, name)                                         \
        static inline type opal_atomic_fetch_##name##_##bits(opal_atomic_##type *addr, type value) \
        {                                                                                          \
            type oldval;                                                                           \
            do {                                                                                   \
                oldval = *addr;                                                                    \
            } while (!opal_atomic_compare_exchange_strong_##bits(addr, &oldval,                    \
                                                                 oldval operation value));         \
                                                                                                   \
            return oldval;                                                                         \
        }                                                                                          \
                                                                                                   \
        static inline type opal_atomic_##name##_fetch_##bits(opal_atomic_##type *addr, type value) \
        {                                                                                          \
            type oldval, newval;                                                                   \
            do {                                                                                   \
                oldval = *addr;                                                                    \
                newval = oldval operation value;                                                   \
            } while (!opal_atomic_compare_exchange_strong_##bits(addr, &oldval, newval);           \
                                                                                                   \
            return newval;                                                                         \
        }

OPAL_ATOMIC_DEFINE_OP(int32_t, 32, +, add)
OPAL_ATOMIC_DEFINE_OP(int32_t, 32, &, and)
OPAL_ATOMIC_DEFINE_OP(int32_t, 32, |, or)
OPAL_ATOMIC_DEFINE_OP(int32_t, 32, ^, xor)
OPAL_ATOMIC_DEFINE_OP(int32_t, 32, -, sub)

OPAL_ATOMIC_DEFINE_OP(int64_t, 64, +, add)
OPAL_ATOMIC_DEFINE_OP(int64_t, 64, &, and)
OPAL_ATOMIC_DEFINE_OP(int64_t, 64, |, or)
OPAL_ATOMIC_DEFINE_OP(int64_t, 64, ^, xor)
OPAL_ATOMIC_DEFINE_OP(int64_t, 64, -, sub)

#include "opal/sys/atomic_impl_minmax_math.h"

#endif /* #ifndef ATOMIC_MATH_IMPL_H */
