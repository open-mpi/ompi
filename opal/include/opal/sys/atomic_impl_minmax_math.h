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
 * Implementation of the min/max atomic functions in terms of compare
 * and swap.  These are broken out from the basic atomic_impl_math.h
 * functions because most atomic implementations do not provide native
 * min/max interfaces.
 */

#ifndef ATOMIC_IMPL_MINMAX_MATH_H
#define ATOMIC_IMPL_MINMAX_MATH_H 1

static inline int32_t opal_atomic_fetch_min_32(opal_atomic_int32_t *addr, int32_t value)
{
    int32_t old = *addr;
    do {
        if (old <= value) {
            break;
        }
    } while (!opal_atomic_compare_exchange_strong_32(addr, &old, value));

    return old;
}

static inline int32_t opal_atomic_min_fetch_32(opal_atomic_int32_t *addr, int32_t value)
{
    int32_t old = opal_atomic_fetch_min_32(addr, value);
    return old <= value ? old : value;
}

static inline int32_t opal_atomic_fetch_max_32(opal_atomic_int32_t *addr, int32_t value)
{
    int32_t old = *addr;
    do {
        if (old >= value) {
            break;
        }
    } while (!opal_atomic_compare_exchange_strong_32(addr, &old, value));

    return old;
}

static inline int32_t opal_atomic_max_fetch_32(opal_atomic_int32_t *addr, int32_t value)
{
    int32_t old = opal_atomic_fetch_max_32(addr, value);
    return old >= value ? old : value;
}

static inline int64_t opal_atomic_fetch_min_64(opal_atomic_int64_t *addr, int64_t value)
{
    int64_t old = *addr;
    do {
        if (old <= value) {
            break;
        }
    } while (!opal_atomic_compare_exchange_strong_64(addr, &old, value));

    return old;
}

static inline int64_t opal_atomic_fetch_max_64(opal_atomic_int64_t *addr, int64_t value)
{
    int64_t old = *addr;
    do {
        if (old >= value) {
            break;
        }
    } while (!opal_atomic_compare_exchange_strong_64(addr, &old, value));

    return old;
}

static inline int64_t opal_atomic_min_fetch_64(opal_atomic_int64_t *addr, int64_t value)
{
    int64_t old = opal_atomic_fetch_min_64(addr, value);
    return old <= value ? old : value;
}

static inline int64_t opal_atomic_max_fetch_64(opal_atomic_int64_t *addr, int64_t value)
{
    int64_t old = opal_atomic_fetch_max_64(addr, value);
    return old >= value ? old : value;
}

#endif /* #ifndef ATOMIC_MATH_MINMAX_IMPL_H */
