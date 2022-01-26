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
 * Implementation of size_t atomic add functions as wrappers around
 * sized implementations.
 */

#ifndef ATOMIC_IMPL_SIZE_T_MATH_H
#define ATOMIC_IMPL_SIZE_T_MATH_H 1

#include <stdlib.h>

static inline size_t opal_atomic_add_fetch_size_t(opal_atomic_size_t *addr, size_t delta)
{
#if SIZEOF_SIZE_T == 4
    return (size_t)opal_atomic_add_fetch_32((opal_atomic_int32_t *) addr, delta);
#elif SIZEOF_SIZE_T == 8
    return (size_t)opal_atomic_add_fetch_64((opal_atomic_int64_t *) addr, delta);
#else
#error "No implementation of opal_atomic_add_fetch_size_t"
#endif
}

static inline size_t opal_atomic_fetch_add_size_t(opal_atomic_size_t *addr, size_t delta)
{
#if SIZEOF_SIZE_T == 4
    return (size_t)opal_atomic_fetch_add_32((opal_atomic_int32_t *) addr, delta);
#elif SIZEOF_SIZE_T == 8
    return (size_t)opal_atomic_fetch_add_64((opal_atomic_int64_t *) addr, delta);
#else
#error "No implementation of opal_atomic_fetch_add_size_t"
#endif
}

static inline size_t opal_atomic_sub_fetch_size_t(opal_atomic_size_t *addr, size_t delta)
{
#if SIZEOF_SIZE_T == 4
    return (size_t)opal_atomic_sub_fetch_32((opal_atomic_int32_t *) addr, delta);
#elif SIZEOF_SIZE_T == 8
    return (size_t)opal_atomic_sub_fetch_64((opal_atomic_int64_t *) addr, delta);
#else
#error "No implementation of opal_atomic_sub_fetch_size_t"
#endif
}

static inline size_t opal_atomic_fetch_sub_size_t(opal_atomic_size_t *addr, size_t delta)
{
#if SIZEOF_SIZE_T == 4
    return (size_t)opal_atomic_fetch_sub_32((opal_atomic_int32_t *) addr, delta);
#elif SIZEOF_SIZE_T == 8
    return (size_t)opal_atomic_fetch_sub_64((opal_atomic_int64_t *) addr, delta);
#else
#error "No implementation of opal_atomic_fetch_sub_size_t"
#endif
}

/**
 * Atomically increment the content depending on the type. This
 * macro detect at compile time the type of the first argument
 * and choose the correct function to be called.
 *
 * \note This macro should only be used for integer types.
 *
 * @param addr          Address of <TYPE>
 * @param delta         Value to add (converted to <TYPE>).
 */
#define opal_atomic_add(ADDR, VALUE) \
    opal_atomic_add_xx((opal_atomic_intptr_t *) (ADDR), (int32_t)(VALUE), sizeof(*(ADDR)))

static inline void opal_atomic_add_xx(opal_atomic_intptr_t *addr, int32_t value, size_t length)
{
    switch (length) {
        case 4:
            (void)opal_atomic_fetch_add_32((opal_atomic_int32_t*)addr, value);
            break;
        case 8:
            (void)opal_atomic_fetch_add_64((opal_atomic_int64_t*)addr, value);
            break;
        default:
            abort();
    }
}

#endif
