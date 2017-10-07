/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2016 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(PMIX_THREAD_USAGE_H)
#define PMIX_THREAD_USAGE_H

#include "pmix_config.h"

#include "src/atomics/sys/atomic.h"
#include "src/include/prefetch.h"

/**
 * Use an atomic operation for increment/decrement
 */

#define PMIX_THREAD_DEFINE_ATOMIC_ADD(type, suffix)     \
static inline type pmix_thread_add_ ## suffix (volatile type *addr, type delta) \
{                                                                   \
    return pmix_atomic_add_ ## suffix (addr, delta);                \
}

#define PMIX_THREAD_DEFINE_ATOMIC_SUB(type, suffix)     \
static inline type pmix_thread_sub_ ## suffix (volatile type *addr, type delta) \
{                                                                       \
    return pmix_atomic_sub_ ## suffix (addr, delta);                \
}

#define PMIX_THREAD_DEFINE_ATOMIC_CMPSET(type, addr_type, suffix)       \
static inline bool pmix_thread_cmpset_bool_ ## suffix (volatile addr_type *addr, type compare, type value) \
{                                                                       \
   return pmix_atomic_cmpset_ ## suffix ((volatile type *) addr, compare, value); \
}

#define PMIX_THREAD_DEFINE_ATOMIC_SWAP(type, addr_type, suffix)         \
static inline type pmix_thread_swap_ ## suffix (volatile addr_type *ptr, type newvalue) \
{                                                                       \
    return pmix_atomic_swap_ ## suffix ((volatile type *) ptr, newvalue); \
}

PMIX_THREAD_DEFINE_ATOMIC_ADD(int32_t, 32)
PMIX_THREAD_DEFINE_ATOMIC_ADD(size_t, size_t)
PMIX_THREAD_DEFINE_ATOMIC_SUB(size_t, size_t)
PMIX_THREAD_DEFINE_ATOMIC_CMPSET(int32_t, int32_t, 32)
PMIX_THREAD_DEFINE_ATOMIC_CMPSET(void *, intptr_t, ptr)
PMIX_THREAD_DEFINE_ATOMIC_SWAP(int32_t, int32_t, 32)
PMIX_THREAD_DEFINE_ATOMIC_SWAP(void *, intptr_t, ptr)

#define PMIX_THREAD_ADD32 pmix_thread_add_32
#define PMIX_ATOMIC_ADD32 pmix_thread_add_32

#define PMIX_THREAD_ADD_SIZE_T pmix_thread_add_size_t
#define PMIX_ATOMIC_ADD_SIZE_T pmix_thread_add_size_t

#define PMIX_THREAD_SUB_SIZE_T pmix_thread_sub_size_t
#define PMIX_ATOMIC_SUB_SIZE_T pmix_thread_sub_size_t

#define PMIX_THREAD_CMPSET_32 pmix_thread_cmpset_bool_32
#define PMIX_ATOMIC_CMPSET_32 pmix_thread_cmpset_bool_32

#define PMIX_THREAD_CMPSET_PTR(x, y, z) pmix_thread_cmpset_bool_ptr ((volatile intptr_t *) x, (void *) y, (void *) z)
#define PMIX_ATOMIC_CMPSET_PTR PMIX_THREAD_CMPSET_PTR

#define PMIX_THREAD_SWAP_32 pmix_thread_swap_32
#define PMIX_ATOMIC_SWAP_32 pmix_thread_swap_32

#define PMIX_THREAD_SWAP_PTR(x, y) pmix_thread_swap_ptr ((volatile intptr_t *) x, (void *) y)
#define PMIX_ATOMIC_SWAP_PTR PMIX_THREAD_SWAP_PTR

/* define 64-bit macros is 64-bit atomic math is available */
#if PMIX_HAVE_ATOMIC_MATH_64

PMIX_THREAD_DEFINE_ATOMIC_ADD(int64_t, 64)
PMIX_THREAD_DEFINE_ATOMIC_CMPSET(int64_t, int64_t, 64)
PMIX_THREAD_DEFINE_ATOMIC_SWAP(int64_t, int64_t, 64)

#define PMIX_THREAD_ADD64 pmix_thread_add_64
#define PMIX_ATOMIC_ADD64 pmix_thread_add_64

#define PMIX_THREAD_CMPSET_64 pmix_thread_cmpset_bool_64
#define PMIX_ATOMIC_CMPSET_64 pmix_thread_cmpset_bool_64

#define PMIX_THREAD_SWAP_64 pmix_thread_swap_64
#define PMIX_ATOMIC_SWAP_64 pmix_thread_swap_64

#endif

#endif /* !defined(PMIX_THREAD_USAGE_H) */
