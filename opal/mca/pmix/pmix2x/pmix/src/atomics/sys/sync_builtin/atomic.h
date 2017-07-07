/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2014-2016 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_SYS_ARCH_ATOMIC_H
#define PMIX_SYS_ARCH_ATOMIC_H 1

/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/
#define PMIX_HAVE_ATOMIC_MEM_BARRIER 1

static inline void pmix_atomic_mb(void)
{
    __sync_synchronize();
}

static inline void pmix_atomic_rmb(void)
{
    __sync_synchronize();
}

static inline void pmix_atomic_wmb(void)
{
    __sync_synchronize();
}

#define PMIXMB() pmix_atomic_mb()

/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/

#define PMIX_HAVE_ATOMIC_CMPSET_32 1
static inline int pmix_atomic_cmpset_acq_32( volatile int32_t *addr,
                                             int32_t oldval, int32_t newval)
{
    return __sync_bool_compare_and_swap(addr, oldval, newval);
}


static inline int pmix_atomic_cmpset_rel_32( volatile int32_t *addr,
                                             int32_t oldval, int32_t newval)
{
    return __sync_bool_compare_and_swap(addr, oldval, newval);}

static inline int pmix_atomic_cmpset_32( volatile int32_t *addr,
                                         int32_t oldval, int32_t newval)
{
    return __sync_bool_compare_and_swap(addr, oldval, newval);
}

#define PMIX_HAVE_ATOMIC_MATH_32 1

#define PMIX_HAVE_ATOMIC_ADD_32 1
static inline int32_t pmix_atomic_add_32(volatile int32_t *addr, int32_t delta)
{
    return __sync_add_and_fetch(addr, delta);
}

#define PMIX_HAVE_ATOMIC_SUB_32 1
static inline int32_t pmix_atomic_sub_32(volatile int32_t *addr, int32_t delta)
{
    return __sync_sub_and_fetch(addr, delta);
}

#if PMIX_ASM_SYNC_HAVE_64BIT

#define PMIX_HAVE_ATOMIC_CMPSET_64 1
static inline int pmix_atomic_cmpset_acq_64( volatile int64_t *addr,
                                             int64_t oldval, int64_t newval)
{
    return __sync_bool_compare_and_swap(addr, oldval, newval);
}

static inline int pmix_atomic_cmpset_rel_64( volatile int64_t *addr,
                                             int64_t oldval, int64_t newval)
{
    return __sync_bool_compare_and_swap(addr, oldval, newval);}


static inline int pmix_atomic_cmpset_64( volatile int64_t *addr,
                                         int64_t oldval, int64_t newval)
{
    return __sync_bool_compare_and_swap(addr, oldval, newval);
}

#define PMIX_HAVE_ATOMIC_MATH_64 1
#define PMIX_HAVE_ATOMIC_ADD_64 1
static inline int64_t pmix_atomic_add_64(volatile int64_t *addr, int64_t delta)
{
    return __sync_add_and_fetch(addr, delta);
}

#define PMIX_HAVE_ATOMIC_SUB_64 1
static inline int64_t pmix_atomic_sub_64(volatile int64_t *addr, int64_t delta)
{
    return __sync_sub_and_fetch(addr, delta);
}

#endif

#if PMIX_HAVE_SYNC_BUILTIN_CSWAP_INT128
static inline int pmix_atomic_cmpset_128 (volatile pmix_int128_t *addr,
                                          pmix_int128_t oldval, pmix_int128_t newval)
{
    return __sync_bool_compare_and_swap(addr, oldval, newval);
}

#define PMIX_HAVE_ATOMIC_CMPSET_128 1

#endif

#endif /* ! PMIX_SYS_ARCH_ATOMIC_H */
