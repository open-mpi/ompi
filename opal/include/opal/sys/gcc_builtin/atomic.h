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
 * Copyright (c) 2014-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016-2021 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2021      Google, LLC. All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_SYS_ARCH_ATOMIC_H
#define OPAL_SYS_ARCH_ATOMIC_H 1

/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/

#if defined(PLATFORM_ARCH_X86_64) && defined (__GNUC__) && !defined(__llvm) && (__GNUC__ < 6)
    /* work around a bug in older gcc versions where ACQUIRE seems to get
     * treated as a no-op instead */
#define OPAL_BUSTED_ATOMIC_MB 1
#else
#define OPAL_BUSTED_ATOMIC_MB 0
#endif

static inline void opal_atomic_mb(void)
{
    __atomic_thread_fence(__ATOMIC_SEQ_CST);
}

static inline void opal_atomic_rmb(void)
{
#if OPAL_BUSTED_ATOMIC_MB
    __asm__ __volatile__("" : : : "memory");
#else
    __atomic_thread_fence(__ATOMIC_ACQUIRE);
#endif
}

static inline void opal_atomic_wmb(void)
{
    __atomic_thread_fence(__ATOMIC_RELEASE);
}


/**********************************************************************
 *
 * Compare and Swap
 *
 *********************************************************************/

/*
 * Suppress numerous (spurious ?) warnings from Oracle Studio compilers
 * see https://community.oracle.com/thread/3968347
 */
#if defined(__SUNPRO_C) || defined(__SUNPRO_CC)
#    pragma error_messages(off, E_ARG_INCOMPATIBLE_WITH_ARG_L)
#endif

static inline bool opal_atomic_compare_exchange_strong_32(opal_atomic_int32_t *addr,
                                                          int32_t *oldval, int32_t newval)
{
    return __atomic_compare_exchange_n(addr, oldval, newval, false, __ATOMIC_ACQUIRE,
                                       __ATOMIC_RELAXED);
}

static inline bool opal_atomic_compare_exchange_strong_acq_32(opal_atomic_int32_t *addr,
                                                              int32_t *oldval, int32_t newval)
{
    return __atomic_compare_exchange_n(addr, oldval, newval, false, __ATOMIC_ACQUIRE,
                                       __ATOMIC_RELAXED);
}

static inline bool opal_atomic_compare_exchange_strong_rel_32(opal_atomic_int32_t *addr,
                                                              int32_t *oldval, int32_t newval)
{
    return __atomic_compare_exchange_n(addr, oldval, newval, false, __ATOMIC_RELEASE,
                                       __ATOMIC_RELAXED);
}

static inline bool opal_atomic_compare_exchange_strong_64(opal_atomic_int64_t *addr,
                                                          int64_t *oldval, int64_t newval)
{
    return __atomic_compare_exchange_n(addr, oldval, newval, false, __ATOMIC_ACQUIRE,
                                       __ATOMIC_RELAXED);
}

static inline bool opal_atomic_compare_exchange_strong_acq_64(opal_atomic_int64_t *addr,
                                                              int64_t *oldval, int64_t newval)
{
    return __atomic_compare_exchange_n(addr, oldval, newval, false, __ATOMIC_ACQUIRE,
                                       __ATOMIC_RELAXED);
}

static inline bool opal_atomic_compare_exchange_strong_rel_64(opal_atomic_int64_t *addr,
                                                              int64_t *oldval, int64_t newval)
{
    return __atomic_compare_exchange_n(addr, oldval, newval, false, __ATOMIC_RELEASE,
                                       __ATOMIC_RELAXED);
}

#include "opal/sys/atomic_impl_ptr_cswap.h"

#if OPAL_HAVE_GCC_BUILTIN_CSWAP_INT128

#    define OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_128 1

static inline bool opal_atomic_compare_exchange_strong_128(opal_atomic_int128_t *addr,
                                                           opal_int128_t *oldval,
                                                           opal_int128_t newval)
{
    return __atomic_compare_exchange_n(addr, oldval, newval, false, __ATOMIC_ACQUIRE,
                                       __ATOMIC_RELAXED);
}

#elif defined(OPAL_HAVE_SYNC_BUILTIN_CSWAP_INT128) && OPAL_HAVE_SYNC_BUILTIN_CSWAP_INT128

#    define OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_128 1

/* __atomic version is not lock-free so use legacy __sync version */

static inline bool opal_atomic_compare_exchange_strong_128(opal_atomic_int128_t *addr,
                                                           opal_int128_t *oldval,
                                                           opal_int128_t newval)
{
    opal_int128_t prev = __sync_val_compare_and_swap(addr, *oldval, newval);
    bool ret = prev == *oldval;
    *oldval = prev;
    return ret;
}

#endif


/**********************************************************************
 *
 * Swap
 *
 *********************************************************************/

static inline int32_t opal_atomic_swap_32(opal_atomic_int32_t *addr, int32_t newval)
{
    int32_t oldval;
    __atomic_exchange(addr, &newval, &oldval, __ATOMIC_RELAXED);
    return oldval;
}

static inline int64_t opal_atomic_swap_64(opal_atomic_int64_t *addr, int64_t newval)
{
    int64_t oldval;
    __atomic_exchange(addr, &newval, &oldval, __ATOMIC_RELAXED);
    return oldval;
}

static inline intptr_t opal_atomic_swap_ptr(opal_atomic_intptr_t *addr, intptr_t newval)
{
    intptr_t oldval;
    __atomic_exchange(addr, &newval, &oldval, __ATOMIC_RELAXED);
    return oldval;
}


/**********************************************************************
 *
 * Atomic spinlocks
 *
 *********************************************************************/

#if defined(__HLE__)

#    include <immintrin.h>

static inline void opal_atomic_lock_init(opal_atomic_lock_t *lock, int32_t value)
{
    lock = value;
}

static inline int opal_atomic_trylock(opal_atomic_lock_t *lock)
{
    int ret = __atomic_exchange_n(&lock, OPAL_ATOMIC_LOCK_LOCKED,
                                  __ATOMIC_ACQUIRE | __ATOMIC_HLE_ACQUIRE);
    if (OPAL_ATOMIC_LOCK_LOCKED == ret) {
        /* abort the transaction */
        _mm_pause();
        return 1;
    }

    return 0;
}

static inline void opal_atomic_lock(opal_atomic_lock_t *lock)
{
    while (OPAL_ATOMIC_LOCK_LOCKED
           == __atomic_exchange_n(&lock, OPAL_ATOMIC_LOCK_LOCKED,
                                  __ATOMIC_ACQUIRE | __ATOMIC_HLE_ACQUIRE)) {
        /* abort the transaction */
        _mm_pause();
    }
}

static inline void opal_atomic_unlock(opal_atomic_lock_t *lock)
{
    __atomic_store_n(&lock, OPAL_ATOMIC_LOCK_UNLOCKED,
                     __ATOMIC_RELEASE | __ATOMIC_HLE_RELEASE);
}

#else  /* #if defined(__HLE__) */

#include "opal/sys/atomic_impl_spinlock.h"

#endif


/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/

#define OPAL_ATOMIC_DEFINE_OP(type, bits, operator, name)                                          \
        static inline type opal_atomic_fetch_##name##_##bits(opal_atomic_##type *addr, type value) \
        {                                                                                          \
            return __atomic_fetch_##name(addr, value, __ATOMIC_RELAXED);                           \
        }                                                                                          \
                                                                                                   \
        static inline type opal_atomic_##name##_fetch_##bits(opal_atomic_##type *addr, type value) \
        {                                                                                          \
            return __atomic_##name##_fetch(addr, value, __ATOMIC_RELAXED);                         \
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

OPAL_ATOMIC_DEFINE_OP(size_t, size_t, +, add)
OPAL_ATOMIC_DEFINE_OP(size_t, size_t, -, sub)

#define opal_atomic_add(ADDR, VALUE) \
     (void) __atomic_fetch_add(ADDR, VALUE, __ATOMIC_RELAXED)

#include "opal/sys/atomic_impl_minmax_math.h"

#if defined(__SUNPRO_C) || defined(__SUNPRO_CC)
#    pragma error_messages(default, E_ARG_INCOMPATIBLE_WITH_ARG_L)
#endif

#endif /* ! OPAL_SYS_ARCH_ATOMIC_H */
