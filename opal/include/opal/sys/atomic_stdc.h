/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2019-2021 Google, LLC. All rights reserved.
 * Copyright (c) 2019      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* This file provides shims between the opal atomics interface and the C11 atomics interface. It
 * is intended as the first step in moving to using C11 atomics across the entire codebase. Once
 * all officially supported compilers offer C11 atomic (GCC 4.9.0+, icc 2018+, pgi, xlc, etc) then
 * this shim will go away and the codebase will be updated to use C11's atomic support
 * directly.
 * This shim contains some functions already present in atomic_impl.h because we do not include
 * atomic_impl.h when using C11 atomics. It would require alot of #ifdefs to avoid duplicate
 * definitions to be worthwhile. */

#if !defined(OPAL_ATOMIC_STDC_H)
#    define OPAL_ATOMIC_STDC_H

#    include "opal_stdint.h"
#    include <stdatomic.h>
#    include <stdint.h>

/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/

static inline void opal_atomic_mb(void)
{
    atomic_thread_fence(memory_order_seq_cst);
}

static inline void opal_atomic_wmb(void)
{
    atomic_thread_fence(memory_order_release);
}

static inline void opal_atomic_rmb(void)
{
#    if defined(PLATFORM_ARCH_X86_64)
    /* work around a bug in older gcc versions (observed in gcc 6.x)
     * where acquire seems to get treated as a no-op instead of being
     * equivalent to __asm__ __volatile__("": : :"memory") on x86_64 */
    opal_atomic_mb();
#    else
    atomic_thread_fence(memory_order_acquire);
#    endif
}


/**********************************************************************
 *
 * Compare and Swap
 *
 *********************************************************************/

#    define opal_atomic_compare_exchange_strong_32(addr, compare, value)                    \
        atomic_compare_exchange_strong_explicit(addr, compare, value, memory_order_relaxed, \
                                                memory_order_relaxed)
#    define opal_atomic_compare_exchange_strong_acq_32(addr, compare, value)                \
        atomic_compare_exchange_strong_explicit(addr, compare, value, memory_order_acquire, \
                                                memory_order_relaxed)
#    define opal_atomic_compare_exchange_strong_rel_32(addr, compare, value)                \
        atomic_compare_exchange_strong_explicit(addr, compare, value, memory_order_release, \
                                                memory_order_relaxed)

#    define opal_atomic_compare_exchange_strong_64(addr, compare, value)                    \
        atomic_compare_exchange_strong_explicit(addr, compare, value, memory_order_relaxed, \
                                                memory_order_relaxed)
#    define opal_atomic_compare_exchange_strong_acq_64(addr, compare, value)                \
        atomic_compare_exchange_strong_explicit(addr, compare, value, memory_order_acquire, \
                                                memory_order_relaxed)
#    define opal_atomic_compare_exchange_strong_rel_64(addr, compare, value)                \
        atomic_compare_exchange_strong_explicit(addr, compare, value, memory_order_release, \
                                                memory_order_relaxed)

#    define opal_atomic_compare_exchange_strong_ptr(addr, compare, value)                   \
        atomic_compare_exchange_strong_explicit(addr, compare, value, memory_order_relaxed, \
                                                memory_order_relaxed)
#    define opal_atomic_compare_exchange_strong_acq_ptr(addr, compare, value)               \
        atomic_compare_exchange_strong_explicit(addr, compare, value, memory_order_acquire, \
                                                memory_order_relaxed)
#    define opal_atomic_compare_exchange_strong_rel_ptr(addr, compare, value)               \
        atomic_compare_exchange_strong_explicit(addr, compare, value, memory_order_release, \
                                                memory_order_relaxed)

#    if OPAL_HAVE_C11_CSWAP_INT128

/* the C11 atomic compare-exchange is lock free so use it */
#        define opal_atomic_compare_exchange_strong_128 atomic_compare_exchange_strong

#        define OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_128 1

#    elif OPAL_HAVE_SYNC_BUILTIN_CSWAP_INT128

/* fall back on the __sync builtin if available since it will emit the expected instruction on
 * x86_64 (cmpxchng16b) */
__opal_attribute_always_inline__ static inline bool
opal_atomic_compare_exchange_strong_128(opal_atomic_int128_t *addr, opal_int128_t *oldval,
                                        opal_int128_t newval)
{
    opal_int128_t prev = __sync_val_compare_and_swap(addr, *oldval, newval);
    bool ret = prev == *oldval;
    *oldval = prev;
    return ret;
}

#        define OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_128 1

#    else

#        define OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_128 0

#    endif


/**********************************************************************
 *
 * Swap
 *
 *********************************************************************/

#    define opal_atomic_swap_32(addr, value) \
        atomic_exchange_explicit((_Atomic unsigned int *) addr, value, memory_order_relaxed)
#    define opal_atomic_swap_64(addr, value) \
        atomic_exchange_explicit((_Atomic unsigned long *) addr, value, memory_order_relaxed)
#    define opal_atomic_swap_ptr(addr, value) \
        atomic_exchange_explicit((_Atomic unsigned long *) addr, value, memory_order_relaxed)


/**********************************************************************
 *
 * Atomic spinlocks
 *
 *********************************************************************/
/*
 * Lock initialization function. It set the lock to UNLOCKED.
 */
static inline void opal_atomic_lock_init(opal_atomic_lock_t *lock, int32_t value)
{
    atomic_flag_clear_explicit(lock, memory_order_relaxed);
}

static inline int opal_atomic_trylock(opal_atomic_lock_t *lock)
{
    return (int) atomic_flag_test_and_set(lock);
}

static inline void opal_atomic_lock(opal_atomic_lock_t *lock)
{
    while (opal_atomic_trylock(lock)) {
    }
}

static inline void opal_atomic_unlock(opal_atomic_lock_t *lock)
{
    atomic_flag_clear(lock);
}


/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/

#    define OPAL_ATOMIC_STDC_DEFINE_FETCH_OP(op, bits, type, operator)                             \
        static inline type opal_atomic_fetch_##op##_##bits(opal_atomic_##type *addr, type value)   \
        {                                                                                          \
            return atomic_fetch_##op##_explicit(addr, value, memory_order_relaxed);                \
        }                                                                                          \
                                                                                                   \
        static inline type opal_atomic_##op##_fetch_##bits(opal_atomic_##type *addr, type value)   \
        {                                                                                          \
            return atomic_fetch_##op##_explicit(addr, value, memory_order_relaxed) operator value; \
        }

OPAL_ATOMIC_STDC_DEFINE_FETCH_OP(add, 32, int32_t, +)
OPAL_ATOMIC_STDC_DEFINE_FETCH_OP(and, 32, int32_t, &)
OPAL_ATOMIC_STDC_DEFINE_FETCH_OP(or, 32, int32_t, |)
OPAL_ATOMIC_STDC_DEFINE_FETCH_OP(xor, 32, int32_t, ^)
OPAL_ATOMIC_STDC_DEFINE_FETCH_OP(sub, 32, int32_t, -)

OPAL_ATOMIC_STDC_DEFINE_FETCH_OP(add, 64, int64_t, +)
OPAL_ATOMIC_STDC_DEFINE_FETCH_OP(and, 64, int64_t, &)
OPAL_ATOMIC_STDC_DEFINE_FETCH_OP(or, 64, int64_t, |)
OPAL_ATOMIC_STDC_DEFINE_FETCH_OP(xor, 64, int64_t, ^)
OPAL_ATOMIC_STDC_DEFINE_FETCH_OP(sub, 64, int64_t, -)

OPAL_ATOMIC_STDC_DEFINE_FETCH_OP(add, size_t, size_t, +)
OPAL_ATOMIC_STDC_DEFINE_FETCH_OP(sub, size_t, size_t, -)

#    define opal_atomic_add(addr, value) \
        (void) atomic_fetch_add_explicit(addr, value, memory_order_relaxed)

#include "opal/sys/atomic_impl_minmax_math.h"

#endif /* !defined(OPAL_ATOMIC_STDC_H) */
