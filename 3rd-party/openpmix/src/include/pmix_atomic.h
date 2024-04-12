/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2011      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2011-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c)           Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file
 *
 * Atomic operations.
 *
 * This API is patterned after the FreeBSD kernel atomic interface
 * (which is influenced by Intel's ia64 architecture).  The
 * FreeBSD interface is documented at
 *
 * http://www.freebsd.org/cgi/man.cgi?query=atomic&sektion=9
 *
 * Only the necessary subset of functions are implemented here.
 *
 * The following #defines will be true / false based on
 * assembly support:
 *
 *  - \c PMIX_HAVE_ATOMIC_MEM_BARRIER atomic memory barriers
 *  - \c PMIX_HAVE_ATOMIC_SPINLOCKS atomic spinlocks
 *  - \c PMIX_HAVE_ATOMIC_MATH_32 if 32 bit add/sub/compare-exchange can be done "atomically"
 *  - \c PMIX_HAVE_ATOMIC_MATH_64 if 64 bit add/sub/compare-exchange can be done "atomically"
 *
 * Note that for the Atomic math, atomic add/sub may be implemented as
 * C code using pmix_atomic_compare_exchange.  The appearance of atomic
 * operation will be upheld in these cases.
 */

#ifndef PMIX_SYS_ATOMIC_H
#define PMIX_SYS_ATOMIC_H 1

#include "src/include/pmix_config.h"
#include "src/include/pmix_stdatomic.h"

#if PMIX_USE_C11_ATOMICS

#include <stdatomic.h>

static inline void pmix_atomic_wmb(void)
{
    atomic_thread_fence(memory_order_release);
}

static inline void pmix_atomic_rmb(void)
{
#    if defined(PMIX_ATOMIC_X86_64)
    /* work around a bug in older gcc versions (observed in gcc 6.x)
     * where acquire seems to get treated as a no-op instead of being
     * equivalent to __asm__ __volatile__("": : :"memory") on x86_64 */
    __asm__ __volatile__("" : : : "memory");
#    else
    atomic_thread_fence(memory_order_acquire);
#    endif
}

#define PMIX_ATOMIC_DEFINE_OP(type, bits, operator, name)                   \
    static inline type                                                      \
    pmix_atomic_fetch_##name##_##bits(pmix_atomic_##type *addr, type value) \
    {                                                                       \
        return atomic_fetch_##name##_explicit(                              \
            addr, value, memory_order_relaxed);                             \
    }                                                                       \
                                                                            \
    static inline type                                                      \
    pmix_atomic_##name##_fetch_##bits(pmix_atomic_##type *addr, type value) \
    {                                                                       \
        return atomic_fetch_##name##_explicit(                              \
            addr, value, memory_order_relaxed) operator value;              \
    }

/* end of PMIX_USE_C11_ATOMICS */
#elif PMIX_USE_GCC_BUILTIN_ATOMICS

static inline void pmix_atomic_wmb(void)
{
    __atomic_thread_fence(__ATOMIC_RELEASE);
}

static inline void pmix_atomic_rmb(void)
{
#if defined(PMIX_ATOMIC_X86_64)
    /* work around a bug in older gcc versions where ACQUIRE seems to get
     * treated as a no-op instead of being equivalent to
     * __asm__ __volatile__("": : :"memory") */
    __asm__ __volatile__("" : : : "memory");
#else
    __atomic_thread_fence(__ATOMIC_ACQUIRE);
#endif
}

#define PMIX_ATOMIC_DEFINE_OP(type, bits, operator, name)                      \
    static inline type                                                         \
    pmix_atomic_fetch_##name##_##bits(pmix_atomic_##type *addr, type value)    \
    {                                                                          \
        return __atomic_fetch_##name(addr, value, __ATOMIC_RELAXED);           \
    }                                                                          \
                                                                               \
    static inline type                                                         \
    pmix_atomic_##name##_fetch_##bits(pmix_atomic_##type *addr, type value)    \
    {                                                                          \
        return __atomic_##name##_fetch(addr, value, __ATOMIC_RELAXED);         \
    }

/* end of PMIX_USE_GCC_BUILTIN_ATOMICS */
#else
#error OpenPMIx requires either C11 atomics support or GCC built-in atomics.
#endif

PMIX_ATOMIC_DEFINE_OP(int32_t, 32, +, add)
PMIX_ATOMIC_DEFINE_OP(int32_t, 32, &, and)
PMIX_ATOMIC_DEFINE_OP(int32_t, 32, |, or)
PMIX_ATOMIC_DEFINE_OP(int32_t, 32, ^, xor)
PMIX_ATOMIC_DEFINE_OP(int32_t, 32, -, sub)

#endif /* PMIX_SYS_ATOMIC_H */
