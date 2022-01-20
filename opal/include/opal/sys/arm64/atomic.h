/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * Copyright (c) 2010      ARM ltd.  All rights reserved.
 * Copyright (c) 2016-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2021      Triad National Security, LLC. All rights reserved.
 * Copyright (c) 2021      Google, LLC. All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "atomic_llsc.h"

#ifndef OPAL_SYS_ARCH_ATOMIC_H
#define OPAL_SYS_ARCH_ATOMIC_H 1


/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/

static inline void opal_atomic_mb(void)
{
    __asm__ __volatile__("dmb sy" : : : "memory");
}

static inline void opal_atomic_rmb(void)
{
    __asm__ __volatile__("dmb ld" : : : "memory");
}

static inline void opal_atomic_wmb(void)
{
    __asm__ __volatile__("dmb st" : : : "memory");
}

static inline void opal_atomic_isync(void)
{
    __asm__ __volatile__("isb");
}


/**********************************************************************
 *
 * Compare and Swap
 *
 *********************************************************************/

static inline bool opal_atomic_compare_exchange_strong_32(opal_atomic_int32_t *addr,
                                                          int32_t *oldval, int32_t newval)
{
    int32_t prev, tmp;
    bool ret;

    __asm__ __volatile__("1:  ldaxr    %w0, [%2]      \n"
                         "    cmp     %w0, %w3        \n"
                         "    bne     2f              \n"
                         "    stxr    %w1, %w4, [%2]  \n"
                         "    cbnz    %w1, 1b         \n"
                         "2:                          \n"
                         : "=&r"(prev), "=&r"(tmp)
                         : "r"(addr), "r"(*oldval), "r"(newval)
                         : "cc", "memory");

    ret = (prev == *oldval);
    *oldval = prev;
    return ret;
}

/* these two functions aren't inlined in the non-gcc case because then
   there would be two function calls (since neither cmpset_32 nor
   atomic_?mb can be inlined).  Instead, we "inline" them by hand in
   the assembly, meaning there is one function call overhead instead
   of two */
static inline bool opal_atomic_compare_exchange_strong_acq_32(opal_atomic_int32_t *addr,
                                                              int32_t *oldval, int32_t newval)
{
    int32_t prev, tmp;
    bool ret;

    __asm__ __volatile__("1:  ldaxr   %w0, [%2]       \n"
                         "    cmp     %w0, %w3        \n"
                         "    bne     2f              \n"
                         "    stxr    %w1, %w4, [%2]  \n"
                         "    cbnz    %w1, 1b         \n"
                         "2:                          \n"
                         : "=&r"(prev), "=&r"(tmp)
                         : "r"(addr), "r"(*oldval), "r"(newval)
                         : "cc", "memory");

    ret = (prev == *oldval);
    *oldval = prev;
    return ret;
}

static inline bool opal_atomic_compare_exchange_strong_rel_32(opal_atomic_int32_t *addr,
                                                              int32_t *oldval, int32_t newval)
{
    int32_t prev, tmp;
    bool ret;

    __asm__ __volatile__("1:  ldxr    %w0, [%2]       \n"
                         "    cmp     %w0, %w3        \n"
                         "    bne     2f              \n"
                         "    stlxr   %w1, %w4, [%2]  \n"
                         "    cbnz    %w1, 1b         \n"
                         "2:                          \n"
                         : "=&r"(prev), "=&r"(tmp)
                         : "r"(addr), "r"(*oldval), "r"(newval)
                         : "cc", "memory");

    ret = (prev == *oldval);
    *oldval = prev;
    return ret;
}

static inline bool opal_atomic_compare_exchange_strong_64(opal_atomic_int64_t *addr,
                                                          int64_t *oldval, int64_t newval)
{
    int64_t prev;
    int tmp;
    bool ret;

    __asm__ __volatile__("1:  ldaxr    %0, [%2]       \n"
                         "    cmp     %0, %3          \n"
                         "    bne     2f              \n"
                         "    stxr    %w1, %4, [%2]   \n"
                         "    cbnz    %w1, 1b         \n"
                         "2:                          \n"
                         : "=&r"(prev), "=&r"(tmp)
                         : "r"(addr), "r"(*oldval), "r"(newval)
                         : "cc", "memory");

    ret = (prev == *oldval);
    *oldval = prev;
    return ret;
}

/* these two functions aren't inlined in the non-gcc case because then
   there would be two function calls (since neither cmpset_64 nor
   atomic_?mb can be inlined).  Instead, we "inline" them by hand in
   the assembly, meaning there is one function call overhead instead
   of two */
static inline bool opal_atomic_compare_exchange_strong_acq_64(opal_atomic_int64_t *addr,
                                                              int64_t *oldval, int64_t newval)
{
    int64_t prev;
    int tmp;
    bool ret;

    __asm__ __volatile__("1:  ldaxr   %0, [%2]        \n"
                         "    cmp     %0, %3          \n"
                         "    bne     2f              \n"
                         "    stxr    %w1, %4, [%2]   \n"
                         "    cbnz    %w1, 1b         \n"
                         "2:                          \n"
                         : "=&r"(prev), "=&r"(tmp)
                         : "r"(addr), "r"(*oldval), "r"(newval)
                         : "cc", "memory");

    ret = (prev == *oldval);
    *oldval = prev;
    return ret;
}

static inline bool opal_atomic_compare_exchange_strong_rel_64(opal_atomic_int64_t *addr,
                                                              int64_t *oldval, int64_t newval)
{
    int64_t prev;
    int tmp;
    bool ret;

    __asm__ __volatile__("1:  ldxr    %0, [%2]        \n"
                         "    cmp     %0, %3          \n"
                         "    bne     2f              \n"
                         "    stlxr   %w1, %4, [%2]   \n"
                         "    cbnz    %w1, 1b         \n"
                         "2:                          \n"
                         : "=&r"(prev), "=&r"(tmp)
                         : "r"(addr), "r"(*oldval), "r"(newval)
                         : "cc", "memory");

    ret = (prev == *oldval);
    *oldval = prev;
    return ret;
}

#include "opal/sys/atomic_impl_ptr_cswap.h"


/**********************************************************************
 *
 * Swap
 *
 *********************************************************************/

static inline int32_t opal_atomic_swap_32(opal_atomic_int32_t *addr, int32_t newval)
{
    int32_t ret, tmp;

    __asm__ __volatile__("1:  ldaxr   %w0, [%2]       \n"
                         "    stlxr   %w1, %w3, [%2]  \n"
                         "    cbnz    %w1, 1b         \n"
                         : "=&r"(ret), "=&r"(tmp)
                         : "r"(addr), "r"(newval)
                         : "cc", "memory");

    return ret;
}

static inline int64_t opal_atomic_swap_64(opal_atomic_int64_t *addr, int64_t newval)
{
    int64_t ret;
    int tmp;

    __asm__ __volatile__("1:  ldaxr   %0, [%2]        \n"
                         "    stlxr   %w1, %3, [%2]   \n"
                         "    cbnz    %w1, 1b         \n"
                         : "=&r"(ret), "=&r"(tmp)
                         : "r"(addr), "r"(newval)
                         : "cc", "memory");

    return ret;
}

#include "opal/sys/atomic_impl_ptr_swap.h"


/**********************************************************************
 *
 * Atomic spinlocks
 *
 *********************************************************************/

#include "opal/sys/atomic_impl_spinlock.h"


/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/

#define OPAL_ASM_MAKE_ATOMIC(type, bits, name, inst, reg)                          \
    static inline type opal_atomic_fetch_##name##_##bits(opal_atomic_##type *addr, \
                                                         type value)               \
    {                                                                              \
        type newval, old;                                                          \
        int32_t tmp;                                                               \
                                                                                   \
        __asm__ __volatile__("1:  ldxr   %" reg "1, [%3]        \n"                \
                             "    " inst "   %" reg "0, %" reg "1, %" reg "4 \n"   \
                             "    stxr   %w2, %" reg "0, [%3]   \n"                \
                             "    cbnz   %w2, 1b         \n"                       \
                             : "=&r"(newval), "=&r"(old), "=&r"(tmp)               \
                             : "r"(addr), "r"(value)                               \
                             : "cc", "memory");                                    \
                                                                                   \
        return old;                                                                \
    }                                                                              \
    static inline type opal_atomic_##name##_fetch_##bits(opal_atomic_##type *addr, \
                                                         type value)               \
    {                                                                              \
        type newval, old;                                                          \
        int32_t tmp;                                                               \
                                                                                   \
        __asm__ __volatile__("1:  ldxr   %" reg "1, [%3]        \n"                \
                             "    " inst "   %" reg "0, %" reg "1, %" reg "4 \n"   \
                             "    stxr   %w2, %" reg "0, [%3]   \n"                \
                             "    cbnz   %w2, 1b         \n"                       \
                             : "=&r"(newval), "=&r"(old), "=&r"(tmp)               \
                             : "r"(addr), "r"(value)                               \
                             : "cc", "memory");                                    \
                                                                                   \
        return newval;                                                             \
    }

OPAL_ASM_MAKE_ATOMIC(int32_t, 32, add, "add", "w")
OPAL_ASM_MAKE_ATOMIC(int32_t, 32, and, "and", "w")
OPAL_ASM_MAKE_ATOMIC(int32_t, 32, or, "orr", "w")
OPAL_ASM_MAKE_ATOMIC(int32_t, 32, xor, "eor", "w")
OPAL_ASM_MAKE_ATOMIC(int32_t, 32, sub, "sub", "w")

OPAL_ASM_MAKE_ATOMIC(int64_t, 64, add, "add", "")
OPAL_ASM_MAKE_ATOMIC(int64_t, 64, and, "and", "")
OPAL_ASM_MAKE_ATOMIC(int64_t, 64, or, "orr", "")
OPAL_ASM_MAKE_ATOMIC(int64_t, 64, xor, "eor", "")
OPAL_ASM_MAKE_ATOMIC(int64_t, 64, sub, "sub", "")

#include "opal/sys/atomic_impl_minmax_math.h"
#include "opal/sys/atomic_impl_size_t_math.h"


#endif /* ! OPAL_SYS_ARCH_ATOMIC_H */
