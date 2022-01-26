/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserverd.
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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

/*
 * On x86_64, we use cmpxchg.
 */

#define SMPLOCK "lock; "


/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/

static inline void opal_atomic_mb(void)
{
    __asm__ __volatile__("mfence": : :"memory");
}

static inline void opal_atomic_rmb(void)
{
    __asm__ __volatile__("" : : : "memory");
}

static inline void opal_atomic_wmb(void)
{
    __asm__ __volatile__("" : : : "memory");
}

static inline void opal_atomic_isync(void)
{
}


/**********************************************************************
 *
 * Compare and Swap
 *
 *********************************************************************/

static inline bool opal_atomic_compare_exchange_strong_32(opal_atomic_int32_t *addr,
                                                          int32_t *oldval, int32_t newval)
{
    unsigned char ret;
    __asm__ __volatile__(SMPLOCK "cmpxchgl %3,%2   \n\t"
                                 "sete     %0      \n\t"
                         : "=qm"(ret), "+a"(*oldval), "+m"(*addr)
                         : "q"(newval)
                         : "memory", "cc");

    return (bool) ret;
}

#define opal_atomic_compare_exchange_strong_acq_32 opal_atomic_compare_exchange_strong_32
#define opal_atomic_compare_exchange_strong_rel_32 opal_atomic_compare_exchange_strong_32

static inline bool opal_atomic_compare_exchange_strong_64(opal_atomic_int64_t *addr,
                                                          int64_t *oldval, int64_t newval)
{
    unsigned char ret;
    __asm__ __volatile__(SMPLOCK "cmpxchgq %3,%2   \n\t"
                                 "sete     %0      \n\t"
                         : "=qm"(ret), "+a"(*oldval), "+m"(*((opal_atomic_long_t *) addr))
                         : "q"(newval)
                         : "memory", "cc");

    return (bool) ret;
}

#define opal_atomic_compare_exchange_strong_acq_64 opal_atomic_compare_exchange_strong_64
#define opal_atomic_compare_exchange_strong_rel_64 opal_atomic_compare_exchange_strong_64

#include "opal/sys/atomic_impl_ptr_cswap.h"

#if OPAL_HAVE_CMPXCHG16B && HAVE_OPAL_INT128_T

static inline bool opal_atomic_compare_exchange_strong_128(opal_atomic_int128_t *addr,
                                                           opal_int128_t *oldval,
                                                           opal_int128_t newval)
{
    unsigned char ret;

    /* cmpxchg16b compares the value at the address with eax:edx (low:high). if the values are
     * the same the contents of ebx:ecx are stores at the address. in all cases the value stored
     * at the address is returned in eax:edx. */
    __asm__ __volatile__(SMPLOCK "cmpxchg16b (%%rsi)   \n\t"
                                 "sete     %0      \n\t"
                         : "=qm"(ret), "+a"(((int64_t *) oldval)[0]), "+d"(((int64_t *) oldval)[1])
                         : "S"(addr), "b"(((int64_t *) &newval)[0]), "c"(((int64_t *) &newval)[1])
                         : "memory", "cc");

    return (bool) ret;
}

#    define OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_128 1

#endif


/**********************************************************************
 *
 * Swap
 *
 *********************************************************************/

static inline int32_t opal_atomic_swap_32(opal_atomic_int32_t *addr, int32_t newval)
{
    int32_t oldval;

    __asm__ __volatile__("xchg %1, %0" : "=r"(oldval), "+m"(*addr) : "0"(newval) : "memory");
    return oldval;
}

static inline int64_t opal_atomic_swap_64(opal_atomic_int64_t *addr, int64_t newval)
{
    int64_t oldval;

    __asm__ __volatile__("xchgq %1, %0" : "=r"(oldval), "+m"(*addr) : "0"(newval) : "memory");
    return oldval;
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

static inline int32_t opal_atomic_fetch_add_32(opal_atomic_int32_t *v, int i)
{
    int ret = i;
    __asm__ __volatile__(SMPLOCK "xaddl %1,%0" : "+m"(*v), "+r"(ret) : : "memory", "cc");
    return ret;
}

static inline int32_t opal_atomic_add_fetch_32(opal_atomic_int32_t *v, int i)
{
    return opal_atomic_fetch_add_32(v, i) + i;
}

static inline int64_t opal_atomic_fetch_add_64(opal_atomic_int64_t *v, int64_t i)
{
    int64_t ret = i;
    __asm__ __volatile__(SMPLOCK "xaddq %1,%0" : "+m"(*v), "+r"(ret) : : "memory", "cc");
    return ret;
}

static inline int64_t opal_atomic_add_fetch_64(opal_atomic_int64_t *v, int64_t i)
{
    return opal_atomic_fetch_add_64(v, i) + i;
}

static inline int32_t opal_atomic_fetch_sub_32(opal_atomic_int32_t *v, int i)
{
    int ret = -i;
    __asm__ __volatile__(SMPLOCK "xaddl %1,%0" : "+m"(*v), "+r"(ret) : : "memory", "cc");
    return ret;
}

static inline int32_t opal_atomic_sub_fetch_32(opal_atomic_int32_t *v, int i)
{
    return opal_atomic_fetch_sub_32(v, i) - i;
}

static inline int64_t opal_atomic_fetch_sub_64(opal_atomic_int64_t *v, int64_t i)
{
    int64_t ret = -i;
    __asm__ __volatile__(SMPLOCK "xaddq %1,%0" : "+m"(*v), "+r"(ret) : : "memory", "cc");
    return ret;
}

static inline int64_t opal_atomic_fetch_sub_64(opal_atomic_int64_t *v, int64_t i)
{
    return opal_atomic_sub_fetch_64(v, i) - i;
}

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

OPAL_ATOMIC_DEFINE_OP(int32_t, 32, &, and)
OPAL_ATOMIC_DEFINE_OP(int32_t, 32, |, or)
OPAL_ATOMIC_DEFINE_OP(int32_t, 32, ^, xor)

OPAL_ATOMIC_DEFINE_OP(int64_t, 64, &, and)
OPAL_ATOMIC_DEFINE_OP(int64_t, 64, |, or)
OPAL_ATOMIC_DEFINE_OP(int64_t, 64, ^, xor)

#include "opal/sys/atomic_math_minmax_impl.h"
#include "opal/sys/atomic_math_size_t_impl.h"

#endif /* ! OPAL_SYS_ARCH_ATOMIC_H */
