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
 * Copyright (c) 2010-2021 IBM Corporation.  All rights reserved.
 * Copyright (c) 2015-2018 Los Alamos National Security, LLC. All rights
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

/*
 * On powerpc ...
 */

#include "opal/sys/powerpc/atomic_helper.h"

/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/

static inline void opal_atomic_mb(void)
{
    __asm__ __volatile__("sync" : : : "memory");
}

static inline void opal_atomic_rmb(void)
{
    __asm__ __volatile__ ("isync" : : : "memory");
}

static inline void opal_atomic_wmb(void)
{
    __asm__ __volatile__("lwsync" : : : "memory");
}

static inline void opal_atomic_isync(void)
{
    __asm__ __volatile__("isync" : : : "memory");
}


/**********************************************************************
 *
 * Compare and Swap
 *
 *********************************************************************/

static inline bool opal_atomic_compare_exchange_strong_32(opal_atomic_int32_t *addr,
                                                          int32_t *oldval, int32_t newval)
{
    int32_t prev;
    bool ret;

    __asm__ __volatile__("1: lwarx   %0, 0, %2  \n\t"
                         "   cmpw    0, %0, %3  \n\t"
                         "   bne-    2f         \n\t"
                         "   stwcx.  %4, 0, %2  \n\t"
                         "   bne-    1b         \n\t"
                         "2:"
                         : "=&r"(prev), "=m"(*addr)
                         : "r" OPAL_ASM_ADDR(addr), "r"(*oldval), "r"(newval), "m"(*addr)
                         : "cc", "memory");

    ret = (prev == *oldval);
    *oldval = prev;
    return ret;
}

static inline bool opal_atomic_compare_exchange_strong_acq_32(opal_atomic_int32_t *addr,
                                                              int32_t *oldval, int32_t newval)
{
    bool rc;

    rc = opal_atomic_compare_exchange_strong_32(addr, oldval, newval);
    opal_atomic_rmb();

    return rc;
}

static inline bool opal_atomic_compare_exchange_strong_rel_32(opal_atomic_int32_t *addr,
                                                              int32_t *oldval, int32_t newval)
{
    opal_atomic_wmb();
    return opal_atomic_compare_exchange_strong_32(addr, oldval, newval);
}


static inline bool opal_atomic_compare_exchange_strong_64(opal_atomic_int64_t *addr,
                                                          int64_t *oldval, int64_t newval)
{
    int64_t prev;
    bool ret;

    __asm__ __volatile__("1: ldarx   %0, 0, %2  \n\t"
                         "   cmpd    0, %0, %3  \n\t"
                         "   bne-    2f         \n\t"
                         "   stdcx.  %4, 0, %2  \n\t"
                         "   bne-    1b         \n\t"
                         "2:"
                         : "=&r"(prev), "=m"(*addr)
                         : "r"(addr), "r"(OPAL_ASM_VALUE64(*oldval)), "r"(OPAL_ASM_VALUE64(newval)),
                           "m"(*addr)
                         : "cc", "memory");

    ret = (prev == *oldval);
    *oldval = prev;
    return ret;
}

static inline bool opal_atomic_compare_exchange_strong_acq_64(opal_atomic_int64_t *addr,
                                                              int64_t *oldval, int64_t newval)
{
    bool rc;

    rc = opal_atomic_compare_exchange_strong_64(addr, oldval, newval);
    opal_atomic_rmb();

    return rc;
}

static inline bool opal_atomic_compare_exchange_strong_rel_64(opal_atomic_int64_t *addr,
                                                              int64_t *oldval, int64_t newval)
{
    opal_atomic_wmb();
    return opal_atomic_compare_exchange_strong_64(addr, oldval, newval);
}

#include "opal/sys/atomic_impl_ptr_cswap.h"


/**********************************************************************
 *
 * Swap
 *
 *********************************************************************/

static inline int32_t opal_atomic_swap_32(opal_atomic_int32_t *addr, int32_t newval)
{
    int32_t ret;

    __asm__ __volatile__("1: lwarx   %0, 0, %2  \n\t"
                         "   stwcx.  %3, 0, %2  \n\t"
                         "   bne-    1b         \n\t"
                         : "=&r"(ret), "=m"(*addr)
                         : "r"(addr), "r"(newval)
                         : "cc", "memory");

    return ret;
}

static inline int64_t opal_atomic_swap_64(opal_atomic_int64_t *addr, int64_t newval)
{
    int64_t ret;

    __asm__ __volatile__("1: ldarx   %0, 0, %2  \n\t"
                         "   stdcx.  %3, 0, %2  \n\t"
                         "   bne-    1b         \n\t"
                         : "=&r"(ret), "=m"(*addr)
                         : "r"(addr), "r"(OPAL_ASM_VALUE64(newval))
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

#define OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_32(type, instr)                                \
    static inline int32_t opal_atomic_fetch_##type##_32(opal_atomic_int32_t *v, int val) \
    {                                                                                    \
        int32_t newval, old;                                                             \
                                                                                         \
        __asm__ __volatile__("1:   lwarx   %1, 0, %4    \n\t"                            \
                             "     " #instr "     %0, %3, %1   \n\t"                     \
                             "     stwcx.  %0, 0, %4    \n\t"                            \
                             "     bne-    1b           \n\t"                            \
                             : "=&r"(newval), "=&r"(old), "=m"(*v)                       \
                             : "r"(val), "r" OPAL_ASM_ADDR(v), "m"(*v)                   \
                             : "cc");                                                    \
                                                                                         \
        return old;                                                                      \
    }                                                                                    \
    static inline int32_t opal_atomic_##type##_fetch_32(opal_atomic_int32_t *v, int val) \
    {                                                                                    \
        int32_t newval, old;                                                             \
                                                                                         \
        __asm__ __volatile__("1:   lwarx   %1, 0, %4    \n\t"                            \
                             "     " #instr "     %0, %3, %1   \n\t"                     \
                             "     stwcx.  %0, 0, %4    \n\t"                            \
                             "     bne-    1b           \n\t"                            \
                             : "=&r"(newval), "=&r"(old), "=m"(*v)                       \
                             : "r"(val), "r" OPAL_ASM_ADDR(v), "m"(*v)                   \
                             : "cc");                                                    \
                                                                                         \
        return newval;                                                                   \
    }

OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_32(add, add)
OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_32(and, and)
OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_32(or, or)
OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_32(xor, xor)
OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_32(sub, subf)

#define OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_64(type, instr)                                    \
    static inline int64_t opal_atomic_fetch_##type##_64(opal_atomic_int64_t *v, int64_t val) \
    {                                                                                        \
        int64_t newval, old;                                                                 \
                                                                                             \
        __asm__ __volatile__("1:   ldarx   %1, 0, %4    \n\t"                                \
                             "     " #instr "     %0, %3, %1   \n\t"                         \
                             "     stdcx.  %0, 0, %4    \n\t"                                \
                             "     bne-    1b           \n\t"                                \
                             : "=&r"(newval), "=&r"(old), "=m"(*v)                           \
                             : "r"(OPAL_ASM_VALUE64(val)), "r" OPAL_ASM_ADDR(v), "m"(*v)     \
                             : "cc");                                                        \
                                                                                             \
        return old;                                                                          \
    }                                                                                        \
    static inline int64_t opal_atomic_##type##_fetch_64(opal_atomic_int64_t *v, int64_t val) \
    {                                                                                        \
        int64_t newval, old;                                                                 \
                                                                                             \
        __asm__ __volatile__("1:   ldarx   %1, 0, %4    \n\t"                                \
                             "     " #instr "     %0, %3, %1   \n\t"                         \
                             "     stdcx.  %0, 0, %4    \n\t"                                \
                             "     bne-    1b           \n\t"                                \
                             : "=&r"(newval), "=&r"(old), "=m"(*v)                           \
                             : "r"(OPAL_ASM_VALUE64(val)), "r" OPAL_ASM_ADDR(v), "m"(*v)     \
                             : "cc");                                                        \
                                                                                             \
        return newval;                                                                       \
    }

OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_64(add, add)
OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_64(and, and)
OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_64(or, or)
OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_64(xor, xor)
OPAL_ATOMIC_POWERPC_DEFINE_ATOMIC_64(sub, subf)

#include "opal/sys/atomic_impl_minmax_math.h"
#include "opal/sys/atomic_impl_size_t_math.h"

#endif /* ! OPAL_SYS_ARCH_ATOMIC_H */
