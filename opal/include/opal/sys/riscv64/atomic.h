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
 * Copyright (c) 2016-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2021      Triad National Security, LLC. All rights reserved.
 * Copyright (c) 2021      Google, LLC. All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2025      Software System Team, SANECHIPS.
 *                         All rights reserved.
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
 * RISC-V memory model uses explicit fence instructions.
 * fence rw, rw — full read/write barrier
 * fence r,  r  — read acquire barrier
 * fence w,  w  — write release barrier
 * fence.i      — instruction fence (for isync)
 *
 *********************************************************************/

static inline void opal_atomic_mb(void)
{
    __asm__ __volatile__("fence rw, rw" : : : "memory");
}

static inline void opal_atomic_rmb(void)
{
    __asm__ __volatile__("fence r, r" : : : "memory");
}

static inline void opal_atomic_wmb(void)
{
    __asm__ __volatile__("fence w, w" : : : "memory");
}

static inline void opal_atomic_isync(void)
{
    __asm__ __volatile__("fence.i" : : : "memory");
}


/**********************************************************************
 *
 * Compare and Swap
 *
 * Uses LR/SC (load-reserved / store-conditional) loops.
 * lr.w.aq — load-reserved word with acquire
 * sc.w.rl — store-conditional word with release
 * lr.d.aq / sc.d.rl — doubleword (64-bit) variants
 *
 * RISC-V has no condition-code register, so no "cc" clobber needed.
 *
 *********************************************************************/

static inline bool opal_atomic_compare_exchange_strong_32(opal_atomic_int32_t *addr,
                                                          int32_t *oldval, int32_t newval)
{
    int32_t prev;
    int tmp;
    bool ret;

    __asm__ __volatile__("1:  lr.w.aq  %0, (%2)        \n"
                         "    bne      %0, %3, 2f       \n"
                         "    sc.w.rl  %1, %4, (%2)     \n"
                         "    bnez     %1, 1b           \n"
                         "2:                            \n"
                         : "=&r"(prev), "=&r"(tmp)
                         : "r"(addr), "r"(*oldval), "r"(newval)
                         : "memory");

    ret = (prev == *oldval);
    *oldval = prev;
    return ret;
}

static inline bool opal_atomic_compare_exchange_strong_acq_32(opal_atomic_int32_t *addr,
                                                              int32_t *oldval, int32_t newval)
{
    int32_t prev;
    int tmp;
    bool ret;

    __asm__ __volatile__("1:  lr.w.aq  %0, (%2)        \n"
                         "    bne      %0, %3, 2f       \n"
                         "    sc.w     %1, %4, (%2)     \n"
                         "    bnez     %1, 1b           \n"
                         "2:                            \n"
                         : "=&r"(prev), "=&r"(tmp)
                         : "r"(addr), "r"(*oldval), "r"(newval)
                         : "memory");

    ret = (prev == *oldval);
    *oldval = prev;
    return ret;
}

static inline bool opal_atomic_compare_exchange_strong_rel_32(opal_atomic_int32_t *addr,
                                                              int32_t *oldval, int32_t newval)
{
    int32_t prev;
    int tmp;
    bool ret;

    __asm__ __volatile__("1:  lr.w     %0, (%2)        \n"
                         "    bne      %0, %3, 2f       \n"
                         "    sc.w.rl  %1, %4, (%2)     \n"
                         "    bnez     %1, 1b           \n"
                         "2:                            \n"
                         : "=&r"(prev), "=&r"(tmp)
                         : "r"(addr), "r"(*oldval), "r"(newval)
                         : "memory");

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

    __asm__ __volatile__("1:  lr.d.aq  %0, (%2)        \n"
                         "    bne      %0, %3, 2f       \n"
                         "    sc.d.rl  %1, %4, (%2)     \n"
                         "    bnez     %1, 1b           \n"
                         "2:                            \n"
                         : "=&r"(prev), "=&r"(tmp)
                         : "r"(addr), "r"(*oldval), "r"(newval)
                         : "memory");

    ret = (prev == *oldval);
    *oldval = prev;
    return ret;
}

static inline bool opal_atomic_compare_exchange_strong_acq_64(opal_atomic_int64_t *addr,
                                                              int64_t *oldval, int64_t newval)
{
    int64_t prev;
    int tmp;
    bool ret;

    __asm__ __volatile__("1:  lr.d.aq  %0, (%2)        \n"
                         "    bne      %0, %3, 2f       \n"
                         "    sc.d     %1, %4, (%2)     \n"
                         "    bnez     %1, 1b           \n"
                         "2:                            \n"
                         : "=&r"(prev), "=&r"(tmp)
                         : "r"(addr), "r"(*oldval), "r"(newval)
                         : "memory");

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

    __asm__ __volatile__("1:  lr.d     %0, (%2)        \n"
                         "    bne      %0, %3, 2f       \n"
                         "    sc.d.rl  %1, %4, (%2)     \n"
                         "    bnez     %1, 1b           \n"
                         "2:                            \n"
                         : "=&r"(prev), "=&r"(tmp)
                         : "r"(addr), "r"(*oldval), "r"(newval)
                         : "memory");

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
    int32_t ret;
    int tmp;

    __asm__ __volatile__("1:  lr.w.aq  %0, (%2)        \n"
                         "    sc.w.rl  %1, %3, (%2)     \n"
                         "    bnez     %1, 1b           \n"
                         : "=&r"(ret), "=&r"(tmp)
                         : "r"(addr), "r"(newval)
                         : "memory");

    return ret;
}

static inline int64_t opal_atomic_swap_64(opal_atomic_int64_t *addr, int64_t newval)
{
    int64_t ret;
    int tmp;

    __asm__ __volatile__("1:  lr.d.aq  %0, (%2)        \n"
                         "    sc.d.rl  %1, %3, (%2)     \n"
                         "    bnez     %1, 1b           \n"
                         : "=&r"(ret), "=&r"(tmp)
                         : "r"(addr), "r"(newval)
                         : "memory");

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
 * Uses AMO (Atomic Memory Operation) instructions from the RISC-V A
 * extension.  The AMO instruction atomically applies the operation to
 * memory and returns the *old* value in the destination register.
 *
 * .aqrl suffix provides both acquire and release ordering, equivalent
 * to a sequentially-consistent operation.
 *
 * RISC-V has no amosub, so subtraction negates the value and reuses
 * amoadd.
 *
 *********************************************************************/

/*
 * All AMO instructions return the *old* value.  Each name_fetch variant
 * derives the new value arithmetically from old and the operand.
 * RISC-V has no amosub; subtraction negates the operand and reuses amoadd.
 */

static inline int32_t opal_atomic_fetch_add_32(opal_atomic_int32_t *addr, int32_t value)
{
    int32_t ret;
    __asm__ __volatile__("amoadd.w.aqrl  %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(value) : "memory");
    return ret;
}
static inline int32_t opal_atomic_add_fetch_32(opal_atomic_int32_t *addr, int32_t value)
{
    int32_t ret;
    __asm__ __volatile__("amoadd.w.aqrl  %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(value) : "memory");
    return ret + value;
}

static inline int64_t opal_atomic_fetch_add_64(opal_atomic_int64_t *addr, int64_t value)
{
    int64_t ret;
    __asm__ __volatile__("amoadd.d.aqrl  %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(value) : "memory");
    return ret;
}
static inline int64_t opal_atomic_add_fetch_64(opal_atomic_int64_t *addr, int64_t value)
{
    int64_t ret;
    __asm__ __volatile__("amoadd.d.aqrl  %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(value) : "memory");
    return ret + value;
}

static inline int32_t opal_atomic_fetch_and_32(opal_atomic_int32_t *addr, int32_t value)
{
    int32_t ret;
    __asm__ __volatile__("amoand.w.aqrl  %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(value) : "memory");
    return ret;
}
static inline int32_t opal_atomic_and_fetch_32(opal_atomic_int32_t *addr, int32_t value)
{
    int32_t ret;
    __asm__ __volatile__("amoand.w.aqrl  %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(value) : "memory");
    return ret & value;
}

static inline int32_t opal_atomic_fetch_or_32(opal_atomic_int32_t *addr, int32_t value)
{
    int32_t ret;
    __asm__ __volatile__("amoor.w.aqrl   %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(value) : "memory");
    return ret;
}
static inline int32_t opal_atomic_or_fetch_32(opal_atomic_int32_t *addr, int32_t value)
{
    int32_t ret;
    __asm__ __volatile__("amoor.w.aqrl   %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(value) : "memory");
    return ret | value;
}

static inline int32_t opal_atomic_fetch_xor_32(opal_atomic_int32_t *addr, int32_t value)
{
    int32_t ret;
    __asm__ __volatile__("amoxor.w.aqrl  %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(value) : "memory");
    return ret;
}
static inline int32_t opal_atomic_xor_fetch_32(opal_atomic_int32_t *addr, int32_t value)
{
    int32_t ret;
    __asm__ __volatile__("amoxor.w.aqrl  %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(value) : "memory");
    return ret ^ value;
}

static inline int32_t opal_atomic_fetch_sub_32(opal_atomic_int32_t *addr, int32_t value)
{
    int32_t ret, neg = -value;
    __asm__ __volatile__("amoadd.w.aqrl  %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(neg) : "memory");
    return ret;
}
static inline int32_t opal_atomic_sub_fetch_32(opal_atomic_int32_t *addr, int32_t value)
{
    int32_t ret, neg = -value;
    __asm__ __volatile__("amoadd.w.aqrl  %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(neg) : "memory");
    return ret - value;
}

static inline int64_t opal_atomic_fetch_and_64(opal_atomic_int64_t *addr, int64_t value)
{
    int64_t ret;
    __asm__ __volatile__("amoand.d.aqrl  %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(value) : "memory");
    return ret;
}
static inline int64_t opal_atomic_and_fetch_64(opal_atomic_int64_t *addr, int64_t value)
{
    int64_t ret;
    __asm__ __volatile__("amoand.d.aqrl  %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(value) : "memory");
    return ret & value;
}

static inline int64_t opal_atomic_fetch_or_64(opal_atomic_int64_t *addr, int64_t value)
{
    int64_t ret;
    __asm__ __volatile__("amoor.d.aqrl   %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(value) : "memory");
    return ret;
}
static inline int64_t opal_atomic_or_fetch_64(opal_atomic_int64_t *addr, int64_t value)
{
    int64_t ret;
    __asm__ __volatile__("amoor.d.aqrl   %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(value) : "memory");
    return ret | value;
}

static inline int64_t opal_atomic_fetch_xor_64(opal_atomic_int64_t *addr, int64_t value)
{
    int64_t ret;
    __asm__ __volatile__("amoxor.d.aqrl  %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(value) : "memory");
    return ret;
}
static inline int64_t opal_atomic_xor_fetch_64(opal_atomic_int64_t *addr, int64_t value)
{
    int64_t ret;
    __asm__ __volatile__("amoxor.d.aqrl  %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(value) : "memory");
    return ret ^ value;
}

static inline int64_t opal_atomic_fetch_sub_64(opal_atomic_int64_t *addr, int64_t value)
{
    int64_t ret, neg = -value;
    __asm__ __volatile__("amoadd.d.aqrl  %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(neg) : "memory");
    return ret;
}
static inline int64_t opal_atomic_sub_fetch_64(opal_atomic_int64_t *addr, int64_t value)
{
    int64_t ret, neg = -value;
    __asm__ __volatile__("amoadd.d.aqrl  %0, %2, (%1)  \n"
                         : "=&r"(ret) : "r"(addr), "r"(neg) : "memory");
    return ret - value;
}

#include "opal/sys/atomic_impl_minmax_math.h"
#include "opal/sys/atomic_impl_size_t_math.h"


#endif /* ! OPAL_SYS_ARCH_ATOMIC_H */
