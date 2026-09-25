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
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#ifndef OPAL_SYS_ARCH_ATOMIC_LLSC_H
#define OPAL_SYS_ARCH_ATOMIC_LLSC_H 1

/*
 * this file is included even when C11 or GCC built-in atomics are
 * used, which is why we must check for gcc inline assembly support.
 */

#if OPAL_C_GCC_INLINE_ASSEMBLY

#include "opal/sys/powerpc/atomic_helper.h"

#define OPAL_HAVE_ATOMIC_LLSC_32             1
#define OPAL_HAVE_ATOMIC_LLSC_64             1

/* NTH: the LL/SC support is done through macros due to issues with non-optimized builds. The reason
 * is that even with an always_inline attribute the compiler may still emit instructions to store
 * then load the arguments to/from the stack. This sequence may cause the ll reservation to be
 * cancelled. */

/* The LL macros (lwarx/ldarx) carry no explicit acquire barrier.  The Power ISA
 * acquire idiom requires a conditional branch whose outcome depends on the loaded
 * value followed by isync; because OPAL splits LL and SC into separate macros that
 * are placed inside a caller-written retry loop, that branch occurs in C code outside
 * the asm block, making isync inside the LL asm architecturally incorrect as an acquire.
 * Using lwsync unconditionally after lwarx would be correct but imposes ~30-50 cycles
 * on every iteration -- including failed SC retries -- without benefit, because all
 * current callers (opal_lifo_pop_atomic, opal_fifo_pop_atomic) already issue an
 * explicit opal_atomic_rmb() between the LL and their first pointer dereference.
 * The SC macros (stwcx./stdcx.) likewise carry no lwsync.  The LL/SC pop paths do
 * no stores between LL and SC that need to be made visible before the SC; a release
 * fence is therefore unnecessary, and placing lwsync inside the LL-SC reservation
 * window (between lwarx and stwcx.) would also increase the probability of spurious
 * reservation loss under contention.
 * Both macros retain a "memory" clobber so the compiler treats them as full compiler
 * barriers even without a hardware fence instruction. */
#define opal_atomic_ll_32(addr, ret)                                                        \
    do {                                                                                    \
        opal_atomic_int32_t *_addr = (addr);                                                \
        __asm__ __volatile__("lwarx   %0, 0, %1  \n\t" : "=&r"(ret) : "r"(_addr) : "memory"); \
    } while (0)

#define opal_atomic_sc_32(addr, value, ret)                         \
    do {                                                            \
        opal_atomic_int32_t *_addr = (addr);                        \
        int32_t _ret, _foo, _newval = (int32_t) value;              \
                                                                    \
        __asm__ __volatile__("   stwcx.  %4, 0, %3  \n\t"           \
                             "   li      %0,0       \n\t"           \
                             "   bne-    1f         \n\t"           \
                             "   ori     %0,%0,1    \n\t"           \
                             "1:"                                   \
                             : "=r"(_ret), "=m"(*_addr), "=r"(_foo) \
                             : "r"(_addr), "r"(_newval)             \
                             : "cc", "memory");                     \
        ret = _ret;                                                 \
    } while (0)

#define opal_atomic_ll_64(addr, ret)                                                        \
    do {                                                                                    \
        opal_atomic_int64_t *_addr = (addr);                                                \
        __asm__ __volatile__("ldarx   %0, 0, %1  \n\t" : "=&r"(ret) : "r"(_addr) : "memory"); \
    } while (0)

#define opal_atomic_sc_64(addr, value, ret)                               \
    do {                                                                  \
        opal_atomic_int64_t *_addr = (addr);                              \
        int64_t _newval = (int64_t) value;                                \
        int32_t _ret;                                                     \
                                                                          \
        __asm__ __volatile__("   stdcx.  %2, 0, %1  \n\t"                 \
                             "   li      %0,0       \n\t"                 \
                             "   bne-    1f         \n\t"                 \
                             "   ori     %0,%0,1    \n\t"                 \
                             "1:"                                         \
                             : "=r"(_ret)                                 \
                             : "r"(_addr), "r"(OPAL_ASM_VALUE64(_newval)) \
                             : "cc", "memory");                           \
        ret = _ret;                                                       \
    } while (0)

#include "opal/sys/atomic_impl_ptr_llsc.h"

#endif /* OPAL_C_GCC_INLINE_ASSEMBLY */

#endif /* OPAL_SYS_ARCH_ATOMIC_LLSC_H */
