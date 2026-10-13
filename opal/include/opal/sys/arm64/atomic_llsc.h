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
 * Copyright (c) 2026      Stony Brook University. All rights reserved.
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

#    if OPAL_C_GCC_INLINE_ASSEMBLY

#        define OPAL_HAVE_ATOMIC_LLSC_32 1
#        define OPAL_HAVE_ATOMIC_LLSC_64 1

/* Plain LL/SC: no hardware ordering.  Use these when the caller provides
 * its own barriers (opal_atomic_rmb / opal_atomic_wmb) or when no ordering
 * is needed (e.g. a spin-retry that does not dereference the loaded pointer).
 * The "memory" clobber acts as a compiler barrier so that the compiler does
 * not reorder surrounding memory accesses across the asm block. */
#        define opal_atomic_ll_32(addr, ret)                                                      \
            do {                                                                                  \
                opal_atomic_int32_t *_addr = (addr);                                              \
                __asm__ __volatile__("ldxr     %w0, [%1]          \n"                             \
                                     : "=&r"(ret) : "r"(_addr) : "memory");                       \
            } while (0)

#        define opal_atomic_sc_32(addr, newval, ret)                  \
            do {                                                      \
                opal_atomic_int32_t *_addr = (addr);                  \
                int32_t _newval = (int32_t) newval;                   \
                int _ret;                                             \
                                                                      \
                __asm__ __volatile__("stxr     %w0, %w2, [%1]     \n" \
                                     : "=&r"(_ret)                    \
                                     : "r"(_addr), "r"(_newval)       \
                                     : "cc", "memory");               \
                                                                      \
                ret = (_ret == 0);                                    \
            } while (0)

#        define opal_atomic_ll_64(addr, ret)                                                      \
            do {                                                                                  \
                opal_atomic_int64_t *_addr = (addr);                                              \
                __asm__ __volatile__("ldxr     %0, [%1]           \n"                             \
                                     : "=&r"(ret) : "r"(_addr) : "memory");                       \
            } while (0)

#        define opal_atomic_sc_64(addr, newval, ret)                 \
            do {                                                     \
                opal_atomic_int64_t *_addr = (addr);                 \
                int64_t _newval = (int64_t) newval;                  \
                int _ret;                                            \
                                                                     \
                __asm__ __volatile__("stxr     %w0, %2, [%1]      \n" \
                                     : "=&r"(_ret)                   \
                                     : "r"(_addr), "r"(_newval)      \
                                     : "cc", "memory");              \
                                                                     \
                ret = (_ret == 0);                                   \
            } while (0)

/* Acquire-LL: embed acquire ordering in the load so that callers do not need
 * a separate opal_atomic_rmb() between the LL and their first dereference of
 * the loaded pointer.  ldaxr (load-acquire exclusive) prevents subsequent
 * memory accesses from being observed before the load.
 * Use opal_atomic_ll_acq_* when the loaded pointer will be immediately
 * dereferenced (e.g. reading item->opal_list_next in lifo/fifo pop). */
#        define opal_atomic_ll_acq_32(addr, ret)                                                  \
            do {                                                                                  \
                opal_atomic_int32_t *_addr = (addr);                                              \
                __asm__ __volatile__("ldaxr    %w0, [%1]          \n"                             \
                                     : "=&r"(ret) : "r"(_addr) : "memory");                       \
            } while (0)

#        define opal_atomic_ll_acq_64(addr, ret)                                                  \
            do {                                                                                  \
                opal_atomic_int64_t *_addr = (addr);                                              \
                __asm__ __volatile__("ldaxr    %0, [%1]           \n"                             \
                                     : "=&r"(ret) : "r"(_addr) : "memory");                       \
            } while (0)

#include "opal/sys/atomic_impl_ptr_llsc.h"

#    endif /* OPAL_C_GCC_INLINE_ASSEMBLY */

#endif /* ! OPAL_SYS_ARCH_ATOMIC_LLSC_H */
