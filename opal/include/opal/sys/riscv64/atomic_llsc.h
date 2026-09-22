/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
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
 * This file is included even when C11 or GCC built-in atomics are
 * used for the main atomic operations, which is why we check for
 * gcc inline assembly support explicitly.
 *
 * NOTE: The LR/SC loops in opal_lifo_pop_atomic / opal_fifo_pop_atomic
 * load item->opal_list_next between the LR and SC instructions.  That
 * load is a memory access not permitted inside a RISC-V "constrained
 * LR/SC loop" (ISA spec A-extension §8.3), so the architecturally
 * guaranteed forward-progress property does not apply.  All known
 * RISC-V implementations handle this correctly, matching the behaviour
 * of ARM64 (ldxr/stxr) and PowerPC (lwarx/stwcx.) where the same
 * split-macro pattern is used and intermediate loads are likewise
 * permitted by those architectures' respective ISA specifications.
 */

#if OPAL_C_GCC_INLINE_ASSEMBLY

#define OPAL_HAVE_ATOMIC_LLSC_32 1
#define OPAL_HAVE_ATOMIC_LLSC_64 1

/* Plain LL/SC: no hardware ordering.  Use these when the caller provides
 * its own barriers (opal_atomic_rmb / opal_atomic_wmb) or when no ordering
 * is needed (e.g. a spin-retry that does not dereference the loaded pointer).
 * The "memory" clobber acts as a compiler barrier so that the compiler does
 * not reorder surrounding memory accesses across the asm block. */

#define opal_atomic_ll_32(addr, ret)                                         \
    do {                                                                     \
        opal_atomic_int32_t *_addr = (addr);                                 \
        __asm__ __volatile__("lr.w %0, (%1)"                                 \
                             : "=&r"(ret) : "r"(_addr) : "memory");          \
    } while (0)

#define opal_atomic_sc_32(addr, newval, ret)                                 \
    do {                                                                     \
        opal_atomic_int32_t *_addr = (addr);                                 \
        int32_t _newval = (int32_t)(newval);                                 \
        int32_t _ret;                                                        \
        __asm__ __volatile__("sc.w %0, %2, (%1)"                             \
                             : "=&r"(_ret)                                   \
                             : "r"(_addr), "r"(_newval)                      \
                             : "memory");                                    \
        ret = (_ret == 0);                                                   \
    } while (0)

#define opal_atomic_ll_64(addr, ret)                                         \
    do {                                                                     \
        opal_atomic_int64_t *_addr = (addr);                                 \
        __asm__ __volatile__("lr.d %0, (%1)"                                 \
                             : "=&r"(ret) : "r"(_addr) : "memory");          \
    } while (0)

#define opal_atomic_sc_64(addr, newval, ret)                                 \
    do {                                                                     \
        opal_atomic_int64_t *_addr = (addr);                                 \
        int64_t _newval = (int64_t)(newval);                                 \
        int32_t _ret;                                                        \
        __asm__ __volatile__("sc.d %0, %2, (%1)"                             \
                             : "=&r"(_ret)                                   \
                             : "r"(_addr), "r"(_newval)                      \
                             : "memory");                                    \
        ret = (_ret == 0);                                                   \
    } while (0)

/* Acquire-LL: embed acquire ordering in the load so that callers do not need
 * a separate opal_atomic_rmb() between the LL and their first dereference of
 * the loaded pointer.  lr.w.aq / lr.d.aq prevent subsequent memory accesses
 * from being observed before the load.
 * Use opal_atomic_ll_acq_* when the loaded pointer will be immediately
 * dereferenced (e.g. reading item->opal_list_next in lifo/fifo pop). */

#define opal_atomic_ll_acq_32(addr, ret)                                     \
    do {                                                                     \
        opal_atomic_int32_t *_addr = (addr);                                 \
        __asm__ __volatile__("lr.w.aq %0, (%1)"                              \
                             : "=&r"(ret) : "r"(_addr) : "memory");          \
    } while (0)

#define opal_atomic_ll_acq_64(addr, ret)                                     \
    do {                                                                     \
        opal_atomic_int64_t *_addr = (addr);                                 \
        __asm__ __volatile__("lr.d.aq %0, (%1)"                              \
                             : "=&r"(ret) : "r"(_addr) : "memory");          \
    } while (0)

#include "opal/sys/atomic_impl_ptr_llsc.h"

#endif /* OPAL_C_GCC_INLINE_ASSEMBLY */

#endif /* ! OPAL_SYS_ARCH_ATOMIC_LLSC_H */
