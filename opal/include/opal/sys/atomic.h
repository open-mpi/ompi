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
 * Copyright (c) 2020-2021 Google, LLC. All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
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
 * Note that for the Atomic math, atomic add/sub may be implemented as
 * C code using opal_atomic_compare_exchange.  The appearance of atomic
 * operation will be upheld in these cases.
 */

#ifndef OPAL_SYS_ATOMIC_H
#define OPAL_SYS_ATOMIC_H 1

#include <stdbool.h>

#include "opal/opal_portable_platform.h"
#include "opal_stdatomic.h"

BEGIN_C_DECLS

/**********************************************************************
 *
 * Memory Barriers
 *
 *********************************************************************/
/**
 * Memory barrier
 *
 * Will use system-specific features to instruct the processor and
 * memory controller that all writes and reads that have been posted
 * before the call to \c opal_atomic_mb() must appear to have
 * completed before the next read or write.
 *
 * \note This can have some expensive side effects, including flushing
 * the pipeline, preventing the cpu from reordering instructions, and
 * generally grinding the memory controller's performance.  Use only
 * if you need *both* read and write barriers.
 */
static inline void opal_atomic_mb(void);

/**
 * Read memory barrier
 *
 * Use system-specific features to instruct the processor and memory
 * conrtoller that all reads that have been posted before the call to
 * \c opal_atomic_rmb() must appear to have been completed before the
 * next read.  Nothing is said about the ordering of writes when using
 * \c opal_atomic_rmb().
 */
static inline void opal_atomic_rmb(void);

/**
 * Write memory barrier.
 *
 * Use system-specific features to instruct the processor and memory
 * conrtoller that all writes that have been posted before the call to
 * \c opal_atomic_wmb() must appear to have been completed before the
 * next write.  Nothing is said about the ordering of reads when using
 * \c opal_atomic_wmb().
 */
static inline void opal_atomic_wmb(void);


/**********************************************************************
 *
 * Compare and Swap
 *
 * Implementations must provide 32 and 64 bit compare-and-swap
 * operations, but may provide the ptr implementation by including
 * atomic_cmpx_ptr_impl.h (which implements the ptr implementation
 * over the 32 and 64 bit implementations).
 *
 *********************************************************************/

/*
 * The stdc implementation is implemetned as macros around the C11
 * atomic interface (which is a type-independent interface).  While it
 * would be better to have type checking so developers using the C11
 * interface didn't accidentally munge something that broke on other
 * implementations, there are a ton of warnings due to volatile casing
 * in the opal_lifo code. Don't enforce the types of the function
 * calls on C11 until we can sort that out.
 */
#if OPAL_USE_C11_ATOMICS == 0

/**
 * Atomic compare and set of 32 bit integers with acquire and release semantics.
 *
 * @param addr          Address of value to be swapped
 * @param oldval        Comparison value
 * @param newval        New value to set if comparison is true
 *
 * @returns If newval was written into addr, the function returns
 * true.  Otherwise, the function returns false and the value of addr
 * at the time of the comparison is returned in oldval.
 */
static inline bool opal_atomic_compare_exchange_strong_32(opal_atomic_int32_t *addr, int32_t *oldval,
                                                          int32_t newval);

/**
 * Atomic compare and set of 32 bit integers with acquire semantics.
 *
 * @param addr          Address of value to be swapped
 * @param oldval        Comparison value
 * @param newval        New value to set if comparison is true
 *
 * @returns If newval was written into addr, the function returns
 * true.  Otherwise, the function returns false and the value of addr
 * at the time of the comparison is returned in oldval.
 */
static inline bool opal_atomic_compare_exchange_strong_acq_32(opal_atomic_int32_t *addr, int32_t *oldval,
                                                              int32_t newval);

/**
 * Atomic compare and set of 32 bit integers with release semantics.
 *
 * @param addr          Address of value to be swapped
 * @param oldval        Comparison value
 * @param newval        New value to set if comparison is true
 *
 * @returns If newval was written into addr, the function returns
 * true.  Otherwise, the function returns false and the value of addr
 * at the time of the comparison is returned in oldval.
 */
static inline bool opal_atomic_compare_exchange_strong_rel_32(opal_atomic_int32_t *addr, int32_t *oldval,
                                                              int32_t newval);

/**
 * Atomic compare and set of 64 bit integers with acquire and release semantics.
 *
 * @param addr          Address of value to be swapped
 * @param oldval        Comparison value
 * @param newval        New value to set if comparison is true
 *
 * @returns If newval was written into addr, the function returns
 * true.  Otherwise, the function returns false and the value of addr
 * at the time of the comparison is returned in oldval.
 */
static inline bool opal_atomic_compare_exchange_strong_64(opal_atomic_int64_t *addr, int64_t *oldval,
                                                          int64_t newval);

/**
 * Atomic compare and set of 64 bit integers with acquire semantics.
 *
 * @param addr          Address of value to be swapped
 * @param oldval        Comparison value
 * @param newval        New value to set if comparison is true
 *
 * @returns If newval was written into addr, the function returns
 * true.  Otherwise, the function returns false and the value of addr
 * at the time of the comparison is returned in oldval.
 */
static inline bool opal_atomic_compare_exchange_strong_acq_64(opal_atomic_int64_t *addr, int64_t *oldval,
                                                              int64_t newval);

/**
 * Atomic compare and set of 64 bit integers with release semantics.
 *
 * @param addr          Address of value to be swapped
 * @param oldval        Comparison value
 * @param newval        New value to set if comparison is true
 *
 * @returns If newval was written into addr, the function returns
 * true.  Otherwise, the function returns false and the value of addr
 * at the time of the comparison is returned in oldval.
 */
static inline bool opal_atomic_compare_exchange_strong_rel_64(opal_atomic_int64_t *addr, int64_t *oldval,
                                                              int64_t newval);

/**
 * Atomic compare and set of pointer-sized integers with acquire and release semantics.
 *
 * @param addr          Address of value to be swapped
 * @param oldval        Comparison value
 * @param newval        New value to set if comparison is true
 *
 * @returns If newval was written into addr, the function returns
 * true.  Otherwise, the function returns false and the value of addr
 * at the time of the comparison is returned in oldval.
 */
static inline bool opal_atomic_compare_exchange_strong_ptr(opal_atomic_intptr_t *addr,
                                                           intptr_t *oldval, intptr_t newval);

/**
 * Atomic compare and set of pointer-sized integers with acquire semantics.
 *
 * @param addr          Address of value to be swapped
 * @param oldval        Comparison value
 * @param newval        New value to set if comparison is true
 *
 * @returns If newval was written into addr, the function returns
 * true.  Otherwise, the function returns false and the value of addr
 * at the time of the comparison is returned in oldval.
 */
static inline bool opal_atomic_compare_exchange_strong_acq_ptr(opal_atomic_intptr_t *addr,
                                                               intptr_t *oldval, intptr_t newval);

/**
 * Atomic compare and set of pointer-sized integers with release semantics.
 *
 * @param addr          Address of value to be swapped
 * @param oldval        Comparison value
 * @param newval        New value to set if comparison is true
 *
 * @returns If newval was written into addr, the function returns
 * true.  Otherwise, the function returns false and the value of addr
 * at the time of the comparison is returned in oldval.
 */
static inline bool opal_atomic_compare_exchange_strong_rel_ptr(opal_atomic_intptr_t *addr,
                                                               intptr_t *oldval, intptr_t newval);

/**********************************************************************
 *
 * Swap
 *
 * Implementations may provide a native implementation of these
 * operations or include atomic_swap_impl.h, which provides
 * implementations over compare-and-swap.
 *
 *********************************************************************/
/**
 * Atomic swap of 32 bit value
 * @param addr           Address of value to be swapped
 * @param newval         New value to set in addr
 *
 * @returns Value in addr before swap
 */
static inline int32_t opal_atomic_swap_32(opal_atomic_int32_t *addr, int32_t newval);

/**
 * Atomic swap of 32 bit value
 * @param addr           Address of value to be swapped
 * @param newval         New value to set in addr
 *
 * @returns Value in addr before swap
 */
static inline int64_t opal_atomic_swap_64(opal_atomic_int64_t *addr, int64_t newval);

/**
 * Atomic swap of 32 bit value
 * @param addr           Address of value to be swapped
 * @param newval         New value to set in addr
 *
 * @returns Value in addr before swap
 */
static inline intptr_t opal_atomic_swap_ptr(opal_atomic_intptr_t *addr, intptr_t newval);

#endif /* #if !(OPAL_ASSEMBLY_BUILTIN == OPAL_BUILTIN_C11 && !defined(__INTEL_COMPILER)) */


/**********************************************************************
 *
 * Atomic spinlocks
 *
 *********************************************************************/
/**
 * Initialize a lock to value
 *
 * @param lock         Address of the lock
 * @param value        Initial value to set lock to
 */
static inline void opal_atomic_lock_init(opal_atomic_lock_t *lock, int32_t value);

/**
 * Try to acquire a lock.
 *
 * @param lock          Address of the lock.
 * @return              0 if the lock was acquired, 1 otherwise.
 */
static inline int opal_atomic_trylock(opal_atomic_lock_t *lock);

/**
 * Acquire a lock by spinning.
 *
 * @param lock          Address of the lock.
 */
static inline void opal_atomic_lock(opal_atomic_lock_t *lock);

/**
 * Release a lock.
 *
 * @param lock          Address of the lock.
 */
static inline void opal_atomic_unlock(opal_atomic_lock_t *lock);


/**********************************************************************
 *
 * Atomic math operations
 *
 *********************************************************************/

static inline int32_t opal_atomic_add_fetch_32(opal_atomic_int32_t *addr, int delta);
static inline int32_t opal_atomic_fetch_add_32(opal_atomic_int32_t *addr, int delta);
static inline int32_t opal_atomic_and_fetch_32(opal_atomic_int32_t *addr, int32_t value);
static inline int32_t opal_atomic_fetch_and_32(opal_atomic_int32_t *addr, int32_t value);
static inline int32_t opal_atomic_or_fetch_32(opal_atomic_int32_t *addr, int32_t value);
static inline int32_t opal_atomic_fetch_or_32(opal_atomic_int32_t *addr, int32_t value);
static inline int32_t opal_atomic_xor_fetch_32(opal_atomic_int32_t *addr, int32_t value);
static inline int32_t opal_atomic_fetch_xor_32(opal_atomic_int32_t *addr, int32_t value);
static inline int32_t opal_atomic_sub_fetch_32(opal_atomic_int32_t *addr, int delta);
static inline int32_t opal_atomic_fetch_sub_32(opal_atomic_int32_t *addr, int delta);
static inline int32_t opal_atomic_min_fetch_32(opal_atomic_int32_t *addr, int32_t value);
static inline int32_t opal_atomic_fetch_min_32(opal_atomic_int32_t *addr, int32_t value);
static inline int32_t opal_atomic_max_fetch_32(opal_atomic_int32_t *addr, int32_t value);
static inline int32_t opal_atomic_fetch_max_32(opal_atomic_int32_t *addr, int32_t value);

static inline int64_t opal_atomic_add_fetch_64(opal_atomic_int64_t *addr, int64_t delta);
static inline int64_t opal_atomic_fetch_add_64(opal_atomic_int64_t *addr, int64_t delta);
static inline int64_t opal_atomic_and_fetch_64(opal_atomic_int64_t *addr, int64_t value);
static inline int64_t opal_atomic_fetch_and_64(opal_atomic_int64_t *addr, int64_t value);
static inline int64_t opal_atomic_or_fetch_64(opal_atomic_int64_t *addr, int64_t value);
static inline int64_t opal_atomic_fetch_or_64(opal_atomic_int64_t *addr, int64_t value);
static inline int64_t opal_atomic_fetch_xor_64(opal_atomic_int64_t *addr, int64_t value);
static inline int64_t opal_atomic_sub_fetch_64(opal_atomic_int64_t *addr, int64_t delta);
static inline int64_t opal_atomic_fetch_sub_64(opal_atomic_int64_t *addr, int64_t delta);
static inline int64_t opal_atomic_min_fetch_64(opal_atomic_int64_t *addr, int64_t value);
static inline int64_t opal_atomic_fetch_min_64(opal_atomic_int64_t *addr, int64_t value);
static inline int64_t opal_atomic_max_fetch_64(opal_atomic_int64_t *addr, int64_t value);
static inline int64_t opal_atomic_fetch_max_64(opal_atomic_int64_t *addr, int64_t value);

static inline size_t opal_atomic_add_fetch_size_t(opal_atomic_size_t *addr, size_t delta);
static inline size_t opal_atomic_fetch_add_size_t(opal_atomic_size_t *addr, size_t delta);

#ifdef DOXYGEN /* because this isn't a proper C prototype */
/**
 * Atomically add delta to addr, type independent
 *
 * @param addr   Address of value to update
 * @param delta  Value by which to change the value in addr
 *
 * Generally implemented as a macro (except for when implemented as a
 * compiler built-in), this function provides a type-independent math
 * operator.
 */
static inline void opal_atomic_add(type *addr, type delta);
#endif


/**********************************************************************
 *
 * Load-linked, Store Conditional
 *
 * Optional.  Check OPAL_HAVE_ATOMIC_LLSC_32,
 * OPAL_HAVE_ATOMIC_LLSC_64, or OPAL_HAVE_ATOMIC_LLSC_PTR before
 * using.  Implemented as macros due to function call behaviors;
 * prototyped here as C++-style functions for readability.
 *
 * C11 and GCC built-in atomics don't provide native LL/SC support, so
 * if there is an architectural implementation, we use it even if
 * we are using the C11 or GCC built-in atomics.
 *
 *********************************************************************/

#ifdef DOXYGEN

static inline void opal_atomic_ll_32(opal_atomic_int32_t *addr, int32_t &ret);

static inline void opal_atomic_sc_32(opal_atomic_int32_t *addr, int32_t newval, int &ret);

static inline void opal_atomic_ll_64(opal_atomic_int64_t *addr, int64_t &ret);

static inline void opal_atomic_sc_64(opal_atomic_int64_t *addr, int64_t newval, int &ret);

static inline void opal_atomic_ll_ptr(opal_atomic_intptr_t *addr, intptr_t &ret);

static inline void opal_atomic_sc_ptr(opal_atomic_intptr_t *addr, intptr_t newval, int &ret);

#endif


/**********************************************************************
 *
 * Load the appropriate architecture files and set some reasonable
 * default values for our support
 *
 *********************************************************************/

#if defined(DOXYGEN)
/* don't include system-level gorp when generating doxygen files */
#elif OPAL_USE_C11_ATOMICS == 1
#    include "opal/sys/atomic_stdc.h"
#elif OPAL_USE_GCC_BUILTIN_ATOMICS == 1
#    include "opal/sys/gcc_builtin/atomic.h"
#elif OPAL_USE_ASM_ATOMICS == 1
#    if defined(PLATFORM_ARCH_X86_64)
#        include "opal/sys/x86_64/atomic.h"
#    elif defined(PLATFORM_ARCH_AARCH64)
#        include "opal/sys/arm64/atomic.h"
#    elif defined(PLATFORM_ARCH_POWERPC) && defined(PLATFORM_ARCH_64)
#        include "opal/sys/powerpc/atomic.h"
#    else
#        error "No asm support found."
#    endif
#else
#error "No atomics support found."
#endif

#if defined(PLATFORM_ARCH_AARCH64)
#    include "opal/sys/arm64/atomic_llsc.h"
#elif defined(PLATFORM_ARCH_POWERPC) && defined(PLATFORM_ARCH_64)
#    include "opal/sys/powerpc/atomic_llsc.h"
#endif


/**********************************************************************
 *
 * Ensure defines for the few optional features are always defined
 *
 *********************************************************************/

#ifndef OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_128
#    define OPAL_HAVE_ATOMIC_COMPARE_EXCHANGE_128 0
#endif

#ifndef OPAL_HAVE_ATOMIC_LLSC_32
#    define OPAL_HAVE_ATOMIC_LLSC_32 0
#endif

#ifndef OPAL_HAVE_ATOMIC_LLSC_64
#    define OPAL_HAVE_ATOMIC_LLSC_64 0
#endif

#ifndef OPAL_HAVE_ATOMIC_LLSC_PTR
#    define OPAL_HAVE_ATOMIC_LLSC_PTR 0
#endif

END_C_DECLS

#endif /* OPAL_SYS_ATOMIC_H */
