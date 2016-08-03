/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2016 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#if !defined(OPAL_THREAD_USAGE_H)
#define OPAL_THREAD_USAGE_H

#include "opal/sys/atomic.h"
#include "opal/prefetch.h"
#include "opal_config.h"

/**
 * Check and see if the process is using multiple threads.
 *
 * @retval true If the process may have more than one thread.
 * @retval false If the process only has a single thread.
 *
 * The value that this function returns is influenced by:
 *
 * - how MPI_INIT or MPI_INIT_THREAD was invoked,
 * - what the final MPI thread level was determined to be,
 * - whether the OMPI or MPI libraries are multi-threaded
 *
 * MPI_INIT and MPI_INIT_THREAD (specifically, back-end OMPI startup
 * functions) invoke opal_set_using_threads() to influence the value of
 * this function, depending on their situation. Some examples:
 *
 * - if MPI_INIT is invoked, and the ompi components in use are
 * single-threaded, this value will be false.
 *
 * - if MPI_INIT_THREAD is invoked with MPI_THREAD_MULTIPLE, we have
 * thread support, and the final thread level is determined to be
 * MPI_THREAD_MULTIPLE, this value will be true.
 *
 * - if the process is a single-threaded OMPI executable (e.g., mpicc),
 * this value will be false.
 *
 * Hence, this function will return false if there is guaranteed to
 * only be one thread in the process.  If there is even the
 * possibility that we may have multiple threads, true will be
 * returned.
 */
#if OMPI_ENABLE_THREAD_MULTIPLE

OPAL_DECLSPEC extern bool opal_uses_threads;
#define opal_using_threads()  opal_uses_threads

#else

#define opal_using_threads()  false

#endif /* OMPI_ENABLE_THREAD_MULTIPLE */

/**
 * Set whether the process is using multiple threads or not.
 *
 * @param have Boolean indicating whether the process is using
 * multiple threads or not.
 *
 * @retval opal_using_threads The new return value from
 * opal_using_threads().
 *
 * This function is used to influence the return value of
 * opal_using_threads().  If configure detected that we have thread
 * support, the return value of future invocations of
 * opal_using_threads() will be the parameter's value.  If configure
 * detected that we have no thread support, then the retuen from
 * opal_using_threads() will always be false.
 */
static inline bool opal_set_using_threads(bool have)
{
#if OMPI_ENABLE_THREAD_MULTIPLE
    opal_uses_threads = have;
#else
    have = true;               /* just shut up the compiler */
#endif
    return opal_using_threads();
}


/**
 * Use an atomic operation for increment/decrement if opal_using_threads()
 * indicates that threads are in use by the application or library.
 */
#define OPAL_THREAD_DEFINE_ATOMIC_ADD(type, suffix)     \
static inline type opal_thread_add_ ## suffix (volatile type *addr, type delta) \
{                                                                       \
    if (OPAL_UNLIKELY(opal_using_threads())) {                          \
        return opal_atomic_add_ ## suffix (addr, delta);                \
    }                                                                   \
                                                                        \
    return (*addr += delta);                                            \
}

#define OPAL_THREAD_DEFINE_ATOMIC_CMPSET(type, addr_type, suffix)       \
static inline bool opal_thread_cmpset_bool_ ## suffix (volatile addr_type *addr, type compare, type value) \
{                                                                       \
    if (OPAL_UNLIKELY(opal_using_threads())) {                          \
        return opal_atomic_cmpset_ ## suffix ((volatile type *) addr, compare, value); \
    }                                                                   \
                                                                        \
    if ((type) *addr == compare) {                                      \
        ((type *) addr)[0] = value;                                     \
        return true;                                                    \
    }                                                                   \
                                                                        \
    return false;                                                       \
}

#define OPAL_THREAD_DEFINE_ATOMIC_SWAP(type, addr_type, suffix)         \
static inline type opal_thread_swap_ ## suffix (volatile addr_type *ptr, type newvalue) \
{                                                                       \
    if (opal_using_threads ()) {                                        \
        return opal_atomic_swap_ ## suffix ((volatile type *) ptr, newvalue); \
    }                                                                   \
                                                                        \
    type old = ((type *) ptr)[0];                                       \
    ((type *) ptr)[0] = newvalue;                                       \
                                                                        \
    return old;                                                         \
}

OPAL_THREAD_DEFINE_ATOMIC_ADD(int32_t, 32)
OPAL_THREAD_DEFINE_ATOMIC_ADD(size_t, size_t)
OPAL_THREAD_DEFINE_ATOMIC_CMPSET(int32_t, int32_t, 32)
OPAL_THREAD_DEFINE_ATOMIC_CMPSET(void *, intptr_t, ptr)
OPAL_THREAD_DEFINE_ATOMIC_SWAP(int32_t, int32_t, 32)
OPAL_THREAD_DEFINE_ATOMIC_SWAP(void *, intptr_t, ptr)

#define OPAL_THREAD_ADD32 opal_thread_add_32
#define OPAL_ATOMIC_ADD32 opal_thread_add_32

#define OPAL_THREAD_ADD_SIZE_T opal_thread_add_size_t
#define OPAL_ATOMIC_ADD_SIZE_T opal_thread_add_size_t

#define OPAL_THREAD_CMPSET_32 opal_thread_cmpset_bool_32
#define OPAL_ATOMIC_CMPSET_32 opal_thread_cmpset_bool_32

#define OPAL_THREAD_CMPSET_PTR(x, y, z) opal_thread_cmpset_bool_ptr ((volatile intptr_t *) x, (void *) y, (void *) z)
#define OPAL_ATOMIC_CMPSET_PTR OPAL_THREAD_CMPSET_PTR

#define OPAL_THREAD_SWAP_32 opal_thread_swap_32
#define OPAL_ATOMIC_SWAP_32 opal_thread_swap_32

#define OPAL_THREAD_SWAP_PTR(x, y) opal_thread_swap_ptr ((volatile intptr_t *) x, (void *) y)
#define OPAL_ATOMIC_SWAP_PTR OPAL_THREAD_SWAP_PTR

/* define 64-bit macros is 64-bit atomic math is available */
#if OPAL_HAVE_ATOMIC_MATH_64

OPAL_THREAD_DEFINE_ATOMIC_ADD(int64_t, 64)
OPAL_THREAD_DEFINE_ATOMIC_CMPSET(int64_t, int64_t, 64)
OPAL_THREAD_DEFINE_ATOMIC_SWAP(int64_t, int64_t, 64)

#define OPAL_THREAD_ADD64 opal_thread_add_64
#define OPAL_ATOMIC_ADD64 opal_thread_add_64

#define OPAL_THREAD_CMPSET_64 opal_thread_cmpset_bool_64
#define OPAL_ATOMIC_CMPSET_64 opal_thread_cmpset_bool_64

#define OPAL_THREAD_SWAP_64 opal_thread_swap_64
#define OPAL_ATOMIC_SWAP_64 opal_thread_swap_64

#endif

#endif /* !defined(OPAL_THREAD_USAGE_H) */
