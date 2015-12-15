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
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
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
    (void)have;               /* just shut up the compiler */
#endif
    return opal_using_threads();
}


/**
 * Use an atomic operation for increment/decrement if opal_using_threads()
 * indicates that threads are in use by the application or library.
 */

#if OMPI_ENABLE_THREAD_MULTIPLE
static inline int32_t
OPAL_THREAD_ADD32(volatile int32_t *addr, int delta)
{
    int32_t ret;

    if (opal_using_threads()) {
        ret = opal_atomic_add_32(addr, delta);
    } else {
        ret = (*addr += delta);
    }

    return ret;
}
#else
static inline int32_t
OPAL_THREAD_ADD32(volatile int32_t *addr, int delta)
{
    int32_t ret;
    ret = (*addr += delta);
    return ret;
}
#endif

#if OPAL_HAVE_ATOMIC_MATH_64
#if OMPI_ENABLE_THREAD_MULTIPLE
static inline int64_t
OPAL_THREAD_ADD64(volatile int64_t *addr, int delta)
{
    int64_t ret;

    if (opal_using_threads()) {
        ret = opal_atomic_add_64(addr, delta);
    } else {
        ret = (*addr += delta);
    }

    return ret;
}
#else
static inline int64_t
OPAL_THREAD_ADD64(volatile int64_t *addr, int delta)
{
    int64_t ret;
    ret = (*addr += delta);
    return ret;
}
#endif
#endif

#if OMPI_ENABLE_THREAD_MULTIPLE
static inline size_t
OPAL_THREAD_ADD_SIZE_T(volatile size_t *addr, int delta)
{
    size_t ret;

    if (opal_using_threads()) {
        ret = opal_atomic_add_size_t(addr, delta);
    } else {
        ret = (*addr += delta);
    }

    return ret;
}
#else
static inline size_t
OPAL_THREAD_ADD_SIZE_T(volatile size_t *addr, int delta)
{
    size_t ret;
    ret = (*addr += delta);
    return ret;
}
#endif

/* BWB: FIX ME: remove if possible */
#define OPAL_CMPSET(x, y, z) ((*(x) == (y)) ? ((*(x) = (z)), 1) : 0)

#if OMPI_ENABLE_THREAD_MULTIPLE
#if OPAL_HAVE_ATOMIC_CMPSET_32
#define OPAL_ATOMIC_CMPSET_32(x, y, z) \
    (opal_using_threads() ? opal_atomic_cmpset_32(x, y, z) : OPAL_CMPSET(x, y, z))
#endif
#if OPAL_HAVE_ATOMIC_CMPSET_64
#define OPAL_ATOMIC_CMPSET_64(x, y, z) \
    (opal_using_threads() ? opal_atomic_cmpset_64(x, y, z) : OPAL_CMPSET(x, y, z))
#endif
#if OPAL_HAVE_ATOMIC_CMPSET_32 || OPAL_HAVE_ATOMIC_CMPSET_64
#define OPAL_ATOMIC_CMPSET(x, y, z) \
    (opal_using_threads() ? opal_atomic_cmpset(x, y, z) : OPAL_CMPSET(x, y, z))
#endif
#else
# if OPAL_HAVE_ATOMIC_CMPSET_32
# define OPAL_ATOMIC_CMPSET_32(x, y, z) OPAL_CMPSET(x, y, z)
#endif
#if OPAL_HAVE_ATOMIC_CMPSET_64
#define OPAL_ATOMIC_CMPSET_64(x, y, z) OPAL_CMPSET(x, y, z)
#endif
#if OPAL_HAVE_ATOMIC_CMPSET_32 || OPAL_HAVE_ATOMIC_CMPSET_64
#define OPAL_ATOMIC_CMPSET(x, y, z) OPAL_CMPSET(x, y, z)
#endif
#endif

#endif /* !defined(OPAL_THREAD_USAGE_H) */
