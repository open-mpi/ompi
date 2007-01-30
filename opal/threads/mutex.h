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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef  OPAL_MUTEX_H
#define  OPAL_MUTEX_H 1

#include "opal_config.h"
#if OMPI_HAVE_THREAD_SUPPORT
#include "opal/sys/atomic.h"
#endif  /* OMPI_HAVE_THREAD_SUPPORT */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * @file:
 *
 * Mutual exclusion functions.
 *
 * Functions for locking of critical sections.
 */
/*
 * declaring this here so that CL does not complain
 */ 
OPAL_DECLSPEC extern bool opal_uses_threads;

/**
 * Opaque mutex object
 */
typedef struct opal_mutex_t opal_mutex_t;


/**
 * Try to acquire a mutex.
 *
 * @param mutex         Address of the mutex.
 * @return              0 if the mutex was acquired, 1 otherwise.
 */
static inline int opal_mutex_trylock(opal_mutex_t *mutex);


/**
 * Acquire a mutex.
 *
 * @param mutex         Address of the mutex.
 */
static inline void opal_mutex_lock(opal_mutex_t *mutex);


/**
 * Release a mutex.
 *
 * @param mutex         Address of the mutex.
 */
static inline void opal_mutex_unlock(opal_mutex_t *mutex);


/**
 * Try to acquire a mutex using atomic operations.
 *
 * @param mutex         Address of the mutex.
 * @return              0 if the mutex was acquired, 1 otherwise.
 */
static inline int opal_mutex_atomic_trylock(opal_mutex_t *mutex);


/**
 * Acquire a mutex using atomic operations.
 *
 * @param mutex         Address of the mutex.
 */
static inline void opal_mutex_atomic_lock(opal_mutex_t *mutex);


/**
 * Release a mutex using atomic operations.
 *
 * @param mutex         Address of the mutex.
 */
static inline void opal_mutex_atomic_unlock(opal_mutex_t *mutex);


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
 * - whether the OMPI or MPI libraries are multi-threaded (Jan 2003:
 *   they're not),
 * - whether configure determined if we have thread support or not
 *
 * MPI_INIT and MPI_INIT_THREAD (specifically, back-end OMPI startup
 * functions) invoke opal_set_using_threads() to influence the value of
 * this function, depending on their situation. Some examples:
 *
 * - if configure determined that we do not have threads, then this
 * value will always be false.
 *
 * - if MPI_INIT is invoked, and the ompi libraries are [still]
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
static inline bool opal_using_threads(void)
{
    return opal_uses_threads;
}


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
#if OMPI_HAVE_THREAD_SUPPORT
    opal_uses_threads = have;
#else
    have = have;               /* just shut up the compiler */
    opal_uses_threads = false;
#endif
    return opal_uses_threads;
}


/**
 * Lock a mutex if opal_using_threads() says that multiple threads may
 * be active in the process.
 *
 * @param mutex Pointer to a opal_mutex_t to lock.
 *
 * If there is a possibility that multiple threads are running in the
 * process (as determined by opal_using_threads()), this function will
 * block waiting to lock the mutex.
 *
 * If there is no possibility that multiple threads are running in the
 * process, return immediately.
 */

#if OMPI_HAVE_THREAD_SUPPORT
#define OPAL_THREAD_LOCK(mutex)                 \
    do {                                        \
        if (opal_using_threads()) {             \
            opal_mutex_lock(mutex);             \
        }                                       \
    } while (0)
#else
#define OPAL_THREAD_LOCK(mutex)
#endif

#if OMPI_HAVE_THREAD_SUPPORT
#define OPAL_THREAD_TRYLOCK(mutex)   (opal_using_threads() ? opal_mutex_trylock(mutex) : 0)
#else
#define OPAL_THREAD_TRYLOCK(mutex)   0
#endif


/**
 * Unlock a mutex if opal_using_threads() says that multiple threads
 * may be active in the process.
 *
 * @param mutex Pointer to a opal_mutex_t to unlock.
 *
 * If there is a possibility that multiple threads are running in the
 * process (as determined by opal_using_threads()), this function will
 * unlock the mutex.
 *
 * If there is no possibility that multiple threads are running in the
 * process, return immediately without modifying the mutex.
 */

#if OMPI_HAVE_THREAD_SUPPORT
#define OPAL_THREAD_UNLOCK(mutex)               \
    do {                                        \
        if (opal_using_threads()) {             \
            opal_mutex_unlock(mutex);           \
        }                                       \
    } while (0)
#else
#define OPAL_THREAD_UNLOCK(mutex)
#endif


/**
 * Lock a mutex if opal_using_threads() says that multiple threads may
 * be active in the process for the duration of the specified action.
 *
 * @param mutex    Pointer to a opal_mutex_t to lock.
 * @param action   A scope over which the lock is held.
 *
 * If there is a possibility that multiple threads are running in the
 * process (as determined by opal_using_threads()), this function will
 * acquire the lock before invoking the specified action and release
 * it on return.
 *
 * If there is no possibility that multiple threads are running in the
 * process, invoke the action without acquiring the lock.
 */

#if OMPI_HAVE_THREAD_SUPPORT
#define OPAL_THREAD_SCOPED_LOCK(mutex, action)  \
    do {                                        \
        if(opal_using_threads()) {              \
            opal_mutex_lock(mutex);             \
            (action);                           \
            opal_mutex_unlock(mutex);           \
        } else {                                \
            (action);                           \
        }                                       \
    } while (0)
#else
#define OPAL_THREAD_SCOPED_LOCK(mutex,action) (action)
#endif

/**
 * Use an atomic operation for increment/decrement if opal_using_threads()
 * indicates that threads are in use by the application or library.
 */

#if OMPI_HAVE_THREAD_SUPPORT
#define OPAL_THREAD_ADD32(x,y) \
   ((OMPI_HAVE_THREAD_SUPPORT && opal_using_threads()) ? \
   opal_atomic_add_32(x,y) : (*x += y))
#else
#define OPAL_THREAD_ADD32(x,y) (*x += y)
#endif

#if OMPI_HAVE_THREAD_SUPPORT
#define OPAL_THREAD_ADD64(x,y) \
   ((OMPI_HAVE_THREAD_SUPPORT && opal_using_threads()) ? \
    opal_atomic_add_64(x,y) : (*x += y))
#else
#define OPAL_THREAD_ADD64(x,y) (*x += y)
#endif

#if OMPI_HAVE_THREAD_SUPPORT
#define OPAL_THREAD_ADD_SIZE_T(x,y) \
   ((OMPI_HAVE_THREAD_SUPPORT && opal_using_threads()) ? \
    opal_atomic_add_size_t(x,y) : (*x += y))
#else
#define OPAL_THREAD_ADD_SIZE_T(x,y) (*x += y)
#endif


/**
 * Always locks a mutex (never compile- or run-time removed)
 *
 * @param mutex A pointer to a opal_mutex_t.
 *
 * Locks the mutex.  This is the macro that you should use for mutexes
 * that should always be locked, regardless of whether the process has
 * multiple threads or not.  This is useful, for example, with shared
 * memory.
 */
#define OPAL_LOCK(mutex)	opal_mutex_atomic_lock(mutex)

/**
 * Always unlocks a mutex (never compile- or run-time removed)
 *
 * @param mutex A pointer to a opal_mutex_t.
 *
 * Unlocks the mutex.  This is the macro that you should use for
 * mutexes that should always be unlocked, regardless of whether the
 * process has multiple threads or not.  This is useful, for example,
 * with shared memory.
 */
#define OPAL_UNLOCK(mutex)      opal_mutex_atomic_unlock(mutex)


/**
 * Lock a mutex for the duration of the specified action.
 *
 * @param mutex    Pointer to a opal_mutex_t to lock.
 * @param action   A scope over which the lock is held.
 *
 * This is the macro that you should use for mutexes that should
 * always be locked, regardless of whether the process has multiple
 * threads or not.  This is useful, for example, with shared memory.
 */
#define OPAL_SCOPED_LOCK(mutex, action)         \
    do {                                        \
        opal_mutex_lock(mutex);                 \
        (action);                               \
        opal_mutex_unlock(mutex);               \
    } while (0)

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#ifdef __WINDOWS__
#include "mutex_windows.h"
#else
#include "mutex_unix.h"
#endif

#endif                          /* OPAL_MUTEX_H */
