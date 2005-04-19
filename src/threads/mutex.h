/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#ifndef  OMPI_MUTEX_H
#define  OMPI_MUTEX_H 1

#include "ompi_config.h"
#include "include/sys/atomic.h"
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
OMPI_DECLSPEC extern bool ompi_uses_threads;

/**
 * Opaque mutex object
 */
typedef struct ompi_mutex_t ompi_mutex_t;


/**
 * Try to acquire a mutex.
 *
 * @param mutex         Address of the mutex.
 * @return              0 if the mutex was acquired, 1 otherwise.
 */
static inline int ompi_mutex_trylock(ompi_mutex_t *mutex);


/**
 * Acquire a mutex.
 *
 * @param mutex         Address of the mutex.
 */
static inline void ompi_mutex_lock(ompi_mutex_t *mutex);


/**
 * Release a mutex.
 *
 * @param mutex         Address of the mutex.
 */
static inline void ompi_mutex_unlock(ompi_mutex_t *mutex);


/**
 * Try to acquire a mutex using atomic operations.
 *
 * @param mutex         Address of the mutex.
 * @return              0 if the mutex was acquired, 1 otherwise.
 */
static inline int ompi_mutex_atomic_trylock(ompi_mutex_t *mutex);


/**
 * Acquire a mutex using atomic operations.
 *
 * @param mutex         Address of the mutex.
 */
static inline void ompi_mutex_atomic_lock(ompi_mutex_t *mutex);


/**
 * Release a mutex using atomic operations.
 *
 * @param mutex         Address of the mutex.
 */
static inline void ompi_mutex_atomic_unlock(ompi_mutex_t *mutex);


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
 * functions) invoke ompi_set_using_threads() to influence the value of
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
static inline bool ompi_using_threads(void)
{
    return ompi_uses_threads;
}


/**
 * Set whether the process is using multiple threads or not.
 *
 * @param have Boolean indicating whether the process is using
 * multiple threads or not.
 *
 * @retval ompi_using_threads The new return value from
 * ompi_using_threads().
 *
 * This function is used to influence the return value of
 * ompi_using_threads().  If configure detected that we have thread
 * support, the return value of future invocations of
 * ompi_using_threads() will be the parameter's value.  If configure
 * detected that we have no thread support, then the retuen from
 * ompi_using_threads() will always be false.
 */
static inline bool ompi_set_using_threads(bool have)
{
#if OMPI_HAVE_THREAD_SUPPORT
    ompi_uses_threads = have;
#else
    ompi_uses_threads = false;
#endif
    return ompi_uses_threads;
}


/**
 * Lock a mutex if ompi_using_threads() says that multiple threads may
 * be active in the process.
 *
 * @param mutex Pointer to a ompi_mutex_t to lock.
 *
 * If there is a possibility that multiple threads are running in the
 * process (as determined by ompi_using_threads()), this function will
 * block waiting to lock the mutex.
 *
 * If there is no possibility that multiple threads are running in the
 * process, return immediately.
 */

#if OMPI_HAVE_THREAD_SUPPORT
#define OMPI_THREAD_LOCK(mutex)                 \
    do {                                        \
        if (ompi_using_threads()) {             \
            ompi_mutex_lock(mutex);             \
        }                                       \
    } while (0)
#else
#define OMPI_THREAD_LOCK(mutex)
#endif


/**
 * Unlock a mutex if ompi_using_threads() says that multiple threads
 * may be active in the process.
 *
 * @param mutex Pointer to a ompi_mutex_t to unlock.
 *
 * If there is a possibility that multiple threads are running in the
 * process (as determined by ompi_using_threads()), this function will
 * unlock the mutex.
 *
 * If there is no possibility that multiple threads are running in the
 * process, return immediately without modifying the mutex.
 */

#if OMPI_HAVE_THREAD_SUPPORT
#define OMPI_THREAD_UNLOCK(mutex)               \
    do {                                        \
        if (ompi_using_threads()) {             \
            ompi_mutex_unlock(mutex);           \
        }                                       \
    } while (0)
#else
#define OMPI_THREAD_UNLOCK(mutex)
#endif


/**
 * Lock a mutex if ompi_using_threads() says that multiple threads may
 * be active in the process for the duration of the specified action.
 *
 * @param mutex    Pointer to a ompi_mutex_t to lock.
 * @param action   A scope over which the lock is held.
 *
 * If there is a possibility that multiple threads are running in the
 * process (as determined by ompi_using_threads()), this function will
 * acquire the lock before invoking the specified action and release
 * it on return.
 *
 * If there is no possibility that multiple threads are running in the
 * process, invoke the action without acquiring the lock.
 */

#if OMPI_HAVE_THREAD_SUPPORT
#define OMPI_THREAD_SCOPED_LOCK(mutex, action)  \
    do {                                        \
        if(ompi_using_threads()) {              \
            ompi_mutex_lock(mutex);             \
            (action);                           \
            ompi_mutex_unlock(mutex);           \
        } else {                                \
            (action);                           \
        }                                       \
    } while (0)
#else
#define OMPI_THREAD_SCOPED_LOCK(mutex,action) (action)
#endif

/**
 * Use an atomic operation for increment/decrement if ompi_using_threads()
 * indicates that threads are in use by the application or library.
 */

#if OMPI_HAVE_THREAD_SUPPORT
#define OMPI_THREAD_ADD32(x,y) \
   ((OMPI_HAVE_THREAD_SUPPORT && ompi_using_threads()) ? \
   ompi_atomic_add_32(x,y) : (*x += y))
#else
#define OMPI_THREAD_ADD32(x,y) (*x += y)
#endif

#if OMPI_HAVE_THREAD_SUPPORT
#define OMPI_THREAD_ADD64(x,y) \
   ((OMPI_HAVE_THREAD_SUPPORT && ompi_using_threads()) ? \
    ompi_atomic_add_32(x,y) : (*x += y))
#else
#define OMPI_THREAD_ADD64(x,y) (*x += y)
#endif


/**
 * Always locks a mutex (never compile- or run-time removed)
 *
 * @param mutex A pointer to a ompi_mutex_t.
 *
 * Locks the mutex.  This is the macro that you should use for mutexes
 * that should always be locked, regardless of whether the process has
 * multiple threads or not.  This is useful, for example, with shared
 * memory.
 */
#define OMPI_LOCK(mutex)	ompi_mutex_atomic_lock(mutex)

/**
 * Always unlocks a mutex (never compile- or run-time removed)
 *
 * @param mutex A pointer to a ompi_mutex_t.
 *
 * Unlocks the mutex.  This is the macro that you should use for
 * mutexes that should always be unlocked, regardless of whether the
 * process has multiple threads or not.  This is useful, for example,
 * with shared memory.
 */
#define OMPI_UNLOCK(mutex)      ompi_mutex_atomic_unlock(mutex)


/**
 * Lock a mutex for the duration of the specified action.
 *
 * @param mutex    Pointer to a ompi_mutex_t to lock.
 * @param action   A scope over which the lock is held.
 *
 * This is the macro that you should use for mutexes that should
 * always be locked, regardless of whether the process has multiple
 * threads or not.  This is useful, for example, with shared memory.
 */
#define OMPI_SCOPED_LOCK(mutex, action)         \
    do {                                        \
        ompi_mutex_lock(mutex);                 \
        (action);                               \
        ompi_mutex_unlock(mutex);               \
    } while (0)

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#ifdef WIN32
#include "mutex_windows.h"
#else
#include "mutex_unix.h"
#endif

#endif                          /* OMPI_MUTEX_H */
