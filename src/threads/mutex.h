/*
 * $HEADER$
 */

#ifndef  OMPI_MUTEX_H
#define  OMPI_MUTEX_H 1

#ifdef HAVE_CONFIG_H
#include "ompi_config.h"
#endif

/**
 * @file:
 *
 * Mutual exclusion functions.
 *
 * Functions for locking of critical sections.
 */

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
    extern bool ompi_uses_threads;
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
    extern bool ompi_uses_threads;
#if OMPI_HAVE_THREADS
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
#define OMPI_THREAD_LOCK(mutex)                 \
    do {                                        \
        if (ompi_using_threads()) {             \
            ompi_mutex_lock(mutex);             \
        }                                       \
    } while (0)

/*
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
#define OMPI_THREAD_UNLOCK(mutex)               \
    do {                                        \
        if (ompi_using_threads()) {             \
            ompi_mutex_unlock(mutex);           \
        }                                       \
    } while (0)


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


#ifdef __WINDOWS__
#error Windows code is untested
#include "mutex_windows.h"
#else
#include "mutex_unix.h"
#endif

#endif                          /* OMPI_MUTEX_H */
