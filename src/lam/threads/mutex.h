/*
 * $HEADER$
 */

/** @file **/

#ifndef LAM_MUTEX_H_
#define LAM_MUTEX_H_

#include "lam_config.h"

#if defined(USE_SPINWAIT)
#include "lam/threads/mutex_spinwait.h"
#else
#include "lam/threads/mutex_spinlock.h"
#endif

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
 * - whether the LAM or MPI libraries are multi-threaded (Jan 2003:
 *   they're not),
 * - whether configure determined if we have thread support or not
 *
 * MPI_INIT and MPI_INIT_THREAD (specifically, back-end LAM startup
 * functions) invoke lam_set_using_threads() to influence the value of
 * this function, depending on their situation. Some examples:
 *
 * - if configure determined that we do not have threads, then this
 * value will always be false.
 *
 * - if MPI_INIT is invoked, and the lam/mpi libraries are [still]
 * single-threaded, this value will be false.
 *
 * - if MPI_INIT_THREAD is invoked with MPI_THREAD_MULTIPLE, we have
 * thread support, and the final thread level is determined to be
 * MPI_THREAD_MULTIPLE, this value will be true.
 *
 * - if the process is a single-threaded LAM executable (e.g., mpicc),
 * this value will be false.
 *
 * Hence, this function will return false if there is guaranteed to
 * only be one thread in the process.  If there is even the
 * possibility that we may have multiple threads, true will be
 * returned.
 */
static inline bool lam_using_threads(void) 
{ 
    extern bool lam_uses_threads;
    return lam_uses_threads; 
}


/**
 * Lock a mutex if lam_using_threads() says that multiple threads may
 * be active in the process for the duration of the specified action.
 *
 * @param mutex    Pointer to a lam_mutex_t to lock.
 * @param action   A scope over which the lock is held.
 *
 * If there is a possibility that multiple threads are running in the
 * process (as determined by lam_using_threads()), this function will
 * acquire the lock before invoking the specified action and release
 * it on return.
 *
 * If there is no possibility that multiple threads are running in the
 * process, invoke the action without acquiring the lock.
 */
#define THREAD_SCOPED_LOCK(mutex,action) \
	if(lam_use_threads()) { \
            lam_mutex_lock(mutex); \
            (action); \
            lam_mutex_unlock(mutex); \
        } else { \
            (action); \
        }

/**
 * Set whether the process is using multiple threads or not.
 *
 * @param have Boolean indicating whether the process is using
 * multiple threads or not.
 *
 * @retval lam_using_threads The new return value from
 * lam_using_threads().
 *
 * This function is used to influence the return value of
 * lam_using_threads().  If configure detected that we have thread
 * support, the return value of future invocations of
 * lam_using_threads() will be the parameter's value.  If configure
 * detected that we have no thread support, then the retuen from
 * lam_using_threads() will always be false.
 */
static inline bool lam_set_using_threads(bool have) 
{ 
    extern bool lam_uses_threads;
#if LAM_HAVE_THREADS
    lam_uses_threads = have;
#else
    lam_uses_threads = false;
#endif
    return lam_uses_threads; 
}

/**
 * Lock a mutex if lam_using_threads() says that multiple threads may
 * be active in the process.
 *
 * @param mutex Pointer to a lam_mutex_t to lock.
 *
 * If there is a possibility that multiple threads are running in the
 * process (as determined by lam_using_threads()), this function will
 * block waiting to lock the mutex.
 *
 * If there is no possibility that multiple threads are running in the
 * process, return immediately.
 */
#define THREAD_LOCK(a)   if (lam_using_threads()) \
                              lam_mutex_lock((a));

/*
 * Unlock a mutex if lam_using_threads() says that multiple threads
 * may be active in the process.
 *
 * @param mutex Pointer to a lam_mutex_t to unlock.
 *
 * If there is a possibility that multiple threads are running in the
 * process (as determined by lam_using_threads()), this function will
 * unlock the mutex.
 *
 * If there is no possibility that multiple threads are running in the
 * process, return immediately without modifying the mutex.
 */
#define THREAD_UNLOCK(a) if (lam_using_threads()) \
                              lam_mutex_unlock((a));

/**
 * Always locks a mutex (never compile- or run-time removed)
 *
 * @param mutex A pointer to a lam_mutex_t.
 *
 * Locks the mutex.  This is the macro that you should use for mutexes
 * that should always be locked, regardless of whether the process has
 * multiple threads or not.  This is useful, for example, with shared
 * memory.
 */
#define LOCK(a)          lam_mutex_lock((a))

/**
 * Always unlocks a mutex (never compile- or run-time removed)
 *
 * @param mutex A pointer to a lam_mutex_t.
 *
 * Unlocks the mutex.  This is the macro that you should use for
 * mutexes that should always be unlocked, regardless of whether the
 * process has multiple threads or not.  This is useful, for example,
 * with shared memory.
 */
#define UNLOCK(a)        lam_mutex_unlock((a));

#endif

