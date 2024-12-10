/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_INITLOCK_H_INCLUDED
#define MPL_INITLOCK_H_INCLUDED

#include <assert.h>
#include "mplconfig.h"
#include "mpl_atomic.h"

/* Note: this "initlock" does not internally yield, so a thread should not wait
 * for something while taking this lock.  This affects the behavior of
 * non-preemptive threads such as Argobots. */

#if defined(MPL_HAVE_PTHREAD_H)

/* If pthread can be used, let's use pthread_mutex. */
#include <pthread.h>

#define MPL_initlock_t pthread_mutex_t

#define MPL_INITLOCK_INITIALIZER PTHREAD_MUTEX_INITIALIZER

static inline void MPL_initlock_lock(MPL_initlock_t * lock)
{
    int ret ATTRIBUTE((unused)) = pthread_mutex_lock(lock);
    assert(ret == 0);
}

static inline int MPL_initlock_trylock(MPL_initlock_t * lock)
{
    int ret = pthread_mutex_trylock(lock);
    return ret == 0 ? 0 : 1;
}

static inline void MPL_initlock_unlock(MPL_initlock_t * lock)
{
    int ret ATTRIBUTE((unused)) = pthread_mutex_unlock(lock);
    assert(ret == 0);
}

#elif !defined(MPL_USE_NO_ATOMIC_PRIMITIVES)

/* Let's use atomic-based initlock as a fall-back implementation. */
typedef struct MPL_initlock_t {
    MPL_atomic_int_t val;
} MPL_initlock_t;

#define MPL_INITLOCK_INITIALIZER { MPL_ATOMIC_INT_T_INITIALIZER(0) }

static inline void MPL_initlock_lock(MPL_initlock_t * lock)
{
    while (MPL_atomic_cas_int(&lock->val, 0, 1));
}

static inline int MPL_initlock_trylock(MPL_initlock_t * lock)
{
    /* Return 0 if the lock is successfully taken.  Otherwise, return non-zero.
     * This trylock is strong since MPL_atomic_cas_int is atomically strong. */
    return MPL_atomic_cas_int(&lock->val, 0, 1);
}

static inline void MPL_initlock_unlock(MPL_initlock_t * lock)
{
    assert(MPL_atomic_relaxed_load_int(&lock->val) == 1);
    MPL_atomic_release_store_int(&lock->val, 0);
}

#else

#error "Neither POSIX thread nor atomic primitives is supported."

#endif

#endif /* MPL_INITLOCK_H_INCLUDED */
