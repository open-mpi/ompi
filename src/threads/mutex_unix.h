/*
 * $HEADER$
 */

#ifndef  OMPI_MUTEX_UNIX_H
#define  OMPI_MUTEX_UNIX_H 1

/**
 * @file:
 *
 * Mutual exclusion functions: Windows implementation.
 *
 * Functions for locking of critical sections.
 *
 * On unix, use pthreads or our own atomic operations as
 * available.
 */


#if OMPI_HAVE_POSIX_THREADS
#include <pthread.h>
#endif

#include "class/ompi_object.h"
#include "include/sys/atomic.h"

struct ompi_mutex_t {
    ompi_object_t super;
    union {
#if OMPI_HAVE_POSIX_THREADS
        pthread_mutex_t thread;
#endif
#if OMPI_SYS_ARCH_ATOMIC_H
        ompi_lock_t atomic;
#endif
    } m_lock;
};

OBJ_CLASS_DECLARATION(ompi_mutex_t);


#if defined(OMPI_SYS_ARCH_ATOMIC_H) && defined(OMPI_HAVE_POSIX_THREADS)

/*
 * ompi_mutex_*        implemented using pthreads
 * ompi_mutex_atomic_* implemented using atomic operations
 */

static inline int ompi_mutex_trylock(ompi_mutex_t * m)
{
    return pthread_mutex_trylock(&m->m_lock.thread);
}

static inline void ompi_mutex_lock(ompi_mutex_t * m)
{
    pthread_mutex_lock(&m->m_lock.thread);
}

static inline void ompi_mutex_unlock(ompi_mutex_t * m)
{
    pthread_mutex_unlock(&m->m_lock.thread);
}


static inline void ompi_mutex_atomic_lock(ompi_mutex_t * m)
{
    ompi_atomic_lock(&m->m_lock.atomic);
}

static inline int ompi_mutex_atomic_trylock(ompi_mutex_t * m)
{
    return ompi_atomic_trylock(&m->m_lock.atomic);
}

static inline void ompi_mutex_atomic_unlock(ompi_mutex_t * m)
{
    ompi_atomic_unlock(&m->m_lock.atomic);
}


#elif defined(OMPI_HAVE_POSIX_THREADS)

/*
 * ompi_mutex_* and ompi_mutex_atomic_* implemented using pthreads
 */

static inline int ompi_mutex_trylock(ompi_mutex_t * m)
{
    return pthread_mutex_trylock(&m->m_lock.thread);
}

static inline void ompi_mutex_lock(ompi_mutex_t * m)
{
    pthread_mutex_lock(&m->m_lock.thread);
}

static inline void ompi_mutex_unlock(ompi_mutex_t * m)
{
    pthread_mutex_unlock(&m->m_lock.thread);
}


static inline int ompi_mutex_atomic_trylock(ompi_mutex_t * m)
{
    return ompi_mutex_trylock(m);
}

static inline void ompi_mutex_atomic_lock(ompi_mutex_t * m)
{
    ompi_mutex_lock(m);
}

static inline void ompi_mutex_atomic_unlock(ompi_mutex_t * m)
{
    ompi_mutex_unlock(m);
}


#elif defined(OMPI_SYS_ARCH_ATOMIC_H)

/*
 * ompi_mutex_* and ompi_mutex_atomic_* implemented using atomic
 * operations
 */

static inline int ompi_mutex_trylock(ompi_mutex_t * m)
{
    return ompi_atomic_trylock(&m->m_lock.atomic);
}

static inline void ompi_mutex_lock(ompi_mutex_t * m)
{
    ompi_atomic_lock(&m->m_lock.atomic);
}

static inline void ompi_mutex_unlock(ompi_mutex_t * m)
{
    ompi_atomic_unlock(&m->m_lock.atomic);
}


static inline int ompi_mutex_atomic_trylock(ompi_mutex_t * m)
{
    return ompi_mutex_trylock(m);
}

static inline void ompi_mutex_atomic_lock(ompi_mutex_t * m)
{
    ompi_mutex_lock(m);
}

static inline void ompi_mutex_atomic_unlock(ompi_mutex_t * m)
{
    ompi_mutex_unlock(m);
}


#else

#error No mutex definition

#endif

#endif                          /* OMPI_MUTEX_UNIX_H */
