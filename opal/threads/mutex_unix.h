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

#ifndef  OPAL_MUTEX_UNIX_H
#define  OPAL_MUTEX_UNIX_H 1

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
#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
#endif

#include "opal/class/opal_object.h"
#include "include/sys/atomic.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
struct opal_mutex_t {
    opal_object_t super;
#if OMPI_HAVE_POSIX_THREADS
    pthread_mutex_t m_lock_pthread;
#endif
    opal_atomic_lock_t m_lock_atomic;
};

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(opal_mutex_t);


#if OPAL_HAVE_ATOMIC_SPINLOCKS && OMPI_HAVE_POSIX_THREADS 

/*
 * opal_mutex_*        implemented using pthreads
 * opal_mutex_atomic_* implemented using atomic operations
 */

static inline int opal_mutex_trylock(opal_mutex_t *m)
{
    return pthread_mutex_trylock(&m->m_lock_pthread);
}

static inline void opal_mutex_lock(opal_mutex_t *m)
{
    pthread_mutex_lock(&m->m_lock_pthread);
}

static inline void opal_mutex_unlock(opal_mutex_t *m)
{
    pthread_mutex_unlock(&m->m_lock_pthread);
}


static inline void opal_mutex_atomic_lock(opal_mutex_t *m)
{
    opal_atomic_lock(&m->m_lock_atomic);
}

static inline int opal_mutex_atomic_trylock(opal_mutex_t *m)
{
    return opal_atomic_trylock(&m->m_lock_atomic);
}

static inline void opal_mutex_atomic_unlock(opal_mutex_t *m)
{
    opal_atomic_unlock(&m->m_lock_atomic);
}


#elif OMPI_HAVE_POSIX_THREADS

/*
 * opal_mutex_* and opal_mutex_atomic_* implemented using pthreads
 */

static inline int opal_mutex_trylock(opal_mutex_t *m)
{
    return pthread_mutex_trylock(&m->m_lock_pthread);
}

static inline void opal_mutex_lock(opal_mutex_t *m)
{
    pthread_mutex_lock(&m->m_lock_pthread);
}

static inline void opal_mutex_unlock(opal_mutex_t *m)
{
    pthread_mutex_unlock(&m->m_lock_pthread);
}


static inline int opal_mutex_atomic_trylock(opal_mutex_t *m)
{
    return opal_mutex_trylock(m);
}

static inline void opal_mutex_atomic_lock(opal_mutex_t *m)
{
    opal_mutex_lock(m);
}

static inline void opal_mutex_atomic_unlock(opal_mutex_t *m)
{
    opal_mutex_unlock(m);
}


#elif OPAL_HAVE_ATOMIC_SPINLOCKS

/*
 * opal_mutex_* and opal_mutex_atomic_* implemented using atomic
 * operations
 */

static inline int opal_mutex_trylock(opal_mutex_t *m)
{
    return opal_atomic_trylock(&m->m_lock_atomic);
}

static inline void opal_mutex_lock(opal_mutex_t *m)
{
    opal_atomic_lock(&m->m_lock_atomic);
}

static inline void opal_mutex_unlock(opal_mutex_t *m)
{
    opal_atomic_unlock(&m->m_lock_atomic);
}


static inline int opal_mutex_atomic_trylock(opal_mutex_t *m)
{
    return opal_mutex_trylock(m);
}

static inline void opal_mutex_atomic_lock(opal_mutex_t *m)
{
    opal_mutex_lock(m);
}

static inline void opal_mutex_atomic_unlock(opal_mutex_t *m)
{
    opal_mutex_unlock(m);
}


#else

#error No mutex definition

#endif

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif                          /* OPAL_MUTEX_UNIX_H */
