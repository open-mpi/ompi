/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef  OMPI_MUTEX_WINDOWS_H
#define  OMPI_MUTEX_WINDOWS_H 1

/**
 * @file:
 *
 * Mutual exclusion functions: Windows implementation.
 *
 * Functions for locking of critical sections.
 *
 * On Windows, base everything on InterlockedExchange().
 */

#include "class/ompi_object.h"
#include "include/sys/atomic.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
struct ompi_mutex_t {
    ompi_object_t super;
    volatile LONG m_lock;
};

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_mutex_t);


static inline int ompi_mutex_trylock(ompi_mutex_t *m)
{
    return (int) InterlockedExchange(&m->m_lock, 1);
}


static inline void ompi_mutex_lock(ompi_mutex_t *m)
{
    while (InterlockedExchange(&m->m_lock, 1)) {
        while (m->m_lock == 1) {
            /* spin */;
        }
    }
}


static inline void ompi_mutex_unlock(ompi_mutex_t *m)
{
    InterlockedExchange(&m->m_lock, 0);
}


static inline int ompi_mutex_atomic_trylock(ompi_mutex_t *m)
{
    return ompi_mutex_trylock(m);
}


static inline void ompi_mutex_atomic_lock(ompi_mutex_t *m)
{
   ompi_mutex_lock(m);
}


static inline void ompi_mutex_atomic_unlock(ompi_mutex_t *m)
{
    ompi_mutex_unlock(m);
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif  /* OMPI_MUTEX_WINDOWS_H */
