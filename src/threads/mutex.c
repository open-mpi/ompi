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

#include "ompi_config.h"

#include "threads/mutex.h"

/*
 * Default to a safe value
 */
bool ompi_uses_threads = (bool) OMPI_HAVE_THREADS;


#ifdef WIN32

#include <windows.h>

static void ompi_mutex_construct(ompi_mutex_t *m)
{
    InterlockedExchange(&m->m_lock, 0);
}

static void ompi_mutex_destruct(ompi_mutex_t *m)
{
}

#else

static void ompi_mutex_construct(ompi_mutex_t *m)
{
#if OMPI_HAVE_POSIX_THREADS
    pthread_mutex_init(&m->m_lock_pthread, 0);
#endif
#if OMPI_HAVE_ATOMIC
    ompi_atomic_init( &m->m_lock_atomic, OMPI_ATOMIC_UNLOCKED );
#endif
}

static void ompi_mutex_destruct(ompi_mutex_t *m)
{
#if OMPI_HAVE_POSIX_THREADS
    pthread_mutex_destroy(&m->m_lock_pthread);
#endif
}

#endif

OBJ_CLASS_INSTANCE(ompi_mutex_t,
                   ompi_object_t,
                   ompi_mutex_construct,
                   ompi_mutex_destruct);
