/*
 * $HEADER$
 */

#ifdef HAVE_CONFIG_H
#include "ompi_config.h"
#endif

#include "threads/mutex.h"

/*
 * Default to a safe value
 */
bool ompi_uses_threads = (bool) OMPI_HAVE_THREADS;


#ifdef __WINDOWS__

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
    pthread_mutex_init(&m->m_lock.thread, 0);
#endif
#if OMPI_SYS_ARCH_ATOMIC_H
    ompi_atomic_unlock(&m->m_lock.atomic);
#endif
}

static void ompi_mutex_destruct(ompi_mutex_t *m)
{
#if OMPI_HAVE_POSIX_THREADS
    pthread_mutex_destroy(&m->m_lock);
#endif
}

#endif

OBJ_CLASS_INSTANCE(ompi_mutex_t,
                   ompi_object_t,
                   ompi_mutex_construct,
                   ompi_mutex_destruct);
