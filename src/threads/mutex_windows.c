/*
 * $HEADER$
 */

#ifdef HAVE_CONFIG_H
#include "ompi_config.h"
#endif

#include "threads/mutex.h"

static void ompi_mutex_construct(ompi_mutex_t *m)
{
    InterlockedExchange(&m->m_lock, 0);
}

OBJ_CLASS_INSTANCE(ompi_mutex_t,
                   ompi_object_t,
                   ompi_mutex_construct,
                   NULL);
