/*
 * $HEADER$
 */
#include "threads/mutex.h"
#if OMPI_HAVE_POSIX_THREADS

static void ompi_mutex_construct(ompi_mutex_t* m)
{
    pthread_mutex_init(&m->m_lock, 0);
}
                                                                                                                   
static void ompi_mutex_destruct(ompi_mutex_t* m)
{
    pthread_mutex_destroy(&m->m_lock);
}

OBJ_CLASS_INSTANCE(
    ompi_mutex_t,
    ompi_object_t,
    ompi_mutex_construct,
    ompi_mutex_destruct
);

#endif

