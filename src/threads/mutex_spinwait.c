#include "threads/mutex.h"
#if defined(OMPI_USE_SPINWAIT)


static void ompi_mutex_construct(ompi_mutex_t* m)
{
    m->m_spinlock = 0;
    m->m_waiting = 0;
    pthread_mutex_init(&m->m_lock, 0);
    pthread_cond_init(&m->m_cond, 0);
}
                                                                                                                   
static void ompi_mutex_destruct(ompi_mutex_t* m)
{
    pthread_mutex_destroy(&m->m_lock);
    pthread_cond_destroy(&m->m_cond);
}

OBJ_CLASS_INSTANCE(
    ompi_mutex_t,
    ompi_object_t,
    ompi_mutex_construct,
    ompi_mutex_destruct
);

#endif

