/*
 * $HEADER$
 */
#include "lam/threads/mutex.h"
#if defined(LAM_USE_PTHREADS)

static void lam_mutex_construct(lam_mutex_t* m)
{
    pthread_mutex_init(&m->m_lock, 0);
}
                                                                                                                   
static void lam_mutex_destruct(lam_mutex_t* m)
{
    pthread_mutex_destroy(&m->m_lock);
}

OBJ_CLASS_INSTANCE(
    lam_mutex_t,
    lam_object_t,
    lam_mutex_construct,
    lam_mutex_destruct
);

#endif

