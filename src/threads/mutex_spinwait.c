#include "lam/threads/mutex.h"
#if defined(LAM_USE_SPINWAIT)


static void lam_mutex_construct(lam_mutex_t* m)
{
    m->m_spinlock = 0;
    m->m_waiting = 0;
    pthread_mutex_init(&m->m_lock, 0);
    pthread_cond_init(&m->m_cond, 0);
}
                                                                                                                   
static void lam_mutex_destruct(lam_mutex_t* m)
{
    pthread_mutex_destroy(&m->m_lock);
    pthread_cond_destroy(&m->m_cond);
}

OBJ_CLASS_INSTANCE(
    lam_mutex_t,
    lam_object_t,
    lam_mutex_construct,
    lam_mutex_destruct
);

#endif

