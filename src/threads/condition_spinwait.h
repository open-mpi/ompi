/* 
 * $HEADER$
 */
#ifndef LAM_CONDITION_SPINWAIT_H
#define LAM_CONDITION_SPINWAIT_H

#include <pthread.h>
#include "threads/mutex.h"


struct lam_condition_t {
    lam_object_t super;
    pthread_cond_t c_cond;
};
typedef struct lam_condition_t lam_condition_t;

OBJ_CLASS_DECLARATION(lam_condition_t);


static inline int lam_condition_wait(lam_condition_t* c, lam_mutex_t* m)
{
    
    int rc;
    pthread_mutex_lock(&m->m_lock);
    /* release the spinlock */
    fetchNset(&m->m_spinlock, 0);
    if(m->m_waiting)
        pthread_cond_signal(&m->m_cond);
    rc = pthread_cond_wait(&c->c_cond, &m->m_lock);
    fetchNset(&m->m_spinlock, 1);
    return rc;
}

static inline int lam_condition_timedwait(lam_condition_t* c, lam_mutex_t* m, const struct timespec *abstime)
{
    int rc;
    pthread_mutex_lock(&m->m_lock);
    /* release the spinlock */
    fetchNset(&m->m_spinlock, 0);
    if(m->m_waiting)
        pthread_cond_signal(&m->m_cond);
    rc = pthread_cond_timedwait(&c->c_cond, &m->m_lock, abstime);
    fetchNset(&m->m_spinlock, 1);
    return rc;
}

static inline int lam_condition_signal(lam_condition_t* c)
{
    return pthread_cond_signal(&c->c_cond);
}

static inline int lam_condition_broadcast(lam_condition_t* c)
{
    return pthread_cond_broadcast(&c->c_cond);
}

#endif

