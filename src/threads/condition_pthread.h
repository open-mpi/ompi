/* 
 * $HEADER$
 */
#ifndef LAM_CONDITION_PTHREAD_H
#define LAM_CONDITION_PTHREAD_H

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
    return pthread_cond_wait(&c->c_cond, &m->m_lock);
}

static inline int lam_condition_timedwait(lam_condition_t* c, lam_mutex_t* m, const struct timespec *abstime)
{
    return pthread_cond_timedwait(&c->c_cond, &m->m_lock, abstime);
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

