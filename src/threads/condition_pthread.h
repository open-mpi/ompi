/* 
 * $HEADER$
 */
#ifndef OMPI_CONDITION_PTHREAD_H
#define OMPI_CONDITION_PTHREAD_H

#include <pthread.h>
#include "threads/mutex.h"


struct ompi_condition_t {
    ompi_object_t super;
    pthread_cond_t c_cond;
};
typedef struct ompi_condition_t ompi_condition_t;

OBJ_CLASS_DECLARATION(ompi_condition_t);


static inline int ompi_condition_wait(ompi_condition_t* c, ompi_mutex_t* m)
{
    return pthread_cond_wait(&c->c_cond, &m->m_lock.thread);
}

static inline int ompi_condition_timedwait(ompi_condition_t* c, ompi_mutex_t* m, const struct timespec *abstime)
{
    return pthread_cond_timedwait(&c->c_cond, &m->m_lock.thread, abstime);
}

static inline int ompi_condition_signal(ompi_condition_t* c)
{
    return pthread_cond_signal(&c->c_cond);
}

static inline int ompi_condition_broadcast(ompi_condition_t* c)
{
    return pthread_cond_broadcast(&c->c_cond);
}

#endif

