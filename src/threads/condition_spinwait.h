/* 
 * $HEADER$
 */
#ifndef OMPI_CONDITION_SPINWAIT_H
#define OMPI_CONDITION_SPINWAIT_H

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

static inline int ompi_condition_timedwait(ompi_condition_t* c, ompi_mutex_t* m, const struct timespec *abstime)
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

static inline int ompi_condition_signal(ompi_condition_t* c)
{
    return pthread_cond_signal(&c->c_cond);
}

static inline int ompi_condition_broadcast(ompi_condition_t* c)
{
    return pthread_cond_broadcast(&c->c_cond);
}

#endif

