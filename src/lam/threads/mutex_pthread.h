/*
 * $HEADER$
 */

#ifndef _LAM_MUTEX_PTHREAD_
#define _LAM_MUTEX_PTHREAD_

#include <pthread.h>
#include "lam/lfc/lam_object.h"
#include "lam/os/atomic.h"


struct lam_mutex_t {
     lam_object_t     super;
     pthread_mutex_t  m_lock;
};
typedef struct lam_mutex_t lam_mutex_t;

OBJ_CLASS_DECLARATION(lam_mutex_t);



static inline void lam_mutex_lock(lam_mutex_t* m)
{
    pthread_mutex_lock(&m->m_lock);
}


static inline int lam_mutex_trylock(lam_mutex_t* m)
{
    return pthread_mutex_trylock(&m->m_lock);
}


static inline void lam_mutex_unlock(lam_mutex_t* m)
{
    pthread_mutex_unlock(&m->m_lock);
}

#endif
