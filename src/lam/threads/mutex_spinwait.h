/*
 * $HEADER$
 */

#ifndef _MUTEX_SPINWAIT_
#define _MUTEX_SPINWAIT_

#include <pthread.h>
#include "os/atomic.h"

#ifndef MUTEX_SPINWAIT
#define MUTEX_SPINWAIT 10000
#endif


typedef struct lam_mutex {
     volatile int     mutex_spinlock;
     volatile int     mutex_waiting;
     pthread_mutex_t  mutex_lock;
     pthread_cond_t   mutex_cond;
} lam_mutex_t;


static inline void lam_mutex_init(lam_mutex_t* m) 
{
    m->mutex_spinlock = 0;
    m->mutex_waiting = 0;
    pthread_mutex_init(&m->mutex_lock, 0);
    pthread_cond_init(&m->mutex_cond, 0);
}


static inline void lam_mutex_lock(lam_mutex_t* m)
{
    unsigned long cnt = 0;
    int locked;

    fetchNadd(&m->mutex_waiting, 1);
    while( ((locked = fetchNset(&m->mutex_spinlock, 1)) == 1)
           && (cnt++ < MUTEX_SPINWAIT) )
        ;
    if(locked) {
        pthread_mutex_lock(&m->mutex_lock);
        while(fetchNset(&m->mutex_spinlock, 1) == 1)
            pthread_cond_wait(&m->mutex_cond, &m->mutex_lock);
        pthread_mutex_unlock(&m->mutex_lock);
    }
    fetchNadd(&m->mutex_waiting, -1);
}


static inline int lam_mutex_trylock(lam_mutex_t* m)
{
    return (fetchNset(&m->mutex_spinlock, 1) == 0);
}


static inline void lam_mutex_unlock(lam_mutex_t* m)
{
    fetchNset(&m->mutex_spinlock, 0); 
    if(m->mutex_waiting) {
        pthread_cond_signal(&m->mutex_cond);
    }
}

#endif

