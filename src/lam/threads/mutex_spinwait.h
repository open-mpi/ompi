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
     volatile int     mtx_spinlock;
     volatile int     mtx_waiting;
     pthread_mutex_t  mtx_lock;
     pthread_cond_t   mtx_cond;
} lam_mutex_t;


static inline void lam_mtx_init(lam_mutex_t* m) 
{
    m->mtx_spinlock = 0;
    m->mtx_waiting = 0;
    pthread_mutex_init(&m->mtx_lock, 0);
    pthread_cond_init(&m->mtx_cond, 0);
}


static inline void lam_mtx_lock(lam_mutex_t* m)
{
    unsigned long cnt = 0;
    int locked;

    fetchNadd(&m->mtx_waiting, 1);
    while( ((locked = fetchNset(&m->mtx_spinlock, 1)) == 1)
           && (cnt++ < MUTEX_SPINWAIT) )
        ;
    if(locked) {
        pthread_mutex_lock(&m->mtx_lock);
        while(fetchNset(&m->mtx_spinlock, 1) == 1)
            pthread_cond_wait(&m->mtx_cond, &m->mtx_lock);
        pthread_mutex_unlock(&m->mtx_lock);
    }
    fetchNadd(&m->mtx_waiting, -1);
}


static inline int lam_mtx_trylock(lam_mutex_t* m)
{
    return (fetchNset(&m->mtx_spinlock, 1) == 0);
}


static inline void lam_mtx_unlock(lam_mutex_t* m)
{
    fetchNset(&m->mtx_spinlock, 0); 
    if(m->mtx_waiting) {
        pthread_cond_signal(&m->mtx_cond);
    }
}

#endif

