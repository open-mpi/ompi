/*
 * $HEADER$
 */

#ifndef OMPI_MUTEX_SPINWAIT_
#define OMPI_MUTEX_SPINWAIT_

#include <pthread.h>
#include "class/ompi_object.h"
#include "os/atomic.h"

#ifndef MUTEX_SPINWAIT
#define MUTEX_SPINWAIT 10000
#endif


struct ompi_mutex_t {
     ompi_object_t     super;
     volatile int     m_spinlock;
     volatile int     m_waiting;
     pthread_mutex_t  m_lock;
     pthread_cond_t   m_cond;
};
typedef struct ompi_mutex_t ompi_mutex_t;

OBJ_CLASS_DECLARATION(ompi_mutex_t);



static inline void ompi_mutex_lock(ompi_mutex_t* m)
{
    if(fetchNset(&m->m_spinlock, 1) == 1) {
        unsigned long cnt = 0;
        int locked;
        fetchNadd(&m->m_waiting, 1);
        while( ((locked = fetchNset(&m->m_spinlock, 1)) == 1)
               && (cnt++ < MUTEX_SPINWAIT) )
            ;
        if(locked) {
            pthread_mutex_lock(&m->m_lock);
            while(fetchNset(&m->m_spinlock, 1) == 1)
                pthread_cond_wait(&m->m_cond, &m->m_lock);
            pthread_mutex_unlock(&m->m_lock);
        }
        fetchNadd(&m->m_waiting, -1);
    }
}


static inline int ompi_mutex_trylock(ompi_mutex_t* m)
{
    return (fetchNset(&m->m_spinlock, 1) == 0);
}


static inline void ompi_mutex_unlock(ompi_mutex_t* m)
{
    fetchNset(&m->m_spinlock, 0); 
    if(m->m_waiting) {
        pthread_cond_signal(&m->m_cond);
    }
}

#endif
