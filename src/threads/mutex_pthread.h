/*
 * $HEADER$
 */

#ifndef _OMPI_MUTEX_PTHREAD_
#define _OMPI_MUTEX_PTHREAD_

#include <pthread.h>
#include "class/ompi_object.h"
#include "include/sys/atomic.h"


struct ompi_mutex_t {
     ompi_object_t     super;
     pthread_mutex_t  m_lock;
};
typedef struct ompi_mutex_t ompi_mutex_t;

OBJ_CLASS_DECLARATION(ompi_mutex_t);



static inline void ompi_mutex_lock(ompi_mutex_t* m)
{
    pthread_mutex_lock(&m->m_lock);
}


static inline int ompi_mutex_trylock(ompi_mutex_t* m)
{
    return pthread_mutex_trylock(&m->m_lock);
}


static inline void ompi_mutex_unlock(ompi_mutex_t* m)
{
    pthread_mutex_unlock(&m->m_lock);
}

#endif
