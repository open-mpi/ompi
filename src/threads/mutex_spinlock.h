/*
 * $HEADER$
 */

#ifndef OMPI_MUTEX_SPINOMPI_LOCK_
#define OMPI_MUTEX_SPINOMPI_LOCK_

#include "class/ompi_object.h"
#include "os/atomic.h"


struct ompi_mutex_t {
    ompi_object_t super;
    ompi_lock_data_t m_lock;

};
typedef struct ompi_mutex_t ompi_mutex_t;


OBJ_CLASS_DECLARATION(ompi_mutex_t);


static inline void ompi_mutex_lock(ompi_mutex_t* m)
{
    spinlock(&m->m_lock);
}

static inline int ompi_mutex_trylock(ompi_mutex_t* m)
{
    return spintrylock(&m->m_lock);
}

static inline void ompi_mutex_unlock(ompi_mutex_t* m)
{
    spinunlock(&m->m_lock);
}

#endif
