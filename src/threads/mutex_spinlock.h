/*
 * $HEADER$
 */

#ifndef LAM_MUTEX_SPINLOCK_
#define LAM_MUTEX_SPINLOCK_

#include "lam/lfc/lam_object.h"
#include "lam/os/atomic.h"


struct lam_mutex_t {
    lam_object_t super;
    lam_lock_data_t m_lock;

};
typedef struct lam_mutex_t lam_mutex_t;


OBJ_CLASS_DECLARATION(lam_mutex_t);


static inline void lam_mutex_lock(lam_mutex_t* m)
{
    spinlock(&m->m_lock);
}

static inline int lam_mutex_trylock(lam_mutex_t* m)
{
    return spintrylock(&m->m_lock);
}

static inline void lam_mutex_unlock(lam_mutex_t* m)
{
    spinunlock(&m->m_lock);
}

#endif
