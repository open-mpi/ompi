/*
 * $HEADER$
 */

#include "mutex.h"
#if defined(LAM_USE_SPINLOCK)

static void lam_mutex_construct(lam_mutex_t* m)
{
    spinunlock(&m->m_lock);
}
                                                                                                             
                                                                                                             
static void lam_mutex_destruct(lam_mutex_t* m)
{
}


OBJ_CLASS_INSTANCE(
    lam_mutex_t,
    lam_object_t,
    lam_mutex_construct,
    lam_mutex_destruct
);

#endif

