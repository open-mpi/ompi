/*
 * $HEADER$
 */

#include "mutex.h"
#include "condition.h"

#if defined(LAM_USE_SPINWAIT)


static void lam_condition_construct(lam_condition_t* c)
{
    pthread_cond_init(&c->c_cond, NULL);
}
                                                                                                                   
static void lam_condition_destruct(lam_condition_t* c)
{
    pthread_cond_destroy(&c->c_cond);
}
                                                                                                                   
OBJ_CLASS_INSTANCE(
    lam_condition_t,
    lam_object_t,
    lam_condition_construct,
    lam_condition_destruct
);

#endif

