/*
 * $HEADER$
 */

#include "mutex.h"
#include "condition.h"

#if (LAM_HAVE_THREADS == 0)

                                                                                                             
static void lam_condition_construct(lam_condition_t* c)
{
    c->c_waiting = 0;
    c->c_signaled = 0;
}
                                                                                                             
                                                                                                             
static void lam_condition_destruct(lam_condition_t* c)
{
}
                                                                                                             
                                                                                                             
OBJ_CLASS_INSTANCE(
    lam_condition_t,
    lam_object_t,
    lam_condition_construct,
    lam_condition_destruct
);
                                                                                                             

#endif

