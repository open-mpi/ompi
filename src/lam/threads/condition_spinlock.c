/*
 * $HEADER$
 */

#include "mutex.h"
#include "condition.h"

#if defined(LAM_USE_SPINLOCK)

                                                                                                             
static void lam_condition_construct(lam_condition_t* c)
{
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

