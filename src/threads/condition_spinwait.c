/*
 * $HEADER$
 */

#include "mutex.h"
#include "condition.h"

#if defined(OMPI_USE_SPINWAIT)


static void ompi_condition_construct(ompi_condition_t* c)
{
    pthread_cond_init(&c->c_cond, NULL);
}
                                                                                                                   
static void ompi_condition_destruct(ompi_condition_t* c)
{
    pthread_cond_destroy(&c->c_cond);
}
                                                                                                                   
OBJ_CLASS_INSTANCE(
    ompi_condition_t,
    ompi_object_t,
    ompi_condition_construct,
    ompi_condition_destruct
);

#endif

