/*
 * $HEADER$
 */

#ifdef HAVE_CONFIG_H
#include "ompi_config.h"
#endif

#include "mutex.h"
#include "condition.h"

#if (OMPI_HAVE_THREADS == 0)

static void ompi_condition_construct(ompi_condition_t *c)
{
    c->c_waiting = 0;
    c->c_signaled = 0;
}


static void ompi_condition_destruct(ompi_condition_t *c)
{
}


OBJ_CLASS_INSTANCE(ompi_condition_t,
                   ompi_object_t,
                   ompi_condition_construct,
                   ompi_condition_destruct);

#endif
