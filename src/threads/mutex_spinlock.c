/*
 * $HEADER$
 */

#include "mutex.h"
#if (OMPI_HAVE_THREADS == 0)

static void ompi_mutex_construct(ompi_mutex_t* m)
{
    spinunlock(&m->m_lock);
}
                                                                                                             
                                                                                                             
static void ompi_mutex_destruct(ompi_mutex_t* m)
{
}


OBJ_CLASS_INSTANCE(
    ompi_mutex_t,
    ompi_object_t,
    ompi_mutex_construct,
    ompi_mutex_destruct
);

#endif

