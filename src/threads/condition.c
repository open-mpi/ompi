/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mutex.h"
#include "condition.h"


static void ompi_condition_construct(ompi_condition_t *c)
{
    c->c_waiting = 0;
    c->c_signaled = 0;
#if OMPI_HAVE_POSIX_THREADS
    pthread_cond_init(&c->c_cond, NULL);
#endif
}


static void ompi_condition_destruct(ompi_condition_t *c)
{
#if OMPI_HAVE_POSIX_THREADS
    pthread_cond_destroy(&c->c_cond);
#endif
}


OBJ_CLASS_INSTANCE(ompi_condition_t,
                   ompi_object_t,
                   ompi_condition_construct,
                   ompi_condition_destruct);

