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

#if OMPI_HAVE_POSIX_THREADS

static void ompi_condition_construct(ompi_condition_t *c)
{
    pthread_cond_init(&c->c_cond, NULL);
}

static void ompi_condition_destruct(ompi_condition_t *c)
{
    pthread_cond_destroy(&c->c_cond);
}

OBJ_CLASS_INSTANCE(ompi_condition_t,
                   ompi_object_t,
                   ompi_condition_construct,
                   ompi_condition_destruct);

#endif
