/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"

#if OMPI_HAVE_POSIX_THREADS && OMPI_ENABLE_PROGRESS_THREADS

static void opal_condition_construct(opal_condition_t *c)
{
    pthread_cond_init(&c->c_cond, NULL);
}

static void opal_condition_destruct(opal_condition_t *c)
{
    pthread_cond_destroy(&c->c_cond);
}

OBJ_CLASS_INSTANCE(opal_condition_t,
                   opal_object_t,
                   opal_condition_construct,
                   opal_condition_destruct);

#endif
