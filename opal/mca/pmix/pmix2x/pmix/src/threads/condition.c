/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"

#include "src/threads/condition.h"


static void pmix_condition_construct(pmix_condition_t *c)
{
    c->c_waiting = 0;
    c->c_signaled = 0;
}


static void pmix_condition_destruct(pmix_condition_t *c)
{
}

PMIX_CLASS_INSTANCE(pmix_condition_t,
                   pmix_object_t,
                   pmix_condition_construct,
                   pmix_condition_destruct);
