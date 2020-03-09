/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "coll_han_trigger.h"

static void mca_coll_task_constructor(mca_coll_task_t * t)
{
    t->func_ptr = NULL;
    t->func_argu = NULL;
}

static void mca_coll_task_destructor(mca_coll_task_t * t)
{
    t->func_ptr = NULL;
    t->func_argu = NULL;
}

OBJ_CLASS_INSTANCE(mca_coll_task_t, opal_object_t, mca_coll_task_constructor,
                   mca_coll_task_destructor);

/* Init task */
int init_task(mca_coll_task_t * t, task_func_ptr func_ptr, void *func_argu)
{
    t->func_ptr = func_ptr;
    t->func_argu = func_argu;
    return OMPI_SUCCESS;
}

/* Issue the task */
int issue_task(mca_coll_task_t * t)
{
    t->func_ptr(t->func_argu);
    return OMPI_SUCCESS;
}
