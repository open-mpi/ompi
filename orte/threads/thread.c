/*
 * Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "orte/threads/threads.h"

bool orte_debug_threads = false;

static void constructor(orte_thread_ctl_t *ptr)
{
    OBJ_CONSTRUCT(&ptr->lock, opal_mutex_t);
    OBJ_CONSTRUCT(&ptr->cond, opal_condition_t);
    ptr->active = false;
    ptr->running = false;
    ptr->stop = false;
    ptr->rate.tv_sec = 0;
    ptr->rate.tv_usec = 0;
}
static void destructor(orte_thread_ctl_t *ptr)
{
    OBJ_DESTRUCT(&ptr->lock);
    OBJ_DESTRUCT(&ptr->cond);
}
OBJ_CLASS_INSTANCE(orte_thread_ctl_t,
                   opal_object_t,
                   constructor, destructor);

