/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* needed for nanosleep() */
#define _POSIX_C_SOURCE 200809L

#include "opal_config.h"
#include <time.h>
#include "opal/class/opal_lifo.h"

static void opal_lifo_construct(opal_lifo_t *lifo)
{
    OBJ_CONSTRUCT(&lifo->opal_lifo_ghost, opal_list_item_t);
    lifo->opal_lifo_ghost.opal_list_next = &lifo->opal_lifo_ghost;
    lifo->opal_lifo_head.data.item = (intptr_t) &lifo->opal_lifo_ghost;
    lifo->opal_lifo_head.data.counter = 0;
}

OBJ_CLASS_INSTANCE(opal_lifo_t, opal_object_t, opal_lifo_construct, NULL);


void opal_lifo_release_cpu(void)
{
    /* NTH: there are many ways to cause the current thread to be suspended. This one
     * should work well in most cases. Another approach would be to use poll (NULL, 0, ) but
     * the interval will be forced to be in ms (instead of ns or us). Note that there
     * is a performance improvement for the lifo test when this call is made on detection
     * of contention but it may not translate into actually MPI or application performance
     * improvements. */
    static struct timespec interval = {.tv_sec = 0, .tv_nsec = 100};
    nanosleep(&interval, NULL);
}
