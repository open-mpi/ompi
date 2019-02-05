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
 * Copyright (c) 2019      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <unistd.h>

#include "opal/mca/threads/qthreads/threads_qthreads.h"
#include "opal/constants.h"
#include "opal/util/sys_limits.h"
#include "opal/util/output.h"
#include "opal/prefetch.h"
#include "opal/mca/threads/threads.h"
#include "opal/mca/threads/tsd.h"

struct opal_tsd_key_value {
    opal_tsd_key_t key;
    opal_tsd_destructor_t destructor;
};

static int opal_main_thread;
struct opal_tsd_key_value *opal_tsd_key_values = NULL;
static int opal_tsd_key_values_count = 0;

/*
 * Constructor
 */
static void opal_thread_construct(opal_thread_t *t)
{
}

OBJ_CLASS_INSTANCE(opal_thread_t,
                   opal_object_t,
                   opal_thread_construct, NULL);


opal_thread_t *opal_thread_get_self(void)
{
    return NULL;
}

bool opal_thread_self_compare(opal_thread_t *t)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

int sync_wait_mt(void *p)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

int opal_thread_join(opal_thread_t *t, void **thr_return)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

void opal_thread_set_main(void)
{
}

int opal_thread_start(opal_thread_t *t)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

OBJ_CLASS_DECLARATION(opal_thread_t);

int opal_tsd_key_create(opal_tsd_key_t *key, opal_tsd_destructor_t destructor)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

int opal_tsd_keys_destruct(void)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}
