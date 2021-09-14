/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
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

#include "opal/constants.h"
#include "opal/mca/threads/qthreads/threads_qthreads.h"
#include "opal/mca/threads/threads.h"
#include "opal/mca/threads/tsd.h"

struct opal_tsd_key_value {
    opal_tsd_key_t key;
    opal_tsd_destructor_t destructor;
};

/* false: uninitialized, true: initialized. */
static opal_atomic_lock_t opal_thread_self_key_lock = OPAL_ATOMIC_LOCK_INIT;
static bool opal_thread_self_key_init = false;
static opal_tsd_key_t opal_thread_self_key;

static inline void self_key_ensure_init(void)
{
    if (false == opal_thread_self_key_init) {
        /* not initialized yet. */
        opal_atomic_lock(&opal_thread_self_key_lock);
        /* check again. */
        if (false == opal_thread_self_key_init) {
            /* This thread is responsible for initializing this key. */
            qthread_key_create(&opal_thread_self_key, NULL);
            opal_atomic_mb();
            opal_thread_self_key_init = true;
        }
        opal_atomic_unlock(&opal_thread_self_key_lock);
    }
    /* opal_thread_self_key has been already initialized. */
}

int opal_tsd_key_create(opal_tsd_key_t *key, opal_tsd_destructor_t destructor)
{
    opal_threads_ensure_init_qthreads();
    qthread_key_create(key, destructor);
    return OPAL_SUCCESS;
}
