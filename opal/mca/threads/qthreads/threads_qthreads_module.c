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

#include "opal/mca/threads/qthreads/threads_qthreads.h"
#include "opal/constants.h"
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

/*
 * Constructor
 */
static void opal_thread_construct(opal_thread_t *t)
{
    t->t_run = 0;
    t->t_thread_ret = 0;
}

OBJ_CLASS_INSTANCE(opal_thread_t,
                   opal_object_t,
                   opal_thread_construct, NULL);

static inline aligned_t *opal_thread_get_qthreads_self(void)
{
    self_key_ensure_init();
    void *ptr = qthread_getspecific(opal_thread_self_key);
    return (aligned_t *)ptr;
}

static aligned_t opal_thread_qthreads_wrapper(void *arg)
{
    opal_thread_t *t = (opal_thread_t *)arg;

    /* Register itself. */
    self_key_ensure_init();
    qthread_setspecific(opal_thread_self_key, t->t_thread_ret_ptr);

    t->t_ret = ((void *(*)(void *))t->t_run)(t);
    return 0;
}

opal_thread_t *opal_thread_get_self(void)
{
    opal_threads_ensure_init_qthreads();
    opal_thread_t *t = OBJ_NEW(opal_thread_t);
    t->t_thread_ret_ptr = opal_thread_get_qthreads_self();
    return t;
}

bool opal_thread_self_compare(opal_thread_t *t)
{
    opal_threads_ensure_init_qthreads();
    return opal_thread_get_qthreads_self() == &t->t_thread_ret;
}

int opal_thread_join(opal_thread_t *t, void **thr_return)
{
    qthread_readFF(NULL, t->t_thread_ret_ptr);
    if (thr_return) {
        *thr_return = t->t_ret;
    }
    t->t_thread_ret = 0;
    return OPAL_SUCCESS;
}

void opal_thread_set_main(void)
{
}

int opal_thread_start(opal_thread_t *t)
{
    opal_threads_ensure_init_qthreads();
    t->t_thread_ret_ptr = &t->t_thread_ret;
    qthread_fork(opal_thread_qthreads_wrapper, t, &t->t_thread_ret);
    return OPAL_SUCCESS;
}

OBJ_CLASS_DECLARATION(opal_thread_t);

int opal_tsd_key_create(opal_tsd_key_t *key, opal_tsd_destructor_t destructor)
{
    opal_threads_ensure_init_qthreads();
    qthread_key_create(key, destructor);
    return OPAL_SUCCESS;
}
