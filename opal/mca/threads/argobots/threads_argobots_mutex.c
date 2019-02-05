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
 * Copyright (c) 2007-2016 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal/mca/threads/argobots/threads_argobots.h"
#include "opal_config.h"

#include <errno.h>

#include "opal/mca/threads/mutex.h"
#include "opal/mca/threads/argobots/threads_argobots_mutex.h"
#include "opal/constants.h"

/*
 * Wait and see if some upper layer wants to use threads, if support
 * exists.
 */
bool opal_uses_threads = false;

static void mca_threads_argobots_mutex_constructor(opal_mutex_t *p_mutex)
{
    opal_threads_argobots_ensure_init();
    p_mutex->m_lock_argobots = OPAL_ABT_MUTEX_NULL;
    p_mutex->m_recursive = 0;
#if OPAL_ENABLE_DEBUG
    p_mutex->m_lock_debug = 0;
    p_mutex->m_lock_file = NULL;
    p_mutex->m_lock_line = 0;
#endif
    opal_atomic_lock_init(&p_mutex->m_lock_atomic, 0);
}

static void mca_threads_argobots_mutex_destructor(opal_mutex_t *p_mutex)
{
    if (OPAL_ABT_MUTEX_NULL != p_mutex->m_lock_argobots) {
        ABT_mutex_free(&p_mutex->m_lock_argobots);
    }
}

static void mca_threads_argobots_recursive_mutex_constructor
        (opal_recursive_mutex_t *p_mutex)
{
    opal_threads_argobots_ensure_init();
    p_mutex->m_lock_argobots = OPAL_ABT_MUTEX_NULL;
    p_mutex->m_recursive = 1;
#if OPAL_ENABLE_DEBUG
    p_mutex->m_lock_debug = 0;
    p_mutex->m_lock_file = NULL;
    p_mutex->m_lock_line = 0;
#endif
    opal_atomic_lock_init(&p_mutex->m_lock_atomic, 0);
}

static void mca_threads_argobots_recursive_mutex_destructor
        (opal_recursive_mutex_t *p_mutex)
{
    if (OPAL_ABT_MUTEX_NULL != p_mutex->m_lock_argobots) {
        ABT_mutex_free(&p_mutex->m_lock_argobots);
    }
}

OBJ_CLASS_INSTANCE(opal_mutex_t,
                   opal_object_t,
                   mca_threads_argobots_mutex_constructor,
                   mca_threads_argobots_mutex_destructor);
OBJ_CLASS_INSTANCE(opal_recursive_mutex_t,
                   opal_object_t,
                   mca_threads_argobots_recursive_mutex_constructor,
                   mca_threads_argobots_recursive_mutex_destructor);

void opal_mutex_create(struct opal_mutex_t *m)
{
    opal_threads_argobots_ensure_init();
    while (OPAL_ABT_MUTEX_NULL == m->m_lock_argobots) {
        ABT_mutex abt_mutex;
        if (m->m_recursive) {
            ABT_mutex_attr abt_mutex_attr;
            ABT_mutex_attr_create(&abt_mutex_attr);
            ABT_mutex_attr_set_recursive(abt_mutex_attr, ABT_TRUE);
            ABT_mutex_create_with_attr(abt_mutex_attr, &abt_mutex);
            ABT_mutex_attr_free(&abt_mutex_attr);
        } else {
            ABT_mutex_create(&abt_mutex);
        }
        void *null_ptr = OPAL_ABT_MUTEX_NULL;
        if (opal_atomic_compare_exchange_strong_ptr(
             (intptr_t *)&m->m_lock_argobots, (intptr_t *)&null_ptr,
             (intptr_t)abt_mutex)) {
            /* mutex is successfully created and substituted. */
            return;
        }
        ABT_mutex_free(&abt_mutex);
    }
}

static void opal_cond_create(opal_cond_t *cond)
{
    opal_threads_argobots_ensure_init();
    while (OPAL_ABT_COND_NULL == *cond) {
        ABT_cond new_cond;
        ABT_cond_create(&new_cond);
        void *null_ptr = OPAL_ABT_COND_NULL;
        if (opal_atomic_compare_exchange_strong_ptr((intptr_t *)cond,
                                                    (intptr_t *)&null_ptr,
                                                    (intptr_t)new_cond)) {
            /* cond is successfully created and substituted. */
            return;
        }
        ABT_cond_free(&new_cond);
    }
}

int opal_cond_init(opal_cond_t *cond)
{
    *cond = OPAL_ABT_COND_NULL;
    return OPAL_SUCCESS;
}

int opal_cond_wait(opal_cond_t *cond, opal_mutex_t *lock)
{
    if (OPAL_ABT_COND_NULL == *cond) {
        opal_cond_create(cond);
    }
    int ret = ABT_cond_wait(*cond, lock->m_lock_argobots);
    return ABT_SUCCESS == ret ? OPAL_SUCCESS : OPAL_ERROR;
}

int opal_cond_broadcast(opal_cond_t *cond)
{
    if (OPAL_ABT_COND_NULL == *cond) {
        opal_cond_create(cond);
    }
    int ret = ABT_cond_broadcast(*cond);
    return ABT_SUCCESS == ret ? OPAL_SUCCESS : OPAL_ERROR;
}

int opal_cond_signal(opal_cond_t *cond)
{
    if (OPAL_ABT_COND_NULL == *cond) {
        opal_cond_create(cond);
    }
    int ret = ABT_cond_signal(*cond);
    return ABT_SUCCESS == ret ? OPAL_SUCCESS : OPAL_ERROR;
}

int opal_cond_destroy(opal_cond_t *cond)
{
    int ret = ABT_SUCCESS;
    if (OPAL_ABT_COND_NULL != *cond) {
        ret = ABT_cond_free(cond);
    }
    return ABT_SUCCESS == ret ? OPAL_SUCCESS : OPAL_ERROR;
}
