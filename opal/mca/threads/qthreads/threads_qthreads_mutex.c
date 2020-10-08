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

#include "opal_config.h"

#include "opal/mca/threads/mutex.h"
#include "opal/constants.h"

/*
 * Wait and see if some upper layer wants to use threads, if support
 * exists.
 */
bool opal_uses_threads = false;

static void opal_mutex_construct(opal_mutex_t *m)
{
    opal_threads_ensure_init_qthreads();
    opal_mutex_create(m);
}

static void opal_mutex_destruct(opal_mutex_t *m)
{
}

OBJ_CLASS_INSTANCE(opal_mutex_t,
                   opal_object_t,
                   opal_mutex_construct,
                   opal_mutex_destruct);

static void opal_recursive_mutex_construct(opal_recursive_mutex_t *m)
{
}

OBJ_CLASS_INSTANCE(opal_recursive_mutex_t,
                   opal_object_t,
                   opal_recursive_mutex_construct,
                   opal_mutex_destruct);

int opal_cond_init(opal_cond_t *cond)
{
    opal_atomic_lock_init(&cond->m_lock, 0);
    cond->m_waiter_head = NULL;
    cond->m_waiter_tail = NULL;
    return OPAL_SUCCESS;
}

typedef struct {
    int m_signaled;
    void *m_prev;
} cond_waiter_t;

int opal_cond_wait(opal_cond_t *cond, opal_mutex_t *lock)
{
    opal_threads_ensure_init_qthreads();
    /* This thread is taking "lock", so only this thread can access this
     * condition variable.  */
    opal_atomic_lock(&cond->m_lock);
    cond_waiter_t waiter = { 0, NULL };
    if (NULL == cond->m_waiter_head) {
        cond->m_waiter_tail = (void *)&waiter;
    } else {
        ((cond_waiter_t *)cond->m_waiter_head)->m_prev = (void *)&waiter;
    }
    cond->m_waiter_head = (void *)&waiter;
    opal_atomic_unlock(&cond->m_lock);

    while (1) {
        opal_mutex_unlock(lock);
        qthread_yield();
        opal_mutex_lock(lock);
        /* Check if someone woke me up. */
        opal_atomic_lock(&cond->m_lock);
        int signaled = waiter.m_signaled;
        opal_atomic_unlock(&cond->m_lock);
        if (1 == signaled) {
            break;
        }
        /* Unlock the lock again. */
    }
    return OPAL_SUCCESS;
}

int opal_cond_broadcast(opal_cond_t *cond)
{
    opal_atomic_lock(&cond->m_lock);
    while (NULL != cond->m_waiter_tail) {
        cond_waiter_t *p_cur_tail = (cond_waiter_t *)cond->m_waiter_tail;
        cond->m_waiter_tail = p_cur_tail->m_prev;
        /* Awaken one of threads in a FIFO manner. */
        p_cur_tail->m_signaled = 1;
    }
    /* No waiters. */
    cond->m_waiter_head = NULL;
    opal_atomic_unlock(&cond->m_lock);
    return OPAL_SUCCESS;
}

int opal_cond_signal(opal_cond_t *cond)
{
    opal_atomic_lock(&cond->m_lock);
    if (NULL != cond->m_waiter_tail) {
        cond_waiter_t *p_cur_tail = (cond_waiter_t *)cond->m_waiter_tail;
        cond->m_waiter_tail = p_cur_tail->m_prev;
        /* Awaken one of threads. */
        p_cur_tail->m_signaled = 1;
        if (NULL == cond->m_waiter_tail) {
            cond->m_waiter_head = NULL;
        }
    }
    opal_atomic_unlock(&cond->m_lock);
    return OPAL_SUCCESS;
}

int opal_cond_destroy(opal_cond_t *cond)
{
    return OPAL_SUCCESS;
}
