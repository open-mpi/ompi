/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2018 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2020      Triad National Security, LLC. All rights
 *                         reserved.
 *
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2021      Argonne National Laboratory.  All rights reserved.
 * Copyright (c) 2022      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_THREADS_QTHREADS_THREADS_QTHREADS_MUTEX_H
#define OPAL_MCA_THREADS_QTHREADS_THREADS_QTHREADS_MUTEX_H 1

#include "opal_config.h"

#include <stdio.h>

#include "opal/class/opal_object.h"
#include "opal/constants.h"
#include "opal/mca/threads/qthreads/threads_qthreads.h"
#include "opal/sys/atomic.h"
#include "opal/util/show_help.h"

BEGIN_C_DECLS

typedef qthread_spinlock_t opal_thread_internal_mutex_t;

#define OPAL_THREAD_INTERNAL_MUTEX_INITIALIZER           QTHREAD_MUTEX_INITIALIZER
#define OPAL_THREAD_INTERNAL_RECURSIVE_MUTEX_INITIALIZER QTHREAD_RECURSIVE_MUTEX_INITIALIZER

static inline int opal_thread_internal_mutex_init(opal_thread_internal_mutex_t *p_mutex,
                                                  bool recursive)
{
    opal_threads_ensure_init_qthreads();
    #if OPAL_ENABLE_DEBUG
    int ret = qthread_spinlock_init(p_mutex,recursive);
    if (QTHREAD_SUCCESS != ret) {
        opal_show_help("help-opal-threads.txt", "mutex init failed", true);
    }
    #else
    qthread_spinlock_init(p_mutex,recursive);
    #endif
    return OPAL_SUCCESS;
}

static inline void opal_thread_internal_mutex_lock(opal_thread_internal_mutex_t *p_mutex)
{
    opal_threads_ensure_init_qthreads();
    #if OPAL_ENABLE_DEBUG
    int ret = qthread_spinlock_lock(p_mutex);
    if (QTHREAD_SUCCESS != ret) {
        opal_show_help("help-opal-threads.txt", "mutex lock failed", true);
    }
    #else
    qthread_spinlock_lock(p_mutex);
    #endif
}

static inline int opal_thread_internal_mutex_trylock(opal_thread_internal_mutex_t *p_mutex)
{
    opal_threads_ensure_init_qthreads();
    int ret = qthread_spinlock_trylock(p_mutex);
    if (QTHREAD_OPFAIL == ret) {
        return 1;
    } else if (QTHREAD_SUCCESS != ret) {
#if OPAL_ENABLE_DEBUG
        opal_show_help("help-opal-threads.txt", "mutex trylock failed", true);
#endif
        return 1;
    } 
    return 0;
}

static inline void opal_thread_internal_mutex_unlock(opal_thread_internal_mutex_t *p_mutex)
{
    opal_threads_ensure_init_qthreads();
    int ret; 
    #if OPAL_ENABLE_DEBUG
    ret = qthread_spinlock_unlock(p_mutex);
    if (QTHREAD_SUCCESS != ret) {
        opal_show_help("help-opal-threads.txt", "mutex unlock failed", true);
    }
    #else
    qthread_spinlock_unlock(p_mutex);
    #endif
    /* For fairness of locking. */
    qthread_yield();
}

static inline void opal_thread_internal_mutex_destroy(opal_thread_internal_mutex_t *p_mutex)
{
    /* No specific operation is needed to destroy opal_thread_internal_mutex_t. */
}

typedef struct opal_thread_cond_waiter_t {
    int m_signaled;
    struct opal_thread_cond_waiter_t *m_prev;
} opal_thread_cond_waiter_t;

typedef struct {
    opal_thread_internal_mutex_t m_lock;
    opal_thread_cond_waiter_t *m_waiter_head;
    opal_thread_cond_waiter_t *m_waiter_tail;
} opal_thread_internal_cond_t;

#define OPAL_THREAD_INTERNAL_COND_INITIALIZER                                          \
    {                                                                                  \
        .m_lock = QTHREAD_MUTEX_INITIALIZER, .m_waiter_head = NULL, .m_waiter_tail = NULL, \
    }

static inline int opal_thread_internal_cond_init(opal_thread_internal_cond_t *p_cond)
{
    qthread_spinlock_init(&p_cond->m_lock, false /* is_recursive */);
    p_cond->m_waiter_head = NULL;
    p_cond->m_waiter_tail = NULL;
    return OPAL_SUCCESS;
}

static inline void opal_thread_internal_cond_wait(opal_thread_internal_cond_t *p_cond,
                                                  opal_thread_internal_mutex_t *p_mutex)
{
    opal_threads_ensure_init_qthreads();
    /* This thread is taking "lock", so only this thread can access this
     * condition variable.  */
    qthread_spinlock_lock(&p_cond->m_lock);
    opal_thread_cond_waiter_t waiter = {0, NULL};
    if (NULL == p_cond->m_waiter_head) {
        p_cond->m_waiter_tail = &waiter;
    } else {
        p_cond->m_waiter_head->m_prev = &waiter;
    }
    p_cond->m_waiter_head = &waiter;
    qthread_spinlock_unlock(&p_cond->m_lock);
    while (1) {       
        opal_thread_internal_mutex_unlock(p_mutex);
        qthread_yield();
        opal_thread_internal_mutex_lock(p_mutex);
        /* Check if someone woke me up. */
        qthread_spinlock_lock(&p_cond->m_lock);
        int signaled = waiter.m_signaled;
        qthread_spinlock_unlock(&p_cond->m_lock);
        if (1 == signaled) {
            break;
        }
        /* Unlock the lock again. */
    }
}

static inline void opal_thread_internal_cond_broadcast(opal_thread_internal_cond_t *p_cond)
{
    qthread_spinlock_lock(&p_cond->m_lock);
    while (NULL != p_cond->m_waiter_tail) {
        opal_thread_cond_waiter_t *p_cur_tail = p_cond->m_waiter_tail;
        p_cond->m_waiter_tail = p_cur_tail->m_prev;
        /* Awaken one of threads in a FIFO manner. */
        p_cur_tail->m_signaled = 1;
    }
    /* No waiters. */
    p_cond->m_waiter_head = NULL;
    qthread_spinlock_unlock(&p_cond->m_lock);
}

static inline void opal_thread_internal_cond_signal(opal_thread_internal_cond_t *p_cond)
{
    qthread_spinlock_lock(&p_cond->m_lock);
    if (NULL != p_cond->m_waiter_tail) {
        opal_thread_cond_waiter_t *p_cur_tail = p_cond->m_waiter_tail;
        p_cond->m_waiter_tail = p_cur_tail->m_prev;
        /* Awaken one of threads. */
        p_cur_tail->m_signaled = 1;
        if (NULL == p_cond->m_waiter_tail) {
            p_cond->m_waiter_head = NULL;
        }
    }
    qthread_spinlock_unlock(&p_cond->m_lock);
}

static inline void opal_thread_internal_cond_destroy(opal_thread_internal_cond_t *p_cond)
{
    /* No destructor is needed. */
}

END_C_DECLS

#endif /* OPAL_MCA_THREADS_QTHREADS_THREADS_QTHREADS_MUTEX_H */
