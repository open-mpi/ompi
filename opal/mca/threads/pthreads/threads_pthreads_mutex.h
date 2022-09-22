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
 * Copyright (c) 2021      Argonne National Laboratory.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_THREADS_PTHREADS_THREADS_PTHREADS_MUTEX_H
#define OPAL_MCA_THREADS_PTHREADS_THREADS_PTHREADS_MUTEX_H

/**
 * @file:
 *
 * Mutual exclusion functions: Unix implementation.
 *
 * Functions for locking of critical sections.
 *
 * On unix, use pthreads or our own atomic operations as
 * available.
 */

#include "opal_config.h"

#include <errno.h>
#include <pthread.h>
#include <stdio.h>

#include "opal/class/opal_object.h"
#include "opal/constants.h"
#include "opal/util/show_help.h"

BEGIN_C_DECLS

typedef pthread_mutex_t opal_thread_internal_mutex_t;

#define OPAL_THREAD_INTERNAL_MUTEX_INITIALIZER PTHREAD_MUTEX_INITIALIZER
#if defined(PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP)
#    define OPAL_THREAD_INTERNAL_RECURSIVE_MUTEX_INITIALIZER PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
#elif defined(PTHREAD_RECURSIVE_MUTEX_INITIALIZER)
#    define OPAL_THREAD_INTERNAL_RECURSIVE_MUTEX_INITIALIZER PTHREAD_RECURSIVE_MUTEX_INITIALIZER
#endif

static inline int opal_thread_internal_mutex_init(opal_thread_internal_mutex_t *p_mutex,
                                                  bool recursive)
{
    int ret;
#if OPAL_ENABLE_DEBUG
    if (recursive) {
        pthread_mutexattr_t mutex_attr;
        ret = pthread_mutexattr_init(&mutex_attr);
        if (0 != ret)
            return OPAL_ERR_IN_ERRNO;
        ret = pthread_mutexattr_settype(&mutex_attr, PTHREAD_MUTEX_RECURSIVE);
        if (0 != ret) {
            ret = pthread_mutexattr_destroy(&mutex_attr);
            assert(0 == ret);
            return OPAL_ERR_IN_ERRNO;
        }
        ret = pthread_mutex_init(p_mutex, &mutex_attr);
        if (0 != ret) {
            ret = pthread_mutexattr_destroy(&mutex_attr);
            assert(0 == ret);
            return OPAL_ERR_IN_ERRNO;
        }
        ret = pthread_mutexattr_destroy(&mutex_attr);
        assert(0 == ret);
    } else {
        ret = pthread_mutex_init(p_mutex, NULL);
    }
#else
    if (recursive) {
        pthread_mutexattr_t mutex_attr;
        ret = pthread_mutexattr_init(&mutex_attr);
        if (0 != ret) {
            return OPAL_ERR_IN_ERRNO;
        }
        pthread_mutexattr_settype(&mutex_attr, PTHREAD_MUTEX_RECURSIVE);
        ret = pthread_mutex_init(p_mutex, &mutex_attr);
        pthread_mutexattr_destroy(&mutex_attr);
    } else {
        ret = pthread_mutex_init(p_mutex, NULL);
    }
#endif
    return 0 == ret ? OPAL_SUCCESS : OPAL_ERR_IN_ERRNO;
}

static inline void opal_thread_internal_mutex_lock(opal_thread_internal_mutex_t *p_mutex)
{
#if OPAL_ENABLE_DEBUG
    int ret = pthread_mutex_lock(p_mutex);
    if (EDEADLK == ret) {
        opal_show_help("help-opal-threads.txt", "mutex lock failed", true);
    }
    assert(0 == ret);
#else
    pthread_mutex_lock(p_mutex);
#endif
}

static inline int opal_thread_internal_mutex_trylock(opal_thread_internal_mutex_t *p_mutex)
{
    int ret = pthread_mutex_trylock(p_mutex);
    return 0 == ret ? 0 : 1;
}

static inline void opal_thread_internal_mutex_unlock(opal_thread_internal_mutex_t *p_mutex)
{
#if OPAL_ENABLE_DEBUG
    int ret = pthread_mutex_unlock(p_mutex);
    assert(0 == ret);
#else
    pthread_mutex_unlock(p_mutex);
#endif
}

static inline void opal_thread_internal_mutex_destroy(opal_thread_internal_mutex_t *p_mutex)
{
#if OPAL_ENABLE_DEBUG
    int ret = pthread_mutex_destroy(p_mutex);
    assert(0 == ret);
#else
    pthread_mutex_destroy(p_mutex);
#endif
}

typedef pthread_cond_t opal_thread_internal_cond_t;

#define OPAL_THREAD_INTERNAL_COND_INITIALIZER PTHREAD_COND_INITIALIZER

static inline int opal_thread_internal_cond_init(opal_thread_internal_cond_t *p_cond)
{
    int ret = pthread_cond_init(p_cond, NULL);
    return 0 == ret ? OPAL_SUCCESS : OPAL_ERR_IN_ERRNO;
}

static inline void opal_thread_internal_cond_wait(opal_thread_internal_cond_t *p_cond,
                                                  opal_thread_internal_mutex_t *p_mutex)
{
#if OPAL_ENABLE_DEBUG
    int ret = pthread_cond_wait(p_cond, p_mutex);
    assert(0 == ret);
#else
    pthread_cond_wait(p_cond, p_mutex);
#endif
}

static inline void opal_thread_internal_cond_broadcast(opal_thread_internal_cond_t *p_cond)
{
#if OPAL_ENABLE_DEBUG
    int ret = pthread_cond_broadcast(p_cond);
    assert(0 == ret);
#else
    pthread_cond_broadcast(p_cond);
#endif
}

static inline void opal_thread_internal_cond_signal(opal_thread_internal_cond_t *p_cond)
{
#if OPAL_ENABLE_DEBUG
    int ret = pthread_cond_signal(p_cond);
    assert(0 == ret);
#else
    pthread_cond_signal(p_cond);
#endif
}

static inline void opal_thread_internal_cond_destroy(opal_thread_internal_cond_t *p_cond)
{
#if OPAL_ENABLE_DEBUG
    int ret = pthread_cond_destroy(p_cond);
    assert(0 == ret);
#else
    pthread_cond_destroy(p_cond);
#endif
}

END_C_DECLS

#endif /* OPAL_MCA_THREADS_PTHREADS_THREADS_PTHREADS_MUTEX_H */
