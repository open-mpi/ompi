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
 * Copyright (c) 2007-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_MUTEX_UNIX_H
#define PMIX_MUTEX_UNIX_H 1

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

#include "src/include/pmix_config.h"

#include <errno.h>
#include <pthread.h>
#include <stdio.h>

#include "src/class/pmix_object.h"

BEGIN_C_DECLS

struct pmix_mutex_t {
    pmix_object_t super;

    pthread_mutex_t m_lock_pthread;

#if PMIX_ENABLE_DEBUG
    int m_lock_debug;
    const char *m_lock_file;
    int m_lock_line;
#endif
};
PMIX_EXPORT PMIX_CLASS_DECLARATION(pmix_mutex_t);
PMIX_EXPORT PMIX_CLASS_DECLARATION(pmix_recursive_mutex_t);

#if defined(PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP)
#    define PMIX_PTHREAD_RECURSIVE_MUTEX_INITIALIZER PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
#elif defined(PTHREAD_RECURSIVE_MUTEX_INITIALIZER)
#    define PMIX_PTHREAD_RECURSIVE_MUTEX_INITIALIZER PTHREAD_RECURSIVE_MUTEX_INITIALIZER
#endif

#if PMIX_ENABLE_DEBUG
#    define PMIX_MUTEX_STATIC_INIT                                                               \
        {                                                                                        \
            .super = PMIX_OBJ_STATIC_INIT(pmix_mutex_t),                                         \
            .m_lock_pthread = PTHREAD_MUTEX_INITIALIZER, .m_lock_debug = 0, .m_lock_file = NULL, \
            .m_lock_line = 0,                                                                    \
        }
#else
#    define PMIX_MUTEX_STATIC_INIT                                                               \
        {                                                                                        \
            .super = PMIX_OBJ_STATIC_INIT(pmix_mutex_t),                                         \
            .m_lock_pthread = PTHREAD_MUTEX_INITIALIZER,                                         \
        }
#endif

#if defined(PMIX_PTHREAD_RECURSIVE_MUTEX_INITIALIZER)

#    if PMIX_ENABLE_DEBUG
#        define PMIX_RECURSIVE_MUTEX_STATIC_INIT                                               \
            {                                                                                  \
                .super = PMIX_OBJ_STATIC_INIT(pmix_mutex_t),                                   \
                .m_lock_pthread = PMIX_PTHREAD_RECURSIVE_MUTEX_INITIALIZER, .m_lock_debug = 0, \
                .m_lock_file = NULL, .m_lock_line = 0,                                         \
            }
#    else
#        define PMIX_RECURSIVE_MUTEX_STATIC_INIT                            \
            {                                                               \
                .super = PMIX_OBJ_STATIC_INIT(pmix_mutex_t),                \
                .m_lock_pthread = PMIX_PTHREAD_RECURSIVE_MUTEX_INITIALIZER, \
            }
#    endif

#endif

/************************************************************************
 *
 * mutex operations (non-atomic versions)
 *
 ************************************************************************/

static inline int pmix_mutex_trylock(pmix_mutex_t *m)
{
#if PMIX_ENABLE_DEBUG
    int ret = pthread_mutex_trylock(&m->m_lock_pthread);
    if (ret == EDEADLK) {
        errno = ret;
        perror("pmix_mutex_trylock()");
        abort();
    }
    return ret;
#else
    return pthread_mutex_trylock(&m->m_lock_pthread);
#endif
}

static inline void pmix_mutex_lock(pmix_mutex_t *m)
{
#if PMIX_ENABLE_DEBUG
    int ret = pthread_mutex_lock(&m->m_lock_pthread);
    if (ret == EDEADLK) {
        errno = ret;
        perror("pmix_mutex_lock()");
        abort();
    }
#else
    pthread_mutex_lock(&m->m_lock_pthread);
#endif
}

static inline void pmix_mutex_unlock(pmix_mutex_t *m)
{
#if PMIX_ENABLE_DEBUG
    int ret = pthread_mutex_unlock(&m->m_lock_pthread);
    if (ret == EPERM) {
        errno = ret;
        perror("pmix_mutex_unlock");
        abort();
    }
#else
    pthread_mutex_unlock(&m->m_lock_pthread);
#endif
}


/************************************************************************
 * Standard locking
 ************************************************************************/

static inline int pmix_mutex_atomic_trylock(pmix_mutex_t *m)
{
    return pmix_mutex_trylock(m);
}

static inline void pmix_mutex_atomic_lock(pmix_mutex_t *m)
{
    pmix_mutex_lock(m);
}

static inline void pmix_mutex_atomic_unlock(pmix_mutex_t *m)
{
    pmix_mutex_unlock(m);
}

END_C_DECLS

#endif /* PMIX_MUTEX_UNIX_H */
