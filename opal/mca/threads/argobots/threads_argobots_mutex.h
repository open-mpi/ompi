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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_MUTEX_H
#define OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_MUTEX_H

#include "opal_config.h"

#include <errno.h>
#include <stdio.h>

#include "opal/mca/threads/argobots/threads_argobots.h"

#include "opal/class/opal_object.h"
#include "opal/sys/atomic.h"
#include "opal/util/output.h"

#include <abt.h>

BEGIN_C_DECLS

/* Don't use ABT_MUTEX_NULL, since it might be not NULL. */
#define OPAL_ABT_MUTEX_NULL 0

struct opal_mutex_t {
    opal_object_t super;

    ABT_mutex m_lock_argobots;
    int m_recursive;

#if OPAL_ENABLE_DEBUG
    int m_lock_debug;
    const char *m_lock_file;
    int m_lock_line;
#endif

    opal_atomic_lock_t m_lock_atomic;
};

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_mutex_t);
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_recursive_mutex_t);

#if OPAL_ENABLE_DEBUG
#define OPAL_MUTEX_STATIC_INIT                                          \
    {                                                                   \
        .super = OPAL_OBJ_STATIC_INIT(opal_mutex_t),                    \
        .m_lock_argobots = OPAL_ABT_MUTEX_NULL,                         \
        .m_recursive = 0,                                               \
        .m_lock_debug = 0,                                              \
        .m_lock_file = NULL,                                            \
        .m_lock_line = 0,                                               \
        .m_lock_atomic = OPAL_ATOMIC_LOCK_INIT,                         \
    }
#else
#define OPAL_MUTEX_STATIC_INIT                                          \
    {                                                                   \
        .super = OPAL_OBJ_STATIC_INIT(opal_mutex_t),                    \
        .m_lock_argobots = OPAL_ABT_MUTEX_NULL,                         \
        .m_recursive = 0,                                               \
        .m_lock_atomic = OPAL_ATOMIC_LOCK_INIT,                         \
    }
#endif

#if OPAL_ENABLE_DEBUG
#define OPAL_RECURSIVE_MUTEX_STATIC_INIT                                \
    {                                                                   \
        .super = OPAL_OBJ_STATIC_INIT(opal_mutex_t),                    \
        .m_lock_argobots = OPAL_ABT_MUTEX_NULL,                         \
        .m_recursive = 1,                                               \
        .m_lock_debug = 0,                                              \
        .m_lock_file = NULL,                                            \
        .m_lock_line = 0,                                               \
        .m_lock_atomic = OPAL_ATOMIC_LOCK_INIT,                         \
    }
#else
#define OPAL_RECURSIVE_MUTEX_STATIC_INIT                                \
    {                                                                   \
        .super = OPAL_OBJ_STATIC_INIT(opal_mutex_t),                    \
        .m_lock_argobots = OPAL_ABT_MUTEX_NULL,                         \
        .m_recursive = 1,                                               \
        .m_lock_atomic = OPAL_ATOMIC_LOCK_INIT,                         \
    }
#endif

/************************************************************************
 *
 * mutex operations (non-atomic versions)
 *
 ************************************************************************/

void opal_mutex_create(struct opal_mutex_t *m);

static inline int opal_mutex_trylock(opal_mutex_t *m)
{
    if (OPAL_ABT_MUTEX_NULL == m->m_lock_argobots) {
        opal_mutex_create(m);
    }
    int ret = ABT_mutex_trylock(m->m_lock_argobots);
    if (ABT_ERR_MUTEX_LOCKED == ret) {
        return 1;
    } else if (ABT_SUCCESS != ret) {
#if OPAL_ENABLE_DEBUG
        opal_output(0, "opal_mutex_trylock()");
#endif
        return 1;
    }
    return 0;
}

static inline void opal_mutex_lock(opal_mutex_t *m)
{
    if (OPAL_ABT_MUTEX_NULL == m->m_lock_argobots) {
        opal_mutex_create(m);
    }
#if OPAL_ENABLE_DEBUG
    int ret = ABT_mutex_lock(m->m_lock_argobots);
    if (ABT_SUCCESS != ret) {
        opal_output(0, "opal_mutex_lock()");
    }
#else
    ABT_mutex_lock(m->m_lock_argobots);
#endif
}

static inline void opal_mutex_unlock(opal_mutex_t *m)
{
    if (OPAL_ABT_MUTEX_NULL == m->m_lock_argobots) {
        opal_mutex_create(m);
    }
#if OPAL_ENABLE_DEBUG
    int ret = ABT_mutex_unlock(m->m_lock_argobots);
    if (ABT_SUCCESS != ret) {
        opal_output(0, "opal_mutex_unlock()");
    }
#else
    ABT_mutex_unlock(m->m_lock_argobots);
#endif
    /* For fairness of locking. */
    ABT_thread_yield();
}

/************************************************************************
 *
 * mutex operations (atomic versions)
 *
 ************************************************************************/

#if OPAL_HAVE_ATOMIC_SPINLOCKS

/************************************************************************
 * Spin Locks
 ************************************************************************/

static inline int opal_mutex_atomic_trylock(opal_mutex_t *m)
{
    return opal_atomic_trylock(&m->m_lock_atomic);
}

static inline void opal_mutex_atomic_lock(opal_mutex_t *m)
{
    opal_atomic_lock(&m->m_lock_atomic);
}

static inline void opal_mutex_atomic_unlock(opal_mutex_t *m)
{
    opal_atomic_unlock(&m->m_lock_atomic);
}

#else

/************************************************************************
 * Standard locking
 ************************************************************************/

static inline int opal_mutex_atomic_trylock(opal_mutex_t *m)
{
    return opal_mutex_trylock(m);
}

static inline void opal_mutex_atomic_lock(opal_mutex_t *m)
{
    opal_mutex_lock(m);
}

static inline void opal_mutex_atomic_unlock(opal_mutex_t *m)
{
    opal_mutex_unlock(m);
}

#endif

#define OPAL_ABT_COND_NULL NULL
typedef ABT_cond opal_cond_t;
#define OPAL_CONDITION_STATIC_INIT OPAL_ABT_COND_NULL

int opal_cond_init(opal_cond_t *cond);
int opal_cond_wait(opal_cond_t *cond, opal_mutex_t *lock);
int opal_cond_broadcast(opal_cond_t *cond);
int opal_cond_signal(opal_cond_t *cond);
int opal_cond_destroy(opal_cond_t *cond);

END_C_DECLS

#endif /* OPAL_MCA_THREADS_ARGOBOTS_THREADS_ARGOBOTS_MUTEX_H */
