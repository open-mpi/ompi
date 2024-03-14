/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2018 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2007      Voltaire. All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2020      Triad National Security, LLC.  All rights reserved.
 * Copyright (c) 2021      Argonne National Laboratory.  All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_THREADS_MUTEX_H
#define OPAL_MCA_THREADS_MUTEX_H

#include "opal_config.h"
#include "opal/sys/atomic.h"

BEGIN_C_DECLS

/**
 * @file:
 *
 * Mutual exclusion functions.
 *
 * Functions for locking of critical sections.
 */

/**
 * Opaque mutex object
 */

typedef struct opal_mutex_t opal_mutex_t;
typedef struct opal_mutex_t opal_recursive_mutex_t;

#include MCA_threads_mutex_base_include_HEADER

struct opal_mutex_t {
    opal_object_t super;
    opal_thread_internal_mutex_t m_lock;
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
#    define OPAL_MUTEX_STATIC_INIT                                                         \
        {                                                                                  \
            .super = OPAL_OBJ_STATIC_INIT(opal_mutex_t),                                   \
            .m_lock = OPAL_THREAD_INTERNAL_MUTEX_INITIALIZER, .m_lock_debug = 0,           \
            .m_lock_file = NULL, .m_lock_line = 0, .m_lock_atomic = OPAL_ATOMIC_LOCK_INIT, \
        }
#else
#    define OPAL_MUTEX_STATIC_INIT                            \
        {                                                     \
            .super = OPAL_OBJ_STATIC_INIT(opal_mutex_t),      \
            .m_lock = OPAL_THREAD_INTERNAL_MUTEX_INITIALIZER, \
            .m_lock_atomic = OPAL_ATOMIC_LOCK_INIT,           \
        }
#endif

#if defined(OPAL_THREAD_INTERNAL_RECURSIVE_MUTEX_INITIALIZER)
#    if OPAL_ENABLE_DEBUG
#        define OPAL_RECURSIVE_MUTEX_STATIC_INIT                                               \
            {                                                                                  \
                .super = OPAL_OBJ_STATIC_INIT(opal_mutex_t),                                   \
                .m_lock = OPAL_THREAD_INTERNAL_RECURSIVE_MUTEX_INITIALIZER, .m_lock_debug = 0, \
                .m_lock_file = NULL, .m_lock_line = 0, .m_lock_atomic = OPAL_ATOMIC_LOCK_INIT, \
            }
#    else
#        define OPAL_RECURSIVE_MUTEX_STATIC_INIT                            \
            {                                                               \
                .super = OPAL_OBJ_STATIC_INIT(opal_mutex_t),                \
                .m_lock = OPAL_THREAD_INTERNAL_RECURSIVE_MUTEX_INITIALIZER, \
                .m_lock_atomic = OPAL_ATOMIC_LOCK_INIT,                     \
            }
#    endif
#endif /* OPAL_THREAD_INTERNAL_RECURSIVE_MUTEX_INITIALIZER */

/**
 * Try to acquire a mutex.
 *
 * @param mutex         Address of the mutex.
 * @return              0 if the mutex was acquired, 1 otherwise.
 */
static inline int opal_mutex_trylock(opal_mutex_t *mutex)
{
    return opal_thread_internal_mutex_trylock(&mutex->m_lock);
}

/**
 * Acquire a mutex.
 *
 * @param mutex         Address of the mutex.
 */
static inline void opal_mutex_lock(opal_mutex_t *mutex)
{
    opal_thread_internal_mutex_lock(&mutex->m_lock);
}

/**
 * Release a mutex.
 *
 * @param mutex         Address of the mutex.
 */
static inline void opal_mutex_unlock(opal_mutex_t *mutex)
{
    opal_thread_internal_mutex_unlock(&mutex->m_lock);
}

/**
 * Try to acquire a mutex using atomic operations.
 *
 * @param mutex         Address of the mutex.
 * @return              0 if the mutex was acquired, 1 otherwise.
 */
static inline int opal_mutex_atomic_trylock(opal_mutex_t *mutex)
{
    return opal_atomic_trylock(&mutex->m_lock_atomic);
}

/**
 * Acquire a mutex using atomic operations.
 *
 * @param mutex         Address of the mutex.
 */
static inline void opal_mutex_atomic_lock(opal_mutex_t *mutex)
{
    opal_atomic_lock(&mutex->m_lock_atomic);
}

/**
 * Release a mutex using atomic operations.
 *
 * @param mutex         Address of the mutex.
 */
static inline void opal_mutex_atomic_unlock(opal_mutex_t *mutex)
{
    opal_atomic_unlock(&mutex->m_lock_atomic);
}

/**
 * Lock a mutex if opal_using_threads() says that multiple threads may
 * be active in the process.
 *
 * @param mutex Pointer to a opal_mutex_t to lock.
 *
 * If there is a possibility that multiple threads are running in the
 * process (as determined by opal_using_threads()), this function will
 * block waiting to lock the mutex.
 *
 * If there is no possibility that multiple threads are running in the
 * process, return immediately.
 */
#define OPAL_THREAD_LOCK(mutex)                    \
    do {                                           \
        if (OPAL_UNLIKELY(opal_using_threads())) { \
            opal_mutex_lock(mutex);                \
        }                                          \
    } while (0)

/**
 * Try to lock a mutex if opal_using_threads() says that multiple
 * threads may be active in the process.
 *
 * @param mutex Pointer to a opal_mutex_t to trylock
 *
 * If there is a possibility that multiple threads are running in the
 * process (as determined by opal_using_threads()), this function will
 * trylock the mutex.
 *
 * If there is no possibility that multiple threads are running in the
 * process, return immediately without modifying the mutex.
 *
 * Returns 0 if mutex was locked, non-zero otherwise.
 */
#define OPAL_THREAD_TRYLOCK(mutex) \
    (OPAL_UNLIKELY(opal_using_threads()) ? opal_mutex_trylock(mutex) : 0)

/**
 * Unlock a mutex if opal_using_threads() says that multiple threads
 * may be active in the process.
 *
 * @param mutex Pointer to a opal_mutex_t to unlock.
 *
 * If there is a possibility that multiple threads are running in the
 * process (as determined by opal_using_threads()), this function will
 * unlock the mutex.
 *
 * If there is no possibility that multiple threads are running in the
 * process, return immediately without modifying the mutex.
 */
#define OPAL_THREAD_UNLOCK(mutex)                  \
    do {                                           \
        if (OPAL_UNLIKELY(opal_using_threads())) { \
            opal_mutex_unlock(mutex);              \
        }                                          \
    } while (0)

/**
 * Lock a mutex if opal_using_threads() says that multiple threads may
 * be active in the process for the duration of the specified action.
 *
 * @param mutex    Pointer to a opal_mutex_t to lock.
 * @param action   A scope over which the lock is held.
 *
 * If there is a possibility that multiple threads are running in the
 * process (as determined by opal_using_threads()), this function will
 * acquire the lock before invoking the specified action and release
 * it on return.
 *
 * If there is no possibility that multiple threads are running in the
 * process, invoke the action without acquiring the lock.
 */
#define OPAL_THREAD_SCOPED_LOCK(mutex, action)     \
    do {                                           \
        if (OPAL_UNLIKELY(opal_using_threads())) { \
            opal_mutex_lock(mutex);                \
            action;                                \
            opal_mutex_unlock(mutex);              \
        } else {                                   \
            action;                                \
        }                                          \
    } while (0)

typedef opal_thread_internal_cond_t opal_cond_t;
#define OPAL_CONDITION_STATIC_INIT OPAL_THREAD_INTERNAL_COND_INITIALIZER
int opal_cond_init(opal_cond_t *cond);
int opal_cond_wait(opal_cond_t *cond, opal_mutex_t *lock);
int opal_cond_broadcast(opal_cond_t *cond);
int opal_cond_signal(opal_cond_t *cond);
int opal_cond_destroy(opal_cond_t *cond);

END_C_DECLS

#endif /* OPAL_MCA_THREADS_MUTEX_H */
