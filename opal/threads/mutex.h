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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2007      Voltaire. All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef  OPAL_MUTEX_H
#define  OPAL_MUTEX_H 1

#include "opal_config.h"

#if OPAL_ENABLE_DEBUG
#include "opal/util/output.h"
#endif

#include "opal/threads/thread_usage.h"

BEGIN_C_DECLS

/**
 * @file:
 *
 * Mutual exclusion functions.
 *
 * Functions for locking of critical sections.
 */

#if OPAL_ENABLE_DEBUG
OPAL_DECLSPEC extern bool opal_debug_threads;
#endif

/**
 * Opaque mutex object
 */
typedef struct opal_mutex_t opal_mutex_t;


/**
 * Try to acquire a mutex.
 *
 * @param mutex         Address of the mutex.
 * @return              0 if the mutex was acquired, 1 otherwise.
 */
static inline int opal_mutex_trylock(opal_mutex_t *mutex);


/**
 * Acquire a mutex.
 *
 * @param mutex         Address of the mutex.
 */
static inline void opal_mutex_lock(opal_mutex_t *mutex);


/**
 * Release a mutex.
 *
 * @param mutex         Address of the mutex.
 */
static inline void opal_mutex_unlock(opal_mutex_t *mutex);


/**
 * Try to acquire a mutex using atomic operations.
 *
 * @param mutex         Address of the mutex.
 * @return              0 if the mutex was acquired, 1 otherwise.
 */
static inline int opal_mutex_atomic_trylock(opal_mutex_t *mutex);


/**
 * Acquire a mutex using atomic operations.
 *
 * @param mutex         Address of the mutex.
 */
static inline void opal_mutex_atomic_lock(opal_mutex_t *mutex);


/**
 * Release a mutex using atomic operations.
 *
 * @param mutex         Address of the mutex.
 */
static inline void opal_mutex_atomic_unlock(opal_mutex_t *mutex);

END_C_DECLS

#include "mutex_unix.h"

BEGIN_C_DECLS

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
#if OMPI_ENABLE_THREAD_MULTIPLE
#define OPAL_THREAD_LOCK(mutex)                 \
    do {                                        \
        if (opal_using_threads()) {             \
            opal_mutex_lock(mutex);             \
        }                                       \
    } while (0)
#elif OPAL_ENABLE_DEBUG
#define OPAL_THREAD_LOCK(mutex)                                         \
    do {                                                                \
        (mutex)->m_lock_debug++;                                        \
        if (opal_debug_threads && 1 != (mutex)->m_lock_debug) {     \
            opal_output(0, "Warning -- mutex already locked at %s:%d,"  \
                        " now at %s:%d",                                \
                        (mutex)->m_lock_file,                           \
                        (mutex)->m_lock_line,                           \
                        __FILE__, __LINE__);                            \
        }                                                               \
        (mutex)->m_lock_file = __FILE__;                                \
        (mutex)->m_lock_line = __LINE__;                                \
    } while (0)
#else
#define OPAL_THREAD_LOCK(mutex)
#endif


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
#if OMPI_ENABLE_THREAD_MULTIPLE
#define OPAL_THREAD_TRYLOCK(mutex)                      \
    (opal_using_threads() ? opal_mutex_trylock(mutex) : 0)
#elif OPAL_ENABLE_DEBUG
static inline int
opal_thread_debug_trylock(opal_mutex_t *mutex, const char *file, int line)
{
    int ret = -1;

    if (0 == (mutex)->m_lock_debug) {
        (mutex)->m_lock_debug++;
        (mutex)->m_lock_file = file;
        (mutex)->m_lock_line = line;
        ret = 0;
    } else {
        if (opal_debug_threads) {
            opal_output(0, "Warning -- during trylock, mutex already locked at %s:%d "
                        "now at %s:%d",  
                        file, line,
                        (mutex)->m_lock_file,
                        (mutex)->m_lock_line);
        }
    }

    return ret;
}
#define OPAL_THREAD_TRYLOCK(mutex) opal_thread_debug_trylock(mutex, __FILE__, __LINE__)
#else
#define OPAL_THREAD_TRYLOCK(mutex) 0
#endif

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
#if OMPI_ENABLE_THREAD_MULTIPLE
#define OPAL_THREAD_UNLOCK(mutex)               \
    do {                                        \
        if (opal_using_threads()) {             \
            opal_mutex_unlock(mutex);           \
        }                                       \
    } while (0)
#elif OPAL_ENABLE_DEBUG
#define OPAL_THREAD_UNLOCK(mutex)                                       \
    do {                                                                \
        (mutex)->m_lock_debug--;                                        \
        if (opal_debug_threads && 0 > (mutex)->m_lock_debug) {      \
            opal_output(0, "Warning -- mutex was double locked from %s:%d", \
                        __FILE__, __LINE__);                            \
        } else if (opal_debug_threads && 0 > (mutex)->m_lock_debug) { \
            opal_output(0, "Warning -- mutex not locked from %s:%d",    \
                        __FILE__, __LINE__);                            \
        } else {                                                        \
            (mutex)->m_lock_file = NULL;                                \
            (mutex)->m_lock_line = 0;                                   \
        }                                                               \
    } while (0)
#else
#define OPAL_THREAD_UNLOCK(mutex)
#endif


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
#if OMPI_ENABLE_THREAD_MULTIPLE
#define OPAL_THREAD_SCOPED_LOCK(mutex, action)  \
    do {                                        \
        if(opal_using_threads()) {              \
            opal_mutex_lock(mutex);             \
            (action);                           \
            opal_mutex_unlock(mutex);           \
        } else {                                \
            (action);                           \
        }                                       \
    } while (0)
#elif OPAL_ENABLE_DEBUG
#define OPAL_THREAD_SCOPED_LOCK(mutex, action)                          \
    do {                                                                \
        if (0 != (mutex)->m_lock_debug) {                               \
            opal_output(0, "scoped_lock: Warning -- mutex already "     \
                        "locked at %s:%d, now at %s:%d",                \
                        __FILE__, __LINE__,                             \
                        (mutex)->m_lock_file,                           \
                        (mutex)->m_lock_line);                          \
        }                                                               \
        (mutex)->m_lock_debug--;                                        \
        (action);                                                       \
        (mutex)->m_lock_debug++;                                        \
    } while (0)
#else
#define OPAL_THREAD_SCOPED_LOCK(mutex, action) (action)
#endif

END_C_DECLS

#endif                          /* OPAL_MUTEX_H */
