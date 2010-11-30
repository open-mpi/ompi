/* 
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#ifndef ORTE_CONDITION_H
#define ORTE_CONDITION_H

#include "orte_config.h"
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_TIME_H
#include <time.h>
#endif
#if OPAL_HAVE_POSIX_THREADS
#include <pthread.h>
#elif OPAL_HAVE_SOLARIS_THREADS
#include <thread.h>
#include <synch.h>
#endif

#include "opal/threads/threads.h"
#include "opal/runtime/opal_cr.h"

#include "orte/runtime/orte_globals.h"

BEGIN_C_DECLS

static inline int orte_condition_wait(opal_condition_t *c, opal_mutex_t *m)
{
    int rc = 0;
    c->c_waiting++;

/*
#if OPAL_HAVE_POSIX_THREADS
    if (orte_progress_threads_enabled) {
        rc = pthread_cond_wait(&c->c_cond, &m->m_lock_pthread);
    } else {
#endif
*/
        if (c->c_signaled) {
            c->c_waiting--;
            opal_mutex_unlock(m);
            opal_progress();
            OPAL_CR_TEST_CHECKPOINT_READY_STALL();
            opal_mutex_lock(m);
            return 0;
        }
        while (c->c_signaled == 0) {
            opal_mutex_unlock(m);
            opal_progress();
            OPAL_CR_TEST_CHECKPOINT_READY_STALL();
            opal_mutex_lock(m);
        }
/*
#if OPAL_HAVE_POSIX_THREADS
    }
#endif
 */

    c->c_signaled--;
    c->c_waiting--;
    return rc;
}

#if OPAL_ENABLE_DEBUG
#define ORTE_CONDITION_WAIT(x, y)                                     \
    do {                                                              \
        if (opal_debug_threads) {                                     \
            opal_output(0, "Entering condition wait for %s at %s:%d", \
                        (NULL == (x)->name) ? "NULL" : (x)->name,     \
                        __FILE__, __LINE__);                          \
        }                                                             \
        orte_condition_wait((x), (y));                                \
    } while (0);
#else
#define ORTE_CONDITION_WAIT(x, y)  orte_condition_wait(x, y)
#endif

static inline int orte_condition_timedwait(opal_condition_t *c,
                                           opal_mutex_t *m,
                                           const struct timespec *abstime)
{
    int rc = 0;
    struct timeval tv;
    struct timeval absolute;

    c->c_waiting++;

/*
#if OPAL_HAVE_POSIX_THREADS
    if (orte_progress_threads_enabled) {
        rc = pthread_cond_timedwait(&c->c_cond, &m->m_lock_pthread, abstime);
    } else {
#endif
*/
        absolute.tv_sec = abstime->tv_sec;
        absolute.tv_usec = abstime->tv_nsec * 1000;
        gettimeofday(&tv,NULL);
        if (c->c_signaled == 0) {
            do {
                opal_mutex_unlock(m);
                opal_progress();
                gettimeofday(&tv,NULL);
                opal_mutex_lock(m);
            } while (c->c_signaled == 0 &&  
                     (tv.tv_sec <= absolute.tv_sec ||
                      (tv.tv_sec == absolute.tv_sec && tv.tv_usec < absolute.tv_usec)));
        }
/*
#if OPAL_HAVE_POSIX_THREADS
    }
#endif
*/
    if (c->c_signaled != 0) c->c_signaled--;
    c->c_waiting--;
    return rc;
}

static inline int orte_condition_signal(opal_condition_t *c)
{
    if (c->c_waiting) {
        c->c_signaled++;
/*
#if OPAL_HAVE_POSIX_THREADS
    if (orte_progress_threads_enabled) {
        pthread_cond_signal(&c->c_cond);
    }
#endif
*/
    }
    return 0;
}

#if OPAL_ENABLE_DEBUG
#define ORTE_CONDITION_SIGNAL(x)                                        \
    do {                                                                \
        if (opal_debug_threads) {                                       \
            opal_output(0, "Signaling condition %s at %s:%d",           \
                        (NULL == (x)->name) ? "NULL" : (x)->name,       \
                        __FILE__, __LINE__);                            \
        }                                                               \
        orte_condition_signal((x));                                     \
    } while(0);
#else
#define ORTE_CONDITION_SIGNAL(x) orte_condition_signal(x)
#endif

static inline int orte_condition_broadcast(opal_condition_t *c)
{
    c->c_signaled = c->c_waiting;
 /*
#if OPAL_HAVE_POSIX_THREADS
    if (orte_progress_threads_enabled) {
        if( 1 == c->c_waiting ) {
            pthread_cond_signal(&c->c_cond);
        } else {
            pthread_cond_broadcast(&c->c_cond);
        }
    }
#endif
 */
    return 0;
}

#if OPAL_ENABLE_DEBUG
#define ORTE_CONDITION_BROADCAST(x)                                     \
    do {                                                                \
        if (opal_debug_threads) {                                       \
            opal_output(0, "Broadcasting condition %s at %s:%d",        \
                (NULL == (x)->name) ? "NULL" : (x)->name,               \
                        __FILE__, __LINE__);                            \
        }                                                               \
        orte_condition_broadcast((x));                                  \
    } while(0);
#else
#define ORTE_CONDITION_BROADCAST(x) orte_condition_broadcast(x)
#endif

END_C_DECLS

#endif

