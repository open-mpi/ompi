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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#ifndef OPAL_CONDITION_SPINLOCK_H
#define OPAL_CONDITION_SPINLOCK_H

#include "opal_config.h"
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_TIME_H
#include <time.h>
#endif
#if OMPI_HAVE_POSIX_THREADS
#include <pthread.h>
#endif

#include "opal/threads/condition.h"
#include "opal/threads/mutex.h"
#include "opal/runtime/opal_progress.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Combine pthread support w/ polled progress to allow run-time selection
 * of threading vs. non-threading progress.
 */

struct opal_condition_t {
    opal_object_t super;
    volatile int c_waiting;
    volatile int c_signaled;
#if OMPI_HAVE_POSIX_THREADS
    pthread_cond_t c_cond;
#endif
};
typedef struct opal_condition_t opal_condition_t;

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_condition_t);


static inline int opal_condition_wait(opal_condition_t *c, opal_mutex_t *m)
{
    int rc = 0;
    c->c_waiting++;
    if (opal_using_threads()) {
#if OMPI_HAVE_POSIX_THREADS && OMPI_ENABLE_PROGRESS_THREADS
        rc = pthread_cond_wait(&c->c_cond, &m->m_lock_pthread);
#else
        if (c->c_signaled) {
            c->c_waiting--;
            opal_mutex_unlock(m);
            opal_progress();
            opal_mutex_lock(m);
            return 0;
        }
        while (c->c_signaled == 0) {
            opal_mutex_unlock(m);
            opal_progress();
            opal_mutex_lock(m);
        }
#endif
    } else {
        while (c->c_signaled == 0) {
            opal_progress();
        }
    }
    c->c_signaled--;
    c->c_waiting--;
    return rc;
}

static inline int opal_condition_timedwait(opal_condition_t *c,
                                           opal_mutex_t *m,
                                           const struct timespec *abstime)
{
    struct timeval tv;
    struct timeval abs;
    int rc = 0;

    c->c_waiting++;
    if (opal_using_threads()) {
#if OMPI_HAVE_POSIX_THREADS && OMPI_ENABLE_PROGRESS_THREADS
        rc = pthread_cond_timedwait(&c->c_cond, &m->m_lock_pthread, abstime);
#else
        abs.tv_sec = abstime->tv_sec;
        abs.tv_usec = abstime->tv_nsec * 1000;
        gettimeofday(&tv,NULL);
        while (c->c_signaled == 0 &&  
               (tv.tv_sec <= abs.tv_sec ||
               (tv.tv_sec == abs.tv_sec && tv.tv_usec < abs.tv_usec))) {
            opal_mutex_unlock(m);
            opal_progress();
            gettimeofday(&tv,NULL);
            opal_mutex_lock(m);
        }
#endif
    } else {
        abs.tv_sec = abstime->tv_sec;
        abs.tv_usec = abstime->tv_nsec * 1000;
        gettimeofday(&tv,NULL);
        while (c->c_signaled == 0 &&  
               (tv.tv_sec <= abs.tv_sec ||
               (tv.tv_sec == abs.tv_sec && tv.tv_usec < abs.tv_usec))) {
            opal_progress();
            gettimeofday(&tv,NULL);
        }
    }
    c->c_signaled--;
    c->c_waiting--;
    return rc;
}

static inline int opal_condition_signal(opal_condition_t *c)
{
    if (c->c_waiting) {
        c->c_signaled++;
#if OMPI_HAVE_POSIX_THREADS && OMPI_ENABLE_PROGRESS_THREADS
        if(opal_using_threads()) {
            pthread_cond_signal(&c->c_cond);
        }
#endif
    }
    return 0;
}

static inline int opal_condition_broadcast(opal_condition_t *c)
{
    c->c_signaled = c->c_waiting;
#if OMPI_HAVE_POSIX_THREADS && OMPI_ENABLE_PROGRESS_THREADS
    if(opal_using_threads()) {
        if( 1 == c->c_waiting ) {
            pthread_cond_signal(&c->c_cond);
        } else {
            pthread_cond_broadcast(&c->c_cond);
        }
    }
#endif
    return 0;
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

