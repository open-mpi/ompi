/* 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#ifndef OMPI_CONDITION_SPINLOCK_H
#define OMPI_CONDITION_SPINLOCK_H

#include "ompi_config.h"
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#if OMPI_HAVE_POSIX_THREADS
#include <pthread.h>
#endif

#include "threads/condition.h"
#include "threads/mutex.h"
#include "runtime/ompi_progress.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Combine pthread support w/ polled progress to allow run-time selection
 * of threading vs. non-threading progress.
 */

struct ompi_condition_t {
    ompi_object_t super;
    volatile int c_waiting;
    volatile int c_signaled;
#if OMPI_HAVE_POSIX_THREADS
    pthread_cond_t c_cond;
#endif
};
typedef struct ompi_condition_t ompi_condition_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_condition_t);


static inline int ompi_condition_wait(ompi_condition_t *c, ompi_mutex_t *m)
{
    int rc = 0;
    c->c_waiting++;
    if (ompi_using_threads()) {
#if OMPI_HAVE_POSIX_THREADS
        rc = pthread_cond_wait(&c->c_cond, &m->m_lock_pthread);
#else
        while (c->c_signaled == 0) {
            ompi_mutex_unlock(m);
            ompi_progress();
            ompi_mutex_lock(m);
        }
#endif
    } else {
        while (c->c_signaled == 0) {
            ompi_progress();
        }
    }
    c->c_signaled--;
    c->c_waiting--;
    return rc;
}

static inline int ompi_condition_timedwait(ompi_condition_t *c,
                                           ompi_mutex_t *m,
                                           const struct timespec *abstime)
{
    struct timeval tv;
    struct timeval abs;
    int rc = 0;

    c->c_waiting++;
    if (ompi_using_threads()) {
#if OMPI_HAVE_POSIX_THREADS
        rc = pthread_cond_timedwait(&c->c_cond, &m->m_lock_pthread, abstime);
#else
        abs.tv_sec = abstime->tv_sec;
        abs.tv_usec = abstime->tv_nsec * 1000;
        gettimeofday(&tv,NULL);
        while (c->c_signaled == 0 &&  
               (tv.tv_sec <= abs.tv_sec ||
               (tv.tv_sec == abs.tv_sec && tv.tv_usec < abs.tv_usec))) {
            ompi_mutex_unlock(m);
            ompi_progress();
            gettimeofday(&tv,NULL);
            ompi_mutex_lock(m);
        }
#endif
    } else {
        abs.tv_sec = abstime->tv_sec;
        abs.tv_usec = abstime->tv_nsec * 1000;
        gettimeofday(&tv,NULL);
        while (c->c_signaled == 0 &&  
               (tv.tv_sec <= abs.tv_sec ||
               (tv.tv_sec == abs.tv_sec && tv.tv_usec < abs.tv_usec))) {
            ompi_progress();
            gettimeofday(&tv,NULL);
        }
    }
    c->c_signaled--;
    c->c_waiting--;
    return rc;
}

static inline int ompi_condition_signal(ompi_condition_t *c)
{
    if (c->c_waiting) {
        c->c_signaled++;
#if OMPI_HAVE_POSIX_THREADS
        if(ompi_using_threads()) {
            pthread_cond_signal(&c->c_cond);
        }
#endif
    }
    return 0;
}

static inline int ompi_condition_broadcast(ompi_condition_t *c)
{
    c->c_signaled += c->c_waiting;
#if OMPI_HAVE_POSIX_THREADS
    if(ompi_using_threads()) {
        pthread_cond_broadcast(&c->c_cond);
    }
#endif
    return 0;
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

