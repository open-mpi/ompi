/* 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "threads/condition.h"
#include "threads/mutex.h"
#include "runtime/ompi_progress.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
struct ompi_condition_t {
    ompi_object_t super;
    volatile int c_waiting;
    volatile int c_signaled;
};
typedef struct ompi_condition_t ompi_condition_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_condition_t);


static inline int ompi_condition_wait(ompi_condition_t *c, ompi_mutex_t *m)
{
    c->c_waiting++;
    if (ompi_using_threads()) {
        while (c->c_signaled == 0) {
            ompi_mutex_unlock(m);
            ompi_progress();
            ompi_mutex_lock(m);
        }
    } else {
        while (c->c_signaled == 0) {
            ompi_progress();
        }
    }
    c->c_signaled--;
    c->c_waiting--;
    return 0;
}

static inline int ompi_condition_timedwait(ompi_condition_t *c,
                                           ompi_mutex_t *m,
                                           const struct timespec *abstime)
{
    struct timeval tv;
    struct timeval abs;
    abs.tv_sec = abstime->tv_sec;
    abs.tv_usec = abstime->tv_nsec * 1000;
    gettimeofday(&tv,NULL);

    c->c_waiting++;
    if (ompi_using_threads()) {
        while (c->c_signaled == 0 &&  
               (tv.tv_sec <= abs.tv_sec ||
               (tv.tv_sec == abs.tv_sec && tv.tv_usec < abs.tv_usec))) {
            ompi_mutex_unlock(m);
            ompi_progress();
            gettimeofday(&tv,NULL);
            ompi_mutex_lock(m);
        }
    } else {
        while (c->c_signaled == 0 &&  
               (tv.tv_sec <= abs.tv_sec ||
               (tv.tv_sec == abs.tv_sec && tv.tv_usec < abs.tv_usec))) {
            ompi_progress();
            gettimeofday(&tv,NULL);
        }
    }
    c->c_signaled--;
    c->c_waiting--;
    return 0;
}

static inline int ompi_condition_signal(ompi_condition_t *c)
{
    if (c->c_waiting) {
        c->c_signaled++;
    }
    return 0;
}

static inline int ompi_condition_broadcast(ompi_condition_t *c)
{
    c->c_signaled += c->c_waiting;
    return 0;
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

