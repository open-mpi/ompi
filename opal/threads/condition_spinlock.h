/* 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#ifndef OMPI_CONDITION_SPINLOCK_H
#define OMPI_CONDITION_SPINLOCK_H

#include "ompi_config.h"
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include "opal/threads/condition.h"
#include "opal/threads/mutex.h"
#include "opal/runtime/opal_progress.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


struct opal_condition_t {
    opal_object_t super;
    volatile int c_waiting;
    volatile int c_signaled;
};
typedef struct opal_condition_t opal_condition_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(opal_condition_t);


static inline int opal_condition_wait(opal_condition_t *c, opal_mutex_t *m)
{
    c->c_waiting++;
    if (opal_using_threads()) {
        while (c->c_signaled == 0) {
            opal_mutex_unlock(m);
            opal_progress();
            opal_mutex_lock(m);
        }
    } else {
        while (c->c_signaled == 0) {
            opal_progress();
        }
    }
    c->c_signaled--;
    c->c_waiting--;
    return 0;
}

static inline int opal_condition_timedwait(opal_condition_t *c,
                                           opal_mutex_t *m,
                                           const struct timespec *abstime)
{
    struct timeval tv;
    struct timeval abs;
    abs.tv_sec = abstime->tv_sec;
    abs.tv_usec = abstime->tv_nsec / 1000;
    gettimeofday(&tv,NULL);

    c->c_waiting++;
    if (opal_using_threads()) {
        while (c->c_signaled == 0 &&  
               (tv.tv_sec <= abs.tv_sec ||
               (tv.tv_sec == abs.tv_sec && tv.tv_usec < abs.tv_usec))) {
            opal_mutex_unlock(m);
            opal_progress();
            gettimeofday(&tv,NULL);
            opal_mutex_lock(m);
        }
    } else {
        while (c->c_signaled == 0 &&  
               (tv.tv_sec <= abs.tv_sec ||
               (tv.tv_sec == abs.tv_sec && tv.tv_usec < abs.tv_usec))) {
            opal_progress();
            gettimeofday(&tv,NULL);
        }
    }
    c->c_signaled--;
    c->c_waiting--;
    return 0;
}

static inline int opal_condition_signal(opal_condition_t *c)
{
    if (c->c_waiting) {
        c->c_signaled++;
    }
    return 0;
}

static inline int opal_condition_broadcast(opal_condition_t *c)
{
    c->c_signaled += c->c_waiting;
    return 0;
}

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

