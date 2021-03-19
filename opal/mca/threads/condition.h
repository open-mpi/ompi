/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2019      Triad National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_THREADS_CONDITION_H
#define OPAL_MCA_THREADS_CONDITION_H

#include "opal_config.h"
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif
#include <pthread.h>
#include <time.h>

#include "opal/constants.h"
#include "opal/mca/threads/mutex.h"
#include "opal/runtime/opal_progress.h"

BEGIN_C_DECLS

/*
 * Combine pthread support w/ polled progress to allow run-time selection
 * of threading vs. non-threading progress.
 */

struct opal_condition_t {
    opal_object_t super;
    volatile int c_waiting;
    volatile int c_signaled;
};
typedef struct opal_condition_t opal_condition_t;

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_condition_t);

static inline int opal_condition_wait(opal_condition_t *c, opal_mutex_t *m)
{
    int rc = OPAL_SUCCESS;
    c->c_waiting++;

    if (opal_using_threads()) {
        if (c->c_signaled) {
            c->c_waiting--;
            opal_mutex_unlock(m);
            opal_progress();
            opal_mutex_lock(m);
            return rc;
        }
        while (0 == c->c_signaled) {
            opal_mutex_unlock(m);
            opal_progress();
            opal_mutex_lock(m);
        }
    } else {
        while (0 == c->c_signaled) {
            opal_progress();
        }
    }

    c->c_signaled--;
    c->c_waiting--;
    return rc;
}

static inline int opal_condition_timedwait(opal_condition_t *c, opal_mutex_t *m,
                                           const struct timespec *abstime)
{
    struct timeval tv;
    struct timeval absolute;
    int rc = OPAL_SUCCESS;

    c->c_waiting++;
    if (opal_using_threads()) {
        absolute.tv_sec = abstime->tv_sec;
        absolute.tv_usec = abstime->tv_nsec / 1000;
        gettimeofday(&tv, NULL);
        if (0 == c->c_signaled) {
            do {
                opal_mutex_unlock(m);
                opal_progress();
                gettimeofday(&tv, NULL);
                opal_mutex_lock(m);
            } while (0 == c->c_signaled
                     && (tv.tv_sec <= absolute.tv_sec
                         || (tv.tv_sec == absolute.tv_sec && tv.tv_usec < absolute.tv_usec)));
        }
    } else {
        absolute.tv_sec = abstime->tv_sec;
        absolute.tv_usec = abstime->tv_nsec / 1000;
        gettimeofday(&tv, NULL);
        if (0 == c->c_signaled) {
            do {
                opal_progress();
                gettimeofday(&tv, NULL);
            } while (0 == c->c_signaled
                     && (tv.tv_sec <= absolute.tv_sec
                         || (tv.tv_sec == absolute.tv_sec && tv.tv_usec < absolute.tv_usec)));
        }
    }

    if (0 != c->c_signaled) {
        c->c_signaled--;
    }
    c->c_waiting--;
    return rc;
}

static inline int opal_condition_signal(opal_condition_t *c)
{
    if (c->c_waiting) {
        c->c_signaled++;
    }
    return OPAL_SUCCESS;
}

static inline int opal_condition_broadcast(opal_condition_t *c)
{
    c->c_signaled = c->c_waiting;
    return OPAL_SUCCESS;
}

END_C_DECLS

#endif /* OPAL_MCA_THREADS_CONDITION_H */
