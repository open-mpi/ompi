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
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef PMIX_CONDITION_SPINLOCK_H
#define PMIX_CONDITION_SPINLOCK_H

#include "pmix_config.h"
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <time.h>
#include <pthread.h>

#include "src/threads/mutex.h"

BEGIN_C_DECLS

struct pmix_condition_t {
    pmix_object_t super;
    volatile int c_waiting;
    volatile int c_signaled;
};
typedef struct pmix_condition_t pmix_condition_t;

PMIX_EXPORT PMIX_CLASS_DECLARATION(pmix_condition_t);


static inline int pmix_condition_wait(pmix_condition_t *c, pmix_mutex_t *m)
{
    int rc = 0;
    c->c_waiting++;

    if (c->c_signaled) {
        c->c_waiting--;
        return 0;
    }

    c->c_signaled--;
    c->c_waiting--;
    return rc;
}

static inline int pmix_condition_signal(pmix_condition_t *c)
{
    if (c->c_waiting) {
        c->c_signaled++;
    }
    return 0;
}

static inline int pmix_condition_broadcast(pmix_condition_t *c)
{
    c->c_signaled = c->c_waiting;
    return 0;
}

END_C_DECLS

#endif
