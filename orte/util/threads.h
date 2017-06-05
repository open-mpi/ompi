/*
 * Copyright (c) 2017      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef ORTE_THREADS_H
#define ORTE_THREADS_H

#include "orte_config.h"

#include "opal/sys/atomic.h"

/* provide macros for forward-proofing the shifting
 * of objects between threads - at some point, we
 * may revamp our threading model */

/* post an object to another thread - for now, we
 * only have a memory barrier */
#define ORTE_POST_OBJECT(o)     opal_atomic_wmb()

/* acquire an object from another thread - for now,
 * we only have a memory barrier */
#define ORTE_ACQUIRE_OBJECT(o)  opal_atomic_rmb()

/* define a threadshift macro */
#define ORTE_THREADSHIFT(x, eb, f, p)                                   \
    do {                                                                \
        opal_event_set((eb), &((x)->ev), -1, OPAL_EV_WRITE, (f), (x));  \
        opal_event_set_priority(&((x)->ev), (p));                       \
        ORTE_POST_OBJECT((x));                                          \
        opal_event_active(&((x)->ev), OPAL_EV_WRITE, 1);                \
    } while(0)

#endif /* ORTE_THREADS_H */
