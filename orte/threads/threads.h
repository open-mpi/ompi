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
 * Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_THREAD_H
#define ORTE_THREAD_H

#include "orte_config.h"

#include "opal/class/opal_object.h"
#if OPAL_ENABLE_DEBUG
#include "opal/util/output.h"
#endif

#include "mutex.h"
#include "condition.h"

BEGIN_C_DECLS

typedef struct {
    opal_object_t super;
    opal_mutex_t lock;
    opal_condition_t cond;
    volatile bool active;
    volatile bool running;
    volatile bool stop;
    struct timeval rate;
} orte_thread_ctl_t;
OBJ_CLASS_DECLARATION(orte_thread_ctl_t);

ORTE_DECLSPEC extern bool orte_debug_threads;

#if OPAL_ENABLE_DEBUG
#define ORTE_ACQUIRE_THREAD(ctl)                                \
    do {                                                        \
        ORTE_THREAD_LOCK(&(ctl)->lock);                         \
        if (orte_debug_threads) {                               \
            opal_output(0, "Waiting for thread %s:%d",          \
                        __FILE__, __LINE__);                    \
        }                                                       \
        while ((ctl)->active) {                                 \
            orte_condition_wait(&(ctl)->cond, &(ctl)->lock);    \
        }                                                       \
        if (orte_debug_threads) {                               \
            opal_output(0, "Thread obtained %s:%d",             \
                        __FILE__, __LINE__);                    \
        }                                                       \
        (ctl)->active = true;                                   \
    } while(0);
#else
#define ORTE_ACQUIRE_THREAD(ctl)                                \
    do {                                                        \
        ORTE_THREAD_LOCK(&(ctl)->lock);                         \
        while ((ctl)->active) {                                 \
            orte_condition_wait(&(ctl)->cond, &(ctl)->lock);    \
        }                                                       \
        (ctl)->active = true;                                   \
    } while(0);
#endif


#if OPAL_ENABLE_DEBUG
#define ORTE_RELEASE_THREAD(ctl)                        \
    do {                                                \
        if (orte_debug_threads) {                       \
            opal_output(0, "Releasing thread %s:%d",    \
                        __FILE__, __LINE__);            \
        }                                               \
        (ctl)->active = false;                          \
        orte_condition_broadcast(&(ctl)->cond);         \
        ORTE_THREAD_UNLOCK(&(ctl)->lock);               \
    } while(0);
#else
#define ORTE_RELEASE_THREAD(ctl)                        \
    do {                                                \
        (ctl)->active = false;                          \
        orte_condition_broadcast(&(ctl)->cond);         \
        ORTE_THREAD_UNLOCK(&(ctl)->lock);               \
    } while(0);
#endif


#define ORTE_WAKEUP_THREAD(ctl)                     \
    do {                                            \
        (ctl)->active = false;                      \
        orte_condition_broadcast(&(ctl)->cond);     \
    } while(0);


END_C_DECLS

#endif /* ORTE_THREAD_H */
