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
#include "opal/util/fd.h"
#include "opal/mca/event/event.h"

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
    opal_event_base_t *evbase;
    char *name;
} orte_thread_ctl_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_thread_ctl_t);

#if OPAL_ENABLE_DEBUG
#define ORTE_ACQUIRE_THREAD(ctl)                                       \
    do {                                                               \
        ORTE_THREAD_LOCK(&(ctl)->lock);                                \
        if (opal_debug_threads) {                                      \
            opal_output(0, "Waiting for thread %s at %s:%d:%s",        \
                        (NULL == (ctl)->name) ? "NULL" : (ctl)->name,  \
                        __FILE__, __LINE__,                            \
                        ((ctl)->active) ? "TRUE" : "FALSE");           \
        }                                                              \
        while ((ctl)->active) {                                        \
            ORTE_CONDITION_WAIT(&(ctl)->cond, &(ctl)->lock);           \
        }                                                              \
        if (opal_debug_threads) {                                      \
            opal_output(0, "Thread %s acquired at %s:%d",              \
                        (NULL == (ctl)->name) ? "NULL" : (ctl)->name,  \
                        __FILE__, __LINE__);                           \
        }                                                              \
        (ctl)->active = true;                                          \
    } while(0);
#else
#define ORTE_ACQUIRE_THREAD(ctl)                                \
    do {                                                        \
        ORTE_THREAD_LOCK(&(ctl)->lock);                         \
        while ((ctl)->active) {                                 \
            ORTE_CONDITION_WAIT(&(ctl)->cond, &(ctl)->lock);    \
        }                                                       \
        (ctl)->active = true;                                   \
    } while(0);
#endif


#if OPAL_ENABLE_DEBUG
#define ORTE_RELEASE_THREAD(ctl)                                       \
    do {                                                               \
        if (opal_debug_threads) {                                      \
            opal_output(0, "Releasing thread %s at %s:%d",             \
                        (NULL == (ctl)->name) ? "NULL" : (ctl)->name,  \
                        __FILE__, __LINE__);                           \
        }                                                              \
        (ctl)->active = false;                                         \
        ORTE_CONDITION_BROADCAST(&(ctl)->cond);                        \
        OPAL_UPDATE_EVBASE((ctl)->evbase, NULL, OPAL_EVENT_NOOP);      \
        ORTE_THREAD_UNLOCK(&(ctl)->lock);                              \
    } while(0);
#else
#define ORTE_RELEASE_THREAD(ctl)                                        \
    do {                                                                \
        (ctl)->active = false;                                          \
        ORTE_CONDITION_BROADCAST(&(ctl)->cond);                         \
        OPAL_UPDATE_EVBASE((ctl)->evbase, NULL, OPAL_EVENT_NOOP);       \
        ORTE_THREAD_UNLOCK(&(ctl)->lock);                               \
    } while(0);
#endif

#if OPAL_ENABLE_DEBUG
#define ORTE_WAKEUP_THREAD(ctl)                                        \
    do {                                                               \
        ORTE_THREAD_LOCK(&(ctl)->lock);                                \
        if (opal_debug_threads) {                                      \
            opal_output(0, "Waking up thread %s at %s:%d",             \
                        (NULL == (ctl)->name) ? "NULL" : (ctl)->name,  \
                        __FILE__, __LINE__);                           \
        }                                                              \
        (ctl)->active = false;                                         \
        ORTE_CONDITION_BROADCAST(&(ctl)->cond);                        \
        OPAL_UPDATE_EVBASE((ctl)->evbase, NULL, OPAL_EVENT_NOOP);      \
        ORTE_THREAD_UNLOCK(&(ctl)->lock);                              \
    } while(0);
#else
#define ORTE_WAKEUP_THREAD(ctl)                                         \
    do {                                                                \
        ORTE_THREAD_LOCK(&(ctl)->lock);                                 \
        (ctl)->active = false;                                          \
        ORTE_CONDITION_BROADCAST(&(ctl)->cond);                         \
        OPAL_UPDATE_EVBASE((ctl)->evbase, NULL, OPAL_EVENT_NOOP);       \
        ORTE_THREAD_UNLOCK(&(ctl)->lock);                               \
    } while(0);
#endif

END_C_DECLS

#endif /* ORTE_THREAD_H */
