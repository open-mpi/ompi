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
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_THREAD_H
#define OPAL_THREAD_H 1

#include "opal_config.h"

#include <pthread.h>
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#include "opal/class/opal_object.h"
#if OPAL_ENABLE_DEBUG
#include "opal/util/output.h"
#endif

#include "mutex.h"
#include "condition.h"

BEGIN_C_DECLS

typedef void *(*opal_thread_fn_t) (opal_object_t *);

#define OPAL_THREAD_CANCELLED   ((void*)1);

struct opal_thread_t {
    opal_object_t super;
    opal_thread_fn_t t_run;
    void* t_arg;
    pthread_t t_handle;
};

typedef struct opal_thread_t opal_thread_t;

#if OPAL_ENABLE_DEBUG
OPAL_DECLSPEC extern bool opal_debug_threads;
#endif


OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_thread_t);

#if OPAL_ENABLE_DEBUG
#define OPAL_ACQUIRE_THREAD(lck, cnd, act)               \
    do {                                                 \
        OPAL_THREAD_LOCK((lck));                         \
        if (opal_debug_threads) {                        \
            opal_output(0, "Waiting for thread %s:%d",   \
                        __FILE__, __LINE__);             \
        }                                                \
        while (*(act)) {                                 \
            opal_condition_wait((cnd), (lck));           \
        }                                                \
        if (opal_debug_threads) {                        \
            opal_output(0, "Thread obtained %s:%d",      \
                        __FILE__, __LINE__);             \
        }                                                \
        *(act) = true;                                   \
    } while(0);
#else
#define OPAL_ACQUIRE_THREAD(lck, cnd, act)               \
    do {                                                 \
        OPAL_THREAD_LOCK((lck));                         \
        while (*(act)) {                                 \
            opal_condition_wait((cnd), (lck));           \
        }                                                \
        *(act) = true;                                   \
    } while(0);
#endif


#if OPAL_ENABLE_DEBUG
#define OPAL_RELEASE_THREAD(lck, cnd, act)              \
    do {                                                \
        if (opal_debug_threads) {                       \
            opal_output(0, "Releasing thread %s:%d",    \
                        __FILE__, __LINE__);            \
        }                                               \
        *(act) = false;                                 \
        opal_condition_broadcast((cnd));                \
        OPAL_THREAD_UNLOCK((lck));                      \
    } while(0);
#else
#define OPAL_RELEASE_THREAD(lck, cnd, act)              \
    do {                                                \
        *(act) = false;                                 \
        opal_condition_broadcast((cnd));                \
        OPAL_THREAD_UNLOCK((lck));                      \
    } while(0);
#endif


#define OPAL_WAKEUP_THREAD(cnd, act)        \
    do {                                    \
        *(act) = false;                     \
        opal_condition_broadcast((cnd));    \
    } while(0);


OPAL_DECLSPEC int  opal_thread_start(opal_thread_t *);
OPAL_DECLSPEC int  opal_thread_join(opal_thread_t *, void **thread_return);
OPAL_DECLSPEC bool opal_thread_self_compare(opal_thread_t*);
OPAL_DECLSPEC opal_thread_t *opal_thread_get_self(void);
OPAL_DECLSPEC void opal_thread_kill(opal_thread_t *, int sig);

END_C_DECLS

#endif /* OPAL_THREAD_H */
