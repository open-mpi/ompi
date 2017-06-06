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
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_THREAD_H
#define PMIX_THREAD_H 1

#include "pmix_config.h"

#include <pthread.h>
#include <signal.h>

#include "src/class/pmix_object.h"
#if PMIX_ENABLE_DEBUG
#include "src/util/output.h"
#endif

#include "mutex.h"
#include "condition.h"

BEGIN_C_DECLS

typedef void *(*pmix_thread_fn_t) (pmix_object_t *);

#define PMIX_THREAD_CANCELLED   ((void*)1);

struct pmix_thread_t {
    pmix_object_t super;
    pmix_thread_fn_t t_run;
    void* t_arg;
    pthread_t t_handle;
};

typedef struct pmix_thread_t pmix_thread_t;

#if PMIX_ENABLE_DEBUG
PMIX_EXPORT extern bool pmix_debug_threads;
#endif


PMIX_EXPORT PMIX_CLASS_DECLARATION(pmix_thread_t);

#if PMIX_ENABLE_DEBUG
#define PMIX_ACQUIRE_THREAD(lck, cnd, act)               \
    do {                                                 \
        PMIX_THREAD_LOCK((lck));                         \
        if (pmix_debug_threads) {                        \
            pmix_output(0, "Waiting for thread %s:%d",   \
                        __FILE__, __LINE__);             \
        }                                                \
        while (*(act)) {                                 \
            pmix_condition_wait((cnd), (lck));           \
        }                                                \
        if (pmix_debug_threads) {                        \
            pmix_output(0, "Thread obtained %s:%d",      \
                        __FILE__, __LINE__);             \
        }                                                \
        *(act) = true;                                   \
    } while(0);
#else
#define PMIX_ACQUIRE_THREAD(lck, cnd, act)               \
    do {                                                 \
        PMIX_THREAD_LOCK((lck));                         \
        while (*(act)) {                                 \
            pmix_condition_wait((cnd), (lck));           \
        }                                                \
        *(act) = true;                                   \
    } while(0);
#endif


#if PMIX_ENABLE_DEBUG
#define PMIX_RELEASE_THREAD(lck, cnd, act)              \
    do {                                                \
        if (pmix_debug_threads) {                       \
            pmix_output(0, "Releasing thread %s:%d",    \
                        __FILE__, __LINE__);            \
        }                                               \
        *(act) = false;                                 \
        pmix_condition_broadcast((cnd));                \
        PMIX_THREAD_UNLOCK((lck));                      \
    } while(0);
#else
#define PMIX_RELEASE_THREAD(lck, cnd, act)              \
    do {                                                \
        *(act) = false;                                 \
        pmix_condition_broadcast((cnd));                \
        PMIX_THREAD_UNLOCK((lck));                      \
    } while(0);
#endif


#define PMIX_WAKEUP_THREAD(cnd, act)        \
    do {                                    \
        *(act) = false;                     \
        pmix_condition_broadcast((cnd));    \
    } while(0);


/* provide a macro for forward-proofing the shifting
 * of objects between threads - at some point, we
 * may revamp our threading model */

/* post an object to another thread - for now, we
 * only have a memory barrier */
#define PMIX_POST_OBJECT(o)     pmix_atomic_wmb()

/* acquire an object from another thread - for now,
 * we only have a memory barrier */
#define PMIX_ACQUIRE_OBJECT(o)  pmix_atomic_rmb()


PMIX_EXPORT int  pmix_thread_start(pmix_thread_t *);
PMIX_EXPORT int  pmix_thread_join(pmix_thread_t *, void **thread_return);
PMIX_EXPORT bool pmix_thread_self_compare(pmix_thread_t*);
PMIX_EXPORT pmix_thread_t *pmix_thread_get_self(void);
PMIX_EXPORT void pmix_thread_kill(pmix_thread_t *, int sig);
PMIX_EXPORT void pmix_thread_set_main(void);

END_C_DECLS

#endif /* PMIX_THREAD_H */
