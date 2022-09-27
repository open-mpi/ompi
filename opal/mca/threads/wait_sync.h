/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2018 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2016      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017-2022 IBM Corporation. All rights reserved.
 * Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2021      Argonne National Laboratory.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_THREADS_WAIT_SYNC_H
#define OPAL_MCA_THREADS_WAIT_SYNC_H

#include "opal/mca/threads/condition.h"
#include "opal/mca/threads/mutex.h"
#include "opal/mca/threads/threads.h"
#include "opal/runtime/opal_progress.h"
#include "opal/sys/atomic.h"

BEGIN_C_DECLS

extern int opal_max_thread_in_progress;

typedef struct ompi_wait_sync_t {
    opal_atomic_int32_t count;
    int32_t status;
    opal_thread_internal_cond_t condition;
    opal_thread_internal_mutex_t lock;
    struct ompi_wait_sync_t *next;
    struct ompi_wait_sync_t *prev;
    volatile bool signaling;
} ompi_wait_sync_t;

#define SYNC_WAIT(sync) (opal_using_threads() ? ompi_sync_wait_mt(sync) : sync_wait_st(sync))

/* The loop in release handles a race condition between the signaling
 * thread and the destruction of the condition variable. The signaling
 * member will be set to false after the final signaling thread has
 * finished operating on the sync object. This is done to avoid
 * extra atomics in the signalling function and keep it as fast
 * as possible. Note that the race window is small so spinning here
 * is more optimal than sleeping since this macro is called in
 * the critical path. */
#define WAIT_SYNC_RELEASE(sync)                                \
    if (opal_using_threads()) {                                \
        while ((sync)->signaling) {                            \
            if (opal_progress_yield_when_idle) {               \
                opal_thread_yield();                           \
            }                                                  \
            continue;                                          \
        }                                                      \
        opal_thread_internal_cond_destroy(&(sync)->condition); \
        opal_thread_internal_mutex_destroy(&(sync)->lock);     \
    }

#define WAIT_SYNC_RELEASE_NOWAIT(sync)                         \
    if (opal_using_threads()) {                                \
        opal_thread_internal_cond_destroy(&(sync)->condition); \
        opal_thread_internal_mutex_destroy(&(sync)->lock);     \
    }

#define WAIT_SYNC_SIGNAL(sync)                                \
    if (opal_using_threads()) {                               \
        opal_thread_internal_mutex_lock(&(sync)->lock);       \
        opal_thread_internal_cond_signal(&(sync)->condition); \
        opal_thread_internal_mutex_unlock(&(sync)->lock);     \
        (sync)->signaling = false;                            \
    }

#define WAIT_SYNC_SIGNALLED(sync)  \
    {                              \
        (sync)->signaling = false; \
    }

/* not static for inline "wait_sync_st" */
OPAL_DECLSPEC extern ompi_wait_sync_t *opal_threads_base_wait_sync_list;

OPAL_DECLSPEC int ompi_sync_wait_mt(ompi_wait_sync_t *sync);
static inline int sync_wait_st(ompi_wait_sync_t *sync)
{
    assert(NULL == opal_threads_base_wait_sync_list);
    assert(NULL == sync->next);
    opal_threads_base_wait_sync_list = sync;

    while (sync->count > 0) {
        opal_progress();
    }
    opal_threads_base_wait_sync_list = NULL;

    return sync->status;
}

#define WAIT_SYNC_INIT(sync, c)                                    \
    do {                                                           \
        (sync)->count = (c);                                       \
        (sync)->next = NULL;                                       \
        (sync)->prev = NULL;                                       \
        (sync)->status = 0;                                        \
        (sync)->signaling = (0 != (c));                            \
        if (opal_using_threads()) {                                \
            opal_thread_internal_cond_init(&(sync)->condition);    \
            opal_thread_internal_mutex_init(&(sync)->lock, false); \
        }                                                          \
    } while (0)

/**
 * Wake up all syncs with a particular status. If status is OMPI_SUCCESS this
 * operation is a NO-OP. Otherwise it will trigger the "error condition" from
 * all registered sync.
 */
OPAL_DECLSPEC void opal_threads_base_wait_sync_global_wakeup_st(int status);
OPAL_DECLSPEC void opal_threads_base_wait_sync_global_wakeup_mt(int status);
#define wait_sync_global_wakeup(st) \
    (opal_using_threads() ? opal_threads_base_wait_sync_global_wakeup_mt(st) : \
    		                opal_threads_base_wait_sync_global_wakeup_st(st))

/**
 * Update the status of the synchronization primitive. If an error is
 * reported the synchronization is completed and the signal
 * triggered. The status of the synchronization will be reported to
 * the waiting threads.
 */
static inline void wait_sync_update(ompi_wait_sync_t *sync, int updates, int status)
{
    if (OPAL_LIKELY(OPAL_SUCCESS == status)) {
        if (0 != (OPAL_THREAD_ADD_FETCH32(&sync->count, -updates))) {
            return;
        }
    } else {
        /* this is an error path so just use the atomic */
        sync->status = status;
        opal_atomic_wmb();
        opal_atomic_swap_32(&sync->count, 0);
    }
    WAIT_SYNC_SIGNAL(sync);
}

END_C_DECLS

#endif /* OPAL_MCA_THREADS_WAIT_SYNC_H */
