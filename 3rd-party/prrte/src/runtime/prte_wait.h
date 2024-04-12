/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Institut National de Recherche en Informatique
 *                         et Automatique. All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * Interface for waitpid / async notification of child death with the
 * libevent runtime system.
 */
#ifndef PRTE_WAIT_H
#define PRTE_WAIT_H

#include "prte_config.h"

#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#include <time.h>
#if HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif

#include "src/event/event-internal.h"
#include "src/util/pmix_output.h"

#include "src/rml/rml_types.h"
#include "src/runtime/prte_globals.h"
#include "src/threads/pmix_threads.h"
#include "types.h"

BEGIN_C_DECLS

/** typedef for callback function used in \c prte_wait_cb */
typedef void (*prte_wait_cbfunc_t)(int fd, short args, void *cb);

/* define a tracker */
typedef struct {
    pmix_list_item_t super;
    prte_event_t ev;
    prte_proc_t *child;
    prte_wait_cbfunc_t cbfunc;
    void *cbdata;
} prte_wait_tracker_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_wait_tracker_t);

/**
 * Disable / re-Enable SIGCHLD handler
 *
 * These functions have to be used after prte_wait_init was called.
 */

PRTE_EXPORT void prte_wait_enable(void);
PRTE_EXPORT void prte_wait_disable(void);

/**
 * Register a callback for process termination
 *
 * Register a callback for notification when this process causes a SIGCHLD.
 * \c waitpid() will have already been called on the process at this
 * time.
 */
PRTE_EXPORT void prte_wait_cb(prte_proc_t *proc,
                              prte_wait_cbfunc_t callback,
                              void *data);

PRTE_EXPORT void prte_wait_cb_cancel(prte_proc_t *proc);

/* In a few places, we need to barrier until something happens
 * that changes a flag to indicate we can release - e.g., waiting
 * for a specific message to arrive. If no progress thread is running,
 * we cycle across prte_progress - however, if a progress thread
 * is active, then we need to just nanosleep to avoid cross-thread
 * confusion
 */
#define PRTE_WAIT_FOR_COMPLETION(flg)                                                \
    do {                                                                             \
        pmix_output_verbose(1, prte_progress_thread_debug,                           \
                            "%s waiting on progress thread at %s:%d",                \
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), __FILE__, __LINE__); \
        while ((flg)) {                                                              \
            /* provide a short quiet period so we                                    \
             * don't hammer the cpu while waiting                                    \
             */                                                                      \
            struct timespec tp = {0, 100000};                                        \
            nanosleep(&tp, NULL);                                                    \
        }                                                                            \
        PMIX_ACQUIRE_OBJECT(flg);                                                    \
    } while (0);

/**
 * In a number of places within the code, we want to setup a timer
 * to detect when some procedure failed to complete. For example,
 * when we launch the daemons, we frequently have no way to directly
 * detect that a daemon failed to launch. Setting a timer allows us
 * to automatically fail out of the launch if we don't hear from a
 * daemon in some specified time window.
 *
 * Computing the amount of time to wait takes a few lines of code, but
 * this macro encapsulates those lines along with the timer event
 * definition just as a convenience. It also centralizes the
 * necessary checks to ensure that the microsecond field is always
 * less than 1M since some systems care about that, and to ensure
 * that the computed wait time doesn't exceed the desired max
 * wait
 *
 * NOTE: the callback function is responsible for releasing the timer
 * event back to the event pool!
 */
#define PRTE_DETECT_TIMEOUT(n, deltat, maxwait, cbfunc, cbd)                                      \
    do {                                                                                          \
        prte_timer_t *tmp;                                                                        \
        int timeout;                                                                              \
        tmp = PMIX_NEW(prte_timer_t);                                                             \
        tmp->payload = (cbd);                                                                     \
        prte_event_evtimer_set(prte_event_base, tmp->ev, (cbfunc), tmp);                          \
        timeout = (deltat) * (n);                                                                 \
        if ((maxwait) > 0 && timeout > (maxwait)) {                                               \
            timeout = (maxwait);                                                                  \
        }                                                                                         \
        tmp->tv.tv_sec = timeout / 1000000;                                                       \
        tmp->tv.tv_usec = timeout % 1000000;                                                      \
        PMIX_OUTPUT_VERBOSE((1, prte_debug_output, "defining timeout: %ld sec %ld usec at %s:%d", \
                             (long) tmp->tv.tv_sec, (long) tmp->tv.tv_usec, __FILE__, __LINE__)); \
        PMIX_POST_OBJECT(tmp);                                                                    \
        prte_event_evtimer_add(tmp->ev, &tmp->tv);                                                \
    } while (0);

/**
 * There are places in the code where we just want to periodically
 * wakeup to do something, and then go back to sleep again. Setting
 * a timer allows us to do this
 *
 * NOTE: the callback function is responsible for releasing the timer
 * event back to the event pool when done! Otherwise, the finalize
 * function will take care of it.
 */
#define PRTE_TIMER_EVENT(sec, usec, cbfunc)                                                     \
    do {                                                                                        \
        prte_timer_t *tm;                                                                       \
        tm = PMIX_NEW(prte_timer_t);                                                            \
        prte_event_evtimer_set(prte_event_base, tm->ev, (cbfunc), tm);                          \
        tm->tv.tv_sec = (sec) + (usec) / 1000000;                                               \
        tm->tv.tv_usec = (usec) % 1000000;                                                      \
        PMIX_OUTPUT_VERBOSE((1, prte_debug_output,                                              \
                             "defining timer event: %ld sec %ld usec at %s:%d",                 \
                             (long) tm->tv.tv_sec, (long) tm->tv.tv_usec, __FILE__, __LINE__)); \
        PMIX_POST_OBJECT(tm);                                                                   \
        prte_event_evtimer_add(tm->ev, &tm->tv);                                                \
    } while (0);

/**
 * \internal
 *
 * Initialize the wait system (allocate mutexes, etc.)
 */
PRTE_EXPORT int prte_wait_init(void);

/**
 * \internal
 *
 * Finalize the wait system (deallocate mutexes, etc.)
 */
PRTE_EXPORT int prte_wait_finalize(void);

END_C_DECLS

#endif /* #ifndef PRTE_WAIT_H */
