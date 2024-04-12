/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2008      Institut National de Recherche en Informatique
 *                         et Automatique. All rights reserved.
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
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

#include "prte_config.h"

#include <assert.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_QUEUE_H
#    include <sys/queue.h>
#endif
#include <errno.h>
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#ifdef HAVE_SYS_WAIT_H
#    include <sys/wait.h>
#endif

#include "src/class/pmix_list.h"
#include "src/class/pmix_object.h"
#include "src/event/event-internal.h"
#include "src/threads/pmix_mutex.h"
#include "src/util/pmix_output.h"

#include "constants.h"
#include "src/mca/errmgr/errmgr.h"
#include "src/runtime/prte_globals.h"
#include "src/threads/pmix_threads.h"
#include "src/util/name_fns.h"

#include "src/runtime/prte_wait.h"

/* Timer Object Declaration */
static void timer_const(prte_timer_t *tm)
{
    tm->ev = prte_event_alloc();
    tm->payload = NULL;
}
static void timer_dest(prte_timer_t *tm)
{
    prte_event_free(tm->ev);
}
PMIX_CLASS_INSTANCE(prte_timer_t, pmix_object_t,
                    timer_const, timer_dest);

static void wccon(prte_wait_tracker_t *p)
{
    p->child = NULL;
    p->cbfunc = NULL;
    p->cbdata = NULL;
}
static void wcdes(prte_wait_tracker_t *p)
{
    if (NULL != p->child) {
        PMIX_RELEASE(p->child);
    }
}
PMIX_CLASS_INSTANCE(prte_wait_tracker_t, pmix_list_item_t,
                    wccon, wcdes);

/* Local Variables */
static prte_event_t handler;
static pmix_list_t pending_cbs;

/* Local Function Prototypes */
static void wait_signal_callback(int fd, short event, void *arg);

/* Interface Functions */

void prte_wait_disable(void)
{
    prte_event_del(&handler);
}

void prte_wait_enable(void)
{
    prte_event_add(&handler, NULL);
}

int prte_wait_init(void)
{
    PMIX_CONSTRUCT(&pending_cbs, pmix_list_t);

    prte_event_set(prte_event_base, &handler, SIGCHLD,
                   PRTE_EV_SIGNAL | PRTE_EV_PERSIST,
                   wait_signal_callback, &handler);

    prte_event_add(&handler, NULL);
    return PRTE_SUCCESS;
}

int prte_wait_finalize(void)
{
    prte_event_del(&handler);

    /* clear out the pending cbs */
    PMIX_LIST_DESTRUCT(&pending_cbs);

    return PRTE_SUCCESS;
}

/* this function *must* always be called from
 * within an event in the prte_event_base */
void prte_wait_cb(prte_proc_t *child,
                  prte_wait_cbfunc_t callback,
                  void *data)
{
    prte_wait_tracker_t *t2;

    if (NULL == child || NULL == callback) {
        /* bozo protection */
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
        return;
    }

    /* see if this proc is still alive */
    if (!PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_ALIVE)) {
        if (NULL != callback) {
            /* already heard this proc is dead, so just do the callback */
            t2 = PMIX_NEW(prte_wait_tracker_t);
            PMIX_RETAIN(child); // protect against race conditions
            t2->child = child;
            t2->cbfunc = callback;
            t2->cbdata = data;
            prte_event_set(prte_event_base, &t2->ev, -1, PRTE_EV_WRITE, t2->cbfunc, t2);
            prte_event_active(&t2->ev, PRTE_EV_WRITE, 1);
        }
        return;
    }

    /* we just override any existing registration */
    PMIX_LIST_FOREACH(t2, &pending_cbs, prte_wait_tracker_t)
    {
        if (t2->child == child) {
            t2->cbfunc = callback;
            t2->cbdata = data;
            return;
        }
    }
    /* get here if this is a new registration */
    t2 = PMIX_NEW(prte_wait_tracker_t);
    PMIX_RETAIN(child); // protect against race conditions
    t2->child = child;
    t2->cbfunc = callback;
    t2->cbdata = data;
    pmix_list_append(&pending_cbs, &t2->super);
}

static void cancel_callback(int fd, short args, void *cbdata)
{
    prte_wait_tracker_t *trk = (prte_wait_tracker_t *) cbdata;
    prte_wait_tracker_t *t2;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(trk);

    PMIX_LIST_FOREACH(t2, &pending_cbs, prte_wait_tracker_t)
    {
        if (t2->child == trk->child) {
            pmix_list_remove_item(&pending_cbs, &t2->super);
            PMIX_RELEASE(t2);
            PMIX_RELEASE(trk);
            return;
        }
    }

    PMIX_RELEASE(trk);
}

void prte_wait_cb_cancel(prte_proc_t *child)
{
    prte_wait_tracker_t *trk;

    if (NULL == child) {
        /* bozo protection */
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
        return;
    }

    /* push this into the event library for handling */
    trk = PMIX_NEW(prte_wait_tracker_t);
    PMIX_RETAIN(child); // protect against race conditions
    trk->child = child;
    PRTE_PMIX_THREADSHIFT(trk, prte_event_base, cancel_callback);
}

/* callback from the event library whenever a SIGCHLD is received */
static void wait_signal_callback(int fd, short event, void *arg)
{
    prte_event_t *signal = (prte_event_t *) arg;
    int status;
    pid_t pid;
    prte_wait_tracker_t *t2;
    PRTE_HIDE_UNUSED_PARAMS(fd, event);

    PMIX_ACQUIRE_OBJECT(signal);

    if (SIGCHLD != PRTE_EVENT_SIGNAL(signal)) {
        return;
    }

    /* we can have multiple children leave but only get one
     * sigchild callback, so reap all the waitpids until we
     * don't get anything valid back */
    while (1) {
        pid = waitpid(-1, &status, WNOHANG);
        if (-1 == pid && EINTR == errno) {
            /* try it again */
            continue;
        }
        /* if we got garbage, then nothing we can do */
        if (pid <= 0) {
            return;
        }

        /* we are already in an event, so it is safe to access the list */
        PMIX_LIST_FOREACH(t2, &pending_cbs, prte_wait_tracker_t)
        {
            if (pid == t2->child->pid) {
                /* found it! */
                t2->child->exit_code = status;
                pmix_list_remove_item(&pending_cbs, &t2->super);
                if (NULL != t2->cbfunc) {
                    prte_event_set(prte_event_base, &t2->ev, -1, PRTE_EV_WRITE, t2->cbfunc, t2);
                    prte_event_active(&t2->ev, PRTE_EV_WRITE, 1);
                } else {
                    PMIX_RELEASE(t2);
                }
                break;
            }
        }
    }
}
