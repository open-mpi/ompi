/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include <pmix.h>

#include "examples.h"

static pmix_proc_t myproc;

/* this is the event notification function we pass down below
 * when registering for general events - i.e.,, the default
 * handler. We don't technically need to register one, but it
 * is usually good practice to catch any events that occur */
static void notification_fn(size_t evhdlr_registration_id, pmix_status_t status,
                            const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }

    EXAMPLES_HIDE_UNUSED_PARAMS(evhdlr_registration_id, source, info, ninfo, results, nresults);

    fprintf(stderr, "Default error handler called with status %s\n", PMIx_Error_string(status));
}

/* this is an event notification function that we explicitly request
 * be called when the PMIX_PROCESS_SET_DEFINE notification is issued.
 */
static void release_fn(size_t evhdlr_registration_id, pmix_status_t status,
                       const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                       pmix_info_t results[], size_t nresults,
                       pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    myrel_t *lock;
    size_t n;
    char *pset = NULL;

    EXAMPLES_HIDE_UNUSED_PARAMS(evhdlr_registration_id, status, source, info, ninfo, results, nresults);

    /* find the return object */
    lock = NULL;
    for (n = 0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_EVENT_RETURN_OBJECT)) {
            lock = (myrel_t *) info[n].value.data.ptr;
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_PSET_NAME)) {
            pset = info[n].value.data.string;
        }
    }
    /* if the object wasn't returned, then that is an error */
    if (NULL == lock) {
        fprintf(stderr, "LOCK WASN'T RETURNED IN RELEASE CALLBACK\n");
        /* let the event handler progress */
        if (NULL != cbfunc) {
            cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
        }
        return;
    }
    /* the status will be PMIX_PROCESS_SET_DEFINE since that is the code
     * we registered to receive, so just return success */
    lock->lock.status = PMIX_SUCCESS;
    if (NULL != pset) {
        lock->lock.answer = strdup(pset);
    }

    /* tell the event handler state machine that we are the last step */
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
    /* release the lock */
    DEBUG_WAKEUP_THREAD(&lock->lock);
}

/* event handler registration is done asynchronously because it
 * may involve the PMIx server registering with the host RM for
 * external events. So we provide a callback function that returns
 * the status of the request (success or an error), plus a numerical index
 * to the registered event. The index is used later on to deregister
 * an event handler - if we don't explicitly deregister it, then the
 * PMIx server will do so when it see us exit */
static void evhandler_reg_callbk(pmix_status_t status, size_t evhandler_ref, void *cbdata)
{
    mylock_t *lock = (mylock_t *) cbdata;

    if (PMIX_SUCCESS != status) {
        fprintf(stderr, "Client %s:%d EVENT HANDLER REGISTRATION FAILED WITH STATUS %d, ref=%lu\n",
                myproc.nspace, myproc.rank, status, (unsigned long) evhandler_ref);
    }
    lock->status = status;
    lock->evhandler_ref = evhandler_ref;
    DEBUG_WAKEUP_THREAD(lock);
}

int main(int argc, char **argv)
{
    int rc;
    pmix_status_t pnm = PMIX_PROCESS_SET_DEFINE;
    mylock_t mylock;
    myrel_t myrel;
    pmix_info_t info;
    pmix_value_t *val;

    EXAMPLES_HIDE_UNUSED_PARAMS(argc, argv);

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %d\n", myproc.nspace, myproc.rank,
                rc);
        exit(0);
    }
    fprintf(stderr, "Client ns %s rank %d: Running\n", myproc.nspace, myproc.rank);

    /* register our default event handler */
    DEBUG_CONSTRUCT_LOCK(&mylock);
    PMIx_Register_event_handler(NULL, 0, NULL, 0, notification_fn, evhandler_reg_callbk,
                                (void *) &mylock);
    DEBUG_WAIT_THREAD(&mylock);
    rc = mylock.status;
    DEBUG_DESTRUCT_LOCK(&mylock);

    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "[%s:%d] Default handler registration failed\n", myproc.nspace,
                myproc.rank);
        goto done;
    }

    /* register for process set name being defined */
    DEBUG_CONSTRUCT_LOCK(&mylock);
    PMIX_INFO_CONSTRUCT(&info);
    DEBUG_CONSTRUCT_MYREL(&myrel);
    PMIX_INFO_LOAD(&info, PMIX_EVENT_RETURN_OBJECT, &myrel, PMIX_POINTER);
    PMIx_Register_event_handler(&pnm, 1, &info, 1, release_fn, evhandler_reg_callbk,
                                (void *) &mylock);
    /* wait for registration to complete */
    DEBUG_WAIT_THREAD(&mylock);
    rc = mylock.status;
    DEBUG_DESTRUCT_LOCK(&mylock);
    PMIX_INFO_DESTRUCT(&info);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "[%s:%d] Debug handler registration failed\n", myproc.nspace,
                myproc.rank);
        goto done;
    }
    /* wait for process set name to be defined */
    DEBUG_WAIT_THREAD(&myrel.lock);
    if (NULL != myrel.lock.answer) {
        fprintf(stderr, "Received process set name %s\n", myrel.lock.answer);
    } else {
        fprintf(stderr, "Received bad answer\n");
    }
    DEBUG_DESTRUCT_MYREL(&myrel);

    // check if I can retrieve my new pset membership
    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_PSET_NAMES, NULL, 0, &val))) {
        fprintf(stderr, "[%s:%d] PMIx_Get PMIX_PSET_NAMES returned %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    fprintf(stderr, "[%s:%d] belongs to psets %s\n", myproc.nspace, myproc.rank, val->data.string);
    PMIX_VALUE_RELEASE(val);

done:
    /* finalize us */
    fprintf(stderr, "Client ns %s rank %d: Finalizing\n", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %d\n", myproc.nspace,
                myproc.rank, rc);
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n",
                myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return (0);
}
