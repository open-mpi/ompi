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
 * Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#define _GNU_SOURCE
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <signal.h>

#include <pmix.h>
#include "simptest.h"

static pmix_proc_t myproc;

/* this is the event notification function we pass down below
 * when registering for general events - i.e.,, the default
 * handler. We don't technically need to register one, but it
 * is usually good practice to catch any events that occur */
static void notification_fn(size_t evhdlr_registration_id,
                            pmix_status_t status,
                            const pmix_proc_t *source,
                            pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc,
                            void *cbdata)
{
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
}

/* event handler registration is done asynchronously because it
 * may involve the PMIx server registering with the host RM for
 * external events. So we provide a callback function that returns
 * the status of the request (success or an error), plus a numerical index
 * to the registered event. The index is used later on to deregister
 * an event handler - if we don't explicitly deregister it, then the
 * PMIx server will do so when it see us exit */
static void evhandler_reg_callbk(pmix_status_t status,
                                 size_t evhandler_ref,
                                 void *cbdata)
{
    mylock_t *lk = (mylock_t*)cbdata;

    if (PMIX_SUCCESS != status) {
        fprintf(stderr, "Client %s:%d EVENT HANDLER REGISTRATION FAILED WITH STATUS %d, ref=%lu\n",
                   myproc.nspace, myproc.rank, status, (unsigned long)evhandler_ref);
    }
    lk->status = status;
    DEBUG_WAKEUP_THREAD(lk);
}

static void infocbfunc(pmix_status_t status,
                       pmix_info_t *info, size_t ninfo,
                       void *cbdata,
                       pmix_release_cbfunc_t release_fn,
                       void *release_cbdata)
{
    mylock_t *lk = (mylock_t*)cbdata;

    fprintf(stderr, "Callback recvd with status %d\n", status);

    /* release the caller */
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }

    lk->status = status;
    DEBUG_WAKEUP_THREAD(lk);
}

int main(int argc, char **argv)
{
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    pmix_proc_t proc;
    uint32_t nprocs, n;
    pmix_info_t *info, *iptr;
    bool flag;
    mylock_t mylock;
    pmix_data_array_t *dptr;

    /* init us - note that the call to "init" includes the return of
     * any job-related info provided by the RM. */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %d\n", myproc.nspace, myproc.rank, rc);
        exit(0);
    }
    fprintf(stderr, "Client ns %s rank %d: Running\n", myproc.nspace, myproc.rank);


    /* register our default event handler - again, this isn't strictly
     * required, but is generally good practice */
    DEBUG_CONSTRUCT_LOCK(&mylock);
    PMIx_Register_event_handler(NULL, 0, NULL, 0,
                                notification_fn, evhandler_reg_callbk, (void*)&mylock);
    DEBUG_WAIT_THREAD(&mylock);
    if (0 != mylock.status) {
        fprintf(stderr, "[%s:%d] Default handler registration failed\n", myproc.nspace, myproc.rank);
        exit(mylock.status);
    }
    DEBUG_DESTRUCT_LOCK(&mylock);

    /* job-related info is found in our nspace, assigned to the
     * wildcard rank as it doesn't relate to a specific rank. Setup
     * a name to retrieve such values */
    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;

    /* get our job size */
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get job size failed: %s\n",
                    myproc.nspace, myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    fprintf(stderr, "Client %s:%d job size %d\n", myproc.nspace, myproc.rank, nprocs);

    /* inform the RM that we are preemptible, and that our checkpoint methods are
     * "signal" on SIGUSR2 and event on PMIX_JCTRL_CHECKPOINT */
    PMIX_INFO_CREATE(info, 2);
    flag = true;
    PMIX_INFO_LOAD(&info[0], PMIX_JOB_CTRL_PREEMPTIBLE, (void*)&flag, PMIX_BOOL);
    /* can't use "load" to load a pmix_data_array_t */
    (void)strncpy(info[1].key, PMIX_JOB_CTRL_CHECKPOINT_METHOD, PMIX_MAX_KEYLEN);
    info[1].value.type = PMIX_DATA_ARRAY;
    dptr = (pmix_data_array_t*)malloc(sizeof(pmix_data_array_t));
    info[1].value.data.darray = dptr;
    dptr->type = PMIX_INFO;
    dptr->size = 2;
    PMIX_INFO_CREATE(dptr->array, dptr->size);
    rc = SIGUSR2;
    iptr = (pmix_info_t*)dptr->array;
    PMIX_INFO_LOAD(&iptr[0], PMIX_JOB_CTRL_CHECKPOINT_SIGNAL, &rc, PMIX_INT);
    rc = PMIX_JCTRL_CHECKPOINT;
    PMIX_INFO_LOAD(&iptr[1], PMIX_JOB_CTRL_CHECKPOINT_EVENT, &rc, PMIX_STATUS);

    /* since this is informational and not a requested operation, the target parameter
     * doesn't mean anything and can be ignored */
    DEBUG_CONSTRUCT_LOCK(&mylock);
    if (PMIX_SUCCESS != (rc = PMIx_Job_control_nb(NULL, 0, info, 2, infocbfunc, (void*)&mylock))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Job_control_nb failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    DEBUG_WAIT_THREAD(&mylock);
    PMIX_INFO_FREE(info, 2);
    if (0 != mylock.status) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Job_control_nb failed: %d\n", myproc.nspace, myproc.rank, mylock.status);
        exit(mylock.status);
    }
    DEBUG_DESTRUCT_LOCK(&mylock);

    /* now request that this process be monitored using heartbeats */
    PMIX_INFO_CREATE(iptr, 1);
    PMIX_INFO_LOAD(&iptr[0], PMIX_MONITOR_HEARTBEAT, NULL, PMIX_POINTER);

    PMIX_INFO_CREATE(info, 3);
    PMIX_INFO_LOAD(&info[0], PMIX_MONITOR_ID, "MONITOR1", PMIX_STRING);
    n = 5;  // require a heartbeat every 5 seconds
    PMIX_INFO_LOAD(&info[1], PMIX_MONITOR_HEARTBEAT_TIME, &n, PMIX_UINT32);
    n = 2;  // two heartbeats can be missed before declaring us "stalled"
    PMIX_INFO_LOAD(&info[2], PMIX_MONITOR_HEARTBEAT_DROPS, &n, PMIX_UINT32);

    /* make the request */
    DEBUG_CONSTRUCT_LOCK(&mylock);
    if (PMIX_SUCCESS != (rc = PMIx_Process_monitor_nb(iptr, PMIX_MONITOR_HEARTBEAT_ALERT,
                                                      info, 3, infocbfunc, (void*)&mylock))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Process_monitor_nb failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    DEBUG_WAIT_THREAD(&mylock);
    PMIX_INFO_FREE(iptr, 1);
    PMIX_INFO_FREE(info, 3);
    if (0 != mylock.status) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Process_monitor_nb failed: %d\n", myproc.nspace, myproc.rank, mylock.status);
        exit(mylock.status);
    }
    DEBUG_DESTRUCT_LOCK(&mylock);

    /* send a heartbeat */
    PMIx_Heartbeat();

    /* call fence to synchronize with our peers - no need to
     * collect any info as we didn't "put" anything */
    PMIX_INFO_CREATE(info, 1);
    flag = false;
    PMIX_INFO_LOAD(info, PMIX_COLLECT_DATA, &flag, PMIX_BOOL);
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, info, 1))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    PMIX_INFO_FREE(info, 1);


  done:
    /* finalize us */
    fprintf(stderr, "Client ns %s rank %d: Finalizing\n", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %d\n", myproc.nspace, myproc.rank, rc);
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n", myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return(0);
}
