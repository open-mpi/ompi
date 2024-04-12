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
 * Copyright (c) 2022      ParTec AG.  All rights reserved.
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
#include <time.h>
#include <unistd.h>

#include "examples.h"
#include <pmix.h>

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

    EXAMPLES_HIDE_UNUSED_PARAMS(evhdlr_registration_id, status, source, info, ninfo, results, nresults);
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
    pmix_status_t rc;
    pmix_value_t *val = NULL, *val2 = NULL;
    pmix_proc_t proc;
    uint32_t nprocs;
    mylock_t mylock;
    pid_t pid;

    EXAMPLES_HIDE_UNUSED_PARAMS(argc, argv);

    pid = getpid();
    fprintf(stderr, "Client %lu: Running\n", (unsigned long) pid);

    /* init us - note that the call to "init" includes the return of
     * any job-related info provided by the RM. This includes any
     * debugger flag instructing us to stop-in-init. If such a directive
     * is included, then the process will be stopped in this call until
     * the "debugger release" notification arrives */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %s\n", myproc.nspace, myproc.rank,
                PMIx_Error_string(rc));
        exit(0);
    }
    fprintf(stderr, "Client ns %s rank %d pid %lu: Running\n", myproc.nspace, myproc.rank,
            (unsigned long) pid);

    /* register our default event handler - again, this isn't strictly
     * required, but is generally good practice */
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

    /* job-related info is found in our nspace, assigned to the
     * wildcard rank as it doesn't relate to a specific rank. Setup
     * a name to retrieve such values */
    PMIX_PROC_CONSTRUCT(&proc);
    PMIX_LOAD_PROCID(&proc, myproc.nspace, PMIX_RANK_WILDCARD);

    /* get the number of procs in our job - univ size is the total number of allocated
     * slots, not the number of procs in the job */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get job size failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    fprintf(stderr, "Client %s:%d num procs %d\n", myproc.nspace, myproc.rank, nprocs);

     /* get a list of our local procs - some may not be in our job */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_LOCAL_PROCS, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get local procs with WILDCARD rank failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    // get the list using our proc ID
    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_LOCAL_PROCS, NULL, 0, &val2))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get local procs with my ID failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    if (PMIX_EQUAL == PMIx_Value_compare(val, val2)) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get local procs GOOD\n", myproc.nspace, myproc.rank);
    } else {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get local procs mismatch\n", myproc.nspace, myproc.rank);
    }
    PMIX_VALUE_RELEASE(val);
    PMIX_VALUE_RELEASE(val2);

    /* get our nodeID in various ways */
    if (PMIX_SUCCESS != (rc = PMIx_Get(NULL, PMIX_NODEID, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get nodeID with NULL proc failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
    }
    // get the nodeID using our proc ID
    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_NODEID, NULL, 0, &val2))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get nodeID with my ID failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
    }
    if (NULL != val && NULL != val2 && PMIX_EQUAL == PMIx_Value_compare(val, val2)) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get nodeID GOOD\n", myproc.nspace, myproc.rank);
    } else {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get nodeID mismatch\n", myproc.nspace, myproc.rank);
    }
    PMIX_VALUE_RELEASE(val);
    PMIX_VALUE_RELEASE(val2);

    /* get our hostname in various ways */
    if (PMIX_SUCCESS != (rc = PMIx_Get(NULL, PMIX_HOSTNAME, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get hostname with NULL proc failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    // get the nodeID using our proc ID
    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_HOSTNAME, NULL, 0, &val2))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get hostname with my ID failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    if (PMIX_EQUAL == PMIx_Value_compare(val, val2)) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get hostname GOOD\n", myproc.nspace, myproc.rank);
    } else {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get hostname mismatch\n", myproc.nspace, myproc.rank);
    }
    PMIX_VALUE_RELEASE(val);
    PMIX_VALUE_RELEASE(val2);

done:
    /* finalize us */
    fprintf(stderr, "Client ns %s rank %d: Finalizing\n", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n",
                myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return (0);
}
