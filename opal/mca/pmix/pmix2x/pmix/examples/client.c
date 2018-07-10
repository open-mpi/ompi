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
 * Copyright (c) 2013-2018 Intel, Inc. All rights reserved.
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

#include <pmix.h>

static volatile bool waiting_for_debugger = true;
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

/* this is an event notification function that we explicitly request
 * be called when the PMIX_ERR_DEBUGGER_RELEASE notification is issued.
 * We could catch it in the general event notification function and test
 * the status to see if it was "debugger release", but it often is simpler
 * to declare a use-specific notification callback point. In this case,
 * we are asking to know when we are told the debugger released us */
static void release_fn(size_t evhdlr_registration_id,
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
    waiting_for_debugger = false;
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
    volatile int *active = (volatile int*)cbdata;

    if (PMIX_SUCCESS != status) {
        fprintf(stderr, "Client %s:%d EVENT HANDLER REGISTRATION FAILED WITH STATUS %d, ref=%lu\n",
                   myproc.nspace, myproc.rank, status, (unsigned long)evhandler_ref);
    }
    *active = status;
}

int main(int argc, char **argv)
{
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    char *tmp, *ptr, *p;
    pmix_proc_t proc;
    uint32_t nprocs, m, n, local_cnt, *localpeers;
    pmix_info_t *info;
    bool flag, local;
    volatile int active;
    pmix_status_t dbg = PMIX_ERR_DEBUGGER_RELEASE;

    /* init us - note that the call to "init" includes the return of
     * any job-related info provided by the RM. This includes any
     * debugger flag instructing us to stop-in-init. If such a directive
     * is included, then the process will be stopped in this call until
     * the "debugger release" notification arrives */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %d\n", myproc.nspace, myproc.rank, rc);
        exit(0);
    }
    fprintf(stderr, "Client ns %s rank %d: Running\n", myproc.nspace, myproc.rank);


    /* register our default event handler - again, this isn't strictly
     * required, but is generally good practice */
    active = -1;
    PMIx_Register_event_handler(NULL, 0, NULL, 0,
                                notification_fn, evhandler_reg_callbk, (void*)&active);
    while (-1 == active) {
        sleep(1);
    }
    if (0 != active) {
        fprintf(stderr, "[%s:%d] Default handler registration failed\n", myproc.nspace, myproc.rank);
        exit(active);
    }

    /* job-related info is found in our nspace, assigned to the
     * wildcard rank as it doesn't relate to a specific rank. Setup
     * a name to retrieve such values */
    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;

    /* check to see if we have been instructed to wait for a debugger
     * to attach to us. We won't get both a stop-in-init AND a
     * wait-for-notify directive, so we should never stop twice. This
     * directive is provided so that something like an MPI implementation
     * can do some initial setup in MPI_Init prior to pausing for the
     * debugger */
    if (PMIX_SUCCESS == (rc = PMIx_Get(&proc, PMIX_DEBUG_WAIT_FOR_NOTIFY, NULL, 0, &val))) {
        /* register for debugger release */
        active = -1;
        PMIx_Register_event_handler(&dbg, 1, NULL, 0,
                                    release_fn, evhandler_reg_callbk, (void*)&active);
        /* wait for registration to complete */
        while (-1 == active) {
            sleep(1);
        }
        if (0 != active) {
            fprintf(stderr, "[%s:%d] Debug handler registration failed\n", myproc.nspace, myproc.rank);
            exit(active);
        }
        /* wait for debugger release */
        while (waiting_for_debugger) {
            sleep(1);
        }
    }

    /* get our universe size */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_UNIV_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get universe size failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    fprintf(stderr, "Client %s:%d universe size %d\n", myproc.nspace, myproc.rank, val->data.uint32);
    /* get the number of procs in our job - univ size is the total number of allocated
     * slots, not the number of procs in the job */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get job size failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    fprintf(stderr, "Client %s:%d num procs %d\n", myproc.nspace, myproc.rank, nprocs);

    /* put a few values */
    if (0 > asprintf(&tmp, "%s-%d-internal", myproc.nspace, myproc.rank)) {
        exit(1);
    }
    value.type = PMIX_UINT32;
    value.data.uint32 = 1234;
    if (PMIX_SUCCESS != (rc = PMIx_Store_internal(&myproc, tmp, &value))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Store_internal failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    free(tmp);

    if (0 > asprintf(&tmp, "%s-%d-local", myproc.nspace, myproc.rank)) {
        exit(1);
    }
    value.type = PMIX_UINT64;
    value.data.uint64 = 1234;
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_LOCAL, tmp, &value))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Put local failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    free(tmp);

    if (0 > asprintf(&tmp, "%s-%d-remote", myproc.nspace, myproc.rank)) {
        exit(1);
    }
    value.type = PMIX_STRING;
    value.data.string = "1234";
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_REMOTE, tmp, &value))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Put remote failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    free(tmp);

    /* push the data to our PMIx server */
    if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Commit failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    /* call fence to synchronize with our peers - instruct
     * the fence operation to collect and return all "put"
     * data from our peers */
    PMIX_INFO_CREATE(info, 1);
    flag = true;
    PMIX_INFO_LOAD(info, PMIX_COLLECT_DATA, &flag, PMIX_BOOL);
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, info, 1))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    PMIX_INFO_FREE(info, 1);

    /* get the number of local peers */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_LOCAL_SIZE, NULL, 0, &val))) {
         fprintf(stderr, "Client ns %s rank %d: PMIx_Get PMIX_LOCAL_SIZE failed: %d", myproc.nspace, myproc.rank, rc);
         goto done;
     }
     local_cnt = val->data.uint32;
     PMIX_VALUE_RELEASE(val);

     /* create an array for the peers */
     localpeers = (uint32_t*)malloc(local_cnt * sizeof(int));

    /* get the list of local peers */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_LOCAL_PEERS, NULL, 0, &val))) {
         fprintf(stderr, "Client ns %s rank %d: PMIx_Get PMIX_LOCAL_PEERS failed: %d", myproc.nspace, myproc.rank, rc);
         goto done;
     }
     ptr = strdup(val->data.string);
     PMIX_VALUE_RELEASE(val);

     /* populate the peers array */
     p = strtok(ptr, ",");
     localpeers[0] = strtoul(p, NULL, 10);
     for (n=1; n < local_cnt; n++) {
        p = strtok(NULL, ",");
        localpeers[n] = strtoul(p, NULL, 10);
     }
     free(ptr);

    /* check the returned data */
    for (n=0; n < nprocs; n++) {
        if (n == myproc.rank) {
            continue;
        }
        proc.rank = n;
        local = false;
        for (m=0; m < local_cnt; m++) {
            if (localpeers[m] == proc.rank) {
                local = true;
                break;
            }
        }
        if (local) {
            if (0 > asprintf(&tmp, "%s-%d-local", proc.nspace, proc.rank)) {
                exit(1);
            }
            if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, tmp, NULL, 0, &val))) {
                fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s failed: %d\n", myproc.nspace, myproc.rank, tmp, rc);
                goto done;
            }
            if (PMIX_UINT64 != val->type) {
                fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s returned wrong type: %d\n", myproc.nspace, myproc.rank, tmp, val->type);
                PMIX_VALUE_RELEASE(val);
                free(tmp);
                goto done;
            }
            if (1234 != val->data.uint64) {
                fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s returned wrong value: %d\n", myproc.nspace, myproc.rank, tmp, (int)val->data.uint64);
                PMIX_VALUE_RELEASE(val);
                free(tmp);
                goto done;
            }
            fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s returned correct\n", myproc.nspace, myproc.rank, tmp);
            PMIX_VALUE_RELEASE(val);
            free(tmp);
        } else {
            if (0 > asprintf(&tmp, "%s-%d-remote", proc.nspace, proc.rank)) {
                exit(1);
            }
            if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, tmp, NULL, 0, &val))) {
                fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s failed: %d\n", myproc.nspace, myproc.rank, tmp, rc);
                goto done;
            }
            if (PMIX_STRING != val->type) {
                fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s returned wrong type: %d\n", myproc.nspace, myproc.rank, tmp, val->type);
                PMIX_VALUE_RELEASE(val);
                free(tmp);
                goto done;
            }
            if (0 != strcmp(val->data.string, "1234")) {
                fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s returned wrong value: %s\n", myproc.nspace, myproc.rank, tmp, val->data.string);
                PMIX_VALUE_RELEASE(val);
                free(tmp);
                goto done;
            }
            fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s returned correct\n", myproc.nspace, myproc.rank, tmp);
            PMIX_VALUE_RELEASE(val);
            free(tmp);
        }
    }
    free(localpeers);

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
