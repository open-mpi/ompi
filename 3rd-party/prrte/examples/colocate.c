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
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
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

/* this is an event notification function that we explicitly request
 * be called when the PMIX_EVENT_JOB_END notification is issued.
 * We could catch it in the general event notification function and test
 * the status to see if it was "job terminated", but it often is simpler
 * to declare a use-specific notification callback point. In this case,
 * we are asking to know whenever a job terminates, and we will then
 * know we can exit */
static void release_fn(size_t evhdlr_registration_id, pmix_status_t status,
                       const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                       pmix_info_t results[], size_t nresults,
                       pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    myrel_t *lock;
    bool found;
    int exit_code;
    size_t n;
    pmix_proc_t *affected = NULL;

    /* find the return object */
    lock = NULL;
    found = false;
    for (n = 0; n < ninfo; n++) {
        if (0 == strncmp(info[n].key, PMIX_EVENT_RETURN_OBJECT, PMIX_MAX_KEYLEN)) {
            lock = (myrel_t *) info[n].value.data.ptr;
            /* not every RM will provide an exit code, but check if one was given */
        } else if (0 == strncmp(info[n].key, PMIX_EXIT_CODE, PMIX_MAX_KEYLEN)) {
            exit_code = info[n].value.data.integer;
            found = true;
        } else if (0 == strncmp(info[n].key, PMIX_EVENT_AFFECTED_PROC, PMIX_MAX_KEYLEN)) {
            affected = info[n].value.data.proc;
        }
    }
    /* if the object wasn't returned, then that is an error */
    if (NULL == lock) {
        fprintf(stderr, "LOCK WASN'T RETURNED IN RELEASE CALLBACK\n");
        /* let the event handler progress */
        if (NULL != cbfunc) {
            cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
        }
        return;
    }

    if (found) {
        if (!lock->exit_code_given) {
            lock->exit_code = exit_code;
            lock->exit_code_given = true;
        }
    }

    DEBUG_WAKEUP_THREAD(&lock->lock);

    /* tell the event handler state machine that we are the last step */
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
    return;
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
    DEBUG_WAKEUP_THREAD(lock);
}

static void printusage(void)
{
    fprintf(stderr, "Usage: colocate [options]\n");
    fprintf(stderr, "\t--cmd foo : spawn the foo executable\n");
    fprintf(stderr, "\t-n/--np/-np N : number of procs to spawn\n");
}

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pmix_value_t *val;
    pmix_proc_t proc, *pptr;
    uint32_t nprocs, n;
    pmix_info_t jinfo[2];
    pid_t pid;
    int exitcode = 0;
    pmix_data_array_t darray;
    pmix_app_t app;
    pmix_nspace_t nsp2;
    uint16_t np = 1;
    mylock_t mylock;
    myrel_t myrel;
    void *dirs;
    pmix_info_t *dinfo;
    size_t dninfo;
    pmix_status_t code = PMIX_EVENT_JOB_END;
    char *cmd = "hostname";

    pid = getpid();

    for (n=1; n < argc; n++) {
        if (0 == strcmp(argv[n], "--cmd") ||
            0 == strcmp(argv[n], "-cmd")) {
            if (NULL == argv[n+1]) {
                printusage();
                exit(1);
            }
            cmd = argv[n+1];
        } else if (0 == strcmp(argv[n], "--np") ||
                   0 == strcmp(argv[n], "-np") ||
                   0 == strcmp(argv[n], "-n")) {
            if (NULL == argv[n+1]) {
                printusage();
                exit(1);
            }
            np = strtol(argv[n+1], NULL, 10);
        } else if (0 == strcmp(argv[n], "--help") ||
                   0 == strcmp(argv[n], "-h")) {
            printusage();
            exit(0);
        }
    }

    /* init us - note that the call to "init" includes the return of
     * any job-related info provided by the RM. This includes the
     * location of all procs in our job */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %d\n",
                myproc.nspace, myproc.rank, rc);
        exit(1);
    }
    fprintf(stderr, "Client %s:%u pid %lu: Running\n",
            myproc.nspace, myproc.rank, (unsigned long) pid);

    /* job-related info is found in our nspace, assigned to the
     * wildcard rank as it doesn't relate to a specific rank. Setup
     * a name to retrieve such values */
    PMIX_LOAD_PROCID(&proc, myproc.nspace, PMIX_RANK_WILDCARD);

    /* get the number of procs in our job */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %u: PMIx_Get job size failed: %d\n",
                myproc.nspace, myproc.rank, rc);
        goto done;
    }
    PMIX_VALUE_GET_NUMBER(rc, val, nprocs, uint32_t);
    PMIX_VALUE_RELEASE(val);
    if (1 < nprocs) {
        fprintf(stderr, "Please only run one proc for this example\n");
        exitcode = 1;
        goto done;
    }

    PMIX_APP_CONSTRUCT(&app);
    app.cmd = strdup("hostname");
    PMIX_ARGV_APPEND(rc, app.argv, app.cmd);
    PMIX_INFO_CONSTRUCT(&jinfo[0]);
    PMIX_DATA_ARRAY_CONSTRUCT(&darray, 1, PMIX_PROC);
    pptr = (pmix_proc_t*)darray.array;
    PMIX_LOAD_PROCID(&pptr[0], myproc.nspace, PMIX_RANK_WILDCARD);
    PMIX_INFO_LOAD(&jinfo[0], PMIX_COLOCATE_PROCS, &darray, PMIX_DATA_ARRAY);
    PMIX_INFO_CONSTRUCT(&jinfo[1]);
    PMIX_INFO_LOAD(&jinfo[1], PMIX_COLOCATE_NPERNODE, &np, PMIX_UINT16);

    fprintf(stderr, "Client %s:%u: calling PMIx_Spawn\n",
            myproc.nspace, myproc.rank);
    rc = PMIx_Spawn(jinfo, 2, &app, 1, nsp2);
    PMIX_APP_DESTRUCT(&app);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Client %s:%u: PMIx_Spawn failed: %s(%d)\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(rc), rc);
        exitcode = rc;
        goto done;
    }
    fprintf(stderr, "Spawn success.\n");

    /* Register callback for when this job terminates */
    DEBUG_CONSTRUCT_LOCK(&myrel.lock);
    myrel.nspace = strdup(nsp2);
    PMIX_LOAD_PROCID(&proc, nsp2, PMIX_RANK_WILDCARD);
    PMIX_INFO_LIST_START(dirs);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_RETURN_OBJECT, &myrel, PMIX_POINTER);
    /* Only call me back when this specific job terminates */
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_AFFECTED_PROC, &proc, PMIX_PROC);
    PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
    PMIX_INFO_LIST_RELEASE(dirs);
    dinfo = darray.array;
    dninfo = darray.size;

    DEBUG_CONSTRUCT_LOCK(&mylock);
    PMIx_Register_event_handler(&code, 1, dinfo, dninfo, release_fn, evhandler_reg_callbk,
                                (void *) &mylock);
    DEBUG_WAIT_THREAD(&mylock);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);
    rc = mylock.status;
    DEBUG_DESTRUCT_LOCK(&mylock);

    if (PMIX_SUCCESS != rc) {
        // hang around a while
        sleep(2);
    } else {
        DEBUG_WAIT_THREAD(&myrel.lock);
    }

done:
    /* finalize us */
    fprintf(stderr, "Client ns %s rank %u: Finalizing\n", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %u:PMIx_Finalize failed: %d\n", myproc.nspace,
                myproc.rank, rc);
    } else {
        fprintf(stderr, "Client ns %s rank %u:PMIx_Finalize successfully completed\n",
                myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return (exitcode);
}
