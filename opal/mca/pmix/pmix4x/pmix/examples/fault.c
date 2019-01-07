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

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <pthread.h>

#include <pmix.h>
#include "examples.h"

static pmix_proc_t myproc;

static void notification_fn(size_t evhdlr_registration_id,
                            pmix_status_t status,
                            const pmix_proc_t *source,
                            pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc,
                            void *cbdata)
{
    myrel_t *lock;
    bool found;
    int exit_code;
    size_t n;
    pmix_proc_t *affected = NULL;

    /* find our return object */
    lock = NULL;
    found = false;
    for (n=0; n < ninfo; n++) {
        if (0 == strncmp(info[n].key, PMIX_EVENT_RETURN_OBJECT, PMIX_MAX_KEYLEN)) {
            lock = (myrel_t*)info[n].value.data.ptr;
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
            cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
        }
        return;
    }

    /* tell the event handler state machine that we are the last step */
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
    fprintf(stderr, "DEBUGGER DAEMON NOTIFIED TERMINATED - AFFECTED %s\n",
            (NULL == affected) ? "NULL" : affected->nspace);

    if (found) {
        lock->exit_code = exit_code;
        lock->exit_code_given = true;
    }
    DEBUG_WAKEUP_THREAD(&lock->lock);
}

static void op_callbk(pmix_status_t status,
                      void *cbdata)
{
    mylock_t *lock = (mylock_t*)cbdata;
    fprintf(stderr, "Client %s:%d OP CALLBACK CALLED WITH STATUS %d\n", myproc.nspace, myproc.rank, status);
    DEBUG_WAKEUP_THREAD(lock);
}

static void evhandler_reg_callbk(pmix_status_t status,
                                  size_t errhandler_ref,
                                  void *cbdata)
{
    mylock_t *lock = (mylock_t*)cbdata;

    fprintf(stderr, "Client %s:%d ERRHANDLER REGISTRATION CALLBACK CALLED WITH STATUS %d, ref=%lu\n",
               myproc.nspace, myproc.rank, status, (unsigned long)errhandler_ref);
    DEBUG_WAKEUP_THREAD(lock);
}

int main(int argc, char **argv)
{
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    pmix_proc_t proc;
    uint32_t nprocs;
    pmix_info_t *info;
    mylock_t mylock;
    myrel_t myrel;
    pmix_status_t code[2] = {PMIX_ERR_PROC_ABORTED, PMIX_ERR_JOB_TERMINATED};

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %d\n", myproc.nspace, myproc.rank, rc);
        exit(0);
    }
    fprintf(stderr, "Client ns %s rank %d: Running\n", myproc.nspace, myproc.rank);

    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;

    /* get our universe size */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_UNIV_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get universe size failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    fprintf(stderr, "Client %s:%d universe size %d\n", myproc.nspace, myproc.rank, nprocs);

    /* register another handler specifically for when the target
     * job completes */
    DEBUG_CONSTRUCT_MYREL(&myrel);
    PMIX_INFO_CREATE(info, 2);
    PMIX_INFO_LOAD(&info[0], PMIX_EVENT_RETURN_OBJECT, &myrel, PMIX_POINTER);
    /* only call me back when one of us terminates */
    PMIX_INFO_LOAD(&info[1], PMIX_NSPACE, myproc.nspace, PMIX_STRING);

    DEBUG_CONSTRUCT_LOCK(&mylock);
    PMIx_Register_event_handler(code, 2, info, 2,
                                notification_fn, evhandler_reg_callbk, (void*)&mylock);
    DEBUG_WAIT_THREAD(&mylock);
    if (PMIX_SUCCESS != mylock.status) {
        rc = mylock.status;
        DEBUG_DESTRUCT_LOCK(&mylock);
        PMIX_INFO_FREE(info, 2);
        goto done;
    }
    DEBUG_DESTRUCT_LOCK(&mylock);
    PMIX_INFO_FREE(info, 2);

    /* call fence to sync */
    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }

    /* rank=0 calls abort */
    if (0 == myproc.rank) {
        sleep(2);
        fprintf(stderr, "Client ns %s rank %d: exiting with error\n", myproc.nspace, myproc.rank);
        exit(1);
    }
    /* everyone simply waits */
    DEBUG_WAIT_THREAD(&myrel.lock);
    DEBUG_DESTRUCT_MYREL(&myrel);

 done:
    /* finalize us */
    fprintf(stderr, "Client ns %s rank %d: Finalizing\n", myproc.nspace, myproc.rank);
    DEBUG_CONSTRUCT_LOCK(&mylock);
    PMIx_Deregister_event_handler(1, op_callbk, &mylock);
    DEBUG_WAIT_THREAD(&mylock);
    DEBUG_DESTRUCT_LOCK(&mylock);

    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %d\n", myproc.nspace, myproc.rank, rc);
    } else {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize successfully completed\n", myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return(0);
}
