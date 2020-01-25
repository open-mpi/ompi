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

#include <pmix.h>
#include "examples.h"

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
    mylock_t *lock = (mylock_t*)cbdata;

    if (PMIX_SUCCESS != status) {
        fprintf(stderr, "Client %s:%d EVENT HANDLER REGISTRATION FAILED WITH STATUS %d, ref=%lu\n",
                   myproc.nspace, myproc.rank, status, (unsigned long)evhandler_ref);
    }
    lock->status = status;
    lock->evhandler_ref = evhandler_ref;
    DEBUG_WAKEUP_THREAD(lock);
}

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pmix_value_t value;
    pmix_value_t *val, *vptr;
    pmix_proc_t proc;
    uint32_t nprocs, n, k;
    pmix_info_t *info;
    bool flag;
    mylock_t mylock;
    pmix_data_array_t da, *dptr;

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
    DEBUG_CONSTRUCT_LOCK(&mylock);
    PMIx_Register_event_handler(NULL, 0, NULL, 0,
                                notification_fn, evhandler_reg_callbk, (void*)&mylock);
    DEBUG_WAIT_THREAD(&mylock);
    rc = mylock.status;
    DEBUG_DESTRUCT_LOCK(&mylock);

    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "[%s:%d] Default handler registration failed\n", myproc.nspace, myproc.rank);
        goto done;
    }

    /* job-related info is found in our nspace, assigned to the
     * wildcard rank as it doesn't relate to a specific rank. Setup
     * a name to retrieve such values */
    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;

    /* get our job size */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get universe size failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    fprintf(stderr, "Client %s:%d job size %d\n", myproc.nspace, myproc.rank, nprocs);

    /* put a data array of pmix_value's */
    val = (pmix_value_t*)malloc(32 * sizeof(pmix_value_t));
    for (n=0; n < 32; n++) {
        val[n].type = PMIX_UINT64;
        val[n].data.uint64 = 2*n;
    }
    da.type = PMIX_VALUE;
    da.size = 32;
    da.array = val;
    value.type = PMIX_DATA_ARRAY;
    value.data.darray = &da;
    rc = PMIx_Put(PMIX_GLOBAL, "test-key", &value);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Put failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    free(val);

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

    /* check the returned data */
    for (n=0; n < nprocs; n++) {
        proc.rank = n;
        if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, "test-key", NULL, 0, &val))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Get on rank %u failed: %d\n",
                    myproc.nspace, myproc.rank, proc.rank, rc);
            goto done;
        }
        if (PMIX_DATA_ARRAY != val->type) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Get on rank %u returned wrong type: %d\n",
                    myproc.nspace, myproc.rank, proc.rank, val->type);
            PMIX_VALUE_RELEASE(val);
            goto done;
        }
        dptr = val->data.darray;
        if (NULL == dptr) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Get %d returned NULL array\n",
                    myproc.nspace, myproc.rank, proc.rank);
            PMIX_VALUE_RELEASE(val);
            goto done;
        }
        if (PMIX_VALUE != dptr->type) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Get %d returned wrong array value type %d\n",
                    myproc.nspace, myproc.rank, proc.rank, dptr->type);
            PMIX_VALUE_RELEASE(val);
            goto done;
        }
        if (32 != dptr->size) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Get %d returned wrong array value size %d\n",
                    myproc.nspace, myproc.rank, proc.rank, (int)dptr->size);
            PMIX_VALUE_RELEASE(val);
            goto done;
        }
        vptr = (pmix_value_t*)dptr->array;
        for (k=0; k < 32; k++) {
            if (PMIX_UINT64 != vptr[k].type) {
                fprintf(stderr, "Client ns %s rank %d: PMIx_Get %d returned wrong type: %d\n",
                        myproc.nspace, myproc.rank, proc.rank, vptr[k].type);
                PMIX_VALUE_RELEASE(val);
                goto done;
            }
            if (2*k != vptr[k].data.uint64) {
                fprintf(stderr, "Client ns %s rank %d: PMIx_Get %d returned wrong value: %lu\n",
                        myproc.nspace, myproc.rank, proc.rank, (unsigned long)vptr[k].data.uint64);
                PMIX_VALUE_RELEASE(val);
                goto done;
            }
        }
    }

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
