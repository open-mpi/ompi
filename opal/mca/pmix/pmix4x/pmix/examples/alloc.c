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

#include <pmix.h>
#include "examples.h"

/* this is a callback function for the PMIx_Query and
 * PMIx_Allocate APIs. The query will callback with a status indicating
 * if the request could be fully satisfied, partially
 * satisfied, or completely failed. The info parameter
 * contains an array of the returned data, with the
 * info->key field being the key that was provided in
 * the query call. Thus, you can correlate the returned
 * data in the info->value field to the requested key.
 *
 * Once we have dealt with the returned data, we must
 * call the release_fn so that the PMIx library can
 * cleanup */
static void infocbfunc(pmix_status_t status,
                       pmix_info_t *info, size_t ninfo,
                       void *cbdata,
                       pmix_release_cbfunc_t release_fn,
                       void *release_cbdata)
{
    myquery_data_t *mq = (myquery_data_t*)cbdata;
    size_t n;

    fprintf(stderr, "Allocation request returned %s", PMIx_Error_string(status));

    /* save the returned info - the PMIx library "owns" it
     * and will release it and perform other cleanup actions
     * when release_fn is called */
    if (0 < ninfo) {
        PMIX_INFO_CREATE(mq->info, ninfo);
        mq->ninfo = ninfo;
        for (n=0; n < ninfo; n++) {
            fprintf(stderr, "Transferring %s\n", info[n].key);
            PMIX_INFO_XFER(&mq->info[n], &info[n]);
        }
    }
    /* the status returned here indicates whether the requested
     * information was found or not - preserve it */
    mq->lock.status = status;

    /* let the library release the data and cleanup from
     * the operation */
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }

    /* release the block */
    DEBUG_WAKEUP_THREAD(&mq->lock);
}

/* this is an event notification function that we explicitly request
 * be called when the PMIX_ERR_ALLOC_COMPLETE notification is issued.
 * We could catch it in the general event notification function and test
 * the status to see if it was "alloc complete", but it often is simpler
 * to declare a use-specific notification callback point. In this case,
 * we are asking to know when the allocation request completes */
static void release_fn(size_t evhdlr_registration_id,
                       pmix_status_t status,
                       const pmix_proc_t *source,
                       pmix_info_t info[], size_t ninfo,
                       pmix_info_t results[], size_t nresults,
                       pmix_event_notification_cbfunc_fn_t cbfunc,
                       void *cbdata)
{
    myrel_t *lock;
    size_t n;

    /* find the return object */
    lock = NULL;
    for (n=0; n < ninfo; n++) {
        if (0 == strncmp(info[n].key, PMIX_EVENT_RETURN_OBJECT, PMIX_MAX_KEYLEN)) {
            lock = (myrel_t*)info[n].value.data.ptr;
            break;
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
    /* the status will be PMIX_ERR_ALLOC_COMPLETE since that is the code
     * we registered to receive. The result of the allocation request is
     * in the info array - for now, just assume success */
    lock->lock.status = PMIX_SUCCESS;
    /* release the lock */
    DEBUG_WAKEUP_THREAD(&lock->lock);
}

/* event handler registration is done asynchronously because it
 * may involve the PMIx server registering with the host RM for
 * external events. So we provide a callback function that returns
 * the status of the request (success or an error), plus a numerical index
 * to the registered event. The index is used later on to deregister
 * an event handler - if we don't explicitly deregister it, then the
 * PMIx server will do so when it sees us exit */
static void evhandler_reg_callbk(pmix_status_t status,
                                 size_t evhandler_ref,
                                 void *cbdata)
{
    mylock_t *lock = (mylock_t*)cbdata;

    if (PMIX_SUCCESS != status) {
        fprintf(stderr, "EVENT HANDLER REGISTRATION FAILED WITH STATUS %d, ref=%lu\n",
                status, (unsigned long)evhandler_ref);
    }
    lock->status = status;
    lock->evhandler_ref = evhandler_ref;
    DEBUG_WAKEUP_THREAD(lock);
}

int main(int argc, char **argv)
{
    pmix_proc_t myproc;
    int rc;
    pmix_value_t value;
    pmix_value_t *val = &value;
    pmix_proc_t proc;
    uint32_t nprocs;
    pmix_info_t *info;
    uint64_t nnodes = 12;
    myquery_data_t mydata;
    pmix_query_t *query;
    char *myallocation = "MYALLOCATION";
    mylock_t mylock;
    pmix_status_t code;
    myrel_t myrel;

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %d\n", myproc.nspace, myproc.rank, rc);
        exit(0);
    }
    fprintf(stderr, "Client ns %s rank %d: Running\n", myproc.nspace, myproc.rank);

    /* get our universe size */
    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_UNIV_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get universe size failed: %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    nprocs = val->data.uint32;
    PMIX_VALUE_RELEASE(val);
    fprintf(stderr, "Client %s:%d universe size %d\n", myproc.nspace, myproc.rank, nprocs);

    if (0 == myproc.rank) {
        /* try to get an allocation */
        DEBUG_CONSTRUCT_MYQUERY(&mydata);
        PMIX_INFO_CREATE(info, 2);
        PMIX_INFO_LOAD(&info[0], PMIX_ALLOC_NUM_NODES, &nnodes, PMIX_UINT64);
        PMIX_INFO_LOAD(&info[0], PMIX_ALLOC_ID, myallocation, PMIX_STRING);
        if (PMIX_SUCCESS != (rc = PMIx_Allocation_request_nb(PMIX_ALLOC_NEW, info, 2, infocbfunc, &mydata))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Allocation_request_nb failed: %d\n", myproc.nspace, myproc.rank, rc);
            goto done;
        }
        DEBUG_WAIT_THREAD(&mydata.lock);
        PMIX_INFO_FREE(info, 2);
        fprintf(stderr, "Client ns %s rank %d: Allocation returned status: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(mydata.lock.status));
        DEBUG_DESTRUCT_MYQUERY(&mydata);

    } else if (1 == myproc.rank) {
        /* demonstrate a notification based approach - register a handler
         * specifically for when the allocation operation completes */
        DEBUG_CONSTRUCT_MYREL(&myrel);
        PMIX_INFO_CREATE(info, 2);
        PMIX_INFO_LOAD(&info[0], PMIX_ALLOC_ID, myallocation, PMIX_STRING);
        PMIX_INFO_LOAD(&info[1], PMIX_EVENT_RETURN_OBJECT, &myrel, PMIX_POINTER);
        DEBUG_CONSTRUCT_LOCK(&mylock);
        code = PMIX_NOTIFY_ALLOC_COMPLETE;
        PMIx_Register_event_handler(&code, 1, info, 2,
                                    release_fn, evhandler_reg_callbk, (void*)&mylock);
        DEBUG_WAIT_THREAD(&mylock);
        PMIX_INFO_FREE(info, 2);
        rc = mylock.status;
        DEBUG_DESTRUCT_LOCK(&mylock);

        /* now wait to hear that the request is complete */
        DEBUG_WAIT_THREAD(&myrel.lock);
        fprintf(stderr, "[%s:%d] Allocation returned status: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(myrel.lock.status));
        DEBUG_DESTRUCT_MYREL(&myrel);

    } else {
        /* demonstrate a query-based approach - wait a little while and ask to
         * see if it was done */
        usleep(10);
        DEBUG_CONSTRUCT_MYQUERY(&mydata);

        PMIX_QUERY_CREATE(query, 1);
        PMIX_ARGV_APPEND(rc, query[0].keys, PMIX_QUERY_ALLOC_STATUS);
        PMIX_INFO_CREATE(query[0].qualifiers, 1);
        PMIX_INFO_LOAD(&query[0].qualifiers[0], PMIX_ALLOC_ID, myallocation, PMIX_STRING);

        if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(query, 1, infocbfunc, (void*)&mydata))) {
            fprintf(stderr, "PMIx_Query_info failed: %d\n", rc);
            goto done;
        }
        DEBUG_WAIT_THREAD(&mydata.lock);
        PMIX_QUERY_FREE(query, 1);
        fprintf(stderr, "[%s:%d] Allocation returned status: %s\n",
                myproc.nspace, myproc.rank, PMIx_Error_string(mydata.lock.status));
        DEBUG_DESTRUCT_MYQUERY(&mydata);
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
