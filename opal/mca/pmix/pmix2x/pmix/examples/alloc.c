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
 * Copyright (c) 2013-2017 Intel, Inc.  All rights reserved.
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

/* define a structure for collecting returned
 * info from an allocation request */
typedef struct {
    volatile bool active;
    pmix_info_t *info;
    size_t ninfo;
} mydata_t;

static volatile bool waiting_for_allocation = true;

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
    mydata_t *mq = (mydata_t*)cbdata;
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

    /* let the library release the data and cleanup from
     * the operation */
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }

    /* release the block */
    mq->active = false;
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
    /* tell the event handler state machine that we are the last step */
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
    /* flag that the allocation is complete so we can exit */
    waiting_for_allocation = false;
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
        fprintf(stderr, "EVENT HANDLER REGISTRATION FAILED WITH STATUS %d, ref=%lu\n",
                status, (unsigned long)evhandler_ref);
    }
    *active = status;
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
    mydata_t mydata;
    pmix_query_t *query;
    char *myallocation = "MYALLOCATION";
    volatile int active;
    pmix_status_t code = PMIX_NOTIFY_ALLOC_COMPLETE;

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

    /* initialize the return info struct */
    mydata.info = NULL;
    mydata.ninfo = 0;

    if (0 == myproc.rank) {
        /* try to get an allocation */
        mydata.active = true;
        PMIX_INFO_CREATE(info, 2);
        PMIX_INFO_LOAD(&info[0], PMIX_ALLOC_NUM_NODES, &nnodes, PMIX_UINT64);
        PMIX_INFO_LOAD(&info[0], PMIX_ALLOC_ID, myallocation, PMIX_STRING);
        if (PMIX_SUCCESS != (rc = PMIx_Allocation_request_nb(PMIX_ALLOC_NEW, info, 2, infocbfunc, NULL))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Allocation_request_nb failed: %d\n", myproc.nspace, myproc.rank, rc);
            goto done;
        }
        while (mydata.active) {
            usleep(10);
        }
        PMIX_INFO_FREE(info, 2);
        if (NULL != mydata.info) {
            PMIX_INFO_FREE(mydata.info, mydata.ninfo);
        }
    } else if (1 == myproc.rank) {
        /* register a handler specifically for when the allocation
         * operation completes */
        PMIX_INFO_CREATE(info, 1);
        PMIX_INFO_LOAD(&info[0], PMIX_ALLOC_ID, myallocation, PMIX_STRING);
        active = -1;
        PMIx_Register_event_handler(&code, 1, info, 1,
                                    release_fn, evhandler_reg_callbk, (void*)&active);
        while (-1 == active) {
            usleep(10);
        }
        if (0 != active) {
            exit(active);
        }
        PMIX_INFO_FREE(info, 1);
        /* now wait to hear that the request is complete */
        while (waiting_for_allocation) {
            usleep(10);
        }
    } else {
        /* I am not the root rank, so let me wait a little while and then
         * query the status of the allocation request */
        usleep(10);
        PMIX_QUERY_CREATE(query, 1);
        PMIX_ARGV_APPEND(rc, query[0].keys, PMIX_QUERY_ALLOC_STATUS);
        PMIX_INFO_CREATE(query[0].qualifiers, 1);
        PMIX_INFO_LOAD(&query[0].qualifiers[0], PMIX_ALLOC_ID, myallocation, PMIX_STRING);
        mydata.active = true;
        if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(query, 1, infocbfunc, (void*)&mydata))) {
            fprintf(stderr, "PMIx_Query_info failed: %d\n", rc);
            goto done;
        }
        while (mydata.active) {
            usleep(10);
        }
        PMIX_QUERY_FREE(query, 1);
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
