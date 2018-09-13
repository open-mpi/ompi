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
 * Copyright (c) 2013-2018 Intel, Inc.  All rights reserved.
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

#include <pmix_tool.h>

/* define a structure for collecting returned
 * info from a query */
typedef struct {
    volatile bool active;
    pmix_info_t *info;
    size_t ninfo;
} myquery_data_t;


static volatile bool waiting_for_debugger = true;
static pmix_proc_t myproc;

/* this is a callback function for the PMIx_Query
 * API. The query will callback with a status indicating
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
static void cbfunc(pmix_status_t status,
                   pmix_info_t *info, size_t ninfo,
                   void *cbdata,
                   pmix_release_cbfunc_t release_fn,
                   void *release_cbdata)
{
    myquery_data_t *mq = (myquery_data_t*)cbdata;
    size_t n;

    /* save the returned info - it will be
     * released in the release_fn */
    if (0 < ninfo) {
        PMIX_INFO_CREATE(mq->info, ninfo);
        mq->ninfo = ninfo;
        for (n=0; n < ninfo; n++) {
            fprintf(stderr, "Transferring %s\n", info[n].key);
            PMIX_INFO_XFER(&mq->info[n], &info[n]);
        }
    }

    /* let the library release the data */
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }

    /* release the block */
    mq->active = false;
}

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
    volatile int *active = (volatile int*)cbdata;

    if (PMIX_SUCCESS != status) {
        fprintf(stderr, "Client %s:%d EVENT HANDLER REGISTRATION FAILED WITH STATUS %d, ref=%lu\n",
                   myproc.nspace, myproc.rank, status, (unsigned long)evhandler_ref);
    }
    *active = status;
}

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pmix_value_t *val;
    pmix_proc_t proc;
    pmix_info_t *info;
    size_t ninfo;
    volatile int active;
    pmix_query_t *query;
    size_t nq, n;
    myquery_data_t myquery_data;

fprintf(stderr, "I AM HERE\n");
fflush(stderr);
    sleep(10);
    exit(0);

    /* init us - since we were launched by the RM, our connection info
     * will have been provided at startup. */
    if (PMIX_SUCCESS != (rc = PMIx_tool_init(&myproc, NULL, 0))) {
        fprintf(stderr, "Debugger daemon ns %s rank %d: PMIx_tool_init failed: %d\n", myproc.nspace, myproc.rank, rc);
        exit(0);
    }
    fprintf(stderr, "Debugger daemon ns %s rank %d: Running\n", myproc.nspace, myproc.rank);


    /* register our default event handler */
    active = -1;
    PMIx_Register_event_handler(NULL, 0, NULL, 0,
                                notification_fn, evhandler_reg_callbk, (void*)&active);
    while (-1 == active) {
        usleep(10);
    }
    if (0 != active) {
        exit(active);
    }

    /* get the nspace of the job we are to debug */
    (void)strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_DEBUG_JOB, NULL, 0, &val))) {
        fprintf(stderr, "[%s:%d] Failed to get job being debugged - error %d\n", myproc.nspace, myproc.rank, rc);
        goto done;
    }
    if (NULL == val) {
        fprintf(stderr, "Got NULL return\n");
        goto done;
    }
    fprintf(stderr, "[%s:%d] Debugging %s\n", myproc.nspace, myproc.rank, val->data.string);

    /* get our local proctable - for scalability reasons, we don't want to
     * have our "root" debugger process get the proctable for everybody and
     * send it out to us. So ask the local PMIx server for the pid's of
     * our local target processes */
    nq = 1;
    PMIX_QUERY_CREATE(query, nq);
    PMIX_ARGV_APPEND(rc, query[0].keys, PMIX_QUERY_LOCAL_PROC_TABLE);
    query[0].nqual = 1;
    PMIX_INFO_CREATE(query[0].qualifiers, 1);
    PMIX_INFO_LOAD(&query[0].qualifiers[0], PMIX_NSPACE, val->data.string, PMIX_STRING);  // the nspace we are enquiring about
    /* setup the caddy to retrieve the data */
    myquery_data.info = NULL;
    myquery_data.ninfo = 0;
    myquery_data.active = true;
    /* execute the query */
    if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(query, nq, cbfunc, (void*)&myquery_data))) {
        fprintf(stderr, "PMIx_Query_info failed: %d\n", rc);
        goto done;
    }
    while (myquery_data.active) {
        usleep(10);
    }
    fprintf(stderr, "[%s:%d] Local proctable received\n", myproc.nspace, myproc.rank);


    /* now that we have the proctable for our local processes, we can do our
     * magic debugger stuff and attach to them. We then send a "release" event
     * to them - i.e., it's the equivalent to setting the MPIR breakpoint. We
     * do this with the event notification system */
    (void)strncpy(proc.nspace, val->data.string, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    /* we send the notification to just the local procs of the job being debugged */
    ninfo = 1;
    PMIX_INFO_CREATE(info, ninfo);
    PMIX_INFO_LOAD(&info[0], PMIX_EVENT_CUSTOM_RANGE, &proc, PMIX_PROC);  // deliver to the target nspace
    fprintf(stderr, "[%s:%u] Sending release\n", myproc.nspace, myproc.rank);
    PMIx_Notify_event(PMIX_ERR_DEBUGGER_RELEASE,
                      NULL, PMIX_RANGE_LOCAL,
                      info, ninfo, NULL, NULL);

    /* do some debugger magic */
    n = 0;
    fprintf(stderr, "[%s:%u] Hanging around awhile, doing debugger magic\n", myproc.nspace, myproc.rank);
    while (n < 5) {
        usleep(1000);
        ++n;
    }

  done:
    /* finalize us */
    fprintf(stderr, "Debugger daemon ns %s rank %d: Finalizing\n", myproc.nspace, myproc.rank);
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Debugger daemon ns %s rank %d:PMIx_Finalize failed: %d\n", myproc.nspace, myproc.rank, rc);
    } else {
        fprintf(stderr, "Debugger daemon ns %s rank %d:PMIx_Finalize successfully completed\n", myproc.nspace, myproc.rank);
    }
    fflush(stderr);
    return(0);
}
