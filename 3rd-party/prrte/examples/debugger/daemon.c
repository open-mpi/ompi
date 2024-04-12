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
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2021      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#define _GNU_SOURCE
#include <libgen.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include "debugger.h"
#include <pmix_tool.h>
/*
 * This module is an example of a PMIx debugger daemon. The debugger daemon
 * handles interactions with application processes on a node in behalf of the
 * front end debugger process.
 */

static pmix_proc_t myproc;
static char *target_namespace = NULL;

/* This is a callback function for the PMIx_Query
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
static void cbfunc(pmix_status_t status, pmix_info_t *info, size_t ninfo, void *cbdata,
                   pmix_release_cbfunc_t release_fn, void *release_cbdata)
{
    myquery_data_t *mq = (myquery_data_t *) cbdata;
    size_t n;

    mq->status = status;

    printf("%s called as daemon callback for PMIx_Query\n", __FUNCTION__);
    /* Save the returned info - it will be * released in the release_fn */
    if (0 < ninfo) {
        PMIX_INFO_CREATE(mq->info, ninfo);
        mq->ninfo = ninfo;
        for (n = 0; n < ninfo; n++) {
            printf("Transferring %s\n", info[n].key);
            PMIX_INFO_XFER(&mq->info[n], &info[n]);
        }
    }

    /* Let the library release the data */
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }

    /* Release the lock */
    DEBUG_WAKEUP_THREAD(&mq->lock);
}

/* This is the event notification function we pass down below
 * when registering for general events - i.e.,, the default
  handler. We don't technically need to register one, but it
 * is usually good practice to catch any events that occur */
static void notification_fn(size_t evhdlr_registration_id, pmix_status_t status,
                            const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    printf("%s called as daemon default event handler for event=%s\n", __FUNCTION__,
           PMIx_Error_string(status));
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
    }
}

/* This is an event notification function that we explicitly request
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

    printf("%s called as daemon callback for event=%s\n", __FUNCTION__, PMIx_Error_string(status));

    /* Be sure notification is for our application process namespace */
    if (0 != strcmp(target_namespace, source->nspace)) {
        printf("Ignoring termination notification for '%s'\n", source->nspace);
        /* Tell the event handler state machine that we are the last step */
        if (NULL != cbfunc) {
            cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
        }
        return;
    }
    /* Find our return object */
    lock = NULL;
    found = false;
    for (n = 0; n < ninfo; n++) {
        /* Retrieve the lock that needs to be released by this callback. */
        if (0 == strncmp(info[n].key, PMIX_EVENT_RETURN_OBJECT, PMIX_MAX_KEYLEN)) {
            lock = (myrel_t *) info[n].value.data.ptr;
            /* Not every RM will provide an exit code, but check if one was
             * given */
        } else if (0 == strncmp(info[n].key, PMIX_EXIT_CODE, PMIX_MAX_KEYLEN)) {
            exit_code = info[n].value.data.integer;
            found = true;
        } else if (0 == strncmp(info[n].key, PMIX_EVENT_AFFECTED_PROC, PMIX_MAX_KEYLEN)) {
            affected = info[n].value.data.proc;
        }
    }
    /* if the lock object wasn't returned, then that is an error */
    if (NULL == lock) {
        fprintf(stderr, "LOCK WASN'T RETURNED IN RELEASE CALLBACK\n");
        /* let the event handler progress */
        if (NULL != cbfunc) {
            cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
        }
        return;
    }

    printf("DEBUGGER DAEMON NAMESPACE %s NOTIFIED THAT JOB TERMINATED - AFFECTED %s\n",
           lock->nspace, (NULL == affected) ? "NULL" : affected->nspace);

    /* If the lock object was found then store return status in the lock
     * object. */
    if (found) {
        lock->exit_code = exit_code;
        lock->exit_code_given = true;
    }

    /* Tell the event handler state machine that we are the last step */
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }

    /* Wake up the thread that is waiting for this callback to complete */
    DEBUG_WAKEUP_THREAD(&lock->lock);
}

/* Event handler registration is done asynchronously because it
 * may involve the PMIx server registering with the host RM for
 * external events. So we provide a callback function that returns
 * the status of the request (success or an error), plus a numerical index
 * to the registered event. The index is used later on to deregister
 * an event handler - if we don't explicitly deregister it, then the
 * PMIx server will do so when it see us exit */
static void evhandler_reg_callbk(pmix_status_t status, size_t evhandler_ref, void *cbdata)
{
    mylock_t *lock = (mylock_t *) cbdata;

    printf("%s called by daemon as registration callback\n", __FUNCTION__);
    if (PMIX_SUCCESS != status) {
        fprintf(stderr, "Client %s:%d EVENT HANDLER REGISTRATION FAILED WITH STATUS %d, ref=%lu\n",
                myproc.nspace, myproc.rank, status, (unsigned long) evhandler_ref);
    }
    lock->status = status;
    DEBUG_WAKEUP_THREAD(lock);
}

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pmix_value_t *val;
    void *dirs;
    pmix_proc_t proc;
    pmix_info_t *info;
    size_t ninfo;
    pmix_query_t *query;
    pmix_proc_info_t *proctable;
    size_t nq;
    size_t n;
    myquery_data_t myquery_data;
    pid_t pid;
    pmix_status_t code = PMIX_EVENT_JOB_END;
    mylock_t mylock;
    myrel_t myrel;
    uint16_t localrank;
    int i;
    pmix_data_array_t darray;
    int cospawned_namespace = 0;
    char hostname[256];

    pid = getpid();
    gethostname(hostname, sizeof hostname);

    /* Initialize this daemon - since we were launched by the RM, our
     * connection info * will have been provided at startup. */
    if (PMIX_SUCCESS != (rc = PMIx_tool_init(&myproc, NULL, 0))) {
        fprintf(stderr, "Debugger daemon: PMIx_tool_init failed: %s\n", PMIx_Error_string(rc));
        exit(0);
    }
    printf("Debugger daemon ns %s on host %s rank %d pid %lu: Running\n", myproc.nspace,
            hostname, myproc.rank, (unsigned long) pid);

    /* Register our default event handler */
    DEBUG_CONSTRUCT_LOCK(&mylock);
    PMIX_INFO_CREATE(info, 1);
    PMIX_INFO_LOAD(&info[0], PMIX_EVENT_HDLR_NAME, "DEFAULT", PMIX_STRING);
    PMIx_Register_event_handler(NULL, 0, info, 1, notification_fn, evhandler_reg_callbk,
                                (void *) &mylock);
    DEBUG_WAIT_THREAD(&mylock);
    PMIX_INFO_FREE(info, 1);
    if (PMIX_SUCCESS != mylock.status) {
        rc = mylock.status;
        DEBUG_DESTRUCT_LOCK(&mylock);
        goto done;
    }
    DEBUG_DESTRUCT_LOCK(&mylock);

    /*
     * Get the namespace of the job we are to debug. If the application and the
     * debugger daemons are spawned separately or if the debugger is attaching
     * to a running application, the debugger will set the application
     * namespace in the PMIX_DEBUG_TARGET attribute, and the daemon retrieves
     * it by calling PMIx_Get.
     *
     * If the application processes and debugger daemons are spawned together
     * (cospawn), then the debugger cannot pass the application namespace since
     * that is not known until after the PMIx_Spawn call completes. However,
     * the applicaton processes and the debugger daemons have the same
     * namespace, so this module uses the debugger namespace, which it knows.
     */
    PMIX_LOAD_PROCID(&proc, myproc.nspace, PMIX_RANK_WILDCARD);
    rc = PMIx_Get(&proc, PMIX_DEBUG_TARGET, NULL, 0, &val);
    if (PMIX_ERR_NOT_FOUND == rc) {
        /* Save the application namespace for later */
        // NOTE: This is a bug. The cospawned namespace should be more distinct.
        /*
        fprintf(stderr,
                "[%s:%d:%lu] Warning: Could not find PMIX_DEBUG_TARGET. Assume Cospawn.\n",
                myproc.nspace, myproc.rank);
        */
        target_namespace = strdup(myproc.nspace);
        cospawned_namespace = 1;
    } else if (rc != PMIX_SUCCESS) {
        fprintf(stderr, "[%s:%d:%lu] Failed to get job being debugged - error %s\n", myproc.nspace,
                myproc.rank, (unsigned long) pid, PMIx_Error_string(rc));
        goto done;
    } else {
        /* Verify that the expected data structures were returned */
        if (NULL == val || PMIX_PROC != val->type) {
            fprintf(stderr, "[%s:%d:%lu] Failed to get job being debugged - NULL data returned\n",
                    myproc.nspace, myproc.rank, (unsigned long) pid);
            goto done;
        }
        printf("[%s:%d:%lu] PMIX_DEBUG_JOB is '%s'\n", proc.nspace, proc.rank, (unsigned long) pid,
               val->data.proc->nspace);
        /* Save the application namespace for later */
        target_namespace = strdup(val->data.proc->nspace);
        PMIX_VALUE_RELEASE(val);
    }

    printf("[%s:%d:%lu] Debugging '%s'\n", myproc.nspace, myproc.rank, (unsigned long) pid,
           target_namespace);

    /* Get my local rank so I can determine which local proc is "mine" to
     * debug */
    val = NULL;
    if (PMIX_SUCCESS != (rc = PMIx_Get(&myproc, PMIX_LOCAL_RANK, NULL, 0, &val))) {
        fprintf(stderr, "[%s:%d:%lu] Failed to get my local rank - error %s\n", myproc.nspace,
                myproc.rank, (unsigned long) pid, PMIx_Error_string(rc));
        goto done;
    }

    /* Verify the expected data object was returned */
    if (NULL == val) {
        fprintf(stderr, "[%s:%d:%lu] Failed to get my local rank - NULL data returned\n",
                myproc.nspace, myproc.rank, (unsigned long) pid);
        goto done;
    }
    if (PMIX_UINT16 != val->type) {
        fprintf(stderr, "[%s:%d:%lu] Failed to get my local rank - returned wrong type %s\n",
                myproc.nspace, myproc.rank, (unsigned long) pid, PMIx_Data_type_string(val->type));
        goto done;
    }

    /* Save the rank */
    localrank = val->data.uint16;
    PMIX_VALUE_RELEASE(val);
    printf("[%s:%d:%lu] my local rank %d\n", myproc.nspace, myproc.rank, (unsigned long) pid,
           (int) localrank);

    /* Register an event handler specifically for when the target job
     * completes */
    DEBUG_CONSTRUCT_LOCK(&myrel.lock);
    myrel.nspace = strdup(proc.nspace);

    PMIX_LOAD_PROCID(&proc, target_namespace, PMIX_RANK_WILDCARD);

    PMIX_INFO_LIST_START(dirs);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_HDLR_NAME, "APP-TERMINATION", PMIX_STRING);
    /* Pass the lock we will use to wait for notification of the
     * PMIX_EVENT_JOB_END event */
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_RETURN_OBJECT, &myrel, PMIX_POINTER);
    /* Only call me back when this specific job terminates */
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_AFFECTED_PROC, &proc, PMIX_PROC);
    PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
    PMIX_INFO_LIST_RELEASE(dirs);
    ninfo = darray.size;
    info = darray.array;
    printf("[%s:%d:%lu] registering for termination of '%s'\n", myproc.nspace, myproc.rank,
           (unsigned long) pid, proc.nspace);

    /* Create a lock to wait for completion of the event registration
     * callback */
    DEBUG_CONSTRUCT_LOCK(&mylock);
    PMIx_Register_event_handler(&code, 1, info, ninfo, release_fn, evhandler_reg_callbk,
                                (void *) &mylock);
    DEBUG_WAIT_THREAD(&mylock);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);
    if (PMIX_SUCCESS != mylock.status) {
        fprintf(stderr, "Failed to register handler for PMIX_EVENT_JOB_END: %s\n",
                PMIx_Error_string(mylock.status));
        rc = mylock.status;
        DEBUG_DESTRUCT_LOCK(&mylock);
        goto done;
    }
    DEBUG_DESTRUCT_LOCK(&mylock);

    /* Get our local proctable - for scalability reasons, we don't want to
     * have our "root" debugger process get the proctable for everybody and
     * send it out to us. So ask the local PMIx server for the pid's of
     * our local target processes
     */
    nq = 1;
    PMIX_QUERY_CREATE(query, nq);
    PMIX_ARGV_APPEND(rc, query[0].keys, PMIX_QUERY_LOCAL_PROC_TABLE);
    n = 0;
    ninfo = 1;
    query[0].nqual = ninfo;
    PMIX_INFO_CREATE(query[0].qualifiers, ninfo);
    /* Set the namespace to query */
    PMIX_INFO_LOAD(&query[0].qualifiers[n], PMIX_NSPACE, target_namespace, PMIX_STRING);

    /* Create the lock used to wait for query completion */
    DEBUG_CONSTRUCT_LOCK(&myquery_data.lock);
    myquery_data.info = NULL;
    myquery_data.ninfo = 0;

    /* Execute the query */
    if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(query, nq, cbfunc, (void *) &myquery_data))) {
        fprintf(stderr, "PMIx_Query_info failed: %d\n", rc);
        goto done;
    }

    /* Wait for the query to complete */
    DEBUG_WAIT_THREAD(&myquery_data.lock);
    DEBUG_DESTRUCT_LOCK(&myquery_data.lock);
    PMIX_QUERY_FREE(query, nq);
    if (PMIX_SUCCESS != myquery_data.status) {
        rc = myquery_data.status;
        fprintf(stderr, "Error querying proc table for '%s': %s\n", target_namespace,
                PMIx_Error_string(myquery_data.status));
        goto done;
    }

    /* Display the process table */
    printf("[%s:%d:%lu] Local proctable received for nspace '%s' has %d entries\n", myproc.nspace,
           myproc.rank, (unsigned long) pid, target_namespace,
           (int) myquery_data.info[0].value.data.darray->size);

    proctable = myquery_data.info[0].value.data.darray->array;
    for (i = 0; i < myquery_data.info[0].value.data.darray->size; i++) {
        printf("Proctable[%d], namespace %s rank %d exec %s\n", i, proctable[i].proc.nspace,
               proctable[i].proc.rank, basename(proctable[i].executable_name));
    }

    /* Now that we have the proctable for our local processes, this daemon can
     * interact with application processes, such as setting initial breakpoints,
     * or other setup for the debugging * session.
     * If the application was launched by the debugger, then all application
     * tasks should be suspended in PMIx_Init, usually within the application's
     * MPI_Init call.
     * Once initial setup is complete, the daemon sends a release event to the
     * application processes and those processes resume execution.
     */
    (void) strncpy(proc.nspace, target_namespace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    // Since we are using the 'wildcard' only one daemon should send
    // the release message.
    // If we are 'cospawned' then the daemons are not ranked separately
    // from the application (this is a bug) so just have everyone
    // send the release.
    if (0 == myproc.rank || 1 == cospawned_namespace) {

        PMIX_INFO_LIST_START(dirs);
        /* Send release notification to application namespace */
        PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_CUSTOM_RANGE, &proc, PMIX_PROC);
        /* Don't send notification to default event handlers */
        PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_NON_DEFAULT, NULL, PMIX_BOOL);
        PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
        PMIX_INFO_LIST_RELEASE(dirs);
        info = darray.array;
        ninfo = darray.size;

        // Todo: Move this to the main tool
        // https://github.com/openpmix/prrte/pull/857#discussion_r600849033
        sleep(1);
        printf("[%s:%u:%lu] Sending release\n", myproc.nspace, myproc.rank, (unsigned long) pid);
        rc = PMIx_Notify_event(PMIX_DEBUGGER_RELEASE, NULL, PMIX_RANGE_CUSTOM, info, ninfo, NULL,
                               NULL);
        PMIX_DATA_ARRAY_DESTRUCT(&darray);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "[%s:%u:%lu] Sending release failed with error %s(%d)\n", myproc.nspace,
                    myproc.rank, (unsigned long) pid, PMIx_Error_string(rc), rc);
            goto done;
        }
    }

    /* At this point the application processes should be running under debugger
     * control. The daemons can interact further with application processes as
     * needed, or just wait for the application * termination.
     * This example just waits for application termination.
     * Note that if the application processes and daemon processes are spawned
     * by the same PMIx_Spawn call, then no PMIX_EVENT_JOB_END
     * notifications are sent since the daemons are part of the same namespace
     * and are still running.
     */
    if (0 == cospawned_namespace) {
        printf("Waiting for application namespace %s to terminate\n", proc.nspace);
        DEBUG_WAIT_THREAD(&myrel.lock);
        printf("Application namespace %s terminated\n", proc.nspace);
    }

done:
    if (NULL != target_namespace) {
        free(target_namespace);
    }
    /* Call PMIx_tool_finalize to shut down the PMIx runtime */
    printf("Debugger daemon ns %s rank %d pid %lu: Finalizing\n", myproc.nspace, myproc.rank,
           (unsigned long) pid);
    rc = PMIx_tool_finalize();
    fclose(stdout);
    fclose(stderr);
    sleep(1);
    return (rc);
}
