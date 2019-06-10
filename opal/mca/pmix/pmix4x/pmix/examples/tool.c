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
 * Copyright (c) 2019      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include <pmix_tool.h>
#include "examples.h"

static pmix_proc_t myproc = {"UNDEF", PMIX_RANK_UNDEF};

static void cbfunc(pmix_status_t status,
                   pmix_info_t *info, size_t ninfo,
                   void *cbdata,
                   pmix_release_cbfunc_t release_fn,
                   void *release_cbdata)
{
    myquery_data_t *mq = (myquery_data_t*)cbdata;
    size_t n;

    mq->lock.status = status;

    /* save the returned info - it will be
     * released in the release_fn */
    if (0 < ninfo) {
        PMIX_INFO_CREATE(mq->info, ninfo);
        mq->ninfo = ninfo;
        for (n=0; n < ninfo; n++) {
            PMIX_INFO_XFER(&mq->info[n], &info[n]);
        }
    }

    /* let the library release the data */
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }

    /* release the block */
    DEBUG_WAKEUP_THREAD(&mq->lock);
}

/* this is an event notification function that we explicitly request
 * be called when the PMIX_ERR_JOB_TERMINATED notification is issued.
 * We could catch it in the general event notification function and test
 * the status to see if it was "job terminated", but it often is simpler
 * to declare a use-specific notification callback point. In this case,
 * we are asking to know whenever a job terminates, and we will then
 * know we can exit */
static void release_fn(size_t evhdlr_registration_id,
                       pmix_status_t status,
                       const pmix_proc_t *source,
                       pmix_info_t info[], size_t ninfo,
                       pmix_info_t results[], size_t nresults,
                       pmix_event_notification_cbfunc_fn_t cbfunc,
                       void *cbdata)
{
    myrel_t *lock;
    bool found;
    int exit_code = 0;
    size_t n;
    pmix_proc_t *affected = NULL;

    /* find the return object */
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

    fprintf(stderr, "TOOL NOTIFIED THAT ALL CHILD PROCS %s TERMINATED \n",
            (NULL == affected) ? "NULL" : affected->nspace);
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
    DEBUG_WAKEUP_THREAD(lock);
}


int main(int argc, char **argv)
{
    pmix_status_t rc;
    pmix_proc_t proc;
    pmix_query_t *query;
    size_t nq, ninfo = 0, n, m;
    myquery_data_t mydata;
    pmix_info_t *info = NULL, *iptr;
    char *server_uri = NULL;
    char *nspace = NULL;
    char *nodename = NULL;
    pmix_data_array_t *darray, *dptr;
    bool geturi = false;
    char hostname[1024];
    char *spawn = NULL;
    pmix_app_t app;
    pmix_nspace_t child;
    pmix_status_t code = PMIX_ERR_JOB_TERMINATED;
    myrel_t myrel;
    mylock_t mylock;

    gethostname(hostname, 1024);
    for (n=1; n < (size_t)argc; n++) {
        if (0 == strcmp("-u", argv[n]) || 0 == strcmp("--url", argv[n])) {
            if (NULL == argv[n+1]) {
                fprintf(stderr, "Must provide URI argument to %s option\n", argv[n]);
                exit(1);
            }
            server_uri = argv[n+1];
            ++n;
            ++ninfo;
        } else if (0 == strcmp("-nspace", argv[n]) || 0 == strcmp("--nspace", argv[n])) {
            if (NULL == argv[n+1]) {
                fprintf(stderr, "Must provide nspace argument to %s option\n", argv[n]);
                exit(1);
            }
            nspace = argv[n+1];
            ++n;
        } else if (0 == strcmp("-uri", argv[n]) || 0 == strcmp("--uri", argv[n])) {
            /* retrieve the PMIx server's uri from the indicated node */
            nodename = argv[n+1];
            geturi = true;
            ++n;
        } else if (0 == strcmp("-spawn", argv[n]) || 0 == strcmp("--spawn", argv[n])) {
            if (NULL == argv[n+1]) {
                fprintf(stderr, "Must provide executable argument to %s option\n", argv[n]);
                exit(1);
            }
            spawn = argv[n+1];
            ++n;
            ninfo += 2;
        }
    }

    PMIX_INFO_CREATE(info, ninfo);
    n = 0;
    if (NULL != server_uri) {
        PMIX_INFO_LOAD(&info[n], PMIX_SERVER_URI, server_uri, PMIX_STRING);
        fprintf(stderr, "Connecting to %s\n", server_uri);
        ++n;
    }
    if (NULL != spawn) {
        PMIX_INFO_LOAD(&info[n], PMIX_TOOL_CONNECT_OPTIONAL, NULL, PMIX_BOOL);
        ++n;
        PMIX_INFO_LOAD(&info[n], PMIX_LAUNCHER, NULL, PMIX_BOOL);
        ++n;
    }

    /* init us */
    if (PMIX_SUCCESS != (rc = PMIx_tool_init(&myproc, info, ninfo))) {
        fprintf(stderr, "PMIx_tool_init failed: %d\n", rc);
        exit(rc);
    }
    if (NULL != info) {
        PMIX_INFO_FREE(info, ninfo);
    }

    if (geturi) {
        nq = 1;
        PMIX_QUERY_CREATE(query, nq);
        PMIX_ARGV_APPEND(rc, query[0].keys, PMIX_SERVER_URI);
        if (NULL != nodename) {
            PMIX_QUERY_QUALIFIERS_CREATE(&query[0], 1);
            PMIX_INFO_LOAD(&query[0].qualifiers[0], PMIX_HOSTNAME, nodename, PMIX_STRING);
        }
        DEBUG_CONSTRUCT_MYQUERY(&mydata);
        if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(query, nq, cbfunc, (void*)&mydata))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Query_info failed: %d\n", myproc.nspace, myproc.rank, rc);
            goto done;
        }
        DEBUG_WAIT_THREAD(&mydata.lock);
        /* find the response */
        if (PMIX_SUCCESS == mydata.lock.status) {
            /* should be in the first key */
            if (PMIX_CHECK_KEY(&mydata.info[0], PMIX_SERVER_URI)) {
                fprintf(stderr, "PMIx server URI for node %s: %s\n",
                        (NULL == nodename) ? hostname : nodename,
                        mydata.info[0].value.data.string);
            } else {
                fprintf(stderr, "Query returned wrong info key at first posn: %s\n", mydata.info[0].key);
            }
        } else {
            fprintf(stderr, "Query returned error: %s\n", PMIx_Error_string(mydata.lock.status));
        }
        DEBUG_DESTRUCT_MYQUERY(&mydata);
        goto done;
    }

    /* if we want to spawn a proc, then do so */
    if (NULL != spawn) {
        DEBUG_CONSTRUCT_LOCK(&myrel.lock);
        /* setup notification so we know when the child has terminated */
        PMIX_INFO_CREATE(info, 2);
        PMIX_INFO_LOAD(&info[0], PMIX_EVENT_RETURN_OBJECT, &myrel, PMIX_POINTER);
        /* only call me back when this specific job terminates - the
         * children will have our nspace */
        PMIX_LOAD_PROCID(&proc, myproc.nspace, PMIX_RANK_WILDCARD);
        PMIX_INFO_LOAD(&info[1], PMIX_EVENT_AFFECTED_PROC, &proc, PMIX_PROC);

        DEBUG_CONSTRUCT_LOCK(&mylock);
        PMIx_Register_event_handler(&code, 1, info, 2,
                                    release_fn, evhandler_reg_callbk, (void*)&mylock);
        DEBUG_WAIT_THREAD(&mylock);
        rc = mylock.status;
        DEBUG_DESTRUCT_LOCK(&mylock);
        PMIX_INFO_FREE(info, 2);

        PMIX_APP_CONSTRUCT(&app);
        PMIX_ARGV_SPLIT(app.argv, spawn, ' ');
        app.cmd = strdup(app.argv[0]);
        app.maxprocs = 1;
        rc = PMIx_Spawn(NULL, 0, &app, 1, child);
        if (PMIX_SUCCESS != rc && PMIX_OPERATION_SUCCEEDED != rc) {
            fprintf(stderr, "Failed to spawn %s\n", PMIx_Error_string(rc));
            goto done;
        }
        /* wait here */
        DEBUG_WAIT_THREAD(&myrel.lock);
        goto done;
    }

    if (NULL == nspace) {
        /* query the list of active nspaces */
        nq = 1;
        PMIX_QUERY_CREATE(query, nq);
        PMIX_ARGV_APPEND(rc, query[0].keys, PMIX_QUERY_NAMESPACE_INFO);
        DEBUG_CONSTRUCT_MYQUERY(&mydata);
        if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(query, nq, cbfunc, (void*)&mydata))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Query_info failed: %d\n", myproc.nspace, myproc.rank, rc);
            goto done;
        }
        DEBUG_WAIT_THREAD(&mydata.lock);
        /* find the response */
        if (PMIX_SUCCESS == mydata.lock.status) {
            /* should be in the first key */
            if (PMIX_CHECK_KEY(&mydata.info[0], PMIX_QUERY_NAMESPACE_INFO)) {
                darray = mydata.info[0].value.data.darray;
                fprintf(stderr, "ACTIVE NSPACES:\n");
                if (NULL == darray || 0 == darray->size || NULL == darray->array) {
                    fprintf(stderr, "\tNone\n");
                } else {
                    info = (pmix_info_t*)darray->array;
                    if (NULL == info) {
                        fprintf(stderr, "Error\n");
                    } else {
                        for (n=0; n < darray->size; n++) {
                            dptr = info[n].value.data.darray;
                            if (NULL == dptr || 0 == dptr->size || NULL == dptr->array) {
                                fprintf(stderr, "Error in array %s\n", (NULL == dptr) ? "NULL" : "NON-NULL");
                                break;
                            }
                            iptr = (pmix_info_t*)dptr->array;
                            for (m=0; m < dptr->size; m++) {
                                fprintf(stderr, "\t%s", iptr[m].value.data.string);
                            }
                            fprintf(stderr, "\n");
                        }
                    }
                }
            } else {
                fprintf(stderr, "Query returned wrong info key at first posn: %s\n", mydata.info[0].key);
            }
        } else {
            fprintf(stderr, "Query returned error: %s\n", PMIx_Error_string(mydata.lock.status));
        }
        DEBUG_DESTRUCT_MYQUERY(&mydata);
    } else {
        nq = 1;
        PMIX_QUERY_CREATE(query, nq);
        PMIX_ARGV_APPEND(rc, query[0].keys, PMIX_JOB_SIZE);
        PMIX_INFO_CREATE(query[0].qualifiers, 1);
        query[0].nqual = 1;
        PMIX_INFO_LOAD(&query[0].qualifiers[0], PMIX_NSPACE, nspace, PMIX_STRING);
        DEBUG_CONSTRUCT_MYQUERY(&mydata);
        if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(query, nq, cbfunc, (void*)&mydata))) {
            fprintf(stderr, "Client ns %s rank %d: PMIx_Query_info failed: %d\n", myproc.nspace, myproc.rank, rc);
            goto done;
        }
        DEBUG_WAIT_THREAD(&mydata.lock);
        /* find the response */
        if (PMIX_SUCCESS == mydata.lock.status) {
            /* should be in the first key */
            if (PMIX_CHECK_KEY(&mydata.info[0], PMIX_JOB_SIZE)) {
                fprintf(stderr, "JOB SIZE FOR NSPACE %s: %lu\n", nspace, (unsigned long)mydata.info[0].value.data.uint32);
            } else {
                fprintf(stderr, "Query returned wrong info key at first posn: %s\n", mydata.info[0].key);
            }
        } else {
            fprintf(stderr, "Query returned error: %s\n", PMIx_Error_string(mydata.lock.status));
        }
        DEBUG_DESTRUCT_MYQUERY(&mydata);
    }

 done:
    /* finalize us */
    PMIx_tool_finalize();
    return(rc);
}
