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

static int attach_to_running_job(char *nspace);
static bool waiting_for_debugger = true;
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
    /* this example doesn't do anything with default events */
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
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
    /* tell the event handler state machine that we are the last step */
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
    /* flag that the debugger is complete so we can exit */
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
    pmix_status_t rc;
    pmix_info_t *info, *dinfo;
    pmix_app_t *app, *debugger;
    size_t ninfo, napps, dninfo;
    char *tdir, *nspace = NULL;
    char appspace[PMIX_MAX_NSLEN+1], dspace[PMIX_MAX_NSLEN+1];
    int i;
    pmix_query_t *query;
    size_t nq, n;
    myquery_data_t myquery_data;
    bool cospawn = false, stop_on_exec = false;
    char cwd[1024];
    volatile int active;
    pmix_status_t code = PMIX_ERR_JOB_TERMINATED;

    /* Process any arguments we were given */
    for (i=1; i < argc; i++) {
        if (0 == strcmp(argv[i], "-h") ||
            0 == strcmp(argv[i], "--help")) {
            /* print the usage message and exit */

        }
        if (0 == strcmp(argv[i], "-a") ||
            0 == strcmp(argv[i], "--attach")) {
            if (NULL != nspace) {
                /* can only support one */
                fprintf(stderr, "Cannot attach to more than one nspace\n");
                exit(1);
            }
            /* the next argument must be the nspace */
            ++i;
            if (argc == i) {
                /* they goofed */
                fprintf(stderr, "The %s option requires an <nspace> argument\n", argv[i]);
                exit(1);
            }
            nspace = strdup(argv[i]);
        } else {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            exit(1);
        }
    }

    /* we need to provide some info to the PMIx tool library so
     * it can find the server's contact info. The simplest way
     * of doing this here is to look for an environmental variable
     * that tells us where to look. The PMIx reference server only
     * allows one instantiation of the server per user, so setting
     * this up is something a user could do in their login script.
     * The reference server is based on OpenMPI, and so the contact
     * info will always be found at:
     *
     * $TMPDIR/ompi.<nodename>.<numerical-userid>/dvm
     *
     * NOTE: we will eliminate this requirement in a future version
     */

    if (NULL == (tdir = getenv("PMIX_SERVER_TMPDIR"))) {
        fprintf(stderr, "Tool usage requires that the PMIX_SERVER_TMPDIR envar\n");
        fprintf(stderr, "be set to point at the directory where the PMIx Reference\n");
        fprintf(stderr, "Server leaves its contact info file.\n");
        exit(1);
    }

    /* init us - pass along the location of the contact file */
    ninfo = 1;
    PMIX_INFO_CREATE(info, ninfo);
    PMIX_INFO_LOAD(&info[0], PMIX_SERVER_TMPDIR, tdir, PMIX_STRING);

    if (PMIX_SUCCESS != (rc = PMIx_tool_init(&myproc, info, ninfo))) {
        fprintf(stderr, "PMIx_tool_init failed: %d\n", rc);
        exit(rc);
    }
    PMIX_INFO_FREE(info, ninfo);

    fprintf(stderr, "Tool ns %s rank %d: Running\n", myproc.nspace, myproc.rank);

    /* register a default event handler */
    active = -1;
    PMIx_Register_event_handler(NULL, 0, NULL, 0,
                                notification_fn, evhandler_reg_callbk, (void*)&active);
    while (-1 == active) {
        usleep(10);
    }
    if (0 != active) {
        exit(active);
    }

    /* register another handler specifically for when the debugger
     * job completes */
    active = -1;
    PMIx_Register_event_handler(&code, 1, NULL, 0,
                                release_fn, evhandler_reg_callbk, (void*)&active);
    while (-1 == active) {
        usleep(10);
    }
    if (0 != active) {
        exit(active);
    }

    /* if we are attaching to a running job, then attach to it */
    if (NULL != nspace) {
        if (PMIX_SUCCESS != (rc = attach_to_running_job(nspace))) {
            fprintf(stderr, "Failed to attach to nspace %s: error code %d\n",
                    nspace, rc);
            goto done;
        }
    } else {
        /* this is an initial launch - we need to launch the application
         * plus the debugger daemons, letting the RM know we are debugging
         * so that it will "pause" the app procs until we are ready. First
         * we need to know if this RM supports co-spawning of daemons with
         * the application, or if we need to launch the daemons as a separate
         * spawn command. The former is faster and more scalable, but not
         * every RM may support it. We also need to ask for debug support
         * so we know if the RM can stop-on-exec, or only supports stop-in-init */
        nq = 1;
        PMIX_QUERY_CREATE(query, nq);
        PMIX_ARGV_APPEND(rc, query[0].keys, PMIX_QUERY_SPAWN_SUPPORT);
        PMIX_ARGV_APPEND(rc, query[0].keys, PMIX_QUERY_DEBUG_SUPPORT);
        /* setup the caddy to retrieve the data */
        myquery_data.info = NULL;
        myquery_data.ninfo = 0;
        myquery_data.active = true;
        /* execute the query */
        fprintf(stderr, "Debugger: querying capabilities\n");
        if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(query, nq, cbfunc, (void*)&myquery_data))) {
            fprintf(stderr, "PMIx_Query_info failed: %d\n", rc);
            goto done;
        }
        while (myquery_data.active) {
            usleep(10);
        }

        /* we should have received back two info structs, one containing
         * a comma-delimited list of PMIx spawn attributes the RM supports,
         * and the other containing a comma-delimited list of PMIx debugger
         * attributes it supports */
        if (2 != myquery_data.ninfo) {
            /* this is an error */
            fprintf(stderr, "PMIx Query returned an incorrect number of results: %lu\n", myquery_data.ninfo);
            PMIX_INFO_FREE(myquery_data.info, myquery_data.ninfo);
            goto done;
        }

        /* we would like to co-spawn the debugger daemons with the app, but
         * let's first check to see if this RM supports that operation by
         * looking for the PMIX_COSPAWN_APP attribute in the spawn support
         *
         * We will also check to see if "stop_on_exec" is supported. Few RMs
         * do so, which is why we have to check. The reference server sadly is
         * not one of them, so we shouldn't find it here
         *
         * Note that the PMIx reference server always returns the query results
         * in the same order as the query keys. However, this is not guaranteed,
         * so we should search the returned info structures to find the desired key
         */
        for (n=0; n < myquery_data.ninfo; n++) {
            if (0 == strcmp(myquery_data.info[n].key, PMIX_QUERY_SPAWN_SUPPORT)) {
                /* see if the cospawn attribute is included */
                if (NULL != strstr(myquery_data.info[n].value.data.string, PMIX_COSPAWN_APP)) {
                    cospawn = true;
                } else {
                    cospawn = false;
                }
            } else if (0 == strcmp(myquery_data.info[n].key, PMIX_QUERY_DEBUG_SUPPORT)) {
                if (NULL != strstr(myquery_data.info[n].value.data.string, PMIX_DEBUG_STOP_ON_EXEC)) {
                    stop_on_exec = true;
                } else {
                    stop_on_exec = false;
                }
            }
        }

        /* if cospawn is true, then we can launch both the app and the debugger
         * daemons at the same time */
        if (cospawn) {

        } else {
            /* we must do these as separate launches, so do the app first */
            napps = 1;
            PMIX_APP_CREATE(app, napps);
            /* setup the executable */
            app[0].cmd = strdup("client");
            PMIX_ARGV_APPEND(rc, app[0].argv, "./client");
            if (NULL == getcwd(cwd, 1024)) {  // point us to our current directory
                goto done;
            }
            app[0].cwd = strdup(cwd);
            app[0].maxprocs = 2;
            /* provide job-level directives so the apps do what the user requested */
            ninfo = 2;
            PMIX_INFO_CREATE(info, ninfo);
            PMIX_INFO_LOAD(&info[0], PMIX_MAPBY, "slot", PMIX_STRING);  // map by slot
            if (stop_on_exec) {
                PMIX_INFO_LOAD(&info[1], PMIX_DEBUG_STOP_ON_EXEC, NULL, PMIX_BOOL);  // procs are to stop on first instruction
            } else {
                PMIX_INFO_LOAD(&info[1], PMIX_DEBUG_STOP_IN_INIT, NULL, PMIX_BOOL);  // procs are to pause in PMIx_Init for debugger attach
            }
            /* spawn the job - the function will return when the app
             * has been launched */
            fprintf(stderr, "Debugger: spawning %s\n", app[0].cmd);
            if (PMIX_SUCCESS != (rc = PMIx_Spawn(info, ninfo, app, napps, appspace))) {
                fprintf(stderr, "Application failed to launch with error: %s\n", PMIx_Error_string(rc));
                goto done;
            }
            PMIX_INFO_FREE(info, ninfo);
            PMIX_APP_FREE(app, napps);

            /* setup the debugger */
            PMIX_APP_CREATE(debugger, 1);
            debugger[0].cmd = strdup("./debuggerd");
            PMIX_ARGV_APPEND(rc, debugger[0].argv, "./debuggerd");
            debugger[0].cwd = strdup(cwd);
            /* provide directives so the daemons go where we want, and
             * let the RM know these are debugger daemons */
            dninfo = 5;
            PMIX_INFO_CREATE(dinfo, dninfo);
            PMIX_INFO_LOAD(&dinfo[0], PMIX_MAPBY, "ppr:1:node", PMIX_STRING);  // instruct the RM to launch one copy of the executable on each node
            PMIX_INFO_LOAD(&dinfo[1], PMIX_DEBUGGER_DAEMONS, NULL, PMIX_BOOL); // these are debugger daemons
            PMIX_INFO_LOAD(&dinfo[2], PMIX_DEBUG_JOB, appspace, PMIX_STRING); // the nspace being debugged
            PMIX_INFO_LOAD(&dinfo[3], PMIX_NOTIFY_COMPLETION, NULL, PMIX_BOOL); // notify us when the debugger job completes
            PMIX_INFO_LOAD(&dinfo[4], PMIX_DEBUG_WAITING_FOR_NOTIFY, NULL, PMIX_BOOL);  // tell the daemon that the proc is waiting to be released
            /* spawn the daemons */
            fprintf(stderr, "Debugger: spawning %s\n", debugger[0].cmd);
            if (PMIX_SUCCESS != (rc = PMIx_Spawn(dinfo, dninfo, debugger, 1, dspace))) {
                fprintf(stderr, "Debugger daemons failed to launch with error: %s\n", PMIx_Error_string(rc));
                goto done;
            }

            /* cleanup */
            PMIX_INFO_FREE(dinfo, dninfo);
            PMIX_APP_FREE(debugger, 1);
        }


        /* this is where a debugger tool would wait until the debug operation is complete */
        while (waiting_for_debugger) {
            sleep(1);
        }
    }

  done:
    PMIx_tool_finalize();

    return(rc);
}

typedef struct {
    volatile bool active;
    pmix_status_t status;
    pmix_info_t *info;
    size_t ninfo;
} mydbug_query_t;


static void infocbfunc(pmix_status_t status,
                       pmix_info_t *info, size_t ninfo,
                       void *cbdata,
                       pmix_release_cbfunc_t release_fn,
                       void *release_cbdata)
{
    mydbug_query_t *q = (mydbug_query_t*)cbdata;
    size_t n;

    q->status = status;
    q->info = NULL;
    q->ninfo = ninfo;
    if (0 < ninfo) {
        PMIX_INFO_CREATE(q->info, q->ninfo);
        for (n=0; n < ninfo; n++) {
            PMIX_INFO_XFER(&q->info[n], &info[n]);
        }
    }
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
    q->active = false;
}

static int attach_to_running_job(char *nspace)
{
    pmix_status_t rc;
    pmix_query_t *query;
    size_t nq;
    mydbug_query_t *q;

    /* query the active nspaces so we can verify that the
     * specified one exists */
    nq = 1;
    PMIX_QUERY_CREATE(query, nq);
    query[0].keys = (char**)malloc(2 * sizeof(char*));
    query[0].keys[0] = strdup(PMIX_QUERY_NAMESPACES);
    query[0].keys[1] = NULL;

    q = (mydbug_query_t*)malloc(sizeof(mydbug_query_t));
    q->active = true;

    if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(query, nq, infocbfunc, (void*)q))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Query_info failed: %d\n", myproc.nspace, myproc.rank, rc);
        return -1;
    }
    /* wait for a response */
    while (q->active) {
        sleep(1);
    }

    if (NULL == q->info) {
        fprintf(stderr, "Query returned no info\n");
        return -1;
    }
    /* the query should have returned a comma-delimited list of nspaces */
    if (PMIX_STRING != q->info[0].value.type) {
        fprintf(stderr, "Query returned incorrect data type: %d\n", q->info[0].value.type);
        return -1;
    }
    if (NULL == q->info[0].value.data.string) {
        fprintf(stderr, "Query returned no active nspaces\n");
        return -1;
    }

    fprintf(stderr, "Query returned %s\n", q->info[0].value.data.string);
    return 0;

#if 0
    /* split the returned string and look for the given nspace */

    /* if not found, then we have an error */
    PMIX_INFO_FREE(info, ninfo);

    /* get the proctable for this nspace */
    ninfo = 1;
    PMIX_INFO_CREATE(info, ninfo);
    (void)strncpy(info[0].key, PMIX_QUERY_PROC_TABLE, PMIX_MAX_KEYLEN);
    (void)strncpy(info[0].qualifier, nspace, PMIX_MAX_KEYLEN);
    if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(info, ninfo, infocbfunc, (void*)&active))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Query_info_nb failed: %d\n", myproc.nspace, myproc.rank, rc);
        return -1;
    }
    /* wait to get a response */

    /* the query should have returned a data_array */
    if (PMIX_DATA_ARRAY != info[0].type) {
        fprintf(stderr, "Query returned incorrect data type: %d\n", info[0].type);
        return -1;
    }
    if (NULL == info[0].data.darray.array) {
        fprintf(stderr, "Query returned no proctable info\n");
        return -1;
    }
    /* the data array consists of a struct:
     *     size_t size;
     *     void* array;
     *
     * In this case, the array is composed of pmix_proc_info_t structs:
     *     pmix_proc_t proc;   // contains the nspace,rank of this proc
     *     char* hostname;
     *     char* executable_name;
     *     pid_t pid;
     *     int exit_code;
     *     pmix_proc_state_t state;
     */

    /* this is where a debugger tool would process the proctable to
     * create whatever blob it needs to provide to its daemons */
    PMIX_INFO_FREE(info, ninfo);

    /* setup the debugger daemon spawn request */
    napps = 1;
    PMIX_APP_CREATE(app, napps);
    /* setup the name of the daemon executable to launch */
    app[0].cmd = strdup("debuggerdaemon");
    app[0].argc = 1;
    app[0].argv = (char**)malloc(2*sizeof(char*));
    app[0].argv[0] = strdup("debuggerdaemon");
    app[0].argv[1] = NULL;
    /* provide directives so the daemons go where we want, and
     * let the RM know these are debugger daemons */
    ninfo = 3;
    PMIX_INFO_CREATE(app[0].info, ninfo);
    PMIX_INFO_LOAD(&app[0].info[0], PMIX_MAPBY, "ppr:1:node", PMIX_STRING);  // instruct the RM to launch one copy of the executable on each node
    PMIX_INFO_LOAD(&app[0].info[1], PMIX_DEBUGGER_DAEMONS, true, PMIX_BOOL); // these are debugger daemons
    PMIX_INFO_LOAD(&app[0].info[2], PMIX_DEBUG_TARGET, nspace, PMIX_STRING); // the "jobid" of the application to be debugged

    /* spawn the daemons */
    PMIx_Spawn(NULL, 0, app, napps, dspace);
    /* cleanup */
    PMIX_APP_FREE(app, napps);

    /* this is where a debugger tool would wait until the debug operation is complete */

    return 0;
#endif
}
