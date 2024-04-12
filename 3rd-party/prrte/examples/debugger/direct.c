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
 * Copyright (c) 2021      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#define _GNU_SOURCE
#include <ctype.h>
#include <limits.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include "debugger.h"
#include <pmix_tool.h>

static pmix_proc_t myproc;
static char client_nspace[PMIX_MAX_NSLEN + 1];
static char daemon_nspace[PMIX_MAX_NSLEN + 1];
static pmix_proc_t *connected_servers;

static bool stop_in_init = true;
static bool stop_on_exec = false;
static bool stop_in_init_supported = false;
static bool stop_on_exec_supported = false;
static bool cospawn_supported = false;
static bool cospawn_reqd = false;
static bool dbactive = true;
static int app_npernode = 2; // > 0. Default 2 ppn
static int app_np
    = 2; // <= 0 means use default from prte. Default to single node. Must be multiple of npernode
static int daemon_colocate_per_proc = 0; // 0 = disable
static int daemon_colocate_per_node = 0; // 0 = disable

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
static void cbfunc(pmix_status_t status, pmix_info_t *info, size_t ninfo, void *cbdata,
                   pmix_release_cbfunc_t release_fn, void *release_cbdata)
{
    myquery_data_t *mq = (myquery_data_t *) cbdata;
    size_t n;

    printf("Called %s as callback for PMIx_Query\n", __FUNCTION__);
    mq->status = status;
    /* save the returned info - the PMIx library "owns" it
     * and will release it and perform other cleanup actions
     * when release_fn is called */
    if (0 < ninfo) {
        PMIX_INFO_CREATE(mq->info, ninfo);
        mq->ninfo = ninfo;
        for (n = 0; n < ninfo; n++) {
            printf("Key %s Type %s(%d)\n", info[n].key, PMIx_Data_type_string(info[n].value.type),
                   info[n].value.type);
            PMIX_INFO_XFER(&mq->info[n], &info[n]);
        }
    }

    /* let the library release the data and cleanup from
     * the operation */
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }

    /* release the block */
    DEBUG_WAKEUP_THREAD(&mq->lock);
}

/* this is the event notification function we pass down below
 * when registering for general events - i.e.,, the default
 * handler. We don't technically need to register one, but it
 * is usually good practice to catch any events that occur */
static void notification_fn(size_t evhdlr_registration_id, pmix_status_t status,
                            const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    myrel_t *lock;
    size_t n;

    printf("%s called as callback for event=%s\n", __FUNCTION__, PMIx_Error_string(status));
    lock = NULL;
    if (PMIX_ERR_UNREACH == status || PMIX_ERR_LOST_CONNECTION == status) {
        /* we should always have info returned to us - if not, there is
         * nothing we can do */
        if (NULL != info) {
            for (n = 0; n < ninfo; n++) {
                if (PMIX_CHECK_KEY(&info[n], PMIX_EVENT_RETURN_OBJECT)) {
                    lock = (myrel_t *) info[n].value.data.ptr;
                }
            }
        }

        /* If a pointer to a lock was passed then save status and
         * release the lock */
        if (NULL != lock) {
            lock->exit_code = status;
            lock->exit_code_given = true;
            DEBUG_WAKEUP_THREAD(&lock->lock);
        }
    }

    /* this example doesn't do anything with default events */
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
    }
}

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

    printf("%s called as callback for event=%s source=%s:%d\n", __FUNCTION__,
           PMIx_Error_string(status), source->nspace, source->rank);
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

    printf("DEBUGGER NOTIFIED THAT JOB %s TERMINATED \n",
           (NULL == affected) ? "NULL" : affected->nspace);
    if (found) {
        if (!lock->exit_code_given) {
            lock->exit_code = exit_code;
            lock->exit_code_given = true;
        }
    }

    /* A system PMIx daemon may have kept track of notifications for
     * termination of previous application runs, and may send those
     * notifications to this process, which has registered a callback for
     * application terminations. Those notifcations need to be ignored.
     *
     * Therefore, in the co-spawn case, we expect one termination notification,
     * which is for the combined application/daemon namespace when the daemon
     * terminates.
     *
     * In the separate spawn case, we expect two terminations, the application
     * and the daemon. */
    if ((0 == strcmp(daemon_nspace, source->nspace))
        || (0 == strcmp(client_nspace, source->nspace))) {
        lock->lock.count--;
        if (0 == lock->lock.count) {
            DEBUG_WAKEUP_THREAD(&lock->lock);
        }
    }

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

    printf("%s called to register callback\n", __FUNCTION__);
    if (PMIX_SUCCESS != status) {
        fprintf(stderr, "Client %s:%d EVENT HANDLER REGISTRATION FAILED WITH STATUS %d, ref=%lu\n",
                myproc.nspace, myproc.rank, status, (unsigned long) evhandler_ref);
    }
    lock->status = status;
    DEBUG_WAKEUP_THREAD(lock);
}

static void debug_ready_cb(size_t evhdlr_registration_id, pmix_status_t status,
                           const pmix_proc_t *source, pmix_info_t info[],
                           size_t ninfo, pmix_info_t results[], size_t nresults,
                           pmix_event_notification_cbfunc_fn_t cbfunc,
                           void *cbdata)
{
    size_t n;
    printf("%s called for event notification %s from nspace %s\n", __FUNCTION__,
           PMIx_Error_string(status), source->nspace);
    for (n = 0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_NSPACE)) {
            printf("Got %s notification for target nspace %s\n",
                   PMIx_Error_string(status), info[n].value.data.string);
            break;
        }
    }
    dbactive = false;
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
}

/* Register for a PMIX_READY_FOR_DEBUG event issued by the system server then
 * wait for that event to be issued or until the timeout limit is reached */
static int wait_for_ready(myrel_t *myrel) 
{
    void *dirs;
    pmix_info_t *info;
    pmix_status_t rc;
    size_t ninfo;
    int n;
    mylock_t mylock;
    pmix_status_t code = PMIX_READY_FOR_DEBUG;
    pmix_data_array_t darray;

    DEBUG_CONSTRUCT_LOCK(&mylock);
    PMIX_INFO_LIST_START(dirs);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_RETURN_OBJECT, &myrel, PMIX_POINTER);
    /* Register for PMIX_READY_FOR_DEBUG event. This is sent from system server once all
     * application processes are ready for debug. */
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_AFFECTED_PROC, &connected_servers[0], PMIX_PROC);
    PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
    PMIX_INFO_LIST_RELEASE(dirs);
    info = darray.array;
    ninfo = darray.size;
    PMIx_Register_event_handler(&code, 1, info, ninfo, debug_ready_cb,
                                evhandler_reg_callbk, (void *) &mylock);
    DEBUG_WAIT_THREAD(&mylock);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);
    printf("Debugger: Registered for READY_FOR_DEBUG event for nspace %s\n", 
           connected_servers[0].nspace);
    rc = mylock.status;
    DEBUG_DESTRUCT_LOCK(&mylock);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Registration for PMIX_READY_FOR_DEBUG failed: %s\n",
                PMIx_Error_string(rc));
        return -1;
    }

    n = 0;
    printf("Waiting for PMIX_READY_FOR_DEBUG event to be posted\n");
    while (dbactive) {
        struct timespec tp = {0, 500000000};
        nanosleep(&tp, NULL);
        ++n;
        if (n > 10) {
            fprintf(stderr, "Error: Target not ready for debug by timeout limit\n");
            return -1;
        }
    }
    return 0;
}

static int cospawn_launch(myrel_t *myrel)
{
    void *dirs;
    pmix_info_t *info;
    pmix_app_t *app;
    size_t ninfo;
    int code = PMIX_EVENT_JOB_END;
    pmix_status_t rc;
    int n;
    pmix_data_array_t data_array;
    mylock_t mylock;
    pmix_proc_t daemon_proc;
    pmix_rank_t all_ranks = PMIX_RANK_WILDCARD;
    char cwd[_POSIX_PATH_MAX + 1];
    char map_str[128];
    pmix_data_array_t darray, daemon_darray;

    printf("Calling %s to spawn application processes and debugger daemon\n", __FUNCTION__);
    /* Provide job-level directives so the apps do what the user requested.
     * These attributes apply to both the application and daemon processes. */
    PMIX_INFO_LIST_START(dirs);
    /* Forward stdout to this process */
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_FWD_STDOUT, NULL, PMIX_BOOL);
    /* Forward stderr to this process */
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_FWD_STDERR, NULL, PMIX_BOOL);
    /* Process that is spawning processes is a tool process */
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_REQUESTOR_IS_TOOL, NULL, PMIX_BOOL);
    /* Map spawned processes by slot */
    sprintf(map_str, "ppr:%d:node", app_npernode);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_MAPBY, map_str, PMIX_STRING);
    PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
    PMIX_INFO_LIST_RELEASE(dirs);
    info = darray.array;
    ninfo = darray.size;

    /* The application and daemon processes are being spawned together
     * so create 2 pmix_app_t structures. The first is parameters for
     * the application and the second is parameters for the daemon. */
    PMIX_APP_CREATE(app, 2);
    /* setup the executable */
    app[0].cmd = strdup("./hello");
    /* Set up the executable command arguments, For the co-spawn case
     * the daemon needs to know the namespace of the tool process
     * in addition to setting the application (argv[0]) */
    PMIX_ARGV_APPEND(rc, app->argv, app[0].cmd);
    app[0].env = NULL;
    /* Set the working directory */
    getcwd(cwd, _POSIX_PATH_MAX);
    app[0].cwd = strdup(cwd);
    /* Two application processes */
    if (app_np > 0) {
        app[0].maxprocs = app_np;
    }

    if (stop_on_exec || stop_in_init) {
        app[0].ninfo = 1;
        PMIX_INFO_CREATE(app[0].info, app[0].ninfo);
        n = 0;
        if (stop_on_exec) {
            /* Stop application at first instruction */
            PMIX_INFO_LOAD(&app[n].info[0], PMIX_DEBUG_STOP_ON_EXEC, NULL, PMIX_BOOL);
        } else if (stop_in_init) {
            /* Stop application in PMIx_Init */
            PMIX_INFO_LOAD(&app[n].info[0], PMIX_DEBUG_STOP_IN_INIT, NULL, PMIX_BOOL);
        }
    } else {
        app[0].ninfo = 0;
        app[0].info = NULL;
    }

    /* Set up the daemon executable */
    app[1].cmd = strdup("./daemon");
    /* Set up daemon arguments, in this case just the executable (argv[0]) */
    PMIX_ARGV_APPEND(rc, app[1].argv, app[1].cmd);
    PMIX_ARGV_APPEND(rc, app[1].argv, myproc.nspace);
    app[1].env = NULL;
    /* Set the working directory */
    app[1].cwd = strdup(cwd);
    /* One daemon process */
    app[1].maxprocs = app_np / app_npernode;
    /* Provide directives so the daemons go where we want, and
     * let the RM know these are debugger daemons */
    PMIX_INFO_LIST_START(dirs);
    /* This process is a debugger daemon */
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_DEBUGGER_DAEMONS, NULL, PMIX_BOOL);
    /* Notify this process when debugger job completes */
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_NOTIFY_COMPLETION, NULL, PMIX_BOOL);
    PMIX_INFO_LIST_CONVERT(rc, dirs, &daemon_darray);
    PMIX_INFO_LIST_RELEASE(dirs);
    app[1].info = daemon_darray.array;
    app[1].ninfo = daemon_darray.size;

    /* Spawn the job - the function will return when the app
     * has been launched */
    rc = PMIx_Spawn(info, ninfo, app, 2, client_nspace);
    myrel->lock.count = 1; // app[0].maxprocs + app[1].maxprocs;
    myrel->nspace = strdup(client_nspace);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);
    PMIX_DATA_ARRAY_DESTRUCT(&daemon_darray);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Application failed to launch with error: %s(%d)\n",
                PMIx_Error_string(rc), rc);
        return rc;
    }
    /* Daemon and application are in same namespace */
    printf("Application namespace is %s\n", client_nspace);
    /* Register the termination event handler here with the intent to
     * filter out non-daemon notifcations .
     * Since the daemon is in the same namespace as the application, it's
     * rank is assigned one higher than the last application process. In
     * this example,the daemon's rank is 2.
     */
    strcpy(daemon_proc.nspace, client_nspace);
    strcpy(daemon_nspace, client_nspace);
    daemon_proc.rank = 2;
    data_array.size = 1;
    data_array.type = PMIX_PROC;
    data_array.array = &daemon_proc;
    PMIX_INFO_LIST_START(dirs);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_CUSTOM_RANGE, &data_array, PMIX_DATA_ARRAY);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_RETURN_OBJECT, myrel, PMIX_POINTER);
    PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
    PMIX_INFO_LIST_RELEASE(dirs);
    info = darray.array;
    ninfo = darray.size;

    DEBUG_CONSTRUCT_LOCK(&mylock);
    PMIx_Register_event_handler(&code, 1, info, ninfo, release_fn, evhandler_reg_callbk,
                                (void *) &mylock);
    DEBUG_WAIT_THREAD(&mylock);
    DEBUG_DESTRUCT_LOCK(&mylock);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);
    return rc;
}

static pmix_status_t spawn_debugger(char *appspace, myrel_t *myrel)
{
    void *dirs;
    pmix_status_t rc;
    pmix_info_t *dinfo;
    pmix_app_t *debugger;
    size_t dninfo;
    char cwd[_POSIX_PATH_MAX];
    mylock_t mylock;
    pmix_status_t code = PMIX_EVENT_JOB_END;
    pmix_proc_t proc;
    void *tinfo;
    pmix_data_array_t darray;

    printf("Calling %s to spawn the debugger daemon\n", __FUNCTION__);
    /* Setup the debugger  spawn parameters*/
    PMIX_APP_CREATE(debugger, 1);
    debugger[0].cmd = strdup("./daemon");
    /* Set up debugger command arguments, in this example, just argv[0] */
    PMIX_ARGV_APPEND(rc, debugger[0].argv, "./daemon");
    /* No environment variables */
    debugger[0].env = NULL;
    /* Set the working directory to our current directory */
    getcwd(cwd, _POSIX_PATH_MAX);
    debugger[0].cwd = strdup(cwd);
    /* Spawn daemon processes - 1 per node if not colocating */
    if (daemon_colocate_per_proc < 0 && daemon_colocate_per_node < 0) {
        debugger[0].maxprocs = app_np / app_npernode;
    }
    /* No spawn attributes set here, all are set in dinfo array */
    debugger[0].ninfo = 0;
    debugger[0].info = NULL;
    /* Set attributes for debugger daemon launch and let the RM know these are
     * debugger daemons */
    PMIX_INFO_LIST_START(tinfo);
    /* Indicate a debugger daemon is being spawned */
    PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_DEBUGGER_DAEMONS, NULL, PMIX_BOOL);
    /* Set the name of the namespace being debugged */
    PMIX_LOAD_PROCID(&proc, appspace, PMIX_RANK_WILDCARD);
    PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_DEBUG_TARGET, &proc, PMIX_PROC);
    /* Number of daemons per node in the application allocation */
    if (daemon_colocate_per_node > 0) {
        PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_DEBUG_DAEMONS_PER_NODE, &daemon_colocate_per_node,
                       PMIX_UINT16);
    }
    /* Number of daemons per proc in the application allocation */
    else if (daemon_colocate_per_proc > 0) {
        PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_DEBUG_DAEMONS_PER_PROC, &daemon_colocate_per_proc,
                       PMIX_UINT16);
    }
    /* Launch one daemon per node -- only needed if co-launch is not supported */
    else {
        PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_MAPBY, "ppr:1:node", PMIX_STRING);
    }
    /* Notify this process when the job completes */
    PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_NOTIFY_COMPLETION, NULL, PMIX_BOOL);
    /* Forward stdout to this process */
    PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_FWD_STDOUT, NULL, PMIX_BOOL);
    /* Forward stderr to this process */
    PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_FWD_STDERR, NULL, PMIX_BOOL);

    PMIX_INFO_LIST_CONVERT(rc, tinfo, &darray);
    dinfo = (pmix_info_t*)darray.array;
    dninfo = darray.size;
    PMIX_INFO_LIST_RELEASE(tinfo);

    /* Spawn the daemons */
    printf("Debugger: spawning %s\n", debugger[0].cmd);
    rc = PMIx_Spawn(dinfo, dninfo, debugger, 1, daemon_nspace);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);
    PMIX_APP_FREE(debugger, 1);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Debugger daemons failed to launch with error: %s\n",
                PMIx_Error_string(rc));
        return rc;
    }
    /* Cleanup */

    /* Register callback for when this job terminates */
    myrel->nspace = strdup(daemon_nspace);
    PMIX_LOAD_PROCID(&proc, daemon_nspace, PMIX_RANK_WILDCARD);
    PMIX_INFO_LIST_START(dirs);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_RETURN_OBJECT, myrel, PMIX_POINTER);
    /* Only call me back when this specific job terminates */
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_AFFECTED_PROC, &proc, PMIX_PROC);
    PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
    PMIX_INFO_LIST_RELEASE(dirs);
    dinfo = darray.array;
    dninfo = darray.size;
    /* Track that we need both jobs to terminate */
    myrel->lock.count++;

    DEBUG_CONSTRUCT_LOCK(&mylock);
    PMIx_Register_event_handler(&code, 1, dinfo, dninfo, release_fn, evhandler_reg_callbk,
                                (void *) &mylock);
    DEBUG_WAIT_THREAD(&mylock);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);
    printf("Debugger: Registered for termination on nspace %s\n", daemon_nspace);
    rc = mylock.status;
    DEBUG_DESTRUCT_LOCK(&mylock);

    return rc;
}

int main(int argc, char **argv)
{
    void *dirs;
    pmix_status_t rc;
    pmix_info_t *info;
    pmix_app_t *app;
    size_t ninfo, napps;
    int i, n;
    pmix_query_t *query;
    size_t nq, num_servers;
    myquery_data_t myquery_data;
    pmix_status_t code = PMIX_EVENT_JOB_END;
    mylock_t mylock;
    myrel_t myrel;
    pid_t pid;
    pmix_proc_t proc;
    pmix_data_array_t darray;
    char cwd[_POSIX_PATH_MAX];
    char map_str[128];
    pmix_rank_t all_ranks = PMIX_RANK_WILDCARD;

    pid = getpid();

    /* Process any arguments we were given */
    for (i = 1; i < argc; i++) {
        if (0 == strcmp(argv[i], "-h") || 0 == strcmp(argv[i], "--help")) {
            /* print the usage message and exit */
            printf("Direct Launch Example\n");
            printf("$ prte --daemonize\n");
            printf("$ %s [OPTIONS]\n", argv[0]);
            printf("\n");
            printf(" -c | --cospawn   Test Cospawn\n");
            printf(" --stop-in-init   Stop application in PMIx_Init (Default)\n");
            printf(" --stop-on-exec   Stop application on exec\n");
            printf(" --app-npernode   Number of processes per node (Default: 2)\n");
            printf(" --app-np         Number of total processes. Must be multiple of "
                   "--app-npernode (Default: 2)\n");
            printf(" --daemon-colocate-per-proc  Test Colaunch with Daemons Per Process (Default: "
                   "0 = off)\n");
            printf(" --daemon-colocate-per-node  Test Colaunch with Daemons Per Node (Default: 0 = "
                   "off)\n");
            exit(0);
        } else if (0 == strcmp(argv[i], "-c") || 0 == strcmp(argv[i], "--cospawn")) {
            cospawn_reqd = true;
            break;
        } else if (0 == strcmp(argv[i], "--stop-in-init")) {
            stop_in_init = true;
            stop_on_exec = false;
            break;
        } else if (0 == strcmp(argv[i], "--stop-on-exec")) {
            stop_in_init = false;
            stop_on_exec = true;
            break;
        } else if (0 == strcmp(argv[i], "--app-npernode")) {
            ++i;
            if (i >= argc && isdigit(argv[i][0])) {
                fprintf(stderr, "Error: --app-npernode requires a positive integer argument\n");
                exit(1);
            }
            app_npernode = atoi(argv[i]);
            if (app_npernode <= 0) {
                fprintf(stderr, "Error: --app-npernode requires a positive integer argument\n");
                exit(1);
            }
        } else if (0 == strcmp(argv[i], "--app-np")) {
            ++i;
            if (i >= argc && isdigit(argv[i][0])) {
                fprintf(stderr, "Error: --app-np requires a positive integer argument\n");
                exit(1);
            }
            app_np = atoi(argv[i]);
            if (app_np < 0) {
                fprintf(stderr, "Error: --app-np requires a positive integer argument\n");
                exit(1);
            }
        } else if (0 == strcmp(argv[i], "--daemon-colocate-per-proc")) {
            ++i;
            if (i >= argc && isdigit(argv[i][0])) {
                fprintf(stderr,
                        "Error: --daemon-colocate-per-proc requires a positive integer argument\n");
                exit(1);
            }
            daemon_colocate_per_proc = atoi(argv[i]);
            if (daemon_colocate_per_proc < 0) {
                fprintf(stderr,
                        "Error: --daemon-colocate-per-proc requires a positive integer argument\n");
                exit(1);
            }
        } else if (0 == strcmp(argv[i], "--daemon-colocate-per-node")) {
            ++i;
            if (i >= argc && isdigit(argv[i][0])) {
                fprintf(stderr,
                        "Error: --daemon-colocate-per-node requires a positive integer argument\n");
                exit(1);
            }
            daemon_colocate_per_node = atoi(argv[i]);
            if (daemon_colocate_per_node < 0) {
                fprintf(stderr,
                        "Error: --daemon-colocate-per-node requires a positive integer argument\n");
                exit(1);
            }
        }
    }

    if (daemon_colocate_per_node > 0 && daemon_colocate_per_proc > 0) {
        fprintf(stderr, "Error: Both --daemon-colocate-per-node and --daemon-colocate-per-node "
                        "options present, but are exclusive\n");
        exit(1);
    }
    if (cospawn_reqd && (daemon_colocate_per_node > 0 || daemon_colocate_per_proc > 0)) {
        fprintf(stderr, "Error: Cospawn and Colaunch are not supported at the same time\n");
        exit(1);
    }
    if (app_np < app_npernode || app_np % app_npernode != 0) {
        fprintf(stderr, "Error: --app-np must be a multiple of --app-npernode\n");
        exit(1);
    }

    PMIX_INFO_LIST_START(dirs);

    /* Use the system connection first, if available */
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_CONNECT_SYSTEM_FIRST, NULL, PMIX_BOOL);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_LAUNCHER, NULL, PMIX_BOOL);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_IOF_LOCAL_OUTPUT, NULL, PMIX_BOOL);
    PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
    PMIX_INFO_LIST_RELEASE(dirs);
    info = darray.array;
    ninfo = darray.size;
    /* Init as a tool */
    if (PMIX_SUCCESS != (rc = PMIx_tool_init(&myproc, info, ninfo))) {
        fprintf(stderr, "PMIx_tool_init failed: %s(%d)\n", PMIx_Error_string(rc), rc);
        exit(rc);
    }
    PMIX_DATA_ARRAY_DESTRUCT(&darray);

    printf("Debugger ns %s rank %d pid %lu: Running\n", myproc.nspace, myproc.rank,
           (unsigned long) pid);

    /* We need to know the server we connected to so we can register for 
     * PMIX_READY_FOR_DEBUG notifications from that server when target processes
     * are ready for debug. There should be only one server */
    if (PMIX_SUCCESS != PMIx_tool_get_servers(&connected_servers, &num_servers)) {
        fprintf(stderr, "Unable to get connected servers: %s\n",
                PMIx_Error_string(rc));
        exit(1);
    }
    printf("Connected system server is %s:%d\n", connected_servers[0].nspace,
           connected_servers[0].rank);
    /* Construct my own release first */
    DEBUG_CONSTRUCT_LOCK(&myrel.lock);

    /* Register a default event handler */
    ninfo = 1;
    PMIX_INFO_CREATE(info, ninfo);
    PMIX_INFO_LOAD(&info[0], PMIX_EVENT_RETURN_OBJECT, &myrel, PMIX_POINTER);
    DEBUG_CONSTRUCT_LOCK(&mylock);
    PMIx_Register_event_handler(NULL, 0, info, ninfo, notification_fn, evhandler_reg_callbk,
                                (void *) &mylock);
    DEBUG_WAIT_THREAD(&mylock);
    DEBUG_DESTRUCT_LOCK(&mylock);
    PMIX_INFO_FREE(info, ninfo);

    /* This is an initial launch - we need to launch the application
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
    DEBUG_CONSTRUCT_LOCK(&myquery_data.lock);
    myquery_data.info = NULL;
    myquery_data.ninfo = 0;
    /* execute the query */
    if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(query, nq, cbfunc, (void *) &myquery_data))) {
        fprintf(stderr, "PMIx_Query_info failed: %d\n", rc);
        goto done;
    }
    DEBUG_WAIT_THREAD(&myquery_data.lock);
    DEBUG_DESTRUCT_LOCK(&myquery_data.lock);

    /* We should have received back two info structs, one containing
     * a comma-delimited list of PMIx spawn attributes the RM supports,
     * and the other containing a comma-delimited list of PMIx debugger
     * attributes it supports */
    if (2 != myquery_data.ninfo) {
        /* this is an error */
        fprintf(stderr, "PMIx Query returned an incorrect number of results: %lu\n",
                myquery_data.ninfo);
        PMIX_INFO_FREE(myquery_data.info, myquery_data.ninfo);
        goto done;
    }

    /* We would like to co-spawn the debugger daemons with the app, but
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
    for (n = 0; n < myquery_data.ninfo; n++) {
        if (0 == strcmp(myquery_data.info[n].key, PMIX_QUERY_SPAWN_SUPPORT)) {
            /* See if the cospawn attribute is included */
            if (NULL != strstr(myquery_data.info[n].value.data.string, PMIX_COSPAWN_APP)) {
                cospawn_supported = true;
            }
        } else if (0 == strcmp(myquery_data.info[n].key, PMIX_QUERY_DEBUG_SUPPORT)) {
            /* See if stop on exec is included */
            if (NULL != strstr(myquery_data.info[n].value.data.string, PMIX_DEBUG_STOP_ON_EXEC)) {
                stop_on_exec_supported = true;
            }
            /* See if stop in init is included */
            if (NULL != strstr(myquery_data.info[n].value.data.string, PMIX_DEBUG_STOP_IN_INIT)) {
                stop_in_init_supported = true;
            }
        }
    }

    if (!stop_on_exec_supported && stop_on_exec) {
        fprintf(stderr, "Error: Stop-on-exec requested but the RM does not support it\n");
        goto done;
    }

    if (!stop_in_init_supported && stop_in_init) {
        fprintf(stderr, "Error: Stop-in-init requested but the RM does not support it\n");
        goto done;
    }

    if (!cospawn_supported && cospawn_reqd) {
        fprintf(stderr, "Error: Cospawn requested but the RM does not support it\n");
        goto done;
    }

    /* If cospawn is available and they requested it, then we launch both
     * the app and the debugger daemons at the same time */
    if (cospawn_supported && cospawn_reqd) {
        cospawn_launch(&myrel);
    } else {
        /* We must do these as separate launches, so do the app first */
        napps = 1;
        PMIX_APP_CREATE(app, napps);
        /* Setup the executable */
        app[0].cmd = strdup("hello");
        PMIX_ARGV_APPEND(rc, app[0].argv, "./hello");
        getcwd(cwd, _POSIX_PATH_MAX); // point us to our current directory
        app[0].cwd = strdup(cwd);
        if (app_np > 0) {
            app[0].maxprocs = app_np;
        }
        app[0].ninfo = 0;
        /* Provide job-level directives so the apps do what the user requested */
        PMIX_INFO_LIST_START(dirs);
        if (stop_on_exec) {
            // procs are to stop on first instruction
            PMIX_INFO_LIST_ADD(rc, dirs, PMIX_DEBUG_STOP_ON_EXEC, NULL, PMIX_BOOL);
        } else {
            // procs are to pause in PMIx_Init for debugger attach
            PMIX_INFO_LIST_ADD(rc, dirs, PMIX_DEBUG_STOP_IN_INIT, NULL, PMIX_BOOL);
        }
        sprintf(map_str, "ppr:%d:node", app_npernode);
        PMIX_INFO_LIST_ADD(rc, dirs, PMIX_MAPBY, map_str, PMIX_STRING); // 1 per node
        PMIX_INFO_LIST_ADD(rc, dirs, PMIX_FWD_STDOUT, NULL, PMIX_BOOL); // forward stdout to me
        PMIX_INFO_LIST_ADD(rc, dirs, PMIX_FWD_STDERR, NULL, PMIX_BOOL); // forward stderr to me
        PMIX_INFO_LIST_ADD(rc, dirs, PMIX_NOTIFY_COMPLETION, NULL,
                       PMIX_BOOL); // notify us when the job completes
        PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
        PMIX_INFO_LIST_RELEASE(dirs);
        info = darray.array;
        ninfo = darray.size;

        /* Spawn the job - the function will return when the app
         * has been launched */
        printf("Debugger: spawning %s\n", app[0].cmd);
        if (PMIX_SUCCESS != (rc = PMIx_Spawn(info, ninfo, app, napps, client_nspace))) {
            fprintf(stderr, "Application failed to launch with error: %s(%d)\n",
                    PMIx_Error_string(rc), rc);
            goto done;
        }
        PMIX_DATA_ARRAY_DESTRUCT(&darray);
        PMIX_APP_FREE(app, napps);

        /* Only call me back when this specific job terminates */
        PMIX_LOAD_PROCID(&proc, client_nspace, PMIX_RANK_WILDCARD);
        /* Register callback for when the app terminates */
        PMIX_INFO_LIST_START(dirs);
        PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_RETURN_OBJECT, &myrel, PMIX_POINTER);
        PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_AFFECTED_PROC, &proc, PMIX_PROC);
        PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
        PMIX_INFO_LIST_RELEASE(dirs);
        info = darray.array;
        ninfo = darray.size;
        /* track number of jobs to terminate */
        myrel.lock.count++;

        DEBUG_CONSTRUCT_LOCK(&mylock);
        PMIx_Register_event_handler(&code, 1, info, ninfo, release_fn, evhandler_reg_callbk,
                                    (void *) &mylock);
        DEBUG_WAIT_THREAD(&mylock);
        printf("Debugger: Registered for termination on nspace %s\n", client_nspace);
        rc = mylock.status;
        DEBUG_DESTRUCT_LOCK(&mylock);
        PMIX_DATA_ARRAY_DESTRUCT(&darray);
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "Registration for PMIX_EVENT_JOB_END failed: %s\n",
                    PMIx_Error_string(rc));
            goto done;
        }

        rc = wait_for_ready(&myrel);
        if (0 != rc) {
            goto done;
        }
        /* Get the proctable for this nspace */
        PMIX_QUERY_CREATE(query, 1);
        PMIX_ARGV_APPEND(rc, query[0].keys, PMIX_QUERY_PROC_TABLE);
        query[0].nqual = 1;
        PMIX_INFO_CREATE(query->qualifiers, query[0].nqual);
        PMIX_INFO_LOAD(&query->qualifiers[0], PMIX_NSPACE, client_nspace, PMIX_STRING);

        DEBUG_CONSTRUCT_LOCK(&myquery_data.lock);
        myquery_data.info = NULL;
        myquery_data.ninfo = 0;

        if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(query, 1, cbfunc, (void *) &myquery_data))) {
            fprintf(stderr, "Debugger[%s:%d] Proctable query failed: %d\n", myproc.nspace,
                    myproc.rank, rc);
            goto done;
        }
        /* Wait to get a response */
        DEBUG_WAIT_THREAD(&myquery_data.lock);
        DEBUG_DESTRUCT_LOCK(&myquery_data.lock);
        /* we should have gotten a response */
        if (PMIX_SUCCESS != myquery_data.status) {
            fprintf(stderr, "Debugger[%s:%d] Proctable query failed: %s\n", myproc.nspace,
                    myproc.rank, PMIx_Error_string(myquery_data.status));
            goto done;
        }
        /* There should have been data */
        if (NULL == myquery_data.info || 0 == myquery_data.ninfo) {
            fprintf(stderr, "Debugger[%s:%d] Proctable query return no results\n", myproc.nspace,
                    myproc.rank);
            goto done;
        }
        /* the query should have returned a data_array */
        if (PMIX_DATA_ARRAY != myquery_data.info[0].value.type) {
            fprintf(stderr, "Debugger[%s:%d] Query returned incorrect data type: %s(%d)\n",
                    myproc.nspace, myproc.rank,
                    PMIx_Data_type_string(myquery_data.info[0].value.type),
                    (int) myquery_data.info[0].value.type);
            return -1;
        }
        if (NULL == myquery_data.info[0].value.data.darray->array) {
            fprintf(stderr, "Debugger[%s:%d] Query returned no proctable info\n", myproc.nspace,
                    myproc.rank);
            goto done;
        }
        /* The data array consists of a struct:
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
        printf("Received proc table for %d procs\n",
               (int) myquery_data.info[0].value.data.darray->size);
        /* now launch the debugger daemons */
        if (PMIX_SUCCESS != (rc = spawn_debugger(client_nspace, &myrel))) {
            fprintf(stderr, "Debugger daemons failed to spawn: %s\n", PMIx_Error_string(rc));
            goto done;
        }
    }

    /* This is where a debugger tool would wait until the debug operation is complete */
    DEBUG_WAIT_THREAD(&myrel.lock);

done:
    DEBUG_DESTRUCT_LOCK(&myrel.lock);
    PMIx_tool_finalize();
    return (rc);
}
