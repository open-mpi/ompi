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
 * Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
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
#include <getopt.h>
#include <libgen.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <limits.h>

#include "debugger.h"
#include <pmix_tool.h>

static pmix_proc_t myproc;
static volatile bool ilactive = true;
static volatile bool dbactive = true;
static volatile bool regpending = true;
static volatile char *appnspace = NULL;
static pmix_nspace_t clientspace;
static int daemon_colocate_per_proc = 0;
static int daemon_colocate_per_node = 0;
static int num_nodes = 1;
static char *hostfile = NULL;


/* this is the event notification function we pass down below
 * when registering for general events - i.e.,, the default
 * handler. We don't technically need to register one, but it
 * is usually good practice to catch any events that occur */
static void notification_fn(size_t evhdlr_registration_id, pmix_status_t status,
                            const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    printf("Default event handler called with status %s\n", PMIx_Error_string(status));

    /* this example doesn't do anything with default events */
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
    ilactive = false;
    printf("\tComplete\n");
}

/* this is the event notification function we pass down below
 * when registering for LOST_CONNECTION, thereby indicating
 * that the intermediate launcher we started has terminated */
static void terminate_fn(size_t evhdlr_registration_id, pmix_status_t status,
                         const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                         pmix_info_t results[], size_t nresults,
                         pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    printf("%s called with status %s\n", __FUNCTION__, PMIx_Error_string(status));
    /* this example doesn't do anything further */
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
    ilactive = false;
}
static void dbgr_complete_fn(size_t evhdlr_registration_id, pmix_status_t status,
                         const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                         pmix_info_t results[], size_t nresults,
                         pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    printf("%s called with status %s\n", __FUNCTION__, PMIx_Error_string(status));
    /* this example doesn't do anything further */
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
    ilactive = false;
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

    printf("%s called with status %s\n", __FUNCTION__, PMIx_Error_string(status));
    if (PMIX_SUCCESS != status) {
        fprintf(stderr, "Client %s:%d event handler registration failed with status %d, ref=%lu\n",
                myproc.nspace, myproc.rank, status, (unsigned long) evhandler_ref);
    }
    lock->status = status;
    regpending = false;
    DEBUG_WAKEUP_THREAD(lock);
}

static void spawn_cbfunc(size_t evhdlr_registration_id, pmix_status_t status,
                         const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                         pmix_info_t results[], size_t nresults,
                         pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    size_t n;

    for (n = 0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_NSPACE)) {
            appnspace = strdup(info[n].value.data.string);
            printf("Got READY-FOR-DEBUG event from nspace %s@%d\n", source->nspace,
                   source->rank);
            break;
        }
    }
    printf("Debugger daemon job: %s\n", appnspace);
    dbactive = false;

    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
}

#define DBGR_LOOP_LIMIT 10

int parse_tool_options(int argc, char **argv)
{
    char *endp;
    int i = 1;

    while ((i < (argc - 1)) && (strncmp(argv[i], "--", 2) == 0)) {
        if (0 == strcmp(argv[i], "--daemon-colocate-per-proc")) {
            daemon_colocate_per_proc = strtol(argv[i + 1], &endp, 10);
            if ('\0' != *endp) {
                fprintf(stderr, "Invalid tool option parameter %s\n", argv[i + 1]);
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--daemon-colocate-per-node")) {
            daemon_colocate_per_node = strtol(argv[i + 1], &endp, 10);
            if ('\0' != *endp) {
                fprintf(stderr, "Invalid tool option parameter %s\n", argv[i + 1]);
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--num-nodes")) {
            num_nodes = strtol(argv[i + 1], &endp, 10);
            if ('\0' != *endp) {
                fprintf(stderr, "Invalid num-nodes value %s\n", argv[i + 1]);
                return -1;
            }
        } else if (0 == strcmp(argv[i], "--hostfile")) {
            hostfile = strdup(argv[i + 1]);
        }
        else {
            fprintf(stderr, "Invalid tool option %s\n", argv[i]);
            return -1;
        }
        i = i + 2;
    }
    if ((0 < daemon_colocate_per_node) && (0 < daemon_colocate_per_proc)) {
        fprintf(stderr, "Cannot specify daemon tasks per node and daemon tasks per proc\n");
        return -1;
    }
    if ((NULL != hostfile) &&
                 ((0 != daemon_colocate_per_node) || (0 != daemon_colocate_per_proc))) {
        fprintf(stderr,
                "hostfile and daemons per node or daemons per proc cannot be combined\n");
        return -1;
    }
    return i;
}

static pmix_status_t spawn_daemons(char **dbgrs)
{
    void *dirs;
    pmix_info_t *info;
    size_t ninfo;
    pmix_status_t rc;
    pmix_app_t app;
    pmix_proc_t target_proc;
    pmix_data_array_t darray;
    pmix_nspace_t dbnspace;
    char cwd[_POSIX_PATH_MAX];

    PMIX_APP_CONSTRUCT(&app);
    app.cmd = strdup("./daemon");
    PMIX_ARGV_APPEND(rc, app.argv, "./daemon");
    getcwd(cwd, _POSIX_PATH_MAX - 1); // point us to our current directory
    app.cwd = strdup(cwd);
    if ((0 < daemon_colocate_per_node) || (0 < daemon_colocate_per_proc)) {
        app.maxprocs = 0;
    }
    else {
        app.maxprocs = 1;
    }
    PMIX_LOAD_PROCID(&target_proc, (void *) appnspace, PMIX_RANK_WILDCARD);
    /* provide directives so the daemons go where we want, and
     * let the RM know these are debugger daemons */
    PMIX_INFO_LIST_START(dirs);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_DEBUGGER_DAEMONS, NULL, PMIX_BOOL); // these are debugger daemons
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_DEBUG_TARGET, &target_proc, PMIX_PROC); // the nspace being debugged
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_NOTIFY_COMPLETION, NULL, PMIX_BOOL); // notify us when the debugger job completes
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_FWD_STDOUT, NULL, PMIX_BOOL); // forward stdout to me
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_FWD_STDERR, NULL, PMIX_BOOL); // forward stderr to me
    if (0 < daemon_colocate_per_proc) {
        PMIX_INFO_LIST_ADD(rc, dirs, PMIX_DEBUG_DAEMONS_PER_PROC, &daemon_colocate_per_proc, PMIX_UINT16);
    }
    else if (0 < daemon_colocate_per_node) {
        PMIX_INFO_LIST_ADD(rc, dirs, PMIX_DEBUG_DAEMONS_PER_NODE, &daemon_colocate_per_node, PMIX_UINT16);
    }
    else {
        // instruct the RM to launch one copy of the daemon on each node
        PMIX_INFO_LIST_ADD(rc, dirs, PMIX_MAPBY, "ppr:1:node:oversubscribe", PMIX_STRING);
        if (NULL != hostfile) {
            app.maxprocs = num_nodes;
            PMIX_INFO_LIST_ADD(rc, dirs, PMIX_HOSTFILE, hostfile, PMIX_STRING);
        }
    }
    PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
    PMIX_INFO_LIST_RELEASE(dirs);
    info = darray.array;
    ninfo = darray.size;

    /* spawn the daemons */
    printf("Debugger: spawning %s\n", app.cmd);
    rc = PMIx_Spawn(info, ninfo, &app, 1, dbnspace);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Debugger daemons failed to launch with error: %s\n",
                PMIx_Error_string(rc));
    }
    *dbgrs = strdup(dbnspace);
    return rc;
}

static pmix_status_t spawn_app(char *myuri, int argc, char **argv,
                               pmix_nspace_t clientnspace)
{
    void *jinfo;
    void *linfo;
    pmix_info_t *info;
    size_t ninfo;
    int n;
    pmix_status_t rc;
    pmix_rank_t rank;
    pmix_app_t app;
    pmix_data_array_t darray;
    char cwd[_POSIX_PATH_MAX];

    /* we are using an intermediate launcher - we will either use the
     * reference server to start it or will fork/exec it ourselves,
     * but either way tell it to wait after launch for directives */
    PMIX_APP_CONSTRUCT(&app);
    /* setup the executable */
    app.cmd = strdup(argv[0]);
    PMIX_ARGV_APPEND(rc, app.argv, argv[0]);
    /* pass it the rest of the cmd line as we don't know
     * how to parse it */
    for (n = 1; n < argc; n++) {
        PMIX_ARGV_APPEND(rc, app.argv, argv[n]);
    }
    getcwd(cwd, _POSIX_PATH_MAX - 1); // point us to our current directory
    app.cwd = strdup(cwd);
    app.maxprocs = 1; // only start one instance of the IL

    /* tell the IL how to connect back to us */
    PMIX_SETENV(rc, PMIX_LAUNCHER_RNDZ_URI, myuri, &app.env);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Failed to set URI in app environment: %s\n", PMIx_Error_string(rc));
        PMIx_tool_finalize();
        return rc;
    }

    /* provide launch directives so the launcher does what we want
     * when it spawns the actual job */
    PMIX_INFO_LIST_START(jinfo);

    /* create the launch directives to tell the launcher what
     * to do with the app it is going to spawn for us */
    PMIX_INFO_LIST_START(linfo);
    rank = PMIX_RANK_WILDCARD;
    PMIX_INFO_LIST_ADD(rc, linfo, PMIX_DEBUG_STOP_IN_INIT, NULL, PMIX_BOOL);  // stop all procs in PMIx_Init
    PMIX_INFO_LIST_ADD(rc, linfo, PMIX_NOTIFY_JOB_EVENTS, NULL, PMIX_BOOL);
    PMIX_INFO_LIST_ADD(rc, linfo, PMIX_FWD_STDOUT, NULL, PMIX_BOOL); // forward stdout to me
    PMIX_INFO_LIST_ADD(rc, linfo, PMIX_FWD_STDERR, NULL, PMIX_BOOL); // forward stderr to me
    PMIX_INFO_LIST_CONVERT(rc, linfo, &darray);
    PMIX_INFO_LIST_ADD(rc, jinfo, PMIX_LAUNCH_DIRECTIVES, &darray, PMIX_DATA_ARRAY);
    PMIX_INFO_LIST_RELEASE(linfo);

    /* convert job info to array */
    PMIX_INFO_LIST_CONVERT(rc, jinfo, &darray);
    PMIX_INFO_LIST_RELEASE(jinfo);
    info = (pmix_info_t *) darray.array;
    ninfo = darray.size;

    /* spawn the launcher - the function will return when the launcher
     * has been started. */
    printf("Spawning launcher\n");
    rc = PMIx_Spawn(info, ninfo, &app, 1, clientnspace);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Launcher %s failed to start with error: %s(%d)\n", argv[1],
                PMIx_Error_string(rc), rc);
    }
    printf("Launcher namespace is %s\n", clientnspace);
    return rc;
}

int main(int argc, char **argv)
{
    pmix_info_t *info;
    pmix_value_t *val;
    char *myuri = NULL;
    void *dirs;
    char *requested_launcher;
    char *launchers[] = {"prun", "mpirun", "mpiexec", "prterun", NULL};
    size_t ninfo;
    pmix_status_t rc;
    int i, launcher_idx, icount;
    size_t n;
    pmix_status_t code;
    bool found;
    pid_t pid;
    mylock_t mylock;
    pmix_proc_t proc;
    pmix_data_array_t darray;
    char *dbgrs;

    /* need to provide args */
    if (2 > argc) {
        printf("Usage: %s [OPTIONS] [launcher] [app]\n", argv[0]);
        printf("OPTIONS:\n");
        printf(" --daemon-colocate-per-proc  Test Colaunch with Daemons Per Process (Default: "
               "0 = off)\n");
        printf(" --daemon-colocate-per-node  Test Colaunch with Daemons Per Node (Default: 0 = "
               "off)\n");
        printf(" --hostfile                  Hostfile specifying where daemons will be loaded\n");
        printf(" --num-nodes                 Number of nodes to use in non-colaunch mode\n");
        exit(0);
    }
    launcher_idx = parse_tool_options(argc, argv);
    if (0 > launcher_idx) {
        exit(1);
    }
    /* check to see if we are using an intermediate launcher - we only
     * support those we recognize */
    found = false;
    requested_launcher = basename(argv[launcher_idx]);
    for (n = 0; NULL != launchers[n]; n++) {
        if (0 == strcmp(requested_launcher, launchers[n])) {
            found = true;
        }
    }
    if (!found) {
        fprintf(stderr, "Wrong test, dude\n");
        exit(1);
    }

    pid = getpid();

    /* do not connect to anyone */
    PMIX_INFO_LIST_START(dirs);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_TOOL_DO_NOT_CONNECT, NULL, PMIX_BOOL);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_LAUNCHER, NULL, PMIX_BOOL);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_IOF_LOCAL_OUTPUT, NULL, PMIX_BOOL);
    PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
    PMIX_INFO_LIST_RELEASE(dirs);
    info = (pmix_info_t *) darray.array;
    ninfo = darray.size;

    /* init as a tool */
    if (PMIX_SUCCESS != (rc = PMIx_tool_init(&myproc, info, ninfo))) {
        fprintf(stderr, "PMIx_tool_init failed: %s(%d)\n", PMIx_Error_string(rc), rc);
        exit(rc);
    }
    PMIX_DATA_ARRAY_DESTRUCT(&darray);

    printf("Debugger ns %s rank %d pid %lu: Running\n", myproc.nspace, myproc.rank,
           (unsigned long) pid);

    /* get server URI as we will need it later */
#ifdef PMIX_MYSERVER_URI
    rc = PMIx_Get(&myproc, PMIX_MYSERVER_URI, NULL, 0, &val);
#else
    rc = PMIx_Get(&myproc, PMIX_SERVER_URI, NULL, 0, &val);
#endif
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Failed to retrieve server URI: %s\n", PMIx_Error_string(rc));
        PMIx_tool_finalize();
        exit(rc);
    }
    myuri = strdup(val->data.string);
    PMIX_VALUE_RELEASE(val);
    printf("Debugger URI: %s\n", myuri);

    /* register an event handler to pickup when the IL
     * we spawned dies */
    DEBUG_CONSTRUCT_LOCK(&mylock);
    code = PMIX_ERR_LOST_CONNECTION;
    PMIX_INFO_CREATE(info, 1);
    PMIX_INFO_LOAD(&info[0], PMIX_EVENT_HDLR_NAME, "LOST-CONNECTION", PMIX_STRING);
    PMIx_Register_event_handler(&code, 1, info, 1, terminate_fn, evhandler_reg_callbk,
                                (void *) &mylock);
    DEBUG_WAIT_THREAD(&mylock);
    DEBUG_DESTRUCT_LOCK(&mylock);
    PMIX_INFO_FREE(info, 1);

    /* register a default event handler */
    DEBUG_CONSTRUCT_LOCK(&mylock);
    PMIX_INFO_CREATE(info, 1);
    PMIX_INFO_LOAD(&info[0], PMIX_EVENT_HDLR_NAME, "DEFAULT", PMIX_STRING);
    PMIx_Register_event_handler(NULL, 0, info, 1, notification_fn, evhandler_reg_callbk,
                                (void *) &mylock);
    DEBUG_WAIT_THREAD(&mylock);
    DEBUG_DESTRUCT_LOCK(&mylock);
    PMIX_INFO_FREE(info, 1);

    rc = spawn_app(myuri, argc - launcher_idx, &argv[launcher_idx], clientspace);
    if (PMIX_SUCCESS != rc) {
        goto done;
    }
    printf("Reconnect to IL at %s\n", clientspace);
    /* set the spawned launcher as our primary server - wait for
     * it to connect to us but provide a timeout so we don't hang
     * waiting forever. The launcher shall connect to us prior
     * to spawning the job we provided it */
    PMIX_LOAD_PROCID(&proc, clientspace, 0);
    i = 2;
    PMIX_INFO_LIST_START(dirs);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_WAIT_FOR_CONNECTION, NULL, PMIX_BOOL);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_TIMEOUT, &i, PMIX_INT);
    PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
    PMIX_INFO_LIST_RELEASE(dirs);
    info = (pmix_info_t*)darray.array;
    ninfo = darray.size;
    rc = PMIx_tool_set_server(&proc, info, ninfo);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);
    if (PMIX_SUCCESS != rc) {
        /* connection failed */
        fprintf(stderr, "Failed to set spawned launcher as primary server: %s\n",
                PMIx_Error_string(rc));
        goto done;
    }

    /* register to receive the ready-for-debug event telling us the
     * nspace of the child job and alerting us that things are ready
     * for us to spawn the debugger daemons - this will be registered
     * with the IL we started */
    printf("Registering READY-FOR-DEBUG handler\n");
    DEBUG_CONSTRUCT_LOCK(&mylock);
    code = PMIX_READY_FOR_DEBUG;
    PMIX_INFO_LIST_START(dirs);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_HDLR_NAME, "READY-FOR-DEBUG", PMIX_STRING);
    PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
    PMIX_INFO_LIST_RELEASE(dirs);
    info = (pmix_info_t*)darray.array;
    ninfo = darray.size;
    PMIx_Register_event_handler(&code, 1, info, ninfo, spawn_cbfunc, evhandler_reg_callbk,
                                (void *) &mylock);
    DEBUG_WAIT_THREAD(&mylock);
    DEBUG_DESTRUCT_LOCK(&mylock);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);
    if (!ilactive) {
        fprintf(stderr, "Error: Launcher not active\n");
        goto done;
    }

    /* release the IL to spawn its job */
    printf("Releasing %s [%s,%d]\n", argv[launcher_idx], proc.nspace, proc.rank);
    PMIX_INFO_LIST_START(dirs);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_NON_DEFAULT, NULL, PMIX_BOOL);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_CUSTOM_RANGE, &proc, PMIX_PROC);
    PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
    PMIX_INFO_LIST_RELEASE(dirs);
    info = (pmix_info_t*)darray.array;
    ninfo = darray.size;
    PMIx_Notify_event(PMIX_DEBUGGER_RELEASE, &myproc, PMIX_RANGE_CUSTOM, info, ninfo, NULL, NULL);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);

    printf("Waiting for application launch\n");
    /* wait for the IL to have launched its application */
    icount = 0;
    while (dbactive && ilactive) {
        struct timespec tp = {0, 500000000};
        nanosleep(&tp, NULL);
        ++icount;
        if (icount > 10) {
            fprintf(stderr, "Error: Failed to launch by the timeout\n");
            goto done;
        }
    }
    if ((!ilactive) || (NULL == appnspace)) {
        /* the launcher failed */
        fprintf(stderr, "Error: Launcher failed\n");
        goto done;
    }

    printf("Application has launched: %s\n", (char *) appnspace);

    /* setup the debugger */
    dbgrs = NULL;
    rc = spawn_daemons(&dbgrs);
    printf("Debugger nspace: %s\n", dbgrs);

    /* wait for the debuggers to terminate */
    printf("Registering handler for debugger termination\n");
    DEBUG_CONSTRUCT_LOCK(&mylock);
    code = PMIX_EVENT_JOB_END;
    PMIX_INFO_LIST_START(dirs);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_HDLR_NAME, "DEBUGGGER-COMPLETE", PMIX_STRING);
    PMIX_LOAD_PROCID(&proc, dbgrs, PMIX_RANK_WILDCARD);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_AFFECTED_PROC, &proc, PMIX_PROC);
    PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
    PMIX_INFO_LIST_RELEASE(dirs);
    info = (pmix_info_t*)darray.array;
    ninfo = darray.size;
    PMIx_Register_event_handler(&code, 1, info, ninfo, dbgr_complete_fn, evhandler_reg_callbk,
                                (void *) &mylock);
    DEBUG_WAIT_THREAD(&mylock);
    DEBUG_DESTRUCT_LOCK(&mylock);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);

    /* wait for the IL to terminate */
    printf("Waiting for IL to terminate\n");
    while (ilactive) {
        struct timespec tp = {0, 500000};
        nanosleep(&tp, NULL);
    }

done:
    PMIx_tool_finalize();

    if (NULL != myuri) {
        free(myuri);
    }

    return (rc);
}
