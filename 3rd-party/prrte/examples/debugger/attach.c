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
 * Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * Copyright (c) 2021      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#define _GNU_SOURCE
#include <limits.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "debugger.h"
#include <pmix_tool.h>

static int attach_to_running_job(char *nspace);
static int query_application_namespace(char *nspace);

static pmix_proc_t myproc;
static char application_namespace[PMIX_MAX_NSLEN + 1];
static char *iof_data;
static int iof_size;
static int iof_registered;
static size_t iof_handler_id;
static int daemon_colocate_per_proc = 0;
static int daemon_colocate_per_node = 0;
static char *hostfile = NULL;



/* This is the event notification function we pass down below
 * when registering for general events - i.e.,, the default
 * handler. We don't technically need to register one, but it
 * is usually good practice to catch any events that occur */
static void notification_fn(size_t evhdlr_registration_id, pmix_status_t status,
                            const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    printf("%s called as callback for event=%s\n", __FUNCTION__, PMIx_Error_string(status));
    /* This example doesn't do anything with default events */
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
    }
}

/* This is the handler function to capture stdio data from the daemon process.
 * It accumulates stdio data in a buffer. That buffer is displayed at the end
 * of this program's execution, instead of as it is received, so the output
 * does not get randomly interspersed with other output. */
static void stdio_callback(size_t iofhdlr, pmix_iof_channel_t channel, pmix_proc_t *source,
                           pmix_byte_object_t *payload, pmix_info_t info[], size_t ninfo)
{
    if (NULL == iof_data) {
        /* Allocate initial string plus trailing '\0' that is not in
         * payload->bytes then copy data and append '\0' */
        iof_size = payload->size;
        /* iof_size counts number of bytes sent, need one more for trailing
         * '\0' */
        iof_data = malloc(iof_size + 1);
        if (NULL == iof_data) {
            fprintf(stderr, "Unable to allocate I/O buffer, terminating\n");
            exit(1);
        }
        memcpy(iof_data, payload->bytes, payload->size);
        iof_data[payload->size] = '\0';
    } else {
        /* Reallocate buffer to hold additional data, copy data and append
         * '\0' at end of buffer. */
        iof_data = realloc(iof_data, iof_size + payload->size + 1);
        if (NULL == iof_data) {
            fprintf(stderr, "Unable to allocate I/O buffer, terminating\n");
            exit(1);
        }
        memcpy(&iof_data[iof_size], payload->bytes, payload->size);
        iof_size = iof_size + payload->size;
        iof_data[iof_size] = '\0';
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

    printf("%s called as callback for event=%s\n", __FUNCTION__, PMIx_Error_string(status));
    /* Find our return object */
    lock = NULL;
    found = false;
    for (n = 0; n < ninfo; n++) {
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
    /* If the lock object wasn't returned, then that is an error */
    if (NULL == lock) {
        fprintf(stderr, "LOCK WASN'T RETURNED IN RELEASE CALLBACK\n");
        /* Let the event handler progress */
        if (NULL != cbfunc) {
            cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
        }
        return;
    }

    printf("DEBUGGER NOTIFIED THAT JOB %s TERMINATED - AFFECTED %s\n", lock->nspace,
           (NULL == affected) ? "NULL" : affected->nspace);
    if (found) {
        lock->exit_code = exit_code;
        lock->exit_code_given = true;
    }
    DEBUG_WAKEUP_THREAD(&lock->lock);

    /* Tell the event handler state machine that we are the last step */
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }

    DEBUG_WAKEUP_THREAD(&lock->lock);
    return;
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

    printf("%s called to register callback refid=%ld\n", __FUNCTION__, evhandler_ref);
    if (PMIX_SUCCESS != status) {
        fprintf(stderr, "Client %s:%d EVENT HANDLER REGISTRATION FAILED WITH STATUS %d, ref=%lu\n",
                myproc.nspace, myproc.rank, status, (unsigned long) evhandler_ref);
    }
    if (NULL == lock) {
        return;
    }
    lock->status = status;
    DEBUG_WAKEUP_THREAD(lock);
}

/* Registration callback for IOF handler. This funcion gets called both when
 * the IOF handler is registered, and when it gets de-registered.
 */
static void iof_reg_callbk(pmix_status_t status, size_t evhandler_ref, void *cbdata)
{
    mylock_t *lock = (mylock_t *) cbdata;

    printf("%s called to register IOF handler refid=%ld\n", __FUNCTION__,
           evhandler_ref);
    if (PMIX_SUCCESS != status) {
        fprintf(stderr, "Client %s:%d EVENT HANDLER REGISTRATION FAILED WITH STATUS %d, ref=%lu\n",
                myproc.nspace, myproc.rank, status, (unsigned long) evhandler_ref);
    }
    iof_handler_id = evhandler_ref;
    if (SIZE_MAX == iof_handler_id) {
        fprintf(stderr, "REGISTRATION RETURNED ERROR HANDLER ID");
    }
    if (NULL == lock) {
        return;
    }
    /* Only post the lock when handler is being registered */
    if (iof_registered) {
        printf("IOF registration handler called for de-registration\n");
        return;
    }
    iof_registered = 1;
    lock->status = status;
    DEBUG_WAKEUP_THREAD(lock);
}

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

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pmix_info_t *info;
    void *attr_list;
    size_t ninfo;
    char *nspace = NULL;
    mylock_t mylock;
    pid_t pid;
    int ns_index;
    pmix_data_array_t attr_array;

    pid = getpid();

    ns_index = parse_tool_options(argc, argv);
    if (0 > ns_index) {
        exit(1);
    } else if (ns_index >= argc) {
        printf("Usage: %s [OPTIONS] app-launcher-namespace\n", argv[0]);
        printf("OPTIONS:\n");
        printf(" --daemon-colocate-per-proc  Test Colaunch with Daemons Per Process (Default: "
               "0 = off)\n");
        printf(" --daemon-colocate-per-node  Test Colaunch with Daemons Per Node (Default: 0 = "
               "off)\n");
        printf(" --hostfile                  Hostfile specifying where daemons will be loaded\n");
        exit(0);
    }
    nspace = strdup(argv[ns_index]);
    info = NULL;
    ninfo = 1;

    /* Initialize as a tool */
    PMIX_INFO_LIST_START(attr_list);
    PMIX_INFO_LIST_ADD(rc, attr_list, PMIX_LAUNCHER, NULL, PMIX_BOOL);
    PMIX_INFO_LIST_ADD(rc, attr_list, PMIX_IOF_LOCAL_OUTPUT, NULL, PMIX_BOOL);
    PMIX_INFO_LIST_CONVERT(rc, attr_list, &attr_array);
    PMIX_INFO_LIST_RELEASE(attr_list);
    info = attr_array.array;
    ninfo = attr_array.size;
    if (PMIX_SUCCESS != (rc = PMIx_tool_init(&myproc, info, ninfo))) {
        fprintf(stderr, "PMIx_tool_init failed: %s(%d)\n", PMIx_Error_string(rc), rc);
        exit(rc);
    }
    PMIX_DATA_ARRAY_DESTRUCT(&attr_array);

    printf("Debugger ns %s rank %d pid %lu: Running\n", myproc.nspace, myproc.rank,
           (unsigned long) pid);

    /* Register a default event handler */
    DEBUG_CONSTRUCT_LOCK(&mylock);
    PMIx_Register_event_handler(NULL, 0, NULL, 0, notification_fn, evhandler_reg_callbk,
                                (void *) &mylock);
    DEBUG_WAIT_THREAD(&mylock);
    DEBUG_DESTRUCT_LOCK(&mylock);
    if (PMIX_SUCCESS != (rc = attach_to_running_job(nspace))) {
        fprintf(stderr, "Failed to attach to nspace %s: error code %d\n", nspace, rc);
    }
    PMIx_tool_finalize();
    if (NULL != iof_data) {
        printf("Forwarded stdio data:\n%s", iof_data);
        printf("End forwarded stdio\n");
    }
    return (rc);
}

static int attach_to_running_job(char *nspace)
{
    void *dirs;
    pmix_status_t rc;
    pmix_proc_t daemon_proc, target_proc;
    pmix_info_t *info;
    pmix_app_t *app;
    pmix_status_t code = PMIX_ERR_LOST_CONNECTION;
    size_t ninfo;
    int n;
    mylock_t mylock, iof_lock;
    myrel_t myrel;
    char cwd[_POSIX_PATH_MAX];
    char dspace[PMIX_MAX_NSLEN + 1];
    pmix_data_array_t darray;

    printf("%s called to attach to application with namespace=%s\n", __FUNCTION__, nspace);
    /* This is where a debugger tool would process the proctable to
     * create whatever blob it needs to provide to its daemons */

    /* We are given the namespace of the launcher. The debugger daemon needs
     * the namespace of the application so it can interact with and control
     * execution of the application tasks.
     *
     * Query the namespaces known to the launcher to get the application
     * namespace. */
    query_application_namespace(nspace);
    printf("Spawn debugger daemon\n");
    /* Set up the debugger daemon spawn request */
    PMIX_APP_CREATE(app, 1);
    /* Set up the name of the daemon executable to launch */
    app->cmd = strdup("./daemon");
    app->argv = (char **) malloc(2 * sizeof(char *));
    /* Set up the debuger daemon arguments, in this case, just argv[0] */
    app->argv[0] = strdup("./daemon");
    app->argv[1] = NULL;
    /* No environment variables */
    app->env = NULL;
    /* Set the daemon's working directory to our current directory */
    getcwd(cwd, _POSIX_PATH_MAX);
    app->cwd = strdup(cwd);
    /* No attributes set in the pmix_app_t structure */
    app->info = NULL;
    app->ninfo = 0;
    if ((0 < daemon_colocate_per_node) || (0 < daemon_colocate_per_proc)) {
        app->maxprocs = 0;
    }
    else {
        app->maxprocs = 1;
    }
    /* Provide directives so the daemon goes where we want, and
     * let the RM know this is a debugger daemon */
    PMIX_LOAD_PROCID(&target_proc, application_namespace, PMIX_RANK_WILDCARD);
    PMIX_INFO_LIST_START(dirs);
    /* Indicate the requestor is a tool process */
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_REQUESTOR_IS_TOOL, NULL, PMIX_BOOL);
    /* Indicate this is a debugger daemon */
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_DEBUGGER_DAEMONS, NULL, PMIX_BOOL);
    /* Set the application namespace to attach to */
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_DEBUG_TARGET, &target_proc, PMIX_PROC);
    /* Forward stdout and stderr to this process */
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_FWD_STDOUT, NULL, PMIX_BOOL);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_FWD_STDERR, NULL, PMIX_BOOL);
    /* Set up daemon mapping based on options */
    if (0 < daemon_colocate_per_node) {
        PMIX_INFO_LIST_ADD(rc, dirs, PMIX_DEBUG_DAEMONS_PER_NODE, 
                           &daemon_colocate_per_node, PMIX_UINT16);
    } else if (0 < daemon_colocate_per_proc) {
        PMIX_INFO_LIST_ADD(rc, dirs, PMIX_DEBUG_DAEMONS_PER_PROC,
                           &daemon_colocate_per_proc, PMIX_UINT16);
    } else if (NULL != hostfile) {
        PMIX_INFO_LIST_ADD(rc, dirs, PMIX_HOSTFILE, hostfile, PMIX_STRING);
        PMIX_INFO_LIST_ADD(rc, dirs, PMIX_MAPBY, "ppr:1:node", PMIX_STRING);
    } 
    else {
        PMIX_INFO_LIST_ADD(rc, dirs, PMIX_MAPBY, "ppr:1:node", PMIX_STRING);
    }
    PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
    PMIX_INFO_LIST_RELEASE(dirs);
    info = darray.array;
    ninfo = darray.size;
    /* Spawn the daemon */
    rc = PMIx_Spawn(info, ninfo, app, 1, dspace);

    PMIX_APP_FREE(app, 1);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);

    printf("Debugger daemon namespace '%s'\n", dspace);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Error spawning debugger daemon, %s\n", PMIx_Error_string(rc));
        return -1;
    }
    PMIX_PROC_LOAD(&daemon_proc, dspace, PMIX_RANK_WILDCARD);
    DEBUG_CONSTRUCT_LOCK(&iof_lock);
    /* Register a handler to handle daemon's stdout and stderr */
    ninfo = 1;
    n = 0;
    PMIX_INFO_CREATE(info, ninfo);
    PMIX_INFO_LOAD(&info[n], PMIX_IOF_REDIRECT, NULL, PMIX_BOOL);
    rc = PMIx_IOF_pull(&daemon_proc, 1, info, ninfo,
                       PMIX_FWD_STDOUT_CHANNEL | PMIX_FWD_STDERR_CHANNEL, stdio_callback,
                       iof_reg_callbk, &iof_lock);
    PMIX_INFO_FREE(info, ninfo);
    DEBUG_WAIT_THREAD(&iof_lock);
    rc = iof_lock.status;
    /* Don't destroy the iof_lock since evhandler_reg_callback gets called
     * multiple times */
    /* This is where a debugger tool would wait until the debug operation is
     * complete */
    /* Register callback for when the debugger daemon terminates */
    DEBUG_CONSTRUCT_LOCK(&myrel.lock);
    myrel.nspace = strdup(dspace);

    /* Register a handler for PMIX_ERR_LOST_CONNECTION, which will occur when the
     * daemon terminates. */
    PMIX_INFO_LIST_START(dirs);
    PMIX_INFO_LIST_ADD(rc, dirs, PMIX_EVENT_RETURN_OBJECT, &myrel, PMIX_POINTER);
    PMIX_INFO_LIST_CONVERT(rc, dirs, &darray);
    PMIX_INFO_LIST_RELEASE(dirs);
    info = darray.array;
    ninfo = darray.size;
    DEBUG_CONSTRUCT_LOCK(&mylock);
    PMIx_Register_event_handler(&code, 1, info, ninfo, release_fn, evhandler_reg_callbk,
                                (void *) &mylock);
    DEBUG_WAIT_THREAD(&mylock);
    PMIX_DATA_ARRAY_DESTRUCT(&darray);
    rc = mylock.status;
    DEBUG_DESTRUCT_LOCK(&mylock);
    printf("Waiting for debugger daemon namespace %s to complete\n", dspace);
    DEBUG_WAIT_THREAD(&myrel.lock);
    printf("Debugger daemon namespace %s terminated\n", dspace);
    return rc;
}

int query_application_namespace(char *nspace)
{
    pmix_info_t *namespace_query_data;
    char *p;
    size_t namespace_query_size;
    pmix_status_t rc;
    pmix_query_t namespace_query;
    int wildcard_rank = PMIX_RANK_WILDCARD;
    int ninfo;
    int n;
    int len;

    printf("%s called to get application namespace\n", __FUNCTION__);
    PMIX_QUERY_CONSTRUCT(&namespace_query);
    PMIX_ARGV_APPEND(rc, namespace_query.keys, PMIX_QUERY_NAMESPACES);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "An error occurred creating namespace query.");
        PMIX_QUERY_DESTRUCT(&namespace_query);
        return -1;
    }
    PMIX_INFO_CREATE(namespace_query.qualifiers, 2);
    ninfo = 2;
    n = 0;
    PMIX_INFO_LOAD(&namespace_query.qualifiers[n], PMIX_NSPACE, nspace, PMIX_STRING);
    n++;
    PMIX_INFO_LOAD(&namespace_query.qualifiers[n], PMIX_RANK, &wildcard_rank, PMIX_INT32);
    namespace_query.nqual = ninfo;
    rc = PMIx_Query_info(&namespace_query, 1, &namespace_query_data, &namespace_query_size);
    PMIX_QUERY_DESTRUCT(&namespace_query);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "An error occurred querying application namespace: %s.\n",
                PMIx_Error_string(rc));
        return -1;
    }
    if ((1 != namespace_query_size) || (PMIX_STRING != namespace_query_data->value.type)) {
        fprintf(stderr, "The response to namespace query has wrong format.\n");
        return -1;
    }
    /* The query retruns a comma-delimited list of namespaces. If there are
     * multple namespaces in the list, then assume the first is the
     * application namespace and the second is the daemon namespace.
     * Copy only the application namespace and terminate the name with '\0' */
    p = strchr(namespace_query_data->value.data.string, ',');
    if (NULL == p) {
        len = strlen(namespace_query_data->value.data.string);
    } else {
        len = p - namespace_query_data->value.data.string;
    }
    strncpy(application_namespace, namespace_query_data->value.data.string, len);
    application_namespace[len] = '\0';
    printf("Application namespace is '%s'\n", application_namespace);
    return 0;
}
