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
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <src/include/pmix_config.h>
#include <pmix_server.h>
#include <src/include/types.h>
#include <src/include/pmix_globals.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <signal.h>

#if PMIX_HAVE_HWLOC
#include <src/hwloc/hwloc-internal.h>
#endif

#include "src/class/pmix_list.h"
#include "src/util/pmix_environ.h"
#include "src/util/output.h"
#include "src/util/printf.h"
#include "src/util/argv.h"

#include "simptest.h"

static pmix_status_t connected(const pmix_proc_t *proc, void *server_object,
                               pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t finalized(const pmix_proc_t *proc, void *server_object,
                               pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t abort_fn(const pmix_proc_t *proc, void *server_object,
                              int status, const char msg[],
                              pmix_proc_t procs[], size_t nprocs,
                              pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t fencenb_fn(const pmix_proc_t procs[], size_t nprocs,
                                const pmix_info_t info[], size_t ninfo,
                                char *data, size_t ndata,
                                pmix_modex_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t dmodex_fn(const pmix_proc_t *proc,
                               const pmix_info_t info[], size_t ninfo,
                               pmix_modex_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t publish_fn(const pmix_proc_t *proc,
                                const pmix_info_t info[], size_t ninfo,
                                pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t lookup_fn(const pmix_proc_t *proc, char **keys,
                               const pmix_info_t info[], size_t ninfo,
                               pmix_lookup_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t unpublish_fn(const pmix_proc_t *proc, char **keys,
                                  const pmix_info_t info[], size_t ninfo,
                                  pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t spawn_fn(const pmix_proc_t *proc,
                              const pmix_info_t job_info[], size_t ninfo,
                              const pmix_app_t apps[], size_t napps,
                              pmix_spawn_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t connect_fn(const pmix_proc_t procs[], size_t nprocs,
                                const pmix_info_t info[], size_t ninfo,
                                pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t disconnect_fn(const pmix_proc_t procs[], size_t nprocs,
                                   const pmix_info_t info[], size_t ninfo,
                                   pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t register_event_fn(pmix_status_t *codes, size_t ncodes,
                                       const pmix_info_t info[], size_t ninfo,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t deregister_events(pmix_status_t *codes, size_t ncodes,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t notify_event(pmix_status_t code,
                                  const pmix_proc_t *source,
                                  pmix_data_range_t range,
                                  pmix_info_t info[], size_t ninfo,
                                  pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t query_fn(pmix_proc_t *proct,
                              pmix_query_t *queries, size_t nqueries,
                              pmix_info_cbfunc_t cbfunc,
                              void *cbdata);
static void tool_connect_fn(pmix_info_t *info, size_t ninfo,
                            pmix_tool_connection_cbfunc_t cbfunc,
                            void *cbdata);
static void log_fn(const pmix_proc_t *client,
                   const pmix_info_t data[], size_t ndata,
                   const pmix_info_t directives[], size_t ndirs,
                   pmix_op_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t alloc_fn(const pmix_proc_t *client,
                              pmix_alloc_directive_t directive,
                              const pmix_info_t data[], size_t ndata,
                              pmix_info_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t jctrl_fn(const pmix_proc_t *requestor,
                              const pmix_proc_t targets[], size_t ntargets,
                              const pmix_info_t directives[], size_t ndirs,
                              pmix_info_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t mon_fn(const pmix_proc_t *requestor,
                            const pmix_info_t *monitor, pmix_status_t error,
                            const pmix_info_t directives[], size_t ndirs,
                            pmix_info_cbfunc_t cbfunc, void *cbdata);

static pmix_server_module_t mymodule = {
    .client_connected = connected,
    .client_finalized = finalized,
    .abort = abort_fn,
    .fence_nb = fencenb_fn,
    .direct_modex = dmodex_fn,
    .publish = publish_fn,
    .lookup = lookup_fn,
    .unpublish = unpublish_fn,
    .spawn = spawn_fn,
    .connect = connect_fn,
    .disconnect = disconnect_fn,
    .register_events = register_event_fn,
    .deregister_events = deregister_events,
    .notify_event = notify_event,
    .query = query_fn,
    .tool_connected = tool_connect_fn,
    .log = log_fn,
    .allocate = alloc_fn,
    .job_control = jctrl_fn,
    .monitor = mon_fn
};

typedef struct {
    pmix_list_item_t super;
    pmix_pdata_t pdata;
} pmix_locdat_t;
PMIX_CLASS_INSTANCE(pmix_locdat_t,
                    pmix_list_item_t,
                    NULL, NULL);

typedef struct {
    pmix_object_t super;
    mylock_t lock;
    pmix_event_t ev;
    pmix_proc_t caller;
    pmix_info_t *info;
    size_t ninfo;
    pmix_op_cbfunc_t cbfunc;
    pmix_spawn_cbfunc_t spcbfunc;
    pmix_release_cbfunc_t relcbfunc;
    void *cbdata;
} myxfer_t;
static void xfcon(myxfer_t *p)
{
    DEBUG_CONSTRUCT_LOCK(&p->lock);
    p->info = NULL;
    p->ninfo = 0;
    p->cbfunc = NULL;
    p->spcbfunc = NULL;
    p->cbdata = NULL;
}
static void xfdes(myxfer_t *p)
{
    DEBUG_DESTRUCT_LOCK(&p->lock);
    if (NULL != p->info) {
        PMIX_INFO_FREE(p->info, p->ninfo);
    }
}
PMIX_CLASS_INSTANCE(myxfer_t,
                    pmix_object_t,
                    xfcon, xfdes);

typedef struct {
    pmix_list_item_t super;
    int exit_code;
    pid_t pid;
} wait_tracker_t;
PMIX_CLASS_INSTANCE(wait_tracker_t,
                    pmix_list_item_t,
                    NULL, NULL);

static volatile int wakeup;
static int exit_code = 0;
static pmix_list_t pubdata;
static pmix_event_t handler;
static pmix_list_t children;
static bool istimeouttest = false;
static mylock_t globallock;

static void set_namespace(int nprocs, char *ranks, char *nspace,
                          pmix_op_cbfunc_t cbfunc, myxfer_t *x);
static void errhandler(size_t evhdlr_registration_id,
                       pmix_status_t status,
                       const pmix_proc_t *source,
                       pmix_info_t info[], size_t ninfo,
                       pmix_info_t results[], size_t nresults,
                       pmix_event_notification_cbfunc_fn_t cbfunc,
                       void *cbdata);
static void wait_signal_callback(int fd, short event, void *arg);
static void errhandler_reg_callbk (pmix_status_t status,
                                   size_t errhandler_ref,
                                   void *cbdata);

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    myxfer_t *x = (myxfer_t*)cbdata;

    /* release the caller, if necessary */
    if (NULL != x->cbfunc) {
        x->cbfunc(PMIX_SUCCESS, x->cbdata);
    }
    DEBUG_WAKEUP_THREAD(&x->lock);
}


static void dlcbfunc(int sd, short flags, void *cbdata)
{
    myxfer_t *x = (myxfer_t*)cbdata;

    PMIx_server_deliver_inventory(x->info, x->ninfo, NULL, 0, opcbfunc, (void*)x);
}

static void infocbfunc(pmix_status_t status,
                       pmix_info_t *info, size_t ninfo,
                       void *cbdata,
                       pmix_release_cbfunc_t release_fn,
                       void *release_cbdata)
{
    mylock_t *lock = (mylock_t*)cbdata;
    myxfer_t *x;
    size_t n;

    /* we don't have any place to send this, so for test
     * purposes only, let's push it back down for processing.
     * Note: it must be thread-shifted first as we are in
     * the callback event thread of the underlying PMIx
     * server */
    x = PMIX_NEW(myxfer_t);
    x->ninfo = ninfo;
    PMIX_INFO_CREATE(x->info, x->ninfo);
    for (n=0; n < ninfo; n++) {
        PMIX_INFO_XFER(&x->info[n], &info[n]);
    }
    PMIX_THREADSHIFT(x, dlcbfunc);

    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
    lock->status = status;
    DEBUG_WAKEUP_THREAD(lock);
}

/* this is an event notification function that we explicitly request
 * be called when the PMIX_MODEL_DECLARED notification is issued.
 * We could catch it in the general event notification function and test
 * the status to see if the status matched, but it often is simpler
 * to declare a use-specific notification callback point. In this case,
 * we are asking to know whenever a model is declared as a means
 * of testing server self-notification */
static void model_callback(size_t evhdlr_registration_id,
                           pmix_status_t status,
                           const pmix_proc_t *source,
                           pmix_info_t info[], size_t ninfo,
                           pmix_info_t results[], size_t nresults,
                           pmix_event_notification_cbfunc_fn_t cbfunc,
                           void *cbdata)
{
    size_t n;

    /* just let us know it was received */
    fprintf(stderr, "SIMPTEST: Model event handler called with status %d(%s)\n",
            status, PMIx_Error_string(status));
    for (n=0; n < ninfo; n++) {
        if (PMIX_STRING == info[n].value.type) {
            fprintf(stderr, "\t%s:\t%s\n", info[n].key, info[n].value.data.string);
        }
    }

    /* we must NOT tell the event handler state machine that we
     * are the last step as that will prevent it from notifying
     * anyone else that might be listening for declarations */
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
    }
    DEBUG_WAKEUP_THREAD(&globallock);
}

/* event handler registration is done asynchronously */
static void model_registration_callback(pmix_status_t status,
                                        size_t evhandler_ref,
                                        void *cbdata)
{
    mylock_t *lock = (mylock_t*)cbdata;

    if (PMIX_SUCCESS != status) {
        fprintf(stderr, "simptest EVENT HANDLER REGISTRATION FAILED WITH STATUS %d, ref=%lu\n",
                   status, (unsigned long)evhandler_ref);
    }
    lock->status = status;
    DEBUG_WAKEUP_THREAD(lock);
}

static void set_handler_default(int sig)
{
    struct sigaction act;

    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);

    sigaction(sig, &act, (struct sigaction *)0);
}

int main(int argc, char **argv)
{
    char **client_env=NULL;
    char **client_argv=NULL;
    char *tmp, **atmp, *executable=NULL;
    int rc, nprocs=1, n, k;
    uid_t myuid;
    gid_t mygid;
    pid_t pid;
    myxfer_t *x;
    pmix_proc_t proc;
    wait_tracker_t *child;
    pmix_info_t *info;
    size_t ninfo;
    bool cross_version = false;
    bool usock = true;
    bool hwloc = false;
#if PMIX_HAVE_HWLOC
    char *hwloc_file = NULL;
#endif
    mylock_t mylock;
    pmix_status_t code;
    sigset_t unblock;

    /* smoke test */
    if (PMIX_SUCCESS != 0) {
        fprintf(stderr, "ERROR IN COMPUTING CONSTANTS: PMIX_SUCCESS = %d\n", PMIX_SUCCESS);
        exit(1);
    }

    /* see if we were passed the number of procs to run or
     * the executable to use */
    for (n=1; n < argc; n++) {
        if (0 == strcmp("-n", argv[n]) &&
            NULL != argv[n+1]) {
            nprocs = strtol(argv[n+1], NULL, 10);
            ++n;  // step over the argument
        } else if (0 == strcmp("-e", argv[n]) &&
                   NULL != argv[n+1]) {
            executable = strdup(argv[n+1]);
            /* check for timeout test */
            if (NULL != strstr(executable, "simptimeout")) {
                istimeouttest = true;
            }
            for (k=n+2; NULL != argv[k]; k++) {
                pmix_argv_append_nosize(&client_argv, argv[k]);
            }
            n += k;
        } else if (0 == strcmp("-x", argv[n])) {
            /* cross-version test - we will set one child to
             * run at a different version. Requires -n >= 2 */
            cross_version = true;
            usock = false;
        } else if (0 == strcmp("-u", argv[n])) {
            /* enable usock */
            usock = false;
#if PMIX_HAVE_HWLOC
        } else if (0 == strcmp("-hwloc", argv[n]) ||
                   0 == strcmp("--hwloc", argv[n])) {
            /* test hwloc support */
            hwloc = true;
        } else if (0 == strcmp("-hwloc-file", argv[n]) ||
                   0 == strcmp("--hwloc-file", argv[n])) {
            if (NULL == argv[n+1]) {
                fprintf(stderr, "The --hwloc-file option requires an argument\n");
                exit(1);
            }
            hwloc_file = strdup(argv[n+1]);
            hwloc = true;
            ++n;
#endif
        } else if (0 == strcmp("-h", argv[n])) {
            /* print the options and exit */
            fprintf(stderr, "usage: simptest <options>\n");
            fprintf(stderr, "    -n N     Number of clients to run\n");
            fprintf(stderr, "    -e foo   Name of the client executable to run (default: simpclient\n");
            fprintf(stderr, "    -x       Test cross-version support\n");
            fprintf(stderr, "    -u       Enable legacy usock support\n");
            fprintf(stderr, "    -hwloc   Test hwloc support\n");
            fprintf(stderr, "    -hwloc-file FILE   Use file to import topology\n");
            exit(0);
        }
    }
    if (NULL == executable) {
        executable = strdup("./simpclient");
    }
    /* check for executable existence and permissions */
    if (0 != access(executable, X_OK)) {
        fprintf(stderr, "Executable %s not found or missing executable permissions\n", executable);
        exit(1);
    }

    if (cross_version && nprocs < 2) {
        fprintf(stderr, "Cross-version testing requires at least two clients\n");
        exit(1);
    }

#if !PMIX_HAVE_HWLOC
    if (hwloc) {
        fprintf(stderr, "PMIx was not configured with HWLOC support - cannot continue\n");
        exit(1);
    }
#endif

    fprintf(stderr, "Testing version %s\n", PMIx_Get_version());

    /* ensure that SIGCHLD is unblocked as we need to capture it */
    if (0 != sigemptyset(&unblock)) {
        fprintf(stderr, "SIGEMPTYSET FAILED\n");
        exit(1);
    }
    if (0 != sigaddset(&unblock, SIGCHLD)) {
        fprintf(stderr, "SIGADDSET FAILED\n");
        exit(1);
    }
    if (0 != sigprocmask(SIG_UNBLOCK, &unblock, NULL)) {
        fprintf(stderr, "SIG_UNBLOCK FAILED\n");
        exit(1);
    }


    /* setup the server library and tell it to support tool connections */
#if PMIX_HAVE_HWLOC
    if (hwloc) {
#if HWLOC_API_VERSION < 0x20000
        ninfo = 3;
#else
        ninfo = 4;
#endif
    } else {
        ninfo = 2;
    }
#else
    ninfo = 2;
#endif

    PMIX_INFO_CREATE(info, ninfo);
    PMIX_INFO_LOAD(&info[0], PMIX_SERVER_TOOL_SUPPORT, NULL, PMIX_BOOL);
    PMIX_INFO_LOAD(&info[1], PMIX_SERVER_GATEWAY, NULL, PMIX_BOOL);
#if PMIX_HAVE_HWLOC
    if (hwloc) {
        if (NULL != hwloc_file) {
            PMIX_INFO_LOAD(&info[2], PMIX_TOPOLOGY_FILE, hwloc_file, PMIX_STRING);
        } else {
            PMIX_INFO_LOAD(&info[2], PMIX_TOPOLOGY, NULL, PMIX_STRING);
        }
#if HWLOC_API_VERSION >= 0x20000
        PMIX_INFO_LOAD(&info[3], PMIX_HWLOC_SHARE_TOPO, NULL, PMIX_BOOL);
#endif
    }
#endif
    if (PMIX_SUCCESS != (rc = PMIx_server_init(&mymodule, info, ninfo))) {
        fprintf(stderr, "Init failed with error %d\n", rc);
        return rc;
    }
    PMIX_INFO_FREE(info, ninfo);

    /* register the default errhandler */
    DEBUG_CONSTRUCT_LOCK(&mylock);
    ninfo = 1;
    PMIX_INFO_CREATE(info, ninfo);
    PMIX_INFO_LOAD(&info[0], PMIX_EVENT_HDLR_NAME, "SIMPTEST-DEFAULT", PMIX_STRING);
    PMIx_Register_event_handler(NULL, 0, info, ninfo,
                                errhandler, errhandler_reg_callbk, (void*)&mylock);
    DEBUG_WAIT_THREAD(&mylock);
    PMIX_INFO_FREE(info, ninfo);
    if (PMIX_SUCCESS != mylock.status) {
        exit(mylock.status);
    }
    DEBUG_DESTRUCT_LOCK(&mylock);

    /* register a handler specifically for when models declare */
    DEBUG_CONSTRUCT_LOCK(&mylock);
    ninfo = 1;
    PMIX_INFO_CREATE(info, ninfo);
    PMIX_INFO_LOAD(&info[0], PMIX_EVENT_HDLR_NAME, "SIMPTEST-MODEL", PMIX_STRING);
    code = PMIX_MODEL_DECLARED;
    PMIx_Register_event_handler(&code, 1, info, ninfo,
                                model_callback, model_registration_callback, (void*)&mylock);
    DEBUG_WAIT_THREAD(&mylock);
    PMIX_INFO_FREE(info, ninfo);
    if (PMIX_SUCCESS != mylock.status) {
        exit(mylock.status);
    }
    DEBUG_DESTRUCT_LOCK(&mylock);

    /* setup the pub data, in case it is used */
    PMIX_CONSTRUCT(&pubdata, pmix_list_t);

    /* setup to see sigchld on the forked tests */
    PMIX_CONSTRUCT(&children, pmix_list_t);
    pmix_event_assign(&handler, pmix_globals.evbase, SIGCHLD,
                      EV_SIGNAL|EV_PERSIST,wait_signal_callback, &handler);
    pmix_event_add(&handler, NULL);

    /* we have a single namespace for all clients */
    atmp = NULL;
    for (n=0; n < nprocs; n++) {
        asprintf(&tmp, "%d", n);
        pmix_argv_append_nosize(&atmp, tmp);
        free(tmp);
    }
    tmp = pmix_argv_join(atmp, ',');
    pmix_argv_free(atmp);
    x = PMIX_NEW(myxfer_t);
    set_namespace(nprocs, tmp, "foobar", opcbfunc, x);

    /* set common argv and env */
    client_env = pmix_argv_copy(environ);
    pmix_argv_prepend_nosize(&client_argv, executable);

    wakeup = nprocs;
    myuid = getuid();
    mygid = getgid();

    /* collect our inventory */
    DEBUG_CONSTRUCT_LOCK(&mylock);
    fprintf(stderr, "Collecting inventory\n");
    if (PMIX_SUCCESS != (rc = PMIx_server_collect_inventory(NULL, 0, infocbfunc, (void*)&mylock))) {
        fprintf(stderr, "Collect inventory failed: %d\n", rc);
        DEBUG_DESTRUCT_LOCK(&mylock);
        goto done;
    }
    DEBUG_WAIT_THREAD(&mylock);
    fprintf(stderr, "Inventory collected: %d\n", mylock.status);
    if (PMIX_SUCCESS != mylock.status) {
        exit(mylock.status);
    }
    DEBUG_DESTRUCT_LOCK(&mylock);

    /* if the nspace registration hasn't completed yet,
     * wait for it here */
    DEBUG_WAIT_THREAD(&x->lock);
    free(tmp);
    PMIX_RELEASE(x);

    /* fork/exec the test */
    (void)strncpy(proc.nspace, "foobar", PMIX_MAX_NSLEN);
    for (n = 0; n < nprocs; n++) {
        proc.rank = n;
        if (PMIX_SUCCESS != (rc = PMIx_server_setup_fork(&proc, &client_env))) {//n
            fprintf(stderr, "Server fork setup failed with error %d\n", rc);
            PMIx_server_finalize();
            return rc;
        }
        /* if cross-version test is requested, then oscillate PTL support
         * by rank */
        if (cross_version) {
            if (0 == n % 2) {
                pmix_setenv("PMIX_MCA_ptl", "tcp", true, &client_env);
            } else {
                pmix_setenv("PMIX_MCA_ptl", "usock", true, &client_env);
            }
        } else if (!usock) {
            /* don't disable usock => enable it on client */
            pmix_setenv("PMIX_MCA_ptl", "usock", true, &client_env);
        }
        x = PMIX_NEW(myxfer_t);
        if (PMIX_SUCCESS != (rc = PMIx_server_register_client(&proc, myuid, mygid,
                                                              NULL, opcbfunc, x))) {
            fprintf(stderr, "Server register client failed with error %d\n", rc);
            PMIx_server_finalize();
            return rc;
        }
        /* don't fork/exec the client until we know it is registered
         * so we avoid a potential race condition in the server */
        DEBUG_WAIT_THREAD(&x->lock);
        PMIX_RELEASE(x);
        pid = fork();
        if (pid < 0) {
            fprintf(stderr, "Fork failed\n");
            PMIx_server_finalize();
            return -1;
        }
        if (pid == 0) {
            sigset_t sigs;
            set_handler_default(SIGTERM);
            set_handler_default(SIGINT);
            set_handler_default(SIGHUP);
            set_handler_default(SIGPIPE);
            set_handler_default(SIGCHLD);
            sigprocmask(0, 0, &sigs);
            sigprocmask(SIG_UNBLOCK, &sigs, 0);
            execve(executable, client_argv, client_env);
            /* Does not return */
            exit(0);
        } else {
            child = PMIX_NEW(wait_tracker_t);
            child->pid = pid;
            pmix_list_append(&children, &child->super);
        }
    }
    pmix_argv_free(client_argv);
    pmix_argv_free(client_env);

    /* hang around until the client(s) finalize */
    while (0 < wakeup) {
        struct timespec ts;
        ts.tv_sec = 0;
        ts.tv_nsec = 100000;
        nanosleep(&ts, NULL);
    }

    /* see if anyone exited with non-zero status unless the test
     * was expected to do so */
    if (NULL == strstr(executable, "simpdie")) {
      n=0;
      PMIX_LIST_FOREACH(child, &children, wait_tracker_t) {
          if (0 != child->exit_code) {
              fprintf(stderr, "Child %d [%d] exited with status %d - test FAILED\n", n, child->pid, child->exit_code);
          }
          ++n;
      }
    } else if (1 == exit_code) {
      exit_code = 0;
    }
    free(executable);

    /* try notifying ourselves */
    ninfo = 3;
    PMIX_INFO_CREATE(info, ninfo);
    PMIX_INFO_LOAD(&info[0], PMIX_PROGRAMMING_MODEL, "PMIX", PMIX_STRING);
    PMIX_INFO_LOAD(&info[1], PMIX_MODEL_LIBRARY_NAME, "test", PMIX_STRING);
    /* mark that it is not to go to any default handlers */
    PMIX_INFO_LOAD(&info[2], PMIX_EVENT_NON_DEFAULT, NULL, PMIX_BOOL);
    DEBUG_CONSTRUCT_LOCK(&globallock);
    PMIx_Notify_event(PMIX_MODEL_DECLARED,
                      &pmix_globals.myid, PMIX_RANGE_PROC_LOCAL,
                      info, ninfo, NULL, NULL);
    DEBUG_WAIT_THREAD(&globallock);
    DEBUG_DESTRUCT_LOCK(&globallock);
    PMIX_INFO_FREE(info, ninfo);

#if 0
    fprintf(stderr, "TEST NONDEFAULT NOTIFICATION\n");
    /* verify that notifications don't recirculate */
    ninfo = 1;
    PMIX_INFO_CREATE(info, ninfo);
     /* mark that it is not to go to any default handlers */
    PMIX_INFO_LOAD(&info[0], PMIX_EVENT_NON_DEFAULT, NULL, PMIX_BOOL);
    PMIx_Notify_event(PMIX_ERR_DEBUGGER_RELEASE,
                      &pmix_globals.myid, PMIX_RANGE_LOCAL,
                      info, ninfo, NULL, NULL);
    PMIX_INFO_FREE(info, ninfo);
    /* wait a little in case we get notified */
    for (ninfo=0; ninfo < 100000; ninfo++) {
        struct timespec t = {0, 100};
        nanosleep(&t, NULL);
    }
#endif

  done:
    /* deregister the event handlers */
    PMIx_Deregister_event_handler(0, NULL, NULL);

    /* release any pub data */
    PMIX_LIST_DESTRUCT(&pubdata);

    /* release the child tracker */
    PMIX_LIST_DESTRUCT(&children);

    /* finalize the server library */
    if (PMIX_SUCCESS != (rc = PMIx_server_finalize())) {
        fprintf(stderr, "Finalize failed with error %d\n", rc);
        exit_code = rc;
    }

    if (0 == exit_code) {
        fprintf(stderr, "Test finished OK!\n");
    } else {
        fprintf(stderr, "TEST FAILED WITH ERROR %d\n", exit_code);
    }

    return exit_code;
}

static void set_namespace(int nprocs, char *ranks, char *nspace,
                          pmix_op_cbfunc_t cbfunc, myxfer_t *x)
{
    char *regex, *ppn;
    int n, m, k;
    pmix_info_t *info;
    pmix_data_array_t *array;

    x->ninfo = 16 + nprocs;

    PMIX_INFO_CREATE(x->info, x->ninfo);
    n = 0;

    PMIx_generate_regex("test000,test001,test002", &regex);
    PMIx_generate_ppn("0;1;2", &ppn);

    (void)strncpy(x->info[n].key, PMIX_NODE_MAP, PMIX_MAX_KEYLEN);
    x->info[n].value.type = PMIX_STRING;
    x->info[n].value.data.string = regex;
    ++n;

    /* if we have some empty nodes, then fill their spots */
    (void)strncpy(x->info[n].key, PMIX_PROC_MAP, PMIX_MAX_KEYLEN);
    x->info[n].value.type = PMIX_STRING;
    x->info[n].value.data.string = ppn;
    ++n;

    (void)strncpy(x->info[n].key, PMIX_UNIV_SIZE, PMIX_MAX_KEYLEN);
    x->info[n].value.type = PMIX_UINT32;
    x->info[n].value.data.uint32 = nprocs;
    ++n;

    (void)strncpy(x->info[n].key, PMIX_SPAWNED, PMIX_MAX_KEYLEN);
    x->info[n].value.type = PMIX_UINT32;
    x->info[n].value.data.uint32 = 0;
    ++n;

    (void)strncpy(x->info[n].key, PMIX_LOCAL_SIZE, PMIX_MAX_KEYLEN);
    x->info[n].value.type = PMIX_UINT32;
    x->info[n].value.data.uint32 = nprocs;
    ++n;

    (void)strncpy(x->info[n].key, PMIX_LOCAL_PEERS, PMIX_MAX_KEYLEN);
    x->info[n].value.type = PMIX_STRING;
    x->info[n].value.data.string = strdup(ranks);
    ++n;

    (void)strncpy(x->info[n].key, PMIX_JOB_SIZE, PMIX_MAX_KEYLEN);
    x->info[n].value.type = PMIX_UINT32;
    x->info[n].value.data.uint32 = nprocs;
    ++n;

    (void)strncpy(x->info[n].key, PMIX_JOBID, PMIX_MAX_KEYLEN);
    x->info[n].value.type = PMIX_STRING;
    x->info[n].value.data.string = strdup("1234");
    ++n;

    (void)strncpy(x->info[n].key, PMIX_NPROC_OFFSET, PMIX_MAX_KEYLEN);
    x->info[n].value.type = PMIX_UINT32;
    x->info[n].value.data.uint32 = 0;
    ++n;

    (void)strncpy(x->info[n].key, PMIX_NODEID, PMIX_MAX_KEYLEN);
    x->info[n].value.type = PMIX_UINT32;
    x->info[n].value.data.uint32 = 0;
    ++n;

    (void)strncpy(x->info[n].key, PMIX_NODE_SIZE, PMIX_MAX_KEYLEN);
    x->info[n].value.type = PMIX_UINT32;
    x->info[n].value.data.uint32 = nprocs;
    ++n;

    (void)strncpy(x->info[n].key, PMIX_NUM_NODES, PMIX_MAX_KEYLEN);
    x->info[n].value.type = PMIX_UINT32;
    x->info[n].value.data.uint32 = 1;
    ++n;

    (void)strncpy(x->info[n].key, PMIX_UNIV_SIZE, PMIX_MAX_KEYLEN);
    x->info[n].value.type = PMIX_UINT32;
    x->info[n].value.data.uint32 = nprocs;
    ++n;

    (void)strncpy(x->info[n].key, PMIX_MAX_PROCS, PMIX_MAX_KEYLEN);
    x->info[n].value.type = PMIX_UINT32;
    x->info[n].value.data.uint32 = nprocs;
    ++n;

    (void)strncpy(x->info[n].key, PMIX_JOB_NUM_APPS, PMIX_MAX_KEYLEN);
    x->info[n].value.type = PMIX_UINT32;
    x->info[n].value.data.uint32 = 1;
    ++n;

    (void)strncpy(x->info[n].key, PMIX_LOCALLDR, PMIX_MAX_KEYLEN);
    x->info[n].value.type = PMIX_PROC_RANK;
    x->info[n].value.data.uint32 = 0;
    ++n;

    /* add the proc-specific data */
    for (m=0; m < nprocs; m++) {
        (void)strncpy(x->info[n].key, PMIX_PROC_DATA, PMIX_MAX_KEYLEN);
        x->info[n].value.type = PMIX_DATA_ARRAY;
        PMIX_DATA_ARRAY_CREATE(array, 5, PMIX_INFO);
        x->info[n].value.data.darray = array;
        info = (pmix_info_t*)array->array;
        k = 0;
        (void)strncpy(info[k].key, PMIX_RANK, PMIX_MAX_KEYLEN);
        info[k].value.type = PMIX_PROC_RANK;
        info[k].value.data.rank = m;
        ++k;
        (void)strncpy(info[k].key, PMIX_GLOBAL_RANK, PMIX_MAX_KEYLEN);
        info[k].value.type = PMIX_PROC_RANK;
        info[k].value.data.rank = m;
        ++k;
        (void)strncpy(info[k].key, PMIX_LOCAL_RANK, PMIX_MAX_KEYLEN);
        info[k].value.type = PMIX_UINT16;
        info[k].value.data.uint16 = m;
        ++k;

        (void)strncpy(info[k].key, PMIX_NODE_RANK, PMIX_MAX_KEYLEN);
        info[k].value.type = PMIX_UINT16;
        info[k].value.data.uint16 = m;
        ++k;

        (void)strncpy(info[k].key, PMIX_NODEID, PMIX_MAX_KEYLEN);
        info[k].value.type = PMIX_UINT32;
        info[k].value.data.uint32 = 0;
        ++k;
        /* move to next proc */
        ++n;
    }
    PMIx_server_register_nspace(nspace, nprocs, x->info, x->ninfo,
                                cbfunc, x);
}

static void errhandler(size_t evhdlr_registration_id,
                       pmix_status_t status,
                       const pmix_proc_t *source,
                       pmix_info_t info[], size_t ninfo,
                       pmix_info_t results[], size_t nresults,
                       pmix_event_notification_cbfunc_fn_t cbfunc,
                       void *cbdata)
{
    pmix_output(0, "SERVER: ERRHANDLER CALLED WITH STATUS %d", status);
    /* we must NOT tell the event handler state machine that we
     * are the last step as that will prevent it from notifying
     * anyone else that might be listening for declarations */
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
    }
}

static void errhandler_reg_callbk (pmix_status_t status,
                                   size_t errhandler_ref,
                                   void *cbdata)
{
    mylock_t *lock = (mylock_t*)cbdata;

    lock->status = status;
    DEBUG_WAKEUP_THREAD(lock);
}

static pmix_status_t connected(const pmix_proc_t *proc, void *server_object,
                               pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    return PMIX_OPERATION_SUCCEEDED;
}
static pmix_status_t finalized(const pmix_proc_t *proc, void *server_object,
                     pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    return PMIX_OPERATION_SUCCEEDED;
}

static void abcbfunc(pmix_status_t status, void *cbdata)
{
    myxfer_t *x = (myxfer_t*)cbdata;

    /* be sure to release the caller */
    if (NULL != x->cbfunc) {
        x->cbfunc(status, x->cbdata);
    }
    PMIX_RELEASE(x);
}

static pmix_status_t abort_fn(const pmix_proc_t *proc,
                              void *server_object,
                              int status, const char msg[],
                              pmix_proc_t procs[], size_t nprocs,
                              pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t rc;
    myxfer_t *x;

    if (NULL != procs) {
        pmix_output(0, "SERVER: ABORT on %s:%d", procs[0].nspace, procs[0].rank);
    } else {
        pmix_output(0, "SERVER: ABORT OF ALL PROCS IN NSPACE %s", proc->nspace);
    }

    /* instead of aborting the specified procs, notify them
     * (if they have registered their errhandler) */

    /* use the myxfer_t object to ensure we release
     * the caller when notification has been queued */
    x = PMIX_NEW(myxfer_t);
    (void)strncpy(x->caller.nspace, proc->nspace, PMIX_MAX_NSLEN);
    x->caller.rank = proc->rank;

    PMIX_INFO_CREATE(x->info, 2);
    (void)strncpy(x->info[0].key, "DARTH", PMIX_MAX_KEYLEN);
    x->info[0].value.type = PMIX_INT8;
    x->info[0].value.data.int8 = 12;
    (void)strncpy(x->info[1].key, "VADER", PMIX_MAX_KEYLEN);
    x->info[1].value.type = PMIX_DOUBLE;
    x->info[1].value.data.dval = 12.34;
    x->cbfunc = cbfunc;
    x->cbdata = cbdata;

    if (PMIX_SUCCESS != (rc = PMIx_Notify_event(status, &x->caller,
                                                PMIX_RANGE_NAMESPACE,
                                                x->info, 2,
                                                abcbfunc, x))) {
        pmix_output(0, "SERVER: FAILED NOTIFY ERROR %d", (int)rc);
    }

    return PMIX_SUCCESS;
}

static void fencbfn(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *scd = (pmix_shift_caddy_t*)cbdata;

    /* pass the provided data back to each participating proc */
    if (NULL != scd->cbfunc.modexcbfunc) {
        scd->cbfunc.modexcbfunc(scd->status, scd->data, scd->ndata, scd->cbdata, NULL, NULL);
    }
    PMIX_RELEASE(scd);
}
static pmix_status_t fencenb_fn(const pmix_proc_t procs[], size_t nprocs,
                      const pmix_info_t info[], size_t ninfo,
                      char *data, size_t ndata,
                      pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    pmix_shift_caddy_t *scd;

    scd = PMIX_NEW(pmix_shift_caddy_t);
    scd->status = PMIX_SUCCESS;
    scd->data = data;
    scd->ndata = ndata;
    scd->cbfunc.modexcbfunc = cbfunc;
    scd->cbdata = cbdata;
    PMIX_THREADSHIFT(scd, fencbfn);
    return PMIX_SUCCESS;
}


static pmix_status_t dmodex_fn(const pmix_proc_t *proc,
                     const pmix_info_t info[], size_t ninfo,
                     pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    pmix_shift_caddy_t *scd;

    /* if this is a timeout test, then do nothing */
    if (istimeouttest) {
        return PMIX_SUCCESS;
    }

    scd = PMIX_NEW(pmix_shift_caddy_t);
    scd->status = PMIX_ERR_NOT_FOUND;
    scd->cbfunc.modexcbfunc = cbfunc;
    scd->cbdata = cbdata;
    PMIX_THREADSHIFT(scd, fencbfn);

    return PMIX_SUCCESS;
}


static pmix_status_t publish_fn(const pmix_proc_t *proc,
                      const pmix_info_t info[], size_t ninfo,
                      pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_locdat_t *p;
    size_t n;

    for (n=0; n < ninfo; n++) {
        p = PMIX_NEW(pmix_locdat_t);
        (void)strncpy(p->pdata.proc.nspace, proc->nspace, PMIX_MAX_NSLEN);
        p->pdata.proc.rank = proc->rank;
        (void)strncpy(p->pdata.key, info[n].key, PMIX_MAX_KEYLEN);
        pmix_value_xfer(&p->pdata.value, (pmix_value_t*)&info[n].value);
        pmix_list_append(&pubdata, &p->super);
    }

    return PMIX_OPERATION_SUCCEEDED;
}

typedef struct {
    pmix_event_t ev;
    pmix_pdata_t *pd;
    size_t n;
    pmix_lookup_cbfunc_t cbfunc;
    void *cbdata;
} lkobj_t;

static void lkcbfn(int sd, short args, void *cbdata)
{
    lkobj_t *lk = (lkobj_t*)cbdata;

    lk->cbfunc(PMIX_SUCCESS, lk->pd, lk->n, lk->cbdata);
    PMIX_PDATA_FREE(lk->pd, lk->n);
    free(lk);
}

static pmix_status_t lookup_fn(const pmix_proc_t *proc, char **keys,
                     const pmix_info_t info[], size_t ninfo,
                     pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    pmix_locdat_t *p, *p2;
    pmix_list_t results;
    size_t i, n;
    pmix_pdata_t *pd = NULL;
    pmix_status_t ret = PMIX_ERR_NOT_FOUND;
    lkobj_t *lk;

    PMIX_CONSTRUCT(&results, pmix_list_t);

    for (n=0; NULL != keys[n]; n++) {
        PMIX_LIST_FOREACH(p, &pubdata, pmix_locdat_t) {
            if (0 == strncmp(keys[n], p->pdata.key, PMIX_MAX_KEYLEN)) {
                p2 = PMIX_NEW(pmix_locdat_t);
                (void)strncpy(p2->pdata.proc.nspace, p->pdata.proc.nspace, PMIX_MAX_NSLEN);
                p2->pdata.proc.rank = p->pdata.proc.rank;
                (void)strncpy(p2->pdata.key, p->pdata.key, PMIX_MAX_KEYLEN);
                pmix_value_xfer(&p2->pdata.value, &p->pdata.value);
                pmix_list_append(&results, &p2->super);
                break;
            }
        }
    }
    if (0 < (n = pmix_list_get_size(&results))) {
        ret = PMIX_SUCCESS;
        PMIX_PDATA_CREATE(pd, n);
        for (i=0; i < n; i++) {
            p = (pmix_locdat_t*)pmix_list_remove_first(&results);
            if (p) {
                (void)strncpy(pd[i].proc.nspace, p->pdata.proc.nspace, PMIX_MAX_NSLEN);
                pd[i].proc.rank = p->pdata.proc.rank;
                (void)strncpy(pd[i].key, p->pdata.key, PMIX_MAX_KEYLEN);
                pmix_value_xfer(&pd[i].value, &p->pdata.value);
            }
        }
    }
    PMIX_LIST_DESTRUCT(&results);
    if (PMIX_SUCCESS == ret) {
        lk = (lkobj_t*)malloc(sizeof(lkobj_t));
        lk->pd = pd;
        lk->n = n;
        lk->cbfunc = cbfunc;
        lk->cbdata = cbdata;
        PMIX_THREADSHIFT(lk, lkcbfn);
    }

    return ret;
}


static pmix_status_t unpublish_fn(const pmix_proc_t *proc, char **keys,
                        const pmix_info_t info[], size_t ninfo,
                        pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_locdat_t *p, *p2;
    size_t n;

    for (n=0; NULL != keys[n]; n++) {
        PMIX_LIST_FOREACH_SAFE(p, p2, &pubdata, pmix_locdat_t) {
            if (0 == strncmp(keys[n], p->pdata.key, PMIX_MAX_KEYLEN)) {
                pmix_list_remove_item(&pubdata, &p->super);
                PMIX_RELEASE(p);
                break;
            }
        }
    }
    return PMIX_OPERATION_SUCCEEDED;
}

static void spcbfunc(pmix_status_t status, void *cbdata)
{
    myxfer_t *x = (myxfer_t*)cbdata;

    if (NULL != x->spcbfunc) {
        x->spcbfunc(PMIX_SUCCESS, "DYNSPACE", x->cbdata);
    }
}

static pmix_status_t spawn_fn(const pmix_proc_t *proc,
                    const pmix_info_t job_info[], size_t ninfo,
                    const pmix_app_t apps[], size_t napps,
                    pmix_spawn_cbfunc_t cbfunc, void *cbdata)
{
    myxfer_t *x;
    size_t n;
    pmix_proc_t *pptr;
    bool spawned;

    /* check the job info for parent and spawned keys */
    for (n=0; n < ninfo; n++) {
        if (0 == strncmp(job_info[n].key, PMIX_PARENT_ID, PMIX_MAX_KEYLEN)) {
            pptr = job_info[n].value.data.proc;
            pmix_output(0, "SPAWN: Parent ID %s:%d", pptr->nspace, pptr->rank);
        } else if (0 == strncmp(job_info[n].key, PMIX_SPAWNED, PMIX_MAX_KEYLEN)) {
            spawned = PMIX_INFO_TRUE(&job_info[n]);
            pmix_output(0, "SPAWN: Spawned %s", spawned ? "TRUE" : "FALSE");
        }
    }

    /* in practice, we would pass this request to the local
     * resource manager for launch, and then have that server
     * execute our callback function. For now, we will fake
     * the spawn and just pretend */

    /* must register the nspace for the new procs before
     * we return to the caller */
    x = PMIX_NEW(myxfer_t);
    x->spcbfunc = cbfunc;
    x->cbdata = cbdata;

    set_namespace(2, "0,1", "DYNSPACE", spcbfunc, x);

    return PMIX_SUCCESS;
}

static int numconnects = 0;

static pmix_status_t connect_fn(const pmix_proc_t procs[], size_t nprocs,
                                const pmix_info_t info[], size_t ninfo,
                                pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    /* in practice, we would pass this request to the local
     * resource manager for handling */

    numconnects++;

    return PMIX_OPERATION_SUCCEEDED;
}


static pmix_status_t disconnect_fn(const pmix_proc_t procs[], size_t nprocs,
                                   const pmix_info_t info[], size_t ninfo,
                                   pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    return PMIX_OPERATION_SUCCEEDED;
}

static pmix_status_t register_event_fn(pmix_status_t *codes, size_t ncodes,
                                       const pmix_info_t info[], size_t ninfo,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    return PMIX_OPERATION_SUCCEEDED;
}

static pmix_status_t deregister_events(pmix_status_t *codes, size_t ncodes,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    return PMIX_OPERATION_SUCCEEDED;
}

static pmix_status_t notify_event(pmix_status_t code,
                                  const pmix_proc_t *source,
                                  pmix_data_range_t range,
                                  pmix_info_t info[], size_t ninfo,
                                  pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    return PMIX_OPERATION_SUCCEEDED;
}

typedef struct query_data_t {
    pmix_event_t ev;
    pmix_info_t *data;
    size_t ndata;
    pmix_info_cbfunc_t cbfunc;
    void *cbdata;
} query_data_t;

static void qfn(int sd, short args, void *cbdata)
{
    query_data_t *qd = (query_data_t*)cbdata;

    qd->cbfunc(PMIX_SUCCESS, qd->data, qd->ndata, qd->cbdata, NULL, NULL);
    PMIX_INFO_FREE(qd->data, qd->ndata);
}

static pmix_status_t query_fn(pmix_proc_t *proct,
                              pmix_query_t *queries, size_t nqueries,
                              pmix_info_cbfunc_t cbfunc,
                              void *cbdata)
{
    size_t n;
    pmix_info_t *info;
    query_data_t qd;

    if (NULL == cbfunc) {
        return PMIX_ERROR;
    }
    /* keep this simple */
    PMIX_INFO_CREATE(info, nqueries);
    for (n=0; n < nqueries; n++) {
        pmix_output(0, "\tKey: %s", queries[n].keys[0]);
        (void)strncpy(info[n].key, queries[n].keys[0], PMIX_MAX_KEYLEN);
        info[n].value.type = PMIX_STRING;
        if (0 > asprintf(&info[n].value.data.string, "%d", (int)n)) {
            return PMIX_ERROR;
        }
    }
    qd.data = info;
    qd.ndata = nqueries;
    qd.cbfunc = cbfunc;
    qd.cbdata = cbdata;
    PMIX_THREADSHIFT(&qd, qfn);
    return PMIX_SUCCESS;
}

static void tool_connect_fn(pmix_info_t *info, size_t ninfo,
                            pmix_tool_connection_cbfunc_t cbfunc,
                            void *cbdata)
{
    pmix_proc_t proc;

    /* just pass back an arbitrary nspace */
    (void)strncpy(proc.nspace, "TOOL", PMIX_MAX_NSLEN);
    proc.rank = 0;

    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, &proc, cbdata);
    }
}

typedef struct {
    pmix_event_t ev;
    pmix_op_cbfunc_t cbfunc;
    void *cbdata;
} mylog_t;

static void foobar(int sd, short args, void *cbdata)
{
    mylog_t *lg = (mylog_t*)cbdata;
    lg->cbfunc(PMIX_SUCCESS, lg->cbdata);
}
static void log_fn(const pmix_proc_t *client,
                   const pmix_info_t data[], size_t ndata,
                   const pmix_info_t directives[], size_t ndirs,
                   pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    mylog_t *lg = (mylog_t *)malloc(sizeof(mylog_t));

    lg->cbfunc = cbfunc;
    lg->cbdata = cbdata;
    PMIX_THREADSHIFT(lg, foobar);
}

static pmix_status_t alloc_fn(const pmix_proc_t *client,
                              pmix_alloc_directive_t directive,
                              const pmix_info_t data[], size_t ndata,
                              pmix_info_cbfunc_t cbfunc, void *cbdata)
{
    return PMIX_OPERATION_SUCCEEDED;
}

static pmix_status_t jctrl_fn(const pmix_proc_t *requestor,
                              const pmix_proc_t targets[], size_t ntargets,
                              const pmix_info_t directives[], size_t ndirs,
                              pmix_info_cbfunc_t cbfunc, void *cbdata)
{
    return PMIX_OPERATION_SUCCEEDED;
}

static pmix_status_t mon_fn(const pmix_proc_t *requestor,
                            const pmix_info_t *monitor, pmix_status_t error,
                            const pmix_info_t directives[], size_t ndirs,
                            pmix_info_cbfunc_t cbfunc, void *cbdata)
{
    return PMIX_ERR_NOT_SUPPORTED;
}


static void wait_signal_callback(int fd, short event, void *arg)
{
    pmix_event_t *sig = (pmix_event_t*) arg;
    int status;
    pid_t pid;
    wait_tracker_t *t2;

    if (SIGCHLD != pmix_event_get_signal(sig)) {
        return;
    }

    /* we can have multiple children leave but only get one
     * sigchild callback, so reap all the waitpids until we
     * don't get anything valid back */
    while (1) {
        pid = waitpid(-1, &status, WNOHANG);
        if (-1 == pid && EINTR == errno) {
            /* try it again */
            continue;
        }
        /* if we got garbage, then nothing we can do */
        if (pid <= 0) {
            return;
        }

        /* we are already in an event, so it is safe to access the list */
        PMIX_LIST_FOREACH(t2, &children, wait_tracker_t) {
            if (pid == t2->pid) {
                /* found it! */
                if (WIFEXITED(status)) {
                    t2->exit_code = WEXITSTATUS(status);
                } else {
                    if (WIFSIGNALED(status)) {
                        t2->exit_code = WTERMSIG(status) + 128;
                    }
                }
                if (0 != t2->exit_code && 0 == exit_code) {
                    exit_code = t2->exit_code;
                }
                --wakeup;
                break;
            }
        }
    }
    fprintf(stderr, "ENDLOOP\n");
}
