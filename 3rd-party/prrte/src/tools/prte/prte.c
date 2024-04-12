/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2007-2009 Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2007-2017 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Geoffroy Vallee. All rights reserved.
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * Copyright (c) 2022-2023 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "src/include/constants.h"
#include "src/include/version.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_STRINGS_H
#    include <strings.h>
#endif /* HAVE_STRINGS_H */
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#    include <sys/param.h>
#endif
#include <ctype.h>
#include <errno.h>
#include <signal.h>
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_WAIT_H
#    include <sys/wait.h>
#endif /* HAVE_SYS_WAIT_H */
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif /* HAVE_SYS_TIME_H */
#include <fcntl.h>
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif
#ifdef HAVE_POLL_H
#    include <poll.h>
#endif

#include "src/event/event-internal.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/prteinstalldirs/prteinstalldirs.h"
#include "src/pmix/pmix-internal.h"
#include "src/threads/pmix_mutex.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_basename.h"
#include "src/util/prte_cmd_line.h"
#include "src/util/daemon_init.h"
#include "src/util/pmix_fd.h"
#include "src/util/pmix_os_dirpath.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_path.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_getcwd.h"
#include "src/util/pmix_show_help.h"
#include "src/util/pmix_string_copy.h"

#include "src/class/pmix_pointer_array.h"
#include "src/runtime/prte_progress_threads.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ess/base/base.h"
#include "src/mca/odls/odls.h"
#include "src/mca/plm/plm.h"
#include "src/mca/rmaps/base/base.h"
#include "src/rml/rml.h"
#include "src/mca/schizo/base/base.h"
#include "src/mca/state/base/base.h"
#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_wait.h"
#include "src/runtime/runtime.h"

#include "include/prte.h"
#include "src/prted/pmix/pmix_server_internal.h"
#include "src/prted/prted.h"

typedef struct {
    prte_pmix_lock_t lock;
    pmix_status_t status;
    pmix_info_t *info;
    size_t ninfo;
} mylock_t;

static pmix_nspace_t spawnednspace;
static pmix_proc_t myproc;
static bool signals_set = false;
static bool forcibly_die = false;
static prte_event_t term_handler;
static prte_event_t epipe_handler;
static int term_pipe[2];
static pmix_mutex_t prun_abort_inprogress_lock = PMIX_MUTEX_STATIC_INIT;
static prte_event_t *forward_signals_events = NULL;
static char *mypidfile = NULL;
static bool verbose = false;
static bool want_prefix_by_default = (bool) PRTE_WANT_PRTE_PREFIX_BY_DEFAULT;
static void abort_signal_callback(int signal);
static void clean_abort(int fd, short flags, void *arg);
static void signal_forward_callback(int fd, short args, void *cbdata);
static void epipe_signal_callback(int fd, short args, void *cbdata);
static int prep_singleton(const char *name);

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    prte_pmix_lock_t *lock = (prte_pmix_lock_t *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(status);

    PMIX_ACQUIRE_OBJECT(lock);
    PRTE_PMIX_WAKEUP_THREAD(lock);
}

static void setupcbfunc(pmix_status_t status, pmix_info_t info[], size_t ninfo,
                        void *provided_cbdata, pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    mylock_t *mylock = (mylock_t *) provided_cbdata;
    size_t n;

    if (NULL != info) {
        mylock->ninfo = ninfo;
        PMIX_INFO_CREATE(mylock->info, mylock->ninfo);
        /* cycle across the provided info */
        for (n = 0; n < ninfo; n++) {
            PMIX_INFO_XFER(&mylock->info[n], &info[n]);
        }
    } else {
        mylock->info = NULL;
        mylock->ninfo = 0;
    }
    mylock->status = status;

    /* release the caller */
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }

    PRTE_PMIX_WAKEUP_THREAD(&mylock->lock);
}

static void spcbfunc(pmix_status_t status, char nspace[], void *cbdata)
{
    prte_pmix_lock_t *lock = (prte_pmix_lock_t *) cbdata;

    PMIX_ACQUIRE_OBJECT(lock);
    lock->status = status;
    if (PMIX_SUCCESS == status) {
        lock->msg = strdup(nspace);
    }
    PRTE_PMIX_WAKEUP_THREAD(lock);
}

static void parent_died_fn(size_t evhdlr_registration_id, pmix_status_t status,
                           const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                           pmix_info_t results[], size_t nresults,
                           pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    PRTE_HIDE_UNUSED_PARAMS(evhdlr_registration_id, status, source, info, ninfo, results, nresults);
    clean_abort(0, 0, NULL);
    cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
}

static void evhandler_reg_callbk(pmix_status_t status, size_t evhandler_ref, void *cbdata)
{
    mylock_t *lock = (mylock_t *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(evhandler_ref);

    lock->status = status;
    PRTE_PMIX_WAKEUP_THREAD(&lock->lock);
}


static int wait_pipe[2];

static int wait_dvm(pid_t pid)
{
    char reply;
    int rc;
    int status;

    close(wait_pipe[1]);
    do {
        rc = read(wait_pipe[0], &reply, 1);
    } while (0 > rc && EINTR == errno);

    if (1 == rc && 'K' == reply) {
        return 0;
    } else if (0 == rc) {
        waitpid(pid, &status, 0);
        if (WIFEXITED(status)) {
            return WEXITSTATUS(status);
        }
    }
    return 255;
}

static void setup_sighandler(int signal, prte_event_t *ev, prte_event_cbfunc_t cbfunc)
{
    prte_event_signal_set(prte_event_base, ev, signal, cbfunc, ev);
    prte_event_signal_add(ev, NULL);
}

static void shutdown_callback(int fd, short flags, void *arg)
{
    prte_timer_t *tm = (prte_timer_t *) arg;
    prte_job_t *jdata;
    PRTE_HIDE_UNUSED_PARAMS(fd, flags);

    if (NULL != tm) {
        /* release the timer */
        PMIX_RELEASE(tm);
    }

    /* if we were ordered to abort, do so */
    pmix_output(0, "%s is executing clean abnormal termination",
                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
    /* do -not- call finalize as this will send a message to the HNP
     * indicating clean termination! Instead, just forcibly cleanup
     * the local session_dir tree and exit
     */
    prte_odls.kill_local_procs(NULL);
    // mark that we are finalizing so the session directory will cleanup
    prte_finalizing = true;
    jdata = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
    PMIX_RELEASE(jdata);
    exit(PRTE_ERROR_DEFAULT_EXIT_CODE);
}

#if PMIX_NUMERIC_VERSION < 0x00040205
static char *pmix_getline(FILE *fp)
{
    char *ret, *buff;
    char input[1024];

    ret = fgets(input, 1024, fp);
    if (NULL != ret) {
        input[strlen(input) - 1] = '\0'; /* remove newline */
        buff = strdup(input);
        return buff;
    }

    return NULL;
}
#endif

int main(int argc, char *argv[])
{
    int rc = 1, i;
    char *param, *timeoutenv, *tpath, *cptr;
    prte_pmix_lock_t lock;
    pmix_list_t apps;
    prte_pmix_app_t *app;
    pmix_info_t *iptr, *iptr2, info;
    pmix_status_t ret;
    bool flag;
    size_t n, ninfo, param_len;
    pmix_app_t *papps;
    size_t napps;
    mylock_t mylock;
    uint32_t ui32;
    char **pargv, **split;
    int pargc;
    prte_job_t *jdata;
    prte_app_context_t *dapp;
    bool proxyrun = false, first;
    void *jinfo;
    pmix_proc_t pname;
    pmix_value_t *val;
    pmix_data_array_t darray;
    char **hostfiles = NULL;
    char **hosts = NULL;
    prte_schizo_base_module_t *schizo;
    prte_ess_base_signal_t *sig;
    pmix_status_t code;
    char *personality;
    pmix_cli_result_t results;
    pmix_cli_item_t *opt;
    FILE *fp;

    /* init the globals */
    PMIX_CONSTRUCT(&apps, pmix_list_t);
    if (NULL == (param = getenv("PRTE_BASENAME"))) {
        prte_tool_basename = pmix_basename(argv[0]);
    } else {
        prte_tool_basename = strdup(param);
    }
    if (0 == strcmp(prte_tool_basename, "prterun")) {
        prte_tool_actual = "prterun";
    } else {
        prte_tool_actual = "prte";
    }
    pargc = argc;
    pargv = pmix_argv_copy_strip(argv); // strip any incoming quoted arguments

    /* save a pristine copy of the environment for launch purposes.
     * This MUST be done so that we can pass it to any local procs we
     * spawn - otherwise, those local procs will get a bunch of
     * params only relevant to PRRTE. Skip all PMIx and PRRTE params
     * as those are only targeting us
     */
    prte_launch_environ = NULL;
    for (i=0; NULL != environ[i]; i++) {
        if (0 != strncmp(environ[i], "PMIX_", 5) &&
            0 != strncmp(environ[i], "PRTE_", 5)) {
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&prte_launch_environ, environ[i]);
        }
    }

    rc = prte_init_minimum();
    if (PRTE_SUCCESS != rc) {
        return rc;
    }

    /* because we have to use the schizo framework and init our hostname
     * prior to parsing the incoming argv for cmd line options, do a hacky
     * search to support passing of impacted options (e.g., verbosity for schizo) */
    rc = prte_schizo_base_parse_prte(pargc, 0, pargv, NULL);
    if (PRTE_SUCCESS != rc) {
        return rc;
    }

    rc = prte_schizo_base_parse_pmix(pargc, 0, pargv, NULL);
    if (PRTE_SUCCESS != rc) {
        return rc;
    }

    /* init the tiny part of PRTE we initially need */
    prte_init_util(PRTE_PROC_MASTER);

    /** setup callbacks for abort signals - from this point
     * forward, we need to abort in a manner that allows us
     * to cleanup. However, we cannot directly use libevent
     * to trap these signals as otherwise we cannot respond
     * to them if we are stuck in an event! So instead use
     * the basic POSIX trap functions to handle the signal,
     * and then let that signal handler do some magic to
     * avoid the hang
     *
     * NOTE: posix traps don't allow us to do anything major
     * in them, so use a pipe tied to a libevent event to
     * reach a "safe" place where the termination event can
     * be created
     */
    if (0 != (rc = pipe(term_pipe))) {
        exit(1);
    }
    /* setup an event to attempt normal termination on signal */
    rc = prte_event_base_open();
    if (PRTE_SUCCESS != rc) {
        fprintf(stderr, "Unable to initialize event library\n");
        exit(1);
    }
    prte_event_set(prte_event_base, &term_handler, term_pipe[0], PRTE_EV_READ, clean_abort, NULL);
    prte_event_add(&term_handler, NULL);

    /* Set both ends of this pipe to be close-on-exec so that no
     children inherit it */
    if (pmix_fd_set_cloexec(term_pipe[0]) != PRTE_SUCCESS ||
        pmix_fd_set_cloexec(term_pipe[1]) != PRTE_SUCCESS) {
        fprintf(stderr, "unable to set the pipe to CLOEXEC\n");
        prte_progress_thread_finalize(NULL);
        exit(1);
    }

    /* setup callback for SIGPIPE */
    setup_sighandler(SIGPIPE, &epipe_handler, epipe_signal_callback);

    /* point the signal trap to a function that will activate that event */
    signal(SIGTERM, abort_signal_callback);
    signal(SIGINT, abort_signal_callback);
    signal(SIGHUP, abort_signal_callback);

    /* open the SCHIZO framework */
    rc = pmix_mca_base_framework_open(&prte_schizo_base_framework,
                                      PMIX_MCA_BASE_OPEN_DEFAULT);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
        return rc;
    }

    if (PRTE_SUCCESS != (rc = prte_schizo_base_select())) {
        PRTE_ERROR_LOG(rc);
        return rc;
    }

    /* look for any personality specification */
    personality = NULL;
    for (i = 0; NULL != argv[i]; i++) {
        if (0 == strcmp(argv[i], "--personality")) {
            personality = argv[i + 1];
            break;
        }
    }

    /* detect if we are running as a proxy and select the active
     * schizo module for this tool */
    schizo = prte_schizo_base_detect_proxy(personality);
    if (NULL == schizo) {
        pmix_show_help("help-schizo-base.txt", "no-proxy", true, prte_tool_basename, personality);
        return 1;
    }
    if (0 != strcmp(schizo->name, "prte")) {
        proxyrun = true;
    } else {
        /* if we are using the "prte" personality, but we
         * are not actually running as "prte" or are actively
         * testing the proxy capability , then we are acting
         * as a proxy */
        if (0 != strcmp(prte_tool_basename, "prte") ||
            prte_schizo_base.test_proxy_launch) {
            proxyrun = true;
        }
    }
    if (NULL == personality) {
        personality = schizo->name;
    }
    /* ensure we don't confuse any downstream PRRTE tools on
     * choice of proxy since some environments forward their envars */
    unsetenv("PRTE_MCA_schizo_proxy");

    /* Register all global MCA Params */
    if (PRTE_SUCCESS != (rc = prte_register_params())) {
        if (PRTE_ERR_SILENT != rc) {
            pmix_show_help("help-prte-runtime", "prte_init:startup:internal-failure", true,
                           "prte register params",
                           PRTE_ERROR_NAME(rc), rc);
        }
        return 1;
    }

    /* parse the input argv to get values, including everyone's MCA params */
    PMIX_CONSTRUCT(&results, pmix_cli_result_t);
    rc = schizo->parse_cli(pargv, &results, PMIX_CLI_WARN);
    if (PRTE_SUCCESS != rc) {
        PMIX_DESTRUCT(&results);
        if (PRTE_OPERATION_SUCCEEDED == rc) {
            return PRTE_SUCCESS;
        }
        if (PRTE_ERR_SILENT != rc) {
            fprintf(stderr, "%s: command line error (%s)\n", prte_tool_basename, prte_strerror(rc));
        }
        return rc;
    }

    /* check if we are running as root - if we are, then only allow
     * us to proceed if the allow-run-as-root flag was given. Otherwise,
     * exit with a giant warning message
     */
    if (0 == geteuid()) {
        schizo->allow_run_as_root(&results); // will exit us if not allowed
    }

    // check for an appfile
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_APPFILE);
    if (NULL != opt) {
        // parse the file and add its context to the argv array
        fp = fopen(opt->values[0], "r");
        if (NULL == fp) {
            pmix_show_help("help-prun", "appfile-failure", true, opt->values[0]);
            return 1;
        }
        first = true;
        while (NULL != (param = pmix_getline(fp))) {
            if (!first) {
                // add a colon delimiter
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&pargv, ":");
                ++pargc;
            }
            // break the line down into parts
            split = PMIX_ARGV_SPLIT_COMPAT(param, ' ');
            for (n=0; NULL != split[n]; n++) {
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&pargv, split[n]);
                ++pargc;
            }
            PMIX_ARGV_FREE_COMPAT(split);
            first = false;
        }
        fclose(fp);
    }

    /* decide if we are to use a persistent DVM, or act alone */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_DVM);
    if (proxyrun && (NULL != opt || NULL != getenv("PRTEPROXY_USE_DVM"))) {
        /* use a persistent DVM - act like prun */
        if (NULL != opt && NULL != opt->values && NULL != opt->values[0]) {
            /* they provided a directive on how to find the DVM */
            if (0 == strncasecmp(opt->values[0], "file:", 5)) {
                /* change the key to match what prun expects */
                free(opt->key);
                opt->key = strdup(PRTE_CLI_DVM_URI);
            } else if (0 == strncasecmp(opt->values[0], "uri:", 4)) {
                free(opt->key);
                opt->key = strdup(PRTE_CLI_DVM_URI);
                /* must remove the "uri:" prefix */
                cptr = strdup(&opt->values[0][4]);
                free(opt->values[0]);
                opt->values[0] = cptr;
            } else if (0 == strncasecmp(opt->values[0], "pid:", 4)) {
                free(opt->key);
                opt->key = strdup(PRTE_CLI_PID);
                /* must remove the "pid:" prefix */
                cptr = strdup(&opt->values[0][4]);
                free(opt->values[0]);
                opt->values[0] = cptr;
            } else if (0 == strncasecmp(opt->values[0], "ns:", 3)) {
                free(opt->key);
                opt->key = strdup(PRTE_CLI_NAMESPACE);
                /* must remove the "ns:" prefix */
                cptr = strdup(&opt->values[0][3]);
                free(opt->values[0]);
                opt->values[0] = cptr;
            } else if (0 == strncasecmp(opt->values[0], "system", 6)) {
                /* direct to search for a system server */
                free(opt->key);
                opt->key = strdup(PRTE_CLI_SYS_SERVER_ONLY);
            } else if (0 == strncasecmp(opt->values[0], "system-first", 6)) {
                /* direct to search for a system server first, and then
                 * take the first available DVM */
                free(opt->key);
                opt->key = strdup(PRTE_CLI_NAMESPACE);
            } else if (0 != strncasecmp(opt->values[0], "search", 6)) {
                /* "search" would mean to look for first available DVM,
                 * so we wouldn't have to adjust anything as the opt
                 * key is already set to PRTE_CLI_DVM, which will be
                 * ignored so that the PMIx_tool_init in prun_common
                 * will conduct its standard server search.
                 * However, if this is not "search", then this is an
                 * unknown option and must be reported to the user as
                 * an error */
                pmix_show_help("help-prun.txt", "bad-dvm-option", true,
                               opt->values[0], prte_tool_basename);
                return 1;
            }
        }
        rc = prun_common(&results, schizo, argc, argv);
        exit(rc);
    }

    /** RUN INDEPENDENTLY */

    /* if we were given a keepalive pipe, set up to monitor it now */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_KEEPALIVE);
    if (NULL != opt) {
        PMIX_SETENV_COMPAT("PMIX_KEEPALIVE_PIPE", opt->values[0], true, &environ);
    }

    /* check for debug options */
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_DEBUG)) {
        prte_debug_flag = true;
    }
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_DEBUG_DAEMONS)) {
        prte_debug_daemons_flag = true;
    }
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_DEBUG_DAEMONS_FILE)) {
        prte_debug_daemons_file_flag = true;
    }
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_LEAVE_SESSION_ATTACHED)) {
        prte_leave_session_attached = true;
    }

    /* detach from controlling terminal
     * otherwise, remain attached so output can get to us
     */
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_DAEMONIZE)) {
        pipe(wait_pipe);
        prte_state_base.parent_fd = wait_pipe[1];
        prte_daemon_init_callback(NULL, wait_dvm);
        close(wait_pipe[0]);
    } else {
#if defined(HAVE_SETSID)
        /* see if we were directed to separate from current session */
        if (pmix_cmd_line_is_taken(&results, PRTE_CLI_SET_SID)) {
            setsid();
        }
#endif
    }

    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_NO_READY_MSG)) {
        prte_state_base.ready_msg = false;
    }

    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_SYSTEM_SERVER)) {
        /* we should act as system-level PMIx server */
        PMIX_SETENV_COMPAT("PRTE_MCA_pmix_system_server", "1", true, &environ);
    }
    /* always act as session-level PMIx server */
    PMIX_SETENV_COMPAT("PRTE_MCA_pmix_session_server", "1", true, &environ);
    /* if we were asked to report a uri, set the MCA param to do so */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_REPORT_URI);
    if (NULL != opt) {
        prte_pmix_server_globals.report_uri = strdup(opt->values[0]);
    }

    /* if we were given a launch agent, set the MCA param for it */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_LAUNCH_AGENT);
    if (NULL != opt) {
        setenv("PRTE_MCA_prte_launch_agent", opt->values[0], true); // cmd line overrides all
    }

    /* if we are supporting a singleton, cache its ID
     * so it can get picked up and registered by server init */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_SINGLETON);
    if (NULL != opt) {
        prte_pmix_server_globals.singleton = strdup(opt->values[0]);
    }

    /* default to a persistent DVM */
    prte_persistent = true;

    /* if we are told to daemonize, then we cannot have apps */
    if (!pmix_cmd_line_is_taken(&results, PRTE_CLI_DAEMONIZE)) {
        /* see if they want to run an application - let's parse
         * the cmd line to get it */
        rc = prte_parse_locals(schizo, &apps, pargv, &hostfiles, &hosts);
        // not-found => no app given
        if (PRTE_SUCCESS != rc && PRTE_ERR_NOT_FOUND != rc) {
            PRTE_UPDATE_EXIT_STATUS(rc);
            goto DONE;
        }
        /* did they provide an app? */
        if (PMIX_SUCCESS != rc || 0 == pmix_list_get_size(&apps)) {
            if (proxyrun) {
                pmix_show_help("help-prun.txt", "prun:executable-not-specified", true,
                               prte_tool_basename, prte_tool_basename);
                PRTE_UPDATE_EXIT_STATUS(rc);
                goto DONE;
            }
            /* nope - just need to wait for instructions */
        } else {
            /* they did provide an app - this is only allowed
             * when running as a proxy! */
            if (!proxyrun) {
                pmix_show_help("help-prun.txt", "prun:executable-incorrectly-given", true,
                               prte_tool_basename, prte_tool_basename);
                PRTE_UPDATE_EXIT_STATUS(rc);
                goto DONE;
            }
            /* mark that we are not a persistent DVM */
            prte_persistent = false;
        }
    }

    /* setup PRTE infrastructure */
    if (PRTE_SUCCESS != (ret = prte_init(&pargc, &pargv, PRTE_PROC_MASTER))) {
        PRTE_ERROR_LOG(ret);
        return ret;
    }
    /* get my proc ID */
    ret = PMIx_Get(NULL, PMIX_PROCID, NULL, 0, &val);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        PRTE_UPDATE_EXIT_STATUS(PRTE_ERR_FATAL);
        goto DONE;
    }
    memcpy(&myproc, val->data.proc, sizeof(pmix_proc_t));
    PMIX_VALUE_RELEASE(val);

    /* setup callbacks for signals we should forward */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_FWD_SIGNALS);
    if (NULL != opt) {
        param = opt->values[0];
    } else {
        param = NULL;
    }
    if (PRTE_SUCCESS != (rc = prte_ess_base_setup_signals(param))) {
        PRTE_UPDATE_EXIT_STATUS(PRTE_ERR_FATAL);
        goto DONE;
    }
    if (0 < (i = pmix_list_get_size(&prte_ess_base_signals))) {
        forward_signals_events = (prte_event_t *) malloc(sizeof(prte_event_t) * i);
        if (NULL == forward_signals_events) {
            ret = PRTE_ERR_OUT_OF_RESOURCE;
            PRTE_UPDATE_EXIT_STATUS(PRTE_ERR_FATAL);
            goto DONE;
        }
        i = 0;
        PMIX_LIST_FOREACH(sig, &prte_ess_base_signals, prte_ess_base_signal_t)
        {
            setup_sighandler(sig->signal, forward_signals_events + i, signal_forward_callback);
            ++i;
        }
    }
    signals_set = true;

    /* if we are supporting a singleton, add it to our jobs */
    if (NULL != prte_pmix_server_globals.singleton) {
        rc = prep_singleton(prte_pmix_server_globals.singleton);
        if (PRTE_SUCCESS != ret) {
            PRTE_UPDATE_EXIT_STATUS(PRTE_ERR_FATAL);
            goto DONE;
        }
    }

    /* setup the keepalive event registration */
    PRTE_PMIX_CONSTRUCT_LOCK(&mylock.lock);
    code = PMIX_ERR_JOB_TERMINATED;
    PMIX_LOAD_PROCID(&pname, "PMIX_KEEPALIVE_PIPE", PMIX_RANK_UNDEF);
    PMIX_INFO_LOAD(&info, PMIX_EVENT_AFFECTED_PROC, &pname, PMIX_PROC);
    PMIx_Register_event_handler(&code, 1, &info, 1, parent_died_fn, evhandler_reg_callbk,
                                (void *) &mylock);
    PRTE_PMIX_WAIT_THREAD(&mylock.lock);
    PMIX_INFO_DESTRUCT(&info);
    PRTE_PMIX_DESTRUCT_LOCK(&mylock.lock);

    /* check for launch directives in case we were launched by a
     * tool wanting to direct our operation - this needs to be
     * done prior to starting the DVM as it may include instructions
     * on the daemon executable, the fork/exec agent to be used by
     * the daemons, or other directives impacting the DVM itself. */
    PMIX_LOAD_PROCID(&pname, myproc.nspace, PMIX_RANK_WILDCARD);
    PMIX_INFO_LOAD(&info, PMIX_OPTIONAL, NULL, PMIX_BOOL);
    /*  Have to cycle over directives we support*/
    ret = PMIx_Get(&pname, PMIX_FORKEXEC_AGENT, &info, 1, &val);
    PMIX_INFO_DESTRUCT(&info);
    if (PMIX_SUCCESS == ret) {
        /* set our fork/exec agent */
        PMIX_VALUE_RELEASE(val);
    }

    /* start the DVM */

    /* get the daemon job object - was created by ess/hnp component */
    if (NULL == (jdata = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace))) {
        pmix_show_help("help-prun.txt", "bad-job-object", true, prte_tool_basename);
        PRTE_UPDATE_EXIT_STATUS(PRTE_ERR_FATAL);
        goto DONE;
    }
    /* ess/hnp also should have created a daemon "app" */
    if (NULL == (dapp = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, 0))) {
        pmix_show_help("help-prun.txt", "bad-app-object", true, prte_tool_basename);
        PRTE_UPDATE_EXIT_STATUS(PRTE_ERR_FATAL);
        goto DONE;
    }

    /* Did the user specify a prefix, or want prefix by default? */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_PREFIX);
    if (NULL != opt || want_prefix_by_default) {
        if (NULL != opt) {
            param = strdup(opt->values[0]);
        } else {
            /* --enable-prun-prefix-default was given to prun */
            param = strdup(prte_install_dirs.prefix);
        }
        /* "Parse" the param, aka remove superfluous path_sep. */
        param_len = strlen(param);
        while (0 == strcmp(PRTE_PATH_SEP, &(param[param_len - 1]))) {
            param[param_len - 1] = '\0';
            param_len--;
            if (0 == param_len) {
                /* We get here if we removed all PATH_SEP's and end up
                   with an empty string.  In this case, the prefix is
                   just a single PATH_SEP. */
                strncpy(param, PRTE_PATH_SEP, sizeof(param) - 1);
                break;
            }
        }
        prte_set_attribute(&dapp->attributes, PRTE_APP_PREFIX_DIR, PRTE_ATTR_GLOBAL,
                           param, PMIX_STRING);
        free(param);
    } else {
        /* Check if called with fully-qualified path to prte.
           (Note: Put this second so can override with --prefix (above). */
        tpath = NULL;
        if ('/' == argv[0][0]) {
            char *tmp_basename = NULL;
            tpath = pmix_dirname(argv[0]);

            if (NULL != tpath) {
                /* Quick sanity check to ensure we got
                   something/bin/<exec_name> and that the installation
                   tree is at least more or less what we expect it to
                   be */
                tmp_basename = pmix_basename(tpath);
                if (0 == strcmp("bin", tmp_basename)) {
                    char *tmp = tpath;
                    tpath = pmix_dirname(tmp);
                    free(tmp);
                } else {
                    free(tpath);
                    tpath = NULL;
                }
                free(tmp_basename);
            }
            if (NULL != tpath) {
                prte_set_attribute(&dapp->attributes, PRTE_APP_PREFIX_DIR, PRTE_ATTR_GLOBAL,
                                   tpath, PMIX_STRING);
            }
        }
    }

    /* apply any provided runtime options to the DVM itself */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_RTOS);
    if (NULL != opt) {
        rc = prte_state_base_set_runtime_options(jdata, opt->values[0]);
        if (PRTE_SUCCESS != rc) {
            PRTE_UPDATE_EXIT_STATUS(PRTE_ERR_FATAL);
            goto DONE;
        }
    }

    /* check a couple of display options for the DVM itself */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_DISPLAY);
    if (NULL != opt) {
        char **targv;
        for (n=0; NULL != opt->values[n]; n++) {
            targv = PMIX_ARGV_SPLIT_COMPAT(opt->values[n], ',');
            for (i=0; NULL != targv[i]; i++) {
                if (PMIX_CHECK_CLI_OPTION(targv[i], PRTE_CLI_ALLOC)) {
                    prte_set_attribute(&jdata->attributes, PRTE_JOB_DISPLAY_ALLOC,
                                       PRTE_ATTR_GLOBAL, NULL, PMIX_BOOL);
                } else if (PMIX_CHECK_CLI_OPTION(targv[i], PRTE_CLI_PARSEABLE) ||
                           PMIX_CHECK_CLI_OPTION(targv[i], PRTE_CLI_PARSABLE)) {
                    prte_set_attribute(&jdata->attributes, PRTE_JOB_DISPLAY_PARSEABLE_OUTPUT,
                                       PRTE_ATTR_GLOBAL, NULL, PMIX_BOOL);
                }
            }
            PMIX_ARGV_FREE_COMPAT(targv);
        }
    }

    /* setup to listen for commands sent specifically to me, even though I would probably
     * be the one sending them! Unfortunately, since I am a participating daemon,
     * there are times I need to send a command to "all daemons", and that means *I* have
     * to receive it too
     */
    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_DAEMON,
                  PRTE_RML_PERSISTENT, prte_daemon_recv, NULL);

    /* setup to capture job-level info */
    PMIX_INFO_LIST_START(jinfo);

    /* see if we ourselves were spawned by someone */
    ret = PMIx_Get(&prte_process_info.myproc, PMIX_PARENT_ID, NULL, 0, &val);
    if (PMIX_SUCCESS == ret) {
        PMIX_LOAD_PROCID(&prte_process_info.my_parent, val->data.proc->nspace, val->data.proc->rank);
        PMIX_VALUE_RELEASE(val);
        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_REQUESTOR_IS_TOOL, NULL, PMIX_BOOL);
        /* indicate that we are launching on behalf of a parent */
        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_PARENT_ID, &prte_process_info.my_parent, PMIX_PROC);
    } else {
        PMIX_LOAD_PROCID(&prte_process_info.my_parent, prte_process_info.myproc.nspace, prte_process_info.myproc.rank);
    }

    /* add any hostfile directives to the daemon job */
    if (prte_persistent) {
        opt = pmix_cmd_line_get_param(&results, PRTE_CLI_HOSTFILE);
        if (NULL != opt) {
            tpath = PMIX_ARGV_JOIN_COMPAT(opt->values, ',');
            prte_set_attribute(&dapp->attributes, PRTE_APP_HOSTFILE,
                               PRTE_ATTR_GLOBAL, tpath, PMIX_STRING);
            free(tpath);
        }

        /* Did the user specify any hosts? */
        opt = pmix_cmd_line_get_param(&results, PRTE_CLI_HOST);
        if (NULL != opt) {
            char *tval;
            tval = PMIX_ARGV_JOIN_COMPAT(opt->values, ',');
            prte_set_attribute(&dapp->attributes, PRTE_APP_DASH_HOST,
                               PRTE_ATTR_GLOBAL, tval, PMIX_STRING);
            free(tval);
        }
    } else {
        /* the directives might be in the app(s) */
        if (NULL != hostfiles) {
            char *tval;
            tval = PMIX_ARGV_JOIN_COMPAT(hostfiles, ',');
            prte_set_attribute(&dapp->attributes, PRTE_APP_HOSTFILE,
                               PRTE_ATTR_GLOBAL, tval, PMIX_STRING);
            free(tval);
            PMIX_ARGV_FREE_COMPAT(hostfiles);
        }
        if (NULL != hosts) {
            char *tval;
            tval = PMIX_ARGV_JOIN_COMPAT(hosts, ',');
            prte_set_attribute(&dapp->attributes, PRTE_APP_DASH_HOST,
                               PRTE_ATTR_GLOBAL, tval, PMIX_STRING);
            free(tval);
            PMIX_ARGV_FREE_COMPAT(hosts);
        }
    }

    /* spawn the DVM - we skip the initial steps as this
     * isn't a user-level application */
    PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_ALLOCATE);

    /* we need to loop the event library until the DVM is alive */
    while (prte_event_base_active && !prte_dvm_ready) {
        prte_event_loop(prte_event_base, PRTE_EVLOOP_ONCE);
    }

    /* check if something went wrong with setting up the dvm, bail out */
    if (!prte_dvm_ready) {
        PRTE_UPDATE_EXIT_STATUS(PRTE_ERR_FATAL);
        goto DONE;
    }

    // see if we are to suicide
    if (PMIX_RANK_INVALID != prted_debug_failure) {
        /* are we the specified vpid? */
        if (PRTE_PROC_MY_NAME->rank == prted_debug_failure ||
            prted_debug_failure == PMIX_RANK_WILDCARD) {
            /* if the user specified we delay, then setup a timer
             * and have it kill us
             */
            if (0 < prted_debug_failure_delay) {
                PRTE_TIMER_EVENT(prted_debug_failure_delay, 0, shutdown_callback);

            } else {
                pmix_output(0, "%s is executing clean abnormal termination",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));

                /* do -not- call finalize as this will send a message to the HNP
                 * indicating clean termination! Instead, just forcibly cleanup
                 * the local session_dir tree and exit
                 */
                jdata = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
                PMIX_RELEASE(jdata);

                /* return with non-zero status */
                ret = PRTE_ERROR_DEFAULT_EXIT_CODE;
                goto DONE;
            }
        }
    }

    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_REPORT_PID);
    if (NULL != opt) {
        /* if the string is a "-", then output to stdout */
        if (0 == strcmp(opt->values[0], "-")) {
            fprintf(stdout, "%lu\n", (unsigned long) getpid());
        } else if (0 == strcmp(opt->values[0], "+")) {
            /* output to stderr */
            fprintf(stderr, "%lu\n", (unsigned long) getpid());
        } else {
            char *leftover;
            int outpipe;
            /* see if it is an integer pipe */
            leftover = NULL;
            outpipe = strtol(opt->values[0], &leftover, 10);
            if (NULL == leftover || 0 == strlen(leftover)) {
                /* stitch together the var names and URI */
                pmix_asprintf(&leftover, "%lu", (unsigned long) getpid());
                /* output to the pipe */
                rc = pmix_fd_write(outpipe, strlen(leftover) + 1, leftover);
                free(leftover);
                close(outpipe);
            } else {
                /* must be a file */
                FILE *fp;
                fp = fopen(opt->values[0], "w");
                if (NULL == fp) {
                    pmix_output(0, "Impossible to open the file %s in write mode\n", opt->values[0]);
                    PRTE_UPDATE_EXIT_STATUS(1);
                    goto DONE;
                }
                /* output my PID */
                fprintf(fp, "%lu\n", (unsigned long) getpid());
                fclose(fp);
                mypidfile = strdup(opt->values[0]);
            }
        }
    }

    if (prte_persistent) {
        PMIX_INFO_LIST_RELEASE(jinfo);
        goto proceed;
    }

    /***** CHECK FOR LAUNCH DIRECTIVES - ADD THEM TO JOB INFO IF FOUND ****/
    PMIX_LOAD_PROCID(&pname, myproc.nspace, PMIX_RANK_WILDCARD);
    PMIX_INFO_LOAD(&info, PMIX_OPTIONAL, NULL, PMIX_BOOL);
    ret = PMIx_Get(&pname, PMIX_LAUNCH_DIRECTIVES, &info, 1, &val);
    PMIX_INFO_DESTRUCT(&info);
    if (PMIX_SUCCESS == ret) {
        iptr = (pmix_info_t *) val->data.darray->array;
        ninfo = val->data.darray->size;
        for (n = 0; n < ninfo; n++) {
            PMIX_INFO_LIST_XFER(ret, jinfo, &iptr[n]);
        }
        PMIX_VALUE_RELEASE(val);
    }

    /* pass the personality */
    PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_PERSONALITY, personality, PMIX_STRING);

    /* get display options */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_DISPLAY);
    if (NULL != opt) {
        ret = prte_schizo_base_parse_display(opt, jinfo);
        if (PRTE_SUCCESS != ret) {
            PRTE_UPDATE_EXIT_STATUS(PRTE_ERR_FATAL);
            goto DONE;
        }
    }

    /* get output options */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_OUTPUT);
    if (NULL != opt) {
        ret = prte_schizo_base_parse_output(opt, jinfo);
        if (PRTE_SUCCESS != ret) {
            PRTE_UPDATE_EXIT_STATUS(PRTE_ERR_FATAL);
            goto DONE;
        }
    }

    /* check for runtime options */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_RTOS);
    if (NULL != opt) {
        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_RUNTIME_OPTIONS, opt->values[0], PMIX_STRING);
    }

    /* check what user wants us to do with stdin */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_STDIN);
    if (NULL != opt) {
        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_STDIN_TGT, opt->values[0], PMIX_STRING);
    }

    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_MAPBY);
    if (NULL != opt) {
        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_MAPBY, opt->values[0], PMIX_STRING);
    }

    /* if the user specified a ranking policy, then set it */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_RANKBY);
    if (NULL != opt) {
        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_RANKBY, opt->values[0], PMIX_STRING);
    }

    /* if the user specified a binding policy, then set it */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_BINDTO);
    if (NULL != opt) {
        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_BINDTO, opt->values[0], PMIX_STRING);
    }

    /* check for an exec agent */
   opt = pmix_cmd_line_get_param(&results, PRTE_CLI_EXEC_AGENT);
    if (NULL != opt) {
        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_EXEC_AGENT, opt->values[0], PMIX_STRING);
    }

    /* mark if recovery was enabled */
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_ENABLE_RECOVERY)) {
        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_JOB_RECOVERABLE, NULL, PMIX_BOOL);
    }
    /* record the max restarts */
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_MAX_RESTARTS);
    if (NULL != opt) {
        ui32 = strtol(opt->values[0], NULL, 10);
        PMIX_LIST_FOREACH(app, &apps, prte_pmix_app_t)
        {
            PMIX_INFO_LIST_ADD(ret, app->info, PMIX_MAX_RESTARTS, &ui32, PMIX_UINT32);
        }
    }
    /* if continuous operation was specified */
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_CONTINUOUS)) {
        /* mark this job as continuously operating */
        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_JOB_CONTINUOUS, NULL, PMIX_BOOL);
    }
#ifdef PMIX_ABORT_NONZERO_EXIT
    /* if ignore non-zero exit was specified */
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_TERM_NONZERO)) {
        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_ABORT_NONZERO_EXIT, NULL, PMIX_BOOL);
    }
#endif
    /* if stop-on-exec was specified */
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_STOP_ON_EXEC)) {
        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_DEBUG_STOP_ON_EXEC, NULL, PMIX_BOOL);
    }

    /* check for a job timeout specification, to be provided in seconds
     * as that is what MPICH used
     */
    timeoutenv = NULL;
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_TIMEOUT);
    if (NULL != opt || NULL != (timeoutenv = getenv("MPIEXEC_TIMEOUT"))) {
        if (NULL != timeoutenv) {
            i = strtol(timeoutenv, NULL, 10);
            /* both cannot be present, or they must agree */
            if (NULL != opt) {
                n = strtol(opt->values[0], NULL, 10);
                if (i != n) {
                    pmix_show_help("help-prun.txt", "prun:timeoutconflict", false,
                                   prte_tool_basename, n, timeoutenv);
                    PRTE_UPDATE_EXIT_STATUS(1);
                    goto DONE;
                }
            }
        } else {
            i = strtol(opt->values[0], NULL, 10);
        }
        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_JOB_TIMEOUT, &i, PMIX_INT);
    }
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_STACK_TRACES)) {
        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_TIMEOUT_STACKTRACES, NULL, PMIX_BOOL);
    }
    if (pmix_cmd_line_is_taken(&results, PRTE_CLI_REPORT_STATE)) {
        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_TIMEOUT_REPORT_STATE, NULL, PMIX_BOOL);
    }
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_SPAWN_TIMEOUT);
    if (NULL != opt) {
        i = strtol(opt->values[0], NULL, 10);
        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_SPAWN_TIMEOUT, &i, PMIX_INT);
    }
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_DO_NOT_AGG_HELP);
    if (NULL != opt) {
        flag = false;
        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_LOG_AGG, &flag, PMIX_BOOL);
    }

    /* give the schizo components a chance to add to the job info */
    schizo->job_info(&results, jinfo);

    /* pickup any relevant envars */
    ninfo = 4;
    PMIX_INFO_CREATE(iptr, ninfo);
    flag = true;
    PMIX_INFO_LOAD(&iptr[0], PMIX_SETUP_APP_ENVARS, &flag, PMIX_BOOL);
    ui32 = geteuid();
    PMIX_INFO_LOAD(&iptr[1], PMIX_USERID, &ui32, PMIX_UINT32);
    ui32 = getegid();
    PMIX_INFO_LOAD(&iptr[2], PMIX_GRPID, &ui32, PMIX_UINT32);
    PMIX_INFO_LOAD(&iptr[3], PMIX_PERSONALITY, personality, PMIX_STRING);

    PRTE_PMIX_CONSTRUCT_LOCK(&mylock.lock);
    ret = PMIx_server_setup_application(prte_process_info.myproc.nspace, iptr, ninfo, setupcbfunc,
                                        &mylock);
    if (PMIX_SUCCESS != ret) {
        pmix_output(0, "Error setting up application: %s", PMIx_Error_string(ret));
        PRTE_PMIX_DESTRUCT_LOCK(&mylock.lock);
        PRTE_UPDATE_EXIT_STATUS(ret);
        goto DONE;
    }
    PRTE_PMIX_WAIT_THREAD(&mylock.lock);
    PMIX_INFO_FREE(iptr, ninfo);
    if (PMIX_SUCCESS != mylock.status) {
        pmix_output(0, "Error setting up application: %s", PMIx_Error_string(mylock.status));
        PRTE_UPDATE_EXIT_STATUS(mylock.status);
        PRTE_PMIX_DESTRUCT_LOCK(&mylock.lock);
        goto DONE;
    }
    PRTE_PMIX_DESTRUCT_LOCK(&mylock.lock);
    /* transfer any returned ENVARS to the job_info */
    if (NULL != mylock.info) {
        for (n = 0; n < mylock.ninfo; n++) {
            if (PMIX_CHECK_KEY(&mylock.info[n], PMIX_SET_ENVAR) ||
                PMIX_CHECK_KEY(&mylock.info[n], PMIX_ADD_ENVAR) ||
                PMIX_CHECK_KEY(&mylock.info[n], PMIX_UNSET_ENVAR) ||
                PMIX_CHECK_KEY(&mylock.info[n], PMIX_PREPEND_ENVAR) ||
                PMIX_CHECK_KEY(&mylock.info[n], PMIX_APPEND_ENVAR)) {
                PMIX_INFO_LIST_XFER(ret, jinfo, &mylock.info[n]);
            }
        }
        PMIX_INFO_FREE(mylock.info, mylock.ninfo);
    }

    /* convert the job info into an array */
    PMIX_INFO_LIST_CONVERT(ret, jinfo, &darray);
    if (PMIX_ERR_EMPTY == ret) {
        iptr = NULL;
        ninfo = 0;
    } else if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        PRTE_UPDATE_EXIT_STATUS(rc);
        goto DONE;
    } else {
        iptr = (pmix_info_t *) darray.array;
        ninfo = darray.size;
    }
    PMIX_INFO_LIST_RELEASE(jinfo);

    /* convert the apps to an array */
    napps = pmix_list_get_size(&apps);
    PMIX_APP_CREATE(papps, napps);
    n = 0;
    PMIX_LIST_FOREACH(app, &apps, prte_pmix_app_t)
    {
        papps[n].cmd = strdup(app->app.cmd);
        papps[n].argv = PMIX_ARGV_COPY_COMPAT(app->app.argv);
        papps[n].env = PMIX_ARGV_COPY_COMPAT(app->app.env);
        papps[n].cwd = strdup(app->app.cwd);
        papps[n].maxprocs = app->app.maxprocs;
        PMIX_INFO_LIST_CONVERT(ret, app->info, &darray);
        if (PMIX_SUCCESS != ret) {
            if (PMIX_ERR_EMPTY == ret) {
                papps[n].info = NULL;
                papps[n].ninfo = 0;
            } else {
                PMIX_ERROR_LOG(ret);
                PRTE_UPDATE_EXIT_STATUS(rc);
                goto DONE;
            }
        } else {
            papps[n].info = (pmix_info_t *) darray.array;
            papps[n].ninfo = darray.size;
        }
        ++n;
    }

    if (verbose) {
        pmix_output(0, "Spawning job");
    }

    /* let the PMIx server handle it for us so that all the job infos
     * get properly recorded - e.g., forwarding IOF */
    PRTE_PMIX_CONSTRUCT_LOCK(&lock);
    ret = PMIx_Spawn_nb(iptr, ninfo, papps, napps, spcbfunc, &lock);
    if (PRTE_SUCCESS != ret) {
        pmix_output(0, "PMIx_Spawn failed (%d): %s", ret, PMIx_Error_string(ret));
        rc = ret;
        PRTE_UPDATE_EXIT_STATUS(rc);
        goto DONE;
    }
    /* we have to cycle the event library here so we can process
     * the spawn request */
    while (prte_event_base_active && lock.active) {
        prte_event_loop(prte_event_base, PRTE_EVLOOP_ONCE);
    }
    PMIX_ACQUIRE_OBJECT(&lock.lock);
    if (PMIX_SUCCESS != lock.status) {
        PRTE_UPDATE_EXIT_STATUS(lock.status);
        goto DONE;
    }
    PMIX_LOAD_NSPACE(spawnednspace, lock.msg);
    PRTE_PMIX_DESTRUCT_LOCK(&lock);

    if (verbose) {
        pmix_output(0, "JOB %s EXECUTING", PRTE_JOBID_PRINT(spawnednspace));
    }

    /* check what user wants us to do with stdin */
    PMIX_LOAD_NSPACE(pname.nspace, spawnednspace);
    opt = pmix_cmd_line_get_param(&results, PRTE_CLI_STDIN);
    if (NULL != opt) {
        if (0 == strcmp(opt->values[0], "all")) {
            pname.rank = PMIX_RANK_WILDCARD;
        } else if (0 == strcmp(opt->values[0], "none")) {
            pname.rank = PMIX_RANK_INVALID;
        } else {
            pname.rank = 0;
        }
    } else {
        pname.rank = 0;
    }
    if (PMIX_RANK_INVALID != pname.rank) {
        PMIX_INFO_CREATE(iptr2, 1);
        PMIX_INFO_LOAD(&iptr2[0], PMIX_IOF_PUSH_STDIN, NULL, PMIX_BOOL);
        PRTE_PMIX_CONSTRUCT_LOCK(&lock);
        ret = PMIx_IOF_push(&pname, 1, NULL, iptr2, 1, opcbfunc, &lock);
        if (PMIX_SUCCESS != ret && PMIX_OPERATION_SUCCEEDED != ret) {
            pmix_output(0, "IOF push of stdin failed: %s", PMIx_Error_string(ret));
        } else if (PMIX_SUCCESS == ret) {
            PRTE_PMIX_WAIT_THREAD(&lock);
        }
        PRTE_PMIX_DESTRUCT_LOCK(&lock);
        PMIX_INFO_FREE(iptr2, 1);
    }

proceed:
    /* loop the event lib until an exit event is detected */
    while (prte_event_base_active) {
        prte_event_loop(prte_event_base, PRTE_EVLOOP_ONCE);
    }

    PMIX_ACQUIRE_OBJECT(prte_event_base_active);

    /* close the push of our stdin */
    PMIX_INFO_LOAD(&info, PMIX_IOF_COMPLETE, NULL, PMIX_BOOL);
    PRTE_PMIX_CONSTRUCT_LOCK(&lock);
    ret = PMIx_IOF_push(NULL, 0, NULL, &info, 1, opcbfunc, &lock);
    if (PMIX_SUCCESS != ret && PMIX_OPERATION_SUCCEEDED != ret) {
        pmix_output(0, "IOF close of stdin failed: %s", PMIx_Error_string(ret));
    } else if (PMIX_SUCCESS == ret) {
        PRTE_PMIX_WAIT_THREAD(&lock);
    }
    PRTE_PMIX_DESTRUCT_LOCK(&lock);
    PMIX_INFO_DESTRUCT(&info);

DONE:
    /* cleanup and leave */
    prte_finalize();

    if (NULL != mypidfile) {
        unlink(mypidfile);
    }

    if (prte_debug_flag) {
        fprintf(stderr, "exiting with status %d\n", prte_exit_status);
    }
    exit(prte_exit_status);
}

static void clean_abort(int fd, short flags, void *arg)
{
    PRTE_HIDE_UNUSED_PARAMS(fd, flags, arg);

    /* if we have already ordered this once, don't keep
     * doing it to avoid race conditions
     */
    if (pmix_mutex_trylock(&prun_abort_inprogress_lock)) { /* returns 1 if already locked */
        if (forcibly_die) {
            /* exit with a non-zero status */
            exit(1);
        }
        fprintf(stderr,
                "%s: abort is already in progress...hit ctrl-c again to forcibly terminate\n\n",
                prte_tool_basename);
        forcibly_die = true;
        /* reset the event */
        prte_event_add(&term_handler, NULL);
        return;
    }

    fflush(stderr);
    /* ensure we exit with a non-zero status */
    PRTE_UPDATE_EXIT_STATUS(PRTE_ERROR_DEFAULT_EXIT_CODE);
    /* ensure that the forwarding of stdin stops */
    prte_dvm_abort_ordered = true;
    /* tell us to be quiet - hey, the user killed us with a ctrl-c,
     * so need to tell them that!
     */
    prte_execute_quiet = true;
    prte_abnormal_term_ordered = true;
    /* We are in an event handler; the job completed procedure
     will delete the signal handler that is currently running
     (which is a Bad Thing), so we can't call it directly.
     Instead, we have to exit this handler and setup to call
     job_completed() after this. */
    prte_plm.terminate_orteds();
}

static bool first = true;
static bool second = true;

static void surekill(void)
{
    prte_proc_t *child;
    int n;
    pid_t pid;

    /* we don't know how far we got, so be careful here */
    if (NULL != prte_local_children) {
        for (n=0; n < prte_local_children->size; n++) {
            child = (prte_proc_t*)pmix_pointer_array_get_item(prte_local_children, n);
            if (NULL != child && 0 < child->pid) {
                pid = child->pid;
#if HAVE_SETPGID
                {
                    pid_t pgrp;
                    pgrp = getpgid(pid);
                    if (-1 != pgrp) {
                        /* target the lead process of the process
                         * group so we ensure that the signal is
                         * seen by all members of that group. This
                         * ensures that the signal is seen by any
                         * child processes our child may have
                         * started
                         */
                        pid = -pgrp;
                    }
                }
#endif
                kill(pid, SIGKILL);
            }
        }
    }
}

/*
 * Attempt to terminate the job and wait for callback indicating
 * the job has been aborted.
 */
static void abort_signal_callback(int fd)
{
    uint8_t foo = 1;
    char *msg = "Abort is in progress...hit ctrl-c again to forcibly terminate\n\n";
    PRTE_HIDE_UNUSED_PARAMS(fd);

    /* if this is the first time thru, just get
     * the current time
     */
    if (first) {
        first = false;
        /* tell the event lib to attempt to abnormally terminate */
        if (-1 == write(term_pipe[1], &foo, 1)) {
            exit(1);
        }
    } else if (second) {
        if (-1 == write(2, (void *) msg, strlen(msg))) {
            exit(1);
        }
        fflush(stderr);
        second = false;
    } else {
        surekill();  // ensure we attempt to kill everything
        pmix_os_dirpath_destroy(prte_process_info.top_session_dir, true, NULL);
        exit(1);
    }
}

static int prep_singleton(const char *name)
{
    char *ptr, *p1;
    prte_job_t *jdata;
    prte_node_t *node;
    prte_proc_t *proc;
    int rc;
    pmix_rank_t rank;
    prte_app_context_t *app;
    char cwd[PRTE_PATH_MAX];

    ptr = strdup(name);
    p1 = strrchr(ptr, '.');
    *p1 = '\0';
    ++p1;
    rank = strtoul(p1, NULL, 10);
    jdata = PMIX_NEW(prte_job_t);
    PMIX_LOAD_NSPACE(jdata->nspace, ptr);
    free(ptr);
    rc = prte_set_job_data_object(jdata);
    if (PRTE_SUCCESS != rc) {
        PRTE_UPDATE_EXIT_STATUS(PRTE_ERR_FATAL);
        PMIX_RELEASE(jdata);
        return PRTE_ERR_FATAL;
    }
    /* must have an app */
    app = PMIX_NEW(prte_app_context_t);
    app->app = strdup(jdata->nspace);
    app->num_procs = 1;
    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&app->argv, app->app);
    getcwd(cwd, sizeof(cwd));
    app->cwd = strdup(cwd);
    pmix_pointer_array_set_item(jdata->apps, 0, app);
    jdata->num_apps = 1;

    /* add a map */
    jdata->map = PMIX_NEW(prte_job_map_t);
    /* add our node to the map since the singleton must
     * be here */
    node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, PRTE_PROC_MY_NAME->rank);
    PMIX_RETAIN(node);
    pmix_pointer_array_add(jdata->map->nodes, node);
    ++(jdata->map->num_nodes);

    /* create a proc for the singleton */
    proc = PMIX_NEW(prte_proc_t);
    PMIX_LOAD_PROCID(&proc->name, jdata->nspace, rank);
    proc->parent = PRTE_PROC_MY_NAME->rank;
    proc->app_idx = 0;
    proc->app_rank = rank;
    proc->local_rank = 0;
    proc->node_rank = 0;
    proc->state = PRTE_PROC_STATE_RUNNING;
    /* link it to the app */
    PMIX_RETAIN(proc);
    pmix_pointer_array_set_item(&app->procs, rank, proc);
    app->first_rank = rank;
    /* link it to the node */
    PMIX_RETAIN(node);
    proc->node = node;
    /* add it to the job */
    pmix_pointer_array_set_item(jdata->procs, rank, proc);
    jdata->num_procs = 1;
    jdata->num_local_procs = 1;
    /* add it to the node */
    PMIX_RETAIN(proc);
    pmix_pointer_array_add(node->procs, proc);
    node->num_procs = 1;
    node->slots_inuse = 1;

    return PRTE_SUCCESS;
}

static void signal_forward_callback(int signum, short args, void *cbdata)
{
    pmix_status_t rc;
    pmix_proc_t proc;
    pmix_info_t info;
    PRTE_HIDE_UNUSED_PARAMS(args, cbdata);

    if (verbose) {
        fprintf(stderr, "%s: Forwarding signal %d to job\n", prte_tool_basename, signum);
    }

    /* send the signal out to the processes */
    PMIX_LOAD_PROCID(&proc, spawnednspace, PMIX_RANK_WILDCARD);
    PMIX_INFO_LOAD(&info, PMIX_JOB_CTRL_SIGNAL, &signum, PMIX_INT);
    rc = PMIx_Job_control(&proc, 1, &info, 1, NULL, NULL);
    if (PMIX_SUCCESS != rc && PMIX_OPERATION_SUCCEEDED != rc) {
        fprintf(stderr, "Signal %d could not be sent to job %s (returned %s)", signum,
                spawnednspace, PMIx_Error_string(rc));
    }
}

/**
 * Deal with sigpipe errors
 */
static int sigpipe_error_count = 0;
static void epipe_signal_callback(int fd, short args, void *cbdata)
{
    PRTE_HIDE_UNUSED_PARAMS(fd, args, cbdata);

    sigpipe_error_count++;

    if (10 < sigpipe_error_count) {
        /* time to abort */
        pmix_output(0, "%s: SIGPIPE detected - aborting", prte_tool_basename);
        clean_abort(0, 0, NULL);
    }

    return;
}
