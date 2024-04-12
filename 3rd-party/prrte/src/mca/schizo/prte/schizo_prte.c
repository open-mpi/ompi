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
 * Copyright (c) 2006-2017 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2017 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2017      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018-2022 IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "prte_config.h"
#include "types.h"

#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include <ctype.h>
#include <getopt.h>


#include "src/util/name_fns.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_basename.h"
#include "src/util/pmix_os_dirpath.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_path.h"
#include "src/util/pmix_environ.h"
#include "src/util/prte_cmd_line.h"
#include "src/util/session_dir.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ess/base/base.h"
#include "src/mca/prteinstalldirs/prteinstalldirs.h"
#include "src/mca/rmaps/base/base.h"
#include "src/mca/state/base/base.h"
#include "src/runtime/prte_globals.h"
#include "src/runtime/pmix_init_util.h"

#include "schizo_prte.h"
#include "src/mca/schizo/base/base.h"

static int parse_cli(char **argv, pmix_cli_result_t *results, bool silent);
static int detect_proxy(char *argv);
static int parse_env(char **srcenv, char ***dstenv, pmix_cli_result_t *cli);
static void allow_run_as_root(pmix_cli_result_t *results);
static void job_info(pmix_cli_result_t *results,
                     void *jobinfo);
static int set_default_rto(prte_job_t *jdata,
                           prte_rmaps_options_t *options);

prte_schizo_base_module_t prte_schizo_prte_module = {
    .name = "prte",
    .parse_cli = parse_cli,
    .parse_env = parse_env,
    .setup_fork = prte_schizo_base_setup_fork,
    .detect_proxy = detect_proxy,
    .allow_run_as_root = allow_run_as_root,
    .job_info = job_info,
    .set_default_rto = set_default_rto,
    .check_sanity = prte_schizo_base_sanity
};

static struct option prteoptions[] = {
    /* basic options */
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_HELP, PMIX_ARG_OPTIONAL, 'h'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_VERSION, PMIX_ARG_NONE, 'V'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_VERBOSE, PMIX_ARG_NONE, 'v'),

    // MCA parameters
    PMIX_OPTION_DEFINE(PRTE_CLI_PRTEMCA, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PMIXMCA, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_TUNE, PMIX_ARG_REQD),

    // DVM options
    PMIX_OPTION_DEFINE(PRTE_CLI_NO_READY_MSG, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_DAEMONIZE, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_SYSTEM_SERVER, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_SET_SID, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_REPORT_PID, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_REPORT_URI, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_DEFAULT_HOSTFILE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_SINGLETON, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_KEEPALIVE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_LAUNCH_AGENT, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_MAX_VM_SIZE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_DEBUG, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_DEBUG_DAEMONS, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_DEBUG_DAEMONS_FILE, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_LEAVE_SESSION_ATTACHED, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_TMPDIR, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PREFIX, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_NOPREFIX, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_FWD_SIGNALS, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_RUN_AS_ROOT, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_DO_NOT_AGG_HELP, PMIX_ARG_NONE),

    // Launch options
    PMIX_OPTION_DEFINE(PRTE_CLI_TIMEOUT, PMIX_ARG_REQD),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_FWD_ENVAR, PMIX_ARG_REQD, 'x'),
    PMIX_OPTION_DEFINE(PRTE_CLI_HOSTFILE, PMIX_ARG_REQD),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_HOST, PMIX_ARG_REQD, 'H'),
    PMIX_OPTION_DEFINE(PRTE_CLI_EXEC_AGENT, PMIX_ARG_REQD),

    // Runtime options
    PMIX_OPTION_DEFINE(PRTE_CLI_RTOS, PMIX_ARG_REQD),

    // output options

    /* developer options */
    PMIX_OPTION_DEFINE(PRTE_CLI_DISPLAY, PMIX_ARG_REQD),

    // deprecated options
    PMIX_OPTION_DEFINE("machinefile", PMIX_ARG_REQD),

    PMIX_OPTION_END
};
static char *prteshorts = "h::vVx:H:";

static struct option prterunoptions[] = {
    /* basic options */
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_HELP, PMIX_ARG_OPTIONAL, 'h'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_VERSION, PMIX_ARG_NONE, 'V'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_VERBOSE, PMIX_ARG_NONE, 'v'),

    // MCA parameters
    PMIX_OPTION_DEFINE(PRTE_CLI_PRTEMCA, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PMIXMCA, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_TUNE, PMIX_ARG_REQD),

    // DVM options
    PMIX_OPTION_DEFINE(PRTE_CLI_SET_SID, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_REPORT_PID, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_REPORT_URI, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_DEFAULT_HOSTFILE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_KEEPALIVE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_LAUNCH_AGENT, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_MAX_VM_SIZE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_DEBUG, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_DEBUG_DAEMONS, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_DEBUG_DAEMONS_FILE, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_LEAVE_SESSION_ATTACHED, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_TMPDIR, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PREFIX, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_NOPREFIX, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_FWD_SIGNALS, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PERSONALITY, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_RUN_AS_ROOT, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_REPORT_CHILD_SEP, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_DVM, PMIX_ARG_REQD),

    // Launch options
    PMIX_OPTION_DEFINE(PRTE_CLI_TIMEOUT, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_REPORT_STATE, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_STACK_TRACES, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_SPAWN_TIMEOUT, PMIX_ARG_REQD),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_NP, PMIX_ARG_REQD, 'n'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_NP, PMIX_ARG_REQD, 'c'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_NPERNODE, PMIX_ARG_REQD, 'N'),
    PMIX_OPTION_DEFINE(PRTE_CLI_APPFILE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_XTERM, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_STOP_ON_EXEC, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_STOP_IN_INIT, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_STOP_IN_APP, PMIX_ARG_NONE),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_FWD_ENVAR, PMIX_ARG_REQD, 'x'),
    PMIX_OPTION_DEFINE(PRTE_CLI_WDIR, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("wd", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_SET_CWD_SESSION, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_PATH, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PSET, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_HOSTFILE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("machinefile", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_ADDHOSTFILE, PMIX_ARG_REQD),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_HOST, PMIX_ARG_REQD, 'H'),
    PMIX_OPTION_DEFINE(PRTE_CLI_ADDHOST, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PRELOAD_FILES, PMIX_ARG_REQD),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_PRELOAD_BIN, PMIX_ARG_NONE, 's'),
    PMIX_OPTION_DEFINE(PRTE_CLI_DO_NOT_AGG_HELP, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_FWD_ENVIRON, PMIX_ARG_OPTIONAL),

    // output options
    PMIX_OPTION_DEFINE(PRTE_CLI_OUTPUT, PMIX_ARG_REQD),

    // input options
    PMIX_OPTION_DEFINE(PRTE_CLI_STDIN, PMIX_ARG_REQD),

    /* Mapping options */
    PMIX_OPTION_DEFINE(PRTE_CLI_MAPBY, PMIX_ARG_REQD),

    /* Ranking options */
    PMIX_OPTION_DEFINE(PRTE_CLI_RANKBY, PMIX_ARG_REQD),

    /* Binding options */
    PMIX_OPTION_DEFINE(PRTE_CLI_BINDTO, PMIX_ARG_REQD),

    /* Runtime options */
    PMIX_OPTION_DEFINE(PRTE_CLI_RTOS, PMIX_ARG_REQD),

    /* display options */
    PMIX_OPTION_DEFINE(PRTE_CLI_DISPLAY, PMIX_ARG_REQD),

    // deprecated options
    PMIX_OPTION_DEFINE("mca", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("xml", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("tag-output", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("timestamp-output", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("output-directory", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("output-filename", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("merge-stderr-to-stdout", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("display-devel-map", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("display-topo", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("report-bindings", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("display-devel-allocation", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("display-map", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("display-allocation", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("rankfile", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("nolocal", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("oversubscribe", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("nooversubscribe", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("use-hwthread-cpus", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("cpu-set", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("cpu-list", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("bind-to-core", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("bynode", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("bycore", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("byslot", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("cpus-per-proc", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("cpus-per-rank", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("npernode", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("pernode", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("npersocket", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("ppr", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("debug", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("do-not-launch", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_OUTPUT_PROCTABLE, PMIX_ARG_OPTIONAL),

    PMIX_OPTION_END
};
static char *prterunshorts = "h::vVpn:c:N:sH:x:";

static struct option prunoptions[] = {
    /* basic options */
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_HELP, PMIX_ARG_OPTIONAL, 'h'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_VERSION, PMIX_ARG_NONE, 'V'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_VERBOSE, PMIX_ARG_NONE, 'v'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_PARSEABLE, PMIX_ARG_NONE, 'p'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_PARSABLE, PMIX_ARG_NONE, 'p'), // synonym for parseable

    // MCA parameters
    PMIX_OPTION_DEFINE(PRTE_CLI_PRTEMCA, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PMIXMCA, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_TUNE, PMIX_ARG_REQD),
    // DVM options
    PMIX_OPTION_DEFINE(PRTE_CLI_SYS_SERVER_FIRST, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_SYS_SERVER_ONLY, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_DO_NOT_CONNECT, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_WAIT_TO_CONNECT, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_NUM_CONNECT_RETRIES, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PID, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_NAMESPACE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_DVM_URI, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PERSONALITY, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_RUN_AS_ROOT, PMIX_ARG_NONE),

    // Launch options
    PMIX_OPTION_DEFINE(PRTE_CLI_TIMEOUT, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_REPORT_STATE, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_STACK_TRACES, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_SPAWN_TIMEOUT, PMIX_ARG_REQD),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_NP, PMIX_ARG_REQD, 'n'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_NP, PMIX_ARG_REQD, 'c'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_NPERNODE, PMIX_ARG_REQD, 'N'),
    PMIX_OPTION_DEFINE(PRTE_CLI_APPFILE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_APPFILE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_XTERM, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_STOP_ON_EXEC, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_STOP_IN_INIT, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_STOP_IN_APP, PMIX_ARG_NONE),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_FWD_ENVAR, PMIX_ARG_REQD, 'x'),
    PMIX_OPTION_DEFINE(PRTE_CLI_WDIR, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("wd", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_SET_CWD_SESSION, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_PATH, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PSET, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_HOSTFILE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("machinefile", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_ADDHOSTFILE, PMIX_ARG_REQD),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_HOST, PMIX_ARG_REQD, 'H'),
    PMIX_OPTION_DEFINE(PRTE_CLI_ADDHOST, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PRELOAD_FILES, PMIX_ARG_REQD),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_PRELOAD_BIN, PMIX_ARG_NONE, 's'),
    PMIX_OPTION_DEFINE(PRTE_CLI_DO_NOT_AGG_HELP, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_FWD_ENVIRON, PMIX_ARG_OPTIONAL),

    // output options
    PMIX_OPTION_DEFINE(PRTE_CLI_OUTPUT, PMIX_ARG_REQD),

    // input options
    PMIX_OPTION_DEFINE(PRTE_CLI_STDIN, PMIX_ARG_REQD),

    /* Mapping options */
    PMIX_OPTION_DEFINE(PRTE_CLI_MAPBY, PMIX_ARG_REQD),

    /* Ranking options */
    PMIX_OPTION_DEFINE(PRTE_CLI_RANKBY, PMIX_ARG_REQD),

    /* Binding options */
    PMIX_OPTION_DEFINE(PRTE_CLI_BINDTO, PMIX_ARG_REQD),

    /* Runtime options */
    PMIX_OPTION_DEFINE(PRTE_CLI_RTOS, PMIX_ARG_REQD),

    /* display options */
    PMIX_OPTION_DEFINE(PRTE_CLI_DISPLAY, PMIX_ARG_REQD),

    PMIX_OPTION_DEFINE(PRTE_CLI_ENABLE_RECOVERY, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_MAX_RESTARTS, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_DISABLE_RECOVERY, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_CONTINUOUS, PMIX_ARG_NONE),

    // deprecated options
    PMIX_OPTION_DEFINE("mca", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("xml", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("tag-output", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("timestamp-output", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("output-directory", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("output-filename", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("merge-stderr-to-stdout", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("display-devel-map", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("display-topo", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("report-bindings", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("display-devel-allocation", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("display-map", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("display-allocation", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("rankfile", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("nolocal", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("oversubscribe", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("nooversubscribe", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("use-hwthread-cpus", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("cpu-set", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("cpu-list", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("bind-to-core", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("bynode", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("bycore", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("byslot", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("cpus-per-proc", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("cpus-per-rank", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("npernode", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("pernode", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("npersocket", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("ppr", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("debug", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("do-not-launch", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_OUTPUT_PROCTABLE, PMIX_ARG_OPTIONAL),

    PMIX_OPTION_END
};
static char *prunshorts = "h::vVpn:c:N:sH:x:";

static struct option prtedoptions[] = {
    /* basic options */
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_HELP, PMIX_ARG_OPTIONAL, 'h'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_VERSION, PMIX_ARG_NONE, 'V'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_VERBOSE, PMIX_ARG_NONE, 'v'),

    // MCA parameters
    PMIX_OPTION_DEFINE(PRTE_CLI_PRTEMCA, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PMIXMCA, PMIX_ARG_REQD),

    // DVM options
    PMIX_OPTION_DEFINE(PRTE_CLI_PUBSUB_SERVER, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_CONTROLLER_URI, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_DAEMONIZE, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_PARENT_URI, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_TREE_SPAWN, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_SYSTEM_SERVER, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_SET_SID, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_DEBUG, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_DEBUG_DAEMONS, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_DEBUG_DAEMONS_FILE, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_TEST_SUICIDE, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_LEAVE_SESSION_ATTACHED, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_BOOTSTRAP, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_RUN_AS_ROOT, PMIX_ARG_NONE),

    PMIX_OPTION_END
};
static char *prtedshorts = "hvV";

static struct option ptermoptions[] = {
    /* basic options */
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_HELP, PMIX_ARG_OPTIONAL, 'h'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_VERSION, PMIX_ARG_NONE, 'V'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_VERBOSE, PMIX_ARG_NONE, 'v'),

    // MCA parameters
    PMIX_OPTION_DEFINE(PRTE_CLI_PMIXMCA, PMIX_ARG_REQD),

    // DVM options
    PMIX_OPTION_DEFINE(PRTE_CLI_SYS_SERVER_FIRST, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_SYS_SERVER_ONLY, PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE(PRTE_CLI_WAIT_TO_CONNECT, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_NUM_CONNECT_RETRIES, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_PID, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_NAMESPACE, PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE(PRTE_CLI_DVM_URI, PMIX_ARG_REQD),

    PMIX_OPTION_END
};
static char *ptermshorts = "hvV";

static struct option pinfooptions[] = {
    /* basic options */
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_HELP, PMIX_ARG_OPTIONAL, 'h'),
    PMIX_OPTION_SHORT_DEFINE(PRTE_CLI_VERSION, PMIX_ARG_NONE, 'V'),
    PMIX_OPTION_SHORT_DEFINE("all", PMIX_ARG_NONE, 'a'),
    PMIX_OPTION_DEFINE("arch", PMIX_ARG_NONE),
    PMIX_OPTION_SHORT_DEFINE("config", PMIX_ARG_NONE, 'c'),
    PMIX_OPTION_DEFINE("hostname", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("internal", PMIX_ARG_NONE),
    PMIX_OPTION_DEFINE("param", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("path", PMIX_ARG_REQD),
    PMIX_OPTION_DEFINE("show-version", PMIX_ARG_REQD),

    /* End of list */
    PMIX_OPTION_END
};
static char *pinfoshorts = "hVac";

static int convert_deprecated_cli(pmix_cli_result_t *results,
                                  bool silent);

static int parse_cli(char **argv, pmix_cli_result_t *results,
                     bool silent)
{
    char *shorts, *helpfile;
    struct option *myoptions;
    int rc, n;
    pmix_cli_item_t *opt;

    if (0 == strcmp(prte_tool_actual, "prte")) {
        myoptions = prteoptions;
        shorts = prteshorts;
        helpfile = "help-prte.txt";
    } else if (0 == strcmp(prte_tool_actual, "prterun")) {
        myoptions = prterunoptions;
        shorts = prterunshorts;
        helpfile = "help-prterun.txt";
    } else if (0 == strcmp(prte_tool_actual, "prted")) {
        myoptions = prtedoptions;
        shorts = prtedshorts;
        helpfile = "help-prted.txt";
    } else if (0 == strcmp(prte_tool_actual, "prun")) {
        myoptions = prunoptions;
        shorts = prunshorts;
        helpfile = "help-prun.txt";
    } else if (0 == strcmp(prte_tool_actual, "pterm")) {
        myoptions = ptermoptions;
        shorts = ptermshorts;
        helpfile = "help-pterm.txt";
    } else if (0 == strcmp(prte_tool_actual, "prte_info")) {
        myoptions = pinfooptions;
        shorts = pinfoshorts;
        helpfile = "help-prte-info.txt";
    }
    pmix_tool_msg = "Report bugs to: https://github.com/openpmix/prrte";
    pmix_tool_org = "PRRTE";
    pmix_tool_version = prte_util_make_version_string("all", PRTE_MAJOR_VERSION, PRTE_MINOR_VERSION,
                                                      PRTE_RELEASE_VERSION, PRTE_GREEK_VERSION, NULL);

    rc = pmix_cmd_line_parse(argv, shorts, myoptions, NULL,
                             results, helpfile);
    if (PMIX_SUCCESS != rc) {
        if (PMIX_OPERATION_SUCCEEDED == rc) {
            /* pmix cmd line interpreter output result
             * successfully - usually means version or
             * some other stock output was generated */
            return PRTE_OPERATION_SUCCEEDED;
        }
        rc = prte_pmix_convert_status(rc);
        return rc;
    }

    /* check for deprecated options - warn and convert them */
    rc = convert_deprecated_cli(results, silent);
    if (PRTE_SUCCESS != rc) {
        return rc;
    }

    // handle relevant MCA params
    PMIX_LIST_FOREACH(opt, &results->instances, pmix_cli_item_t) {
        if (0 == strcmp(opt->key, PRTE_CLI_PRTEMCA)) {
            for (n=0; NULL != opt->values[n]; n++) {
                prte_schizo_base_expose(opt->values[n], "PRTE_MCA_");
            }
        } else if (0 == strcmp(opt->key, PRTE_CLI_PMIXMCA)) {
            for (n=0; NULL != opt->values[n]; n++) {
                prte_schizo_base_expose(opt->values[n], "PMIX_MCA_");
            }
        }
    }
    return PRTE_SUCCESS;
}

static int convert_deprecated_cli(pmix_cli_result_t *results,
                                  bool silent)
{
    char *option, *p1, *p2, *tmp, *tmp2, *output;
    int rc = PRTE_SUCCESS;
    pmix_cli_item_t *opt, *nxt;
    bool warn;

    if (silent) {
        warn = false;
    } else {
        warn = prte_mca_schizo_prte_component.warn_deprecations;
    }

    PMIX_LIST_FOREACH_SAFE(opt, nxt, &results->instances, pmix_cli_item_t) {
        option = opt->key;
        if (0 == strcmp(option, "n")) {
            /* if they passed a "--n" option, we need to convert it
             * back to the "--np" one without a deprecation warning */
            rc = prte_schizo_base_add_directive(results, option, PRTE_CLI_NP, opt->values[0], false);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --nolocal -> --map-by :nolocal */
        else if (0 == strcmp(option, "nolocal")) {
            rc = prte_schizo_base_add_qualifier(results, option,
                                                PRTE_CLI_MAPBY, PRTE_CLI_NOLOCAL,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --oversubscribe -> --map-by :OVERSUBSCRIBE */
        else if (0 == strcmp(option, "oversubscribe")) {
            rc = prte_schizo_base_add_qualifier(results, option,
                                                PRTE_CLI_MAPBY, PRTE_CLI_OVERSUB,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --nooversubscribe -> --map-by :NOOVERSUBSCRIBE */
        else if (0 == strcmp(option, "nooversubscribe")) {
            rc = prte_schizo_base_add_qualifier(results, option,
                                                PRTE_CLI_MAPBY, PRTE_CLI_NOOVER,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --use-hwthread-cpus -> --map-by hwtcpus */
        else if (0 == strcmp(option, "use-hwthread-cpus")) {
            rc = prte_schizo_base_add_qualifier(results, option,
                                                PRTE_CLI_MAPBY, PRTE_CLI_HWTCPUS,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
            if (NULL != prte_set_slots) {
                free(prte_set_slots);
            }
            prte_set_slots = strdup("hwthreads");
        }
        /* --cpu-set and --cpu-list -> --map-by pe-list:X
         */
        else if (0 == strcmp(option, "cpu-set") || 0 == strcmp(option, "cpu-list")) {
            pmix_asprintf(&p2, "%s%s", PRTE_CLI_PELIST, opt->values[0]);
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --bind-to-core and --bind-to-socket -> --bind-to X */
        else if (0 == strcmp(option, "bind-to-core")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_BINDTO, PRTE_CLI_CORE,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        } else if (0 == strcmp(option, "bind-to-socket")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_BINDTO, PRTE_CLI_PACKAGE,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --bynode -> "--map-by X --rank-by X" */
        else if (0 == strcmp(option, "bynode")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, PRTE_CLI_NODE,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --bycore -> "--map-by X --rank-by X" */
        else if (0 == strcmp(option, "bycore")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, PRTE_CLI_CORE,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --byslot -> "--map-by X --rank-by X" */
        else if (0 == strcmp(option, "byslot")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, PRTE_CLI_SLOT,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --cpus-per-proc/rank X -> --map-by :pe=X */
        else if (0 == strcmp(option, "cpus-per-proc") || 0 == strcmp(option, "cpus-per-rank")) {
            pmix_asprintf(&p2, "%s%s", PRTE_CLI_PE, opt->values[0]);
            rc = prte_schizo_base_add_qualifier(results, option,
                                                PRTE_CLI_MAPBY, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* -N ->   map-by ppr:N:node */
        else if (0 == strcmp(option, "N")) {
            pmix_asprintf(&p2, "ppr:%s:node", opt->values[0]);
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --npernode X and --npersocket X -> --map-by ppr:X:node/socket */
        else if (0 == strcmp(option, "npernode")) {
            pmix_asprintf(&p2, "ppr:%s:node", opt->values[0]);
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        } else if (0 == strcmp(option, "pernode")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, "ppr:1:node",
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        } else if (0 == strcmp(option, "npersocket")) {
            pmix_asprintf(&p2, "ppr:%s:package", opt->values[0]);
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --ppr X -> --map-by ppr:X */
        else if (0 == strcmp(option, "ppr")) {
            /* if they didn't specify a complete pattern, then this is an error */
            if (NULL == strchr(opt->values[0], ':')) {
                pmix_show_help("help-schizo-base.txt", "bad-ppr", true, opt->values[0], true);
                return PRTE_ERR_SILENT;
            }
            pmix_asprintf(&p2, "ppr:%s", opt->values[0]);
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --am[ca] X -> --tune X */
        else if (0 == strcmp(option, "amca") || 0 == strcmp(option, "am")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_TUNE, opt->values[0],
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --rankfile X -> map-by rankfile:file=X */
        else if (0 == strcmp(option, "rankfile")) {
            pmix_asprintf(&p2, "%s%s", PRTE_CLI_QFILE, opt->values[0]);
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_MAPBY, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --tag-output  ->  "--output tag */
        else if (0 == strcmp(option, "tag-output")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_OUTPUT, PRTE_CLI_TAG,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --timestamp-output  ->  --output timestamp */
        else if (0 == strcmp(option, "timestamp-output")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_OUTPUT, PRTE_CLI_TIMESTAMP,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --output-directory DIR  ->  --output dir=DIR */
        else if (0 == strcmp(option, "output-directory")) {
            pmix_asprintf(&p2, "dir=%s", opt->values[0]);
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_OUTPUT, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --output-filename DIR  ->  --output file=file */
        else if (0 == strcmp(option, "--output-filename")) {
            pmix_asprintf(&p2, "file=%s", opt->values[0]);
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_OUTPUT, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --xml  ->  --output xml */
        else if (0 == strcmp(option, "xml")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_OUTPUT, PRTE_CLI_XML,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --display-devel-map  -> --display allocation-devel */
        else if (0 == strcmp(option, "display-devel-map")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_DISPLAY, PRTE_CLI_MAPDEV,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --output-proctable  ->  --runtime-options output-proctable */
        else if (0 == strcmp(option, PRTE_CLI_OUTPUT_PROCTABLE)) {
            if (NULL != opt->values && NULL != opt->values[0]) {
                pmix_asprintf(&p2, "%s=%s", PRTE_CLI_OUTPUT_PROCTABLE, opt->values[0]);
            } else {
                p2 = strdup(PRTE_CLI_OUTPUT_PROCTABLE);
            }
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_RTOS, p2,
                                                warn);
            free(p2);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --display-map  ->  --display map */
        else if (0 == strcmp(option, "display-map")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_DISPLAY, PRTE_CLI_MAP,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --display-topo  ->  --display topo */
        else if (0 == strcmp(option, "display-topo")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_DISPLAY, PRTE_CLI_TOPO,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --report-bindings  ->  --display bind */
        else if (0 == strcmp(option, "report-bindings")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_DISPLAY, PRTE_CLI_BIND,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --display-allocation  ->  --display allocation */
        else if (0 == strcmp(option, "display-allocation")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_DISPLAY, PRTE_CLI_ALLOC,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --debug will be deprecated starting with open mpi v5
         */
        else if (0 == strcmp(option, "debug")) {
            if (warn) {
                pmix_show_help("help-schizo-base.txt", "deprecated-inform", true, option,
                               "This CLI option will be deprecated starting in Open MPI v5");
            }
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --do-not-launch  ->  --runtime-options donotlaunch */
        else if (0 == strcmp(option, "do-not-launch")) {
            rc = prte_schizo_base_add_directive(results, option,
                                                PRTE_CLI_RTOS, PRTE_CLI_NOLAUNCH,
                                                warn);
            PMIX_CLI_REMOVE_DEPRECATED(results, opt);
        }
        /* --map-by socket ->  --map-by package */
        else if (0 == strcmp(option, PRTE_CLI_MAPBY)) {
            /* check the value of the option for "socket" */
            if (0 == strncasecmp(opt->values[0], "socket", strlen("socket"))) {
                p1 = strdup(opt->values[0]); // save the original option
                /* replace "socket" with "package" */
                if (NULL == (p2 = strchr(opt->values[0], ':'))) {
                    /* no modifiers */
                    tmp = strdup("package");
                } else {
                    *p2 = '\0';
                    ++p2;
                    pmix_asprintf(&tmp, "package:%s", p2);
                }
                if (warn) {
                    pmix_asprintf(&p2, "%s %s", option, p1);
                    pmix_asprintf(&tmp2, "%s %s", option, tmp);
                    /* can't just call show_help as we want every instance to be reported */
                    output = pmix_show_help_string("help-schizo-base.txt",
                                                   "deprecated-converted", true,
                                                   p2, tmp2);
                    fprintf(stderr, "%s\n", output);
                    free(output);
                    free(p2);
                    free(tmp2);
                }
                free(p1);
                free(opt->values[0]);
                opt->values[0] = tmp;
            }
        }
        /* --rank-by */
        else if (0 == strcmp(option, PRTE_CLI_RANKBY)) {
            /* check the value of the option for object-level directives - show help
             * for ranking if given */
            if (0 == strncasecmp(opt->values[0], "socket", strlen("socket")) ||
                0 == strncasecmp(opt->values[0], "l1cache", strlen("l1cache"))  ||
                0 == strncasecmp(opt->values[0], "l2cache", strlen("l2cache")) ||
                0 == strncasecmp(opt->values[0], "l3cache", strlen("l3cache")) ||
                0 == strncasecmp(opt->values[0], "numa", strlen("numa")) ||
                0 == strncasecmp(opt->values[0], "hwthread", strlen("hwthread")) ||
                0 == strncasecmp(opt->values[0], "core", strlen("core"))) {
                p1 = strdup(opt->values[0]); // save the original option
                /* replace "socket" with "package" */
                if (NULL == (p2 = strchr(opt->values[0], ':'))) {
                    /* no modifiers */
                    tmp = strdup("package");
                } else {
                    *p2 = '\0';
                    ++p2;
                    pmix_asprintf(&tmp, "package:%s", p2);
                }
                if (warn) {
                    pmix_asprintf(&p2, "%s %s", option, p1);
                    pmix_asprintf(&tmp2, "%s %s", option, tmp);
                    /* can't just call show_help as we want every instance to be reported */
                    output = pmix_show_help_string("help-schizo-base.txt", "deprecated-converted", true, p2,
                                                   tmp2);
                    fprintf(stderr, "%s\n", output);
                    free(output);
                    free(p2);
                    free(tmp2);
                }
                free(p1);
                free(opt->values[0]);
                opt->values[0] = tmp;
            }
        }
        /* --bind-to socket ->  --bind-to package */
        else if (0 == strcmp(option, PRTE_CLI_BINDTO)) {
            /* check the value of the option for "socket" */
            if (0 == strncasecmp(opt->values[0], "socket", strlen("socket"))) {
                p1 = strdup(opt->values[0]); // save the original option
                /* replace "socket" with "package" */
                if (NULL == (p2 = strchr(opt->values[0], ':'))) {
                    /* no modifiers */
                    tmp = strdup("package");
                } else {
                    *p2 = '\0';
                    ++p2;
                    pmix_asprintf(&tmp, "package:%s", p2);
                }
                if (warn) {
                    pmix_asprintf(&p2, "%s %s", option, p1);
                    pmix_asprintf(&tmp2, "%s %s", option, tmp);
                    /* can't just call show_help as we want every instance to be reported */
                    output = pmix_show_help_string("help-schizo-base.txt", "deprecated-converted", true, p2,
                                                   tmp2);
                    fprintf(stderr, "%s\n", output);
                    free(output);
                    free(p2);
                    free(tmp2);
                }
                free(p1);
                free(opt->values[0]);
                opt->values[0] = tmp;
            }
        }
    }

    return rc;
}

static int parse_env(char **srcenv, char ***dstenv,
                     pmix_cli_result_t *cli)
{
    int i, j, n;
    char *p1, *p2;
    char **env;
    char **xparams = NULL, **xvals = NULL;
    char *param, *value;
    pmix_cli_item_t *opt;
    PRTE_HIDE_UNUSED_PARAMS(srcenv);

    pmix_output_verbose(1, prte_schizo_base_framework.framework_output,
                        "%s schizo:prte: parse_env",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));

    if (NULL == cli) {
        return PRTE_SUCCESS;
    }

    env = *dstenv;

    /* look for -x options - not allowed to conflict with a -mca option */
    opt = pmix_cmd_line_get_param(cli, PRTE_CLI_FWD_ENVAR);
    if (NULL != opt) {
        for (j=0; NULL != opt->values[j]; j++) {
            /* the value is the envar */
            p1 = opt->values[j];
            /* if there is an '=' in it, then they are setting a value */
            if (NULL != (p2 = strchr(p1, '='))) {
                *p2 = '\0';
                ++p2;
            } else {
                p2 = getenv(p1);
                if (NULL == p2) {
                    pmix_show_help("help-schizo-base.txt", "missing-envar-param", true, p1);
                    continue;
                }
            }

            /* check if it is already present in the environment */
            for (n = 0; NULL != env && NULL != env[n]; n++) {
                param = strdup(env[n]);
                value = strchr(param, '=');
                *value = '\0';
                value++;
                /* check if parameter is already present */
                if (0 == strcmp(param, p1)) {
                    /* we do have it - check for same value */
                    if (0 != strcmp(value, p2)) {
                        /* this is an error - different values */
                        pmix_show_help("help-schizo-base.txt", "duplicate-mca-value", true, p1, p2,
                                       value);
                        free(param);
                        PMIX_ARGV_FREE_COMPAT(xparams);
                        PMIX_ARGV_FREE_COMPAT(xvals);
                        return PRTE_ERR_BAD_PARAM;
                    }
                }
                free(param);
            }

            /* check if we already processed a conflicting -x version with MCA prefix */
            if (NULL != xparams) {
                for (i = 0; NULL != xparams[i]; i++) {
                    if (0 == strncmp("PRTE_MCA_", p1, strlen("PRTE_MCA_"))) {
                        /* this is an error - different values */
                        pmix_show_help("help-schizo-base.txt", "duplicate-mca-value", true, p1, p2,
                                       xvals[i]);
                        PMIX_ARGV_FREE_COMPAT(xparams);
                        PMIX_ARGV_FREE_COMPAT(xvals);
                        return PRTE_ERR_BAD_PARAM;
                    }
                }
            }

            /* cache this for later inclusion - do not modify dstenv in this loop */
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&xparams, p1);
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&xvals, p2);
        }
    }

    /* add the -x values */
    if (NULL != xparams) {
        for (i = 0; NULL != xparams[i]; i++) {
            PMIX_SETENV_COMPAT(xparams[i], xvals[i], true, dstenv);
        }
        PMIX_ARGV_FREE_COMPAT(xparams);
        PMIX_ARGV_FREE_COMPAT(xvals);
    }

    return PRTE_SUCCESS;
}

static int detect_proxy(char *personalities)
{
    char *evar;

    pmix_output_verbose(2, prte_schizo_base_framework.framework_output,
                        "%s[%s]: detect proxy with %s (%s)",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), __FILE__,
                        (NULL == personalities) ? "NULL" : personalities,
                        prte_tool_basename);

    /* COMMAND-LINE OVERRRULES ALL */
    if (NULL != personalities) {
        /* this is a list of personalities we need to check -
         * if it contains "prte", then we are available but
         * at a low priority */
        if (NULL != strstr(personalities, "prte")) {
            return prte_mca_schizo_prte_component.priority;
        }
        return 0;
    }

    /* if we were told the proxy, then use it */
    if (NULL != (evar = getenv("PRTE_MCA_schizo_proxy"))) {
        if (0 == strcmp(evar, "prte")) {
            /* they asked exclusively for us */
            return 100;
        } else {
            /* they asked for somebody else */
            return 0;
        }
    }

    /* if neither of those were true, then just use our default */
    return prte_mca_schizo_prte_component.priority;
}

static void allow_run_as_root(pmix_cli_result_t *cli)
{
    char *r1, *r2;

    if (pmix_cmd_line_is_taken(cli, "allow-run-as-root")) {
        prte_allow_run_as_root = true;
        return;
    }

    if (NULL != (r1 = getenv("PRTE_ALLOW_RUN_AS_ROOT"))
        && NULL != (r2 = getenv("PRTE_ALLOW_RUN_AS_ROOT_CONFIRM"))) {
        if (0 == strcmp(r1, "1") && 0 == strcmp(r2, "1")) {
            prte_allow_run_as_root = true;
            return;
        }
    }

    prte_schizo_base_root_error_msg();
}

static void job_info(pmix_cli_result_t *results,
                     void *jobinfo)
{
    PRTE_HIDE_UNUSED_PARAMS(results, jobinfo);
    return;
}

static int set_default_rto(prte_job_t *jdata,
                           prte_rmaps_options_t *options)
{
    PRTE_HIDE_UNUSED_PARAMS(options);
    return prte_state_base_set_runtime_options(jdata, NULL);
}
