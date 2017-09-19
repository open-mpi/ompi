/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2017 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2007-2009 Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2007-2017 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif  /* HAVE_STRINGS_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <errno.h>
#include <signal.h>
#include <ctype.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif  /* HAVE_SYS_WAIT_H */
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */
#include <fcntl.h>
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include "opal/mca/event/event.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/pmix/base/base.h"
#include "opal/mca/base/base.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/basename.h"
#include "opal/util/cmd_line.h"
#include "opal/util/opal_environ.h"
#include "opal/util/opal_getcwd.h"
#include "opal/util/show_help.h"
#include "opal/util/fd.h"
#include "opal/sys/atomic.h"

#include "opal/version.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_info_support.h"
#include "opal/runtime/opal_progress_threads.h"
#include "opal/util/os_path.h"
#include "opal/util/path.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/state/state.h"

/* ensure I can behave like a daemon */
#include "prun.h"

/**
 * Global struct for caching orte command line options.
 */
struct orte_cmd_options_t {
    char *help;
    bool version;
    bool verbose;
    char *report_pid;
    char *report_uri;
    bool terminate;
    bool debugger;
    int num_procs;
    char *appfile;
    char *wdir;
    bool set_cwd_to_session_dir;
    char *path;
    char *preload_files;
    bool sleep;
    char *stdin_target;
    char *prefix;
    char *path_to_mpirun;
    bool disable_recovery;
    bool preload_binaries;
    bool index_argv;
    bool run_as_root;
    char *personality;
    bool create_dvm;
    bool terminate_dvm;
    bool nolocal;
    bool no_oversubscribe;
    bool oversubscribe;
    int cpus_per_proc;
    bool pernode;
    int npernode;
    bool use_hwthreads_as_cpus;
    int npersocket;
    char *mapping_policy;
    char *ranking_policy;
    char *binding_policy;
    bool report_bindings;
    char *cpu_list;
    bool debug;
    bool tag_output;
    bool timestamp_output;
    char *output_filename;
    bool merge;
    bool continuous;
    char *hnp;
    bool staged_exec;
    int timeout;
    bool report_state_on_timeout;
    bool get_stack_traces;
    int pid;
    bool system_server_only;
    bool system_server_first;
};
typedef struct orte_cmd_options_t orte_cmd_options_t;
static orte_cmd_options_t orte_cmd_options = {0};
static opal_cmd_line_t *orte_cmd_line = NULL;
static opal_list_t job_info;
static volatile bool active = false;

static int create_app(int argc, char* argv[],
                      opal_list_t *jdata,
                      opal_pmix_app_t **app,
                      bool *made_app, char ***app_env);
static int parse_locals(opal_list_t *jdata, int argc, char* argv[]);
static void set_classpath_jar_file(opal_pmix_app_t *app, int index, char *jarfile);


static opal_cmd_line_init_t cmd_line_init[] = {
    /* Various "obvious" options */
    { NULL, 'h', NULL, "help", 1,
      &orte_cmd_options.help, OPAL_CMD_LINE_TYPE_STRING,
      "This help message", OPAL_CMD_LINE_OTYPE_GENERAL },
    { NULL, 'V', NULL, "version", 0,
      &orte_cmd_options.version, OPAL_CMD_LINE_TYPE_BOOL,
      "Print version and exit", OPAL_CMD_LINE_OTYPE_GENERAL },
    { "orte_execute_quiet", 'q', NULL, "quiet", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Suppress helpful messages", OPAL_CMD_LINE_OTYPE_GENERAL },

    /* exit status reporting */
    { "orte_report_child_jobs_separately", '\0', "report-child-jobs-separately", "report-child-jobs-separately", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Return the exit status of the primary job only", OPAL_CMD_LINE_OTYPE_OUTPUT },

    /* select XML output */
    { "orte_xml_output", '\0', "xml", "xml", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Provide all output in XML format", OPAL_CMD_LINE_OTYPE_OUTPUT },
    { "orte_xml_file", '\0', "xml-file", "xml-file", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide all output in XML format to the specified file", OPAL_CMD_LINE_OTYPE_OUTPUT },

    /* tag output */
    { "orte_tag_output", '\0', "tag-output", "tag-output", 0,
      &orte_cmd_options.tag_output, OPAL_CMD_LINE_TYPE_BOOL,
      "Tag all output with [job,rank]", OPAL_CMD_LINE_OTYPE_OUTPUT },
    { "orte_timestamp_output", '\0', "timestamp-output", "timestamp-output", 0,
      &orte_cmd_options.timestamp_output, OPAL_CMD_LINE_TYPE_BOOL,
      "Timestamp all application process output", OPAL_CMD_LINE_OTYPE_OUTPUT },
    { "orte_output_filename", '\0', "output-filename", "output-filename", 1,
      &orte_cmd_options.output_filename, OPAL_CMD_LINE_TYPE_STRING,
      "Redirect output from application processes into filename/job/rank/std[out,err,diag]",
      OPAL_CMD_LINE_OTYPE_OUTPUT },
    { NULL, '\0', "merge-stderr-to-stdout", "merge-stderr-to-stdout", 0,
      &orte_cmd_options.merge, OPAL_CMD_LINE_TYPE_BOOL,
      "Merge stderr to stdout for each process", OPAL_CMD_LINE_OTYPE_OUTPUT },
    { "orte_xterm", '\0', "xterm", "xterm", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Create a new xterm window and display output from the specified ranks there",
      OPAL_CMD_LINE_OTYPE_OUTPUT },

    /* select stdin option */
    { NULL, '\0', "stdin", "stdin", 1,
      &orte_cmd_options.stdin_target, OPAL_CMD_LINE_TYPE_STRING,
      "Specify procs to receive stdin [rank, all, none] (default: 0, indicating rank 0)",
      OPAL_CMD_LINE_OTYPE_INPUT },

    /* request that argv[0] be indexed */
    { NULL, '\0', "index-argv-by-rank", "index-argv-by-rank", 0,
      &orte_cmd_options.index_argv, OPAL_CMD_LINE_TYPE_BOOL,
      "Uniquely index argv[0] for each process using its rank",
      OPAL_CMD_LINE_OTYPE_INPUT },

    /* Specify the launch agent to be used */
    { "orte_launch_agent", '\0', "launch-agent", "launch-agent", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Command used to start processes on remote nodes (default: orted)",
      OPAL_CMD_LINE_OTYPE_LAUNCH },

    /* Preload the binary on the remote machine */
    { NULL, 's', NULL, "preload-binary", 0,
      &orte_cmd_options.preload_binaries, OPAL_CMD_LINE_TYPE_BOOL,
      "Preload the binary on the remote machine before starting the remote process.",
      OPAL_CMD_LINE_OTYPE_LAUNCH },

    /* Preload files on the remote machine */
    { NULL, '\0', NULL, "preload-files", 1,
      &orte_cmd_options.preload_files, OPAL_CMD_LINE_TYPE_STRING,
      "Preload the comma separated list of files to the remote machines current working directory before starting the remote process.",
      OPAL_CMD_LINE_OTYPE_LAUNCH },

    /* Use an appfile */
    { NULL, '\0', NULL, "app", 1,
      &orte_cmd_options.appfile, OPAL_CMD_LINE_TYPE_STRING,
      "Provide an appfile; ignore all other command line options",
      OPAL_CMD_LINE_OTYPE_LAUNCH },

    /* Number of processes; -c, -n, --n, -np, and --np are all
       synonyms */
    { NULL, 'c', "np", "np", 1,
      &orte_cmd_options.num_procs, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run", OPAL_CMD_LINE_OTYPE_GENERAL },
    { NULL, '\0', "n", "n", 1,
      &orte_cmd_options.num_procs, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run", OPAL_CMD_LINE_OTYPE_GENERAL },

    /* Set a hostfile */
    { NULL, '\0', "hostfile", "hostfile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile", OPAL_CMD_LINE_OTYPE_LAUNCH },
    { NULL, '\0', "machinefile", "machinefile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile", OPAL_CMD_LINE_OTYPE_LAUNCH },
    { "orte_default_hostfile", '\0', "default-hostfile", "default-hostfile", 1,
        NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a default hostfile", OPAL_CMD_LINE_OTYPE_LAUNCH },
    { "opal_if_do_not_resolve", '\0', "do-not-resolve", "do-not-resolve", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not attempt to resolve interfaces", OPAL_CMD_LINE_OTYPE_DEVEL },

    { "orte_rankfile", '\0', "rf", "rankfile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a rankfile file", OPAL_CMD_LINE_OTYPE_MAPPING },

    /* Export environment variables; potentially used multiple times,
       so it does not make sense to set into a variable */
    { NULL, 'x', NULL, NULL, 1,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      "Export an environment variable, optionally specifying a value (e.g., \"-x foo\" exports the environment variable foo and takes its value from the current environment; \"-x foo=bar\" exports the environment variable name foo and sets its value to \"bar\" in the started processes)", OPAL_CMD_LINE_OTYPE_LAUNCH },

      /* Mapping controls */
    { "rmaps_base_display_map", '\0', "display-map", "display-map", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Display the process map just before launch", OPAL_CMD_LINE_OTYPE_DEBUG },
    { "rmaps_base_display_devel_map", '\0', "display-devel-map", "display-devel-map", 0,
       NULL, OPAL_CMD_LINE_TYPE_BOOL,
       "Display a detailed process map (mostly intended for developers) just before launch",
       OPAL_CMD_LINE_OTYPE_DEVEL },
    { "rmaps_base_display_topo_with_map", '\0', "display-topo", "display-topo", 0,
       NULL, OPAL_CMD_LINE_TYPE_BOOL,
       "Display the topology as part of the process map (mostly intended for developers) just before launch",
       OPAL_CMD_LINE_OTYPE_DEVEL },
    { "rmaps_base_display_diffable_map", '\0', "display-diffable-map", "display-diffable-map", 0,
       NULL, OPAL_CMD_LINE_TYPE_BOOL,
       "Display a diffable process map (mostly intended for developers) just before launch",
       OPAL_CMD_LINE_OTYPE_DEVEL },
    { NULL, 'H', "host", "host", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "List of hosts to invoke processes on",
      OPAL_CMD_LINE_OTYPE_MAPPING },
    { "rmaps_base_no_schedule_local", '\0', "nolocal", "nolocal", 0,
      &orte_cmd_options.nolocal, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not run any MPI applications on the local node",
      OPAL_CMD_LINE_OTYPE_MAPPING },
    { "rmaps_base_no_oversubscribe", '\0', "nooversubscribe", "nooversubscribe", 0,
      &orte_cmd_options.no_oversubscribe, OPAL_CMD_LINE_TYPE_BOOL,
      "Nodes are not to be oversubscribed, even if the system supports such operation",
      OPAL_CMD_LINE_OTYPE_MAPPING },
    { "rmaps_base_oversubscribe", '\0', "oversubscribe", "oversubscribe", 0,
      &orte_cmd_options.oversubscribe, OPAL_CMD_LINE_TYPE_BOOL,
      "Nodes are allowed to be oversubscribed, even on a managed system, and overloading of processing elements",
      OPAL_CMD_LINE_OTYPE_MAPPING },
    { "rmaps_base_cpus_per_rank", '\0', "cpus-per-proc", "cpus-per-proc", 1,
      &orte_cmd_options.cpus_per_proc, OPAL_CMD_LINE_TYPE_INT,
      "Number of cpus to use for each process [default=1]",
      OPAL_CMD_LINE_OTYPE_MAPPING },
    { "rmaps_base_cpus_per_rank", '\0', "cpus-per-rank", "cpus-per-rank", 1,
      &orte_cmd_options.cpus_per_proc, OPAL_CMD_LINE_TYPE_INT,
      "Synonym for cpus-per-proc", OPAL_CMD_LINE_OTYPE_MAPPING },

    /* backward compatiblity */
    { "rmaps_base_bycore", '\0', "bycore", "bycore", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to map and rank processes round-robin by core",
      OPAL_CMD_LINE_OTYPE_COMPAT },
    { "rmaps_base_bynode", '\0', "bynode", "bynode", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to map and rank processes round-robin by node",
      OPAL_CMD_LINE_OTYPE_COMPAT },
    { "rmaps_base_byslot", '\0', "byslot", "byslot", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to map and rank processes round-robin by slot",
      OPAL_CMD_LINE_OTYPE_COMPAT },

    /* Nperxxx options that do not require topology and are always
     * available - included for backwards compatibility
     */
    { "rmaps_ppr_pernode", '\0', "pernode", "pernode", 0,
      &orte_cmd_options.pernode, OPAL_CMD_LINE_TYPE_BOOL,
      "Launch one process per available node",
      OPAL_CMD_LINE_OTYPE_COMPAT },
    { "rmaps_ppr_n_pernode", '\0', "npernode", "npernode", 1,
      &orte_cmd_options.npernode, OPAL_CMD_LINE_TYPE_INT,
      "Launch n processes per node on all allocated nodes",
      OPAL_CMD_LINE_OTYPE_COMPAT },
    { "rmaps_ppr_n_pernode", '\0', "N", NULL, 1,
      &orte_cmd_options.npernode, OPAL_CMD_LINE_TYPE_INT,
      "Launch n processes per node on all allocated nodes (synonym for 'map-by node')",
      OPAL_CMD_LINE_OTYPE_MAPPING },

    /* declare hardware threads as independent cpus */
    { "hwloc_base_use_hwthreads_as_cpus", '\0', "use-hwthread-cpus", "use-hwthread-cpus", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Use hardware threads as independent cpus", OPAL_CMD_LINE_OTYPE_MAPPING },

    /* include npersocket for backwards compatibility */
    { "rmaps_ppr_n_persocket", '\0', "npersocket", "npersocket", 1,
      &orte_cmd_options.npersocket, OPAL_CMD_LINE_TYPE_INT,
      "Launch n processes per socket on all allocated nodes",
      OPAL_CMD_LINE_OTYPE_COMPAT },

    /* Mapping options */
    { "rmaps_base_mapping_policy", '\0', NULL, "map-by", 1,
      &orte_cmd_options.mapping_policy, OPAL_CMD_LINE_TYPE_STRING,
      "Mapping Policy [slot | hwthread | core | socket (default) | numa | board | node]",
      OPAL_CMD_LINE_OTYPE_MAPPING },

      /* Ranking options */
    { "rmaps_base_ranking_policy", '\0', NULL, "rank-by", 1,
      &orte_cmd_options.ranking_policy, OPAL_CMD_LINE_TYPE_STRING,
      "Ranking Policy [slot (default) | hwthread | core | socket | numa | board | node]",
      OPAL_CMD_LINE_OTYPE_RANKING },

      /* Binding options */
    { "hwloc_base_binding_policy", '\0', NULL, "bind-to", 1,
      &orte_cmd_options.binding_policy, OPAL_CMD_LINE_TYPE_STRING,
      "Policy for binding processes. Allowed values: none, hwthread, core, l1cache, l2cache, l3cache, socket, numa, board (\"none\" is the default when oversubscribed, \"core\" is the default when np<=2, and \"socket\" is the default when np>2). Allowed qualifiers: overload-allowed, if-supported", OPAL_CMD_LINE_OTYPE_BINDING },

    /* backward compatiblity */
    { "hwloc_base_bind_to_core", '\0', "bind-to-core", "bind-to-core", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Bind processes to cores", OPAL_CMD_LINE_OTYPE_COMPAT },
    { "hwloc_base_bind_to_socket", '\0', "bind-to-socket", "bind-to-socket", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Bind processes to sockets", OPAL_CMD_LINE_OTYPE_COMPAT },

    { "hwloc_base_report_bindings", '\0', "report-bindings", "report-bindings", 0,
      &orte_cmd_options.report_bindings, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to report process bindings to stderr",
      OPAL_CMD_LINE_OTYPE_BINDING },

    /* slot list option */
    { "hwloc_base_cpu_list", '\0', "cpu-list", "cpu-list", 1,
      &orte_cmd_options.cpu_list, OPAL_CMD_LINE_TYPE_STRING,
      "List of processor IDs to bind processes to [default=NULL]",
      OPAL_CMD_LINE_OTYPE_BINDING },

    /* generalized pattern mapping option */
    { "rmaps_ppr_pattern", '\0', NULL, "ppr", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Comma-separated list of number of processes on a given resource type [default: none]",
      OPAL_CMD_LINE_OTYPE_MAPPING },

    /* Allocation options */
    { "orte_display_alloc", '\0', "display-allocation", "display-allocation", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Display the allocation being used by this job", OPAL_CMD_LINE_OTYPE_DEBUG },
    { "orte_display_devel_alloc", '\0', "display-devel-allocation", "display-devel-allocation", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Display a detailed list (mostly intended for developers) of the allocation being used by this job",
      OPAL_CMD_LINE_OTYPE_DEVEL },
    { "hwloc_base_cpu_set", '\0', "cpu-set", "cpu-set", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Comma-separated list of ranges specifying logical cpus allocated to this job [default: none]",
      OPAL_CMD_LINE_OTYPE_DEBUG },

    /* mpiexec-like arguments */
    { NULL, '\0', "wdir", "wdir", 1,
      &orte_cmd_options.wdir, OPAL_CMD_LINE_TYPE_STRING,
      "Set the working directory of the started processes",
      OPAL_CMD_LINE_OTYPE_LAUNCH },
    { NULL, '\0', "wd", "wd", 1,
      &orte_cmd_options.wdir, OPAL_CMD_LINE_TYPE_STRING,
      "Synonym for --wdir", OPAL_CMD_LINE_OTYPE_LAUNCH },
    { NULL, '\0', "set-cwd-to-session-dir", "set-cwd-to-session-dir", 0,
      &orte_cmd_options.set_cwd_to_session_dir, OPAL_CMD_LINE_TYPE_BOOL,
      "Set the working directory of the started processes to their session directory",
      OPAL_CMD_LINE_OTYPE_LAUNCH },
    { NULL, '\0', "path", "path", 1,
      &orte_cmd_options.path, OPAL_CMD_LINE_TYPE_STRING,
      "PATH to be used to look for executables to start processes",
      OPAL_CMD_LINE_OTYPE_LAUNCH },

    /* User-level debugger arguments */
    { NULL, '\0', "tv", "tv", 0,
      &orte_cmd_options.debugger, OPAL_CMD_LINE_TYPE_BOOL,
      "Deprecated backwards compatibility flag; synonym for \"--debug\"",
      OPAL_CMD_LINE_OTYPE_DEBUG },
    { NULL, '\0', "debug", "debug", 0,
      &orte_cmd_options.debugger, OPAL_CMD_LINE_TYPE_BOOL,
      "Invoke the user-level debugger indicated by the orte_base_user_debugger MCA parameter",
      OPAL_CMD_LINE_OTYPE_DEBUG },
    { "orte_base_user_debugger", '\0', "debugger", "debugger", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Sequence of debuggers to search for when \"--debug\" is used",
      OPAL_CMD_LINE_OTYPE_DEBUG },
    { "orte_output_debugger_proctable", '\0', "output-proctable", "output-proctable", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Output the debugger proctable after launch",
      OPAL_CMD_LINE_OTYPE_DEBUG },

    { "orte_report_events", '\0', "report-events", "report-events", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Report events to a tool listening at the specified URI", OPAL_CMD_LINE_OTYPE_DEBUG },

    { "orte_enable_recovery", '\0', "enable-recovery", "enable-recovery", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable recovery from process failure [Default = disabled]",
      OPAL_CMD_LINE_OTYPE_UNSUPPORTED },

    { "orte_max_restarts", '\0', "max-restarts", "max-restarts", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Max number of times to restart a failed process",
      OPAL_CMD_LINE_OTYPE_UNSUPPORTED },

    { NULL, '\0', "continuous", "continuous", 0,
      &orte_cmd_options.continuous, OPAL_CMD_LINE_TYPE_BOOL,
      "Job is to run until explicitly terminated", OPAL_CMD_LINE_OTYPE_DEBUG },

    { NULL, '\0', "disable-recovery", "disable-recovery", 0,
      &orte_cmd_options.disable_recovery, OPAL_CMD_LINE_TYPE_BOOL,
      "Disable recovery (resets all recovery options to off)",
      OPAL_CMD_LINE_OTYPE_UNSUPPORTED },

    { NULL, '\0', "personality", "personality", 1,
      &orte_cmd_options.personality, OPAL_CMD_LINE_TYPE_STRING,
      "Comma-separated list of programming model, languages, and containers being used (default=\"ompi\")",
      OPAL_CMD_LINE_OTYPE_LAUNCH },

    /* tell the dvm to terminate */
    { NULL, '\0', "terminate", "terminate", 0,
      &orte_cmd_options.terminate_dvm, OPAL_CMD_LINE_TYPE_BOOL,
      "Terminate the DVM", OPAL_CMD_LINE_OTYPE_DVM },

    /* look first for a system server */
    { NULL, '\0', "system-server-first", "system-server-first", 0,
      &orte_cmd_options.system_server_first, OPAL_CMD_LINE_TYPE_BOOL,
      "First look for a system server and connect to it if found", OPAL_CMD_LINE_OTYPE_DVM },

    /* connect only to a system server */
    { NULL, '\0', "system-server-only", "system-server-only", 0,
      &orte_cmd_options.system_server_only, OPAL_CMD_LINE_TYPE_BOOL,
      "Connect only to a system-level server", OPAL_CMD_LINE_OTYPE_DVM },

    /* provide a connection PID */
    { NULL, '\0', "pid", "pid", 1,
      &orte_cmd_options.pid, OPAL_CMD_LINE_TYPE_INT,
      "PID of the session-level daemon to which we should connect",
      OPAL_CMD_LINE_OTYPE_DVM },

    /* End of list */
    { NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};


static void infocb(int status,
                   opal_list_t *info,
                   void *cbdata,
                   opal_pmix_release_cbfunc_t release_fn,
                   void *release_cbdata)
{
    opal_pmix_lock_t *lock = (opal_pmix_lock_t*)cbdata;
    OPAL_ACQUIRE_OBJECT(lock);

    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
    OPAL_PMIX_WAKEUP_THREAD(lock);
}

static void regcbfunc(int status, size_t ref, void *cbdata)
{
    opal_pmix_lock_t *lock = (opal_pmix_lock_t*)cbdata;
    OPAL_ACQUIRE_OBJECT(lock);
    OPAL_PMIX_WAKEUP_THREAD(lock);
}

static void release(int sd, short args, void *cbdata)
{
    active = false;
}

static bool fired = false;
static void evhandler(int status,
                      const opal_process_name_t *source,
                      opal_list_t *info, opal_list_t *results,
                      opal_pmix_notification_complete_fn_t cbfunc,
                      void *cbdata)
{
    opal_value_t *val;

    if (NULL != info) {
        OPAL_LIST_FOREACH(val, info, opal_value_t) {
            if (0 == strcmp(val->key, OPAL_PMIX_JOB_TERM_STATUS)) {
                opal_output(0, "JOB COMPLETED WITH STATUS %d",
                            val->data.integer);
            }
        }
    }
    if (NULL != cbfunc) {
        cbfunc(OPAL_SUCCESS, NULL, NULL, NULL, cbdata);
    }
    if (!fired) {
        fired = true;
        ORTE_ACTIVATE_PROC_STATE(ORTE_PROC_MY_NAME, ORTE_PROC_STATE_TERMINATED);
    }
}


int prun(int argc, char *argv[])
{
    int rc, i;
    char *param;
    opal_pmix_lock_t lock;
    opal_list_t apps;
    opal_value_t *val;
    opal_list_t info;
    opal_jobid_t jobid;
    struct timespec tp = {0, 100000};

    /* init the globals */
    memset(&orte_cmd_options, 0, sizeof(orte_cmd_options));
    OBJ_CONSTRUCT(&job_info, opal_list_t);
    OBJ_CONSTRUCT(&apps, opal_list_t);

    /* search the argv for MCA params */
    for (i=0; NULL != argv[i]; i++) {
        if (':' == argv[i][0] ||
            NULL == argv[i+1] || NULL == argv[i+2]) {
            break;
        }
        if (0 == strncmp(argv[i], "-"OPAL_MCA_CMD_LINE_ID, strlen("-"OPAL_MCA_CMD_LINE_ID)) ||
            0 == strncmp(argv[i], "--"OPAL_MCA_CMD_LINE_ID, strlen("--"OPAL_MCA_CMD_LINE_ID)) ||
            0 == strncmp(argv[i], "-g"OPAL_MCA_CMD_LINE_ID, strlen("-g"OPAL_MCA_CMD_LINE_ID)) ||
            0 == strncmp(argv[i], "--g"OPAL_MCA_CMD_LINE_ID, strlen("--g"OPAL_MCA_CMD_LINE_ID))) {
            (void) mca_base_var_env_name (argv[i+1], &param);
            opal_setenv(param, argv[i+2], true, &environ);
            free(param);
        } else if (0 == strcmp(argv[i], "-am") ||
                   0 == strcmp(argv[i], "--am")) {
            (void)mca_base_var_env_name("mca_base_param_file_prefix", &param);
            opal_setenv(param, argv[i+1], true, &environ);
            free(param);
        } else if (0 == strcmp(argv[i], "-tune") ||
                   0 == strcmp(argv[i], "--tune")) {
            (void)mca_base_var_env_name("mca_base_envar_file_prefix", &param);
            opal_setenv(param, argv[i+1], true, &environ);
            free(param);
        }
    }

    /* init only the util portion of OPAL */
    if (OPAL_SUCCESS != (rc = opal_init_util(&argc, &argv))) {
        return rc;
    }

    /* setup our cmd line */
    orte_cmd_line = OBJ_NEW(opal_cmd_line_t);
    if (OPAL_SUCCESS != (rc = opal_cmd_line_add(orte_cmd_line, cmd_line_init))) {
        return rc;
    }

    /* now that options have been defined, finish setup */
    mca_base_cmd_line_setup(orte_cmd_line);

    /* parse the result to get values */
    if (OPAL_SUCCESS != (rc = opal_cmd_line_parse(orte_cmd_line,
                                                  true, false, argc, argv)) ) {
        if (OPAL_ERR_SILENT != rc) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0],
                    opal_strerror(rc));
        }
        return rc;
    }

    /* see if print version is requested. Do this before
     * check for help so that --version --help works as
     * one might expect. */
     if (orte_cmd_options.version) {
        char *str;
        str = opal_info_make_version_str("all",
                                         OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                         OPAL_RELEASE_VERSION,
                                         OPAL_GREEK_VERSION,
                                         OPAL_REPO_REV);
        if (NULL != str) {
            fprintf(stdout, "%s (%s) %s\n\nReport bugs to %s\n",
                    "prun", "PMIx Reference Server", str, PACKAGE_BUGREPORT);
            free(str);
        }
        exit(0);
    }

    /* check if we are running as root - if we are, then only allow
     * us to proceed if the allow-run-as-root flag was given. Otherwise,
     * exit with a giant warning flag
     */
    if (0 == geteuid() && !orte_cmd_options.run_as_root) {
        /* show_help is not yet available, so print an error manually */
        fprintf(stderr, "--------------------------------------------------------------------------\n");
        if (orte_cmd_options.help) {
            fprintf(stderr, "prun cannot provide the help message when run as root.\n\n");
        } else {
            fprintf(stderr, "prun has detected an attempt to run as root.\n\n");
        }

        fprintf(stderr, "Running as root is *strongly* discouraged as any mistake (e.g., in\n");
        fprintf(stderr, "defining TMPDIR) or bug can result in catastrophic damage to the OS\n");
        fprintf(stderr, "file system, leaving your system in an unusable state.\n\n");

        fprintf(stderr, "We strongly suggest that you run prun as a non-root user.\n\n");

        fprintf(stderr, "You can override this protection by adding the --allow-run-as-root\n");
        fprintf(stderr, "option to your command line.  However, we reiterate our strong advice\n");
        fprintf(stderr, "against doing so - please do so at your own risk.\n");
        fprintf(stderr, "--------------------------------------------------------------------------\n");
        exit(1);
    }

    /* process any mca params */
    rc = mca_base_cmd_line_process_args(orte_cmd_line, &environ, &environ);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    /* Check for help request */
    if (orte_cmd_options.help) {
        char *str, *args = NULL;
        args = opal_cmd_line_get_usage_msg(orte_cmd_line);
        str = opal_show_help_string("help-orterun.txt", "orterun:usage", false,
                                    "prun", "PSVR", OPAL_VERSION,
                                    "prun", args,
                                    PACKAGE_BUGREPORT);
        if (NULL != str) {
            printf("%s", str);
            free(str);
        }
        free(args);

        /* If someone asks for help, that should be all we do */
        exit(0);
    }

    /* tell the ess/tool component that we want to connect only to a system-level
     * PMIx server */
    if (orte_cmd_options.system_server_only) {
        opal_setenv(OPAL_MCA_PREFIX"ess_tool_system_server_only", "1", true, &environ);
    }
    if (orte_cmd_options.system_server_first) {
        opal_setenv(OPAL_MCA_PREFIX"ess_tool_system_server_first", "1", true, &environ);
    }
    /* if they specified the DVM's pid, then pass it along */
    if (0 != orte_cmd_options.pid) {
        asprintf(&param, "%d", orte_cmd_options.pid);
        opal_setenv(OPAL_MCA_PREFIX"ess_tool_server_pid", param, true, &environ);
        free(param);
    }

    /* now initialize ORTE */
    if (OPAL_SUCCESS != (rc = orte_init(&argc, &argv, ORTE_PROC_TOOL))) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    /* if the user just wants us to terminate a DVM, then do so */
    if (orte_cmd_options.terminate_dvm) {
        OBJ_CONSTRUCT(&info, opal_list_t);
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_JOB_CTRL_TERMINATE);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(&info, &val->super);
        fprintf(stderr, "TERMINATING DVM...");
        OPAL_PMIX_CONSTRUCT_LOCK(&lock);
        rc = opal_pmix.job_control(NULL, &info, infocb, (void*)&lock);
        OPAL_PMIX_WAIT_THREAD(&lock);
        OPAL_PMIX_DESTRUCT_LOCK(&lock);
        OPAL_LIST_DESTRUCT(&info);
        fprintf(stderr, "DONE\n");
        goto DONE;
    }

    orte_state.add_proc_state(ORTE_PROC_STATE_TERMINATED, release, ORTE_SYS_PRI);

    /* get here if they want to run an application, so let's parse
     * the cmd line to get it */

    if (OPAL_SUCCESS != (rc = parse_locals(&apps, argc, argv))) {
        OPAL_ERROR_LOG(rc);
        OPAL_LIST_DESTRUCT(&apps);
        goto DONE;
    }

    /* bozo check */
    if (0 == opal_list_get_size(&apps)) {
        opal_output(0, "No application specified!");
        goto DONE;
    }

    /* init flag */
    active = true;

    /* register for job terminations so we get notified when
     * our job completes */
    OPAL_PMIX_CONSTRUCT_LOCK(&lock);
    OBJ_CONSTRUCT(&info, opal_list_t);
    val = OBJ_NEW(opal_value_t);
    val->key = strdup("foo");
    val->type = OPAL_INT;
    val->data.integer = OPAL_ERR_JOB_TERMINATED;
    opal_list_append(&info, &val->super);
    opal_pmix.register_evhandler(&info, NULL, evhandler, regcbfunc, &lock);
    OPAL_PMIX_WAIT_THREAD(&lock);
    OPAL_PMIX_DESTRUCT_LOCK(&lock);
    OPAL_LIST_DESTRUCT(&info);

    if (OPAL_SUCCESS != (rc = opal_pmix.spawn(&job_info, &apps, &jobid))) {
        opal_output(0, "Job failed to spawn: %s", opal_strerror(rc));
        goto DONE;
    }
    OPAL_LIST_DESTRUCT(&job_info);
    OPAL_LIST_DESTRUCT(&apps);

    opal_output(0, "JOB %s EXECUTING", OPAL_JOBID_PRINT(jobid));

    while (active) {
        nanosleep(&tp, NULL);
    }

  DONE:
    /* cleanup and leave */
    orte_finalize();
    return 0;
}

static int parse_locals(opal_list_t *jdata, int argc, char* argv[])
{
    int i, rc;
    int temp_argc;
    char **temp_argv, **env;
    opal_pmix_app_t *app;
    bool made_app;

    /* Make the apps */
    temp_argc = 0;
    temp_argv = NULL;
    opal_argv_append(&temp_argc, &temp_argv, argv[0]);

    /* NOTE: This bogus env variable is necessary in the calls to
       create_app(), below.  See comment immediately before the
       create_app() function for an explanation. */

    env = NULL;
    for (i = 1; i < argc; ++i) {
        if (0 == strcmp(argv[i], ":")) {
            /* Make an app with this argv */
            if (opal_argv_count(temp_argv) > 1) {
                if (NULL != env) {
                    opal_argv_free(env);
                    env = NULL;
                }
                app = NULL;
                rc = create_app(temp_argc, temp_argv, jdata, &app, &made_app, &env);
                if (OPAL_SUCCESS != rc) {
                    /* Assume that the error message has already been
                       printed; no need to cleanup -- we can just
                       exit */
                    exit(1);
                }
                if (made_app) {
                    opal_list_append(jdata, &app->super);
                }

                /* Reset the temps */

                temp_argc = 0;
                temp_argv = NULL;
                opal_argv_append(&temp_argc, &temp_argv, argv[0]);
            }
        } else {
            opal_argv_append(&temp_argc, &temp_argv, argv[i]);
        }
    }

    if (opal_argv_count(temp_argv) > 1) {
        app = NULL;
        rc = create_app(temp_argc, temp_argv, jdata, &app, &made_app, &env);
        if (ORTE_SUCCESS != rc) {
            /* Assume that the error message has already been printed;
               no need to cleanup -- we can just exit */
            exit(1);
        }
        if (made_app) {
            opal_list_append(jdata, &app->super);
        }
    }
    if (NULL != env) {
        opal_argv_free(env);
    }
    opal_argv_free(temp_argv);

    /* All done */

    return ORTE_SUCCESS;
}


/*
 * This function takes a "char ***app_env" parameter to handle the
 * specific case:
 *
 *   orterun --mca foo bar -app appfile
 *
 * That is, we'll need to keep foo=bar, but the presence of the app
 * file will cause an invocation of parse_appfile(), which will cause
 * one or more recursive calls back to create_app().  Since the
 * foo=bar value applies globally to all apps in the appfile, we need
 * to pass in the "base" environment (that contains the foo=bar value)
 * when we parse each line in the appfile.
 *
 * This is really just a special case -- when we have a simple case like:
 *
 *   orterun --mca foo bar -np 4 hostname
 *
 * Then the upper-level function (parse_locals()) calls create_app()
 * with a NULL value for app_env, meaning that there is no "base"
 * environment that the app needs to be created from.
 */
static int create_app(int argc, char* argv[],
                      opal_list_t *jdata,
                      opal_pmix_app_t **app_ptr,
                      bool *made_app, char ***app_env)
{
    char cwd[OPAL_PATH_MAX];
    int i, j, count, rc;
    char *param, *value;
    opal_pmix_app_t *app = NULL;
    bool found = false;
    char *appname = NULL;
    opal_value_t *val;

    *made_app = false;

    /* parse the cmd line - do this every time thru so we can
     * repopulate the globals */
    if (OPAL_SUCCESS != (rc = opal_cmd_line_parse(orte_cmd_line, true, false,
                                                  argc, argv)) ) {
        if (OPAL_ERR_SILENT != rc) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0],
                    opal_strerror(rc));
        }
        return rc;
    }

    /* Setup application context */
    app = OBJ_NEW(opal_pmix_app_t);
    opal_cmd_line_get_tail(orte_cmd_line, &count, &app->argv);

    /* See if we have anything left */
    if (0 == count) {
        opal_show_help("help-orterun.txt", "orterun:executable-not-specified",
                       true, "prun", "prun");
        rc = OPAL_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* Grab all MCA environment variables */
    app->env = opal_argv_copy(*app_env);
    for (i=0; NULL != environ[i]; i++) {
        if (0 == strncmp("PMIX_", environ[i], 5)) {
            /* check for duplicate in app->env - this
             * would have been placed there by the
             * cmd line processor. By convention, we
             * always let the cmd line override the
             * environment
             */
            param = strdup(environ[i]);
            value = strchr(param, '=');
            *value = '\0';
            value++;
            opal_setenv(param, value, false, &app->env);
            free(param);
        }
    }

    /* Did the user request a specific wdir? */

    if (NULL != orte_cmd_options.wdir) {
        /* if this is a relative path, convert it to an absolute path */
        if (opal_path_is_absolute(orte_cmd_options.wdir)) {
            app->cwd = strdup(orte_cmd_options.wdir);
        } else {
            /* get the cwd */
            if (OPAL_SUCCESS != (rc = opal_getcwd(cwd, sizeof(cwd)))) {
                opal_show_help("help-orterun.txt", "orterun:init-failure",
                               true, "get the cwd", rc);
                goto cleanup;
            }
            /* construct the absolute path */
            app->cwd = opal_os_path(false, cwd, orte_cmd_options.wdir, NULL);
        }
    } else if (orte_cmd_options.set_cwd_to_session_dir) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_SET_SESSION_CWD);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(&job_info, &val->super);
    } else {
        if (OPAL_SUCCESS != (rc = opal_getcwd(cwd, sizeof(cwd)))) {
            opal_show_help("help-orterun.txt", "orterun:init-failure",
                           true, "get the cwd", rc);
            goto cleanup;
        }
        app->cwd = strdup(cwd);
    }

    /* Did the user specify a hostfile. Need to check for both
     * hostfile and machine file.
     * We can only deal with one hostfile per app context, otherwise give an error.
     */
    found = false;
    if (0 < (j = opal_cmd_line_get_ninsts(orte_cmd_line, "hostfile"))) {
        if (1 < j) {
            opal_show_help("help-orterun.txt", "orterun:multiple-hostfiles",
                           true, "prun", NULL);
            return ORTE_ERR_FATAL;
        } else {
            value = opal_cmd_line_get_param(orte_cmd_line, "hostfile", 0, 0);
            val = OBJ_NEW(opal_value_t);
            val->key = strdup(OPAL_PMIX_HOSTFILE);
            val->type = OPAL_STRING;
            val->data.string = value;
            opal_list_append(&job_info, &val->super);
            found = true;
        }
    }
    if (0 < (j = opal_cmd_line_get_ninsts(orte_cmd_line, "machinefile"))) {
        if (1 < j || found) {
            opal_show_help("help-orterun.txt", "orterun:multiple-hostfiles",
                           true, "prun", NULL);
            return ORTE_ERR_FATAL;
        } else {
            value = opal_cmd_line_get_param(orte_cmd_line, "machinefile", 0, 0);
            val = OBJ_NEW(opal_value_t);
            val->key = strdup(OPAL_PMIX_HOSTFILE);
            val->type = OPAL_STRING;
            val->data.string = value;
            opal_list_append(&job_info, &val->super);
        }
    }

    /* Did the user specify any hosts? */
    if (0 < (j = opal_cmd_line_get_ninsts(orte_cmd_line, "host"))) {
        char **targ=NULL, *tval;
        for (i = 0; i < j; ++i) {
            value = opal_cmd_line_get_param(orte_cmd_line, "host", i, 0);
            opal_argv_append_nosize(&targ, value);
        }
        tval = opal_argv_join(targ, ',');
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_HOST);
        val->type = OPAL_STRING;
        val->data.string = tval;
        opal_list_append(&job_info, &val->super);
    }

    /* check for bozo error */
    if (0 > orte_cmd_options.num_procs) {
        opal_show_help("help-orterun.txt", "orterun:negative-nprocs",
                       true, "prun", app->argv[0],
                       orte_cmd_options.num_procs, NULL);
        return ORTE_ERR_FATAL;
    }

    app->maxprocs = orte_cmd_options.num_procs;

    /* see if we need to preload the binary to
     * find the app - don't do this for java apps, however, as we
     * can't easily find the class on the cmd line. Java apps have to
     * preload their binary via the preload_files option
     */
    if (NULL == strstr(app->argv[0], "java")) {
        if (orte_cmd_options.preload_binaries) {
            val = OBJ_NEW(opal_value_t);
            val->key = strdup(OPAL_PMIX_SET_SESSION_CWD);
            val->type = OPAL_BOOL;
            val->data.flag = true;
            opal_list_append(&job_info, &val->super);
            val = OBJ_NEW(opal_value_t);
            val->key = strdup(OPAL_PMIX_PRELOAD_BIN);
            val->type = OPAL_BOOL;
            val->data.flag = true;
            opal_list_append(&job_info, &val->super);
        }
    }
    if (NULL != orte_cmd_options.preload_files) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_PRELOAD_FILES);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(&job_info, &val->super);
    }

    /* Do not try to find argv[0] here -- the starter is responsible
       for that because it may not be relevant to try to find it on
       the node where orterun is executing.  So just strdup() argv[0]
       into app. */

    app->cmd = strdup(app->argv[0]);
    if (NULL == app->cmd) {
        opal_show_help("help-orterun.txt", "orterun:call-failed",
                       true, "prun", "library", "strdup returned NULL", errno);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* if this is a Java application, we have a bit more work to do. Such
     * applications actually need to be run under the Java virtual machine
     * and the "java" command will start the "executable". So we need to ensure
     * that all the proper java-specific paths are provided
     */
    appname = opal_basename(app->cmd);
    if (0 == strcmp(appname, "java")) {
        /* see if we were given a library path */
        found = false;
        for (i=1; NULL != app->argv[i]; i++) {
            if (NULL != strstr(app->argv[i], "java.library.path")) {
                char *dptr;
                /* find the '=' that delineates the option from the path */
                if (NULL == (dptr = strchr(app->argv[i], '='))) {
                    /* that's just wrong */
                    rc = ORTE_ERR_BAD_PARAM;
                    goto cleanup;
                }
                /* step over the '=' */
                ++dptr;
                /* yep - but does it include the path to the mpi libs? */
                found = true;
                if (NULL == strstr(app->argv[i], opal_install_dirs.libdir)) {
                    /* doesn't appear to - add it to be safe */
                    if (':' == app->argv[i][strlen(app->argv[i]-1)]) {
                        asprintf(&value, "-Djava.library.path=%s%s", dptr, opal_install_dirs.libdir);
                    } else {
                        asprintf(&value, "-Djava.library.path=%s:%s", dptr, opal_install_dirs.libdir);
                    }
                    free(app->argv[i]);
                    app->argv[i] = value;
                }
                break;
            }
        }
        if (!found) {
            /* need to add it right after the java command */
            asprintf(&value, "-Djava.library.path=%s", opal_install_dirs.libdir);
            opal_argv_insert_element(&app->argv, 1, value);
            free(value);
        }

        /* see if we were given a class path */
        found = false;
        for (i=1; NULL != app->argv[i]; i++) {
            if (NULL != strstr(app->argv[i], "cp") ||
                NULL != strstr(app->argv[i], "classpath")) {
                /* yep - but does it include the path to the mpi libs? */
                found = true;
                /* check if mpi.jar exists - if so, add it */
                value = opal_os_path(false, opal_install_dirs.libdir, "mpi.jar", NULL);
                if (access(value, F_OK ) != -1) {
                    set_classpath_jar_file(app, i+1, "mpi.jar");
                }
                free(value);
                /* check for oshmem support */
                value = opal_os_path(false, opal_install_dirs.libdir, "shmem.jar", NULL);
                if (access(value, F_OK ) != -1) {
                    set_classpath_jar_file(app, i+1, "shmem.jar");
                }
                free(value);
                /* always add the local directory */
                asprintf(&value, "%s:%s", app->cwd, app->argv[i+1]);
                free(app->argv[i+1]);
                app->argv[i+1] = value;
                break;
            }
        }
        if (!found) {
            /* check to see if CLASSPATH is in the environment */
            found = false;  // just to be pedantic
            for (i=0; NULL != environ[i]; i++) {
                if (0 == strncmp(environ[i], "CLASSPATH", strlen("CLASSPATH"))) {
                    value = strchr(environ[i], '=');
                    ++value; /* step over the = */
                    opal_argv_insert_element(&app->argv, 1, value);
                    /* check for mpi.jar */
                    value = opal_os_path(false, opal_install_dirs.libdir, "mpi.jar", NULL);
                    if (access(value, F_OK ) != -1) {
                        set_classpath_jar_file(app, 1, "mpi.jar");
                    }
                    free(value);
                    /* check for shmem.jar */
                    value = opal_os_path(false, opal_install_dirs.libdir, "shmem.jar", NULL);
                    if (access(value, F_OK ) != -1) {
                        set_classpath_jar_file(app, 1, "shmem.jar");
                    }
                    free(value);
                    /* always add the local directory */
                    (void)asprintf(&value, "%s:%s", app->cwd, app->argv[1]);
                    free(app->argv[1]);
                    app->argv[1] = value;
                    opal_argv_insert_element(&app->argv, 1, "-cp");
                    found = true;
                    break;
                }
            }
            if (!found) {
                /* need to add it right after the java command - have
                 * to include the working directory and trust that
                 * the user set cwd if necessary
                 */
                char *str, *str2;
                /* always start with the working directory */
                str = strdup(app->cwd);
                /* check for mpi.jar */
                value = opal_os_path(false, opal_install_dirs.libdir, "mpi.jar", NULL);
                if (access(value, F_OK ) != -1) {
                    (void)asprintf(&str2, "%s:%s", str, value);
                    free(str);
                    str = str2;
                }
                free(value);
                /* check for shmem.jar */
                value = opal_os_path(false, opal_install_dirs.libdir, "shmem.jar", NULL);
                if (access(value, F_OK ) != -1) {
                    asprintf(&str2, "%s:%s", str, value);
                    free(str);
                    str = str2;
                }
                free(value);
                opal_argv_insert_element(&app->argv, 1, str);
                free(str);
                opal_argv_insert_element(&app->argv, 1, "-cp");
            }
        }
        /* try to find the actual command - may not be perfect */
        for (i=1; i < opal_argv_count(app->argv); i++) {
            if (NULL != strstr(app->argv[i], "java.library.path")) {
                continue;
            } else if (NULL != strstr(app->argv[i], "cp") ||
                       NULL != strstr(app->argv[i], "classpath")) {
                /* skip the next field */
                i++;
                continue;
            }
            /* declare this the winner */
            opal_setenv("OMPI_COMMAND", app->argv[i], true, &app->env);
            /* collect everything else as the cmd line */
            if ((i+1) < opal_argv_count(app->argv)) {
                value = opal_argv_join(&app->argv[i+1], ' ');
                opal_setenv("OMPI_ARGV", value, true, &app->env);
                free(value);
            }
            break;
        }
    } else {
        /* add the cmd to the environment for MPI_Info to pickup */
        opal_setenv("OMPI_COMMAND", appname, true, &app->env);
        if (1 < opal_argv_count(app->argv)) {
            value = opal_argv_join(&app->argv[1], ' ');
            opal_setenv("OMPI_ARGV", value, true, &app->env);
            free(value);
        }
    }

    *app_ptr = app;
    app = NULL;
    *made_app = true;

    /* All done */

 cleanup:
    if (NULL != app) {
        OBJ_RELEASE(app);
    }
    if (NULL != appname) {
        free(appname);
    }
    return rc;
}

static void set_classpath_jar_file(opal_pmix_app_t *app, int index, char *jarfile)
{
    if (NULL == strstr(app->argv[index], jarfile)) {
        /* nope - need to add it */
        char *fmt = ':' == app->argv[index][strlen(app->argv[index]-1)]
                    ? "%s%s/%s" : "%s:%s/%s";
        char *str;
        asprintf(&str, fmt, app->argv[index], opal_install_dirs.libdir, jarfile);
        free(app->argv[index]);
        app->argv[index] = str;
    }
}
