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
 * Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2009 Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2007-2016 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2016 Intel, Inc. All rights reserved.
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
#include "opal/mca/hwloc/base/base.h"
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
#if OPAL_ENABLE_FT_CR == 1
#include "opal/runtime/opal_cr.h"
#endif

#include "opal/version.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_info_support.h"
#include "opal/util/os_path.h"
#include "opal/util/path.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"

#include "orte/util/proc_info.h"
#include "orte/util/pre_condition_transports.h"
#include "orte/util/session_dir.h"
#include "orte/util/hnp_contact.h"
#include "orte/util/show_help.h"

#include "orte/mca/dfs/dfs.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/ras/ras.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/schizo/schizo.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/errmgr_private.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/state/state.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_locks.h"
#include "orte/runtime/orte_quit.h"

/* ensure I can behave like a daemon */
#include "orte/orted/orted.h"
#include "orte/orted/orted_submit.h"
#include "orterun.h"

/* instance the standard MPIR interfaces */
#define MPIR_MAX_PATH_LENGTH 512
#define MPIR_MAX_ARG_LENGTH 1024
struct MPIR_PROCDESC *MPIR_proctable = NULL;
int MPIR_proctable_size = 0;
volatile int MPIR_being_debugged = 0;
volatile int MPIR_debug_state = 0;
int MPIR_i_am_starter = 0;
int MPIR_partial_attach_ok = 1;
char MPIR_executable_path[MPIR_MAX_PATH_LENGTH] = {0};
char MPIR_server_arguments[MPIR_MAX_ARG_LENGTH] = {0};
volatile int MPIR_forward_output = 0;
volatile int MPIR_forward_comm = 0;
char MPIR_attach_fifo[MPIR_MAX_PATH_LENGTH] = {0};
int MPIR_force_to_main = 0;
static void orte_debugger_dump(void);
static void orte_debugger_init_before_spawn(orte_job_t *jdata);
static void orte_debugger_init_after_spawn(int fd, short event, void *arg);
static void orte_debugger_detached(int fd, short event, void *arg);
static void attach_debugger(int fd, short event, void *arg);
static void build_debugger_args(orte_app_context_t *debugger);
static void open_fifo (void);
static int attach_fd = -1;
static bool fifo_active=false;
static opal_event_t *attach=NULL;

ORTE_DECLSPEC void* MPIR_Breakpoint(void);

static void orte_timeout_wakeup(int sd, short args, void *cbdata);

/*
 * Breakpoint function for parallel debuggers
 */
void* MPIR_Breakpoint(void)
{
    return NULL;
}

/*
 * Globals
 */
static char **global_mca_env = NULL;
static orte_std_cntr_t total_num_apps = 0;
static bool want_prefix_by_default = (bool) ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT;
static bool globals_init = false;

static opal_cmd_line_init_t cmd_line_init[] = {
    /* Various "obvious" options */
    { NULL, 'h', NULL, "help", 0,
      &orte_cmd_line.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },
    { NULL, 'V', NULL, "version", 0,
      &orte_cmd_line.version, OPAL_CMD_LINE_TYPE_BOOL,
      "Print version and exit" },
    { NULL, 'v', NULL, "verbose", 0,
      &orte_cmd_line.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Be verbose" },
    { "orte_execute_quiet", 'q', NULL, "quiet", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Suppress helpful messages" },
    { NULL, '\0', "report-pid", "report-pid", 1,
      &orte_cmd_line.report_pid, OPAL_CMD_LINE_TYPE_STRING,
      "Printout pid on stdout [-], stderr [+], or a file [anything else]" },
    { NULL, '\0', "report-uri", "report-uri", 1,
      &orte_cmd_line.report_uri, OPAL_CMD_LINE_TYPE_STRING,
      "Printout URI on stdout [-], stderr [+], or a file [anything else]" },

    /* exit status reporting */
    { "orte_report_child_jobs_separately", '\0', "report-child-jobs-separately", "report-child-jobs-separately", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Return the exit status of the primary job only" },

    /* hetero apps */
    { "orte_hetero_apps", '\0', NULL, "hetero-apps", 0,
        NULL, OPAL_CMD_LINE_TYPE_BOOL,
    "Indicates that multiple app_contexts are being provided that are a mix of 32/64 bit binaries" },

    /* select XML output */
    { "orte_xml_output", '\0', "xml", "xml", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Provide all output in XML format" },
    { "orte_xml_file", '\0', "xml-file", "xml-file", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide all output in XML format to the specified file" },

    /* tag output */
    { "orte_tag_output", '\0', "tag-output", "tag-output", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Tag all output with [job,rank]" },
    { "orte_timestamp_output", '\0', "timestamp-output", "timestamp-output", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Timestamp all application process output" },
    { "orte_output_filename", '\0', "output-filename", "output-filename", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Redirect output from application processes into filename/job/rank/std[out,err,diag]" },
    { NULL, '\0', "merge-stderr-to-stdout", "merge-stderr-to-stdout", 0,
      &orte_cmd_line.merge, OPAL_CMD_LINE_TYPE_BOOL,
      "Merge stderr to stdout for each process"},
    { "orte_xterm", '\0', "xterm", "xterm", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Create a new xterm window and display output from the specified ranks there" },

    /* select stdin option */
    { NULL, '\0', "stdin", "stdin", 1,
      &orte_cmd_line.stdin_target, OPAL_CMD_LINE_TYPE_STRING,
      "Specify procs to receive stdin [rank, all, none] (default: 0, indicating rank 0)" },

    /* request that argv[0] be indexed */
    { NULL, '\0', "index-argv-by-rank", "index-argv-by-rank", 0,
      &orte_cmd_line.index_argv, OPAL_CMD_LINE_TYPE_BOOL,
      "Uniquely index argv[0] for each process using its rank" },

    /* Specify the launch agent to be used */
    { "orte_launch_agent", '\0', "launch-agent", "launch-agent", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Command used to start processes on remote nodes (default: orted)" },

    /* Preload the binary on the remote machine */
    { NULL, 's', NULL, "preload-binary", 0,
      &orte_cmd_line.preload_binaries, OPAL_CMD_LINE_TYPE_BOOL,
      "Preload the binary on the remote machine before starting the remote process." },

    /* Preload files on the remote machine */
    { NULL, '\0', NULL, "preload-files", 1,
      &orte_cmd_line.preload_files, OPAL_CMD_LINE_TYPE_STRING,
      "Preload the comma separated list of files to the remote machines current working directory before starting the remote process." },

#if OPAL_ENABLE_FT_CR == 1
    /* Tell SStore to preload a snapshot before launch */
    { NULL, '\0', NULL, "sstore-load", 1,
      &orte_cmd_line.sstore_load, OPAL_CMD_LINE_TYPE_STRING,
      "Internal Use Only! Tell SStore to preload a snapshot before launch." },
#endif

    /* Use an appfile */
    { NULL, '\0', NULL, "app", 1,
      &orte_cmd_line.appfile, OPAL_CMD_LINE_TYPE_STRING,
      "Provide an appfile; ignore all other command line options" },

    /* Number of processes; -c, -n, --n, -np, and --np are all
       synonyms */
    { NULL, 'c', "np", "np", 1,
      &orte_cmd_line.num_procs, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run" },
    { NULL, '\0', "n", "n", 1,
      &orte_cmd_line.num_procs, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run" },

    /* maximum size of VM - typically used to subdivide an allocation */
    { "orte_max_vm_size", '\0', "max-vm-size", "max-vm-size", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run" },

    /* Set a hostfile */
    { NULL, '\0', "hostfile", "hostfile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile" },
    { NULL, '\0', "machinefile", "machinefile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile" },
    { "orte_default_hostfile", '\0', "default-hostfile", "default-hostfile", 1,
        NULL, OPAL_CMD_LINE_TYPE_STRING,
    "Provide a default hostfile" },
    { "opal_if_do_not_resolve", '\0', "do-not-resolve", "do-not-resolve", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not attempt to resolve interfaces" },

    /* uri of PMIx publish/lookup server, or at least where to get it */
    { "pmix_server_uri", '\0', "ompi-server", "ompi-server", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Specify the URI of the publish/lookup server, or the name of the file (specified as file:filename) that contains that info" },

    { "carto_file_path", '\0', "cf", "cartofile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a cartography file" },

    { "orte_rankfile", '\0', "rf", "rankfile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a rankfile file" },

    /* Export environment variables; potentially used multiple times,
       so it does not make sense to set into a variable */
    { NULL, 'x', NULL, NULL, 1,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      "Export an environment variable, optionally specifying a value (e.g., \"-x foo\" exports the environment variable foo and takes its value from the current environment; \"-x foo=bar\" exports the environment variable name foo and sets its value to \"bar\" in the started processes)" },

      /* Mapping controls */
    { "rmaps_base_display_map", '\0', "display-map", "display-map", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Display the process map just before launch"},
    { "rmaps_base_display_devel_map", '\0', "display-devel-map", "display-devel-map", 0,
       NULL, OPAL_CMD_LINE_TYPE_BOOL,
       "Display a detailed process map (mostly intended for developers) just before launch"},
    { "rmaps_base_display_topo_with_map", '\0', "display-topo", "display-topo", 0,
       NULL, OPAL_CMD_LINE_TYPE_BOOL,
       "Display the topology as part of the process map (mostly intended for developers) just before launch"},
    { "rmaps_base_display_diffable_map", '\0', "display-diffable-map", "display-diffable-map", 0,
       NULL, OPAL_CMD_LINE_TYPE_BOOL,
       "Display a diffable process map (mostly intended for developers) just before launch"},
    { NULL, 'H', "host", "host", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "List of hosts to invoke processes on" },
    { "rmaps_base_no_schedule_local", '\0', "nolocal", "nolocal", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not run any MPI applications on the local node" },
    { "rmaps_base_no_oversubscribe", '\0', "nooversubscribe", "nooversubscribe", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Nodes are not to be oversubscribed, even if the system supports such operation"},
    { "rmaps_base_oversubscribe", '\0', "oversubscribe", "oversubscribe", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Nodes are allowed to be oversubscribed, even on a managed system, and overloading of processing elements"},
    { "rmaps_base_cpus_per_rank", '\0', "cpus-per-proc", "cpus-per-proc", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Number of cpus to use for each process [default=1]" },
    { "rmaps_base_cpus_per_rank", '\0', "cpus-per-rank", "cpus-per-rank", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Synonym for cpus-per-proc" },

    /* backward compatiblity */
    { "rmaps_base_bycore", '\0', "bycore", "bycore", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to map and rank processes round-robin by core" },
    { "rmaps_base_bynode", '\0', "bynode", "bynode", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to map and rank processes round-robin by node" },
    { "rmaps_base_byslot", '\0', "byslot", "byslot", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to map and rank processes round-robin by slot" },

    /* Nperxxx options that do not require topology and are always
     * available - included for backwards compatibility
     */
    { "rmaps_ppr_pernode", '\0', "pernode", "pernode", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Launch one process per available node" },
    { "rmaps_ppr_n_pernode", '\0', "npernode", "npernode", 1,
        NULL, OPAL_CMD_LINE_TYPE_INT,
        "Launch n processes per node on all allocated nodes" },
    { "rmaps_ppr_n_pernode", '\0', "N", NULL, 1,
        NULL, OPAL_CMD_LINE_TYPE_INT,
        "Launch n processes per node on all allocated nodes (synonym for npernode)" },

    /* declare hardware threads as independent cpus */
    { "hwloc_base_use_hwthreads_as_cpus", '\0', "use-hwthread-cpus", "use-hwthread-cpus", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Use hardware threads as independent cpus" },

    /* include npersocket for backwards compatibility */
    { "rmaps_ppr_n_persocket", '\0', "npersocket", "npersocket", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Launch n processes per socket on all allocated nodes" },

    /* Mapping options */
    { "rmaps_base_mapping_policy", '\0', NULL, "map-by", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Mapping Policy [slot | hwthread | core | socket (default) | numa | board | node]" },

      /* Ranking options */
    { "rmaps_base_ranking_policy", '\0', NULL, "rank-by", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Ranking Policy [slot (default) | hwthread | core | socket | numa | board | node]" },

      /* Binding options */
    { "hwloc_base_binding_policy", '\0', NULL, "bind-to", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Policy for binding processes. Allowed values: none, hwthread, core, l1cache, l2cache, l3cache, socket, numa, board (\"none\" is the default when oversubscribed, \"core\" is the default when np<=2, and \"socket\" is the default when np>2). Allowed qualifiers: overload-allowed, if-supported" },

    /* backward compatiblity */
    { "hwloc_base_bind_to_core", '\0', "bind-to-core", "bind-to-core", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Bind processes to cores" },
    { "hwloc_base_bind_to_socket", '\0', "bind-to-socket", "bind-to-socket", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Bind processes to sockets" },

    { "hwloc_base_report_bindings", '\0', "report-bindings", "report-bindings", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to report process bindings to stderr" },

    /* slot list option */
    { "hwloc_base_slot_list", '\0', "slot-list", "slot-list", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "List of processor IDs to bind processes to [default=NULL]"},

    /* generalized pattern mapping option */
    { "rmaps_ppr_pattern", '\0', NULL, "ppr", 1,
        NULL, OPAL_CMD_LINE_TYPE_STRING,
        "Comma-separated list of number of processes on a given resource type [default: none]" },

    /* Allocation options */
    { "orte_display_alloc", '\0', "display-allocation", "display-allocation", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Display the allocation being used by this job"},
    { "orte_display_devel_alloc", '\0', "display-devel-allocation", "display-devel-allocation", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Display a detailed list (mostly intended for developers) of the allocation being used by this job"},
    { "hwloc_base_cpu_set", '\0', "cpu-set", "cpu-set", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Comma-separated list of ranges specifying logical cpus allocated to this job [default: none]"},

    /* mpiexec-like arguments */
    { NULL, '\0', "wdir", "wdir", 1,
      &orte_cmd_line.wdir, OPAL_CMD_LINE_TYPE_STRING,
      "Set the working directory of the started processes" },
    { NULL, '\0', "wd", "wd", 1,
      &orte_cmd_line.wdir, OPAL_CMD_LINE_TYPE_STRING,
      "Synonym for --wdir" },
    { NULL, '\0', "set-cwd-to-session-dir", "set-cwd-to-session-dir", 0,
      &orte_cmd_line.set_cwd_to_session_dir, OPAL_CMD_LINE_TYPE_BOOL,
      "Set the working directory of the started processes to their session directory" },
    { NULL, '\0', "path", "path", 1,
      &orte_cmd_line.path, OPAL_CMD_LINE_TYPE_STRING,
      "PATH to be used to look for executables to start processes" },

    /* User-level debugger arguments */
    { NULL, '\0', "tv", "tv", 0,
      &orte_cmd_line.debugger, OPAL_CMD_LINE_TYPE_BOOL,
      "Deprecated backwards compatibility flag; synonym for \"--debug\"" },
    { NULL, '\0', "debug", "debug", 0,
      &orte_cmd_line.debugger, OPAL_CMD_LINE_TYPE_BOOL,
      "Invoke the user-level debugger indicated by the orte_base_user_debugger MCA parameter" },
    { "orte_base_user_debugger", '\0', "debugger", "debugger", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Sequence of debuggers to search for when \"--debug\" is used" },
    { "orte_output_debugger_proctable", '\0', "output-proctable", "output-proctable", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Output the debugger proctable after launch" },

    /* OpenRTE arguments */
    { "orte_debug", 'd', "debug-devel", "debug-devel", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE" },

    { "orte_debug_daemons", '\0', "debug-daemons", "debug-daemons", 0,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Enable debugging of any OpenRTE daemons used by this application" },

    { "orte_debug_daemons_file", '\0', "debug-daemons-file", "debug-daemons-file", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of any OpenRTE daemons used by this application, storing output in files" },

    { "orte_leave_session_attached", '\0', "leave-session-attached", "leave-session-attached", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE" },

    { "orte_do_not_launch", '\0', "do-not-launch", "do-not-launch", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Perform all necessary operations to prepare to launch the application, but do not actually launch it" },

    { NULL, '\0', NULL, "prefix", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Prefix where Open MPI is installed on remote nodes" },
    { NULL, '\0', NULL, "noprefix", 0,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Disable automatic --prefix behavior" },

    { "orte_report_launch_progress", '\0', "show-progress", "show-progress", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Output a brief periodic report on launch progress" },

    { "orte_use_regexp", '\0', "use-regexp", "use-regexp", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Use regular expressions for launch" },

    { "orte_report_events", '\0', "report-events", "report-events", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Report events to a tool listening at the specified URI" },

    { "orte_enable_recovery", '\0', "enable-recovery", "enable-recovery", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable recovery from process failure [Default = disabled]" },

    { "orte_max_restarts", '\0', "max-restarts", "max-restarts", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Max number of times to restart a failed process" },

    { "orte_hetero_nodes", '\0', NULL, "hetero-nodes", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Nodes in cluster may differ in topology, so send the topology back from each node [Default = false]" },

#if OPAL_ENABLE_CRDEBUG == 1
    { "opal_cr_enable_crdebug", '\0', "crdebug", "crdebug", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable C/R Debugging" },
#endif

    { NULL, '\0', "disable-recovery", "disable-recovery", 0,
      &orte_cmd_line.disable_recovery, OPAL_CMD_LINE_TYPE_BOOL,
      "Disable recovery (resets all recovery options to off)" },

    { "state_novm_select", '\0', "novm", "novm", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Execute without creating an allocation-spanning virtual machine (only start daemons on nodes hosting application procs)" },

    { "orte_staged_execution", '\0', "staged", "staged", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Used staged execution if inadequate resources are present (cannot support MPI jobs)" },

    { NULL, '\0', "allow-run-as-root", "allow-run-as-root", 0,
      &orte_cmd_line.run_as_root, OPAL_CMD_LINE_TYPE_BOOL,
      "Allow execution as root (STRONGLY DISCOURAGED)" },

    { NULL, '\0', "personality", "personality", 1,
      &orte_cmd_line.personality, OPAL_CMD_LINE_TYPE_STRING,
      "Comma-separated list of programming model, languages, and containers being used (default=\"ompi\")" },

    { NULL, '\0', "dvm", "dvm", 0,
      &orte_cmd_line.create_dvm, OPAL_CMD_LINE_TYPE_BOOL,
      "Create a persistent distributed virtual machine (DVM)" },

    /* End of list */
    { NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

/* local data */
static opal_list_t job_stack;

/*
 * Local functions
 */
static int create_app(int argc, char* argv[],
                      orte_job_t *jdata,
                      orte_app_context_t **app,
                      bool *made_app, char ***app_env);
static int init_globals(void);
static int parse_globals(int argc, char* argv[], opal_cmd_line_t *cmd_line);
static int parse_locals(orte_job_t *jdata, int argc, char* argv[]);
static void set_classpath_jar_file(orte_app_context_t *app, int index, char *jarfile);
static int parse_appfile(orte_job_t *jdata, char *filename, char ***env);
static void run_debugger(char *basename, opal_cmd_line_t *cmd_line,
                         int argc, char *argv[], int num_procs) __opal_attribute_noreturn__;

static void spawn_next_job(opal_buffer_t *bptr, void *cbdata)
{
    orte_job_t *jdata = (orte_job_t*)cbdata;

    /* add the data to the job's file map */
    orte_set_attribute(&jdata->attributes, ORTE_JOB_FILE_MAPS, ORTE_ATTR_GLOBAL, &bptr, OPAL_BUFFER);

    /* spawn the next job */
    orte_plm.spawn(jdata);
}
static void run_next_job(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_job_t *jdata;
    orte_process_name_t name;

    /* get next job on stack */
    jdata = (orte_job_t*)opal_list_remove_first(&job_stack);

    if (NULL == jdata) {
        /* all done - trip the termination sequence */
        orte_event_base_active = false;
        OBJ_DESTRUCT(&job_stack);
        OBJ_RELEASE(caddy);
        return;
    }

    if (NULL != orte_dfs.get_file_map) {
        /* collect any file maps and spawn the next job */
        name.jobid = caddy->jdata->jobid;
        name.vpid = ORTE_VPID_WILDCARD;

        orte_dfs.get_file_map(&name, spawn_next_job, jdata);
    } else {
        /* just spawn the job */
        orte_plm.spawn(jdata);
    }

    OBJ_RELEASE(caddy);
}

int orterun(int argc, char *argv[])
{
    int rc;
    opal_cmd_line_t cmd_line;
    char *param;
    orte_job_t *daemons;
    orte_app_context_t *app, *dapp;
    orte_job_t *jdata=NULL, *jptr;
#if OPAL_ENABLE_FT_CR == 1
    char *tmp_env_var = NULL;
#endif

    /* find our basename (the name of the executable) so that we can
       use it in pretty-print error messages */
    orte_basename = opal_basename(argv[0]);

    /* bozo check - we don't allow recursive calls of orterun */
    if (NULL != getenv("OMPI_UNIVERSE_SIZE")) {
        fprintf(stderr, "\n\n**********************************************************\n\n");
        fprintf(stderr, "Open MPI does not support recursive calls of %s\n", orte_basename);
        fprintf(stderr, "\n**********************************************************\n");
        exit(1);
    }

    /* Setup and parse the command line */
    init_globals();
    opal_cmd_line_create(&cmd_line, cmd_line_init);
    mca_base_cmd_line_setup(&cmd_line);
    if (OPAL_SUCCESS != (rc = opal_cmd_line_parse(&cmd_line, true,
                                                  argc, argv)) ) {
        if (OPAL_ERR_SILENT != rc) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0],
                    opal_strerror(rc));
        }
        return rc;
    }

    /* print version if requested.  Do this before check for help so
       that --version --help works as one might expect. */
    if (orte_cmd_line.version) {
        char *str, *project_name = NULL;
        if (0 == strcmp(orte_basename, "mpirun")) {
            project_name = "Open MPI";
        } else {
            project_name = "OpenRTE";
        }
        str = opal_info_make_version_str("all",
                                         OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                         OPAL_RELEASE_VERSION,
                                         OPAL_GREEK_VERSION,
                                         OPAL_REPO_REV);
        if (NULL != str) {
            fprintf(stdout, "%s (%s) %s\n\nReport bugs to %s\n",
                    orte_basename, project_name, str, PACKAGE_BUGREPORT);
            free(str);
        }
        exit(0);
    }

    /* check if we are running as root - if we are, then only allow
     * us to proceed if the allow-run-as-root flag was given. Otherwise,
     * exit with a giant warning flag
     */
    if (0 == geteuid() && !orte_cmd_line.run_as_root) {
        fprintf(stderr, "--------------------------------------------------------------------------\n");
        if (orte_cmd_line.help) {
            fprintf(stderr, "%s cannot provide the help message when run as root.\n", orte_basename);
        } else {
            /* show_help is not yet available, so print an error manually */
            fprintf(stderr, "%s has detected an attempt to run as root.\n", orte_basename);
        }
        fprintf(stderr, "Running at root is *strongly* discouraged as any mistake (e.g., in\n");
        fprintf(stderr, "defining TMPDIR) or bug can result in catastrophic damage to the OS\n");
        fprintf(stderr, "file system, leaving your system in an unusable state.\n\n");
        fprintf(stderr, "You can override this protection by adding the --allow-run-as-root\n");
        fprintf(stderr, "option to your cmd line. However, we reiterate our strong advice\n");
        fprintf(stderr, "against doing so - please do so at your own risk.\n");
        fprintf(stderr, "--------------------------------------------------------------------------\n");
        exit(1);
    }

    /*
     * Since this process can now handle MCA/GMCA parameters, make sure to
     * process them - we can do this step WITHOUT first calling opal_init
     */
    if (OPAL_SUCCESS != mca_base_cmd_line_process_args(&cmd_line, &environ, &environ)) {
        exit(1);
    }

    /* Ensure that enough of OPAL is setup for us to be able to run */
    /*
     * NOTE: (JJH)
     *  We need to allow 'mca_base_cmd_line_process_args()' to process command
     *  line arguments *before* calling opal_init_util() since the command
     *  line could contain MCA parameters that affect the way opal_init_util()
     *  functions. AMCA parameters are one such option normally received on the
     *  command line that affect the way opal_init_util() behaves.
     *  It is "safe" to call mca_base_cmd_line_process_args() before
     *  opal_init_util() since mca_base_cmd_line_process_args() does *not*
     *  depend upon opal_init_util() functionality.
     */
    /* Need to initialize OPAL so that install_dirs are filled in */
    if (OPAL_SUCCESS != opal_init(&argc, &argv)) {
        exit(1);
    }

    /* Check for help request */
    if (orte_cmd_line.help) {
        char *str, *args = NULL;
        char *project_name = NULL;
        if (0 == strcmp(orte_basename, "mpirun")) {
            project_name = "Open MPI";
        } else {
            project_name = "OpenRTE";
        }
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        str = opal_show_help_string("help-orterun.txt", "orterun:usage", false,
                                    orte_basename, project_name, OPAL_VERSION,
                                    orte_basename, args,
                                    PACKAGE_BUGREPORT);
        if (NULL != str) {
            printf("%s", str);
            free(str);
        }
        free(args);

        /* If someone asks for help, that should be all we do */
        opal_finalize();
        exit(0);
    }

    /* may look strange, but the way we handle prefix is a little weird
     * and probably needs to be addressed more fully at some future point.
     * For now, we have a conflict between app_files and cmd line usage.
     * Since app_files are used by the C/R system, we will make an
     * adjustment here to avoid perturbing that system.
     *
     * We cannot just have the cmd line parser place any found value
     * in the global struct as the app_file parser would replace it.
     * So handle this specific cmd line option manually.
     */
    orte_cmd_line.prefix = NULL;
    orte_cmd_line.path_to_mpirun = NULL;
    if (opal_cmd_line_is_taken(&cmd_line, "prefix") ||
        '/' == argv[0][0] || want_prefix_by_default) {
        size_t param_len;
        if ('/' == argv[0][0]) {
            char* tmp_basename = NULL;
            /* If they specified an absolute path, strip off the
               /bin/<exec_name>" and leave just the prefix */
            orte_cmd_line.path_to_mpirun = opal_dirname(argv[0]);
            /* Quick sanity check to ensure we got
               something/bin/<exec_name> and that the installation
               tree is at least more or less what we expect it to
               be */
            tmp_basename = opal_basename(orte_cmd_line.path_to_mpirun);
            if (0 == strcmp("bin", tmp_basename)) {
                char* tmp = orte_cmd_line.path_to_mpirun;
                orte_cmd_line.path_to_mpirun = opal_dirname(tmp);
                free(tmp);
            } else {
                free(orte_cmd_line.path_to_mpirun);
                orte_cmd_line.path_to_mpirun = NULL;
            }
            free(tmp_basename);
        }
        /* if both are given, check to see if they match */
        if (opal_cmd_line_is_taken(&cmd_line, "prefix") && NULL != orte_cmd_line.path_to_mpirun) {
            char *tmp_basename;
            /* if they don't match, then that merits a warning */
            param = strdup(opal_cmd_line_get_param(&cmd_line, "prefix", 0, 0));
            /* ensure we strip any trailing '/' */
            if (0 == strcmp(OPAL_PATH_SEP, &(param[strlen(param)-1]))) {
                param[strlen(param)-1] = '\0';
            }
            tmp_basename = strdup(orte_cmd_line.path_to_mpirun);
            if (0 == strcmp(OPAL_PATH_SEP, &(tmp_basename[strlen(tmp_basename)-1]))) {
                tmp_basename[strlen(tmp_basename)-1] = '\0';
            }
            if (0 != strcmp(param, tmp_basename)) {
                orte_show_help("help-orterun.txt", "orterun:double-prefix",
                               true, orte_basename, orte_basename,
                               param, tmp_basename, orte_basename);
                /* use the prefix over the path-to-mpirun so that
                 * people can specify the backend prefix as different
                 * from the local one
                 */
                free(orte_cmd_line.path_to_mpirun);
                orte_cmd_line.path_to_mpirun = NULL;
            }
            free(tmp_basename);
        } else if (NULL != orte_cmd_line.path_to_mpirun) {
            param = strdup(orte_cmd_line.path_to_mpirun);
        } else if (opal_cmd_line_is_taken(&cmd_line, "prefix")){
            /* must be --prefix alone */
            param = strdup(opal_cmd_line_get_param(&cmd_line, "prefix", 0, 0));
        } else {
            /* --enable-orterun-prefix-default was given to orterun */
            param = strdup(opal_install_dirs.prefix);
        }

        if (NULL != param) {
            /* "Parse" the param, aka remove superfluous path_sep. */
            param_len = strlen(param);
            while (0 == strcmp (OPAL_PATH_SEP, &(param[param_len-1]))) {
                param[param_len-1] = '\0';
                param_len--;
                if (0 == param_len) {
                    orte_show_help("help-orterun.txt", "orterun:empty-prefix",
                                   true, orte_basename, orte_basename);
                    free(param);
                    return ORTE_ERR_FATAL;
                }
            }

            orte_cmd_line.prefix = param;
        }
        want_prefix_by_default = true;
    }

    /* flag that I am the HNP - needs to be done prior to
     * registering params
     */
    orte_process_info.proc_type = ORTE_PROC_HNP;

    /* Setup MCA params */
    orte_register_params();

    /* save the environment for launch purposes. This MUST be
     * done so that we can pass it to any local procs we
     * spawn - otherwise, those local procs won't see any
     * non-MCA envars were set in the enviro prior to calling
     * orterun
     */
    orte_launch_environ = opal_argv_copy(environ);

    /* Intialize our Open RTE environment
     * Set the flag telling orte_init that I am NOT a
     * singleton, but am "infrastructure" - prevents setting
     * up incorrect infrastructure that only a singleton would
     * require
     */
    if (ORTE_SUCCESS != (rc = orte_init(&argc, &argv, ORTE_PROC_HNP))) {
        /* cannot call ORTE_ERROR_LOG as it could be the errmgr
         * never got loaded!
         */
        return rc;
    }
    /* finalize OPAL. As it was opened again from orte_init->opal_init
     * we continue to have a reference count on it. So we have to finalize it twice...
     */
    opal_finalize();

    /* default our personality to OMPI */
    if (NULL == orte_cmd_line.personality) {
        opal_argv_append_nosize(&orte_cmd_line.personalities, "ompi");
    } else {
        orte_cmd_line.personalities = opal_argv_split(orte_cmd_line.personality, ',');
    }
    /* Check for some "global" command line params */
    parse_globals(argc, argv, &cmd_line);
    OBJ_DESTRUCT(&cmd_line);

    /* create a new job object to hold the info for this one - the
     * jobid field will be filled in by the PLM when the job is
     * launched
     */
    jdata = OBJ_NEW(orte_job_t);
    if (NULL == jdata) {
        /* cannot call ORTE_ERROR_LOG as the errmgr
         * hasn't been loaded yet!
         */
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    jdata->personality = opal_argv_copy(orte_cmd_line.personalities);

    /* check what user wants us to do with stdin */
    if (0 == strcmp(orte_cmd_line.stdin_target, "all")) {
        jdata->stdin_target = ORTE_VPID_WILDCARD;
    } else if (0 == strcmp(orte_cmd_line.stdin_target, "none")) {
        jdata->stdin_target = ORTE_VPID_INVALID;
    } else {
        jdata->stdin_target = strtoul(orte_cmd_line.stdin_target, NULL, 10);
    }

    /* if we want the argv's indexed, indicate that */
    if (orte_cmd_line.index_argv) {
        orte_set_attribute(&jdata->attributes, ORTE_JOB_INDEX_ARGV, ORTE_ATTR_GLOBAL, NULL, OPAL_BOOL);
    }

    /* Parse each app, adding it to the job object */
    parse_locals(jdata, argc, argv);

    if (0 == jdata->num_apps) {
        /* This should never happen -- this case should be caught in
           create_app(), but let's just double check... */
        orte_show_help("help-orterun.txt", "orterun:nothing-to-do",
                       true, orte_basename);
        exit(ORTE_ERROR_DEFAULT_EXIT_CODE);
    }

#if OPAL_ENABLE_FT_CR == 1
    /* Disable OPAL CR notifications for this tool */
    opal_cr_set_enabled(false);
    (void) mca_base_var_env_name("opal_cr_is_tool", &tmp_env_var);
    opal_setenv(tmp_env_var,
                "1",
                true, &environ);
    free(tmp_env_var);
#endif

    /* get the daemon job object */
    daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);

    /* check for request to report uri */
    if (NULL != orte_cmd_line.report_uri) {
        FILE *fp;
        char *rml_uri;
        rml_uri = orte_rml.get_contact_info();
        if (0 == strcmp(orte_cmd_line.report_uri, "-")) {
            /* if '-', then output to stdout */
            printf("%s\n",  (NULL == rml_uri) ? "NULL" : rml_uri);
        } else if (0 == strcmp(orte_cmd_line.report_uri, "+")) {
            /* if '+', output to stderr */
            fprintf(stderr, "%s\n",  (NULL == rml_uri) ? "NULL" : rml_uri);
        } else {
            fp = fopen(orte_cmd_line.report_uri, "w");
            if (NULL == fp) {
                orte_show_help("help-orterun.txt", "orterun:write_file", false,
                               orte_basename, "uri", orte_cmd_line.report_uri);
                exit(0);
            }
            fprintf(fp, "%s\n", (NULL == rml_uri) ? "NULL" : rml_uri);
            fclose(fp);
        }
        if (NULL != rml_uri) {
            free(rml_uri);
        }
    }

    /* If we have a prefix, then modify the PATH and
       LD_LIBRARY_PATH environment variables in our copy. This
       will ensure that any locally-spawned children will
       have our executables and libraries in their path

       For now, default to the prefix_dir provided in the first app_context.
       Since there always MUST be at least one app_context, we are safe in
       doing this.
    */
    param = NULL;
    if (NULL != (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, 0)) &&
        orte_get_attribute(&app->attributes, ORTE_APP_PREFIX_DIR, (void**)&param, OPAL_STRING)) {
        char *oldenv, *newenv, *lib_base, *bin_base;

        /* copy the prefix into the daemon job so that any launcher
         * can find the orteds when we launch the virtual machine
         */
        if (NULL == (dapp = (orte_app_context_t*)opal_pointer_array_get_item(daemons->apps, 0))) {
            /* that's an error in the ess */
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        orte_set_attribute(&dapp->attributes, ORTE_APP_PREFIX_DIR, ORTE_ATTR_LOCAL, param, OPAL_STRING);

        lib_base = opal_basename(opal_install_dirs.libdir);
        bin_base = opal_basename(opal_install_dirs.bindir);

        /* Reset PATH */
        newenv = opal_os_path( false, param, bin_base, NULL );
        oldenv = getenv("PATH");
        if (NULL != oldenv) {
            char *temp;
            asprintf(&temp, "%s:%s", newenv, oldenv );
            free( newenv );
            newenv = temp;
        }
        opal_setenv("PATH", newenv, true, &orte_launch_environ);
        if (orte_debug_flag) {
            opal_output(0, "%s: reset PATH: %s", orte_basename, newenv);
        }
        free(newenv);
        free(bin_base);

        /* Reset LD_LIBRARY_PATH */
        newenv = opal_os_path( false, param, lib_base, NULL );
        oldenv = getenv("LD_LIBRARY_PATH");
        if (NULL != oldenv) {
            char* temp;
            asprintf(&temp, "%s:%s", newenv, oldenv);
            free(newenv);
            newenv = temp;
        }
        opal_setenv("LD_LIBRARY_PATH", newenv, true, &orte_launch_environ);
        if (orte_debug_flag) {
            opal_output(0, "%s: reset LD_LIBRARY_PATH: %s",
                        orte_basename, newenv);
        }
        free(newenv);
        free(lib_base);
        free(param);
    }

    /* pre-condition any network transports that require it */
    if (ORTE_SUCCESS != (rc = orte_pre_condition_transports(jdata))) {
        ORTE_ERROR_LOG(rc);
        orte_show_help("help-orterun.txt", "orterun:precondition", false,
                       orte_basename, NULL, NULL, rc);
        ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
        goto DONE;
    }

    /* if we were asked to tag output, mark it so */
    if (orte_tag_output) {
        orte_set_attribute(&jdata->attributes, ORTE_JOB_TAG_OUTPUT, ORTE_ATTR_GLOBAL, NULL, OPAL_BOOL);
    }
    /* if we were asked to timestamp output, mark it so */
    if (orte_timestamp_output) {
        orte_set_attribute(&jdata->attributes, ORTE_JOB_TIMESTAMP_OUTPUT, ORTE_ATTR_GLOBAL, NULL, OPAL_BOOL);
    }
    /* if we were asked to output to files, pass it along */
    if (NULL != orte_output_filename) {
        orte_set_attribute(&jdata->attributes, ORTE_JOB_OUTPUT_TO_FILE, ORTE_ATTR_GLOBAL, orte_output_filename, OPAL_STRING);
    }
    /* if we were asked to merge stderr to stdout, mark it so */
    if (orte_cmd_line.merge) {
        orte_set_attribute(&jdata->attributes, ORTE_JOB_MERGE_STDERR_STDOUT, ORTE_ATTR_GLOBAL, NULL, OPAL_BOOL);
    }
    /* setup to listen for commands sent specifically to me, even though I would probably
     * be the one sending them! Unfortunately, since I am a participating daemon,
     * there are times I need to send a command to "all daemons", and that means *I* have
     * to receive it too
     */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON,
                            ORTE_RML_PERSISTENT, orte_daemon_recv, NULL);

    /* setup for debugging */
    orte_debugger_init_before_spawn(jdata);
    orte_state.add_job_state(ORTE_JOB_STATE_READY_FOR_DEBUGGERS,
                             orte_debugger_init_after_spawn,
                             ORTE_SYS_PRI);
    orte_state.add_job_state(ORTE_JOB_STATE_DEBUGGER_DETACH,
                             orte_debugger_detached,
                             ORTE_SYS_PRI);

    if (orte_staged_execution) {
        /* staged execution is requested - each app_context
         * is treated as a separate job and executed in
         * sequence
         */
        int i;
        jdata->num_procs = 0;
        OBJ_CONSTRUCT(&job_stack, opal_list_t);
        for (i=1; i < jdata->apps->size; i++) {
            if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
                continue;
            }
            jptr = OBJ_NEW(orte_job_t);
            opal_list_append(&job_stack, &jptr->super);
            /* transfer the app */
            opal_pointer_array_set_item(jdata->apps, i, NULL);
            --jdata->num_apps;
            /* reset the app_idx */
            app->idx = 0;
            opal_pointer_array_set_item(jptr->apps, 0, app);
            ++jptr->num_apps;
        }
        /* define a state machine position
         * that is fired when each job completes so we can then start
         * the next job in our stack
         */
        if (ORTE_SUCCESS != (rc = orte_state.set_job_state_callback(ORTE_JOB_STATE_NOTIFY_COMPLETED, run_next_job))) {
            ORTE_ERROR_LOG(rc);
            ORTE_UPDATE_EXIT_STATUS(rc);
            goto DONE;
        }
    }

    /* check for a job timeout specification, to be provided in seconds
     * as that is what MPICH used
     */
    if (NULL != (param = getenv("MPIEXEC_TIMEOUT"))) {
        if (NULL == (orte_mpiexec_timeout = OBJ_NEW(orte_timer_t))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            ORTE_UPDATE_EXIT_STATUS(ORTE_ERR_OUT_OF_RESOURCE);
            goto DONE;
        }
        orte_mpiexec_timeout->tv.tv_sec = strtol(param, NULL, 10);
        orte_mpiexec_timeout->tv.tv_usec = 0;
        opal_event_evtimer_set(orte_event_base, orte_mpiexec_timeout->ev,
                               orte_timeout_wakeup, jdata);
        opal_event_set_priority(orte_mpiexec_timeout->ev, ORTE_ERROR_PRI);
        opal_event_evtimer_add(orte_mpiexec_timeout->ev, &orte_mpiexec_timeout->tv);
    }

    /* spawn the job and its daemons */
    rc = orte_plm.spawn(jdata);

    /* loop the event lib until an exit event is detected */
    while (orte_event_base_active) {
        opal_event_loop(orte_event_base, OPAL_EVLOOP_ONCE);
    }

    /* ensure all local procs are dead */
    orte_odls.kill_local_procs(NULL);

 DONE:
    /* if it was created, remove the debugger attach fifo */
    if (0 <= attach_fd) {
        if (fifo_active) {
            opal_event_del(attach);
            free(attach);
        }
        close(attach_fd);
        unlink(MPIR_attach_fifo);
    }

    /* cleanup and leave */
    orte_finalize();

    if (orte_debug_flag) {
        fprintf(stderr, "exiting with status %d\n", orte_exit_status);
    }
    exit(orte_exit_status);
}

static int init_globals(void)
{
    /* Only CONSTRUCT things once */
    if (!globals_init) {
        orte_cmd_line.env_val =     NULL;
        orte_cmd_line.appfile =     NULL;
        orte_cmd_line.wdir =        NULL;
        orte_cmd_line.path =        NULL;
        orte_cmd_line.stdin_target = "0";
        orte_cmd_line.report_pid        = NULL;
        orte_cmd_line.report_uri        = NULL;
        orte_cmd_line.disable_recovery = false;
        orte_cmd_line.index_argv = false;
        orte_cmd_line.run_as_root = false;
        orte_cmd_line.personality = NULL;
        orte_cmd_line.personalities = NULL;
        orte_cmd_line.create_dvm = false;
    }

    /* Reset the other fields every time */

    orte_cmd_line.help                       = false;
    orte_cmd_line.version                    = false;
    orte_cmd_line.verbose                    = false;
    orte_cmd_line.debugger                   = false;
    orte_cmd_line.num_procs                  =  0;
    if( NULL != orte_cmd_line.env_val )
        free( orte_cmd_line.env_val );
    orte_cmd_line.env_val =     NULL;
    if( NULL != orte_cmd_line.appfile )
        free( orte_cmd_line.appfile );
    orte_cmd_line.appfile =     NULL;
    if( NULL != orte_cmd_line.wdir )
        free( orte_cmd_line.wdir );
    orte_cmd_line.set_cwd_to_session_dir = false;
    orte_cmd_line.wdir =        NULL;
    if( NULL != orte_cmd_line.path )
        free( orte_cmd_line.path );
    orte_cmd_line.path =        NULL;

    orte_cmd_line.preload_binaries = false;
    orte_cmd_line.preload_files  = NULL;

#if OPAL_ENABLE_FT_CR == 1
    orte_cmd_line.sstore_load = NULL;
#endif

    /* All done */
    globals_init = true;
    return ORTE_SUCCESS;
}


static int parse_globals(int argc, char* argv[], opal_cmd_line_t *cmd_line)
{
    /* check for request to report pid */
    if (NULL != orte_cmd_line.report_pid) {
        FILE *fp;
        if (0 == strcmp(orte_cmd_line.report_pid, "-")) {
            /* if '-', then output to stdout */
            printf("%d\n", (int)getpid());
        } else if (0 == strcmp(orte_cmd_line.report_pid, "+")) {
            /* if '+', output to stderr */
            fprintf(stderr, "%d\n", (int)getpid());
        } else {
            fp = fopen(orte_cmd_line.report_pid, "w");
            if (NULL == fp) {
                orte_show_help("help-orterun.txt", "orterun:write_file", false,
                               orte_basename, "pid", orte_cmd_line.report_pid);
                exit(0);
            }
            fprintf(fp, "%d\n", (int)getpid());
            fclose(fp);
        }
    }

    /* Do we want a user-level debugger? */

    if (orte_cmd_line.debugger) {
        run_debugger(orte_basename, cmd_line, argc, argv, orte_cmd_line.num_procs);
    }

     /* if recovery was disabled on the cmd line, do so */
    if (orte_cmd_line.disable_recovery) {
        orte_enable_recovery = false;
        orte_max_restarts = 0;
    }

    return ORTE_SUCCESS;
}


static int parse_locals(orte_job_t *jdata, int argc, char* argv[])
{
    int i, rc, app_num;
    int temp_argc;
    char **temp_argv, **env;
    orte_app_context_t *app;
    bool made_app;
    orte_std_cntr_t j, size1;

    /* Make the apps */
    temp_argc = 0;
    temp_argv = NULL;
    opal_argv_append(&temp_argc, &temp_argv, argv[0]);

    /* NOTE: This bogus env variable is necessary in the calls to
       create_app(), below.  See comment immediately before the
       create_app() function for an explanation. */

    env = NULL;
    for (app_num = 0, i = 1; i < argc; ++i) {
        if (0 == strcmp(argv[i], ":")) {
            /* Make an app with this argv */
            if (opal_argv_count(temp_argv) > 1) {
                if (NULL != env) {
                    opal_argv_free(env);
                    env = NULL;
                }
                app = NULL;
                rc = create_app(temp_argc, temp_argv, jdata, &app, &made_app, &env);
                /** keep track of the number of apps - point this app_context to that index */
                if (ORTE_SUCCESS != rc) {
                    /* Assume that the error message has already been
                       printed; no need to cleanup -- we can just
                       exit */
                    exit(1);
                }
                if (made_app) {
                    app->idx = app_num;
                    ++app_num;
                    opal_pointer_array_add(jdata->apps, app);
                    ++jdata->num_apps;
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
            app->idx = app_num;
            ++app_num;
            opal_pointer_array_add(jdata->apps, app);
            ++jdata->num_apps;
        }
    }
    if (NULL != env) {
        opal_argv_free(env);
    }
    opal_argv_free(temp_argv);

    /* Once we've created all the apps, add the global MCA params to
       each app's environment (checking for duplicates, of
       course -- yay opal_environ_merge()).  */

    if (NULL != global_mca_env) {
        size1 = (size_t)opal_pointer_array_get_size(jdata->apps);
        /* Iterate through all the apps */
        for (j = 0; j < size1; ++j) {
            app = (orte_app_context_t *)
                opal_pointer_array_get_item(jdata->apps, j);
            if (NULL != app) {
                /* Use handy utility function */
                env = opal_environ_merge(global_mca_env, app->env);
                opal_argv_free(app->env);
                app->env = env;
            }
        }
    }

    /* Now take a subset of the MCA params and set them as MCA
       overrides here in orterun (so that when we orte_init() later,
       all the components see these MCA params).  Here's how we decide
       which subset of the MCA params we set here in orterun:

       1. If any global MCA params were set, use those
       2. If no global MCA params were set and there was only one app,
          then use its app MCA params
       3. Otherwise, don't set any
    */

    env = NULL;
    if (NULL != global_mca_env) {
        env = global_mca_env;
    } else {
        if (opal_pointer_array_get_size(jdata->apps) >= 1) {
            /* Remember that pointer_array's can be padded with NULL
               entries; so only use the app's env if there is exactly
               1 non-NULL entry */
            app = (orte_app_context_t *)
                opal_pointer_array_get_item(jdata->apps, 0);
            if (NULL != app) {
                env = app->env;
                for (j = 1; j < opal_pointer_array_get_size(jdata->apps); ++j) {
                    if (NULL != opal_pointer_array_get_item(jdata->apps, j)) {
                        env = NULL;
                        break;
                    }
                }
            }
        }
    }

    if (NULL != env) {
        size1 = opal_argv_count(env);
        for (j = 0; j < size1; ++j) {
            /* Use-after-Free error possible here.  putenv does not copy
             * the string passed to it, and instead stores only the pointer.
             * env[j] may be freed later, in which case the pointer
             * in environ will now be left dangling into a deallocated
             * region.
             * So we make a copy of the variable.
             */
            char *s = strdup(env[j]);

            if (NULL == s) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
            putenv(s);
        }
    }

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
                      orte_job_t *jdata,
                      orte_app_context_t **app_ptr,
                      bool *made_app, char ***app_env)
{
    opal_cmd_line_t cmd_line;
    char cwd[OPAL_PATH_MAX];
    int i, j, count, rc;
    char *param, *value;
    orte_app_context_t *app = NULL;
    bool cmd_line_made = false;
    bool found = false;
    char *appname;

    *made_app = false;

    /* Pre-process the command line if we are going to parse an appfile later.
     * save any mca command line args so they can be passed
     * separately to the daemons.
     * Use Case:
     *  $ cat launch.appfile
     *  -np 1 -mca aaa bbb ./my-app -mca ccc ddd
     *  -np 1 -mca aaa bbb ./my-app -mca eee fff
     *  $ mpirun -np 2 -mca foo bar --app launch.appfile
     * Only pick up '-mca foo bar' on this pass.
     */
    if (NULL != orte_cmd_line.appfile) {
        if (ORTE_SUCCESS != (rc = orte_schizo.parse_cli(orte_cmd_line.personalities, argc, 0, argv))) {
            goto cleanup;
        }
    }

    /* Parse application command line options. */

    init_globals();
    opal_cmd_line_create(&cmd_line, cmd_line_init);
    mca_base_cmd_line_setup(&cmd_line);
    cmd_line_made = true;
    rc = opal_cmd_line_parse(&cmd_line, true, argc, argv);
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }
    mca_base_cmd_line_process_args(&cmd_line, app_env, &global_mca_env);

    /* Is there an appfile in here? */

    if (NULL != orte_cmd_line.appfile) {
        OBJ_DESTRUCT(&cmd_line);
        return parse_appfile(jdata, strdup(orte_cmd_line.appfile), app_env);
    }

    /* Setup application context */

    app = OBJ_NEW(orte_app_context_t);
    opal_cmd_line_get_tail(&cmd_line, &count, &app->argv);

    /* See if we have anything left */

    if (0 == count) {
        orte_show_help("help-orterun.txt", "orterun:executable-not-specified",
                       true, orte_basename, orte_basename);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /*
     * Get mca parameters so we can pass them to the daemons.
     * Use the count determined above to make sure we do not go past
     * the executable name. Example:
     *   mpirun -np 2 -mca foo bar ./my-app -mca bip bop
     * We want to pick up '-mca foo bar' but not '-mca bip bop'
     */
    if (ORTE_SUCCESS != (rc = orte_schizo.parse_cli(orte_cmd_line.personalities,
                                                    argc, count, argv))) {
        goto cleanup;
    }

    /* Grab all OMPI_* environment variables */

    app->env = opal_argv_copy(*app_env);
    if (ORTE_SUCCESS != (rc = orte_schizo.parse_env(orte_cmd_line.personalities,
                                                    orte_cmd_line.path,
                                                    &cmd_line,
                                                    environ, &app->env))) {
        goto cleanup;
    }


    /* Did the user request a specific wdir? */

    if (NULL != orte_cmd_line.wdir) {
        /* if this is a relative path, convert it to an absolute path */
        if (opal_path_is_absolute(orte_cmd_line.wdir)) {
            app->cwd = strdup(orte_cmd_line.wdir);
        } else {
            /* get the cwd */
            if (OPAL_SUCCESS != (rc = opal_getcwd(cwd, sizeof(cwd)))) {
                orte_show_help("help-orterun.txt", "orterun:init-failure",
                               true, "get the cwd", rc);
                goto cleanup;
            }
            /* construct the absolute path */
            app->cwd = opal_os_path(false, cwd, orte_cmd_line.wdir, NULL);
        }
        orte_set_attribute(&app->attributes, ORTE_APP_USER_CWD, ORTE_ATTR_GLOBAL, NULL, OPAL_BOOL);
    } else if (orte_cmd_line.set_cwd_to_session_dir) {
        orte_set_attribute(&app->attributes, ORTE_APP_SSNDIR_CWD, ORTE_ATTR_GLOBAL, NULL, OPAL_BOOL);
        orte_set_attribute(&app->attributes, ORTE_APP_USER_CWD, ORTE_ATTR_GLOBAL, NULL, OPAL_BOOL);
    } else {
        if (OPAL_SUCCESS != (rc = opal_getcwd(cwd, sizeof(cwd)))) {
            orte_show_help("help-orterun.txt", "orterun:init-failure",
                           true, "get the cwd", rc);
            goto cleanup;
        }
        app->cwd = strdup(cwd);
    }

    /* if this is the first app_context, check for prefix directions.
     * We only do this for the first app_context because the launchers
     * only look at the first one when setting the prefix - we do NOT
     * support per-app_context prefix settings!
     */
    if (0 == total_num_apps) {
        /* Check to see if the user explicitly wanted to disable automatic
           --prefix behavior */

        if (opal_cmd_line_is_taken(&cmd_line, "noprefix")) {
            want_prefix_by_default = false;
        }

        /* Did the user specify a prefix, or want prefix by default? */
        if (opal_cmd_line_is_taken(&cmd_line, "prefix") || want_prefix_by_default) {
            size_t param_len;
            /* if both the prefix was given and we have a prefix
             * given above, check to see if they match
             */
            if (opal_cmd_line_is_taken(&cmd_line, "prefix") &&
                NULL != orte_cmd_line.prefix) {
                /* if they don't match, then that merits a warning */
                param = strdup(opal_cmd_line_get_param(&cmd_line, "prefix", 0, 0));
                /* ensure we strip any trailing '/' */
                if (0 == strcmp(OPAL_PATH_SEP, &(param[strlen(param)-1]))) {
                    param[strlen(param)-1] = '\0';
                }
                value = strdup(orte_cmd_line.prefix);
                if (0 == strcmp(OPAL_PATH_SEP, &(value[strlen(value)-1]))) {
                    value[strlen(value)-1] = '\0';
                }
                if (0 != strcmp(param, value)) {
                    orte_show_help("help-orterun.txt", "orterun:app-prefix-conflict",
                                   true, orte_basename, value, param);
                    /* let the global-level prefix take precedence since we
                     * know that one is being used
                     */
                    free(param);
                    param = strdup(orte_cmd_line.prefix);
                }
                free(value);
            } else if (NULL != orte_cmd_line.prefix) {
                param = strdup(orte_cmd_line.prefix);
            } else if (opal_cmd_line_is_taken(&cmd_line, "prefix")){
                /* must be --prefix alone */
                param = strdup(opal_cmd_line_get_param(&cmd_line, "prefix", 0, 0));
            } else {
                /* --enable-orterun-prefix-default was given to orterun */
                param = strdup(opal_install_dirs.prefix);
            }

            if (NULL != param) {
                /* "Parse" the param, aka remove superfluous path_sep. */
                param_len = strlen(param);
                while (0 == strcmp (OPAL_PATH_SEP, &(param[param_len-1]))) {
                    param[param_len-1] = '\0';
                    param_len--;
                    if (0 == param_len) {
                        orte_show_help("help-orterun.txt", "orterun:empty-prefix",
                                       true, orte_basename, orte_basename);
                        free(param);
                        return ORTE_ERR_FATAL;
                    }
                }
                orte_set_attribute(&app->attributes, ORTE_APP_PREFIX_DIR, ORTE_ATTR_GLOBAL, param, OPAL_STRING);
                free(param);
            }
        }
    }

    /* Did the user specify a hostfile. Need to check for both
     * hostfile and machine file.
     * We can only deal with one hostfile per app context, otherwise give an error.
     */
    if (0 < (j = opal_cmd_line_get_ninsts(&cmd_line, "hostfile"))) {
        if(1 < j) {
            orte_show_help("help-orterun.txt", "orterun:multiple-hostfiles",
                           true, orte_basename, NULL);
            return ORTE_ERR_FATAL;
        } else {
            value = opal_cmd_line_get_param(&cmd_line, "hostfile", 0, 0);
            orte_set_attribute(&app->attributes, ORTE_APP_HOSTFILE, ORTE_ATTR_LOCAL, value, OPAL_STRING);
        }
    }
    if (0 < (j = opal_cmd_line_get_ninsts(&cmd_line, "machinefile"))) {
        if(1 < j || orte_get_attribute(&app->attributes, ORTE_APP_HOSTFILE, NULL, OPAL_STRING)) {
            orte_show_help("help-orterun.txt", "orterun:multiple-hostfiles",
                           true, orte_basename, NULL);
            return ORTE_ERR_FATAL;
        } else {
            value = opal_cmd_line_get_param(&cmd_line, "machinefile", 0, 0);
            orte_set_attribute(&app->attributes, ORTE_APP_HOSTFILE, ORTE_ATTR_LOCAL, value, OPAL_STRING);
        }
    }

    /* Did the user specify any hosts? */
    if (0 < (j = opal_cmd_line_get_ninsts(&cmd_line, "host"))) {
        char **targ=NULL, *tval;
        for (i = 0; i < j; ++i) {
            value = opal_cmd_line_get_param(&cmd_line, "host", i, 0);
            opal_argv_append_nosize(&targ, value);
        }
        tval = opal_argv_join(targ, ',');
        orte_set_attribute(&app->attributes, ORTE_APP_DASH_HOST, ORTE_ATTR_LOCAL, tval, OPAL_STRING);
        opal_argv_free(targ);
        free(tval);
    } else if (NULL != orte_default_dash_host) {
        orte_set_attribute(&app->attributes, ORTE_APP_DASH_HOST, ORTE_ATTR_LOCAL,
                           orte_default_dash_host, OPAL_STRING);
    }

    /* check for bozo error */
    if (0 > orte_cmd_line.num_procs) {
        orte_show_help("help-orterun.txt", "orterun:negative-nprocs",
                       true, orte_basename, app->argv[0],
                       orte_cmd_line.num_procs, NULL);
        return ORTE_ERR_FATAL;
    }

    app->num_procs = (orte_std_cntr_t)orte_cmd_line.num_procs;
    total_num_apps++;

    /* Capture any preload flags */
    if (orte_cmd_line.preload_binaries) {
        orte_set_attribute(&app->attributes, ORTE_APP_PRELOAD_BIN, ORTE_ATTR_LOCAL, NULL, OPAL_BOOL);
    }
    /* if we were told to cwd to the session dir and the app was given in
     * relative syntax, then we need to preload the binary to
     * find the app - don't do this for java apps, however, as we
     * can't easily find the class on the cmd line. Java apps have to
     * preload their binary via the preload_files option
     */
    if (!opal_path_is_absolute(app->argv[0]) &&
        NULL == strstr(app->argv[0], "java")) {
        if (orte_cmd_line.preload_binaries) {
            orte_set_attribute(&app->attributes, ORTE_APP_SSNDIR_CWD, ORTE_ATTR_GLOBAL, NULL, OPAL_BOOL);
        } else if (orte_get_attribute(&app->attributes, ORTE_APP_SSNDIR_CWD, NULL, OPAL_BOOL)) {
            orte_set_attribute(&app->attributes, ORTE_APP_PRELOAD_BIN, ORTE_ATTR_LOCAL, NULL, OPAL_BOOL);
        }
    }
    if (NULL != orte_cmd_line.preload_files) {
        orte_set_attribute(&app->attributes, ORTE_APP_PRELOAD_FILES, ORTE_ATTR_LOCAL,
                           orte_cmd_line.preload_files, OPAL_STRING);
    }

#if OPAL_ENABLE_FT_CR == 1
    if(NULL != orte_cmd_line.sstore_load) {
        orte_set_attribute(&app->attributes, ORTE_APP_SSTORE_LOAD, ORTE_ATTR_LOCAL,
                           orte_cmd_line.sstore_load, OPAL_STRING);
    }
#endif

    /* Do not try to find argv[0] here -- the starter is responsible
       for that because it may not be relevant to try to find it on
       the node where orterun is executing.  So just strdup() argv[0]
       into app. */

    app->app = strdup(app->argv[0]);
    if (NULL == app->app) {
        orte_show_help("help-orterun.txt", "orterun:call-failed",
                       true, orte_basename, "library", "strdup returned NULL", errno);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* if this is a Java application, we have a bit more work to do. Such
     * applications actually need to be run under the Java virtual machine
     * and the "java" command will start the "executable". So we need to ensure
     * that all the proper java-specific paths are provided
     */
    appname = opal_basename(app->app);
    if (0 == strcmp(appname, "java")) {
        /* see if we were given a library path */
        found = false;
        for (i=1; NULL != app->argv[i]; i++) {
            if (NULL != strstr(app->argv[i], "java.library.path")) {
                /* yep - but does it include the path to the mpi libs? */
                found = true;
                if (NULL == strstr(app->argv[i], opal_install_dirs.libdir)) {
                    /* doesn't appear to - add it to be safe */
                    if (':' == app->argv[i][strlen(app->argv[i]-1)]) {
                        asprintf(&value, "-Djava.library.path=%s%s", app->argv[i], opal_install_dirs.libdir);
                    } else {
                        asprintf(&value, "-Djava.library.path=%s:%s", app->argv[i], opal_install_dirs.libdir);
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
    free(appname);

    *app_ptr = app;
    app = NULL;
    *made_app = true;

    /* All done */

 cleanup:
    if (NULL != app) {
        OBJ_RELEASE(app);
    }
    if (cmd_line_made) {
        OBJ_DESTRUCT(&cmd_line);
    }
    return rc;
}

static void set_classpath_jar_file(orte_app_context_t *app, int index, char *jarfile)
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

static int parse_appfile(orte_job_t *jdata, char *filename, char ***env)
{
    size_t i, len;
    FILE *fp;
    char line[BUFSIZ];
    int rc, argc, app_num;
    orte_app_context_t *app;
    bool blank, made_app;
    char bogus[] = "bogus ";
    char **tmp_env;

    /*
     * Make sure to clear out this variable so we don't do anything odd in
     * app_create()
     */
    if( NULL != orte_cmd_line.appfile ) {
        free( orte_cmd_line.appfile );
        orte_cmd_line.appfile =     NULL;
    }

    /* Try to open the file */

    fp = fopen(filename, "r");
    if (NULL == fp) {
        orte_show_help("help-orterun.txt", "orterun:appfile-not-found", true,
                       filename);
        return ORTE_ERR_NOT_FOUND;
    }

    /* Read in line by line */

    line[sizeof(line) - 1] = '\0';
    app_num = 0;
    do {
        char **argv;

        /* We need a bogus argv[0] (because when argv comes in from
           the command line, argv[0] is "orterun", so the parsing
           logic ignores it).  So create one here rather than making
           an argv and then pre-pending a new argv[0] (which would be
           rather inefficient). */

        line[0] = '\0';
        strcat(line, bogus);

        if (NULL == fgets(line + sizeof(bogus) - 1,
                          sizeof(line) - sizeof(bogus) - 1, fp)) {
            break;
        }

        /* Remove a trailing newline */

        len = strlen(line);
        if (len > 0 && '\n' == line[len - 1]) {
            line[len - 1] = '\0';
            if (len > 0) {
                --len;
            }
        }

        /* Remove comments */

        for (i = 0; i < len; ++i) {
            if ('#' == line[i]) {
                line[i] = '\0';
                break;
            } else if (i + 1 < len && '/' == line[i] && '/' == line[i + 1]) {
                line[i] = '\0';
                break;
            }
        }

        /* Is this a blank line? */

        len = strlen(line);
        for (blank = true, i = sizeof(bogus); i < len; ++i) {
            if (!isspace(line[i])) {
                blank = false;
                break;
            }
        }
        if (blank) {
            continue;
        }

        /* We got a line with *something* on it.  So process it */

        argv = opal_argv_split(line, ' ');
        argc = opal_argv_count(argv);
        if (argc > 0) {

            /* Create a temporary env to use in the recursive call --
               that is: don't disturb the original env so that we can
               have a consistent global env.  This allows for the
               case:

                   orterun --mca foo bar --appfile file

               where the "file" contains multiple apps.  In this case,
               each app in "file" will get *only* foo=bar as the base
               environment from which its specific environment is
               constructed. */

            if (NULL != *env) {
                tmp_env = opal_argv_copy(*env);
                if (NULL == tmp_env) {
                    opal_argv_free(argv);
                    fclose(fp);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
            } else {
                tmp_env = NULL;
            }

            rc = create_app(argc, argv, jdata, &app, &made_app, &tmp_env);
            if (ORTE_SUCCESS != rc) {
                /* Assume that the error message has already been
                   printed; no need to cleanup -- we can just exit */
                exit(1);
            }
            if (NULL != tmp_env) {
                opal_argv_free(tmp_env);
            }
            opal_argv_free(argv);
            if (made_app) {
                app->idx = app_num;
                ++app_num;
                opal_pointer_array_add(jdata->apps, app);
                ++jdata->num_apps;
            }
        }
    } while (!feof(fp));
    fclose(fp);

    /* All done */

    free(filename);
    return ORTE_SUCCESS;
}
/*
 * Process one line from the orte_base_user_debugger MCA param and
 * look for that debugger in the path.  If we find it, fill in
 * new_argv.
 */
static int process(char *orig_line, char *basename, opal_cmd_line_t *cmd_line,
                   int argc, char **argv, char ***new_argv, int num_procs)
{
    int ret = ORTE_SUCCESS;
    int i, j, count;
    char *line = NULL, *tmp = NULL, *full_line = strdup(orig_line);
    char **orterun_argv = NULL, **executable_argv = NULL, **line_argv = NULL;
    char cwd[OPAL_PATH_MAX];
    bool used_num_procs = false;
    bool single_app = false;
    bool fail_needed_executable = false;

    line = full_line;
    if (NULL == line) {
        ret = ORTE_ERR_OUT_OF_RESOURCE;
        goto out;
    }

    /* Trim off whitespace at the beginning and ending of line */

    for (i = 0; '\0' != line[i] && isspace(line[i]); ++line) {
        continue;
    }
    for (i = strlen(line) - 2; i > 0 && isspace(line[i]); ++i) {
        line[i] = '\0';
    }
    if (strlen(line) <= 0) {
        ret = ORTE_ERROR;
        goto out;
    }

    /* Get the tail of the command line (i.e., the user executable /
       argv) */

    opal_cmd_line_get_tail(cmd_line, &i, &executable_argv);

    /* Make a new copy of the orterun command line args, without the
       orterun token itself, and without the --debug, --debugger, and
       -tv flags. */

    orterun_argv = opal_argv_copy(argv);
    count = opal_argv_count(orterun_argv);
    opal_argv_delete(&count, &orterun_argv, 0, 1);
    for (i = 0; NULL != orterun_argv[i]; ++i) {
        count = opal_argv_count(orterun_argv);
        if (0 == strcmp(orterun_argv[i], "-debug") ||
            0 == strcmp(orterun_argv[i], "--debug")) {
            opal_argv_delete(&count, &orterun_argv, i, 1);
        } else if (0 == strcmp(orterun_argv[i], "-tv") ||
                   0 == strcmp(orterun_argv[i], "--tv")) {
            opal_argv_delete(&count, &orterun_argv, i, 1);
        } else if (0 == strcmp(orterun_argv[i], "--debugger") ||
                   0 == strcmp(orterun_argv[i], "-debugger")) {
            opal_argv_delete(&count, &orterun_argv, i, 2);
        }
    }

    /* Replace @@ tokens - line should never realistically be bigger
       than MAX_INT, so just cast to int to remove compiler warning */

    *new_argv = NULL;
    line_argv = opal_argv_split(line, ' ');
    if (NULL == line_argv) {
        ret = ORTE_ERR_NOT_FOUND;
        goto out;
    }
    for (i = 0; NULL != line_argv[i]; ++i) {
        if (0 == strcmp(line_argv[i], "@mpirun@") ||
            0 == strcmp(line_argv[i], "@orterun@")) {
            opal_argv_append_nosize(new_argv, argv[0]);
        } else if (0 == strcmp(line_argv[i], "@mpirun_args@") ||
                   0 == strcmp(line_argv[i], "@orterun_args@")) {
            for (j = 0; NULL != orterun_argv && NULL != orterun_argv[j]; ++j) {
                opal_argv_append_nosize(new_argv, orterun_argv[j]);
            }
        } else if (0 == strcmp(line_argv[i], "@np@")) {
            used_num_procs = true;
            asprintf(&tmp, "%d", num_procs);
            opal_argv_append_nosize(new_argv, tmp);
            free(tmp);
        } else if (0 == strcmp(line_argv[i], "@single_app@")) {
            /* This token is only a flag; it is not replaced with any
               alternate text */
            single_app = true;
        } else if (0 == strcmp(line_argv[i], "@executable@")) {
            /* If we found the executable, paste it in.  Otherwise,
               this is a possible error. */
            if (NULL != executable_argv) {
                opal_argv_append_nosize(new_argv, executable_argv[0]);
            } else {
                fail_needed_executable = true;
            }
        } else if (0 == strcmp(line_argv[i], "@executable_argv@")) {
            /* If we found the tail, paste in the argv.  Otherwise,
               this is a possible error. */
            if (NULL != executable_argv) {
                for (j = 1; NULL != executable_argv[j]; ++j) {
                    opal_argv_append_nosize(new_argv, executable_argv[j]);
                }
            } else {
                fail_needed_executable = true;
            }
        } else {
            /* It wasn't a special token, so just copy it over */
            opal_argv_append_nosize(new_argv, line_argv[i]);
        }
    }

    /* Can we find argv[0] in the path? */

    getcwd(cwd, OPAL_PATH_MAX);
    tmp = opal_path_findv((*new_argv)[0], X_OK, environ, cwd);
    if (NULL != tmp) {
        free(tmp);

        /* Ok, we found a good debugger.  Check for some error
           conditions. */
        tmp = opal_argv_join(argv, ' ');

        /* We do not support launching a debugger that requires the
           -np value if the user did not specify -np on the command
           line. */
        if (used_num_procs && 0 == num_procs) {
            free(tmp);
            tmp = opal_argv_join(orterun_argv, ' ');
            orte_show_help("help-orterun.txt", "debugger requires -np",
                           true, (*new_argv)[0], argv[0], tmp,
                           (*new_argv)[0]);
            /* Fall through to free / fail, below */
        }

        /* Some debuggers do not support launching MPMD */
        else if (single_app && NULL != strstr(tmp, " : ")) {
            orte_show_help("help-orterun.txt",
                           "debugger only accepts single app", true,
                           (*new_argv)[0], (*new_argv)[0]);
            /* Fall through to free / fail, below */
        }

        /* Some debuggers do not use orterun/mpirun, and therefore
           must have an executable to run (e.g., cannot use mpirun's
           app context file feature). */
        else if (fail_needed_executable) {
            orte_show_help("help-orterun.txt",
                           "debugger requires executable", true,
                           (*new_argv)[0], argv[0], (*new_argv)[0], argv[0],
                           (*new_argv)[0]);
            /* Fall through to free / fail, below */
        }

        /* Otherwise, we succeeded.  Return happiness. */
        else {
            goto out;
        }
    }

    /* All done -- didn't find it */

    opal_argv_free(*new_argv);
    *new_argv = NULL;
    ret = ORTE_ERR_NOT_FOUND;

 out:
    if (NULL != orterun_argv) {
        opal_argv_free(orterun_argv);
    }
    if (NULL != executable_argv) {
        opal_argv_free(executable_argv);
    }
    if (NULL != line_argv) {
        opal_argv_free(line_argv);
    }
    if (NULL != tmp) {
        free(tmp);
    }
    if (NULL != full_line) {
        free(full_line);
    }
    return ret;
}

/**
 * Run a user-level debugger
 */
static void run_debugger(char *basename, opal_cmd_line_t *cmd_line,
                         int argc, char *argv[], int num_procs)
{
    int i, id, ret;
    char **new_argv = NULL;
    const char **tmp;
    char *value, **lines, *env_name;

    /* Get the orte_base_debug MCA parameter and search for a debugger
       that can run */

    id = mca_base_var_find("orte", "orte", NULL, "base_user_debugger");
    if (id < 0) {
        orte_show_help("help-orterun.txt", "debugger-mca-param-not-found",
                       true);
        exit(1);
    }

    ret = mca_base_var_get_value (id, &tmp, NULL, NULL);
    if (OPAL_SUCCESS != ret || NULL == tmp || NULL == tmp[0]) {
        orte_show_help("help-orterun.txt", "debugger-orte_base_user_debugger-empty",
                       true);
        exit(1);
    }

    /* Look through all the values in the MCA param */

    lines = opal_argv_split(tmp[0], ':');
    for (i = 0; NULL != lines[i]; ++i) {
        if (ORTE_SUCCESS == process(lines[i], basename, cmd_line, argc, argv,
                                    &new_argv, num_procs)) {
            break;
        }
    }

    /* If we didn't find one, abort */

    if (NULL == lines[i]) {
        orte_show_help("help-orterun.txt", "debugger-not-found", true);
        exit(1);
    }
    opal_argv_free(lines);

    /* We found one */

    /* cleanup the MPIR arrays in case the debugger doesn't set them */
    memset((char*)MPIR_executable_path, 0, MPIR_MAX_PATH_LENGTH);
    memset((char*)MPIR_server_arguments, 0, MPIR_MAX_ARG_LENGTH);

    /* Set an MCA param so that everyone knows that they are being
       launched under a debugger; not all debuggers are consistent
       about setting MPIR_being_debugged in both the launcher and the
       MPI processes */
    ret = mca_base_var_env_name ("orte_in_parallel_debugger", &env_name);
    if (OPAL_SUCCESS == ret && NULL != env_name) {
        opal_setenv(env_name, "1", true, &environ);
        free(env_name);
    }

    /* Launch the debugger */
    execvp(new_argv[0], new_argv);
    value = opal_argv_join(new_argv, ' ');
    orte_show_help("help-orterun.txt", "debugger-exec-failed",
                   true, basename, value, new_argv[0]);
    free(value);
    opal_argv_free(new_argv);
    exit(1);
}

/****    DEBUGGER CODE ****/
/*
 * Debugger support for orterun
 *
 * We interpret the MPICH debugger interface as follows:
 *
 * a) The launcher
 *      - spawns the other processes,
 *      - fills in the table MPIR_proctable, and sets MPIR_proctable_size
 *      - sets MPIR_debug_state to MPIR_DEBUG_SPAWNED ( = 1)
 *      - calls MPIR_Breakpoint() which the debugger will have a
 *    breakpoint on.
 *
 *  b) Applications start and then spin until MPIR_debug_gate is set
 *     non-zero by the debugger.
 *
 * This file implements (a).
 *
 **************************************************************************
 *
 * Note that we have presently tested both TotalView and DDT parallel
 * debuggers.  They both nominally subscribe to the Etnus attaching
 * interface, but there are differences between the two.
 *
 * TotalView: user launches "totalview mpirun -a ...<mpirun args>...".
 * TV launches mpirun.  mpirun launches the application and then calls
 * MPIR_Breakpoint().  This is the signal to TV that it's a parallel
 * MPI job.  TV then reads the proctable in mpirun and attaches itself
 * to all the processes (it takes care of launching itself on the
 * remote nodes).  Upon attaching to all the MPI processes, the
 * variable MPIR_being_debugged is set to 1.  When it has finished
 * attaching itself to all the MPI processes that it wants to,
 * MPIR_Breakpoint() returns.
 *
 * DDT: user launches "ddt bin -np X <mpi app name>".  DDT fork/exec's
 * mpirun to launch ddt-debugger on the back-end nodes via "mpirun -np
 * X ddt-debugger" (not the lack of other arguments -- we can't pass
 * anything to mpirun).  This app will eventually fork/exec the MPI
 * app.  DDT does not current set MPIR_being_debugged in the MPI app.
 *
 **************************************************************************
 *
 * We support two ways of waiting for attaching debuggers.  The
 * implementation spans this file and ompi/debuggers/ompi_debuggers.c.
 *
 * 1. If using orterun: MPI processes will have the
 * orte_in_parallel_debugger MCA param set to true (because not all
 * debuggers consistently set MPIR_being_debugged in both the launcher
 * and in the MPI procs).  The HNP will call MPIR_Breakpoint() and
 * then RML send a message to VPID 0 (MCW rank 0) when it returns
 * (MPIR_Breakpoint() doesn't return until the debugger has attached
 * to all relevant processes).  Meanwhile, VPID 0 blocks waiting for
 * the RML message.  All other VPIDs immediately call the grpcomm
 * barrier (and therefore block until the debugger attaches).  Once
 * VPID 0 receives the RML message, we know that the debugger has
 * attached to all processes that it cares about, and VPID 0 then
 * joins the grpcomm barrier, allowing the job to continue.  This
 * scheme has the side effect of nicely supporting partial attaches by
 * parallel debuggers (i.e., attaching to only some of the MPI
 * processes; not necessarily all of them).
 *
 * 2. If not using orterun: in this case, we know that there will not be an RML message
 * sent to VPID 0.  So we have to look for a magic environment
 * variable from the launcher to know if the jobs will be attached by
 * a debugger (e.g., set by yod, srun, ...etc.), and if so, spin on
 * MPIR_debug_gate.  These environment variable names must be
 * hard-coded in the OMPI layer (see ompi/debuggers/ompi_debuggers.c).
 */

/* local globals and functions */
#define DUMP_INT(X) fprintf(stderr, "  %s = %d\n", # X, X);
#define FILE_MODE (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)

struct MPIR_PROCDESC {
    char *host_name;        /* something that can be passed to inet_addr */
    char *executable_name;  /* name of binary */
    int pid;                /* process pid */
};


static void orte_debugger_dump(void)
{
    int i;

    DUMP_INT(MPIR_being_debugged);
    DUMP_INT(MPIR_debug_state);
    DUMP_INT(MPIR_partial_attach_ok);
    DUMP_INT(MPIR_i_am_starter);
    DUMP_INT(MPIR_forward_output);
    DUMP_INT(MPIR_proctable_size);
    fprintf(stderr, "  MPIR_proctable:\n");
    for (i = 0; i < MPIR_proctable_size; i++) {
        fprintf(stderr,
                "    (i, host, exe, pid) = (%d, %s, %s, %d)\n",
                i,
                MPIR_proctable[i].host_name,
                MPIR_proctable[i].executable_name,
                MPIR_proctable[i].pid);
    }
    fprintf(stderr, "MPIR_executable_path: %s\n",
            ('\0' == MPIR_executable_path[0]) ?
            "NULL" : (char*) MPIR_executable_path);
    fprintf(stderr, "MPIR_server_arguments: %s\n",
            ('\0' == MPIR_server_arguments[0]) ?
            "NULL" : (char*) MPIR_server_arguments);
}

/**
 * Initialization of data structures for running under a debugger
 * using the MPICH/TotalView parallel debugger interface.  Before the
 * spawn we need to check if we are being run under a TotalView-like
 * debugger; if so then inform applications via an MCA parameter.
 */
static void orte_debugger_init_before_spawn(orte_job_t *jdata)
{
    char *env_name;
    orte_app_context_t *app;
    int i;
    char *attach_fifo;

    if (!MPIR_being_debugged && !orte_in_parallel_debugger) {
        /* if we were given a test debugger, then we still want to
         * colaunch it
         */
        if (NULL != orte_debugger_test_daemon) {
            opal_output_verbose(2, orte_debug_output,
                                "%s No debugger test daemon specified",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            goto launchit;
        }
        /* if we were given an auto-detect rate, then we want to setup
         * an event so we periodically do the check
         */
        if (0 < orte_debugger_check_rate) {
            opal_output_verbose(2, orte_debug_output,
                                "%s Setting debugger attach check rate for %d seconds",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                orte_debugger_check_rate);
            ORTE_TIMER_EVENT(orte_debugger_check_rate, 0, attach_debugger, ORTE_SYS_PRI);
        } else if (orte_create_session_dirs) {
            /* create the attachment FIFO and setup readevent - cannot be
             * done if no session dirs exist!
             */
            attach_fifo = opal_os_path(false, orte_process_info.job_session_dir, "debugger_attach_fifo", NULL);
            if ((mkfifo(attach_fifo, FILE_MODE) < 0) && errno != EEXIST) {
                opal_output(0, "CANNOT CREATE FIFO %s: errno %d", attach_fifo, errno);
                free(attach_fifo);
                return;
            }
            strncpy(MPIR_attach_fifo, attach_fifo, MPIR_MAX_PATH_LENGTH - 1);
        free(attach_fifo);
        open_fifo();
        }
        return;
    }

 launchit:
    opal_output_verbose(1, orte_debug_output, "Info: Spawned by a debugger");

    /* tell the procs they are being debugged */
    (void) mca_base_var_env_name ("orte_in_parallel_debugger", &env_name);

    for (i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        opal_setenv(env_name, "1", true, &app->env);
    }
    free(env_name);
}

static void setup_debugger_job(void)
{
    orte_job_t *debugger;
    orte_app_context_t *app;
    orte_proc_t *proc;
    int i, rc;
    orte_node_t *node;
    orte_vpid_t vpid=0;
    char cwd[OPAL_PATH_MAX];

    /* setup debugger daemon job */
    debugger = OBJ_NEW(orte_job_t);
    /* create a jobid for these daemons - this is done solely
     * to avoid confusing the rest of the system's bookkeeping
     */
    orte_plm_base_create_jobid(debugger);
    /* flag the job as being debugger daemons */
    ORTE_FLAG_SET(debugger, ORTE_JOB_FLAG_DEBUGGER_DAEMON);
    /* unless directed, we do not forward output */
    if (!MPIR_forward_output) {
        ORTE_FLAG_SET(debugger, ORTE_JOB_FLAG_FORWARD_OUTPUT);
    }
    /* dont push stdin */
    debugger->stdin_target = ORTE_VPID_INVALID;
    /* add it to the global job pool */
    opal_hash_table_set_value_uint32(orte_job_data, debugger->jobid, debugger);
    /* create an app_context for the debugger daemon */
    app = OBJ_NEW(orte_app_context_t);
    if (NULL != orte_debugger_test_daemon) {
        app->app = strdup(orte_debugger_test_daemon);
    } else {
        app->app = strdup((char*)MPIR_executable_path);
    }
    /* don't currently have an option to pass the debugger
     * cwd - probably should add one someday
     */
    if (OPAL_SUCCESS != (rc = opal_getcwd(cwd, sizeof(cwd)))) {
        orte_show_help("help-orterun.txt", "orterun:init-failure",
                       true, "get the cwd", rc);
        return;
    }
    app->cwd = strdup(cwd);
    orte_remove_attribute(&app->attributes, ORTE_APP_USER_CWD);
    opal_argv_append_nosize(&app->argv, app->app);
    build_debugger_args(app);
    opal_pointer_array_add(debugger->apps, app);
    debugger->num_apps = 1;
    /* create a job map */
    debugger->map = OBJ_NEW(orte_job_map_t);
    /* in building the map, we want to launch one debugger daemon
     * on each node that *already has an application process on it*.
     * We cannot just launch one debugger daemon on EVERY node because
     * the original job may not have placed procs on every node. So
     * we construct the map here by cycling across all nodes, adding
     * only those nodes where num_procs > 0.
     */
    for (i=0; i < orte_node_pool->size; i++) {
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
            continue;
        }
        /* if this node wasn't included in the vm, ignore it */
        if (NULL == node->daemon) {
            continue;
        }
        /* if the node doesn't have any app procs on it, ignore it */
        if (node->num_procs < 1) {
            continue;
        }
        /* this node has at least one proc, so add it to our map */
        OBJ_RETAIN(node);
        opal_pointer_array_add(debugger->map->nodes, node);
        debugger->map->num_nodes++;
        /* add a debugger daemon to the node - note that the
         * debugger daemon does NOT count against our subscribed slots
         */
        proc = OBJ_NEW(orte_proc_t);
        proc->name.jobid = debugger->jobid;
        proc->name.vpid = vpid++;
        /* set the local/node ranks - we don't actually care
         * what these are, but the odls needs them
         */
        proc->local_rank = 0;
        proc->node_rank = 0;
        proc->app_rank = proc->name.vpid;
        /* flag the proc as ready for launch */
        proc->state = ORTE_PROC_STATE_INIT;
        proc->app_idx = 0;

        OBJ_RETAIN(node);  /* maintain accounting on object */
        proc->node = node;
        /* add the proc to the job */
        opal_pointer_array_set_item(debugger->procs, proc->name.vpid, proc);
        debugger->num_procs++;

        /* add the proc to the node's array */
        OBJ_RETAIN(proc);
        opal_pointer_array_add(node->procs, (void*)proc);
        node->num_procs++;
    }
    /* schedule it for launch */
    debugger->state = ORTE_JOB_STATE_INIT;
    ORTE_ACTIVATE_JOB_STATE(debugger, ORTE_JOB_STATE_LAUNCH_APPS);
}

static bool mpir_breakpoint_fired = false;

static void _send_notification(void)
{
    opal_buffer_t buf;
    int status = OPAL_ERR_DEBUGGER_RELEASE;
    orte_grpcomm_signature_t sig;
    int rc;

    OBJ_CONSTRUCT(&buf, opal_buffer_t);

    /* pack the debugger_attached status */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &status, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return;
    }
    status = 0;

    /* notify all procs */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &status, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return;
    }

    /* all procs are impacted */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &status, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return;
    }

    /* no further info to provide */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &status, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return;
    }

    /* xcast it to everyone */
    OBJ_CONSTRUCT(&sig, orte_grpcomm_signature_t);
    sig.signature = (orte_process_name_t*)malloc(sizeof(orte_process_name_t));
    sig.signature[0].jobid = ORTE_PROC_MY_NAME->jobid;
    sig.signature[0].vpid = ORTE_VPID_WILDCARD;
    sig.sz = 1;

    if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(&sig, ORTE_RML_TAG_NOTIFICATION, &buf))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&sig);
    OBJ_DESTRUCT(&buf);
}

/*
 * Initialization of data structures for running under a debugger
 * using the MPICH/TotalView parallel debugger interface. This stage
 * of initialization must occur after spawn
 *
 * NOTE: We -always- perform this step to ensure that any debugger
 * that attaches to us post-launch of the application can get a
 * completed proctable
 */
void orte_debugger_init_after_spawn(int fd, short event, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_job_t *jdata = caddy->jdata;
    orte_proc_t *proc;
    orte_app_context_t *appctx;
    orte_vpid_t i, j;
    char **aliases, *aptr;

    /* if we couldn't get thru the mapper stage, we might
     * enter here with no procs. Avoid the "zero byte malloc"
     * message by checking here
     */
    if (MPIR_proctable || 0 == jdata->num_procs) {
        /* already initialized */
        opal_output_verbose(5, orte_debug_output,
                            "%s: debugger already initialized or zero procs",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        OBJ_RELEASE(caddy);
        if (!mpir_breakpoint_fired) {
            /* record that we have triggered the debugger */
            mpir_breakpoint_fired = true;

            /* trigger the debugger */
            MPIR_Breakpoint();

            /* notify all procs that the debugger is ready */
            _send_notification();
        }
        return;
    }

    /* fill in the proc table for the application processes */

    opal_output_verbose(5, orte_debug_output,
                        "%s: Setting up debugger process table for applications",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    MPIR_debug_state = 1;

    /* set the total number of processes in the job */
    MPIR_proctable_size = jdata->num_procs;

    /* allocate MPIR_proctable */
    MPIR_proctable = (struct MPIR_PROCDESC *)malloc(sizeof(struct MPIR_PROCDESC) *
                                                    MPIR_proctable_size);
    if (MPIR_proctable == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(caddy);
        return;
    }

    if (orte_debugger_dump_proctable) {
        opal_output(orte_clean_output, "MPIR Proctable for job %s", ORTE_JOBID_PRINT(jdata->jobid));
    }

    /* initialize MPIR_proctable */
    for (j=0; j < jdata->num_procs; j++) {
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, j))) {
            continue;
        }
        /* store this data in the location whose index
         * corresponds to the proc's rank
         */
        i = proc->name.vpid;
        if (NULL == (appctx = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, proc->app_idx))) {
            continue;
        }

        /* take the indicated alias as the hostname, if aliases exist */
        if (orte_retain_aliases) {
            aliases = NULL;
            aptr = NULL;
            if (orte_get_attribute(&proc->node->attributes, ORTE_NODE_ALIAS, (void**)&aptr, OPAL_STRING)) {
                aliases = opal_argv_split(aptr, ',');
                free(aptr);
                if (orte_use_hostname_alias <= opal_argv_count(aliases)) {
                    MPIR_proctable[i].host_name = strdup(aliases[orte_use_hostname_alias-1]);
                }
                opal_argv_free(aliases);
            }
        } else {
            /* just use the default name */
            MPIR_proctable[i].host_name = strdup(proc->node->name);
        }

        if ( 0 == strncmp(appctx->app, OPAL_PATH_SEP, 1 )) {
            MPIR_proctable[i].executable_name =
                opal_os_path( false, appctx->app, NULL );
        } else {
            MPIR_proctable[i].executable_name =
                opal_os_path( false, appctx->cwd, appctx->app, NULL );
        }
        MPIR_proctable[i].pid = proc->pid;
        if (orte_debugger_dump_proctable) {
            opal_output(orte_clean_output, "%s: Host %s Exe %s Pid %d",
                        ORTE_VPID_PRINT(i), MPIR_proctable[i].host_name,
                        MPIR_proctable[i].executable_name, MPIR_proctable[i].pid);
        }
    }

    if (0 < opal_output_get_verbosity(orte_debug_output)) {
        orte_debugger_dump();
    }

    /* if we are being launched under a debugger, then we must wait
     * for it to be ready to go and do some things to start the job
     */
    if (MPIR_being_debugged || NULL != orte_debugger_test_daemon) {
        /* if we are not launching debugger daemons, then trigger
         * the debugger - otherwise, we need to wait for the debugger
         * daemons to be started
         */
        if ('\0' == MPIR_executable_path[0] && NULL == orte_debugger_test_daemon) {
            /* record that we have triggered the debugger */
            mpir_breakpoint_fired = true;

            /* trigger the debugger */
            MPIR_Breakpoint();

            /* notify all procs that the debugger is ready */
            _send_notification();
        } else {
            /* if I am launching debugger daemons, then I need to do so now
             * that the job has been started and I know which nodes have
             * apps on them
             */
            opal_output_verbose(2, orte_debug_output,
                                "%s Cospawning debugger daemons %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                (NULL == orte_debugger_test_daemon) ?
                                MPIR_executable_path : orte_debugger_test_daemon);
            setup_debugger_job();
        }
        /* we don't have anything else to do */
        OBJ_RELEASE(caddy);
        return;
    }

    /* if we are not being debugged, then just cleanup and depart */
    OBJ_RELEASE(caddy);
}

static void orte_debugger_detached(int fd, short event, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    OBJ_RELEASE(caddy);

    /* need to ensure MPIR_Breakpoint is called again if another debugger attaches */
    mpir_breakpoint_fired = false;
}

static void open_fifo (void)
{
    if (attach_fd > 0) {
        close(attach_fd);
    }

    attach_fd = open(MPIR_attach_fifo, O_RDONLY | O_NONBLOCK, 0);
    if (attach_fd < 0) {
        opal_output(0, "%s unable to open debugger attach fifo",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        return;
    }

    /* Set this fd to be close-on-exec so that children don't see it */
    if (opal_fd_set_cloexec(attach_fd) != OPAL_SUCCESS) {
        opal_output(0, "%s unable to set debugger attach fifo to CLOEXEC",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        close(attach_fd);
        attach_fd = -1;
        return;
    }

    opal_output_verbose(2, orte_debug_output,
                        "%s Monitoring debugger attach fifo %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        MPIR_attach_fifo);
    attach = (opal_event_t*)malloc(sizeof(opal_event_t));
    opal_event_set(orte_event_base, attach, attach_fd, OPAL_EV_READ, attach_debugger, attach);

    fifo_active = true;
    opal_event_add(attach, 0);
}

static void attach_debugger(int fd, short event, void *arg)
{
    unsigned char fifo_cmd;
    int rc;
    orte_timer_t *tm;
    opal_event_t *attach;

    if (fifo_active) {
        attach = (opal_event_t*)arg;
        fifo_active = false;

        rc = read(attach_fd, &fifo_cmd, sizeof(fifo_cmd));
        if (!rc) {
            /* release the current event */
            opal_event_free(attach);
        /* reopen device to clear hangup */
            open_fifo();
            return;
        }
        if (1 != fifo_cmd) {
            /* ignore the cmd */
            fifo_active = true;
            opal_event_add(attach, 0);
            return;
        }
    }

    if (!MPIR_being_debugged && !orte_debugger_test_attach) {
        /* false alarm - reset the read or timer event */
        if (0 == orte_debugger_check_rate) {
            fifo_active = true;
            opal_event_add(attach, 0);
        } else if (!MPIR_being_debugged) {
            tm = (orte_timer_t*)arg;
            /* re-add the event */
            opal_event_evtimer_add(tm->ev, &tm->tv);
        }
        return;
    }

    opal_output_verbose(1, orte_debug_output,
                        "%s Attaching debugger %s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == orte_debugger_test_daemon) ? MPIR_executable_path : orte_debugger_test_daemon);

    /* a debugger has attached! All the MPIR_Proctable
     * data is already available, so we only need to
     * check to see if we should spawn any daemons
     */
     if ('\0' != MPIR_executable_path[0] || NULL != orte_debugger_test_daemon) {
        opal_output_verbose(2, orte_debug_output,
                            "%s Spawning debugger daemons %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            (NULL == orte_debugger_test_daemon) ?
                            MPIR_executable_path : orte_debugger_test_daemon);
        setup_debugger_job();
    }

    /* reset the read or timer event */
    if (0 == orte_debugger_check_rate) {
        fifo_active = true;
        opal_event_add(attach, 0);
    } else if (!MPIR_being_debugged) {
        tm = (orte_timer_t*)arg;
        /* re-add the event */
        opal_event_evtimer_add(tm->ev, &tm->tv);
    }
}

static void build_debugger_args(orte_app_context_t *debugger)
{
    int i, j;
    char mpir_arg[MPIR_MAX_ARG_LENGTH];

    if ('\0' != MPIR_server_arguments[0]) {
        j=0;
        memset(mpir_arg, 0, MPIR_MAX_ARG_LENGTH);
        for (i=0; i < MPIR_MAX_ARG_LENGTH; i++) {
            if (MPIR_server_arguments[i] == '\0') {
                if (0 < j) {
                    opal_argv_append_nosize(&debugger->argv, mpir_arg);
                    memset(mpir_arg, 0, MPIR_MAX_ARG_LENGTH);
                    j=0;
                }
            } else {
                mpir_arg[j] = MPIR_server_arguments[i];
                j++;
            }
        }
    }
}

void orte_timeout_wakeup(int sd, short args, void *cbdata)
{
    char *tm;

    /* this function gets called when the job execution time
     * has hit a prescribed limit - so just abort
     */
    tm = getenv("MPIEXEC_TIMEOUT");
    orte_show_help("help-orterun.txt", "orterun:timeout",
                   true, (NULL == tm) ? "NULL" : tm);
    ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
    /* abort the job */
    ORTE_ACTIVATE_JOB_STATE(NULL, ORTE_JOB_STATE_ALL_JOBS_COMPLETE);
    /* set the global abnormal exit flag  */
    orte_abnormal_term_ordered = true;
}
