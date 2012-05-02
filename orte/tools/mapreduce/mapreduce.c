/* -*- C -*-
 *
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
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2009 Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2007-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /* HAVE_STDLIB_H */
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
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/base/base.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/basename.h"
#include "opal/util/cmd_line.h"
#include "opal/util/opal_environ.h"
#include "opal/util/opal_getcwd.h"
#include "opal/util/show_help.h"
#include "opal/sys/atomic.h"
#if OPAL_ENABLE_FT_CR == 1
#include "opal/runtime/opal_cr.h"
#endif

#include "opal/version.h"
#include "opal/runtime/opal.h"
#include "opal/util/os_path.h"
#include "opal/util/path.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"

#include "orte/util/proc_info.h"
#include "orte/util/pre_condition_transports.h"
#include "orte/util/session_dir.h"
#include "orte/util/hnp_contact.h"
#include "orte/util/show_help.h"

#include "orte/mca/odls/odls.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/ras/ras.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/errmgr_private.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/state/state.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_data_server.h"
#include "orte/runtime/orte_locks.h"
#include "orte/runtime/orte_quit.h"

/* ensure I can behave like a daemon */
#include "orte/orted/orted.h"

/**
 * Global struct for catching mapreduce command line options.
 */
struct mapreduce_globals_t {
    bool help;
    bool version;
    bool verbose;
    char *report_pid;
    char *report_uri;
    bool exit;
    bool debugger;
    int num_procs;
    char *env_val;
    char *appfile;
    char *wdir;
    char *path;
    char *preload_files;
    char *preload_files_dest_dir;
    opal_mutex_t lock;
    bool sleep;
    char *ompi_server;
    bool wait_for_server;
    int server_wait_timeout;
    char *stdin_target;
    char *prefix;
    char *path_to_mpirun;
#if OPAL_ENABLE_FT_CR == 1
    char *sstore_load;
#endif
    bool disable_recovery;
    bool mapper;
    bool reducer;
    bool combiner;
    bool single_job;
    orte_job_t *combiner_job;
};

/*
 * Globals
 */
static char **global_mca_env = NULL;
static orte_std_cntr_t total_num_apps = 0;
static bool want_prefix_by_default = (bool) ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT;
static char *ompi_server=NULL;
/* maintain a local array of job "chains" - each chain starts with a
 * mapper and can contain any number of reducer jobs, each operating
 * in a sequential chain in that the output of one stage is fed to
 * the input of the next.
 */
static opal_pointer_array_t chains;

/*
 * Globals
 */
struct mapreduce_globals_t mapreduce_globals;
static bool globals_init = false;

static opal_cmd_line_init_t cmd_line_init[] = {
    /* Various "obvious" options */
    { NULL, NULL, NULL, 'h', NULL, "help", 0,
      &mapreduce_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },
    { NULL, NULL, NULL, 'V', NULL, "version", 0,
      &mapreduce_globals.version, OPAL_CMD_LINE_TYPE_BOOL,
      "Print version and exit" },
    { NULL, NULL, NULL, 'v', NULL, "verbose", 0,
      &mapreduce_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Be verbose" },
    { "orte", "execute", "quiet", 'q', NULL, "quiet", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Suppress helpful messages" },
    { NULL, NULL, NULL, '\0', "report-pid", "report-pid", 1,
      &mapreduce_globals.report_pid, OPAL_CMD_LINE_TYPE_STRING,
      "Printout pid on stdout [-], stderr [+], or a file [anything else]" },
    { NULL, NULL, NULL, '\0', "report-uri", "report-uri", 1,
      &mapreduce_globals.report_uri, OPAL_CMD_LINE_TYPE_STRING,
      "Printout URI on stdout [-], stderr [+], or a file [anything else]" },
    
    /* exit status reporting */
    { "orte", "report", "child_jobs_separately", '\0', "report-child-jobs-separately", "report-child-jobs-separately", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Return the exit status of the primary job only" },
    
    /* hetero apps */
    { "orte", "hetero", "apps", '\0', NULL, "hetero-apps", 0,
        NULL, OPAL_CMD_LINE_TYPE_BOOL,
    "Indicates that multiple app_contexts are being provided that are a mix of 32/64 bit binaries" },
    
    /* select XML output */
    { "orte", "xml", "output", '\0', "xml", "xml", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Provide all output in XML format" },
    { "orte", "xml", "file", '\0', "xml-file", "xml-file", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide all output in XML format to the specified file" },

    /* tag output */
    { "orte", "tag", "output", '\0', "tag-output", "tag-output", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Tag all output with [job,rank]" },
    { "orte", "timestamp", "output", '\0', "timestamp-output", "timestamp-output", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Timestamp all application process output" },
    { "orte", "output", "filename", '\0', "output-filename", "output-filename", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Redirect output from application processes into filename.rank" },
    { "orte", "xterm", NULL, '\0', "xterm", "xterm", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Create a new xterm window and display output from the specified ranks there" },

    /* ===============  MAPREDUCE OPTIONS =============== */
    /* specify input files */
    { "iof", "base", "input_files", '\0', "input-files", "input-files", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Comma-separated list of input files to be read and sent to stdin of procs" },

    /* mapper designation */
    { NULL, NULL, NULL, '\0', "mapper", "mapper", 0,
      &mapreduce_globals.mapper, OPAL_CMD_LINE_TYPE_BOOL,
      "Mapper application" },

    /* reducer designation */
    { NULL, NULL, NULL, '\0', "reducer", "reducer", 0,
      &mapreduce_globals.reducer, OPAL_CMD_LINE_TYPE_BOOL,
      "Reducer application" },

    /* combiner designation */
    { NULL, NULL, NULL, '\0', "combiner", "combiner", 0,
      &mapreduce_globals.combiner, OPAL_CMD_LINE_TYPE_BOOL,
      "Combiner application" },

    /* ================================================== */

    /* Specify the launch agent to be used */
    { "orte", "launch", "agent", '\0', "launch-agent", "launch-agent", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Command used to start processes on remote nodes (default: orted)" },
    
    /* Preload the binary on the remote machine */
    { "orte", "preload", "binaries", 's', NULL, "preload-binary", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Preload the binary on the remote machine before starting the remote process." },

    /* Preload files on the remote machine */
    { NULL, NULL, NULL, '\0', NULL, "preload-files", 1,
      &mapreduce_globals.preload_files, OPAL_CMD_LINE_TYPE_STRING,
      "Preload the comma separated list of files to the remote machines current working directory before starting the remote process." },

    /* Where to Preload files on the remote machine */
    { NULL, NULL, NULL, '\0', NULL, "preload-files-dest-dir", 1,
      &mapreduce_globals.preload_files_dest_dir, OPAL_CMD_LINE_TYPE_STRING,
      "The destination directory to use in conjunction with --preload-files. By default the absolute and relative paths provided by --preload-files are used." },

    /* Use an appfile */
    { NULL, NULL, NULL, '\0', NULL, "app", 1,
      &mapreduce_globals.appfile, OPAL_CMD_LINE_TYPE_STRING,
      "Provide an appfile; ignore all other command line options" },

    /* Number of processes; -c, -n, --n, -np, and --np are all
       synonyms */
    { NULL, NULL, NULL, 'c', "np", "np", 1,
      &mapreduce_globals.num_procs, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run" },
    { NULL, NULL, NULL, '\0', "n", "n", 1,
      &mapreduce_globals.num_procs, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run" },
    
    /* Set a hostfile */
    { NULL, NULL, NULL, '\0', "hostfile", "hostfile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile" },
    { NULL, NULL, NULL, '\0', "machinefile", "machinefile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile" },
    { "orte", "default", "hostfile", '\0', "default-hostfile", "default-hostfile", 1,
        NULL, OPAL_CMD_LINE_TYPE_STRING,
    "Provide a default hostfile" },
    { "opal", "if", "do_not_resolve", '\0', "do-not-resolve", "do-not-resolve", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not attempt to resolve interfaces" },
    
    /* uri of Open MPI server, or at least where to get it */
    { NULL, NULL, NULL, '\0', "ompi-server", "ompi-server", 1,
      &mapreduce_globals.ompi_server, OPAL_CMD_LINE_TYPE_STRING,
      "Specify the URI of the Open MPI server, or the name of the file (specified as file:filename) that contains that info" },
    { NULL, NULL, NULL, '\0', "wait-for-server", "wait-for-server", 0,
      &mapreduce_globals.wait_for_server, OPAL_CMD_LINE_TYPE_BOOL,
      "If ompi-server is not already running, wait until it is detected (default: false)" },
    { NULL, NULL, NULL, '\0', "server-wait-time", "server-wait-time", 1,
      &mapreduce_globals.server_wait_timeout, OPAL_CMD_LINE_TYPE_INT,
      "Time in seconds to wait for ompi-server (default: 10 sec)" },
    
    { "carto", "file", "path", '\0', "cf", "cartofile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a cartography file" },

    { "orte", "rankfile", NULL, '\0', "rf", "rankfile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a rankfile file" },

    /* Export environment variables; potentially used multiple times,
       so it does not make sense to set into a variable */
    { NULL, NULL, NULL, 'x', NULL, NULL, 1,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      "Export an environment variable, optionally specifying a value (e.g., \"-x foo\" exports the environment variable foo and takes its value from the current environment; \"-x foo=bar\" exports the environment variable name foo and sets its value to \"bar\" in the started processes)" },

      /* Mapping controls */
    { "rmaps", "base", "display_map", '\0', "display-map", "display-map", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Display the process map just before launch"},
    { "rmaps", "base", "display_devel_map", '\0', "display-devel-map", "display-devel-map", 0,
       NULL, OPAL_CMD_LINE_TYPE_BOOL,
       "Display a detailed process map (mostly intended for developers) just before launch"},
    { "rmaps", "base", "display_topo_with_map", '\0', "display-topo", "display-topo", 0,
       NULL, OPAL_CMD_LINE_TYPE_BOOL,
       "Display the topology as part of the process map (mostly intended for developers) just before launch"},
    { "rmaps", "base", "display_diffable_map", '\0', "display-diffable-map", "display-diffable-map", 0,
       NULL, OPAL_CMD_LINE_TYPE_BOOL,
       "Display a diffable process map (mostly intended for developers) just before launch"},
    { NULL, NULL, NULL, 'H', "host", "host", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "List of hosts to invoke processes on" },
    { "rmaps", "base", "no_schedule_local", '\0', "nolocal", "nolocal", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not run any applications on the local node" },
    { "rmaps", "base", "no_oversubscribe", '\0', "nooversubscribe", "nooversubscribe", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Nodes are not to be oversubscribed, even if the system supports such operation"},
    { "rmaps", "base", "oversubscribe", '\0', "oversubscribe", "oversubscribe", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Nodes are allowed to be oversubscribed, even on a managed system"},
#if 0
    { "rmaps", "base", "cpus_per_rank", '\0', "cpus-per-proc", "cpus-per-proc", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Number of cpus to use for each process [default=1]" },
    { "rmaps", "base", "cpus_per_rank", '\0', "cpus-per-rank", "cpus-per-rank", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Synonym for cpus-per-proc" },
#endif

#if OPAL_HAVE_HWLOC
    /* declare hardware threads as independent cpus */
    { "hwloc", "base", "use_hwthreads_as_cpus", '\0', "use-hwthread-cpus", "use-hwthread-cpus", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Use hardware threads as independent cpus" },

      /* Binding options */
    { "hwloc", "base", "binding_policy", '\0', NULL, "bind-to", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Policy for binding processes [none (default) | hwthread | core | socket | numa | board] (supported qualifiers: overload-allowed,if-supported)" },

    /* backward compatiblity */
    { "hwloc", "base", "bind_to_core", '\0', "bind-to-core", "bind-to-core", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Bind processes to cores" },
    { "hwloc", "base", "bind_to_socket", '\0', "bind-to-socket", "bind-to-socket", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Bind processes to sockets" },

    { "hwloc", "base", "report_bindings", '\0', "report-bindings", "report-bindings", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to report process bindings to stderr" },

    /* slot list option */
    { "hwloc", "base", "slot_list", '\0', "slot-list", "slot-list", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "List of processor IDs to bind processes to [default=NULL]"},

    /* generalized pattern mapping option */
    { "rmaps", "ppr", "pattern", '\0', NULL, "ppr", 1,
        NULL, OPAL_CMD_LINE_TYPE_STRING,
        "Comma-separated list of number of processes on a given resource type [default: none]" },
#endif

    /* Allocation options */
    { "ras", "base", "display_alloc", '\0', "display-allocation", "display-allocation", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Display the allocation being used by this job"},
    { "ras", "base", "display_devel_alloc", '\0', "display-devel-allocation", "display-devel-allocation", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Display a detailed list (mostly intended for developers) of the allocation being used by this job"},
#if OPAL_HAVE_HWLOC
    { "hwloc", "base", "cpu_set", '\0', "cpu-set", "cpu-set", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Comma-separated list of ranges specifying logical cpus allocated to this job [default: none]"},
#endif
    { NULL, NULL, NULL, 'H', "host", "host", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "List of hosts to invoke processes on" },

    /* mpiexec-like arguments */
    { NULL, NULL, NULL, '\0', "wdir", "wdir", 1,
      &mapreduce_globals.wdir, OPAL_CMD_LINE_TYPE_STRING,
      "Set the working directory of the started processes" },
    { NULL, NULL, NULL, '\0', "wd", "wd", 1,
      &mapreduce_globals.wdir, OPAL_CMD_LINE_TYPE_STRING,
      "Synonym for --wdir" },
    { NULL, NULL, NULL, '\0', "path", "path", 1,
      &mapreduce_globals.path, OPAL_CMD_LINE_TYPE_STRING,
      "PATH to be used to look for executables to start processes" },

    /* OpenRTE arguments */
    { "orte", "debug", NULL, 'd', "debug-devel", "debug-devel", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE" },
    
    { "orte", "debug", "daemons", '\0', "debug-daemons", "debug-daemons", 0,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Enable debugging of any OpenRTE daemons used by this application" },
    
    { "orte", "debug", "daemons_file", '\0', "debug-daemons-file", "debug-daemons-file", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of any OpenRTE daemons used by this application, storing output in files" },
    
    { "orte", "leave", "session_attached", '\0', "leave-session-attached", "leave-session-attached", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE" },

    { "orte", "do_not", "launch", '\0', "do-not-launch", "do-not-launch", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Perform all necessary operations to prepare to launch the application, but do not actually launch it" },
    
    { NULL, NULL, NULL, '\0', NULL, "prefix", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Prefix where Open MPI is installed on remote nodes" },
    { NULL, NULL, NULL, '\0', NULL, "noprefix", 0,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Disable automatic --prefix behavior" },

    { "orte", "report", "launch_progress", '\0', "show-progress", "show-progress", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Output a brief periodic report on launch progress" },

    { "orte", "use", "regexp", '\0', "use-regexp", "use-regexp", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Use regular expressions for launch" },

    { "orte", "report", "events", '\0', "report-events", "report-events", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Report events to a tool listening at the specified URI" },

    { "orte", "enable", "recovery", '\0', "enable-recovery", "enable-recovery", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable recovery from process failure [Default = disabled]" },

    { "orte", "max", "restarts", '\0', "max-restarts", "max-restarts", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Max number of times to restart a failed process" },

#if OPAL_HAVE_HWLOC
    { "orte", "hetero", "nodes", '\0', NULL, "hetero-nodes", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Nodes in cluster may differ in topology, so send the topology back from each node [Default = false]" },
#endif

    { NULL, NULL, NULL, '\0', "disable-recovery", "disable-recovery", 0,
      &mapreduce_globals.disable_recovery, OPAL_CMD_LINE_TYPE_BOOL,
      "Disable recovery (resets all recovery options to off)" },

    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

/*
 * Local functions
 */
static int create_app(int argc, char* argv[],
                      orte_app_context_t **app,
                      bool *made_app, char ***app_env,
                      orte_job_controls_t *jtype);
static int init_globals(void);
static int parse_globals(int argc, char* argv[], opal_cmd_line_t *cmd_line);
static int parse_locals(int argc, char* argv[]);
static int parse_appfile(char *filename, char ***env);
static void do_wireup(int fd, short sd, void *cbdata);

int main(int argc, char *argv[])
{
    int rc, i, j;
    opal_cmd_line_t cmd_line;
    char *param;
    orte_job_t *daemons;
    orte_app_context_t *app, *dapp;
    orte_job_t *jdata=NULL;
    opal_list_t *chain;
    opal_list_item_t *item;

    /* find our basename (the name of the executable) so that we can
       use it in pretty-print error messages */
    orte_basename = opal_basename(argv[0]);

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

    /*
     * Since this process can now handle MCA/GMCA parameters, make sure to
     * process them.
     */
    mca_base_cmd_line_process_args(&cmd_line, &environ, &environ);
    
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
    if (OPAL_SUCCESS != opal_init_util(&argc, &argv)) {
        exit(1);
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
    mapreduce_globals.prefix = NULL;
    mapreduce_globals.path_to_mpirun = NULL;
    if (opal_cmd_line_is_taken(&cmd_line, "prefix") ||
        '/' == argv[0][0] || want_prefix_by_default) {
        size_t param_len;

        if ('/' == argv[0][0]) {
            char* tmp_basename = NULL;
            /* If they specified an absolute path, strip off the
               /bin/<exec_name>" and leave just the prefix */
            mapreduce_globals.path_to_mpirun = opal_dirname(argv[0]);
            /* Quick sanity check to ensure we got
               something/bin/<exec_name> and that the installation
               tree is at least more or less what we expect it to
               be */
            tmp_basename = opal_basename(mapreduce_globals.path_to_mpirun);
            if (0 == strcmp("bin", tmp_basename)) {
                char* tmp = mapreduce_globals.path_to_mpirun;
                mapreduce_globals.path_to_mpirun = opal_dirname(tmp);
                free(tmp);
            } else {
                free(mapreduce_globals.path_to_mpirun);
                mapreduce_globals.path_to_mpirun = NULL;
            }
            free(tmp_basename);
        }
        /* if both are given, check to see if they match */
        if (opal_cmd_line_is_taken(&cmd_line, "prefix") && NULL != mapreduce_globals.path_to_mpirun) {
            /* if they don't match, then that merits a warning */
            param = strdup(opal_cmd_line_get_param(&cmd_line, "prefix", 0, 0));
            if (0 != strcmp(param, mapreduce_globals.path_to_mpirun)) {
                orte_show_help("help-orterun.txt", "orterun:double-prefix",
                               true, orte_basename, param,
                               mapreduce_globals.path_to_mpirun, orte_basename);
                /* use the prefix over the path-to-mpirun so that
                 * people can specify the backend prefix as different
                 * from the local one
                 */
                free(mapreduce_globals.path_to_mpirun);
                mapreduce_globals.path_to_mpirun = NULL;
            } else {
                /* since they match, just use param */
                free(mapreduce_globals.path_to_mpirun);
            }
        } else if (NULL != mapreduce_globals.path_to_mpirun) {
            param = mapreduce_globals.path_to_mpirun;
        } else if (opal_cmd_line_is_taken(&cmd_line, "prefix")){
            /* must be --prefix alone */
            param = strdup(opal_cmd_line_get_param(&cmd_line, "prefix", 0, 0));
        } else {
            /* --enable-mapreduce-prefix-default was given to mapreduce */
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
                    return ORTE_ERR_FATAL;
                }
            }

            mapreduce_globals.prefix = strdup(param);
            free(param);
        }
        want_prefix_by_default = true;
    }

    /* flag that I am the HNP - needs to be done prior to
     * registering params
     */
    orte_process_info.proc_type = ORTE_PROC_HNP;

    /* Setup MCA params */
    orte_register_params();

    /***    NOTIFY IF DEPRECATED OPAL_PAFFINITY_ALONE WAS SET   ***/
    if (opal_paffinity_alone) {
        orte_show_help("help-opal-runtime.txt",
                       "opal_paffinity_alone:deprecated",
                       true);
    }

    /* Check for some "global" command line params */
    parse_globals(argc, argv, &cmd_line);
    OBJ_DESTRUCT(&cmd_line);

    /* Parse each app, adding it to a separate job in one or more MR-chains */
    OBJ_CONSTRUCT(&chains, opal_pointer_array_t);
    opal_pointer_array_init(&chains, 1, INT_MAX, 1);
    parse_locals(argc, argv);
    
    /* combine jobs as specified by user */
    if (mapreduce_globals.single_job) {
        jdata = OBJ_NEW(orte_job_t);
        for (i=0; i < chains.size; i++) {
            if (NULL == (chain = (opal_list_t*)opal_pointer_array_get_item(&chains, i))) {
                continue;
            }
            while (NULL != (item = opal_list_remove_first(chain))) {
                orte_job_t *jptr = (orte_job_t*)item;
                for (j=0; j < jptr->apps->size; j++) {
                    if (NULL != (app = (orte_app_context_t*)opal_pointer_array_get_item(jptr->apps, j))) {
                        opal_pointer_array_add(jdata->apps, app);
                        jdata->num_apps++;
                    }
                }
                OBJ_RELEASE(item);
            }
            OBJ_RELEASE(chain);
            opal_pointer_array_set_item(&chains, i, NULL);
        }
        chain = OBJ_NEW(opal_list_t);
        opal_list_append(chain, &jdata->super);
        opal_pointer_array_set_item(&chains, 0, chain);
        orte_num_jobs = 1;
    }

    /* save the environment for launch purposes. This MUST be
     * done so that we can pass it to any local procs we
     * spawn - otherwise, those local procs won't see any
     * non-MCA envars were set in the enviro prior to calling
     * mapreduce
     */
    orte_launch_environ = opal_argv_copy(environ);
    
    /* purge any ess flag set externally */
    opal_unsetenv("OMPI_MCA_ess", &orte_launch_environ);
    
    /* flag mapreduce operations */
    orte_map_reduce = true;

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
    /* finalize the OPAL utils. As they are opened again from orte_init->opal_init
     * we continue to have a reference count on them. So we have to finalize them twice...
     */
    opal_finalize_util();

    /* get the daemon job object */
    daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);

    /* check for request to report uri */
    if (NULL != mapreduce_globals.report_uri) {
        FILE *fp;
        char *rml_uri;
        rml_uri = orte_rml.get_contact_info();
        if (0 == strcmp(mapreduce_globals.report_uri, "-")) {
            /* if '-', then output to stdout */
            printf("%s\n",  (NULL == rml_uri) ? "NULL" : rml_uri);
        } else if (0 == strcmp(mapreduce_globals.report_uri, "+")) {
            /* if '+', output to stderr */
            fprintf(stderr, "%s\n",  (NULL == rml_uri) ? "NULL" : rml_uri);
        } else {
            fp = fopen(mapreduce_globals.report_uri, "w");
            if (NULL == fp) {
                orte_show_help("help-orterun.txt", "orterun:write_file", false,
                               orte_basename, "uri", mapreduce_globals.report_uri);
                exit(0);
            }
            fprintf(fp, "%s\n", (NULL == rml_uri) ? "NULL" : rml_uri);
            fclose(fp);
        }
        if (NULL != rml_uri) {
            free(rml_uri);
        }        
    }
    
    /* Change the default behavior of libevent such that we want to
       continually block rather than blocking for the default timeout
       and then looping around the progress engine again.  There
       should be nothing in the orted that cannot block in libevent
       until "something" happens (i.e., there's no need to keep
       cycling through progress because the only things that should
       happen will happen in libevent).  This is a minor optimization,
       but what the heck... :-) */
    opal_progress_set_event_flag(OPAL_EVLOOP_ONCE);
    
    /* If we have a prefix, then modify the PATH and
       LD_LIBRARY_PATH environment variables in our copy. This
       will ensure that any locally-spawned children will
       have our executables and libraries in their path

       For now, default to the prefix_dir provided in the first app_context.
       Since there always MUST be at least one app_context, we are safe in
       doing this.
    */
    chain = (opal_list_t*)opal_pointer_array_get_item(&chains, 0);
    jdata = (orte_job_t*)opal_list_get_first(chain);
    if (NULL != (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, 0)) &&
        NULL != app->prefix_dir) {
        char *oldenv, *newenv, *lib_base, *bin_base;
        
        /* copy the prefix into the daemon job so that any launcher
         * can find the orteds when we launch the virtual machine
         */
        if (NULL == (dapp = (orte_app_context_t*)opal_pointer_array_get_item(daemons->apps, 0))) {
            /* that's an error in the ess */
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        dapp->prefix_dir = strdup(app->prefix_dir);

        lib_base = opal_basename(opal_install_dirs.libdir);
        bin_base = opal_basename(opal_install_dirs.bindir);

        /* Reset PATH */
        newenv = opal_os_path( false, app->prefix_dir, bin_base, NULL );
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
        newenv = opal_os_path( false, app->prefix_dir, lib_base, NULL );
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
    }
    
    /* pre-condition any network transports that require it */
    for (i=0; i < chains.size; i++) {
        if (NULL == (chain = (opal_list_t*)opal_pointer_array_get_item(&chains, i))) {
            continue;
        }
        for (item = opal_list_get_first(chain);
             item != opal_list_get_end(chain);
             item = opal_list_get_next(item)) {
            jdata = (orte_job_t*)item;
            if (ORTE_SUCCESS != (rc = orte_pre_condition_transports(jdata))) {
                ORTE_ERROR_LOG(rc);
                orte_show_help("help-orterun.txt", "orterun:precondition", false,
                               orte_basename, NULL, NULL, rc);
                ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
                goto DONE;
            }
        }
    }

    /* setup to listen for commands sent specifically to me, even though I would probably
     * be the one sending them! Unfortunately, since I am a participating daemon,
     * there are times I need to send a command to "all daemons", and that means *I* have
     * to receive it too
     */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON,
                                 ORTE_RML_PERSISTENT, orte_daemon_recv, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
        goto DONE;
    }
    
    /* setup the data server */
    if (ORTE_SUCCESS != (rc = orte_data_server_init())) {
        ORTE_ERROR_LOG(rc);
        ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
        goto DONE;
    }
    
    /* if an uri for the ompi-server was provided, set the route */
    if (NULL != ompi_server) {
        opal_buffer_t buf;
        /* setup our route to the server */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &ompi_server, 1, OPAL_STRING);
        if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(&buf))) {
            ORTE_ERROR_LOG(rc);
            ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
            goto DONE;
        }
        OBJ_DESTRUCT(&buf);        
        /* check if we are to wait for the server to start - resolves
         * a race condition that can occur when the server is run
         * as a background job - e.g., in scripts
         */
        if (mapreduce_globals.wait_for_server) {
            /* ping the server */
            struct timeval timeout;
            timeout.tv_sec = mapreduce_globals.server_wait_timeout;
            timeout.tv_usec = 0;
            if (ORTE_SUCCESS != (rc = orte_rml.ping(ompi_server, &timeout))) {
                /* try it one more time */
                if (ORTE_SUCCESS != (rc = orte_rml.ping(ompi_server, &timeout))) {
                    /* okay give up */
                    orte_show_help("help-orterun.txt", "orterun:server-not-found", true,
                                   orte_basename, ompi_server,
                                   (long)mapreduce_globals.server_wait_timeout,
                                   ORTE_ERROR_NAME(rc));
                    ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
                    goto DONE;
                }
            }
        }
    }
    
    /* define a callback point for after jobs are assigned their jobids */
    if (ORTE_SUCCESS != (rc = orte_state.set_job_state_callback(ORTE_JOB_STATE_INIT_COMPLETE, do_wireup))) {
        ORTE_ERROR_LOG(rc);
    }

    /* spawn the jobs and their daemons */
    for (i=0; i < chains.size; i++) {
        if (NULL == (chain = (opal_list_t*)opal_pointer_array_get_item(&chains, i))) {
            continue;
        }
        for (item = opal_list_get_first(chain);
             item != opal_list_get_end(chain);
             item = opal_list_get_next(item)) {
            jdata = (orte_job_t*)item;
            if (ORTE_SUCCESS != orte_plm.spawn(jdata)) {
                goto DONE;
            }
        }
    }

    /* loop the event lib until an exit event is detected */
    while (orte_event_base_active) {
        opal_event_loop(orte_event_base, OPAL_EVLOOP_ONCE);
    }

 DONE:
    /* update the exit status, in case it wasn't done */
    ORTE_UPDATE_EXIT_STATUS(orte_exit_status);

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
        OBJ_CONSTRUCT(&mapreduce_globals.lock, opal_mutex_t);
        mapreduce_globals.env_val =     NULL;
        mapreduce_globals.appfile =     NULL;
        mapreduce_globals.wdir =        NULL;
        mapreduce_globals.path =        NULL;
        mapreduce_globals.ompi_server = NULL;
        mapreduce_globals.wait_for_server = false;
        mapreduce_globals.server_wait_timeout = 10;
        mapreduce_globals.stdin_target = "0";
        mapreduce_globals.report_pid        = NULL;
        mapreduce_globals.report_uri        = NULL;
        mapreduce_globals.disable_recovery = false;
        mapreduce_globals.single_job = false;
        mapreduce_globals.combiner_job = NULL;
    }

    /* Reset the other fields every time */

    mapreduce_globals.help                       = false;
    mapreduce_globals.version                    = false;
    mapreduce_globals.verbose                    = false;
    mapreduce_globals.debugger                   = false;
    mapreduce_globals.num_procs                  =  0;
    if( NULL != mapreduce_globals.env_val )
        free( mapreduce_globals.env_val );
    mapreduce_globals.env_val =     NULL;
    if( NULL != mapreduce_globals.appfile )
        free( mapreduce_globals.appfile );
    mapreduce_globals.appfile =     NULL;
    if( NULL != mapreduce_globals.wdir )
        free( mapreduce_globals.wdir );
    mapreduce_globals.wdir =        NULL;
    if( NULL != mapreduce_globals.path )
        free( mapreduce_globals.path );
    mapreduce_globals.path =        NULL;

    mapreduce_globals.preload_files  = NULL;
    mapreduce_globals.preload_files_dest_dir = NULL;

#if OPAL_ENABLE_FT_CR == 1
    mapreduce_globals.sstore_load = NULL;
#endif

    mapreduce_globals.mapper = false;
    mapreduce_globals.reducer = false;
    mapreduce_globals.combiner = false;

    /* All done */
    globals_init = true;
    return ORTE_SUCCESS;
}


static int parse_globals(int argc, char* argv[], opal_cmd_line_t *cmd_line)
{
    /* print version if requested.  Do this before check for help so
       that --version --help works as one might expect. */
    if (mapreduce_globals.version) {
        char *str, *project_name = NULL;
        if (0 == strcmp(orte_basename, "mpirun")) {
            project_name = "Open MPI";
        } else {
            project_name = "OpenRTE";
        }
        str = opal_show_help_string("help-orterun.txt", "orterun:version", 
                                    false,
                                    orte_basename, project_name, OPAL_VERSION,
                                    PACKAGE_BUGREPORT);
        if (NULL != str) {
            printf("%s", str);
            free(str);
        }
        exit(0);
    }

    /* Check for help request */
    if (mapreduce_globals.help) {
        char *str, *args = NULL;
        char *project_name = NULL;
        if (0 == strcmp(orte_basename, "mpirun")) {
            project_name = "Open MPI";
        } else {
            project_name = "OpenRTE";
        }
        args = opal_cmd_line_get_usage_msg(cmd_line);
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
        exit(0);
    }

    /* check for request to report pid */
    if (NULL != mapreduce_globals.report_pid) {
        FILE *fp;
        if (0 == strcmp(mapreduce_globals.report_pid, "-")) {
            /* if '-', then output to stdout */
            printf("%d\n", (int)getpid());
        } else if (0 == strcmp(mapreduce_globals.report_pid, "+")) {
            /* if '+', output to stderr */
            fprintf(stderr, "%d\n", (int)getpid());
        } else {
            fp = fopen(mapreduce_globals.report_pid, "w");
            if (NULL == fp) {
                orte_show_help("help-orterun.txt", "orterun:write_file", false,
                               orte_basename, "pid", mapreduce_globals.report_pid);
                exit(0);
            }
            fprintf(fp, "%d\n", (int)getpid());
            fclose(fp);
        }
    }
    
     /* if recovery was disabled on the cmd line, do so */
    if (mapreduce_globals.disable_recovery) {
        orte_enable_recovery = false;
        orte_max_restarts = 0;
    }

    return ORTE_SUCCESS;
}


static int parse_locals(int argc, char* argv[])
{
    int i, rc;
    int temp_argc;
    char **temp_argv, **env;
    orte_app_context_t *app;
    bool made_app;
    orte_std_cntr_t j, size1;
    orte_job_t *jdata;
    orte_job_controls_t jtype;
    opal_list_t *chain;

    /* if the ompi-server was given, then set it up here */
    if (NULL != mapreduce_globals.ompi_server) {
        /* someone could have passed us a file instead of a uri, so
         * we need to first check to see what we have - if it starts
         * with "file", then we know it is a file. Otherwise, we assume
         * it is a uri as provided by the ompi-server's output
         * of an ORTE-standard string. Note that this is NOT a standard
         * uri as it starts with the process name!
         */
        if (0 == strncmp(mapreduce_globals.ompi_server, "file", strlen("file")) ||
            0 == strncmp(mapreduce_globals.ompi_server, "FILE", strlen("FILE"))) {
            char input[1024], *filename;
            FILE *fp;
            
            /* it is a file - get the filename */
            filename = strchr(mapreduce_globals.ompi_server, ':');
            if (NULL == filename) {
                /* filename is not correctly formatted */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-bad", true,
                               orte_basename, mapreduce_globals.ompi_server);
                exit(1);
            }
            ++filename; /* space past the : */
            
            if (0 >= strlen(filename)) {
                /* they forgot to give us the name! */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-missing", true,
                               orte_basename, mapreduce_globals.ompi_server);
                exit(1);
            }
            
            /* open the file and extract the uri */
            fp = fopen(filename, "r");
            if (NULL == fp) { /* can't find or read file! */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-access", true,
                               orte_basename, mapreduce_globals.ompi_server);
                exit(1);
            }
            if (NULL == fgets(input, 1024, fp)) {
                /* something malformed about file */
                fclose(fp);
                orte_show_help("help-orterun.txt", "orterun:ompi-server-file-bad", true,
                               orte_basename, mapreduce_globals.ompi_server,
                               orte_basename);
                exit(1);
            }
            fclose(fp);
            input[strlen(input)-1] = '\0';  /* remove newline */
            ompi_server = strdup(input);
        } else if (0 == strncmp(mapreduce_globals.ompi_server, "pid", strlen("pid")) ||
                   0 == strncmp(mapreduce_globals.ompi_server, "PID", strlen("PID"))) {
            opal_list_t hnp_list;
            opal_list_item_t *item;
            orte_hnp_contact_t *hnp;
            char *ptr;
            pid_t pid;
            
            ptr = strchr(mapreduce_globals.ompi_server, ':');
            if (NULL == ptr) {
                /* pid is not correctly formatted */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-pid-bad", true,
                               orte_basename, orte_basename,
                               mapreduce_globals.ompi_server, orte_basename);
                exit(1);
            }
            ++ptr; /* space past the : */
            
            if (0 >= strlen(ptr)) {
                /* they forgot to give us the pid! */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-pid-bad", true,
                               orte_basename, orte_basename,
                               mapreduce_globals.ompi_server, orte_basename);
                exit(1);
            }
            
            pid = strtoul(ptr, NULL, 10);
            
            /* to search the local mpirun's, we have to partially initialize the
             * orte_process_info structure. This won't fully be setup until orte_init,
             * but we finagle a little bit of it here
             */
            if (ORTE_SUCCESS != (rc = orte_session_dir_get_name(NULL, &orte_process_info.tmpdir_base,
                                                                &orte_process_info.top_session_dir,
                                                                NULL, NULL, NULL))) {
                orte_show_help("help-orterun.txt", "orterun:ompi-server-could-not-get-hnp-list", true,
                               orte_basename, orte_basename);
                exit(1);
            }
            
            OBJ_CONSTRUCT(&hnp_list, opal_list_t);
            
            /* get the list of HNPs, but do -not- setup contact info to them in the RML */
            if (ORTE_SUCCESS != (rc = orte_list_local_hnps(&hnp_list, false))) {
                orte_show_help("help-orterun.txt", "orterun:ompi-server-could-not-get-hnp-list", true,
                               orte_basename, orte_basename);
                exit(1);
            }
            
            /* search the list for the desired pid */
            while (NULL != (item = opal_list_remove_first(&hnp_list))) {
                hnp = (orte_hnp_contact_t*)item;
                if (pid == hnp->pid) {
                    ompi_server = strdup(hnp->rml_uri);
                    goto hnp_found;
                }
                OBJ_RELEASE(item);
            }
            /* if we got here, it wasn't found */
            orte_show_help("help-orterun.txt", "orterun:ompi-server-pid-not-found", true,
                           orte_basename, orte_basename, pid, mapreduce_globals.ompi_server,
                           orte_basename);
            OBJ_DESTRUCT(&hnp_list);
            exit(1);
        hnp_found:
            /* cleanup rest of list */
            while (NULL != (item = opal_list_remove_first(&hnp_list))) {
                OBJ_RELEASE(item);
            }
            OBJ_DESTRUCT(&hnp_list);
        } else {
            ompi_server = strdup(mapreduce_globals.ompi_server);
        }
    }

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
                rc = create_app(temp_argc, temp_argv, &app, &made_app, &env, &jtype);
                /** keep track of the number of apps - point this app_context to that index */
                if (ORTE_SUCCESS != rc) {
                    /* Assume that the error message has already been
                       printed; no need to cleanup -- we can just
                       exit */
                    exit(1);
                }
                if (made_app) {
                    app->idx = 0;
                    jdata = OBJ_NEW(orte_job_t);
                    jdata->controls |= jtype;
                    opal_pointer_array_add(jdata->apps, app);
                    ++jdata->num_apps;
                    if (ORTE_JOB_CONTROL_MAPPER == jtype) {
                        chain = OBJ_NEW(opal_list_t);
                        opal_pointer_array_add(&chains, chain);
                    } else if (ORTE_JOB_CONTROL_COMBINER == jtype) {
                        chain = OBJ_NEW(opal_list_t);
                        opal_pointer_array_add(&chains, chain);
                        /* flag the combiner job */
                        if (NULL != mapreduce_globals.combiner_job) {
                            /* cannot have more than one combiner job */
                            orte_show_help("help-orterun.txt", "multiple-combiners", true);
                            exit(1);
                        }
                        mapreduce_globals.combiner_job = jdata;
                    }
                    opal_list_append(chain, &jdata->super);
                    /* track number of jobs */
                    orte_num_jobs++;
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
        rc = create_app(temp_argc, temp_argv, &app, &made_app, &env, &jtype);
        if (ORTE_SUCCESS != rc) {
            /* Assume that the error message has already been printed;
               no need to cleanup -- we can just exit */
            exit(1);
        }
        if (made_app) {
            app->idx = 0;
            jdata = OBJ_NEW(orte_job_t);
            jdata->controls |= jtype;
            opal_pointer_array_add(jdata->apps, app);
            ++jdata->num_apps;
            if (ORTE_JOB_CONTROL_MAPPER == jtype) {
                chain = OBJ_NEW(opal_list_t);
                opal_pointer_array_add(&chains, chain);
            } else if (ORTE_JOB_CONTROL_COMBINER == jtype) {
                chain = OBJ_NEW(opal_list_t);
                opal_pointer_array_add(&chains, chain);
                /* flag the combiner job */
                if (NULL != mapreduce_globals.combiner_job) {
                    /* cannot have more than one combiner job */
                    orte_show_help("help-orterun.txt", "multiple-combiners", true);
                    exit(1);
                }
                mapreduce_globals.combiner_job = jdata;
            }
            opal_list_append(chain, &jdata->super);
            /* track number of jobs */
            orte_num_jobs++;
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
       overrides here in mapreduce (so that when we orte_init() later,
       all the components see these MCA params).  Here's how we decide
       which subset of the MCA params we set here in mapreduce:

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


static int capture_cmd_line_params(int argc, int start, char **argv)
{
    int i, j, k;
    bool ignore;
    char *no_dups[] = {
        "grpcomm",
        "odls",
        "rml",
        "routed",
        NULL
    };
    
    for (i = 0; i < (argc-start); ++i) {
        if (0 == strcmp("-mca",  argv[i]) ||
            0 == strcmp("--mca", argv[i]) ) {
            /* It would be nice to avoid increasing the length
             * of the orted cmd line by removing any non-ORTE
             * params. However, this raises a problem since
             * there could be OPAL directives that we really
             * -do- want the orted to see - it's only the OMPI
             * related directives we could ignore. This becomes
             * a very complicated procedure, however, since
             * the OMPI mca params are not cleanly separated - so
             * filtering them out is nearly impossible.
             *
             * see if this is already present so we at least can
             * avoid growing the cmd line with duplicates
             */
            ignore = false;
            if (NULL != orted_cmd_line) {
                for (j=0; NULL != orted_cmd_line[j]; j++) {
                    if (0 == strcmp(argv[i+1], orted_cmd_line[j])) {
                        /* already here - if the value is the same,
                         * we can quitely ignore the fact that they
                         * provide it more than once. However, some
                         * frameworks are known to have problems if the
                         * value is different. We don't have a good way
                         * to know this, but we at least make a crude
                         * attempt here to protect ourselves.
                         */
                        if (0 == strcmp(argv[i+2], orted_cmd_line[j+1])) {
                            /* values are the same */
                            ignore = true;
                            break;
                        } else {
                            /* values are different - see if this is a problem */
                            for (k=0; NULL != no_dups[k]; k++) {
                                if (0 == strcmp(no_dups[k], argv[i+1])) {
                                    /* print help message
                                     * and abort as we cannot know which one is correct
                                     */
                                    orte_show_help("help-orterun.txt", "orterun:conflicting-params",
                                                   true, orte_basename, argv[i+1],
                                                   argv[i+2], orted_cmd_line[j+1]);
                                    return ORTE_ERR_BAD_PARAM;
                                }
                            }
                            /* this passed muster - just ignore it */
                            ignore = true;
                            break;
                        }
                    }
                }
            }
            if (!ignore) {
                opal_argv_append_nosize(&orted_cmd_line, argv[i]);
                opal_argv_append_nosize(&orted_cmd_line, argv[i+1]);
                opal_argv_append_nosize(&orted_cmd_line, argv[i+2]);
            }
            i += 2;
        }
    }
    
    return ORTE_SUCCESS;
}


/*
 * This function takes a "char ***app_env" parameter to handle the
 * specific case:
 *
 *   mapreduce --mca foo bar -app appfile
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
 *   mapreduce --mca foo bar -np 4 hostname
 *
 * Then the upper-level function (parse_locals()) calls create_app()
 * with a NULL value for app_env, meaning that there is no "base"
 * environment that the app needs to be created from.
 */
static int create_app(int argc, char* argv[],
                      orte_app_context_t **app_ptr,
                      bool *made_app, char ***app_env,
                      orte_job_controls_t *jtype)
{
    opal_cmd_line_t cmd_line;
    char cwd[OPAL_PATH_MAX];
    int i, j, count, rc;
    char *param, *value, *value2;
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
    if (NULL != mapreduce_globals.appfile) {
        if (ORTE_SUCCESS != (rc = capture_cmd_line_params(argc, 0, argv))) {
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

    if (NULL != mapreduce_globals.appfile) {
        OBJ_DESTRUCT(&cmd_line);
        return parse_appfile(strdup(mapreduce_globals.appfile), app_env);
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
    if (ORTE_SUCCESS != (rc = capture_cmd_line_params(argc, count, argv))) {
        goto cleanup;
    }
    
    /* Grab all OMPI_* environment variables */

    app->env = opal_argv_copy(*app_env);
    for (i = 0; NULL != environ[i]; ++i) {
        if (0 == strncmp("OMPI_", environ[i], 5)) {
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
    
    /* add the ompi-server, if provided */
    if (NULL != ompi_server) {
        opal_setenv("OMPI_MCA_pubsub_orte_server", ompi_server, true, &app->env);
    }

    /* Did the user request to export any environment variables on the cmd line? */
    if (opal_cmd_line_is_taken(&cmd_line, "x")) {
        j = opal_cmd_line_get_ninsts(&cmd_line, "x");
        for (i = 0; i < j; ++i) {
            param = opal_cmd_line_get_param(&cmd_line, "x", i, 0);

            if (NULL != strchr(param, '=')) {
                opal_argv_append_nosize(&app->env, param);
            } else {
                value = getenv(param);
                if (NULL != value) {
                    if (NULL != strchr(value, '=')) {
                        opal_argv_append_nosize(&app->env, value);
                    } else {
                        asprintf(&value2, "%s=%s", param, value);
                        opal_argv_append_nosize(&app->env, value2);
                        free(value2);
                    }
                } else {
                    opal_output(0, "Warning: could not find environment variable \"%s\"\n", param);
                }
            }
        }
    }

    /* Did the user request to export any environment variables via MCA param? */
    if (NULL != orte_forward_envars) {
        char **vars;
        vars = opal_argv_split(orte_forward_envars, ',');
        for (i=0; NULL != vars[i]; i++) {
            if (NULL != strchr(vars[i], '=')) {
                /* user supplied a value */
                opal_argv_append_nosize(&app->env, vars[i]);
            } else {
                /* get the value from the environ */
                value = getenv(vars[i]);
                if (NULL != value) {
                    if (NULL != strchr(value, '=')) {
                        opal_argv_append_nosize(&app->env, value);
                    } else {
                        asprintf(&value2, "%s=%s", vars[i], value);
                        opal_argv_append_nosize(&app->env, value2);
                        free(value2);
                    }
                } else {
                    opal_output(0, "Warning: could not find environment variable \"%s\"\n", param);
                }
            }
        }
        opal_argv_free(vars);
    }

    /* If the user specified --path, store it in the user's app
       environment via the OMPI_exec_path variable. */
    if (NULL != mapreduce_globals.path) {
        asprintf(&value, "OMPI_exec_path=%s", mapreduce_globals.path);
        opal_argv_append_nosize(&app->env, value);
        free(value);
    }

    /* Did the user request a specific wdir? */
    if (NULL != mapreduce_globals.wdir) {
        /* if this is a relative path, convert it to an absolute path */
        if (opal_path_is_absolute(mapreduce_globals.wdir)) {
            app->cwd = strdup(mapreduce_globals.wdir);
        } else {
            /* get the cwd */
            if (OPAL_SUCCESS != (rc = opal_getcwd(cwd, sizeof(cwd)))) {
                orte_show_help("help-orterun.txt", "orterun:init-failure",
                               true, "get the cwd", rc);
                goto cleanup;
            }
            /* construct the absolute path */
            app->cwd = opal_os_path(false, cwd, mapreduce_globals.wdir, NULL);
        }
        app->user_specified_cwd = true;
    } else {
        if (OPAL_SUCCESS != (rc = opal_getcwd(cwd, sizeof(cwd)))) {
            orte_show_help("help-orterun.txt", "orterun:init-failure",
                           true, "get the cwd", rc);
            goto cleanup;
        }
        app->cwd = strdup(cwd);
        app->user_specified_cwd = false;
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
                NULL != mapreduce_globals.prefix) {
                /* if they don't match, then that merits a warning */
                param = strdup(opal_cmd_line_get_param(&cmd_line, "prefix", 0, 0));
                if (0 != strcmp(param, mapreduce_globals.prefix)) {
                    orte_show_help("help-orterun.txt", "orterun:app-prefix-conflict",
                                   true, orte_basename, mapreduce_globals.prefix, param);
                    /* let the global-level prefix take precedence since we
                     * know that one is being  used
                     */
                    free(param);
                    param = mapreduce_globals.prefix;
                } else {
                    /* since they match, just use param */
                    free(mapreduce_globals.prefix);
                    mapreduce_globals.prefix = NULL;
                }
            } else if (NULL != mapreduce_globals.prefix) {
                param = mapreduce_globals.prefix;
            } else if (opal_cmd_line_is_taken(&cmd_line, "prefix")){
                /* must be --prefix alone */
                param = strdup(opal_cmd_line_get_param(&cmd_line, "prefix", 0, 0));
            } else {
                /* --enable-orterun-prefix-default was given to mapreduce */
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
                        return ORTE_ERR_FATAL;
                    }
                }

                app->prefix_dir = strdup(param);
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
            app->hostfile = strdup(value);
        }
    }
    if (0 < (j = opal_cmd_line_get_ninsts(&cmd_line, "machinefile"))) {
        if(1 < j || NULL != app->hostfile) {
            orte_show_help("help-orterun.txt", "orterun:multiple-hostfiles",
                           true, orte_basename, NULL);
            return ORTE_ERR_FATAL;
        } else {
            value = opal_cmd_line_get_param(&cmd_line, "machinefile", 0, 0);
            app->hostfile = strdup(value);
        }
    }
 
    /* Did the user specify any hosts? */
    if (0 < (j = opal_cmd_line_get_ninsts(&cmd_line, "host"))) {
        for (i = 0; i < j; ++i) {
            value = opal_cmd_line_get_param(&cmd_line, "host", i, 0);
            opal_argv_append_nosize(&app->dash_host, value);
        }
    }

    /* Get the numprocs */
    app->num_procs = (orte_std_cntr_t)mapreduce_globals.num_procs;
    total_num_apps++;

    /* Preserve if we are to preload the binary */
    app->preload_binary = orte_preload_binaries;
    if( NULL != mapreduce_globals.preload_files)
        app->preload_files  = strdup(mapreduce_globals.preload_files);
    else 
        app->preload_files = NULL;
    if( NULL != mapreduce_globals.preload_files_dest_dir)
        app->preload_files_dest_dir  = strdup(mapreduce_globals.preload_files_dest_dir);
    else 
        app->preload_files_dest_dir = NULL;

    /* flag type of app */
    if (mapreduce_globals.mapper) {
        *jtype = ORTE_JOB_CONTROL_MAPPER;
    } else if (mapreduce_globals.reducer) {
        *jtype = ORTE_JOB_CONTROL_REDUCER;
    } else if (mapreduce_globals.combiner) {
        *jtype = ORTE_JOB_CONTROL_COMBINER;
    } else {
        /* should specify type - however, we will assume that
         * the first app is the mapper and all others are reducers
         */
        if (1 < total_num_apps) {
            *jtype = ORTE_JOB_CONTROL_REDUCER;
        } else {
            *jtype = ORTE_JOB_CONTROL_MAPPER;
        }
    }

    /* Do not try to find argv[0] here -- the starter is responsible
       for that because it may not be relevant to try to find it on
       the node where mapreduce is executing.  So just strdup() argv[0]
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
        for (i=0; NULL != app->argv[i]; i++) {
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
        for (i=0; NULL != app->argv[i]; i++) {
            if (NULL != strstr(app->argv[i], "cp") ||
                NULL != strstr(app->argv[i], "classpath")) {
                /* yep - but does it include the path to the mpi libs? */
                found = true;
                if (NULL == strstr(app->argv[i+1], "mpi.jar")) {
                    /* nope - need to add it */
                    if (':' == app->argv[i+1][strlen(app->argv[i+1]-1)]) {
                        asprintf(&value, "%s%s/mpi.jar", app->argv[i+1], opal_install_dirs.libdir);
                    } else {
                        asprintf(&value, "%s:%s/mpi.jar", app->argv[i+1], opal_install_dirs.libdir);
                    }
                    free(app->argv[i+1]);
                    app->argv[i+1] = value;
                }
                break;
            }
        }
        if (!found) {
            /* check to see if CLASSPATH is in the environment */
            for (i=0; NULL != environ[i]; i++) {
                if (0 == strncmp(environ[i], "CLASSPATH", strlen("CLASSPATH"))) {
                    /* check if mpi.jar is present */
                    if (NULL != strstr(environ[i], "mpi.jar")) {
                        /* yes - just add the envar to the argv in the
                         * right format
                         */
                        value = strchr(environ[i], '=');
                        ++value; /* step over the = */
                        opal_argv_insert_element(&app->argv, 1, value);
                        opal_argv_insert_element(&app->argv, 1, "-cp");
                    } else {
                        /* need to add it */
                        value = strchr(environ[i], '=');
                        ++value; /* step over the = */
                        if (':' == value[strlen(value-1)]) {
                            asprintf(&param, "%s%s/mpi.jar", value, opal_install_dirs.libdir);
                        } else {
                            asprintf(&param, "%s:%s/mpi.jar", value, opal_install_dirs.libdir);
                        }
                        opal_argv_insert_element(&app->argv, 1, param);
                        opal_argv_insert_element(&app->argv, 1, "-cp");
                        free(param);
                    }
                    found = true;
                    break;
                }
            }
            if (!found) {
                /* need to add it right after the java command - have
                 * to include the current directory and trust that
                 * the user set cwd if necessary
                 */
                asprintf(&value, ".:%s/mpi.jar", opal_install_dirs.libdir);
                opal_argv_insert_element(&app->argv, 1, value);
                free(value);
                opal_argv_insert_element(&app->argv, 1, "-cp");
            }
        }
    }
    free(appname);
    if (mapreduce_globals.verbose) {
        value = opal_argv_join(app->argv, ' ');
        free(value);
    }
    
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


static int parse_appfile(char *filename, char ***env)
{
    size_t i, len;
    FILE *fp;
    char line[BUFSIZ];
    int rc, argc;
    char **argv;
    orte_app_context_t *app;
    bool blank, made_app;
    char bogus[] = "bogus ";
    char **tmp_env;
    orte_job_t *jdata;
    orte_job_controls_t jtype;
    opal_list_t *chain;

    /*
     * Make sure to clear out this variable so we don't do anything odd in
     * app_create()
     */
    if( NULL != mapreduce_globals.appfile ) {
        free( mapreduce_globals.appfile );
        mapreduce_globals.appfile =     NULL;
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
    do {

        /* We need a bogus argv[0] (because when argv comes in from
           the command line, argv[0] is "mapreduce", so the parsing
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

               mapreduce --mca foo bar --appfile file

               where the "file" contains multiple apps.  In this case,
               each app in "file" will get *only* foo=bar as the base
               environment from which its specific environment is
               constructed. */

            if (NULL != *env) {
                tmp_env = opal_argv_copy(*env);
                if (NULL == tmp_env) {
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
            } else {
                tmp_env = NULL;
            }

            rc = create_app(argc, argv, &app, &made_app, &tmp_env, &jtype);
            if (ORTE_SUCCESS != rc) {
                /* Assume that the error message has already been
                   printed; no need to cleanup -- we can just exit */
                exit(1);
            }
            if (NULL != tmp_env) {
                opal_argv_free(tmp_env);
            }
            if (made_app) {
                app->idx = 0;
                jdata = OBJ_NEW(orte_job_t);
                jdata->controls |= jtype;
                opal_pointer_array_add(jdata->apps, app);
                ++jdata->num_apps;
                if (ORTE_JOB_CONTROL_MAPPER == jtype) {
                    chain = OBJ_NEW(opal_list_t);
                    opal_pointer_array_add(&chains, chain);
                } else if (ORTE_JOB_CONTROL_COMBINER == jtype) {
                    chain = OBJ_NEW(opal_list_t);
                    opal_pointer_array_add(&chains, chain);
                    /* flag the combiner job */
                    if (NULL != mapreduce_globals.combiner_job) {
                        /* cannot have more than one combiner job */
                        orte_show_help("help-orterun.txt", "multiple-combiners", true);
                        exit(1);
                    }
                    mapreduce_globals.combiner_job = jdata;
                }
                opal_list_append(chain, &jdata->super);
                /* track number of jobs */
                orte_num_jobs++;
            }
        }
    } while (!feof(fp));
    fclose(fp);

    /* All done */

    free(filename);
    return ORTE_SUCCESS;
}

static int num_cbacks=0;

static void do_wireup(int fd, short sd, void *cbdata)
{
    opal_list_item_t *item, *itm;
    opal_list_t *chain;
    int i;
    orte_job_t *jdata, *jptr;

    /* track the number of callbacks */
    num_cbacks++;

    /* if all jobs have completed this phase, then assign
     * wireup targets
     */
    if (num_cbacks == orte_num_jobs) {
        for (i=0; i < chains.size; i++) {
            if (NULL == (chain = (opal_list_t*)opal_pointer_array_get_item(&chains, i))) {
                continue;
            }
            for (item = opal_list_get_first(chain);
                 item != opal_list_get_end(chain);
                 item = opal_list_get_next(item)) {
                jdata = (orte_job_t*)item;
                /* ensure stdin is pulled for each job */
                /* see where this job's stdout should go */
                if (ORTE_JOB_CONTROL_MAPPER & jdata->controls) {
                    jdata->stdin_target = ORTE_VPID_WILDCARD;
                    /* mappers send their output to the next job in the chain */
                    itm = opal_list_get_next(item);
                    if (itm != opal_list_get_end(chain)) {
                        jptr = (orte_job_t*)itm;
                        jdata->stdout_target = jptr->jobid;
                    }
                } else if (ORTE_JOB_CONTROL_REDUCER & jdata->controls) {
                    jdata->stdin_target = ORTE_VPID_WILDCARD;
                    /* reducer feeds its output to the next job in the chain, if it exists */
                    itm = opal_list_get_next(item);
                    if (itm != opal_list_get_end(chain)) {
                        jptr = (orte_job_t*)itm;
                        jdata->stdout_target = jptr->jobid;
                    } else {
                        /* if a combiner exists, then feed the output there */
                        if (NULL != mapreduce_globals.combiner_job) {
                            jdata->stdout_target = mapreduce_globals.combiner_job->jobid;
                        }
                    }
                } else if (!(ORTE_JOB_CONTROL_COMBINER & jdata->controls)) {
                    /* should have been something */
                    orte_show_help("help-orterun.txt", "orterun:unrecognized-mr-type",
                                   true, orte_basename);
                    exit(ORTE_ERROR_DEFAULT_EXIT_CODE);
                }
            }
        }
        /* now send all the jobs to allocate, in reverse order
         * to ensure that all recipients of input are in place
         * BEFORE a source begins to generate output
         */
        for (i=chains.size-1; 0 <= i; i--) {
            if (NULL == (chain = (opal_list_t*)opal_pointer_array_get_item(&chains, i))) {
                continue;
            }
            while (NULL != (item = opal_list_remove_last(chain))) {
                jdata = (orte_job_t*)item;
                ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_ALLOCATE);
            }
        }
    }
}
