/* -*- C -*-
 *
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2010 Cisco Systems, Inc. All rights reserved.
 * Copyright (c) 2007-2009 Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"


#include <stdio.h>
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

#include "opal/event/event.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/base/base.h"
#include "opal/threads/condition.h"
#include "opal/util/argv.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/util/basename.h"
#include "opal/util/cmd_line.h"
#include "opal/util/opal_environ.h"
#include "opal/util/opal_getcwd.h"
#include "orte/util/show_help.h"
#include "opal/util/trace.h"
#include "opal/sys/atomic.h"
#if OPAL_ENABLE_FT == 1
#include "opal/runtime/opal_cr.h"
#endif

#include "opal/version.h"
#include "opal/runtime/opal.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/os_path.h"
#include "opal/util/path.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"

#include "orte/util/proc_info.h"
#include "orte/util/pre_condition_transports.h"
#include "orte/util/session_dir.h"
#include "orte/util/name_fns.h"
#include "orte/util/parse_options.h"

#include "orte/mca/odls/odls.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_data_server.h"
#include "orte/runtime/orte_locks.h"

/* ensure I can behave like a daemon */
#include "orte/orted/orted.h"

#include "debuggers.h"
#include "orterun.h"

/*
 * Globals
 */
static struct opal_event term_handler;
static struct opal_event int_handler;
static struct opal_event epipe_handler;
#ifndef __WINDOWS__
static struct opal_event sigusr1_handler;
static struct opal_event sigusr2_handler;
static struct opal_event sigtstp_handler;
static struct opal_event sigcont_handler;
#endif  /* __WINDOWS__ */
static orte_job_t *jdata;
static int num_aborted = 0;
static int num_killed = 0;
static int num_failed_start = 0;
static char **global_mca_env = NULL;
static bool have_zero_np = false;
static orte_std_cntr_t total_num_apps = 0;
static bool want_prefix_by_default = (bool) ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT;
static opal_event_t *orterun_event, *orteds_exit_event;
static char *ompi_server=NULL;
static opal_event_t *abort_exit_event=NULL;
static bool forcibly_die = false;
static opal_event_t *timeout_ev=NULL;

/*
 * Globals
 */
struct orterun_globals_t orterun_globals;
static bool globals_init = false;

static opal_cmd_line_init_t cmd_line_init[] = {
    /* Various "obvious" options */
    { NULL, NULL, NULL, 'h', NULL, "help", 0,
      &orterun_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },
    { NULL, NULL, NULL, 'V', NULL, "version", 0,
      &orterun_globals.version, OPAL_CMD_LINE_TYPE_BOOL,
      "Print version and exit" },
    { NULL, NULL, NULL, 'v', NULL, "verbose", 0,
      &orterun_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Be verbose" },
    { NULL, NULL, NULL, 'q', NULL, "quiet", 0,
      &orterun_globals.quiet, OPAL_CMD_LINE_TYPE_BOOL,
      "Suppress helpful messages" },
    { NULL, NULL, NULL, '\0', "report-pid", "report-pid", 0,
      &orterun_globals.report_pid, OPAL_CMD_LINE_TYPE_BOOL,
      "Printout pid" },
    
    /* hetero apps */
    { "orte", "hetero", "apps", '\0', NULL, "hetero", 0,
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
    
    /* select stdin option */
    { NULL, NULL, NULL, '\0', "stdin", "stdin", 1,
      &orterun_globals.stdin_target, OPAL_CMD_LINE_TYPE_STRING,
      "Specify procs to receive stdin [rank, none] (default: 0, indicating rank 0)" },
    
    /* Specify the launch agent to be used */
    { "orte", "launch", "agent", '\0', "launch-agent", "launch-agent", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Command used to start processes on remote nodes (default: orted)" },
    
    /* Preload the binary on the remote machine */
    { NULL, NULL, NULL, 's', NULL, "preload-binary", 0,
      &orterun_globals.preload_binary, OPAL_CMD_LINE_TYPE_BOOL,
      "Preload the binary on the remote machine before starting the remote process." },

    /* Preload files on the remote machine */
    { NULL, NULL, NULL, '\0', NULL, "preload-files", 1,
      &orterun_globals.preload_files, OPAL_CMD_LINE_TYPE_STRING,
      "Preload the comma separated list of files to the remote machines current working directory before starting the remote process." },

    /* Where to Preload files on the remote machine */
    { NULL, NULL, NULL, '\0', NULL, "preload-files-dest-dir", 1,
      &orterun_globals.preload_files_dest_dir, OPAL_CMD_LINE_TYPE_STRING,
      "The destination directory to use in conjunction with --preload-files. By default the absolute and relative paths provided by --preload-files are used." },

    /* Use an appfile */
    { NULL, NULL, NULL, '\0', NULL, "app", 1,
      &orterun_globals.appfile, OPAL_CMD_LINE_TYPE_STRING,
      "Provide an appfile; ignore all other command line options" },

    /* Number of processes; -c, -n, --n, -np, and --np are all
       synonyms */
    { NULL, NULL, NULL, 'c', "np", "np", 1,
      &orterun_globals.num_procs, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run" },
    { NULL, NULL, NULL, '\0', "n", "n", 1,
      &orterun_globals.num_procs, OPAL_CMD_LINE_TYPE_INT,
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
      &orterun_globals.ompi_server, OPAL_CMD_LINE_TYPE_STRING,
      "Specify the URI of the Open MPI server, or the name of the file (specified as file:filename) that contains that info" },
    { NULL, NULL, NULL, '\0', "wait-for-server", "wait-for-server", 0,
      &orterun_globals.wait_for_server, OPAL_CMD_LINE_TYPE_BOOL,
      "If ompi-server is not already running, wait until it is detected (default: false)" },
    { NULL, NULL, NULL, '\0', "server-wait-time", "server-wait-time", 1,
      &orterun_globals.server_wait_timeout, OPAL_CMD_LINE_TYPE_INT,
      "Time in seconds to wait for ompi-server (default: 10 sec)" },
    
    { "carto", "file", "path", '\0', "cf", "cartofile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a cartography file" },

    { "rmaps_rank", "file", "path", '\0', "rf", "rankfile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a rankfile file" },

    /* Export environment variables; potentially used multiple times,
       so it does not make sense to set into a variable */
    { NULL, NULL, NULL, 'x', NULL, NULL, 1,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      "Export an environment variable, optionally specifying a value (e.g., \"-x foo\" exports the environment variable foo and takes its value from the current environment; \"-x foo=bar\" exports the environment variable name foo and sets its value to \"bar\" in the started processes)" },

    /* Mapping options */
    { NULL, NULL, NULL, '\0', "bynode", "bynode", 0,
      &orterun_globals.by_node, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to assign processes round-robin by node" },
    { NULL, NULL, NULL, '\0', "byslot", "byslot", 0,
      &orterun_globals.by_slot, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to assign processes round-robin by slot (the default)" },
    { NULL, NULL, NULL, '\0', "bycore", "bycore", 0,
      &orterun_globals.by_slot, OPAL_CMD_LINE_TYPE_BOOL,
      "Alias for byslot" },
    { NULL, NULL, NULL, '\0', "bysocket", "bysocket", 0,
      &orterun_globals.by_socket, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to assign processes round-robin by socket" },
    { NULL, NULL, NULL, '\0', "byboard", "byboard", 0,
      &orterun_globals.by_slot, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to assign processes round-robin by board (equivalent to bynode if only 1 board/node)" },
    { "rmaps", "base", "pernode", '\0', "pernode", "pernode", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Launch one process per available node on the specified number of nodes [no -np => use all allocated nodes]" },
    { "rmaps", "base", "n_pernode", '\0', "npernode", "npernode", 1,
        NULL, OPAL_CMD_LINE_TYPE_INT,
        "Launch n processes per node on all allocated nodes" },
    { "rmaps", "base", "slot_list", '\0', "slot-list", "slot-list", 1,
        NULL, OPAL_CMD_LINE_TYPE_STRING,
        "List of processor IDs to bind MPI processes to (e.g., used in conjunction with rank files)" },
    { "rmaps", "base", "no_oversubscribe", '\0', "nooversubscribe", "nooversubscribe", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Nodes are not to be oversubscribed, even if the system supports such operation"},
    { "rmaps", "base", "loadbalance", '\0', "loadbalance", "loadbalance", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Balance total number of procs across all allocated nodes"},
    { "rmaps", "base", "display_map", '\0', "display-map", "display-map", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Display the process map just before launch"},
    { "rmaps", "base", "display_devel_map", '\0', "display-devel-map", "display-devel-map", 0,
       NULL, OPAL_CMD_LINE_TYPE_BOOL,
       "Display a detailed process map (mostly intended for developers) just before launch"},
    { NULL, NULL, NULL, 'H', "host", "host", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "List of hosts to invoke processes on" },
    { "rmaps", "base", "no_schedule_local", '\0', "nolocal", "nolocal", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not run any MPI applications on the local node" },
    { "rmaps", "base", "cpus_per_rank", '\0', "cpus-per-proc", "cpus-per-proc", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Number of cpus to use for each process [default=1]" },
    { "rmaps", "base", "cpus_per_rank", '\0', "cpus-per-rank", "cpus-per-rank", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Synonym for cpus-per-proc" },
    { "rmaps", "base", "n_perboard", '\0', "nperboard", "nperboard", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Launch n processes per board on all allocated nodes" },
    { "rmaps", "base", "n_persocket", '\0', "npersocket", "npersocket", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Launch n processes per socket on all allocated nodes" },

    /* binding options */
    { NULL, NULL, NULL, '\0', "bind-to-none", "bind-to-none", 0,
      &orterun_globals.bind_to_none, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not bind processes to cores or sockets (default)" },
    { NULL, NULL, NULL, '\0', "bind-to-core", "bind-to-core", 0,
      &orterun_globals.bind_to_core, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to bind processes to specific cores" },
    { NULL, NULL, NULL, '\0', "bind-to-board", "bind-to-board", 0,
      &orterun_globals.bind_to_board, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to bind processes to specific boards (meaningless on 1 board/node)" },
    { NULL, NULL, NULL, '\0', "bind-to-socket", "bind-to-socket", 0,
      &orterun_globals.bind_to_socket, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to bind processes to sockets" },
    { "rmaps", "base", "stride", '\0', "stride", "stride", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "When binding multiple cores to a rank, the step size to use between cores [default: 1]" },
    { "odls", "base", "report_bindings", '\0', "report-bindings", "report-bindings", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to report process bindings to stderr [default: 0 = no]" },

    /* Allocation options */
    { "ras", "base", "display_alloc", '\0', "display-allocation", "display-allocation", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Display the allocation being used by this job"},
    { "ras", "base", "display_devel_alloc", '\0', "display-devel-allocation", "display-devel-allocation", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Display a detailed list (mostly intended for developers) of the allocation being used by this job"},
    { "orte", "cpu", "set", '\0', "cpu-set", "cpu-set", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Comma-separated list of ranges specifying logical cpus allocated to this job [default: none]"},

    /* cluster hardware info */
    { "orte", "num", "boards", '\0', "num-boards", "num-boards", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Number of processor boards/node (1-256) [default: 1]"},
    { "orte", "num", "sockets", '\0', "num-sockets", "num-sockets", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Number of sockets/board (1-256) [default: 1]"},
    { "orte", "num", "cores", '\0', "num-cores", "num-cores", 1,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Number of cores/socket (1-256) [default: 1]"},

    /* mpiexec-like arguments */
    { NULL, NULL, NULL, '\0', "wdir", "wdir", 1,
      &orterun_globals.wdir, OPAL_CMD_LINE_TYPE_STRING,
      "Set the working directory of the started processes" },
    { NULL, NULL, NULL, '\0', "wd", "wd", 1,
      &orterun_globals.wdir, OPAL_CMD_LINE_TYPE_STRING,
      "Synonym for --wdir" },
    { NULL, NULL, NULL, '\0', "path", "path", 1,
      &orterun_globals.path, OPAL_CMD_LINE_TYPE_STRING,
      "PATH to be used to look for executables to start processes" },

    /* User-level debugger arguments */
    { NULL, NULL, NULL, '\0', "tv", "tv", 0,
      &orterun_globals.debugger, OPAL_CMD_LINE_TYPE_BOOL,
      "Deprecated backwards compatibility flag; synonym for \"--debug\"" },
    { NULL, NULL, NULL, '\0', "debug", "debug", 0,
      &orterun_globals.debugger, OPAL_CMD_LINE_TYPE_BOOL,
      "Invoke the user-level debugger indicated by the orte_base_user_debugger MCA parameter" },
    { "orte", "base", "user_debugger", '\0', "debugger", "debugger", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Sequence of debuggers to search for when \"--debug\" is used" },

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

    { NULL, NULL, NULL, '\0', "tmpdir", "tmpdir", 1,
      &orte_process_info.tmpdir_base, OPAL_CMD_LINE_TYPE_STRING,
      "Set the root for the session directory tree for orterun ONLY" },

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

/* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

/*
 * Local functions
 */
static void job_completed(int trigpipe, short event, void *arg);
static void terminated(int trigpipe, short event, void *arg);
static void timeout_callback(int fd, short ign, void *arg);
static void abort_signal_callback(int fd, short flags, void *arg);
static void abort_exit_callback(int fd, short event, void *arg);
static void epipe_signal_callback(int fd, short flags, void *arg);
static void signal_forward_callback(int fd, short event, void *arg);
static int create_app(int argc, char* argv[], orte_app_context_t **app,
                      bool *made_app, char ***app_env);
static int init_globals(void);
static int parse_globals(int argc, char* argv[], opal_cmd_line_t *cmd_line);
static int parse_locals(int argc, char* argv[]);
static int parse_appfile(char *filename, char ***env);
static void dump_aborted_procs(void);


int orterun(int argc, char *argv[])
{
    int rc;
    opal_cmd_line_t cmd_line;
    char * tmp_env_var = NULL;

    /* find our basename (the name of the executable) so that we can
       use it in pretty-print error messages */
    orte_cmd_basename = opal_basename(argv[0]);

    /* Setup and parse the command line */
    init_globals();
    opal_cmd_line_create(&cmd_line, cmd_line_init);
    mca_base_cmd_line_setup(&cmd_line);
    if (ORTE_SUCCESS != (rc = opal_cmd_line_parse(&cmd_line, true,
                                                   argc, argv)) ) {
        return rc;
    }

    /*
     * Since this process can now handle MCA/GMCA parameters, make sure to
     * process them.
     */
    mca_base_cmd_line_process_args(&cmd_line, &environ, &environ);
    
    /* Need to initialize OPAL so that install_dirs are filled in */
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
    if (OPAL_SUCCESS != opal_init_util()) {
        exit(1);
    }
    
    /* setup the exit triggers */
    OBJ_CONSTRUCT(&orte_exit, orte_trigger_event_t);
    OBJ_CONSTRUCT(&orteds_exit, orte_trigger_event_t);
    
    /* flag that I am the HNP */
    orte_process_info.hnp = true;

    /* Setup MCA params */
    orte_register_params();

    /* Check for some "global" command line params */
    parse_globals(argc, argv, &cmd_line);
    OBJ_DESTRUCT(&cmd_line);

    /* create a new job object to hold the info for this one - the
     * jobid field will be filled in by the PLM when the job is
     * launched
     */
    jdata = OBJ_NEW(orte_job_t);
    if (NULL == jdata) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    /* check what user wants us to do with stdin */
    if (0 == strcmp(orterun_globals.stdin_target, "all")) {
        jdata->stdin_target = ORTE_VPID_WILDCARD;
    } else if (0 == strcmp(orterun_globals.stdin_target, "none")) {
        jdata->stdin_target = ORTE_VPID_INVALID;
    } else {
        jdata->stdin_target = strtoul(orterun_globals.stdin_target, NULL, 10);
    }
    
    /* Parse each app, adding it to the job object */
    parse_locals(argc, argv);
    
    if (0 == jdata->num_apps) {
        /* This should never happen -- this case should be caught in
        create_app(), but let's just double check... */
        orte_show_help("help-orterun.txt", "orterun:nothing-to-do",
                       true, orte_cmd_basename);
        exit(ORTE_ERROR_DEFAULT_EXIT_CODE);
    }

    /* save the environment for launch purposes. This MUST be
     * done so that we can pass it to any local procs we
     * spawn - otherwise, those local procs won't see any
     * non-MCA envars were set in the enviro prior to calling
     * orterun
     */
    orte_launch_environ = opal_argv_copy(environ);
    
#if OPAL_ENABLE_FT == 1
    /* Disable OPAL CR notifications for this tool */
    opal_cr_set_enabled(false);
    tmp_env_var = mca_base_param_env_var("opal_cr_is_tool");
    opal_setenv(tmp_env_var,
                "1",
                true, &environ);
    free(tmp_env_var);
#endif
    tmp_env_var = NULL; /* Silence compiler warning */
    
    /* Intialize our Open RTE environment
     * Set the flag telling orte_init that I am NOT a
     * singleton, but am "infrastructure" - prevents setting
     * up incorrect infrastructure that only a singleton would
     * require
     */
    if (ORTE_SUCCESS != (rc = orte_init(ORTE_NON_TOOL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
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
    
    /* setup an event we can wait for that will tell
     * us to terminate - both normal and abnormal
     * termination will call us here. Use the
     * same exit fd as the daemon does so that orted_comm
     * can cause either of us to exit since we share that code
     */
    if (ORTE_SUCCESS != (rc = orte_wait_event(&orterun_event, &orte_exit, "job_complete", job_completed))) {
        orte_show_help("help-orterun.txt", "orterun:event-def-failed", true,
                       orte_cmd_basename, ORTE_ERROR_NAME(rc));
        ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
        goto DONE;
    }
    
    /* setup an event that will
     * trigger when the orteds are gone and tell the orteds that it is
     * okay to finalize and exit, we are done with them.
     * We set this up here in order to provide a way for us to
     * wakeup and terminate should the daemons themselves fail to launch,
     * and before we define signal handlers since they will call the
     * exit event trigger!
     */
    if (ORTE_SUCCESS != (rc = orte_wait_event(&orteds_exit_event, &orteds_exit, "orted_exit", terminated))) {
        orte_show_help("help-orterun.txt", "orterun:event-def-failed", true,
                       orte_cmd_basename, ORTE_ERROR_NAME(rc));
        goto DONE;
    }

    /* setup callback for SIGPIPE */
    opal_signal_set(&epipe_handler, SIGPIPE,
                    epipe_signal_callback, &epipe_handler);
    opal_signal_add(&epipe_handler, NULL);
    /** setup callbacks for abort signals - from this point
     * forward, we need to abort in a manner that allows us
     * to cleanup
     */
    opal_signal_set(&term_handler, SIGTERM,
                    abort_signal_callback, &term_handler);
    opal_signal_add(&term_handler, NULL);
    opal_signal_set(&int_handler, SIGINT,
                    abort_signal_callback, &int_handler);
    opal_signal_add(&int_handler, NULL);

#ifndef __WINDOWS__
    /** setup callbacks for signals we should foward */
    opal_signal_set(&sigusr1_handler, SIGUSR1,
                    signal_forward_callback, &sigusr1_handler);
    opal_signal_add(&sigusr1_handler, NULL);
    opal_signal_set(&sigusr2_handler, SIGUSR2,
                    signal_forward_callback, &sigusr2_handler);
    opal_signal_add(&sigusr2_handler, NULL);
    if (orte_forward_job_control) {
        opal_signal_set(&sigtstp_handler, SIGTSTP,
                        signal_forward_callback, &sigtstp_handler);
        opal_signal_add(&sigtstp_handler, NULL);
        opal_signal_set(&sigcont_handler, SIGCONT,
                        signal_forward_callback, &sigcont_handler);
        opal_signal_add(&sigcont_handler, NULL);
    }
#endif  /* __WINDOWS__ */
    
    /* we are an hnp, so update the contact info field for later use */
    orte_process_info.my_hnp_uri = orte_rml.get_contact_info();
    
    /* we are also officially a daemon, so better update that field too */
    orte_process_info.my_daemon_uri = orte_rml.get_contact_info();
    
    /* If we have a prefix, then modify the PATH and
        LD_LIBRARY_PATH environment variables in our copy. This
        will ensure that any locally-spawned children will
        have our executables and libraries in their path

        For now, default to the prefix_dir provided in the first app_context.
        Since there always MUST be at least one app_context, we are safe in
        doing this.
    */
    if (NULL != ((orte_app_context_t*)jdata->apps->addr[0])->prefix_dir) {
        char *oldenv, *newenv, *lib_base, *bin_base;
        
        lib_base = opal_basename(opal_install_dirs.libdir);
        bin_base = opal_basename(opal_install_dirs.bindir);

        /* Reset PATH */
        newenv = opal_os_path( false, ((orte_app_context_t*)jdata->apps->addr[0])->prefix_dir, bin_base, NULL );
        oldenv = getenv("PATH");
        if (NULL != oldenv) {
            char *temp;
            asprintf(&temp, "%s:%s", newenv, oldenv );
            free( newenv );
            newenv = temp;
        }
        opal_setenv("PATH", newenv, true, &orte_launch_environ);
        if (orte_debug_flag) {
            opal_output(0, "%s: reset PATH: %s", orte_cmd_basename, newenv);
        }
        free(newenv);
        free(bin_base);
        
        /* Reset LD_LIBRARY_PATH */
        newenv = opal_os_path( false, ((orte_app_context_t*)jdata->apps->addr[0])->prefix_dir, lib_base, NULL );
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
                        orte_cmd_basename, newenv);
        }
        free(newenv);
        free(lib_base);
    }
    
    /* We actually do *not* want orterun to voluntarily yield() the
        processor more than necessary.  Orterun already blocks when
        it is doing nothing, so it doesn't use any more CPU cycles than
        it should; but when it *is* doing something, we do not want it
        to be unnecessarily delayed because it voluntarily yielded the
        processor in the middle of its work.
        
        For example: when a message arrives at orterun, we want the
        OS to wake us up in a timely fashion (which most OS's
        seem good about doing) and then we want orterun to process
        the message as fast as possible.  If orterun yields and lets
        aggressive MPI applications get the processor back, it may be a
        long time before the OS schedules orterun to run again
        (particularly if there is no IO event to wake it up).  Hence,
        routed OOB messages (for example) may be significantly delayed
        before being delivered to MPI processes, which can be
        problematic in some scenarios (e.g., COMM_SPAWN, BTL's that
        require OOB messages for wireup, etc.). */
    opal_progress_set_yield_when_idle(false);

    /* pre-condition any network transports that require it */
    if (ORTE_SUCCESS != (rc = orte_pre_condition_transports(jdata))) {
        ORTE_ERROR_LOG(rc);
        orte_show_help("help-orterun.txt", "orterun:precondition", false,
                       orte_cmd_basename, NULL, NULL, rc);
        return rc;
    }

    /* setup to listen for commands sent specifically to me, even though I would probably
     * be the one sending them! Unfortunately, since I am a participating daemon,
     * there are times I need to send a command to "all daemons", and that means *I* have
     * to receive it too
     */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DAEMON,
                                 ORTE_RML_NON_PERSISTENT, orte_daemon_recv, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* setup the data server */
    if (ORTE_SUCCESS != (rc = orte_data_server_init())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* if an uri for the ompi-server was provided, set the route */
    if (NULL != ompi_server) {
        opal_buffer_t buf;
        /* setup our route to the server */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &ompi_server, 1, OPAL_STRING);
        orte_rml_base_update_contact_info(&buf);
        OBJ_DESTRUCT(&buf);        
        /* check if we are to wait for the server to start - resolves
         * a race condition that can occur when the server is run
         * as a background job - e.g., in scripts
         */
        if (orterun_globals.wait_for_server) {
            /* ping the server */
            struct timeval timeout;
            timeout.tv_sec = orterun_globals.server_wait_timeout;
            timeout.tv_usec = 0;
            if (ORTE_SUCCESS != (rc = orte_rml.ping(ompi_server, &timeout))) {
                /* try it one more time */
                if (ORTE_SUCCESS != (rc = orte_rml.ping(ompi_server, &timeout))) {
                    /* okay give up */
                    orte_show_help("help-orterun.txt", "orterun:server-not-found", true,
                                   orte_cmd_basename, ompi_server,
                                   (long)orterun_globals.server_wait_timeout,
                                   ORTE_ERROR_NAME(rc));
                    orte_exit_status = ORTE_ERROR_DEFAULT_EXIT_CODE;
                    goto DONE;
                }
            }
        }
    }
    
    /* setup for debugging */
    orte_debugger_init_before_spawn(jdata);
    
    /* Spawn the job */
    rc = orte_plm.spawn(jdata);
    
    /* complete debugger interface */
    orte_debugger_init_after_spawn(jdata);
    
    /* now wait until the termination event fires */
    opal_event_dispatch();
    
    /* we only reach this point by jumping there due
     * to an error - so just cleanup and leave
     */
DONE:    
    /* whack any lingering session directory files from our jobs */
    orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
    
    /* cleanup our data server */
    orte_data_server_finalize();
    
    orte_finalize();
    free(orte_cmd_basename);
    return orte_exit_status;
}

static void job_completed(int trigpipe, short event, void *arg)
{
    int rc;
    orte_job_state_t exit_state;
    orte_job_t *daemons;
    
    /* if the abort exit event is set, delete it */
    if (NULL != abort_exit_event) {
        opal_evtimer_del(abort_exit_event);
        free(abort_exit_event);
    }
    
    exit_state = jdata->state;

    if (ORTE_JOB_STATE_TERMINATED != exit_state) {
        /* abnormal termination of some kind */
        dump_aborted_procs();
        /* If we showed more abort messages than were allowed,
        show a followup message here */
        if (num_failed_start > 1) {
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "<stderr>");
            }
            fprintf(orte_xml_fp, "%d total process%s failed to start",
                   num_failed_start, ((num_failed_start > 1) ? "es" : ""));
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "&#010;</stderr>");
            }
            fprintf(orte_xml_fp, "\n");
        }
        if (num_aborted > 1) {
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "<stderr>");
            }
            fprintf(orte_xml_fp, "%d total process%s aborted",
                   num_aborted, ((num_aborted > 1) ? "es" : ""));
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "&#010;</stderr>");
            }
            fprintf(orte_xml_fp, "\n");
        }
        if (num_killed > 1) {
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "<stderr>");
            }
            fprintf(orte_xml_fp, "%d total process%s killed (some possibly by %s during cleanup)",
                   num_killed, ((num_killed > 1) ? "es" : ""), orte_cmd_basename);
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "&#010;</stderr>");
            }
            fprintf(orte_xml_fp, "\n");
        }
    }
    
    /* if the debuggers were run, clean up */
    orte_debugger_finalize();

    if (ORTE_SUCCESS != (rc = orte_plm.terminate_orteds())) {        
        /* since we know that the sends didn't completely go out,
         * we know that the prior event will never fire. Add a timeout so
         * that those daemons that can respond have a chance to do
         * so
         */
        /* get the orted job data object */
        if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
            /* we are totally hozed */
            goto DONE;
        }
        ORTE_DETECT_TIMEOUT(&timeout_ev, daemons->num_procs,
                            orte_timeout_usec_per_proc,
                            orte_max_timeout, timeout_callback);
    }
    
#ifndef __WINDOWS__
    /* now wait to hear it has been done */
    opal_event_dispatch();
#else
    /* We are using WT_EXECUTEINWAITTHREAD mode of threading pool,
       the other callbacks won't be triggerred until this thread finishes,
       so just return to main thread and process the rest events there.  */
    return;
#endif
    
    /* if we cannot order the daemons to terminate, then
     * all we can do is cleanly exit ourselves
     */
DONE:
    /* whack any lingering session directory files from our jobs */
    orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
    
    /* cleanup our data server */
    orte_data_server_finalize();
    
    orte_finalize();
    free(orte_cmd_basename);
    exit(rc);
    
}

static void terminated(int trigpipe, short event, void *arg)
{
    orte_job_t *daemons;
    orte_proc_t **procs;
    orte_vpid_t i;
    
    /* clear the event timer */
    if (NULL != timeout_ev) {
        opal_evtimer_del(timeout_ev);
        free(timeout_ev);
    }
    
    /* Remove the epipe handler */
    opal_signal_del(&epipe_handler);
    /* Remove the TERM and INT signal handlers */
    opal_signal_del(&term_handler);
    opal_signal_del(&int_handler);
#ifndef __WINDOWS__
    /** Remove the USR signal handlers */
    opal_signal_del(&sigusr1_handler);
    opal_signal_del(&sigusr2_handler);
    if (orte_forward_job_control) {
        opal_signal_del(&sigtstp_handler);
        opal_signal_del(&sigcont_handler);
    }
#endif  /* __WINDOWS__ */
    
    /* get the daemon job object */
    if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        /* nothing more we can do - tell user something really messed
         * up and exit
         */
        orte_show_help("help-orterun.txt", "orterun:no-orted-object-exit",
                       true, orte_cmd_basename);
        goto finish;
    }
    
    /* did any daemons fail to respond? Remember we already
     * set ourselves to terminated
     */
    if (daemons->num_terminated != daemons->num_procs) {
        /* alert user to that fact and which nodes didn't respond and
         * print a warning that the user may still have some manual
         * cleanup to do.
         */
        orte_show_help("help-orterun.txt", "orterun:unclean-exit",
                       true, orte_cmd_basename);
        procs = (orte_proc_t**)daemons->procs->addr;
        for (i=1; i < daemons->num_procs; i++)
        {
            if (ORTE_PROC_STATE_TERMINATED != procs[i]->state) {
                /* print out node name */
                orte_node_t *node = procs[i]->node;
                if (NULL != node && NULL != node->name) {
                    if (NULL != procs[i]->rml_uri) {
                        fprintf(stderr, "\t%s\n", node->name);
                    } else {
                        fprintf(stderr, "\t%s - daemon did not report back when launched\n", node->name);
                    }
                }
            }
        }
    } else {
        /* we cleaned up! let the user know */
        if (!orterun_globals.quiet && orte_abnormal_term_ordered){
            fprintf(stderr, "%s: clean termination accomplished\n\n", orte_cmd_basename);
        }
    }
    
finish:
    /* now clean ourselves up and exit */
    
    /* whack any lingering session directory files from our jobs */
    orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);

    /* cleanup our data server */
    orte_data_server_finalize();
    
    orte_finalize();
    
    free(orte_cmd_basename);
    if (orte_debug_flag) {
        fprintf(stderr, "orterun: exiting with status %d\n", orte_exit_status);
    }
    exit(orte_exit_status);
}

/*
 * On abnormal termination - dump the
 * exit status of the aborted procs.
 */

static void dump_aborted_procs(void)
{
    orte_std_cntr_t i, n;
    orte_proc_t *proc, **procs;
    orte_app_context_t **apps;
    orte_job_t **jobs, *job;
    bool found=false;
    
    /* find the job that caused the problem - be sure to start the loop
     * at 1 as the daemons are in 0 and will clearly be "running", so no
     * point in checking them
     */
    jobs = (orte_job_t**)orte_job_data->addr;
    for (n=1; n < orte_job_data->size; n++) {
        if (NULL == jobs[n]) {
            /* the array is left-justified, so we can quit on the first NULL */
            return;
        }
        if (ORTE_JOB_STATE_UNDEF != jobs[n]->state &&
            ORTE_JOB_STATE_INIT != jobs[n]->state &&
            ORTE_JOB_STATE_LAUNCHED != jobs[n]->state &&
            ORTE_JOB_STATE_RUNNING != jobs[n]->state &&
            ORTE_JOB_STATE_TERMINATED != jobs[n]->state &&
            ORTE_JOB_STATE_ABORT_ORDERED != jobs[n]->state) {
            /* this is a guilty party */
            job = jobs[n];
            proc = job->aborted_proc;
            procs = (orte_proc_t**)job->procs->addr;
            apps = (orte_app_context_t**)job->apps->addr;
            /* flag that we found at least one */
            found = true;
            /* cycle through and count the number that were killed or aborted */
            for (i=0; i < job->procs->size; i++) {
                if (NULL == procs[i]) {
                    /* array is left-justfied - we are done */
                    break;
                }
                if (ORTE_PROC_STATE_FAILED_TO_START == procs[i]->state) {
                    ++num_failed_start;
                } else if (ORTE_PROC_STATE_ABORTED == procs[i]->state) {
                    ++num_aborted;
                } else if (ORTE_PROC_STATE_ABORTED_BY_SIG == procs[i]->state) {
                    ++num_killed;
                }
            }
            if (ORTE_JOB_STATE_FAILED_TO_START == job->state) {
                if (NULL == proc) {
                    orte_show_help("help-orterun.txt", "orterun:proc-failed-to-start-no-status-no-node", true,
                                   orte_cmd_basename);
                    return;
                }
                if (ORTE_ERR_SYS_LIMITS_PIPES == proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:sys-limit-pipe", true,
                                   orte_cmd_basename, proc->node->name,
                                   (unsigned long)proc->name.vpid);
                } else if (ORTE_ERR_PIPE_SETUP_FAILURE == proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:pipe-setup-failure", true,
                                   orte_cmd_basename, proc->node->name,
                                   (unsigned long)proc->name.vpid);
                } else if (ORTE_ERR_SYS_LIMITS_CHILDREN == proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:sys-limit-children", true,
                                   orte_cmd_basename, proc->node->name,
                                   (unsigned long)proc->name.vpid);
                } else if (ORTE_ERR_FAILED_GET_TERM_ATTRS == proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:failed-term-attrs", true,
                                   orte_cmd_basename, proc->node->name,
                                   (unsigned long)proc->name.vpid);
                } else if (ORTE_ERR_WDIR_NOT_FOUND == proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:wdir-not-found", true,
                                   orte_cmd_basename, apps[proc->app_idx]->cwd,
                                   proc->node->name, (unsigned long)proc->name.vpid);
                } else if (ORTE_ERR_EXE_NOT_FOUND == proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:exe-not-found", true,
                                   orte_cmd_basename, apps[proc->app_idx]->app,
                                   proc->node->name, (unsigned long)proc->name.vpid);
                } else if (ORTE_ERR_EXE_NOT_ACCESSIBLE == proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:exe-not-accessible", true,
                                   orte_cmd_basename, apps[proc->app_idx]->app, proc->node->name,
                                   (unsigned long)proc->name.vpid);
                } else if (ORTE_ERR_PIPE_READ_FAILURE == proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:pipe-read-failure", true,
                                   orte_cmd_basename, proc->node->name, (unsigned long)proc->name.vpid);
                } else if (0 != proc->exit_code) {
                    orte_show_help("help-orterun.txt", "orterun:proc-failed-to-start", true,
                                   orte_cmd_basename, ORTE_ERROR_NAME(proc->exit_code), proc->node->name,
                                   (unsigned long)proc->name.vpid);
                } else {
                    orte_show_help("help-orterun.txt", "orterun:proc-failed-to-start-no-status", true,
                                   orte_cmd_basename, proc->node->name);
                }
            } else if (ORTE_JOB_STATE_ABORTED == job->state) {
                if (NULL == proc) {
                    orte_show_help("help-orterun.txt", "orterun:proc-aborted-unknown", true,
                                   orte_cmd_basename);
                } else {
                    orte_show_help("help-orterun.txt", "orterun:proc-ordered-abort", true,
                                   orte_cmd_basename, (unsigned long)proc->name.vpid, (unsigned long)proc->pid,
                                   proc->node->name, orte_cmd_basename);
                }
            } else if (ORTE_JOB_STATE_ABORTED_BY_SIG == job->state) {  /* aborted by signal */
                if (NULL == proc) {
                    orte_show_help("help-orterun.txt", "orterun:proc-aborted-signal-unknown", true,
                                   orte_cmd_basename);
                } else {
#ifdef HAVE_STRSIGNAL
                    if (NULL != strsignal(WTERMSIG(proc->exit_code))) {
                        orte_show_help("help-orterun.txt", "orterun:proc-aborted-strsignal", true,
                                       orte_cmd_basename, (unsigned long)proc->name.vpid, (unsigned long)proc->pid,
                                       proc->node->name, WTERMSIG(proc->exit_code), 
                                       strsignal(WTERMSIG(proc->exit_code)));
                    } else {
#endif
                        orte_show_help("help-orterun.txt", "orterun:proc-aborted", true,
                                       orte_cmd_basename, (unsigned long)proc->name.vpid, (unsigned long)proc->pid,
                                       proc->node->name, WTERMSIG(proc->exit_code));
#ifdef HAVE_STRSIGNAL
                    }
#endif
                }
            } else if (ORTE_JOB_STATE_ABORTED_WO_SYNC == job->state) { /* proc exited w/o finalize */
                if (NULL == proc) {
                    orte_show_help("help-orterun.txt", "orterun:proc-exit-no-sync-unknown", true,
                                   orte_cmd_basename, orte_cmd_basename);
                } else {
                    orte_show_help("help-orterun.txt", "orterun:proc-exit-no-sync", true,
                                   orte_cmd_basename, (unsigned long)proc->name.vpid, (unsigned long)proc->pid,
                                   proc->node->name, orte_cmd_basename);
                }
            }
            return;
        }
    }
    
    /* if we got here, then we couldn't find the job that aborted -
     * report that fact and give up
     */
    orte_show_help("help-orterun.txt", "orterun:proc-aborted-unknown", true, orte_cmd_basename);
}

static void timeout_callback(int fd, short ign, void *arg)
{
    /* fire the trigger that takes us to terminated so we don't
     * loop back into trying to kill things
     */
    orte_trigger_event(&orteds_exit);
}

static void abort_exit_callback(int fd, short ign, void *arg)
{
    int ret;

    if (!orterun_globals.quiet){
        fprintf(stderr, "%s: killing job...\n\n", orte_cmd_basename);
    }
    
    /* since we are being terminated by a user's signal, be
     * sure to exit with a non-zero exit code - but don't
     * overwrite any error code from a proc that might have
     * failed, in case that is why the user ordered us
     * to terminate
     */
    ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);

    /* terminate the job - this will also wakeup orterun so
     * it can report to the user and kill all the orteds.
     * Check the jobid, though, just in case the user
     * hit ctrl-c before we had a chance to setup the
     * job in the system - in which case there is nothing
     * to terminate!
     *
     * NOTE: we don't have to worry about jdata being NULL
     * because we don't setup to trap the signals until
     * after jdata has been OBJ_NEW'd
     */
    if (jdata->jobid != ORTE_JOBID_INVALID) {
        /* terminate the job - this will wake us up and
         * call the "terminated" function so we clean up
         * and exit
         */
        ret = orte_plm.terminate_job(ORTE_JOBID_WILDCARD);
        if (ORTE_SUCCESS != ret) {
            /* If we failed the terminate_job() above, then we
             * need to explicitly wake ourselves up to exit
             */
            ORTE_UPDATE_EXIT_STATUS(ret);
            orte_trigger_event(&orte_exit);
        }
        /* give ourselves a time limit on how long to wait
         * for the job to die, just in case we can't make it go
         * away for some reason. Don't send us directly back
         * to job_completed, though, as that function may be
         * what has failed
         */
        ORTE_DETECT_TIMEOUT(&abort_exit_event, jdata->num_procs,
                            orte_timeout_usec_per_proc,
                            orte_max_timeout, 
                            timeout_callback);
        
    } else {
        /* if the jobid is invalid, then we didn't get to
         * the point of setting the job up, so there is nothing
         * to do but just clean ourselves up and exit
         */
        orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
        
        /* need to release jdata separately as it won't be
         * in the global array, and so won't be released
         * during finalize
         */
        OBJ_RELEASE(jdata);

        orte_finalize();
        free(orte_cmd_basename);
        ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
        exit(orte_exit_status);
    }
}

/*
 * Attempt to terminate the job and wait for callback indicating
 * the job has been aborted.
 */
static void abort_signal_callback(int fd, short flags, void *arg)
{
    /* if we have already ordered this once, don't keep
     * doing it to avoid race conditions
     */
    if (!opal_atomic_trylock(&orte_abort_inprogress_lock)) { /* returns 1 if already locked */
        if (forcibly_die) {
            /* kill any local procs */
            orte_odls.kill_local_procs(ORTE_JOBID_WILDCARD, false);
            
            /* whack any lingering session directory files from our jobs */
            orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
            
            /* cleanup our data server */
            orte_data_server_finalize();
            
            /* exit with a non-zero status */
            exit(ORTE_ERROR_DEFAULT_EXIT_CODE);
        }
        fprintf(stderr, "%s: abort is already in progress...hit ctrl-c again to forcibly terminate\n\n", orte_cmd_basename);
        forcibly_die = true;
        return;
    }

    /* set the global abnormal exit flag so we know not to
     * use the standard xcast for terminating orteds
     */
    orte_abnormal_term_ordered = true;
    /* ensure that the forwarding of stdin stops */
    orte_job_term_ordered = true;

    /* We are in an event handler; the job completed procedure
       will delete the signal handler that is currently running
       (which is a Bad Thing), so we can't call it directly.
       Instead, we have to exit this handler and setup to call
       job_completed() after this. */
    ORTE_TIMER_EVENT(0, 0, abort_exit_callback);
}

/**
 * Deal with sigpipe errors
 */
static void epipe_signal_callback(int fd, short flags, void *arg)
{
    /* for now, we just announce and ignore them */
    OPAL_OUTPUT_VERBOSE((1, orte_debug_verbosity,
                         "%s reports a SIGPIPE error on fd %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), fd));
    return;
}

/**
 * Pass user signals to the remote application processes
 */
static void  signal_forward_callback(int fd, short event, void *arg)
{
    struct opal_event *signal = (struct opal_event*)arg;
    int signum, ret;

    signum = OPAL_EVENT_SIGNAL(signal);
    if (!orterun_globals.quiet){
        fprintf(stderr, "%s: Forwarding signal %d to job\n",
                orte_cmd_basename, signum);
    }

    /** send the signal out to the processes, including any descendants */
    if (ORTE_SUCCESS != (ret = orte_plm.signal_job(jdata->jobid, signum))) {
        fprintf(stderr, "Signal %d could not be sent to the job (returned %d)",
                signum, ret);
    }
}


static int init_globals(void)
{
    /* Only CONSTRUCT things once */
    if (!globals_init) {
        OBJ_CONSTRUCT(&orterun_globals.lock, opal_mutex_t);
        orterun_globals.env_val =     NULL;
        orterun_globals.appfile =     NULL;
        orterun_globals.wdir =        NULL;
        orterun_globals.path =        NULL;
        orterun_globals.ompi_server = NULL;
        orterun_globals.wait_for_server = false;
        orterun_globals.server_wait_timeout = 10;
        orterun_globals.stdin_target = "0";
    }

    /* Reset the other fields every time */

    orterun_globals.help                       = false;
    orterun_globals.version                    = false;
    orterun_globals.verbose                    = false;
    orterun_globals.quiet                      = false;
    orterun_globals.report_pid                 = false;
    orterun_globals.by_node                    = false;
    orterun_globals.by_slot                    = false;
    orterun_globals.by_board                   = false;
    orterun_globals.by_socket                  = false;
    orterun_globals.bind_to_core               = false;
    orterun_globals.bind_to_board              = false;
    orterun_globals.bind_to_socket             = false;
    orterun_globals.debugger                   = false;
    orterun_globals.num_procs                  =  0;
    if( NULL != orterun_globals.env_val )
        free( orterun_globals.env_val );
    orterun_globals.env_val =     NULL;
    if( NULL != orterun_globals.appfile )
        free( orterun_globals.appfile );
    orterun_globals.appfile =     NULL;
    if( NULL != orterun_globals.wdir )
        free( orterun_globals.wdir );
    orterun_globals.wdir =        NULL;
    if( NULL != orterun_globals.path )
        free( orterun_globals.path );
    orterun_globals.path =        NULL;

    orterun_globals.preload_binary = false;
    orterun_globals.preload_files  = NULL;
    orterun_globals.preload_files_dest_dir = NULL;

    /* All done */
    globals_init = true;
    return ORTE_SUCCESS;
}


static int parse_globals(int argc, char* argv[], opal_cmd_line_t *cmd_line)
{
    /* print version if requested.  Do this before check for help so
       that --version --help works as one might expect. */
    if (orterun_globals.version && 
        !(1 == argc || orterun_globals.help)) {
        char *project_name = NULL;
        if (0 == strcmp(orte_cmd_basename, "mpirun")) {
            project_name = "Open MPI";
        } else {
            project_name = "OpenRTE";
        }
        orte_show_help("help-orterun.txt", "orterun:version", false,
                       orte_cmd_basename, project_name, OPAL_VERSION,
                       PACKAGE_BUGREPORT);
        /* if we were the only argument, exit */
        if (2 == argc) exit(0);
    }

    /* Check for help request */
    if (1 == argc || orterun_globals.help) {
        char *args = NULL;
        char *project_name = NULL;
        if (0 == strcmp(orte_cmd_basename, "mpirun")) {
            project_name = "Open MPI";
        } else {
            project_name = "OpenRTE";
        }
        args = opal_cmd_line_get_usage_msg(cmd_line);
        orte_show_help("help-orterun.txt", "orterun:usage", false,
                       orte_cmd_basename, project_name, OPAL_VERSION,
                       orte_cmd_basename, args,
                       PACKAGE_BUGREPORT);
        free(args);

        /* If someone asks for help, that should be all we do */
        exit(0);
    }

    /* check for request to report pid */
    if (orterun_globals.report_pid) {
        printf("%s pid: %d\n", orte_cmd_basename, (int)getpid());
    }
    
    /* Do we want a user-level debugger? */

    if (orterun_globals.debugger) {
        orte_run_debugger(orte_cmd_basename, cmd_line, argc, argv, orterun_globals.num_procs);
    }

    /* extract any rank assignment policy directives */
    if (orterun_globals.by_node) {
        ORTE_SET_MAPPING_POLICY(ORTE_MAPPING_BYNODE);
    } else if (orterun_globals.by_board) {
        ORTE_SET_MAPPING_POLICY(ORTE_MAPPING_BYBOARD);
    } else if (orterun_globals.by_socket) {
        ORTE_SET_MAPPING_POLICY(ORTE_MAPPING_BYSOCKET);
    } else if (orterun_globals.by_slot) {
        ORTE_SET_MAPPING_POLICY(ORTE_MAPPING_BYSLOT);
    }
    /* if nothing was specified, leave it as set by
     * mca param
     */
    
    /* extract any binding policy directives */
    if (orterun_globals.bind_to_socket) {
        ORTE_SET_BINDING_POLICY(ORTE_BIND_TO_SOCKET);
    } else if (orterun_globals.bind_to_board) {
        ORTE_SET_BINDING_POLICY(ORTE_BIND_TO_BOARD);
    } else if (orterun_globals.bind_to_core) {
        ORTE_SET_BINDING_POLICY(ORTE_BIND_TO_CORE);
    } else if (orterun_globals.bind_to_none) {
        ORTE_SET_BINDING_POLICY(ORTE_BIND_TO_NONE);
    }
    /* if nothing was specified, leave it as set
     * by mca param
     */
        
    return ORTE_SUCCESS;
}


static int parse_locals(int argc, char* argv[])
{
    int i, rc, app_num;
    int temp_argc;
    char **temp_argv, **env;
    orte_app_context_t *app;
    bool made_app;
    orte_std_cntr_t j, size1;

    /* if the ompi-server was given, then set it up here */
    if (NULL != orterun_globals.ompi_server) {
        /* someone could have passed us a file instead of a uri, so
         * we need to first check to see what we have - if it starts
         * with "file", then we know it is a file. Otherwise, we assume
         * it is a uri as provided by the ompi-server's output
         * of an ORTE-standard string. Note that this is NOT a standard
         * uri as it starts with the process name!
         */
        if (0 == strncmp(orterun_globals.ompi_server, "file", strlen("file")) ||
            0 == strncmp(orterun_globals.ompi_server, "FILE", strlen("FILE"))) {
            char input[1024], *filename;
            FILE *fp;
            
            /* it is a file - get the filename */
            filename = strchr(orterun_globals.ompi_server, ':');
            if (NULL == filename) {
                /* filename is not correctly formatted */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-bad", true,
                               orte_cmd_basename, orterun_globals.ompi_server);
                exit(1);
            }
            ++filename; /* space past the : */
            
            if (0 >= strlen(filename)) {
                /* they forgot to give us the name! */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-missing", true,
                               orte_cmd_basename, orterun_globals.ompi_server);
                exit(1);
            }
            
            /* open the file and extract the uri */
            fp = fopen(filename, "r");
            if (NULL == fp) { /* can't find or read file! */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-access", true,
                               orte_cmd_basename, orterun_globals.ompi_server);
                exit(1);
            }
            if (NULL == fgets(input, 1024, fp)) {
                /* something malformed about file */
                fclose(fp);
                orte_show_help("help-orterun.txt", "orterun:ompi-server-file-bad", true,
                               orte_cmd_basename, orterun_globals.ompi_server,
                               orte_cmd_basename);
                exit(1);
            }
            fclose(fp);
            input[strlen(input)-1] = '\0';  /* remove newline */
            ompi_server = strdup(input);
        } else {
            ompi_server = strdup(orterun_globals.ompi_server);
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
    for (app_num = 0, i = 1; i < argc; ++i) {
        if (0 == strcmp(argv[i], ":")) {
            /* Make an app with this argv */
            if (opal_argv_count(temp_argv) > 1) {
                if (NULL != env) {
                    opal_argv_free(env);
                    env = NULL;
                }
                app = NULL;
                rc = create_app(temp_argc, temp_argv, &app, &made_app, &env);
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
                    /* The v1.3/v1.4 series does not support more than 127
                       app contexts.  v1.5 and beyond does support more
                       than 127 contexts, but back-porting the fix would
                       be a gigantic pain.  See
                       https://svn.open-mpi.org/trac/ompi/ticket/2430 for
                       more details. */
                    if (app_num > 127) {
                        orte_show_help("help-orterun.txt", "too many app contexts",
                                       true, orte_cmd_basename);
                        exit(1);
                    }
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
        rc = create_app(temp_argc, temp_argv, &app, &made_app, &env);
        if (ORTE_SUCCESS != rc) {
            /* Assume that the error message has already been printed;
               no need to cleanup -- we can just exit */
            exit(1);
        }
        if (made_app) {
            app->idx = app_num;
            ++app_num;
            /* The v1.3/v1.4 series does not support more than 127 app
               contexts.  v1.5 and beyond does support more than 127
               contexts, but back-porting the fix would be a gigantic
               pain.  See
               https://svn.open-mpi.org/trac/ompi/ticket/2430 for more
               details. */
            if (app_num > 127) {
                orte_show_help("help-orterun.txt", "too many app contexts",
                               true, orte_cmd_basename);
                exit(1);
            }
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
                                                   true, orte_cmd_basename, argv[i+1],
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
static int create_app(int argc, char* argv[], orte_app_context_t **app_ptr,
                      bool *made_app, char ***app_env)
{
    opal_cmd_line_t cmd_line;
    char cwd[OMPI_PATH_MAX];
    int i, j, count, rc;
    char *param, *value, *value2;
    orte_app_context_t *app = NULL;
    bool cmd_line_made = false;

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
    if (NULL != orterun_globals.appfile) {
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

    if (NULL != orterun_globals.appfile) {
        OBJ_DESTRUCT(&cmd_line);
        return parse_appfile(strdup(orterun_globals.appfile), app_env);
    }

    /* Setup application context */

    app = OBJ_NEW(orte_app_context_t);
    opal_cmd_line_get_tail(&cmd_line, &count, &app->argv);

    /* See if we have anything left */

    if (0 == count) {
        orte_show_help("help-orterun.txt", "orterun:executable-not-specified",
                       true, orte_cmd_basename, orte_cmd_basename);
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
        bool found_serv = false;
        asprintf(&param, "OMPI_MCA_pubsub_orte_server=%s", ompi_server);
        /* this shouldn't exist, but if it does... */
        for (i=0; i < opal_argv_count(app->env); i++) {
            if (0 == strcmp(param, app->env[i])) {
                free(app->env[i]);
                app->env[i] = strdup(param);
                found_serv = true;
                break;
            }
        }
        if (!found_serv) {
            opal_argv_append_nosize(&app->env, param); /* add it */
        }
        free(param);
    }

    /* Did the user request to export any environment variables? */

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

    /* If the user specified --path, store it in the user's app
       environment via the OMPI_exec_path variable. */
    if (NULL != orterun_globals.path) {
        asprintf(&value, "OMPI_exec_path=%s", orterun_globals.path);
        opal_argv_append_nosize(&app->env, value);
        free(value);
    }

    /* Did the user request a specific wdir? */

    if (NULL != orterun_globals.wdir) {
        /* if this is a relative path, convert it to an absolute path */
        if (opal_path_is_absolute(orterun_globals.wdir)) {
            app->cwd = strdup(orterun_globals.wdir);
        } else {
            /* get the cwd */
            if (OPAL_SUCCESS != (rc = opal_getcwd(cwd, sizeof(cwd)))) {
                orte_show_help("help-orterun.txt", "orterun:init-failure",
                               true, "get the cwd", rc);
                goto cleanup;
            }
            /* construct the absolute path */
            app->cwd = opal_os_path(false, cwd, orterun_globals.wdir, NULL);
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

    /* Check to see if the user explicitly wanted to disable automatic
       --prefix behavior */

    if (opal_cmd_line_is_taken(&cmd_line, "noprefix")) {
        want_prefix_by_default = false;
    }

    /* Did the user specify a specific prefix for this app_context_t
       or provide an absolute path name to argv[0]? */
    if (opal_cmd_line_is_taken(&cmd_line, "prefix") ||
        '/' == argv[0][0] || want_prefix_by_default) {
        size_t param_len;

        /* The --prefix option takes precedence over /path/to/orterun */
        if (opal_cmd_line_is_taken(&cmd_line, "prefix")) {
            param = opal_cmd_line_get_param(&cmd_line, "prefix", 0, 0);
        } 
        /* /path/to/orterun */
        else if ('/' == argv[0][0]) {
            char* tmp_basename = NULL;
            /* If they specified an absolute path, strip off the
               /bin/<exec_name>" and leave just the prefix */
            param = opal_dirname(argv[0]);
            /* Quick sanity check to ensure we got
               something/bin/<exec_name> and that the installation
               tree is at least more or less what we expect it to
               be */
            tmp_basename = opal_basename(param);
            if (0 == strcmp("bin", tmp_basename)) {
                char* tmp = param;
                param = opal_dirname(tmp);
                free(tmp);
            } else {
                free(param);
                param = NULL;
            }
            free(tmp_basename);
        }
        /* --enable-orterun-prefix-default was given to orterun */
        else {
            param = opal_install_dirs.prefix;
        }

        if (NULL != param) {
            /* "Parse" the param, aka remove superfluous path_sep. */
            param_len = strlen(param);
            while (0 == strcmp (OPAL_PATH_SEP, &(param[param_len-1]))) {
                param[param_len-1] = '\0';
                param_len--;
                if (0 == param_len) {
                    orte_show_help("help-orterun.txt", "orterun:empty-prefix",
                                   true, orte_cmd_basename, orte_cmd_basename);
                    return ORTE_ERR_FATAL;
                }
            }

            app->prefix_dir = strdup(param);
        }
    }

    /* Did the user specify a hostfile. Need to check for both 
     * hostfile and machine file. 
     * We can only deal with one hostfile per app context, otherwise give an error.
     */
    if (0 < (j = opal_cmd_line_get_ninsts(&cmd_line, "hostfile"))) {
        if(1 < j) {
            orte_show_help("help-orterun.txt", "orterun:multiple-hostfiles",
                           true, orte_cmd_basename, NULL);
            return ORTE_ERR_FATAL;
        } else {
            value = opal_cmd_line_get_param(&cmd_line, "hostfile", 0, 0);
            app->hostfile = strdup(value);
        }
    }
    if (0 < (j = opal_cmd_line_get_ninsts(&cmd_line, "machinefile"))) {
        if(1 < j || NULL != app->hostfile) {
            orte_show_help("help-orterun.txt", "orterun:multiple-hostfiles",
                           true, orte_cmd_basename, NULL);
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

    app->num_procs = (orte_std_cntr_t)orterun_globals.num_procs;

    /* If the user didn't specify the number of processes to run, then we
       default to launching an app process using every slot. We can't do
       anything about that here - we leave it to the RMAPS framework's
       components to note this and deal with it later.
        
       HOWEVER, we ONLY support this mode of operation if the number of
       app_contexts is equal to ONE. If the user provides multiple applications,
       we simply must have more information - in this case, generate an
       error.
    */
    if (app->num_procs == 0) {
        have_zero_np = true;  /** flag that we have a zero_np situation */
    }

    if (0 < total_num_apps && have_zero_np) {
        /** we have more than one app and a zero_np - that's no good.
         * note that we have to do this as a two step logic check since
         * the user may fail to specify num_procs for the first app, but
         * then give us another application.
         */
        orte_show_help("help-orterun.txt", "orterun:multi-apps-and-zero-np",
                       true, orte_cmd_basename, NULL);
        return ORTE_ERR_FATAL;
    }
    
    total_num_apps++;

    /* Preserve if we are to preload the binary */
    app->preload_binary = orterun_globals.preload_binary;
    if( NULL != orterun_globals.preload_files)
        app->preload_files  = strdup(orterun_globals.preload_files);
    else 
        app->preload_files = NULL;
    if( NULL != orterun_globals.preload_files_dest_dir)
        app->preload_files_dest_dir  = strdup(orterun_globals.preload_files_dest_dir);
    else 
        app->preload_files_dest_dir = NULL;


    /* Do not try to find argv[0] here -- the starter is responsible
       for that because it may not be relevant to try to find it on
       the node where orterun is executing.  So just strdup() argv[0]
       into app. */

    app->app = strdup(app->argv[0]);
    if (NULL == app->app) {
        orte_show_help("help-orterun.txt", "orterun:call-failed",
                       true, orte_cmd_basename, "library", "strdup returned NULL", errno);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
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
    int rc, argc, app_num;
    char **argv;
    orte_app_context_t *app;
    bool blank, made_app;
    char bogus[] = "bogus ";
    char **tmp_env;

    /*
     * Make sure to clear out this variable so we don't do anything odd in
     * app_create()
     */
    if( NULL != orterun_globals.appfile ) {
        free( orterun_globals.appfile );
        orterun_globals.appfile =     NULL;
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
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
            } else {
                tmp_env = NULL;
            }

            rc = create_app(argc, argv, &app, &made_app, &tmp_env);
            if (ORTE_SUCCESS != rc) {
                /* Assume that the error message has already been
                   printed */
                fclose(fp);
                orte_finalize();
                exit(1);
            }
            if (NULL != tmp_env) {
                opal_argv_free(tmp_env);
            }
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
