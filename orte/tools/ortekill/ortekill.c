/* -*- C -*-
 *
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc. All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"

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

#include "opal/event/event.h"
#include "opal/mca/base/base.h"
#include "opal/threads/condition.h"
#include "opal/util/argv.h"
#include "opal/util/basename.h"
#include "opal/util/cmd_line.h"
#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/trace.h"
#include "opal/version.h"

#include "orte/orte_constants.h"

#include "orte/class/orte_pointer_array.h"
#include "orte/util/proc_info.h"
#include "orte/util/sys_info.h"
#include "orte/util/universe_setup_file_io.h"
#include "orte/util/pre_condition_transports.h"

#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/pls/pls.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/schema/schema.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"

/*
 * Globals
 */
static char *ortekill_basename = NULL;

/*
 * setup globals for catching orterun command line options
 */
struct globals_t {
    bool help;
    bool version;
    bool verbose;
    bool quiet;
    bool exit;
    bool no_wait_for_job_completion;
    bool by_node;
    bool by_slot;
    bool per_node;
    bool no_oversubscribe;
    bool debugger;
    bool no_local_schedule;
    bool displaymapatlaunch;
    int num_procs;
    int exit_status;
    char *hostfile;
    char *env_val;
    char *appfile;
    char *wdir;
    char *path;
    opal_mutex_t lock;
    opal_condition_t cond;
} ortekill_globals;


opal_cmd_line_init_t cmd_line_init[] = {
    /* Various "obvious" options */
    { NULL, NULL, NULL, 'h', NULL, "help", 0,
      &ortekill_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },
    { NULL, NULL, NULL, 'V', NULL, "version", 0,
      &ortekill_globals.version, OPAL_CMD_LINE_TYPE_BOOL,
      "Print version and exit" },
    { NULL, NULL, NULL, 'v', NULL, "verbose", 0,
      &ortekill_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Be verbose" },
    { NULL, NULL, NULL, 'q', NULL, "quiet", 0,
      &ortekill_globals.quiet, OPAL_CMD_LINE_TYPE_BOOL,
      "Suppress helpful messages" },

    /* Use an appfile */
    { NULL, NULL, NULL, '\0', NULL, "app", 1,
      &ortekill_globals.appfile, OPAL_CMD_LINE_TYPE_STRING,
      "Provide an appfile; ignore all other command line options" },

    /* Number of processes; -c, -n, --n, -np, and --np are all
       synonyms */
    { NULL, NULL, NULL, 'c', "np", "np", 1,
      &ortekill_globals.num_procs, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run" },
    { NULL, NULL, NULL, '\0', "n", "n", 1,
      &ortekill_globals.num_procs, OPAL_CMD_LINE_TYPE_INT,
      "Number of processes to run" },
    
    /* Set a hostfile */
    { "rds", "hostfile", "path", '\0', "hostfile", "hostfile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile" },
    { "rds", "hostfile", "path", '\0', "machinefile", "machinefile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile" },

    /* Don't wait for the process to finish before exiting */
    { NULL, NULL, NULL, '\0', "nw", "nw", 0,
      &ortekill_globals.no_wait_for_job_completion, OPAL_CMD_LINE_TYPE_BOOL,
      "Launch the processes and do not wait for their completion (i.e., let orterun complete as soon a successful launch occurs)" },

    /* Export environment variables; potentially used multiple times,
       so it does not make sense to set into a variable */
    { NULL, NULL, NULL, 'x', NULL, NULL, 1,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      "Export an environment variable, optionally specifying a value (e.g., \"-x foo\" exports the environment variable foo and takes its value from the current environment; \"-x foo=bar\" exports the environment variable name foo and sets its value to \"bar\" in the started processes)" },

    /* Specific mapping (C, cX, N, nX) */
#if 0
    /* JJH --map is not currently implemented so don't advertise it until it is */
    { NULL, NULL, NULL, '\0', NULL, "map", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Mapping of processes to nodes / CPUs" },
#endif
    { NULL, NULL, NULL, '\0', "bynode", "bynode", 0,
      &ortekill_globals.by_node, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to allocate/map processes round-robin by node" },
    { NULL, NULL, NULL, '\0', "byslot", "byslot", 0,
      &ortekill_globals.by_slot, OPAL_CMD_LINE_TYPE_BOOL,
      "Whether to allocate/map processes round-robin by slot (the default)" },
    { NULL, NULL, NULL, '\0', "pernode", "pernode", 0,
        &ortekill_globals.per_node, OPAL_CMD_LINE_TYPE_BOOL,
        "If no number of process is specified, this will cause one process per available node to be executed" },
    { NULL, NULL, NULL, '\0', "nooversubscribe", "nooversubscribe", 0,
      &ortekill_globals.no_oversubscribe, OPAL_CMD_LINE_TYPE_BOOL,
      "Nodes are not to be oversubscribed, even if the system supports such operation"},
    { NULL, NULL, NULL, '\0', "display-map-at-launch", "display-map-at-launch", 0,
        &ortekill_globals.displaymapatlaunch, OPAL_CMD_LINE_TYPE_BOOL,
        "Display the process map just before launch"},
    
    /* mpiexec-like arguments */
    { NULL, NULL, NULL, '\0', "wdir", "wdir", 1,
      &ortekill_globals.wdir, OPAL_CMD_LINE_TYPE_STRING,
      "Set the working directory of the started processes" },
    { NULL, NULL, NULL, '\0', "path", "path", 1,
      &ortekill_globals.path, OPAL_CMD_LINE_TYPE_STRING,
      "PATH to be used to look for executables to start processes" },
    /* These arguments can be specified multiple times */
#if 0
    /* JMS: Removed because it's not really implemented */
    { NULL, NULL, NULL, '\0', "arch", "arch", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Architecture to start processes on" },
#endif
    { NULL, NULL, NULL, 'H', "host", "host", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "List of hosts to invoke processes on" },

    /* OSC mpiexec-like arguments */
    { NULL, NULL, NULL, '\0', "nolocal", "nolocal", 0,
      &ortekill_globals.no_local_schedule, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not run any MPI applications on the local node" },

    /* User-level debugger arguments */
    { NULL, NULL, NULL, '\0', "tv", "tv", 0,
      &ortekill_globals.debugger, OPAL_CMD_LINE_TYPE_BOOL,
      "Deprecated backwards compatibility flag; synonym for \"--debug\"" },
    { NULL, NULL, NULL, '\0', "debug", "debug", 0,
      &ortekill_globals.debugger, OPAL_CMD_LINE_TYPE_BOOL,
      "Invoke the user-level debugger indicated by the orte_base_user_debugger MCA parameter" },
    { "orte", "base", "user_debugger", '\0', "debugger", "debugger", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Sequence of debuggers to search for when \"--debug\" is used" },

    /* OpenRTE arguments */
    { "orte", "debug", NULL, 'd', NULL, "debug-devel", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE" },
    
    { "orte", "debug", "daemons", '\0', NULL, "debug-daemons", 0,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Enable debugging of any OpenRTE daemons used by this application" },
    
    { "orte", "debug", "daemons_file", '\0', NULL, "debug-daemons-file", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of any OpenRTE daemons used by this application, storing output in files" },
    
    { "orte", "no_daemonize", NULL, '\0', NULL, "no-daemonize", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not detach OpenRTE daemons used by this application" },
    
    { "universe", NULL, NULL, '\0', NULL, "universe", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Set the universe name as username@hostname:universe_name for this application" },
    
    { NULL, NULL, NULL, '\0', NULL, "tmpdir", 1,
      &orte_process_info.tmpdir_base, OPAL_CMD_LINE_TYPE_STRING,
      "Set the root for the session directory tree for orterun ONLY" },

    { NULL, NULL, NULL, '\0', NULL, "prefix", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Prefix where Open MPI is installed on remote nodes" },
    { NULL, NULL, NULL, '\0', NULL, "noprefix", 0,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Disable automatic --prefix behavior" },

    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};


int main(int argc, char *argv[])
{
    int rc;
    int id, iparam;

    /* Setup MCA params */

    mca_base_param_init();
    orte_register_params(false);

    /* find our basename (the name of the executable) so that we can
       use it in pretty-print error messages */
    ortekill_basename = opal_basename(argv[0]);

     /* Intialize our Open RTE environment */
    /* Set the flag telling orte_init that I am NOT a
     * singleton, but am "infrastructure" - prevents setting
     * up incorrect infrastructure that only a singleton would
     * require
     */
    if (ORTE_SUCCESS != (rc = orte_init(ORTE_INFRASTRUCTURE))) {
        opal_show_help("help-orterun.txt", "orterun:init-failure", true,
                       "orte_init()", rc);
        return rc;
    }

    /* check for daemon flags and push them into the environment
     * since this isn't being automatically done
     */
    id = mca_base_param_reg_int_name("orte_debug", "daemons",
                                     "Whether to debug the ORTE daemons or not",
                                     false, false, (int)false, &iparam);
    if (iparam) {
        char *tmp = mca_base_param_environ_variable("orte", "debug", "daemons");
        if (ORTE_SUCCESS != (rc = opal_setenv(tmp, "1", true, &environ))) {
            opal_show_help("help-orterun.txt", "orterun:environ", false,
                           ortekill_basename, tmp, "1", rc);
            free(tmp);
            return rc;
        }
        free(tmp);
    }
    id = mca_base_param_reg_int_name("orte", "debug",
                                     "Top-level ORTE debug switch",
                                     false, false, 0, &iparam);
    if (iparam) {
        char *tmp = mca_base_param_environ_variable("orte", NULL, "debug");
        if (ORTE_SUCCESS != (rc = opal_setenv(tmp, "1", true, &environ))) {
            opal_show_help("help-orterun.txt", "orterun:environ", false,
                           ortekill_basename, tmp, "1", rc);
            free(tmp);
            return rc;
        }
        free(tmp);
    }
    id = mca_base_param_reg_int_name("orte_debug", "daemons_file",
                                     "Whether want stdout/stderr of daemons to go to a file or not",
                                     false, false, 0, &iparam);
    if (iparam) {
        char *tmp = mca_base_param_environ_variable("orte", "debug",
                                                    "daemons_file");
        if (ORTE_SUCCESS != (rc = opal_setenv(tmp, "1", true, &environ))) {
            opal_show_help("help-orterun.txt", "orterun:environ", false,
                           ortekill_basename, tmp, "1", rc);
            free(tmp);
            return rc;
        }
        free(tmp);
    }
        
    orte_finalize();
    free(ortekill_basename);
    return rc;
}
