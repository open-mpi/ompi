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
 * Copyright (c) 2006      Cisco Systems, Inc. All rights reserved.
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
#include "opal/install_dirs.h"
#include "opal/mca/base/base.h"
#include "opal/threads/condition.h"
#include "opal/util/argv.h"
#include "opal/util/basename.h"
#include "opal/util/cmd_line.h"
#include "opal/util/daemon_init.h"
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
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"

/*
 * Globals
 */
static char *orteboot_basename = NULL;

/*
 * setup globals for catching orteboot command line options
 */
struct globals_t {
    bool help;
    bool version;
    bool verbose;
    bool quiet;
    bool exit;
    bool debug;
    char *hostfile;
    char *wdir;
    opal_mutex_t lock;
    opal_condition_t cond;
} orteboot_globals;


opal_cmd_line_init_t cmd_line_init[] = {
    /* Various "obvious" options */
    { NULL, NULL, NULL, 'h', NULL, "help", 0,
      &orteboot_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },
    { NULL, NULL, NULL, 'V', NULL, "version", 0,
      &orteboot_globals.version, OPAL_CMD_LINE_TYPE_BOOL,
      "Print version and exit" },
    { NULL, NULL, NULL, 'v', NULL, "verbose", 0,
      &orteboot_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Be verbose" },
    { NULL, NULL, NULL, 'q', NULL, "quiet", 0,
      &orteboot_globals.quiet, OPAL_CMD_LINE_TYPE_BOOL,
      "Suppress helpful messages" },

    /* Set a hostfile */
    { "rds", "hostfile", "path", '\0', "hostfile", "hostfile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile" },
    { "rds", "hostfile", "path", '\0', "machinefile", "machinefile", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile" },

    /* mpiexec-like arguments */
    { NULL, NULL, NULL, '\0', "wdir", "wdir", 1,
      &orteboot_globals.wdir, OPAL_CMD_LINE_TYPE_STRING,
      "Set the working directory of the started processes" },

    /* These arguments can be specified multiple times */
    { NULL, NULL, NULL, 'H', "host", "host", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "List of hosts to invoke processes on" },

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
      "Set the root for the session directory tree for orteboot ONLY" },

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

#if !defined(__WINDOWS__)
extern char** environ;
#endif   /* !defined(__WINDOWS__) */
/*
 * Local functions
 */

int main(int argc, char *argv[])
{
    int rc;
    int id, iparam;
    opal_cmd_line_t cmd_line;

    OBJ_CONSTRUCT(&orteboot_globals.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&orteboot_globals.cond, opal_condition_t);
    orteboot_globals.hostfile = NULL;
    orteboot_globals.wdir = NULL;
    orteboot_globals.help = false;
    orteboot_globals.version = false;
    orteboot_globals.verbose = false;
    orteboot_globals.exit = false;
    orteboot_globals.debug = false;
    
    /* Setup MCA params */
    mca_base_param_init();

    /* find our basename (the name of the executable) so that we can
     * use it in pretty-print error messages
     */
    orteboot_basename = opal_basename(argv[0]);

    /* Setup and parse the command line */
    opal_cmd_line_create(&cmd_line, cmd_line_init);
    mca_base_cmd_line_setup(&cmd_line);
    if (ORTE_SUCCESS != (rc = opal_cmd_line_parse(&cmd_line, true,
                                                   argc, argv))) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        opal_show_help("help-orteboot.txt", "orteboot:usage", false,
                       argv[0], args);
        free(args);
        return rc;
    }
    
    /* print version if requested.  Do this before check for help so
        that --version --help works as one might expect. */
    if (orteboot_globals.version && 
        !(1 == argc || orteboot_globals.help)) {
        char *project_name = NULL;
        if (0 == strcmp(orteboot_basename, "ompiboot")) {
            project_name = "Open MPI";
        } else {
            project_name = "OpenRTE";
        }
        opal_show_help("help-orteboot.txt", "orteboot:version", false,
                       orteboot_basename, project_name, OPAL_VERSION,
                       PACKAGE_BUGREPORT);
        /* if we were the only argument, exit */
        if (2 == argc) exit(0);
    }
    
    /* Check for help request */
    if (1 == argc || orteboot_globals.help) {
        char *args = NULL;
        char *project_name = NULL;
        if (0 == strcmp(orteboot_basename, "ompiboot")) {
            project_name = "Open MPI";
        } else {
            project_name = "OpenRTE";
        }
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        opal_show_help("help-orteboot.txt", "orteboot:usage", false,
                       orteboot_basename, project_name, OPAL_VERSION,
                       orteboot_basename, args,
                       PACKAGE_BUGREPORT);
        free(args);
        
        /* If someone asks for help, that should be all we do */
        exit(0);
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
            opal_show_help("help-orteboot.txt", "orteboot:environ", false,
                           orteboot_basename, tmp, "1", rc);
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
            opal_show_help("help-orteboot.txt", "orteboot:environ", false,
                           orteboot_basename, tmp, "1", rc);
            free(tmp);
            return rc;
        }
        free(tmp);
        /* set the local debug flag too! */
        orteboot_globals.debug = true;
    }
    
    id = mca_base_param_reg_int_name("orte_debug", "daemons_file",
                                     "Whether want stdout/stderr of daemons to go to a file or not",
                                     false, false, 0, &iparam);
    if (iparam) {
        char *tmp = mca_base_param_environ_variable("orte", "debug",
                                                    "daemons_file");
        if (ORTE_SUCCESS != (rc = opal_setenv(tmp, "1", true, &environ))) {
            opal_show_help("help-orteboot.txt", "orteboot:environ", false,
                           orteboot_basename, tmp, "1", rc);
            free(tmp);
            return rc;
        }
        free(tmp);
    }
    id = mca_base_param_reg_int_name("orte", "no_daemonize",
                                     "Whether to properly daemonize the ORTE daemons or not",
                                     false, false, 0, &iparam);
    if (iparam) {
        char *tmp = mca_base_param_environ_variable("orte", "no_daemonize", NULL);
        if (ORTE_SUCCESS != (rc = opal_setenv(tmp, "1", true, &environ))) {
            opal_show_help("help-orteboot.txt", "orteboot:environ", false,
                           orteboot_basename, tmp, "1", rc);
            free(tmp);
            return rc;
        }
        free(tmp);
    }
    
    /* just do a fork/exec of orted --seed --persistent and then exit */
    
    
    free(orteboot_basename);
    return ORTE_SUCCESS;
}

