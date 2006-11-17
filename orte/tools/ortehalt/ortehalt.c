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
#include "orte/orte_constants.h"

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
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

#include "opal/event/event.h"
#include "opal/install_dirs.h"
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

#include "orte/class/orte_pointer_array.h"
#include "orte/util/proc_info.h"
#include "orte/util/sys_info.h"
#include "orte/util/universe_setup_file_io.h"

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

static char *orte_basename = NULL;

/*
 * setup globals for catching orterun command line options
 */
struct globals_t {
    bool help;
    bool version;
    bool verbose;
    bool quiet;
    bool exit;
    int exit_status;
    char *wdir;
    char *path;
    opal_mutex_t lock;
    opal_condition_t cond;
} ortehalt_globals;


opal_cmd_line_init_t cmd_line_init[] = {
    /* Various "obvious" options */
    { NULL, NULL, NULL, 'h', NULL, "help", 0,
      &ortehalt_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },
    { NULL, NULL, NULL, 'V', NULL, "version", 0,
      &ortehalt_globals.version, OPAL_CMD_LINE_TYPE_BOOL,
      "Print version and exit" },
    { NULL, NULL, NULL, 'v', NULL, "verbose", 0,
      &ortehalt_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Be verbose" },
    { NULL, NULL, NULL, 'q', NULL, "quiet", 0,
      &ortehalt_globals.quiet, OPAL_CMD_LINE_TYPE_BOOL,
      "Suppress helpful messages" },

    /* OpenRTE arguments */
    { "orte", "debug", NULL, 'd', NULL, "debug-devel", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE" },
    
    { "universe", NULL, NULL, '\0', NULL, "universe", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Set the universe name as username@hostname:universe_name for this application" },
    
    { NULL, NULL, NULL, '\0', NULL, "tmpdir", 1,
      &orte_process_info.tmpdir_base, OPAL_CMD_LINE_TYPE_STRING,
      "Set the root for the session directory tree for orterun ONLY" },

    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

#if !defined(__WINDOWS__)
extern char** environ;
#endif   /* !defined(__WINDOWS__) */

int main(int argc, char *argv[])
{
    int rc;
    int id, iparam;

    /* Setup MCA params */

    mca_base_param_init();
    orte_register_params(false);

    /* find our basename (the name of the executable) so that we can
       use it in pretty-print error messages */
    orte_basename = opal_basename(argv[0]);

    /* check for daemon flags and push them into the environment
        * since this isn't being automatically done
        */
    id = mca_base_param_reg_int_name("orte", "debug",
                                     "Top-level ORTE debug switch",
                                     false, false, 0, &iparam);
    if (iparam) {
        char *tmp = mca_base_param_environ_variable("orte", NULL, "debug");
        if (ORTE_SUCCESS != (rc = opal_setenv(tmp, "1", true, &environ))) {
            opal_show_help("help-ortehalt.txt", "ortehalt:environ", false,
                           orte_basename, tmp, "1", rc);
            free(tmp);
            return rc;
        }
        free(tmp);
    }
    
    /* Intialize our Open RTE environment */
    /* Set the flag telling orte_init that I am NOT a
     * singleton, but am "infrastructure" - prevents setting
     * up incorrect infrastructure that only a singleton would
     * require
     */
    if (ORTE_SUCCESS != (rc = orte_init(true))) {
        opal_show_help("help-orterun.txt", "orterun:init-failure", true,
                       "orte_init()", rc);
        return rc;
    }

    
    orte_finalize();
    free(orte_basename);
    return rc;
}
