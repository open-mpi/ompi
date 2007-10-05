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

#include "opal/event/event.h"
#include "opal/mca/base/base.h"
#include "opal/util/basename.h"
#include "opal/util/cmd_line.h"
#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/version.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"

#include "orte/dss/dss.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"

static char *ortehalt_basename = NULL;

/*
 * setup globals for catching ortehalt command line options
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

    /* OpenRTE arguments */
    { "orte", "debug", NULL, 'd', NULL, "debug-devel", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE" },
    
    { "universe", NULL, NULL, '\0', NULL, "universe", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Set the universe name as username@hostname:universe_name for this application" },
    
    { NULL, NULL, NULL, '\0', NULL, "tmpdir", 1,
      &orte_process_info.tmpdir_base, OPAL_CMD_LINE_TYPE_STRING,
      "Set the root for the session directory tree for ortehalt ONLY" },

    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

int main(int argc, char *argv[])
{
    orte_buffer_t *cmd;
    orte_daemon_cmd_flag_t command;
    int rc;
    int id, iparam;

    /* Setup MCA params */

    mca_base_param_init();
    orte_register_params(false);

    /* find our basename (the name of the executable) so that we can
       use it in pretty-print error messages */
    ortehalt_basename = opal_basename(argv[0]);

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
                           ortehalt_basename, tmp, "1", rc);
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
    if (ORTE_SUCCESS != (rc = orte_init(ORTE_INFRASTRUCTURE))) {
        opal_show_help("help-ortehalt.txt", "ortehalt:init-failure", true,
                       "orte_init()", rc);
        return rc;
    }

    
    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) {
        opal_show_help("help-ortehalt.txt", "ortehalt:init-failure", true,
                       "orte_init()", rc);
        return ORTE_ERROR;
    }
    
    command = ORTE_DAEMON_HALT_VM_CMD;
    
    rc = orte_dss.pack(cmd, &command, 1, ORTE_DAEMON_CMD);
    if ( ORTE_SUCCESS != rc ) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, cmd, ORTE_RML_TAG_DAEMON, 0);
    if ( 0 > rc ) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    OBJ_RELEASE(cmd);
    
    orte_finalize();
    free(ortehalt_basename);
    return rc;
}

