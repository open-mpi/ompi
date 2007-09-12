/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
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
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /*  HAVE_STDLIB_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif  /* HAVE_SYS_STAT_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif  /* HAVE_SYS_WAIT_H */
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif  /* HAVE_SYS_PARAM_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif  /* HAVE_DIRENT_H */
#include <signal.h>

#include "orte/orte_constants.h"

#include "opal/util/cmd_line.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_dirpath.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/univ_info.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "opal/util/os_path.h"
#include "orte/util/session_dir.h"
#include "orte/util/universe_setup_file_io.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rml/rml.h"

#include "opal/runtime/opal.h"
#if OPAL_ENABLE_FT == 1
#include "opal/runtime/opal_cr.h"
#endif
#include "orte/runtime/runtime.h"

/******************
 * Local Functions
 ******************/
static int orte_clean_init(void);
static int parse_args(int argc, char *argv[]);
#if !defined(__WINDOWS__)
static void kill_procs(void);
#endif  /* !defined(__WINDOWS__) */

/*****************************************
 * Global Vars for Command line Arguments
 *****************************************/
typedef struct {
    bool help;
    bool verbose;
} orte_clean_globals_t;

orte_clean_globals_t orte_clean_globals;

opal_cmd_line_init_t cmd_line_opts[] = {
    { NULL, NULL, NULL, 
      'h', NULL, "help", 
      0,
      &orte_clean_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL, NULL, NULL, 
      'v', NULL, "verbose", 
      0,
      &orte_clean_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Generate verbose output" },

    /* End of list */
    { NULL, NULL, NULL, 
      '\0', NULL, NULL, 
      0,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      NULL }
};

/*
 * This utility will do a brute force clean of a node.  It will
 * attempt to clean up any files in the user's session directory.
 * It will also look for any orted and orterun processes that are
 * not part of this job, and kill them off.
*/
int
main(int argc, char *argv[])
{
    int ret, exit_status = ORTE_SUCCESS;

    /***************
     * Initialize
     ***************/
    if (ORTE_SUCCESS != (ret = parse_args(argc, argv))) {
        return ret;
    }
    if (ORTE_SUCCESS != (ret = orte_clean_init())) {
        exit_status = ret;
        goto cleanup;
    }
    /*
     * Clean out all /tmp directories except for our own.
     */
    orte_universe_clean_directories(orte_universe_info.name, orte_clean_globals.verbose);
#if !defined(__WINDOWS__)
    kill_procs();
#endif  /* !defined(__WINDOWS__) */

    orte_finalize();
    opal_finalize();

 cleanup:
    return exit_status;
}
/*
 * Parse the command line arguments using the functions command
 * line utility functions.
 */
static int parse_args(int argc, char *argv[]) {
    int ret;
    opal_cmd_line_t cmd_line;
    orte_clean_globals_t tmp = { false, false };

    /* Parse the command line options */

    /* NOTE: There is a bug in the PGI 6.2 series that causes the
       compiler to choke when copying structs containing bool members
       by value.  So do a memcpy here instead. */
    memcpy(&orte_clean_globals, &tmp, sizeof(tmp));

    /*
     * Initialize list of available command line options.
     */
    opal_cmd_line_create(&cmd_line, cmd_line_opts);
    ret = opal_cmd_line_parse(&cmd_line, true, argc, argv);

    opal_setenv(mca_base_param_env_var("opal_cr_is_tool"),
                "1", true, NULL);

    /**
     * Now start parsing our specific arguments
     */
    if (OPAL_SUCCESS != ret || 
        orte_clean_globals.help) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(&cmd_line);
        opal_show_help("help-orte-clean.txt", "usage", true,
                       args);
        free(args);
        return ORTE_ERROR;
    }

    OBJ_DESTRUCT(&cmd_line);

    return ORTE_SUCCESS;
}

static int orte_clean_init(void) {
    int exit_status = ORTE_SUCCESS, ret;

    /*
     * We are trying to attach to another process' GPR so we need to 
     * attach no matter if it is identified as private or not.
     */
    opal_setenv(mca_base_param_env_var("universe_console"),
                "1", true, NULL);

#if OPAL_ENABLE_FT == 1
    /* Disable the checkpoint notification routine for this
     * tool. As we will never need to checkpoint this tool.
     * Note: This must happen before opal_init().
     */
    opal_cr_set_enabled(false);
    
    /* Select the none component, since we don't actually use a checkpointer */
    opal_setenv(mca_base_param_env_var("crs"),
                "none",
                true, &environ);
#endif

    /***************************
     * We need all of OPAL
     ***************************/
    if (ORTE_SUCCESS != (ret = opal_init())) {
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = orte_system_init(ORTE_INFRASTRUCTURE, ORTE_NON_BARRIER))) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

#if !defined(__WINDOWS__)
/*
 * This function makes a call to "ps" to find out the processes that 
 * are running on this node.  It then attempts to kill off any orteds
 * and orteruns that are not related to this job.
 */
static
void kill_procs(void) {
    int ortedpid;
    char procname[MAXPATHLEN]; /* only really need 8, but being safe */
    char pidstr[MAXPATHLEN];   /* only really need 8, but being safe */
    char user[MAXPATHLEN];
    int procpid;
    FILE *psfile;
    bool kill_orteruns = false;
    int orunpid = 0;

    /*
     * This is the command that is used to get the information about
     * all the processes that are running.  The output looks like the
     * following:
     * COMMAND    PID     USER
     * tcsh     12556    rolfv
     * ps       14424    rolfv
     * etc.
     * Currently, we do not make use of the USER field, but we may later
     * on so we grab it also.
     */

    /*
     * The configure determines if there is a valid ps command for us to
     * use.  If it is set to unknown, then we skip this section.
     */
    char command[] = ORTE_CLEAN_PS_CMD;
    if (!(strcmp("unknown", command))) {
	return;
    }

    /*
     * Try to get the pid of our orterun process from our universe name.
     * This works in the case where one is using the default universe name
     * which appends the pid after the 'default-universe-' string.  In
     * this way, we avoid killing our own mpirun process.  Note that if
     * we cannot determine our orterun pid, then we skip killing the
     * orterun processes to avoid odd behavior for the user.
     */
    if (!(strncmp(ORTE_DEFAULT_UNIVERSE, orte_universe_info.name, 
                  sizeof(ORTE_DEFAULT_UNIVERSE)-1))) {
        char *tptr;

        /* 
         * Set a pointer to the pid part of the name.  The pointer
	 * is adjusted by the name along with one extra to remove the
         * dash before the pid.  Then convert to a pid.  If the strtol()
         * returns zero, then we got an error on the conversion and we
         * will skip killing the orteruns.  
         */
        tptr = orte_universe_info.name + sizeof(ORTE_DEFAULT_UNIVERSE);
	if (0 != (orunpid = (int)strtol(tptr, (char **)NULL, 10))) {
	    kill_orteruns = true;
	}
    }

    /*
     * Get our parent pid which is the pid of the orted.
     */
    ortedpid = getppid();

    /*
     * There is a race condition here.  The problem is that we are looking
     * for any processes named orted.  However, one may erroneously find more
     * orteds then there really are because the orted is doing a series of
     * fork/execs. If we run with more than one orte-clean on a node, then
     * one of the orte-cleans may catch the other one while it has forked,
     * but not exec'ed.  It will therefore kill an orte-clean.  Now one
     * can argue it is silly to run more than one orte-clean on a node, and
     * this is true.  We will have to figure out how to prevent this.  For
     * now, we use a big hammer and just sleep a second to decrease the
     * probability.
     */
    sleep(1);

    psfile = popen(command, "r");
    /*
     * Read the first line of the output.  We just throw it away
     * as it is the header consisting of the words COMMAND, PID and
     * USER.
     */
    if ((fscanf(psfile, "%s%s%s", procname, pidstr, user)) == EOF) {
        return;
    } 
        
    while ((fscanf(psfile, "%s%s%s", procname, pidstr, user)) != EOF) {

        procpid = atoi(pidstr);

        /*
         * Look for any orteds that are not our parent and attempt to
         * kill them.  We currently do not worry whether we are the
         * owner or not.  If we are not, we will just fail to send
         * the signal and that is OK.  This also allows a root process
         * to kill all orteds.
         */
        if (!strcmp("orted", procname)) {
            if (procpid != ortedpid) {
                if (orte_clean_globals.verbose) {
                    opal_output(0, "orte-clean: found potential rogue orted process"
                                " (pid=%d,user=%s), sending SIGKILL...\n", 
                                procpid, user);
                }
                /*
                 * We ignore the return code here as we do not really
                 * care whether this worked or not.
                 */
                (void)kill(procpid, SIGKILL);
            }
        }

        /*
         * Now check for any orteruns.
         */
        if (kill_orteruns) {
            if (!strcmp("orterun", procname)) {
                if (procpid != orunpid) {
                    if (orte_clean_globals.verbose) {
                        opal_output(0, "orte-clean: found potential rogue orterun process"
                                    " (pid=%d,user=%s), sending SIGKILL...\n", 
                                    procpid, user);
                    }
                    /*
                     * We ignore the return code here as we do not really
                     * care whether this worked or not.
                     */
                    (void)kill(procpid, SIGKILL);
                }
            }
        }
    }
    return;
}
#endif  /* !defined(__WINDOWS__) */
