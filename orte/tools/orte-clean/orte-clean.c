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
#include "orte/constants.h"

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
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif  /* HAVE_PWD_H */

#include "opal/util/cmd_line.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_dirpath.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"
#include "opal/util/os_path.h"
#include "orte/util/session_dir.h"

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
     * Clean out all session directories - we don't have to protect
     * our own session directory because (since we are a tool) we
     * didn't create one!
     */
    if (orte_clean_globals.verbose) {
        fprintf(stderr, "orte-clean: cleaning session dir tree %s\n", 
                orte_process_info.top_session_dir);
    }
    opal_os_dirpath_destroy(orte_process_info.top_session_dir, true, NULL);
    
    /* now kill any lingering procs, if we can */
#if !defined(__WINDOWS__)
    kill_procs();
#endif  /* !defined(__WINDOWS__) */

    orte_finalize();

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

    if (ORTE_SUCCESS != (ret = orte_init(ORTE_TOOL_WITH_NAME))) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

#if !defined(__WINDOWS__)
static char *orte_getline(FILE *fp)
{
    char *ret, *buff;
    char input[1024];
    int i;
    
    ret = fgets(input, 1024, fp);
    if (NULL != ret) {
        /* remove trailing spaces */
        for (i=strlen(input)-2; i > 0; i--) {
            if (input[i] != ' ') {
                input[i+1] = '\0';
                break;
            }
        }
        buff = strdup(input);
        return buff;
    }
    
    return NULL;
}

/*
 * This function makes a call to "ps" to find out the processes that 
 * are running on this node.  It then attempts to kill off any orteds
 * and orteruns that are not related to this job.
 */
static
void kill_procs(void) {
    int ortedpid;
    char *procname;
    char *pidstr;
    char *user;
    int procpid;
    FILE *psfile;
    char *inputline, *tmpline;
    char *this_user;
    int uid;
	struct passwd *pwdent;
    
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
    if (0 == strcmp("unknown", command)) {
        return;
    }

    if (orte_clean_globals.verbose) {
        fprintf(stderr, "orte-clean: killing any lingering procs\n");
    }

    /*
     * Get our parent pid which is the pid of the orted.
     */
    ortedpid = getppid();

	/* get the name of the user */
    uid = getuid();
#ifdef HAVE_GETPWUID
    pwdent = getpwuid(uid);
#else
    pwdent = NULL;
#endif
    if (NULL != pwdent) {
        this_user = strdup(pwdent->pw_name);
    } else {
        if (0 > asprintf(&this_user, "%d", uid)) {
            return;
        }
    }
    
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
    if (NULL == (inputline = orte_getline(psfile))) {
        free(this_user);
        return;
    } 
    free(inputline);  /* dump the header line */
    
    while (NULL != (inputline = orte_getline(psfile))) {
        
        /* the user name is at the end of the line, with a space
         * preceeding it - extract that field
         */
        user = strrchr(inputline, ' ');
        *user = '\0';  /* null terminate the remainder of the line */
        user++;  /* increment to point to the beginning of the user name */
        
        /* if we are not the user, dump this input */
        if (0 != strcmp(user, this_user)) {
            /* not us */
            free(inputline);
            continue;
        }
         /* copy just the first part so we can search
         * from the back of the string
         */
        tmpline = strdup(inputline);
        
        /* parse the truncated line for the procname and pid */
        pidstr = strrchr(tmpline, ' ');
        *pidstr = '\0';  /* NULL terminate the front of the line */
        pidstr++;
        procpid = atoi(pidstr);
        
        /* since we null-terminated inputline at the end of the
         * procname field, we can now search that field to
         * separate out the base command name in case they
         * have a bunch of path stuff at the start
         */
        if (NULL == (procname = strrchr(tmpline, '/'))) {
            procname = tmpline;  /* no path in command name */
        } else {
            procname++;  /* move past the / */
        }
        
        /*
         * Look for any orteds that are not our parent and attempt to
         * kill them.  We currently do not worry whether we are the
         * owner or not.  If we are not, we will just fail to send
         * the signal and that is OK.  This also allows a root process
         * to kill all orteds.
         *
         * NOTE: need to also look for "(orted)" as a non-active
         * proc is sometimes reported that way
         */
        if (0 == strncmp("orted", procname, strlen("orted")) ||
            0 == strncmp("(orted)", procname, strlen("(orted)"))) {
            if (procpid != ortedpid) {
                if (orte_clean_globals.verbose) {
                    fprintf(stderr, "orte-clean: found potential rogue orted process"
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
        if (0 == strncmp("orterun", procname, strlen("orterun")) ||
            0 == strncmp("mpirun", procname, strlen("mpirun"))) {
            /* if we are on the same node as the HNP, then the ortedpid
             * is the same as that of our orterun, so don't kill it
             */
            if (procpid != ortedpid) {
                if (orte_clean_globals.verbose) {
                    fprintf(stderr, "orte-clean: found potential rogue orterun process"
                            " (pid=%d,user=%s), sending SIGKILL...\n", 
                            procpid, user);
                    
                }
                /* if we are a singleton, check the hnp_pid as well */
                if (orte_process_info.singleton) {
                    if (procpid != orte_process_info.hnp_pid) {
                        (void)kill(procpid, SIGKILL);
                    }
                } else {
                    /* We ignore the return code here as we do not really
                     * care whether this worked or not.
                     */
                    (void)kill(procpid, SIGKILL);
                }
            }
        }
        free(inputline);
        free(tmpline);
    }
    free(this_user);
    return;
}
#endif  /* !defined(__WINDOWS__) */
