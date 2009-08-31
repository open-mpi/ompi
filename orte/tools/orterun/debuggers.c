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
 * Copyright (c) 2007      Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2007-2009 Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

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
 *	  breakpoint on.
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
 * 2. If not using orterun: in this case, ORTE_DISABLE_FULL_SUPPORT
 * will be true, and we know that there will not be an RML message
 * sent to VPID 0.  So we have to look for a magic environment
 * variable from the launcher to know if the jobs will be attached by
 * a debugger (e.g., set by yod, srun, ...etc.), and if so, spin on
 * MPIR_debug_gate.  These environment variable names must be
 * hard-coded in the OMPI layer (see ompi/debuggers/ompi_debuggers.c).
 */

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /* HAVE_STDLIB_H */
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif  /* HAVE_STRINGS_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <ctype.h>

#include "opal/util/argv.h"
#include "opal/util/path.h"
#include "opal/util/os_path.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/show_help.h"

#include "debuggers.h"

/* +++ begin MPICH/TotalView std debugger interface definitions */

struct MPIR_PROCDESC {
    char *host_name;        /* something that can be passed to inet_addr */
    char *executable_name;  /* name of binary */
    int pid;                /* process pid */
};

struct MPIR_PROCDESC *MPIR_proctable = NULL;
int MPIR_proctable_size = 0;
int MPIR_being_debugged = 0;
volatile int MPIR_debug_state = 0;
volatile int MPIR_i_am_starter = 0;
volatile int MPIR_partial_attach_ok = 1;

/* --- end MPICH/TotalView std debugger interface definitions */


#define DUMP_INT(X) fprintf(stderr, "  %s = %d\n", # X, X);

static void dump(void)
{
    int i;

    DUMP_INT(MPIR_being_debugged);
    DUMP_INT(MPIR_debug_state);
    DUMP_INT(MPIR_partial_attach_ok);
    DUMP_INT(MPIR_i_am_starter);
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
}

/*
 * Process one line from the orte_base_user_debugger MCA param and
 * look for that debugger in the path.  If we find it, fill in
 * new_argv.
 */
static int process(char *orig_line, char *basename, opal_cmd_line_t *cmd_line,
                   int argc, char **argv, char ***new_argv, int num_procs) 
{
    int i;
    char *line, *full_line = strdup(orig_line);
    char *user_argv, *tmp, *tmp2, **tmp_argv, **executable;
    char cwd[OMPI_PATH_MAX];
    bool used_num_procs = false;
    bool single_app = false;
    bool fail_needed_executable = false;

    line = full_line;
    if (NULL == line) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* Trim off whitespace at the beginning and ending of line */

    for (i = 0; '\0' != line[i] && isspace(line[i]); ++line) {
        continue;
    }
    for (i = strlen(line) - 2; i > 0 && isspace(line[i]); ++i) {
        line[i] = '\0';
    }
    if (strlen(line) <= 0) {
        return ORTE_ERROR;
    }

    /* Get the tail of the command line (i.e., the user executable /
       argv) */

    opal_cmd_line_get_tail(cmd_line, &i, &executable);

    /* Remove --debug, --debugger, and -tv from the user command line
       params */

    if (1 == argc) {
        user_argv = strdup("");
    } else {
        tmp_argv = opal_argv_copy(argv);
        for (i = 0; NULL != tmp_argv[i]; ++i) {
            if (0 == strcmp(tmp_argv[i], "-debug") ||
                0 == strcmp(tmp_argv[i], "--debug")) {
                free(tmp_argv[i]);
                tmp_argv[i] = strdup("");
            } else if (0 == strcmp(tmp_argv[i], "-tv") ||
                0 == strcmp(tmp_argv[i], "--tv")) {
                free(tmp_argv[i]);
                tmp_argv[i] = strdup("");
            } else if (0 == strcmp(tmp_argv[i], "--debugger") ||
                       0 == strcmp(tmp_argv[i], "-debugger")) {
                free(tmp_argv[i]);
                tmp_argv[i] = strdup("");
                if (NULL != tmp_argv[i + 1]) {
                    ++i;
                    free(tmp_argv[i]);
                    tmp_argv[i] = strdup("");
                }
            }
        }
        user_argv = opal_argv_join(tmp_argv + 1, ' ');
        opal_argv_free(tmp_argv);
    }

    /* Replace @@ tokens - line should never realistically be bigger
       than MAX_INT, so just cast to int to remove compiler warning */

    for (i = 0; i < (int) strlen(line); ++i) {
        tmp = NULL;
        if (0 == strncmp(line + i, "@mpirun@", 8)) {
            line[i] = '\0';
            asprintf(&tmp, "%s%s%s", line, argv[0], line + i + 8);
        } else if (0 == strncmp(line + i, "@orterun@", 9)) {
            line[i] = '\0';
            asprintf(&tmp, "%s%s%s", line, argv[0], line + i + 9);
        } else if (0 == strncmp(line + i, "@mpirun_args@", 13)) {
            line[i] = '\0';
            asprintf(&tmp, "%s%s%s", line, user_argv, line + i + 13);
        } else if (0 == strncmp(line + i, "@orterun_args@", 14)) {
            line[i] = '\0';
            asprintf(&tmp, "%s%s%s", line, user_argv, line + i + 14);
        } else if (0 == strncmp(line + i, "@np@", 4)) {
            line[i] = '\0';
            asprintf(&tmp, "%s%d%s", line, num_procs,
                     line + i + 4);
            used_num_procs = true;
        } else if (0 == strncmp(line + i, "@single_app@", 12)) {
            line[i] = '\0';
            /* This token is only a flag; it is not replaced with any
               alternate text */
            asprintf(&tmp, "%s%s", line, line + i + 12);
            single_app = true;
        } else if (0 == strncmp(line + i, "@executable@", 12)) {
            line[i] = '\0';
            /* If we found the executable, paste it in.  Otherwise,
               this is a possible error. */
            if (NULL != executable) {
                asprintf(&tmp, "%s%s%s", line, executable[0], line + i + 12);
            } else {
                fail_needed_executable = true;
            }
        } else if (0 == strncmp(line + i, "@executable_argv@", 17)) {
            line[i] = '\0';
            /* If we found the tail, paste in the argv.  Otherwise,
               this is a possible error. */
            if (NULL != executable) {
                if (NULL != executable[1]) {
                    /* Put in the argv */
                    tmp2 = opal_argv_join(executable + 1, ' ');
                    asprintf(&tmp, "%s%s%s", line, tmp2, line + i + 17);
                    free(tmp2);
                } else {
                    /* There is no argv; just paste the front and back
                       together, removing the @token@ */
                    asprintf(&tmp, "%s%s", line, line + i + 17);
                }
            } else {
                fail_needed_executable = true;
            }
        }

        if (NULL != tmp) {
            free(full_line);
            full_line = line = tmp;
            --i;
        }
    }

    /* Split up into argv */

    *new_argv = opal_argv_split(line, ' ');
    free(full_line);

    /* Can we find argv[0] in the path? */

    getcwd(cwd, OMPI_PATH_MAX);
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
            orte_show_help("help-orterun.txt", "debugger requires -np",
                           true, (*new_argv)[0], argv[0], user_argv, 
                           (*new_argv)[0]);
            /* Fall through to free / fail, below */
        } 

        /* Some debuggers do not support launching MPMD */
        else if (single_app && NULL != strchr(tmp, ':')) {
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
            free(tmp);
            return ORTE_SUCCESS;
        }
        free(tmp);
    }

    /* All done -- didn't find it */

    opal_argv_free(*new_argv);
    *new_argv = NULL;
    return ORTE_ERR_NOT_FOUND;
}

/**
 * Run a user-level debugger
 */
void orte_run_debugger(char *basename, opal_cmd_line_t *cmd_line,
                       int argc, char *argv[], int num_procs)
{
    int i, id;
    char **new_argv = NULL;
    char *value, **lines, *env_name;

    /* Get the orte_base_debug MCA parameter and search for a debugger
       that can run */
    
    id = mca_base_param_find("orte", NULL, "base_user_debugger");
    if (id < 0) {
        orte_show_help("help-orterun.txt", "debugger-mca-param-not-found", 
                       true);
        exit(1);
    }
    value = NULL;
    mca_base_param_lookup_string(id, &value);
    if (NULL == value) {
        orte_show_help("help-orterun.txt", "debugger-orte_base_user_debugger-empty",
                       true);
        exit(1);
    }

    /* Look through all the values in the MCA param */

    lines = opal_argv_split(value, ':');
    free(value);
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
    /* Set an MCA param so that everyone knows that they are being
       launched under a debugger; not all debuggers are consistent
       about setting MPIR_being_debugged in both the launcher and the
       MPI processes */
    env_name = mca_base_param_environ_variable("orte", 
                                               "in_parallel_debugger", NULL);
    if (NULL != env_name) {
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


/**
 * Initialization of data structures for running under a debugger
 * using the MPICH/TotalView parallel debugger interface.  Before the
 * spawn we need to check if we are being run under a TotalView-like
 * debugger; if so then inform applications via an MCA parameter.
 */
void orte_debugger_init_before_spawn(orte_job_t *jdata)
{
    char *env_name;
    orte_app_context_t **apps;
    orte_std_cntr_t i;

    if (!MPIR_being_debugged && !orte_in_parallel_debugger) {
        /* not being debugged */
        return;
    }

    if (orte_debug_flag) {
        opal_output(0, "Info: Spawned by a debugger");
    }
    
    /* tell the procs they are being debugged */
    apps = (orte_app_context_t**)jdata->apps->addr;
    env_name = mca_base_param_environ_variable("orte", 
                                               "in_parallel_debugger", NULL);
    
    for (i=0; i < jdata->num_apps; i++) {
        opal_setenv(env_name, "1", true, &apps[i]->env);
    }
    free(env_name);
}


/**
 * Initialization of data structures for running under a debugger
 * using the MPICH/TotalView parallel debugger interface. This stage
 * of initialization must occur after spawn
 * 
 * NOTE: We -always- perform this step to ensure that any debugger
 * that attaches to us post-launch of the application can get a
 * completed proctable
 */
void orte_debugger_init_after_spawn(orte_job_t *jdata)
{
    orte_proc_t **procs;
    orte_app_context_t *appctx, **apps;
    orte_vpid_t i, j;
    opal_buffer_t buf;
    orte_process_name_t rank0;
    int rc;

    if (MPIR_proctable) {
        /* already initialized */
        return;
    }

    /* fill in the proc table for the application processes */
    
    if (orte_debug_flag) {
        opal_output(0, "Info: Setting up debugger process table for applications\n");
    }
    
    MPIR_debug_state = 1;
    
    /* set the total number of processes in the job */
    MPIR_proctable_size = jdata->num_procs;
    
    /* allocate MPIR_proctable */
    MPIR_proctable = (struct MPIR_PROCDESC *) malloc(sizeof(struct MPIR_PROCDESC) *
                                                     MPIR_proctable_size);
    if (MPIR_proctable == NULL) {
        opal_output(0, "Error: Out of memory\n");
        return;
    }
    
    /* initialize MPIR_proctable */
    procs = (orte_proc_t**)jdata->procs->addr;
    apps = (orte_app_context_t**)jdata->apps->addr;
    for (j=0; j < jdata->num_procs; j++) {
        if (NULL == procs[j]) {
            opal_output(0, "Error: undefined proc at position %ld\n", (long)j);
        }
        /* store this data in the location whose index
         * corresponds to the proc's rank
         */
        i = procs[j]->name.vpid;
        appctx = apps[procs[j]->app_idx];
        
        MPIR_proctable[i].host_name = strdup(procs[j]->node->name);
        if ( 0 == strncmp(appctx->app, OPAL_PATH_SEP, 1 )) { 
            MPIR_proctable[i].executable_name = 
            opal_os_path( false, appctx->app, NULL ); 
        } else {
            MPIR_proctable[i].executable_name =
            opal_os_path( false, appctx->cwd, appctx->app, NULL ); 
        } 
        MPIR_proctable[i].pid = procs[j]->pid;
    }

    if (orte_debug_flag) {
        dump();
    }

    /* if we are being launched under a debugger, then we must wait
     * for it to be ready to go and do some things to start the job
     */
    if (MPIR_being_debugged) {
        /* wait for all procs to have reported their contact info - this
         * ensures that (a) they are all into mpi_init, and (b) the system
         * has the contact info to successfully send a message to rank=0
         */
        ORTE_PROGRESSED_WAIT(false, jdata->num_reported, jdata->num_procs);
        
        (void) MPIR_Breakpoint();
        
        /* send a message to rank=0 to release it */
        OBJ_CONSTRUCT(&buf, opal_buffer_t); /* don't need anything in this */
        rank0.jobid = jdata->jobid;
        rank0.vpid = 0;
        if (0 > (rc = orte_rml.send_buffer(&rank0, &buf, ORTE_RML_TAG_DEBUGGER_RELEASE, 0))) {
            opal_output(0, "Error: could not send debugger release to MPI procs - error %s", ORTE_ERROR_NAME(rc));
        }
        OBJ_DESTRUCT(&buf);
    }
}


/**
 * Release resources associated with data structures for running under
 * a debugger using the MPICH/TotalView parallel debugger interface.
 */
void orte_debugger_finalize(void)
{
    if (MPIR_proctable) {
        free(MPIR_proctable);
        MPIR_proctable = NULL;
    }
}

/**
 * Breakpoint function for parallel debuggers
 */
void *MPIR_Breakpoint(void)
{
    return NULL;
}
