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
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
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

#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/util/path.h"
#include "opal/util/os_path.h"
#include "opal/class/opal_list.h"
#include "opal/mca/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmgr/rmgr_types.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/runtime/runtime.h"
#include "totalview.h"

/* +++ begin MPICH/TotalView interface definitions */

#define MPIR_DEBUG_SPAWNED  1
#define MPIR_DEBUG_ABORTING 2

struct MPIR_PROCDESC {
    char *host_name;        /* something that can be passed to inet_addr */
    char *executable_name;  /* name of binary */
    int pid;                /* process pid */
};

struct MPIR_PROCDESC *MPIR_proctable = NULL;
int MPIR_proctable_size = 0;
int MPIR_being_debugged = 0;
int MPIR_force_to_main = 0;
volatile int MPIR_debug_state = 0;
volatile int MPIR_i_am_starter = 0;
volatile int MPIR_debug_gate = 0;
volatile int MPIR_acquired_pre_main = 0;

/* --- end MPICH/TotalView interface definitions */

/*
 * NOTE: The job description in the registry will likely evolve to use
 * the "jobgrp_t", but this works for now.
 * 
 * An initial skeleton of how to implement this with jobgrp_t is
 * available in SVN as orte/tools/orterun/totalview.c, version 7075.
 */


#define DUMP_INT(X) fprintf(stderr, "  %s = %d\n", # X, X);

static void dump(void)
{
    int i;

    DUMP_INT(MPIR_being_debugged);
    DUMP_INT(MPIR_debug_gate);
    DUMP_INT(MPIR_debug_state);
    DUMP_INT(MPIR_acquired_pre_main);
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
static int process(char *orig_line, char *basename, int argc, char **argv, 
                   char ***new_argv) 
{
    int i;
    char *line, *full_line = strdup(orig_line);
    char *user_argv, *tmp, **tmp_argv;
    char cwd[PATH_MAX];

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
        }

        if (NULL != tmp) {
            free(full_line);
            full_line = line = tmp;
            --i;
        }
    }

    /* Split up into argv */

    *new_argv = opal_argv_split(line, ' ');
    free(line);

    /* Can we find argv[0] in the path? */

    getcwd(cwd, PATH_MAX);
    tmp = opal_path_findv((*new_argv)[0], X_OK, environ, cwd);
    if (NULL != tmp) {
        free(tmp);
        return ORTE_SUCCESS;
    }

    /* All done -- didn't find it */

    opal_argv_free(*new_argv);
    *new_argv = NULL;
    return ORTE_ERR_NOT_FOUND;
}

/**
 * Run a user-level debugger
 */
void orte_run_debugger(char *basename, int argc, char *argv[])
{
    int i, id;
    char **new_argv = NULL;
    char *value, **lines;

    /* Get the orte_base_debug MCA parameter and search for a debugger
       that can run */
    
    id = mca_base_param_find("orte", NULL, "base_user_debugger");
    if (id < 0) {
        opal_show_help("help-orterun.txt", "debugger-mca-param-not-found", 
                       true);
        exit(1);
    }
    value = NULL;
    mca_base_param_lookup_string(id, &value);
    if (NULL == value) {
        opal_show_help("help-orterun.txt", "debugger-orte_base_user_debugger-empty",
                       true);
        exit(1);
    }

    /* Look through all the values in the MCA param */

    lines = opal_argv_split(value, ':');
    free(value);
    for (i = 0; NULL != lines[i]; ++i) {
        if (ORTE_SUCCESS == process(lines[i], basename, argc, argv, 
                                    &new_argv)) {
            break;
        }
    }

    /* If we didn't find one, abort */

    if (NULL == lines[i]) {
        opal_show_help("help-orterun.txt", "debugger-not-found", true);
        exit(1);
    }
    opal_argv_free(lines);

    /* We found one */

    execvp(new_argv[0], new_argv);
    value = opal_argv_join(new_argv, ' ');
    opal_show_help("help-orterun.txt", "debugger-exec-failed",
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
void orte_totalview_init_before_spawn(void)
{
    if (MPIR_DEBUG_SPAWNED == MPIR_being_debugged) {

        int value;
        char *s;

        if (orte_debug_flag) {
            opal_output(0, "Info: Spawned by a debugger");
        }

        if (mca_base_param_reg_int_name("orte", "mpi_wait_for_totalview",
                                        "Whether the MPI application should wait for a debugger or not",
                                        false, false, (int)false, &value) < 0) {
            opal_output(0, "Error: mca_base_param_reg_int_name\n");
        }

        /* push mca parameter into the environment (not done automatically?) */

        s = mca_base_param_environ_variable("orte", "mpi_wait_for_totalview", NULL);
        if (ORTE_SUCCESS != opal_setenv(s, "1", true, &environ)) {
            opal_output(0, "Error: Can't setenv %s\n", s);
        }
        free(s);
    }
}


/**
 * Initialization of data structures for running under a debugger
 * using the MPICH/TotalView parallel debugger interface. This stage
 * of initialization must occur after stage2 of spawn and is invoked
 * via a callback.
 * 
 * @param jobid  The jobid returned by spawn.
 */
void orte_totalview_init_after_spawn(orte_jobid_t jobid)
{
    orte_job_map_t *map;
    opal_list_item_t *item, *item2;
    orte_mapped_node_t *node;
    orte_mapped_proc_t *proc;
    orte_app_context_t *appctx;
    orte_std_cntr_t i;
    int rc;

    if (MPIR_proctable) {
        /* already initialized */
        return;
    }

    if (0) { /* debugging daemons <<-- needs work */

        if (orte_debug_flag) {
            opal_output(0, "Info: Setting up debugger process table for daemons\n");
        }

    } else {

        /*
         * Debugging applications or not being debugged.
         *
         * Either way, fill in the proc table for the application
         * processes in case someone attaches later.
         */

        if (orte_debug_flag) {
            opal_output(0, "Info: Setting up debugger process table for applications\n");
        }

        MPIR_debug_state = 1;

        /* Get the resource map for this job */

        rc = orte_rmaps.get_job_map(&map, jobid);
        if (ORTE_SUCCESS != rc) {
            opal_output(0, "Error: Can't get resource map\n");
            ORTE_ERROR_LOG(rc);
        }

        /* find the total number of processes in the job */

        for (i=0; i < map->num_apps; i++) {
            MPIR_proctable_size += map->apps[i]->num_procs;
        }

        /* allocate MPIR_proctable */

        MPIR_proctable = (struct MPIR_PROCDESC *) malloc(sizeof(struct MPIR_PROCDESC) *
                                                         MPIR_proctable_size);
        if (MPIR_proctable == NULL) {
            opal_output(0, "Error: Out of memory\n");
            OBJ_RELEASE(map);
        }

        /* initialize MPIR_proctable */

        i=0;
        for (item =  opal_list_get_first(&map->nodes);
             item != opal_list_get_end(&map->nodes);
             item =  opal_list_get_next(item)) {
            node = (orte_mapped_node_t*)item;
            
            for (item2 = opal_list_get_first(&node->procs);
                 item2 != opal_list_get_end(&node->procs);
                 item2 = opal_list_get_next(item2)) {
                proc = (orte_mapped_proc_t*)item2;
                appctx = map->apps[proc->app_idx];
                
                MPIR_proctable[i].host_name = strdup(node->nodename);
                MPIR_proctable[i].executable_name =
                    opal_os_path( false, appctx->cwd, appctx->app, NULL );
                MPIR_proctable[i].pid = proc->pid;
                i++;
            }
        }
        OBJ_RELEASE(map);
    }

    if (orte_debug_flag) {
        dump();
    }

    (void) MPIR_Breakpoint();
}


/**
 * Release resources associated with data structures for running under
 * a debugger using the MPICH/TotalView parallel debugger interface.
 */
void orte_totalview_finalize(void)
{
    if (MPIR_proctable) {
        free(MPIR_proctable);
    }
}

/**
 * Breakpoint function for parallel debuggers
 */
void *MPIR_Breakpoint(void)
{
    return NULL;
}
