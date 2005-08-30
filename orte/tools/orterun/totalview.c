/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
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
#include <stdlib.h>
#include <strings.h>

/*
 * The environment
 */
extern char** environ;


/* +++ begin MPICH/TotalView interface definitions */

#define MPIR_DEBUG_SPAWNED  1
#define MPIR_DEBUG_ABORTING 2

struct MPIR_PROCDESC {
    char *host_name;        /* something that can be passed to inet_addr */
    char *executable_name;  /* name of binary */
    int pid;                /* process pid */
};
typedef struct MPIR_PROCDESC MPIR_PROCDESC;

MPIR_PROCDESC *MPIR_proctable = NULL;
int MPIR_proctable_size = 0;
int MPIR_being_debugged = 0;
int MPIR_force_to_main = 0;
volatile int MPIR_debug_state = 0;
volatile int MPIR_i_am_starter = 0;
volatile int MPIR_debug_gate = 0;
volatile int MPIR_acquired_pre_main = 0;
volatile int debugger_dummy = 0;

void *MPIR_Breakpoint(void);

/* --- end MPICH/TotalView interface definitions */

#include "opal/util/output.h"
#include "opal/class/opal_list.h"
#include "mca/base/base.h"
#include "mca/errmgr/errmgr.h"
#include "mca/rmgr/rmgr_types.h"
#include "mca/rmaps/base/rmaps_base_map.h"
#include "runtime/runtime.h"

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


/**
 * Initialization of data structures for running under a debugger
 * using the MPICH/TotalView parallel debugger interface.  Before the
 * spawn we need to check if we have being run under a TotalView-like
 * debugger; if so then inform applications via an MCA parameter.
 */
void orte_totalview_init_before_spawn(void)
{
    if (MPIR_DEBUG_SPAWNED == MPIR_debug_state) {

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
    opal_list_t list_of_resource_maps;
    opal_list_item_t *item;
    int i;
    int rc;

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

        OBJ_CONSTRUCT(&list_of_resource_maps, opal_list_t);

        /* Get a list of the resource maps for this job */

        rc = orte_rmaps_base_get_map(jobid, &list_of_resource_maps);
        if (ORTE_SUCCESS != rc) {
            opal_output(0, "Error: Can't get list of resource maps\n");
            ORTE_ERROR_LOG(rc);
        }

        /* find the total number of processes in the job */

        for (item =  opal_list_get_first(&list_of_resource_maps);
             item != opal_list_get_end(&list_of_resource_maps);
             item =  opal_list_get_next(item)) {
            orte_rmaps_base_map_t *map = (orte_rmaps_base_map_t*) item;
            MPIR_proctable_size += map->num_procs;
        }

        /* allocate MPIR_proctable */

        MPIR_proctable = (MPIR_PROCDESC *) malloc(sizeof(MPIR_PROCDESC) *
                                                  MPIR_proctable_size);
        if (MPIR_proctable == NULL) {
            opal_output(0, "Error: Out of memory\n");
            OBJ_DESTRUCT(&list_of_resource_maps);
        }

        /* initialize MPIR_proctable */

        for (item =  opal_list_get_first(&list_of_resource_maps);
             item != opal_list_get_end(&list_of_resource_maps);
             item =  opal_list_get_next(item)) {
            orte_rmaps_base_map_t *map = (orte_rmaps_base_map_t*) item;
            for (i = 0; i < map->num_procs; i++) {
                orte_rmaps_base_proc_t *proc = map->procs[i];
                MPIR_proctable[i].host_name = proc->proc_node->node_name;
                MPIR_proctable[i].executable_name = proc->app;
                MPIR_proctable[i].pid = proc->local_pid;
            }
        }

        OBJ_DESTRUCT(&list_of_resource_maps);

    }

    if (orte_debug_flag) {
        dump();
    }

    (void) MPIR_Breakpoint();

    return ORTE_SUCCESS;
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
