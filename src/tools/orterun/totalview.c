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

#include "util/output.h"


/* +++ begin MPICH/TotalView interface definitions */

#define MPIR_DEBUG_SPAWNED  1
#define MPIR_DEBUG_ABORTING 2

struct MPIR_PROCDESC {
    char *host_name;        /* something that can be passed to inet_addr */
    char *executable_name;  /* name of binary */
    int pid;                /* process pid */
};

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

#if OMPI_ENABLE_DEBUG

#define DUMP_INT(X) fprintf(stderr, "  %s = %d\n", # X, X);

static void dump(void)
{
    DUMP_INT(MPIR_being_debugged);
    DUMP_INT(MPIR_debug_gate);
    DUMP_INT(MPIR_debug_state);
    DUMP_INT(MPIR_acquired_pre_main);
    DUMP_INT(MPIR_i_am_starter);
    DUMP_INT(MPIR_proctable_size);
    fprintf(stderr, "MPIR_proctable:\n");
    for (int i = 0; i < MPIR_proctable_size; i++) {
        fprintf(stderr,
                "    (i, host, exe, pid) = (%d, %s, %s, %ld)\n",
                i,
                MPIR_proctable[i].host_name,
                MPIR_proctable[i].executable_name,
                (long) MPIR_proctable[i].pid);
    }
}

#endif

/**
 * Initialization of data structures for running under a debugger
 * using the MPICH/TotalView parallel debugger interface.
 */
void orte_totalview_init(size_t job_grp)
{
    /* 
     * fill in proc table
     */

    if (RunParams.dbg.Spawned && RunParams.dbg.WaitInDaemon) {

        /*
         * Debugging daemons
         */

        int h;

        ompi_output_verbose(10, 0, "Info: Setting up debugger "
                            "process table for daemons\n");

        /* allocate memory for process table */
        MPIR_proctable_size = RunParams.NHosts;
        MPIR_proctable = (MPIR_PROCDESC *) malloc(sizeof(MPIR_PROCDESC) *
                                                  MPIR_proctable_size);
        if (MPIR_proctable == NULL) {
            ompi_output(0, "Error: Out of memory\n");
            exit(1);
        }

        MPIR_being_debugged = 1;

        for (h = 0; h < RunParams.NHosts; h++) {
            MPIR_proctable[h].host_name = RunParams.HostList[h];
            MPIR_proctable[h].executable_name = RunParams.ExeList[h];
            MPIR_proctable[h].pid = server->daemonPIDForHostRank(h);
        }

    } else {
        
        /*
         * Debugging applications or not being debugged.
         *
         * Either way, fill in the proc table for the application
         * processes in case someone attaches later.
         */

        ompi_output_verbose(10, 0, "Info: Setting up debugger "
                            "process table for applications\n");

        /* allocate memory for process table */
        MPIR_proctable_size = RunParams.TotalProcessCount;
        MPIR_proctable = (MPIR_PROCDESC *) malloc(sizeof(MPIR_PROCDESC) *
                                                  MPIR_proctable_size);
        if (MPIR_proctable == NULL) {
            ompi_output(0, "Error: Out of memory\n");
            exit(1);
        }

        int i = 0;
        for (int h = 0; h < RunParams.NHosts; h++) {
            for (int p = 0; p < (RunParams.ProcessCount)[h]; p++) {
                /* fill in application process information */
                MPIR_proctable[i].host_name = RunParams.HostList[h];
                MPIR_proctable[i].executable_name = RunParams.ExeList[h];
                MPIR_proctable[i].pid = RunParams.AppPIDs[h][p];
                i++;
            }
        }

        if (ENABLE_BPROC && RunParams.dbg.GDB) {
            spawn_gdb_in_xterms();
            return;
        }
    }

    if (1 /* verbose */) {
        dump();
    }

    if (RunParams.dbg.Spawned) {
        MPIR_debug_state = MPIR_DEBUG_SPAWNED;
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
