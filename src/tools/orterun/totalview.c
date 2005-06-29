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
#include "class/ompi_list.h"
#include "mca/errmgr/errmgr.h"
#include "mca/schema/schema.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"


/*
 * Local objects for use in getting daemon and app process data
 */

/*
 * Per process data object
 */
typedef struct {
    ompi_list_item_t *item;
    char *nodename;
    char *IP_address;
    char *executable;
    pid_t pid;
} orte_totalview_proc_data_t;

/* constructor */
static void orte_totalview_proc_data_construct(orte_totalview_proc_data_t* ptr)
{
    ptr->nodename = NULL;
    ptr->IP_address = NULL;
    ptr->executable = NULL;
}

/* destructor */
static void orte_totalview_proc_data_destructor(orte_totalview_proc_data* ptr)
{
    if (NULL != ptr->nodename) {
        free(ptr->nodename);
    }

    if (NULL != ptr->IP_address) {
        free(ptr->IP_address);
    }

    if (NULL != ptr->executable) {
        free(ptr->executable);
    }

}

/* define instance of orte_gpr_replica_segment_t */
OBJ_CLASS_INSTANCE(
          orte_totalview_proc_data_t,  /* type name */
          ompi_list_item_t, /* parent "class" name */
          orte_totalview_proc_data_construct, /* constructor */
          orte_totalview_proc_data_destructor); /* destructor */


/*
 * Local functions for retrieving daemon and app process info
 */
static int orte_totalview_get_job_info(size_t *num_procs,
                                ompi_list_t *ptr,
                                orte_jobid_t jobid,
                                orte_jobgrp_t job_grp);

static int orte_totalview_get_daemon_info(size_t *num_procs,
                                ompi_list_t *ptr,
                                orte_jobgrp_t job_grp);

static int orte_totalview_get_app_info(size_t *num_procs,
                                ompi_list_t *ptr,
                                orte_jobgrp_t job_grp);

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
void orte_totalview_init(orte_jobgrp_t job_grp)
{
    ompi_list_t proc_info;
    
    OBJ_CONSTRUCT(&proc_info, ompi_list_t);
    
    /* 
     * fill in proc table
     */

    if (RunParams.dbg.Spawned && RunParams.dbg.WaitInDaemon) {

        /*
         * Debugging daemons
         */

        int h, rc;
        size_t num_daemons;

        ompi_output_verbose(10, 0, "Info: Setting up debugger "
                            "process table for daemons\n");

        /* get the daemon process data */
        if (ORTE_SUCCESS != (rc = orte_totalview_get_daemon_info(&num_daemons, &proc_info, job_grp))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        
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
        size_t num_procs;
        
        /*
         * Debugging applications or not being debugged.
         *
         * Either way, fill in the proc table for the application
         * processes in case someone attaches later.
         */

        ompi_output_verbose(10, 0, "Info: Setting up debugger "
                            "process table for applications\n");

        /* get the application process data */
        if (ORTE_SUCCESS != (rc = orte_totalview_get_app_info(&num_procs, &proc_info, job_grp))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
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

    /* cleanup */
    OBJ_DESTRUCT(&proc_info);
    
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


/**
 * Get daemon info required for Totalview
 */
int orte_totalview_get_daemon_info(size_t *num_procs,
                                ompi_list_t *ptr,
                                orte_jobgrp_t job_grp)
{
    /* the daemons for a job group are ALWAYS located on jobid zero
     * within that group. Therefore, we can create the appropriate
     * registry requests using that knowledge
     */
    if (ORTE_SUCCESS != (rc = orte_totalview_get_job_info(num_procs, ptr, job_grp, 0))) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

static int orte_totalview_get_job_info(size_t *num_procs,
                                ompi_list_t *ptr,
                                orte_jobid_t jobid,
                                orte_jobgrp_t job_grp)
{
    size_t i, j, k, cnt;
    orte_totalview_proc_data_t *pdat;
    char *segment=NULL, *save_exec=NULL;
    char *keys[] = {
        ORTE_JOB_SLOTS_KEY,
        ORTE_NODE_NAME_KEY,
        ORTE_JOB_APP_CONTEXT_KEY,
        ORTE_PROC_LOCAL_PID_KEY,
        ORTE_PROC_APP_CONTEXT_KEY,
        ORTE_PROC_RML_IP_ADDRESS_KEY,
        NULL
    };
    orte_gpr_value_t **values, *value;
    orte_gpr_keyval_t *keyval;
    int rc;
    
    *num_procs = 0;
    
    /* get the segment name */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, job_grp, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* query all proc entries. We specify no tokens here since we want
     * all the process entries off of the segments.
     */
    if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
                                           segment, NULL, keys,
                                           &cnt, &values))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* parse the response */
    for(i=0; i < cnt; i++) {
        value = values[i];
        pdat = OBJ_NEW(orte_totalview_proc_data_t);
        if (NULL == pdat) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            for (j=i; j < cnt; j++) OBJ_RELEASE(values[j]);
            goto CLEANUP;
        }
        for(k=0; k<value->cnt; k++) {
            keyval = value->keyvals[k];
            /* these values come from the ORTE_JOB_GLOBALS
             * container
             */
            if(strcmp(keyval->key, ORTE_JOB_SLOTS_KEY) == 0) {
                *num_procs = keyval->value.size;
                continue;
            }
            if(strcmp(keyval->key, ORTE_JOB_APP_CONTEXT_KEY) == 0) {
                save_exec = strdup((keyval->value.app_context)->app);
                continue;
            }
            /* these values come from the containers that
             * each correspond to a specific process
             */
            if(strcmp(keyval->key, ORTE_NODE_NAME_KEY) == 0) {
                pdat->nodename = strdup(keyval->value.strptr);
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_LOCAL_PID_KEY) == 0) {
                pdat->pid = keyval->value.pid;
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_RML_IP_ADDRESS_KEY) == 0) {
                pdat->IP_address = strdup(keyval->value.strptr);
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_APP_CONTEXT_KEY) == 0) {
                pdat->executable = strdup((keyval->value.app_context)->app);
                continue;
            }
        }
        ompi_list_append(ptr, pdat->item);
        
        OBJ_RELEASE(value);
    }
    
    /* we had to get the default executable path from the ORTE_JOB_GLOBALS
     * container. If we didn't, then something is very wrong - abort
     */
    if (NULL == save_exec) {
        ORTE_ERROR_LOG(ORTE_ERR_GPR_DATA_CORRUPT);
        rc = ORTE_ERR_GPR_DATA_CORRUPT;
        goto CLEANUP;
    }
    
    /* check all daemon info to find where the executable path
     * hasn't been entered - i.e., those daemons that used the
     * default path that was stored in the ORTE_JOB_GLOBALS
     * container - and store the default path in them
     */
    for (pdat = (orte_totalview_proc_data_t*)ompi_list_get_first(ptr);
         pdat != (orte_totalview_proc_data_t*)ompi_list_get_end(ptr);
         pdat = (orte_totalview_proc_data_t*)ompi_list_get_next(pdat)) {
         if (NULL == pdat->executable)
            pdat->executable = strdup(save_exec);
    }
    
    /* cleanup */
    if (NULL != values) free(values);
    if (NULL != segment) free(segment);
    if (NULL != save_exec) free(save_exec);
    
    return ORTE_SUCCESS;
}


/**
 * Get application process info required for Totalview
 */
int orte_totalview_get_app_info(size_t *num_procs,
                                ompi_list_t *ptr,
                                orte_jobgrp_t job_grp)
{
    /* need to find all the jobid's that are part of this jobgrp.
     * Only the "0" jobid belongs to the daemons - all other jobids
     * have application processes running on them
     */
    orte_jobid_t *jobids=NULL;
    size_t i, n;
    int rc=ORTE_SUCCESS;
    
    *num_procs = 0;
    
    if (ORTE_SUCCESS != (rc = orte_ns.get_jobgrp_members(&jobids, job_grp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* now get job info for all non-zero jobids, adding up all
     * the procs each call brings back
     */
    for (i=0; i < num_jobids; i++) {
        if (0 != jobids[i]) {
            if (ORTE_SUCCESS != (rc = orte_totalview_get_job_info(&n, ptr, job_grp, jobids[i]))) {
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
            *num_procs = *num_procs + n;
        }
    }

CLEANUP:
    if (NULL != jobids) free(jobids);
    
    return rc;
}

