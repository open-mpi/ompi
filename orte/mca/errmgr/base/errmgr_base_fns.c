/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#include "orte/constants.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#include <stdarg.h>

#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_locks.h"

#include "opal/util/trace.h"
#include "opal/util/output.h"
#include "orte/util/name_fns.h"
#include "orte/util/session_dir.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/odls/odls.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"

/*
 * Local Function Declaration
 */
static int orte_errmgr_base_stabalize_runtime(orte_job_t *jdata,
                                              orte_process_name_t *proc,
                                              orte_proc_state_t state);


/*
 * Public interfaces
 */
void orte_errmgr_base_log(int error_code, char *filename, int line)
{
    OPAL_TRACE(1);
    
    if (ORTE_ERR_SILENT == error_code) {
        /* if the error is silent, say nothing */
        return;
    }
    
    opal_output(0, "%s ORTE_ERROR_LOG: %s in file %s at line %d",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                ORTE_ERROR_NAME(error_code), filename, line);
}

int orte_errmgr_base_proc_aborted(orte_process_name_t *name, int exit_code)
{
    int rc;
    orte_job_t *jdata;
    orte_proc_t *proc;
    int i;
    orte_proc_state_t state = ORTE_PROC_STATE_ABORTED;
    int stack_state = ORTE_ERRMGR_STACK_STATE_NONE;
    orte_errmgr_base_module_t *module = NULL;

    if( ORTE_PROC_IS_APP ) {
        return ORTE_SUCCESS;
    }

    stack_state  = ORTE_ERRMGR_STACK_STATE_NONE;
    stack_state |= ORTE_ERRMGR_STACK_STATE_JOB_ABORT;

    /********************************
     * Stabalize the runtime
     ********************************/
    if( !orte_errmgr_base_shutting_down ) {
        OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                             "errmgr:base:proc_aborted() %s) "
                             "------- %s fault reported! Process %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (name->jobid == ORTE_PROC_MY_HNP->jobid ? "Daemon" : "App. Process"),
                             ORTE_NAME_PRINT(name)));
    }

    /* get the job data object for this process */
    if (NULL == (jdata = orte_get_job_data_object(name->jobid))) {
        /* nothing we can do - abort things */
        goto PROCESS;
    }
    
    /* if the proc was terminated by cmd, ignore it */
    if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, name->vpid))) {
        /* nothing we can do */
        goto PROCESS;
    }

    if( !orte_errmgr_base_shutting_down ) {
        OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                             "errmgr:base:proc_aborted() %s) "
                             "------- %s fault reported! Process %s, state (0x%x)",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (name->jobid == ORTE_PROC_MY_HNP->jobid ? "Daemon" : "App. Process"),
                             ORTE_NAME_PRINT(name),
                             proc->state ));
    }

    if (ORTE_PROC_STATE_KILLED_BY_CMD == proc->state) {
        /* don't do anything or else we can enter an infinite loop */
        return ORTE_SUCCESS;
    }

    if( ORTE_SUCCESS != orte_errmgr_base_stabalize_runtime(jdata, name, state)) {
        goto PROCESS;
    }

    /********************************
     * Call the active modules
     ********************************/
    if( orte_errmgr_base_enable_recovery && !orte_errmgr_base_shutting_down) {
        OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                             "errmgr:base:proc_aborted() %s) "
                             "------- Attempting recovery... (%3d active components)",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             orte_errmgr_base_modules.size));

        stack_state |= ORTE_ERRMGR_STACK_STATE_STABLIZED;
        for(i = 0; i < orte_errmgr_base_modules.size; ++i) {
            module = (orte_errmgr_base_module_t*)opal_pointer_array_get_item(&orte_errmgr_base_modules, i);
            if( NULL == module ) {
                continue;
            }
            if( NULL != module->internal_process_fault ) {
                module->internal_process_fault(jdata, name, state, &stack_state);
            }
        }
    }

    /********************************
     * If the active modules still need us to abort, then do so
     ********************************/
    if( !(ORTE_ERRMGR_STACK_STATE_JOB_ABORT & (stack_state)) ) {
        OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                             "errmgr:base:proc_aborted() %s) "
                             "------- Successfully recovered from process %s fault! Continuing...",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(name)));
        return ORTE_SUCCESS;
    }

 PROCESS:
    if( !orte_errmgr_base_shutting_down ) {
        OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                             "errmgr:base:proc_aborted() %s) "
                             "------- Not able to recover from process %s fault! Aborting...",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(name)));
    }

    /* if we are already in progress, then ignore this call */
    if (!opal_atomic_trylock(&orte_abort_inprogress_lock)) { /* returns 1 if already locked */
        OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_output,
                             "%s errmgr:base: abort in progress, ignoring proc %s aborted with status %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(name), exit_code));
        
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_output,
                         "%s errmgr:base: proc %s aborted with status %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(name), exit_code));
    
    orte_job_term_ordered = true;
    
    /* if the proc is a daemon, then we are abnormally terminating */
    if (ORTE_PROC_MY_NAME->jobid == name->jobid) {
        orte_abnormal_term_ordered = true;
    }
    
    /* indicate that all jobs other than the one containing this
     * proc have been ordered to abort - this is necessary to avoid
     * duplicate ordering of "abort".
     *
     * NOTE: be sure to not include the 0 job data location as this
     * contains the daemons!
     */
    for (i=1; i < orte_job_data->size; i++) {
        /* the array may have holes in it as we are recovering
         * jobids as they complete, so check everything
         */
        if (NULL == (jdata = orte_get_job_data_object(name->jobid))) {
            continue;
        }
        if (ORTE_JOB_STATE_ABORTED != jdata->state &&
            ORTE_JOB_STATE_ABORTED_BY_SIG != jdata->state &&
            ORTE_JOB_STATE_ABORTED_WO_SYNC != jdata->state) {
            jdata->state = ORTE_JOB_STATE_ABORT_ORDERED;
        }
    }

    /* tell the plm to terminate all jobs */
    if (ORTE_SUCCESS != (rc = orte_plm.terminate_job(ORTE_JOBID_WILDCARD))) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* set the exit status, just in case whomever called us failed
     * to do so - it can only be done once, so we are protected
     * from overwriting it
     */
    ORTE_UPDATE_EXIT_STATUS(exit_code);
    
    /* just return - let the daemons report back so we can properly
     * know when to actually exit
     */

    return ORTE_SUCCESS;
}

int orte_errmgr_base_incomplete_start(orte_jobid_t job, int exit_code)
{
    int rc;
    orte_job_t *jdata;
    orte_proc_state_t state = ORTE_PROC_STATE_FAILED_TO_START;
    int stack_state = ORTE_ERRMGR_STACK_STATE_NONE;

    if( ORTE_PROC_IS_APP ) {
        return ORTE_SUCCESS;
    }

    stack_state  = ORTE_ERRMGR_STACK_STATE_NONE;
    stack_state |= ORTE_ERRMGR_STACK_STATE_JOB_ABORT;

    /********************************
     * Stabalize the runtime
     ********************************/
    OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                         "errmgr:base:incomplete_start() %s) "
                         "------- Incomplete start of job %s!",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job)));

    /* get the job data object for this process */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        /* nothing we can do - abort things */
        goto PROCESS;
    }

    if( ORTE_SUCCESS != orte_errmgr_base_stabalize_runtime(jdata, NULL, state)) {
        goto PROCESS;
    }

    /********************************
     * Call the active modules
     * JJH: Currently, if we cannot launch the job, then we should just abort.
     * JJH: Add job launch recovery logic...
     ********************************/
#if 0
    if( orte_errmgr_base_enable_recovery ) {
        OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                             "errmgr:base:incomplete_start() %s) "
                             "------- Attempting recovery... (%3d active components)",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             orte_errmgr_base_modules.size));
        stack_state |= ORTE_ERRMGR_STACK_STATE_STABLIZED;
        for(i = 0; i < orte_errmgr_base_modules.size; ++i) {
            module = (orte_errmgr_base_module_t*)opal_pointer_array_get_item(&orte_errmgr_base_modules, i);
            if( NULL == module ) {
                continue;
            }
            if( NULL != module->internal_process_fault ) {
                module->internal_process_fault(jdata, NULL, state, &stack_state);
            }
        }
    }
#endif

    /********************************
     * If the active modules still need us to abort, then do so
     ********************************/
    if( !(ORTE_ERRMGR_STACK_STATE_JOB_ABORT & (stack_state)) ) {
        OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                             "errmgr:base:incomplete_start() %s) "
                             "------- Successfully recovered from incomplete start of job %s! Continuing...",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(job) ));
        return ORTE_SUCCESS;
    }

 PROCESS:
    OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                         "errmgr:base:incomplete_start() %s) "
                         "------- Not able to recover from incomplete start of job %s! Aborting...",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job) ));

    /* if we are already in progress, then ignore this call */
    if (!opal_atomic_trylock(&orte_abort_inprogress_lock)) { /* returns 1 if already locked */
        OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_output,
                             "%s errmgr:base: abort in progress, ignoring incomplete start on job %s with status %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(job), exit_code));
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_output,
                         "%s errmgr:base: job %s reported incomplete start with status %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job), exit_code));

    orte_job_term_ordered = true;
    
    /* tell the plm to terminate all jobs */
    if (ORTE_SUCCESS != (rc = orte_plm.terminate_job(ORTE_JOBID_WILDCARD))) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* set the exit status, just in case whomever called us failed
     * to do so - it can only be done once, so we are protected
     * from overwriting it
     */
    ORTE_UPDATE_EXIT_STATUS(exit_code);
    
    /* just return - let the daemons report back so we can properly
     * know when to actually exit
     */

    return ORTE_SUCCESS;
}

int orte_errmgr_base_comm_failed(orte_process_name_t *name, int exit_code)
{
    orte_job_t *jdata = NULL;
    orte_proc_state_t state = ORTE_PROC_STATE_COMM_FAILED;
    int stack_state = ORTE_ERRMGR_STACK_STATE_NONE;
    orte_errmgr_base_module_t *module = NULL;
    int i;

    stack_state  = ORTE_ERRMGR_STACK_STATE_NONE;
    stack_state |= ORTE_ERRMGR_STACK_STATE_JOB_ABORT;

    /********************************
     * Stabalize the runtime
     ********************************/
    OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                         "errmgr:base:comm_failed() %s) "
                         "------- Communication to Process %s failed!",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(name) ));

    /* get the job data object for this process */
    if (NULL == (jdata = orte_get_job_data_object(name->jobid))) {
        /* nothing we can do - abort things */
        goto PROCESS;
    }

    if( ORTE_SUCCESS != orte_errmgr_base_stabalize_runtime(jdata, name, state)) {
        goto PROCESS;
    }

    /********************************
     * Call the active modules
     ********************************/
    if( orte_errmgr_base_enable_recovery ) {
        OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                             "errmgr:base:comm_failed() %s) "
                             "------- Attempting recovery... (%3d active components)",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             orte_errmgr_base_modules.size));

        stack_state |= ORTE_ERRMGR_STACK_STATE_STABLIZED;
        for(i = 0; i < orte_errmgr_base_modules.size; ++i) {
            module = (orte_errmgr_base_module_t*)opal_pointer_array_get_item(&orte_errmgr_base_modules, i);
            if( NULL == module ) {
                continue;
            }
            if( NULL != module->internal_process_fault ) {
                module->internal_process_fault(jdata, name, state, &stack_state);
            }
        }
    }

    /********************************
     * If the active modules still need us to abort, then do so
     ********************************/
    if( !(ORTE_ERRMGR_STACK_STATE_JOB_ABORT & (stack_state)) ) {
        OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                             "errmgr:base:comm_failed() %s) "
                             "------- Successfully recovered from communication fault with process %s! Continuing...",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(name) ));
        return ORTE_SUCCESS;
    }

 PROCESS:
    OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                         "errmgr:base:comm_failed() %s) "
                         "------- Not able to recover from communication fault with process %s! Aborting...",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(name) ));

    /*
     * Default action is to abort
     */
    ORTE_UPDATE_EXIT_STATUS(exit_code);
    orte_abnormal_term_ordered = true;
    orte_trigger_event(&orte_exit);

    return ORTE_SUCCESS;
}

int orte_errmgr_base_abort(int error_code, char *fmt, ...)
{
    va_list arglist;
    
    /* If there was a message, output it */
    va_start(arglist, fmt);
    if( NULL != fmt ) {
        char* buffer = NULL;
        vasprintf( &buffer, fmt, arglist );
        opal_output( 0, "%s", buffer );
        free( buffer );
    }
    va_end(arglist);
    
    /* if I am a daemon or the HNP... */
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
        /* whack my local procs */
        orte_odls.kill_local_procs(NULL);
        /* whack any session directories */
        orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);
    } else {
        /* cleanup my session directory */
        orte_session_dir_finalize(ORTE_PROC_MY_NAME);
    }
    
    /* abnormal exit */
    orte_ess.abort(error_code, false);

    return ORTE_SUCCESS;
}

int orte_errmgr_base_predicted_fault(char ***proc_list,
                                     char ***node_list,
                                     char ***suggested_nodes)
{
    orte_errmgr_base_module_t *module = NULL;
    int i;

    /*
     * If the user did not ask for recovery, then do not process recovery events
     */
    if( !orte_errmgr_base_enable_recovery ) {
        OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                             "errmgr:base:predicted_fault() %s) "
                             "------- Recovery currently disabled! Skipping...",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) ));
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                         "errmgr:base:predicted_fault() %s) "
                         "------- Notifying components... (%3d active components)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         orte_errmgr_base_modules.size));

    for(i = 0; i < orte_errmgr_base_modules.size; ++i) {
        module = (orte_errmgr_base_module_t*)opal_pointer_array_get_item(&orte_errmgr_base_modules, i);
        if( NULL == module ) {
            continue;
        }
        if( NULL != module->internal_predicted_fault ) {
            module->internal_predicted_fault(proc_list, node_list, suggested_nodes);
        }
    }

    return ORTE_SUCCESS;
}

int orte_errmgr_base_suggest_map_targets(orte_proc_t *proc,
                                         orte_node_t *oldnode,
                                         opal_list_t *node_list)
{
    orte_errmgr_base_module_t *module = NULL;
    int i;

    /*
     * If the user did not ask for recovery, then do not process recovery events
     */
    if( !orte_errmgr_base_enable_recovery ) {
        OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                             "errmgr:base:suggest_map_targets() %s) "
                             "------- Recovery currently disabled! Skipping...",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) ));
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                         "errmgr:base:suggest_map_targets() %s) "
                         "------- Notifying components... (%3d active components)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         orte_errmgr_base_modules.size));

    for(i = 0; i < orte_errmgr_base_modules.size; ++i) {
        module = (orte_errmgr_base_module_t*)opal_pointer_array_get_item(&orte_errmgr_base_modules, i);
        if( NULL == module ) {
            continue;
        }
        if( NULL != module->internal_suggest_map_targets ) {
            module->internal_suggest_map_targets(proc, oldnode, node_list);
        }
    }

    return ORTE_SUCCESS;
}

int orte_errmgr_base_ft_event(int state)
{
    orte_errmgr_base_module_t *module = NULL;
    int i;

    OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                         "errmgr:base:ft_event() %s) "
                         "------- Notifying components... (%3d active components)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         orte_errmgr_base_modules.size));

    for(i = 0; i < orte_errmgr_base_modules.size; ++i) {
        module = (orte_errmgr_base_module_t*)opal_pointer_array_get_item(&orte_errmgr_base_modules, i);
        if( NULL == module ) {
            continue;
        }
        if( NULL != module->internal_ft_event ) {
            module->internal_ft_event(state);
        }
    }

    return ORTE_SUCCESS;
}

/*
 * Local functions
 */
static int orte_errmgr_base_stabalize_runtime(orte_job_t *jdata,
                                              orte_process_name_t *proc,
                                              orte_proc_state_t state)
{
    orte_proc_t *loc_proc=NULL, *child_proc;
    orte_std_cntr_t i_proc;
    int32_t i;

    /*
     * orterun is trying to shutdown, so just let it
     */
    if( orte_errmgr_base_shutting_down ) {
        return ORTE_SUCCESS;
    }

    /*
     * orte_errmgr_base_incomplete_start() will pass a NULL since all processes
     * are effected by this fault.
     * JJH: Since we do not handle the recovery from such errors yet, just
     *      skip processing, and go to the abort sequence.
     */
    if( NULL == proc ) {
        return ORTE_SUCCESS;
    }

    /*
     * Set the process state in the job data structure
     */
    for(i = 0; i < jdata->procs->size; ++i) {
        if (NULL == (loc_proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
            continue;
        }

        if( loc_proc->name.vpid != proc->vpid) {
            continue;
        }

        loc_proc->state = state;

        break;
    }
    
    /*
     * RHC: Since we do not handle the recovery from such errors yet, just
     *      skip processing, and go to the abort sequence.
     */
    if (NULL == loc_proc) {
        return ORTE_SUCCESS;
    }
    
    /*
     * If this is a part of the control plane (HNP/orted)
     */
    if( proc->jobid == ORTE_PROC_MY_NAME->jobid ) {
        /*
         * Remove the route to this process
         */
        orte_routed.delete_route(proc);

        /*
         * If the aborted daemon had active processes on its node, then we should
         * make sure to signal that all the children are gone.
         */
        if( loc_proc->node->num_procs > 0 ) {
            OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                                 "errmgr:base:stabalize_runtime() %s) "
                                 "------- Daemon lost with the following processes",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

            for(i_proc = 0; i_proc < opal_pointer_array_get_size(loc_proc->node->procs); ++i_proc) {
                child_proc = (orte_proc_t*)opal_pointer_array_get_item(loc_proc->node->procs, i_proc);
                if( NULL == child_proc ) {
                    continue;
                }

                OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_output,
                                     "errmgr:base:stabalize_runtime() %s) "
                                     "\t %s [0x%x]",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&child_proc->name),
                                     child_proc->state));

                if( child_proc->last_errmgr_state < child_proc->state ) {
                    child_proc->last_errmgr_state = child_proc->state;
                    orte_errmgr_base_proc_aborted(&child_proc->name, -1);
                }
            }
        }
    }

    return ORTE_SUCCESS;
}
