/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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

#include <stdlib.h>
#include <stdarg.h>

#include "opal/class/opal_list.h"
#include "opal/util/trace.h"
#include "orte/util/show_help.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_locks.h"
#include "orte/mca/plm/plm.h"
#include "orte/util/session_dir.h"
#include "orte/util/name_fns.h"

#include "orte/mca/errmgr/base/errmgr_private.h"
#include "errmgr_default.h"

/*
 * This function gets called by the PLM when an orted notifies us
 * that a process has aborted
 * Various components will follow their own strategy for dealing with
 * this situation. For this component, we simply kill the job.
 */
void orte_errmgr_default_proc_aborted(orte_process_name_t *name, int exit_code)
{
    int rc;
    orte_job_t **jobs;
    orte_std_cntr_t i;
    
    OPAL_TRACE(1);
    
    /* if we are already in progress, then ignore this call */
    if (!opal_atomic_trylock(&orte_abort_inprogress_lock)) { /* returns 1 if already locked */
        OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_output,
                             "%s errmgr:default: abort in progress, ignoring proc %s aborted with status %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(name), exit_code));
        
        return;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_output,
                         "%s errmgr:default: proc %s aborting with status %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(name), exit_code));
    
    orte_job_term_ordered = true;
    
    /* indicate that all jobs other than the one containing this
     * proc have been orted to abort - this is necessary to avoid
     * duplicate ordering of "abort".
     *
     * NOTE: be sure to not include the 0 job data location as this
     * contains the daemons!
     */
    jobs = (orte_job_t**)orte_job_data->addr;
    for (i=1; i < orte_job_data->size; i++) {
        /* the array is left justfied, so we can quit once
         * we see a NULL
         */
        if (NULL == jobs[i]) {
            break;
        }
        if (ORTE_JOB_STATE_ABORTED != jobs[i]->state &&
            ORTE_JOB_STATE_ABORTED_BY_SIG != jobs[i]->state &&
            ORTE_JOB_STATE_ABORTED_WO_SYNC != jobs[i]->state) {
            jobs[i]->state = ORTE_JOB_STATE_ABORT_ORDERED;
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

    /* wakeup orterun so we can exit */
    orte_trigger_event(&orte_exit);    
}

/*
 * This function gets called by the PLM when an orted notifies us that
 * a job failed to start.
 * Various components will follow their own strategy for dealing with
 * this situation. For this component, we simply kill the job.
 */
void orte_errmgr_default_incomplete_start(orte_jobid_t job, int exit_code)
{
    int rc;
    
    OPAL_TRACE(1);
    
    /* if we are already in progress, then ignore this call */
    if (!opal_atomic_trylock(&orte_abort_inprogress_lock)) { /* returns 1 if already locked */
        OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_output,
                             "%s errmgr:default: abort in progress, ignoring incomplete start on job %s with status %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(job), exit_code));
        return;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_output,
                         "%s errmgr:default: job %s reported incomplete start with status %d",
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
    
    /* wakeup orterun so we can exit */
    orte_trigger_event(&orte_exit);   
}

/*
 * Register a callback function upon a change to a specified job state.
 */
int orte_errmgr_default_register_callback(orte_jobid_t job,
                                      orte_job_state_t state,
                                      orte_errmgr_cb_fn_t cbfunc,
                                      void *cbdata)
{
   return ORTE_ERR_NOT_IMPLEMENTED;
}
