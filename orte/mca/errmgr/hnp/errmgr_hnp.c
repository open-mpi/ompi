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
#include "orte/orte_constants.h"

#include <stdlib.h>
#include <stdarg.h>

#include "opal/class/opal_list.h"
#include "opal/util/trace.h"
#include "opal/util/output.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/params.h"
#include "orte/runtime/orte_wakeup.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/pls/pls.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/schema/schema.h"
#include "orte/dss/dss.h"
#include "orte/mca/rmgr/rmgr.h"

#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/hnp/errmgr_hnp.h"

/*
 * This function gets called when the someone updates a process
 * state to indicate it has aborted. That action results in
 * the firing of a registry trigger that passes a minimal
 * data message here. The only part of that message we need
 * is the segment name so we can extract the jobid from it
 *
 * Various components will follow their own strategy for dealing with
 * this situation. For this component, we simply kill the job.
 */
int orte_errmgr_hnp_proc_aborted(orte_gpr_notify_message_t *msg)
{
    orte_jobid_t job;
    opal_list_t attrs;
    opal_list_item_t *item;
    int rc;
    
    OPAL_TRACE(1);
    
    opal_output(orte_errmgr_base_output, "errmgr:hnp: proc abort has been detected");
    
    /* This trigger is named, so we can extract the jobid
     * directly from the trigger name
     */
    if (ORTE_SUCCESS != (rc = orte_schema.extract_jobid_from_std_trigger_name(&job, msg->target))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* set the job state */
    if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(job, ORTE_JOB_STATE_ABORTED))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* tell the pls to terminate the ENTIRE FAMLIY of this job - this is necessary to avoid
     * "hanging" portions of the application if the aborted job was dynamically spawned
     * from another job
     */
    OBJ_CONSTRUCT(&attrs, opal_list_t);
    orte_rmgr.add_attribute(&attrs, ORTE_NS_USE_JOB_FAMILY, ORTE_UNDEF, NULL, ORTE_RMGR_ATTR_OVERRIDE);
    if (ORTE_SUCCESS != (rc = orte_pls.terminate_job(job, &orte_abort_timeout, &attrs))) {
        ORTE_ERROR_LOG(rc);
    }
    while (NULL != (item = opal_list_remove_first(&attrs))) OBJ_RELEASE(item);
    OBJ_DESTRUCT(&attrs);
    
    /* orterun will only wakeup when all procs IN THE ROOT JOB report terminated. The terminate_job
     * function *should* have done that - however, it is possible during abnormal
     * startup that it will fail to happen. If we get here, we force the issue by
     * deliberately causing the TERMINATE trigger to fire
     */
    if (ORTE_SUCCESS != (rc = orte_wakeup(job))) {
        ORTE_ERROR_LOG(rc);
    }    

    return rc;
}

/*
 * This function gets called when someone updates a process
 * state to indicate it failed to start. That action results in
 * the firing of a registry trigger that passes a minimal
 * data message here. The only part of that message we need
 * is the segment name so we can extract the jobid from it
 *
 * Various components will follow their own strategy for dealing with
 * this situation. For this component, we simply kill the job.
 */
int orte_errmgr_hnp_incomplete_start(orte_gpr_notify_message_t *msg)
{
    orte_jobid_t job;
    opal_list_t attrs;
    opal_list_item_t *item;
    int rc;
    
    OPAL_TRACE(1);
    
    /* This trigger is named, so we can extract the jobid
     * directly from the trigger name
     */
    if (ORTE_SUCCESS != (rc = orte_schema.extract_jobid_from_std_trigger_name(&job, msg->target))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    opal_output(orte_errmgr_base_output, "errmgr_hnp: incomplete start reported - job %lu", (unsigned long)job);
    
    /* set the job state */
    if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(job, ORTE_JOB_STATE_FAILED_TO_START))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* tell the pls to terminate the job - kill this job and all members of its family
     * as we have no way to handle it otherwise at this time
     */
    OBJ_CONSTRUCT(&attrs, opal_list_t);
    orte_rmgr.add_attribute(&attrs, ORTE_NS_USE_JOB_FAMILY, ORTE_UNDEF, NULL, ORTE_RMGR_ATTR_OVERRIDE);
    if (ORTE_SUCCESS != (rc = orte_pls.terminate_job(job, &orte_abort_timeout, &attrs))) {
        ORTE_ERROR_LOG(rc);
    }
    while (NULL != (item = opal_list_remove_first(&attrs))) OBJ_RELEASE(item);
    OBJ_DESTRUCT(&attrs);
    
    /* orterun will only wakeup when all procs IN THE ROOT JOB report terminated. The terminate_job
     * function *should* have done that - however, it is possible during abnormal
     * startup that it will fail to happen. If we get here, we force the issue by
     * deliberately causing the TERMINATE trigger to fire
     */
    if (ORTE_SUCCESS != (rc = orte_wakeup(job))) {
        ORTE_ERROR_LOG(rc);
    }    
    
    return rc;
}

/*
 * This function gets called when the HNP itself detects an internal error!
 * Ideally, we would find some way to tell all the active jobs to die before
 * we depart ourselves. Unfortunately, at this time, we aren't sure we can do
 * this - later, we'll add some more intelligence by, for example, checking
 * the error code to see if it's something that would allow us to alert
 * the remote orteds.
 *
 * For now, we'll just depart!
 */
void orte_errmgr_hnp_error_detected(int error_code, char *fmt, ...)
{
    va_list arglist;
    
    /* If there was a message, output it */
    
    va_start(arglist, fmt);
    if( NULL != fmt ) {
        char* buffer = NULL;
        vasprintf( &buffer, fmt, arglist );
        opal_output( 0, buffer );
        free( buffer );
    }
    va_end(arglist);

    /* abnormal exit */
    orte_abort(error_code, false);
}

/*
 * This function gets called when the HNP desperately needs to just die.
 * Nothing can be done by definition here - this function ONLY gets
 * called as an absolute last resort
 */
void orte_errmgr_hnp_abort(void)
{
    OPAL_TRACE(1);
    
    /* abnormal exit */
    orte_abort(-1, false);
}

/*
 * This function gets called when a process wants to request that the HNP
 * abort some set of processes for it. Since this component IS for the HNP,
 * that means we need to actually execute this request! Call upon the PLS
 * as needed to execute the abort requests
 */
int orte_errmgr_hnp_abort_procs_request(orte_process_name_t *procs, orte_std_cntr_t nprocs)
{
    int rc;
    
    OPAL_TRACE(1);
    
    rc = ORTE_SUCCESS;
    return rc;
}

/*
 * Register the HNP's errmgr functions to be called when the job encounters
 * certain pre-identified problem states.
 *
 * NOTE: It is imperative that ONLY the HNP perform this registration!
 */
int orte_errmgr_hnp_register_job(orte_jobid_t job)
{
    /* we need to setup two counters and their corresponding triggers - one
     * to alert us when something fails to launch, and another for when
     * someone aborts
     */
    int rc;
    
    OPAL_TRACE(1);
    
    /* define the ABORT trigger to fire when any process aborts */
    if (ORTE_SUCCESS != (rc = orte_smr.define_alert_monitor(job, ORTE_NUM_ABORTED_TRIGGER,
                                                            ORTE_PROC_NUM_ABORTED, 0, 1, true,
                                                            orte_errmgr_hnp_proc_aborted, NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* define the FAILED_LAUNCH trigger to fire when the launch fails */
    if (ORTE_SUCCESS != (rc = orte_smr.define_alert_monitor(job, ORTE_FAILED_TO_START_TRIGGER,
                                                            ORTE_PROC_NUM_FAILED_START, 0, 1, true,
                                                            orte_errmgr_hnp_incomplete_start, NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}
