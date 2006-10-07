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

#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/runtime/runtime.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/errmgr/base/errmgr_private.h"
#include "orte/mca/errmgr/bproc/errmgr_bproc.h"

/*
 * This function gets called when the SMR updates a process state to
 * indicate that it aborted. Since the bproc component is only active on
 * non-HNP processes, this function will NEVER be called
 */
int orte_errmgr_bproc_proc_aborted(orte_gpr_notify_message_t *msg)
{
    OPAL_TRACE(1);
    
    return ORTE_ERR_NOT_AVAILABLE;
}

/*
 * This function gets called when the SMR updates a process state to
 * indicate that it failed to start. Since the bproc component is only active on
 * non-HNP processes, this function will NEVER be called
 */
int orte_errmgr_bproc_incomplete_start(orte_gpr_notify_message_t *msg)
{
    OPAL_TRACE(1);
    
    return ORTE_ERR_NOT_AVAILABLE;
}

/*
 * This function gets called when a process detects an internal error.
 * Bproc is unusually bad about letting us pass information that we
 * aborted as opposed to normally terminated. There is no way to locally
 * monitor the process state on a remote node, so the only thing we
 * can do is pass the info back to the Bproc PLS on the HNP and let it
 * figure out what to do.
 */
void orte_errmgr_bproc_error_detected(int error_code, char *fmt, ...)
{
    va_list arglist;
    orte_buffer_t* cmd;
    uint8_t command;
    int rc;
        
    OPAL_TRACE(1);
    
    /* If there was a message, output it */
    va_start(arglist, fmt);
    if( NULL != fmt ) {
        char* buffer = NULL;
        vasprintf( &buffer, fmt, arglist );
        opal_output( 0, buffer );
        free( buffer );
    }
    va_end(arglist);
    
    /* Now prepare and send a message to the BProc PLS so it knows that
     * we abnormally terminated. It doesn't matter what is in the
     * message - the fact that it gets received is adequate
     */
    command = 0x01;
    
    cmd = OBJ_NEW(orte_buffer_t);
    if (cmd == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return;
    }
    
    /* just pack something */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_UINT8))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return;
    }
    
    /* send the alert */
    if (0 > orte_rml.send_buffer(orte_errmgr_bproc_globals.replica, cmd, ORTE_RML_TAG_BPROC_ABORT, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
        return;
    }
    OBJ_RELEASE(cmd);
    
    /* okay, now we can truly abort. Tell the abort function not to bother writing out
     * an abort file - we can't do anything with it anyway!
     */
    orte_abort(error_code, false);
}

/*
 * This function gets called when a process desperately needs to just die.
 * Nothing can be done by definition here - this function ONLY gets
 * called as an absolute last resort.
 */
void orte_errmgr_bproc_abort()
{
    /* abnormal exit - no point in writing out an abort file as bproc doesn't
     * know what to do with it anyway
     */
    orte_abort(-1, false);
}

/*
 * Alternatively, some systems (e.g., OpenMPI) need to tell us to kill
 * some other subset of processes along with us. Send that info to the
 * HNP so it can kill them.
 *
 * NOTE: this function assumes that the underlying ORTE infrastructure is
 * still operational. Use of this function should therefore be restricted
 * to cases where the problem is in a higher layer (e.g., MPI) as the
 * process is likely to "hang" if an ORTE problem has been encountered.
 */ 
int orte_errmgr_bproc_abort_procs_request(orte_process_name_t *procs, orte_std_cntr_t nprocs)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_errmgr_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;
    
    OPAL_TRACE(1);
    
    /* protect us against error */
    if (NULL == procs) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    command = ORTE_ERRMGR_ABORT_PROCS_REQUEST_CMD;
    
    cmd = OBJ_NEW(orte_buffer_t);
    if (cmd == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* pack the command */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_ERRMGR_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    /* pack the number of procs we are requesting be aborted */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &nprocs, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    /* pack the array of proc names */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, procs, nprocs, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    /* send the request */
    if (0 > orte_rml.send_buffer(orte_errmgr_bproc_globals.replica, cmd, ORTE_RML_TAG_ERRMGR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);
    
    /* setup a buffer for the answer */
    answer = OBJ_NEW(orte_buffer_t);
    if(answer == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* enter a blocking receive until we hear back */
    if (0 > orte_rml.recv_buffer(orte_errmgr_bproc_globals.replica, answer, ORTE_RML_TAG_ERRMGR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &command, &count, ORTE_ERRMGR_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
    /* check that this is the right command */
    if (ORTE_ERRMGR_ABORT_PROCS_REQUEST_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    /* clean up and leave */
    OBJ_RELEASE(answer);
    return ORTE_SUCCESS;
}

/*
 * It is imperative that ONLY an HNP perform this registration!
 */
int orte_errmgr_bproc_register_job(orte_jobid_t job)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_errmgr_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;
    
    OPAL_TRACE(1);
    
    command = ORTE_ERRMGR_REGISTER_JOB_CMD;
    
    cmd = OBJ_NEW(orte_buffer_t);
    if (cmd == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* pack the command */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_ERRMGR_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    /* pack the jobid we are requesting be monitored */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    /* send the request */
    if (0 > orte_rml.send_buffer(orte_errmgr_bproc_globals.replica, cmd, ORTE_RML_TAG_ERRMGR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);
    
    /* setup a buffer for the answer */
    answer = OBJ_NEW(orte_buffer_t);
    if(answer == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* enter a blocking receive until we hear back */
    if (0 > orte_rml.recv_buffer(orte_errmgr_bproc_globals.replica, answer, ORTE_RML_TAG_ERRMGR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &command, &count, ORTE_ERRMGR_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
    /* check that this is the right command */
    if (ORTE_ERRMGR_REGISTER_JOB_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    /* clean up and leave */
    OBJ_RELEASE(answer);
    return ORTE_SUCCESS;
}
