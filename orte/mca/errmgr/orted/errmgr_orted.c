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

#include "opal/util/output.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/errmgr/base/errmgr_private.h"
#include "orte/mca/errmgr/orted/errmgr_orted.h"

/*
 * This function only gets called on HNP components! Orteds learn about
 * a proc aborting from the HNP.
 */
int orte_errmgr_orted_proc_aborted(orte_gpr_notify_message_t *msg)
{
    return ORTE_ERR_NOT_AVAILABLE;
}

/* This function only gets called on HNP components! Orteds learn about
 * an incomplete start from the HNP.
 */
int orte_errmgr_orted_incomplete_start(orte_gpr_notify_message_t *msg)
{
    return ORTE_ERR_NOT_AVAILABLE;
}

/*
 * This function gets called when the orted itself detects an internal error!
 * At some point in future, to be polite, we tell any of our own local
 * processes to die before we abandon them
 */
void orte_errmgr_orted_error_detected(int error_code, char *fmt, ...)
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
    
    /* cleanup my session directory */
    orte_session_dir_finalize(orte_process_info.my_name);

    /* abnormal exit */
    orte_abort(error_code, false);
}

/*
 * This function gets called when we desperately need to just die.
 * Nothing can be done by definition here - this function ONLY gets
 * called as an absolute last resort
 */
void orte_errmgr_orted_abort(void)
{
    /* cleanup my session directory */
    orte_session_dir_finalize(orte_process_info.my_name);

    /* abnormal exit */
    orte_abort(-1, false);
}

/*
 * This function is called by the orted to request that some set of processes
 * be aborted by the HNP. This would likely be an unusual request as the orted
 * would have no knowledge of other processes or real reason to order them killed.
 * Still, the capability is provided here.
 */
int orte_errmgr_orted_abort_procs_request(orte_process_name_t *procs, orte_std_cntr_t nprocs)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_errmgr_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;
    
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
    if (0 > orte_rml.send_buffer(orte_errmgr_orted_globals.replica, cmd, ORTE_RML_TAG_ERRMGR, 0)) {
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
    if (0 > orte_rml.recv_buffer(orte_errmgr_orted_globals.replica, answer, ORTE_RML_TAG_ERRMGR)) {
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
int orte_errmgr_orted_register_job(orte_jobid_t job)
{
     return ORTE_ERR_NOT_AVAILABLE;
}
