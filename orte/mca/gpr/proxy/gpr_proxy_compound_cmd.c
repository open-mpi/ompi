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
/** @file:
 *
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/orte_types.h"
#include "orte/dss/dss.h"
#include "opal/util/output.h"
#include "orte/util/proc_info.h"

#include "orte/mca/ns/ns_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/oob/oob_types.h"
#include "orte/mca/rml/rml.h"

#include "gpr_proxy.h"


int orte_gpr_proxy_begin_compound_cmd(void)
{
    orte_gpr_cmd_flag_t command;
    int rc;
    
    command = ORTE_GPR_COMPOUND_CMD;
    
    OPAL_THREAD_LOCK(&orte_gpr_proxy_globals.wait_for_compound_mutex);

    if (orte_gpr_proxy_globals.compound_cmd_mode) {
	   orte_gpr_proxy_globals.compound_cmd_waiting++;
	   opal_condition_wait(&orte_gpr_proxy_globals.compound_cmd_condition, &orte_gpr_proxy_globals.wait_for_compound_mutex);
	   orte_gpr_proxy_globals.compound_cmd_waiting--;
    }

    orte_gpr_proxy_globals.compound_cmd_mode = true;
    if (NULL != orte_gpr_proxy_globals.compound_cmd) {
        OBJ_RELEASE(orte_gpr_proxy_globals.compound_cmd);
    }

    orte_gpr_proxy_globals.compound_cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == orte_gpr_proxy_globals.compound_cmd) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        orte_gpr_proxy_globals.compound_cmd_mode = false;
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(orte_gpr_proxy_globals.compound_cmd, &command,
                                            1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        orte_gpr_proxy_globals.compound_cmd_mode = false;
        OBJ_RELEASE(orte_gpr_proxy_globals.compound_cmd);
        return rc;
    }
    
    OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.wait_for_compound_mutex);
    return ORTE_SUCCESS;
}


int orte_gpr_proxy_stop_compound_cmd(void)
{
    OPAL_THREAD_LOCK(&orte_gpr_proxy_globals.wait_for_compound_mutex);

    orte_gpr_proxy_globals.compound_cmd_mode = false;
    if (NULL != orte_gpr_proxy_globals.compound_cmd) {
	   OBJ_RELEASE(orte_gpr_proxy_globals.compound_cmd);
    }

    if (orte_gpr_proxy_globals.compound_cmd_waiting) {
	   opal_condition_signal(&orte_gpr_proxy_globals.compound_cmd_condition);
    }

    OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.wait_for_compound_mutex);
    return ORTE_SUCCESS;
}


int orte_gpr_proxy_exec_compound_cmd(void)
{
    orte_buffer_t *answer;
    orte_gpr_cmd_flag_t command;
    orte_std_cntr_t n;
    int rc, response;
    
    if (orte_gpr_proxy_globals.debug) {
	   opal_output(0, "[%lu,%lu,%lu] transmitting compound command",
		    ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    OPAL_THREAD_LOCK(&orte_gpr_proxy_globals.wait_for_compound_mutex);
    rc = ORTE_SUCCESS;
    
    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, orte_gpr_proxy_globals.compound_cmd, ORTE_RML_TAG_GPR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        rc = ORTE_ERR_COMM_FAILURE;
	    goto CLEANUP;
    }
    orte_gpr_proxy_globals.compound_cmd_mode = false;
    OBJ_RELEASE(orte_gpr_proxy_globals.compound_cmd);
        
    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    
	if (0 > orte_rml.recv_buffer(orte_process_info.gpr_replica, answer, ORTE_RML_TAG_GPR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        rc = ORTE_ERR_COMM_FAILURE;
	    goto CLEANUP;
	}

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &command, &n, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        goto CLEANUP;
    }
    
    if (ORTE_GPR_COMPOUND_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        rc = ORTE_ERR_COMM_FAILURE;
        goto CLEANUP;
    }
    
    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &response, &n, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
    }
    
    if (ORTE_SUCCESS == rc) {
        rc = (int)response;
    }
    
 CLEANUP:
    if (orte_gpr_proxy_globals.compound_cmd_waiting) {
	   opal_condition_signal(&orte_gpr_proxy_globals.compound_cmd_condition);
    }

    OPAL_THREAD_UNLOCK(&orte_gpr_proxy_globals.wait_for_compound_mutex);

    return rc;
}


