/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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

#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "dps/dps.h"
#include "util/output.h"
#include "util/proc_info.h"

#include "mca/ns/ns_types.h"
#include "mca/oob/oob_types.h"
#include "mca/rml/rml.h"

#include "gpr_proxy.h"


int orte_gpr_proxy_begin_compound_cmd(void)
{
    orte_gpr_cmd_flag_t command;
    int rc;
    
    command = ORTE_GPR_COMPOUND_CMD;
    
    OMPI_THREAD_LOCK(&orte_gpr_proxy_globals.wait_for_compound_mutex);

    if (orte_gpr_proxy_globals.compound_cmd_mode) {
	   orte_gpr_proxy_globals.compound_cmd_waiting++;
	   ompi_condition_wait(&orte_gpr_proxy_globals.compound_cmd_condition, &orte_gpr_proxy_globals.wait_for_compound_mutex);
	   orte_gpr_proxy_globals.compound_cmd_waiting--;
    }

    orte_gpr_proxy_globals.compound_cmd_mode = true;
    if (NULL != orte_gpr_proxy_globals.compound_cmd) {
        OBJ_RELEASE(orte_gpr_proxy_globals.compound_cmd);
    }

    orte_gpr_proxy_globals.compound_cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == orte_gpr_proxy_globals.compound_cmd) {
        orte_gpr_proxy_globals.compound_cmd_mode = false;
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dps.pack(orte_gpr_proxy_globals.compound_cmd, &command,
                                            1, ORTE_GPR_CMD))) {
        orte_gpr_proxy_globals.compound_cmd_mode = false;
        OBJ_RELEASE(orte_gpr_proxy_globals.compound_cmd);
        return rc;
    }
    
    OMPI_THREAD_UNLOCK(&orte_gpr_proxy_globals.wait_for_compound_mutex);
    return ORTE_SUCCESS;
}


int orte_gpr_proxy_stop_compound_cmd(void)
{
    OMPI_THREAD_LOCK(&orte_gpr_proxy_globals.wait_for_compound_mutex);

    orte_gpr_proxy_globals.compound_cmd_mode = false;
    if (NULL != orte_gpr_proxy_globals.compound_cmd) {
	   OBJ_RELEASE(orte_gpr_proxy_globals.compound_cmd);
    }

    if (orte_gpr_proxy_globals.compound_cmd_waiting) {
	   ompi_condition_signal(&orte_gpr_proxy_globals.compound_cmd_condition);
    }

    OMPI_THREAD_UNLOCK(&orte_gpr_proxy_globals.wait_for_compound_mutex);
    return ORTE_SUCCESS;
}


int orte_gpr_proxy_exec_compound_cmd(void)
{
    orte_buffer_t *answer;
    orte_gpr_cmd_flag_t command;
    size_t n;
    int rc;
    int32_t response;
    
    if (orte_gpr_proxy_globals.debug) {
	   ompi_output(0, "[%d,%d,%d] transmitting compound command",
		    ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    OMPI_THREAD_LOCK(&orte_gpr_proxy_globals.wait_for_compound_mutex);
    rc = ORTE_SUCCESS;
    
    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, orte_gpr_proxy_globals.compound_cmd, ORTE_RML_TAG_GPR, 0)) {
        rc = ORTE_ERR_COMM_FAILURE;
	    goto CLEANUP;
    }

    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    
	if (0 > orte_rml.recv_buffer(orte_process_info.gpr_replica, answer, ORTE_RML_TAG_GPR)) {
        OBJ_RELEASE(answer);
        rc = ORTE_ERR_COMM_FAILURE;
	    goto CLEANUP;
	}

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(answer, &command, &n, ORTE_GPR_CMD))) {
        OBJ_RELEASE(answer);
        goto CLEANUP;
    }
    
    if (ORTE_GPR_COMPOUND_CMD != command) {
        OBJ_RELEASE(answer);
        rc = ORTE_ERR_COMM_FAILURE;
        goto CLEANUP;
    }
    
    n = 1;
    rc = orte_dps.unpack(answer, &response, &n, ORTE_INT32);
    
    if (ORTE_SUCCESS == rc) {
        rc = (int)response;
    }
    
 CLEANUP:
    orte_gpr_proxy_globals.compound_cmd_mode = false;
    OBJ_RELEASE(orte_gpr_proxy_globals.compound_cmd);

    if (orte_gpr_proxy_globals.compound_cmd_waiting) {
	   ompi_condition_signal(&orte_gpr_proxy_globals.compound_cmd_condition);
    }

    OMPI_THREAD_UNLOCK(&orte_gpr_proxy_globals.wait_for_compound_mutex);

    return rc;
}


