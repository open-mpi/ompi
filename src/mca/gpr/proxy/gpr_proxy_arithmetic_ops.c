/* -*- C -*-
 *
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
 * The Open MPI General Purpose Registry - Replica component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "include/orte_constants.h"
#include "dps/dps_types.h"
#include "util/output.h"
#include "util/proc_info.h"

#include "mca/errmgr/errmgr.h"
#include "mca/ns/ns_types.h"
#include "mca/oob/oob_types.h"
#include "mca/rml/rml.h"

#include "gpr_proxy.h"


int orte_gpr_proxy_increment_value(orte_gpr_value_t *value)
{
    orte_buffer_t *cmd, *answer;
    int rc, ret;
    
    if (orte_gpr_proxy_globals.debug) {
	    ompi_output(0, "[%d,%d,%d] gpr_proxy_increment_value entered", 
                    ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    if (orte_gpr_proxy_globals.compound_cmd_mode) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_increment_value(
                                    orte_gpr_proxy_globals.compound_cmd,
				                   value))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_increment_value(cmd, value))) {
	    OBJ_RELEASE(cmd);
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, cmd, ORTE_RML_TAG_GPR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
	    return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);
    
    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (0 > orte_rml.recv_buffer(orte_process_info.gpr_replica, answer, ORTE_RML_TAG_GPR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr_base_unpack_increment_value(answer, &ret))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
    OBJ_RELEASE(answer);
    
    return ret;
}

int orte_gpr_proxy_decrement_value(orte_gpr_value_t *value)
{
    orte_buffer_t *cmd, *answer;
    int rc, ret;
    
    if (orte_gpr_proxy_globals.debug) {
        ompi_output(0, "[%d,%d,%d] gpr_proxy_decrement_value entered", 
                    ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    if (orte_gpr_proxy_globals.compound_cmd_mode) {
        if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_decrement_value(
                                    orte_gpr_proxy_globals.compound_cmd,
                                 value))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_decrement_value(cmd, value))) {
      OBJ_RELEASE(cmd);
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, cmd, ORTE_RML_TAG_GPR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
       return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_RELEASE(cmd);
    
    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (0 > orte_rml.recv_buffer(orte_process_info.gpr_replica, answer, ORTE_RML_TAG_GPR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr_base_unpack_decrement_value(answer, &ret))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
    OBJ_RELEASE(answer);
    
    return ret;
}
