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

#include "orte/dss/dss.h"
#include "orte/util/proc_info.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmaps/base/rmaps_private.h"

/*
 * Map a job
 */
int orte_rmaps_base_proxy_map_job(orte_jobid_t job, opal_list_t *attributes)
{
    orte_buffer_t* cmd;
    orte_buffer_t* answer;
    orte_rmaps_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;
    
    command = ORTE_RMAPS_MAP_CMD;
    
    cmd = OBJ_NEW(orte_buffer_t);
    if (cmd == NULL) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* pack the command */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &command, 1, ORTE_RMAPS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    /* pack the jobid */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    /* pack the attributes */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(cmd, attributes, 1, ORTE_ATTR_LIST))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }
    
    /* send the request */
    if (0 > orte_rml.send_buffer(ORTE_PROC_MY_HNP, cmd, ORTE_RML_TAG_RMAPS, 0)) {
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
    if (0 > orte_rml.recv_buffer(ORTE_PROC_MY_HNP, answer, ORTE_RML_TAG_RMAPS)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(answer, &command, &count, ORTE_RMAPS_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
    /* check that this is the right command */
    if (ORTE_RMAPS_MAP_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }
    
    /* clean up and leave */
    OBJ_RELEASE(answer);
    return ORTE_SUCCESS;
}
