/* -*- C -*-
 *
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
 * The Open MPI General Purpose Registry - Replica component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/replica/communications/gpr_replica_comm.h"

int orte_gpr_replica_recv_increment_value_cmd(orte_buffer_t *cmd, orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_INCREMENT_VALUE_CMD;
    orte_gpr_value_t *value;
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_itag_t *itags=NULL;
    orte_std_cntr_t n;
    int rc, ret;

    OPAL_TRACE(3);
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(cmd, &value, &n, ORTE_GPR_VALUE))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

    /* find the segment */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, value->segment))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

    /* convert tokens to array of itags */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&itags, seg,
                                        value->tokens, &(value->num_tokens)))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }
    
    if (ORTE_SUCCESS != (ret = orte_gpr_replica_increment_value_fn(value->addr_mode, seg,
                                itags, value->num_tokens, value->cnt, value->keyvals))) {
        ORTE_ERROR_LOG(ret);
    }
    
    /* release list of itags */
    if (NULL != itags) {
      free(itags);
    }

    /* release value object */
    OBJ_RELEASE(value);
    
    if (ORTE_SUCCESS == ret) {
        if (ORTE_SUCCESS != 
            (rc = orte_gpr_replica_check_events())) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

 RETURN_ERROR:
    if (ORTE_SUCCESS != (rc = orte_dss.pack(answer, &ret, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ret;
}

int orte_gpr_replica_recv_decrement_value_cmd(orte_buffer_t *cmd, orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_DECREMENT_VALUE_CMD;
    orte_gpr_value_t *value;
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_itag_t *itags=NULL;
    orte_std_cntr_t n;
    int rc, ret;

    OPAL_TRACE(3);
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(cmd, &value, &n, ORTE_GPR_VALUE))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

    /* find the segment */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, value->segment))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

    /* convert tokens to array of itags */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&itags, seg,
                                        value->tokens, &(value->num_tokens)))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }
    
    if (ORTE_SUCCESS != (ret = orte_gpr_replica_decrement_value_fn(value->addr_mode, seg,
                                itags, value->num_tokens, value->cnt, value->keyvals))) {
        ORTE_ERROR_LOG(ret);
    }
    
    /* release list of itags */
    if (NULL != itags) {
      free(itags);
    }

    /* release value object */
    OBJ_RELEASE(value);
    
    if (ORTE_SUCCESS == ret) {
        if (ORTE_SUCCESS != 
            (rc = orte_gpr_replica_check_events())) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
 RETURN_ERROR:
    if (ORTE_SUCCESS != (rc = orte_dss.pack(answer, &ret, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ret;
}
