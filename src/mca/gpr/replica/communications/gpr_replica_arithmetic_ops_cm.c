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

#include "dps/dps.h"
#include "mca/errmgr/errmgr.h"

#include "gpr_replica_comm.h"

int orte_gpr_replica_recv_increment_value_cmd(orte_buffer_t *cmd, orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_INCREMENT_VALUE_CMD;
    orte_gpr_value_t *value;
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_itag_t *itags=NULL;
    size_t n;
    int rc, ret;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(cmd, &value, &n, ORTE_GPR_VALUE))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);
    /* find the segment */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, value->segment))) {
        ORTE_ERROR_LOG(rc);
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
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

    if (ORTE_SUCCESS == ret) {
        if (ORTE_SUCCESS != 
            (rc = orte_gpr_replica_check_subscriptions(seg, ORTE_GPR_REPLICA_VALUE_INCREMENTED))) {
            ORTE_ERROR_LOG(rc);
            OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            return rc;
        }
    }

 RETURN_ERROR:
    if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, &ret, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }
    
    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return ret;
}

int orte_gpr_replica_recv_decrement_value_cmd(orte_buffer_t *cmd, orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_DECREMENT_VALUE_CMD;
    orte_gpr_value_t *value;
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_itag_t *itags=NULL;
    size_t n;
    int rc, ret;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(cmd, &value, &n, ORTE_GPR_VALUE))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);
    /* find the segment */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, value->segment))) {
        ORTE_ERROR_LOG(rc);
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        ret = rc;
        goto RETURN_ERROR;
    }

    /* convert tokens to array of itags */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&itags, seg,
                                        value->tokens, &(value->num_tokens)))) {
        ORTE_ERROR_LOG(rc);
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
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

    if (ORTE_SUCCESS == ret) {
        if (ORTE_SUCCESS != 
            (rc = orte_gpr_replica_check_subscriptions(seg, ORTE_GPR_REPLICA_VALUE_DECREMENTED))) {
            ORTE_ERROR_LOG(rc);
            OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            return rc;
        }
    }
    
 RETURN_ERROR:
    if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, &ret, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }
    
    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return ret;
}
