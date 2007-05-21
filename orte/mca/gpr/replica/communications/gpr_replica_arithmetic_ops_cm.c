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

int orte_gpr_replica_recv_arith_op_cmd(orte_buffer_t *input_buffer, orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_ARITH_CMD;
    orte_std_cntr_t n, num_tokens, num_keys;
    orte_gpr_addr_mode_t addr_mode;
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_itag_t *tokentags=NULL, *keytags=NULL;
    orte_dss_arith_op_t op_flag;
    orte_data_value_t *operand;
    int rc, ret;
    char *segment=NULL, **tokens=NULL, **keys=NULL;
    
    OPAL_TRACE(3);
    
    if (ORTE_SUCCESS != (rc = orte_dss.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(input_buffer, &addr_mode, &n, ORTE_GPR_ADDR_MODE))) {
        ORTE_ERROR_LOG(ret);
        goto RETURN_ERROR;
    }
    
    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(input_buffer, &segment, &n, ORTE_STRING))) {
        ORTE_ERROR_LOG(ret);
        goto RETURN_ERROR;
    }
    
    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(input_buffer, &num_tokens, &n, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(ret);
        goto RETURN_ERROR;
    }
    
    if (0 < num_tokens) {  /* tokens provided - get them */
        tokens = (char**)malloc(num_tokens*sizeof(char*));
        if (NULL == tokens) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            ret = ORTE_ERR_OUT_OF_RESOURCE;
            goto RETURN_ERROR;
        }
        n = num_tokens;
        if (ORTE_SUCCESS != (ret = orte_dss.unpack(input_buffer, tokens, &n, ORTE_STRING))) {
            ORTE_ERROR_LOG(ret);
            free(tokens);
            goto RETURN_ERROR;
        }
    } else {  /* no tokens provided */
        tokens = NULL;
    }

    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(input_buffer, &num_keys, &n, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(ret);
        goto RETURN_ERROR;
    }

    if (0 < num_keys) {  /* keys provided - get them */
    keys = (char**)malloc(num_keys*sizeof(char*));
    if (NULL == keys) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        ret = ORTE_ERR_OUT_OF_RESOURCE;
        goto RETURN_ERROR;
    }
    n = num_keys;
    if (ORTE_SUCCESS != (ret = orte_dss.unpack(input_buffer, keys, &n, ORTE_STRING))) {
        ORTE_ERROR_LOG(ret);
        goto RETURN_ERROR;
    }
    } else {  /* no keys provided */
    keys = NULL;
    }

    /* find the segment */
    if (ORTE_SUCCESS != (ret = orte_gpr_replica_find_seg(&seg, true, segment))) {
        ORTE_ERROR_LOG(ret);
        goto RETURN_ERROR;
    }

    /* convert tokens to array of itags */
    if (ORTE_SUCCESS != (ret = orte_gpr_replica_get_itag_list(&tokentags, seg,
                                                              tokens, &num_tokens))) {
        ORTE_ERROR_LOG(ret);
        goto RETURN_ERROR;
    }

    /* convert keys to array of itags */
    if (ORTE_SUCCESS != (ret = orte_gpr_replica_get_itag_list(&keytags, seg,
                                                              keys, &num_keys))) {
        ORTE_ERROR_LOG(ret);
        goto RETURN_ERROR;
    }

    /** unpack the operation flag */
    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(input_buffer, &op_flag, &n, ORTE_ARITH_OP))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }
    
    /** unpack the operand */
    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(input_buffer, &operand, &n, ORTE_DATA_VALUE))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

   if (ORTE_SUCCESS != (rc = orte_gpr_replica_arith_op_fn(addr_mode, seg, tokentags, num_tokens,
                                                          num_keys, keytags, op_flag, operand))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }
    
RETURN_ERROR:
    /** cleanup memory */
    OBJ_RELEASE(operand);
    if (NULL != tokentags) free(tokentags);
    if (NULL != keytags) free(keytags);
    
    /** pack the resulting status to return to caller */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(answer, &ret, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
    }
    
    if (ORTE_SUCCESS == ret) {
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_check_events())) {
            ORTE_ERROR_LOG(rc);
        }
    }
    
    return ret;
}

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
