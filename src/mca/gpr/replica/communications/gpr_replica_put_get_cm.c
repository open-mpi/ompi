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

int orte_gpr_replica_recv_put_cmd(orte_buffer_t *buffer, orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_PUT_CMD;
    orte_gpr_value_t **values = NULL, *val;
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_itag_t *itags=NULL;
    orte_data_type_t type;
    int8_t action_taken=0;
    int i=0, rc, ret;
    size_t cnt;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
            
    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    cnt = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.peek(buffer, &type, &cnt))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

    if (ORTE_GPR_VALUE != type || 0 >= cnt) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        ret = ORTE_ERR_BAD_PARAM;
        goto RETURN_ERROR;
    }
    
    values = (orte_gpr_value_t**)malloc(cnt * sizeof(orte_gpr_value_t*));
    if (NULL == values) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        ret = ORTE_ERR_OUT_OF_RESOURCE;
        goto RETURN_ERROR;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, values, &cnt, ORTE_GPR_VALUE))) {
        ORTE_ERROR_LOG(rc);
        free(values);
        ret = rc;
        goto RETURN_ERROR;
    }

    for (i=0; i < (int)cnt; i++) {
        val = values[i];
        
        /* find the segment */
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, val->segment))) {
            ORTE_ERROR_LOG(rc);
            ret = rc;
            goto RETURN_ERROR;
        }
    
        /* convert tokens to array of itags */
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&itags, seg,
                                            val->tokens, &(val->num_tokens)))) {
            ORTE_ERROR_LOG(rc);
            ret = rc;
            goto RETURN_ERROR;
        }
    
        if (ORTE_SUCCESS != (ret = orte_gpr_replica_put_fn(val->addr_mode, seg, itags,
                    val->num_tokens, val->cnt, val->keyvals, &action_taken))) {
            ORTE_ERROR_LOG(ret);
            goto RETURN_ERROR;
        }
    
        if (ORTE_SUCCESS == ret) {
            if (ORTE_SUCCESS != 
                (rc = orte_gpr_replica_check_subscriptions(seg, action_taken))) {
                ORTE_ERROR_LOG(rc);
                OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
                return rc;
            }
        }

        free(itags);
        itags = NULL;
    }

 RETURN_ERROR:
    /* release list of itags */
    if (NULL != itags) {
        free(itags);
    }

    /* release values */
    if (NULL != values) {
        for (i=0; i < (int)cnt; i++) {
            if (NULL != values[i]) {
                OBJ_RELEASE(values[i]);
            }
        }
        free(values);
    }
    
    if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, &ret, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }
    
    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
    return ret;
}

int orte_gpr_replica_recv_get_cmd(orte_buffer_t *input_buffer,
                                  orte_buffer_t *output_buffer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_GET_CMD;
    orte_gpr_addr_mode_t addr_mode;
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_itag_t *tokentags=NULL, *keytags=NULL;
    int num_tokens=0, num_keys=0, rc, ret;
    char *segment=NULL, **tokens=NULL, **keys=NULL;
    int i=0, cnt=0;
    size_t n;
    orte_gpr_value_t **values=NULL;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(output_buffer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
            
    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dps.unpack(input_buffer, &addr_mode, &n, ORTE_GPR_ADDR_MODE))) {
        ORTE_ERROR_LOG(ret);
        goto RETURN_ERROR;
    }

    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dps.unpack(input_buffer, &segment, &n, ORTE_STRING))) {
        ORTE_ERROR_LOG(ret);
        goto RETURN_ERROR;
    }

    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dps.unpack(input_buffer, &num_tokens, &n, ORTE_INT))) {
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
        if (ORTE_SUCCESS != (ret = orte_dps.unpack(input_buffer, tokens, &n, ORTE_STRING))) {
            ORTE_ERROR_LOG(ret);
            free(tokens);
            goto RETURN_ERROR;
        }
    } else {  /* no tokens provided */
        tokens = NULL;
    }

    n = 1;
    if (ORTE_SUCCESS != (ret = orte_dps.unpack(input_buffer, &num_keys, &n, ORTE_INT))) {
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
        if (ORTE_SUCCESS != (ret = orte_dps.unpack(input_buffer, keys, &n, ORTE_STRING))) {
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

    /* get the answer */
    if (ORTE_SUCCESS != (ret = orte_gpr_replica_get_fn(addr_mode, seg,
                                            tokentags, num_tokens,
                                            keytags, num_keys,
                                            &cnt, &values))) {
        ORTE_ERROR_LOG(ret);
        goto RETURN_ERROR;
    }

 RETURN_ERROR:

    /* pack the number of values */
    if (ORTE_SUCCESS != (rc = orte_dps.pack(output_buffer, &cnt, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
    }

    /* pack the answer into the output output_buffer */
    if (0 < cnt) {
        if (ORTE_SUCCESS != (rc = orte_dps.pack(output_buffer, values, cnt, ORTE_GPR_VALUE))) {
            ORTE_ERROR_LOG(rc);
            ret = rc;
        }
    }
        
    if (NULL != segment) {
        free(segment);
    }
    
    if (NULL != tokens) {
        for (i=0; i<num_tokens; i++) {
            free(tokens[i]);
        }
        free(tokens);
    }

    if (NULL != keys) {
        for (i=0; i<num_keys; i++) {
            free(keys[i]);
        }
        free(keys);
    }

    if (NULL != tokentags) {
        free(tokentags);
    }
    
    if (NULL != keytags) {
        free(keytags);
    }
    
    /* pack response code */
    if (ORTE_SUCCESS != (rc = orte_dps.pack(output_buffer, &ret, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
    }
    
    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return ret;
}

