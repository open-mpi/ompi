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

int orte_gpr_replica_recv_delete_segment_cmd(orte_buffer_t *buffer, orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_DELETE_SEGMENT_CMD;
    char *segment=NULL;
    orte_gpr_replica_segment_t *seg=NULL;
    size_t n;
    int rc, ret;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, &segment, &n, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, false, segment))) {
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

    if (ORTE_SUCCESS != (ret = orte_gpr_replica_release_segment(&seg))) {
        ORTE_ERROR_LOG(ret);
    }

    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

 RETURN_ERROR:
    if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, &ret, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ret;
}

int orte_gpr_replica_recv_delete_entries_cmd(orte_buffer_t *buffer, orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_DELETE_ENTRIES_CMD;
    orte_gpr_addr_mode_t addr_mode;
    orte_gpr_replica_itag_t *token_itags=NULL, *key_itags=NULL;
    orte_gpr_replica_segment_t *seg=NULL;
    char *segment=NULL, **tokens=NULL, **keys=NULL;
    int num_tokens=0, num_keys=0, rc, i, ret;
    size_t n;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, &addr_mode, &n, ORTE_GPR_ADDR_MODE))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, &segment, &n, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, &num_tokens, &n, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

    if (0 >= num_tokens) {  /* no tokens provided - wildcard case */
        tokens = NULL;
    } else {  /* tokens provided */
        tokens = (char**)malloc(num_tokens*sizeof(char*));
        if (NULL == tokens) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            ret = ORTE_ERR_OUT_OF_RESOURCE;
            goto RETURN_ERROR;
        }
        if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, tokens, (size_t*)&num_tokens, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            ret = rc;
            goto RETURN_ERROR;
        }
     }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, &num_keys, &n, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

    if (0 >= num_keys) {  /* no keys provided - wildcard case */
        keys = NULL;
    } else {  /* keys provided */
        keys = (char**)malloc(num_keys*sizeof(char*));
        if (NULL == keys) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            ret = ORTE_ERR_OUT_OF_RESOURCE;
            goto RETURN_ERROR;
        }
        if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, keys, (size_t*)&num_keys, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            ret = rc;
            goto RETURN_ERROR;
        }
     }

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

     /* locate the segment */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, false, segment))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        goto RETURN_ERROR;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&token_itags, seg, tokens, &num_tokens))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        goto RETURN_ERROR;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&key_itags, seg, keys, &num_keys))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        goto RETURN_ERROR;
    }

    ret = orte_gpr_replica_delete_entries_fn(addr_mode, seg,
                                            token_itags, num_tokens,
                                            key_itags, num_keys);

    if (ORTE_SUCCESS == ret) {
        orte_gpr_replica_check_subscriptions(seg, ORTE_GPR_REPLICA_ENTRY_DELETED);
    }
    
    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);


 RETURN_ERROR:
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

    if (NULL != token_itags) {
        free(token_itags);
    }

    if (NULL != key_itags) {
        free(key_itags);
    }

    if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, &ret, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ret;
}

int orte_gpr_replica_recv_index_cmd(orte_buffer_t *buffer,
                                    orte_buffer_t *answer)
{
    orte_gpr_cmd_flag_t command=ORTE_GPR_INDEX_CMD;
    orte_data_type_t type;
    size_t n, cnt;
    orte_gpr_replica_segment_t *seg=NULL;
    char *segment=NULL, **index=NULL;
    int rc, ret;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, &command, 1, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_dps.peek(buffer, &type, &n))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    if (ORTE_STRING != type) {  /* get index of segment names */
        seg = NULL;
    } else {
        if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, &segment, &n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            ret = rc;
            OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            goto RETURN_ERROR;
        }
        /* locate the segment */
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, false, segment))) {
            ORTE_ERROR_LOG(rc);
            ret = rc;
            OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            goto RETURN_ERROR;
        }
    }

    if (ORTE_SUCCESS != (ret = orte_gpr_replica_index_fn(seg, &cnt, index))) {
        ORTE_ERROR_LOG(ret);
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        goto RETURN_ERROR;
    }

    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, &cnt, 1, ORTE_SIZE))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto RETURN_ERROR;
    }

    if (0 < cnt) {  /* got a non-zero answer back */
        if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, index, cnt, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            ret = rc;
            goto RETURN_ERROR;
        }
    }
        
 RETURN_ERROR:
    if (NULL != segment) {
        free(segment);
    }
    
    if (NULL != index) {
       for (n=0; n < cnt; n++) {
           free(index[(int)n]);
       }
       free(index);
    }
    
    if (ORTE_SUCCESS != (rc = orte_dps.pack(answer, &ret, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ret;
}
