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

#include "util/proc_info.h"
#include "mca/ns/ns_types.h"
#include "mca/errmgr/errmgr.h"

#include "gpr_replica_api.h"


int orte_gpr_replica_increment_value(orte_gpr_value_t *value)
{
    int rc;
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_itag_t *itags=NULL;

    /* protect ourselves against errors */
    if (NULL == value) {
          ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
          return ORTE_ERR_BAD_PARAM;
    }

    if (orte_gpr_replica_globals.compound_cmd_mode) {
       if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_increment_value(
                            orte_gpr_replica_globals.compound_cmd, value))) {
           ORTE_ERROR_LOG(rc);
       }
       return rc;
    }

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    /* find the segment */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, value->segment))) {
        ORTE_ERROR_LOG(rc);
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    /* convert tokens to array of itags */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&itags, seg,
                                        value->tokens, &(value->num_tokens)))) {
        ORTE_ERROR_LOG(rc);
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_increment_value_fn(value->addr_mode, seg,
                                itags, value->num_tokens, value->cnt, value->keyvals))) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* release list of itags */
    if (NULL != itags) {
      free(itags);
    }

    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    if (ORTE_SUCCESS == rc) {
        if (ORTE_SUCCESS != 
            (rc = orte_gpr_replica_check_subscriptions(seg, ORTE_GPR_REPLICA_VALUE_INCREMENTED))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        orte_gpr_replica_process_callbacks();
    }
    
    return rc;
}

int orte_gpr_replica_decrement_value(orte_gpr_value_t *value)
{
    int rc;
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_itag_t *itags=NULL;

    /* protect ourselves against errors */
    if (NULL == value) {
          ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
          return ORTE_ERR_BAD_PARAM;
    }

    if (orte_gpr_replica_globals.compound_cmd_mode) {
       if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_decrement_value(
                            orte_gpr_replica_globals.compound_cmd, value))) {
           ORTE_ERROR_LOG(rc);
       }
       return rc;
    }

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    /* find the segment */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, value->segment))) {
        ORTE_ERROR_LOG(rc);
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    /* convert tokens to array of itags */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&itags, seg,
                                        value->tokens, &(value->num_tokens)))) {
        ORTE_ERROR_LOG(rc);
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_decrement_value_fn(value->addr_mode, seg,
                                itags, value->num_tokens, value->cnt, value->keyvals))) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* release list of itags */
    if (NULL != itags) {
      free(itags);
    }

    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    if (ORTE_SUCCESS == rc) {
        if (ORTE_SUCCESS != 
            (rc = orte_gpr_replica_check_subscriptions(seg, ORTE_GPR_REPLICA_VALUE_DECREMENTED))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        orte_gpr_replica_process_callbacks();
    }
    
    return rc;
}
