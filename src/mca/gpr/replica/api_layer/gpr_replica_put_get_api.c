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

#include "util/output.h"
#include "util/proc_info.h"
#include "mca/ns/ns_types.h"
#include "mca/errmgr/errmgr.h"

#include "gpr_replica_api.h"

int orte_gpr_replica_put(int cnt, orte_gpr_value_t **values)
{
    int rc, i, j;
    int8_t action_taken;
    orte_gpr_value_t *val;
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_itag_t *itags=NULL;

    /* protect ourselves against errors */
    if (NULL == values) {
        	if (orte_gpr_replica_globals.debug) {
        	    ompi_output(0, "[%d,%d,%d] gpr replica: error in input - put rejected",
                                ORTE_NAME_ARGS(orte_process_info.my_name));
        	}
        	return ORTE_ERROR;
    }

    if (orte_gpr_replica_globals.compound_cmd_mode) {
	   return orte_gpr_base_pack_put(orte_gpr_replica_globals.compound_cmd, cnt, values);
    }

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    for (i=0; i < cnt; i++) {
        val = values[i];
        
        /* first check for error - all keyvals must have a non-NULL string key */
        for (j=0; j < val->cnt; j++) {
            if (NULL == (val->keyvals[j])->key) {
                ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
                return ORTE_ERR_BAD_PARAM;
            }
        }
        
        /* find the segment */
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, val->segment))) {
            OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            return rc;
        }
    
        /* convert tokens to array of itags */
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&itags, seg,
                                            val->tokens, &(val->num_tokens)))) {
            OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            return rc;
        }
    
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_put_fn(val->addr_mode, seg, itags, val->num_tokens,
    				val->cnt, val->keyvals, &action_taken))) {
            goto CLEANUP;
        }
    
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_check_subscriptions(seg, action_taken))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }

        free(itags);
        itags = NULL;
    }

CLEANUP:
    /* release list of itags */
    if (NULL != itags) {
	   free(itags);
    }

    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

  
    if (ORTE_SUCCESS == rc) {
        return orte_gpr_replica_process_callbacks();
    }


    return rc;

}


int orte_gpr_replica_put_nb(int cnt, orte_gpr_value_t **values,
                            orte_gpr_notify_cb_fn_t cbfunc, void *user_tag)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


int orte_gpr_replica_get(orte_gpr_addr_mode_t addr_mode,
                         char *segment, char **tokens, char **keys,
                         int *cnt, orte_gpr_value_t ***values)
{
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_itag_t *tokentags=NULL, *keytags=NULL;
    int num_tokens=0, num_keys=0, rc;

    *cnt = 0;
    *values = NULL;
    
    /* protect against errors */
    if (NULL == segment) {
	   return ORTE_ERR_BAD_PARAM;
    }

    if (orte_gpr_replica_globals.compound_cmd_mode) {
	   return orte_gpr_base_pack_get(orte_gpr_replica_globals.compound_cmd,
                                        addr_mode, segment, tokens, keys);
    }

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    /* find the segment */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, segment))) {
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    /* convert tokens to array of itags */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&tokentags, seg,
                                                        tokens, &num_tokens))) {
        goto CLEANUP;
    }

    /* convert keys to array of itags */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&keytags, seg,
                                                        keys, &num_keys))) {
        goto CLEANUP;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_fn(addr_mode, seg,
                                            tokentags, num_tokens,
                                            keytags, num_keys,
                                            cnt, values))) {
        goto CLEANUP;
    }
    
CLEANUP:
    if (NULL != tokentags) {
	   free(tokentags);
    }

    if (NULL != keytags) {
      free(keytags);
    }

    OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
    return rc;

}


int orte_gpr_replica_get_nb(orte_gpr_addr_mode_t addr_mode,
                                char *segment, char **tokens, char **keys,
                                orte_gpr_notify_cb_fn_t cbfunc, void *user_tag)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}
