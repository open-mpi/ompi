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
/** @file:
 *
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/util/proc_info.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/errmgr/errmgr.h"

#include "gpr_replica_api.h"

int orte_gpr_replica_put(orte_std_cntr_t cnt, orte_gpr_value_t **values)
{
    int rc = ORTE_SUCCESS;
    orte_std_cntr_t i, j;
    orte_gpr_value_t *val;
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_itag_t *itags=NULL;

    OPAL_TRACE(1);

    /* protect ourselves against errors */
    if (NULL == values) {
            return ORTE_ERROR;
    }

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    for (i=0; i < cnt; i++) {
        val = values[i];

        /* first check for error - all keyvals must have a non-NULL string key */
        for (j=0; j < val->cnt; j++) {
            if (NULL == (val->keyvals[j])->key) {
                ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
                OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
                return ORTE_ERR_BAD_PARAM;
            }
        }

        /* find the segment */
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, val->segment))) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            return rc;
        }

        /* convert tokens to array of itags */
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&itags, seg,
                                            val->tokens, &(val->num_tokens)))) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
            return rc;
        }

        if (ORTE_SUCCESS != (rc = orte_gpr_replica_put_fn(val->addr_mode, seg, itags, val->num_tokens,
                    val->cnt, val->keyvals))) {
            goto CLEANUP;
        }

        if (ORTE_SUCCESS != (rc = orte_gpr_replica_check_events())) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }

        if (NULL != itags) {
           free(itags);
        }
        itags = NULL;
    }

CLEANUP:
    /* release list of itags */
    if (NULL != itags) {
       free(itags);
    }

    if (ORTE_SUCCESS == rc) {
        rc = orte_gpr_replica_process_callbacks();
    }

    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;

}


int orte_gpr_replica_put_nb(orte_std_cntr_t cnt, orte_gpr_value_t **values,
                            orte_gpr_notify_cb_fn_t cbfunc, void *user_tag)
{
    OPAL_TRACE(1);

    return ORTE_ERR_NOT_IMPLEMENTED;
}


int orte_gpr_replica_get(orte_gpr_addr_mode_t addr_mode,
                         char *segment, char **tokens, char **keys,
                         orte_std_cntr_t *cnt, orte_gpr_value_t ***values)
{
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_itag_t *tokentags=NULL, *keytags=NULL;
    orte_std_cntr_t num_tokens=0, num_keys=0;
    int rc;

    OPAL_TRACE(1);

    *cnt = 0;
    *values = NULL;

    /* protect against errors */
    if (NULL == segment) {
       return ORTE_ERR_BAD_PARAM;
    }

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    /* find the segment */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, segment))) {
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
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

    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
    return rc;

}


int orte_gpr_replica_get_conditional(orte_gpr_addr_mode_t addr_mode,
                         char *segment, char **tokens, char **keys,
                         orte_std_cntr_t num_conditions, orte_gpr_keyval_t **conditions,
                         orte_std_cntr_t *cnt, orte_gpr_value_t ***values)
{
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_itag_t *tokentags=NULL, *keytags=NULL;
    orte_gpr_replica_itagval_t **conds=NULL;
    orte_std_cntr_t num_tokens=0, num_keys=0, i;
    int rc;

    OPAL_TRACE(1);

    *cnt = 0;
    *values = NULL;

    /* protect against errors */
    if (NULL == segment) {
       return ORTE_ERR_BAD_PARAM;
    }

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    /* find the segment */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, segment))) {
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
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

    /* convert conditions to itagvals */
    conds = (orte_gpr_replica_itagval_t**)malloc(num_conditions*sizeof(orte_gpr_replica_itagval_t*));
    memset(conds, 0, num_conditions*sizeof(orte_gpr_replica_itagval_t*)); /* init the space */

    if (NULL == conds) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    for (i=0; i < num_conditions; i++) {
        conds[i] = OBJ_NEW(orte_gpr_replica_itagval_t);
        if (NULL == conds[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_create_itag(&(conds[i]->itag), seg, conditions[i]->key))) {
            goto CLEANUP;
        }
        conds[i]->value = OBJ_NEW(orte_data_value_t);
        if (NULL == conds[i]->value) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }
        conds[i]->value->type = conditions[i]->value->type;
        if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&((conds[i]->value)->data), conditions[i]->value->data, conds[i]->value->type))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_conditional_fn(addr_mode, seg,
                                            tokentags, num_tokens, keytags, num_keys,
                                            num_conditions, conds,
                                            cnt, values))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
    }

CLEANUP:
    if (NULL != tokentags) {
       free(tokentags);
    }

    if (NULL != keytags) {
      free(keytags);
    }

    for (i=0; i < num_conditions; i++) {
        if (NULL != conds[i]) OBJ_RELEASE(conds[i]);
    }
    if (NULL != conds) free(conds);

    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
    return rc;

}


int orte_gpr_replica_get_nb(orte_gpr_addr_mode_t addr_mode,
                                char *segment, char **tokens, char **keys,
                                orte_gpr_notify_cb_fn_t cbfunc, void *user_tag)
{
    OPAL_TRACE(1);

    return ORTE_ERR_NOT_IMPLEMENTED;
}
