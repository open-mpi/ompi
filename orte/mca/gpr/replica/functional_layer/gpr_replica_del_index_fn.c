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

#include "orte/orte_constants.h"

#include "opal/util/output.h"
#include "orte/util/proc_info.h"
#include "opal/util/trace.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/replica/functional_layer/gpr_replica_fn.h"


int orte_gpr_replica_delete_entries_fn(orte_gpr_addr_mode_t addr_mode,
				     orte_gpr_replica_segment_t *seg,
				     orte_gpr_replica_itag_t *token_itags, orte_std_cntr_t num_tokens,
                      orte_gpr_replica_itag_t *key_itags, orte_std_cntr_t num_keys)
{
    orte_gpr_replica_container_t **cptr;
    orte_gpr_replica_itagval_t  **ivals;
    orte_gpr_replica_addr_mode_t tok_mode;
    orte_std_cntr_t i, j, k, n, p;
    int rc;

    OPAL_TRACE(2);
    
    /* if num_tokens == 0 and num_keys == 0, remove segment. We don't record
     * any actions when doing this so that subscriptions don't fire like mad
     */
    if (0 == num_tokens && 0 == num_keys) {
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_release_segment(&seg))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    
    /* initialize storage for actions taken */
    orte_pointer_array_clear(orte_gpr_replica_globals.acted_upon);
    orte_gpr_replica_globals.num_acted_upon = 0;
    
    /* extract the token address mode */
    tok_mode = ORTE_GPR_REPLICA_TOKMODE(addr_mode);
    if (0x00 == tok_mode) {  /* default tokens addressing mode to AND */
        tok_mode = ORTE_GPR_REPLICA_AND;
    }

    /* find the specified container(s) */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_containers(seg, tok_mode,
                                    token_itags, num_tokens))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (0 == orte_gpr_replica_globals.num_srch_cptr) {
        /* nothing found - no ERROR_LOG entry created as this is
         * not a system failure. Likewise, don't return an error code
         * as this is not necessarily an error - don't want to cause
         * somebody to abort as a result.
         */
        return ORTE_SUCCESS;
    }
    
    /* go through the containers looking for the specified entries,
     * removing those that are found
     */
    cptr = (orte_gpr_replica_container_t**)(orte_gpr_replica_globals.srch_cptr)->addr;
    for (j=0, k=0; k < orte_gpr_replica_globals.num_srch_cptr &&
                   j < (orte_gpr_replica_globals.srch_cptr)->size; j++) {
        if (NULL != cptr[j]) {
            k++;
            /* If no keys are provided, then remove entire container  */
            if (0 < num_tokens && 0 == num_keys){
                rc = orte_gpr_replica_release_container(seg, cptr[j]);
                if (ORTE_SUCCESS != rc) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
            else if( 0 < num_keys) {
                for (i=0; i < num_keys; i++) {  /* for each provided key */
                    if (ORTE_SUCCESS == orte_gpr_replica_search_container(
                                                       ORTE_GPR_REPLICA_OR,
                                                       key_itags, 1, cptr[j])) {
                        if (0 < orte_gpr_replica_globals.num_srch_ival) {
                            /* found this key at least once - delete all
                             * occurrences
                             */
                            ivals = (orte_gpr_replica_itagval_t**)
                                (orte_gpr_replica_globals.srch_ival)->addr;
                            for (n=0, p=0; p < orte_gpr_replica_globals.num_srch_ival &&
                                     n < (orte_gpr_replica_globals.srch_ival)->size; n++) {
                                if (NULL != ivals[n]) {
                                    p++;
                                    if (ORTE_SUCCESS != (rc = orte_gpr_replica_delete_itagval(seg, cptr[j], ivals[n]))) {
                                        ORTE_ERROR_LOG(rc);
                                        return rc;
                                    }
                                    if ( 0 == ((cptr[j])->itagvals)->size) {
                                        /* If container is empty, remove it */
                                        rc = orte_gpr_replica_release_container(seg, cptr[j]);
                                        if (ORTE_SUCCESS != rc) {
                                            ORTE_ERROR_LOG(rc);
                                            return rc;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    return ORTE_SUCCESS;
}

int orte_gpr_replica_delete_entries_nb_fn(
                    orte_gpr_addr_mode_t addr_mode,
                    orte_gpr_replica_segment_t *seg,
                    orte_gpr_replica_itag_t *token_itags, orte_std_cntr_t num_tokens,
                    orte_gpr_replica_itag_t *key_tags, orte_std_cntr_t num_keys)
{
    OPAL_TRACE(2);
    
    return ORTE_ERR_NOT_IMPLEMENTED;
}                           


int orte_gpr_replica_index_fn(orte_gpr_replica_segment_t *seg,
                            orte_std_cntr_t *cnt, char ***index)
{
    char **ptr;
    orte_gpr_replica_segment_t **segs;
    char **dict;
    orte_std_cntr_t i, j;


    OPAL_TRACE(2);
    
    /* set default responses */
    *index = NULL;
    *cnt = 0;
    
    if (NULL == seg) { /* looking for index of global registry */
        /* it is possible that no segments might exist - for example, if someone
         * requested an index immediately after system start. Protect against
         * that case
         */
        if (0 == orte_gpr_replica.num_segs) {
            return ORTE_SUCCESS;
        }
        *index = (char**)malloc(orte_gpr_replica.num_segs * sizeof(char*));
        if (NULL == *index) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        ptr = *index;
        segs = (orte_gpr_replica_segment_t**) (orte_gpr_replica.segments)->addr;
        for (i=0, j=0; j < orte_gpr_replica.num_segs &&
                       i < (orte_gpr_replica.segments)->size; i++) {
            if (NULL != segs[i]) {
                ptr[j] = strdup(segs[i]->name);
                if (NULL == ptr[j]) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    *cnt = j;
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                j++;
            }
        }
        *cnt = orte_gpr_replica.num_segs;
        return ORTE_SUCCESS;
    }
    
    /* must have requested index of a specific segment */
    if (0 < seg->num_dict_entries) {
        *index = (char**)malloc(orte_gpr_replica.num_segs * sizeof(char*));
        if (NULL == *index) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        ptr = *index;
        dict = (char**)(seg->dict)->addr;
    
        for (i=0, j=0; j < seg->num_dict_entries &&
                       i < (seg->dict)->size; i++) {
            if (NULL != dict[i]) {
                ptr[j] = strdup(dict[i]);
                if (NULL == ptr[j]) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    *cnt = j;
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                j++;
            }
        }
        *cnt = seg->num_dict_entries;
        return ORTE_SUCCESS;
    }

    /* it's okay if there are no entries, so return success */
    return ORTE_SUCCESS;
}


int orte_gpr_replica_index_nb_fn(orte_gpr_replica_segment_t *seg,
                        orte_gpr_notify_cb_fn_t cbfunc, void *user_tag)
{
    OPAL_TRACE(2);
    
    return ORTE_ERR_NOT_IMPLEMENTED;
}

