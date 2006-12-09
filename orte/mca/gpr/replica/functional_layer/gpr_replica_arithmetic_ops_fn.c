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

#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/replica/transition_layer/gpr_replica_tl.h"

#include "orte/mca/gpr/replica/functional_layer/gpr_replica_fn.h"


int orte_gpr_replica_increment_value_fn(orte_gpr_addr_mode_t addr_mode,
                                orte_gpr_replica_segment_t *seg,
                                orte_gpr_replica_itag_t *tokentags,
                                orte_std_cntr_t num_tokens, orte_std_cntr_t cnt,
                                orte_gpr_keyval_t **keyvals)
{
    orte_gpr_replica_container_t **cptr;
    orte_gpr_replica_itag_t itag;
    orte_gpr_replica_addr_mode_t tok_mode;
    orte_gpr_replica_itagval_t **ival;
    int rc;
    orte_std_cntr_t i, j, k, m, n;

    OPAL_TRACE(2);
    
    /* extract the token address mode */
    tok_mode = ORTE_GPR_REPLICA_TOKMODE(addr_mode);
    if (0x00 == tok_mode) {  /* default tokens addressing mode to AND */
        tok_mode = ORTE_GPR_REPLICA_AND;
    }

    /* find the specified container(s) */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_containers(seg, tok_mode,
                                    tokentags, num_tokens))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (0 == orte_gpr_replica_globals.num_srch_cptr) { /* nothing found */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* otherwise, go through list of containers. For each one,
       find the entry and then add one to its value */
    cptr = (orte_gpr_replica_container_t**)(orte_gpr_replica_globals.srch_cptr)->addr;
    for (j=0, m=0; m < orte_gpr_replica_globals.num_srch_cptr &&
                   j < (orte_gpr_replica_globals.srch_cptr)->size; j++) { /* for each container */
        if (NULL != cptr[j]) {
            m++;
            for (i=0; i < cnt; i++) { /* for each provided keyval to be incremented */
                if (ORTE_SUCCESS == orte_gpr_replica_dict_lookup(&itag, seg, keyvals[i]->key) &&
                    ORTE_SUCCESS == orte_gpr_replica_search_container(
                                            ORTE_GPR_REPLICA_OR, &itag, 1, cptr[j]) &&
                    0 < orte_gpr_replica_globals.num_srch_ival) {
                    ival = (orte_gpr_replica_itagval_t**)((orte_gpr_replica_globals.srch_ival)->addr);
                    for (k=0, n=0; n < orte_gpr_replica_globals.num_srch_ival &&
                                   k < (orte_gpr_replica_globals.srch_ival)->size; k++) { /* for each found keyval */
                        if (NULL != ival[k]) {
                            n++;
                            if (ORTE_SUCCESS != (rc = orte_dss.increment(ival[k]->value))) {
                                ORTE_ERROR_LOG(rc);
                                return rc;
                            }
                        }
                    }
                }
            }
        }
    }
    return ORTE_SUCCESS;
}


int orte_gpr_replica_decrement_value_fn(orte_gpr_addr_mode_t addr_mode,
                                orte_gpr_replica_segment_t *seg,
                                orte_gpr_replica_itag_t *tokentags,
                                orte_std_cntr_t num_tokens, orte_std_cntr_t cnt,
                                orte_gpr_keyval_t **keyvals)
{
    orte_gpr_replica_container_t **cptr;
    orte_gpr_replica_itag_t itag;
    orte_gpr_replica_addr_mode_t tok_mode;
    orte_gpr_replica_itagval_t **ival;
    int rc;
    orte_std_cntr_t i, j, k, m, n;

    OPAL_TRACE(2);
    
    /* extract the token address mode */
    tok_mode = ORTE_GPR_REPLICA_TOKMODE(addr_mode);
    if (0x00 == tok_mode) {  /* default tokens addressing mode to AND */
        tok_mode = ORTE_GPR_REPLICA_AND;
    }

    /* find the specified container(s) */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_containers(seg, tok_mode,
                                    tokentags, num_tokens))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (0 == orte_gpr_replica_globals.num_srch_cptr) { /* nothing found */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* otherwise, go through list of containers. For each one,
       find the entry and then subtract one from its value */
    cptr = (orte_gpr_replica_container_t**)(orte_gpr_replica_globals.srch_cptr)->addr;
    for (j=0, m=0; m < orte_gpr_replica_globals.num_srch_cptr &&
                   j < (orte_gpr_replica_globals.srch_cptr)->size; j++) { /* for each container */
        if (NULL != cptr[j]) {
            m++;
            for (i=0; i < cnt; i++) { /* for each provided keyval to be incremented */
                if (ORTE_SUCCESS == orte_gpr_replica_dict_lookup(&itag, seg, keyvals[i]->key) &&
                    ORTE_SUCCESS == orte_gpr_replica_search_container(
                                            ORTE_GPR_REPLICA_OR, &itag, 1, cptr[j]) &&
                    0 < orte_gpr_replica_globals.num_srch_ival) {
                    ival = (orte_gpr_replica_itagval_t**)((orte_gpr_replica_globals.srch_ival)->addr);
                    for (k=0, n=0; n < orte_gpr_replica_globals.num_srch_ival &&
                                   k < (orte_gpr_replica_globals.srch_ival)->size; k++) { /* for each found keyval */
                        if (NULL != ival[k]) {
                            n++;
                            if (ORTE_SUCCESS != (rc = orte_dss.decrement(ival[k]->value))) {
                                ORTE_ERROR_LOG(rc);
                                return rc;
                            }
                        }
                    }
                } else {
                    return ORTE_ERR_NOT_FOUND;
                }
            }
        }
    }
    return ORTE_SUCCESS;
}
