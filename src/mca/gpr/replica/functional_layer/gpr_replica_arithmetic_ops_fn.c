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

#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"

#include "mca/gpr/replica/transition_layer/gpr_replica_tl.h"

#include "gpr_replica_fn.h"


int orte_gpr_replica_increment_value_fn(orte_gpr_addr_mode_t addr_mode,
                                orte_gpr_replica_segment_t *seg,
                                orte_gpr_replica_itag_t *tokentags,
                                int num_tokens, int cnt,
                                orte_gpr_keyval_t **keyvals)
{
    orte_gpr_replica_container_t **cptr;
    orte_gpr_replica_itag_t itag;
    orte_gpr_replica_addr_mode_t tok_mode;
    orte_gpr_replica_itagval_t **ival;
    int rc, i, j, k, num_found;

    /* extract the token address mode */
    tok_mode = 0x004f & addr_mode;
    if (0x00 == tok_mode) {  /* default tokens addressing mode to AND */
        tok_mode = ORTE_GPR_REPLICA_AND;
    }

    /* find the specified container(s) */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_containers(&num_found, seg, tok_mode,
                                    tokentags, num_tokens))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (0 == num_found) { /* nothing found */
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* otherwise, go through list of containers. For each one,
       find the entry and then add one to its value */
    cptr = (orte_gpr_replica_container_t**)(orte_gpr_replica_globals.srch_cptr)->addr;
    for (j=0; j < (orte_gpr_replica_globals.srch_cptr)->size; j++) { /* for each container */
        if (NULL != cptr[j]) {
            for (i=0; i < cnt; i++) { /* for each provided keyval to be incremented */
                if (ORTE_SUCCESS == orte_gpr_replica_dict_lookup(&itag, seg, keyvals[i]->key) &&
                    ORTE_SUCCESS == orte_gpr_replica_search_container(&num_found,
                                            ORTE_GPR_REPLICA_OR, &itag, 1, cptr[j]) &&
                    0 < num_found) {
                    ival = (orte_gpr_replica_itagval_t**)((orte_gpr_replica_globals.srch_ival)->addr);
                    for (k=0; k < (orte_gpr_replica_globals.srch_ival)->size; k++) { /* for each found keyval */
                        if (NULL != ival[k]) {
                            switch (ival[k]->type) {
                                case ORTE_UINT8:
                                    ival[k]->value.ui8++;
                                    break;
                                
                                case ORTE_UINT16:
                                    ival[k]->value.ui16++;
                                    break;
                                
                                case ORTE_UINT32:
                                    ival[k]->value.ui32++;
                                    break;
                                
                            #ifdef HAVE_I64
                                case ORTE_UINT64:
                                    ival[k]->value.ui64++;
                                    break;
                                    
                                case ORTE_INT64:
                                    ival[k]->value.i64++;
                                    break;
                            #endif
                            
                                case ORTE_INT8:
                                    ival[k]->value.ui32++;
                                    break;
                                
                                case ORTE_INT16:
                                    ival[k]->value.i16++;
                                    break;
                                
                                case ORTE_INT32:
                                    ival[k]->value.i32++;
                                    break;
                                
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
                                int num_tokens, int cnt,
                                orte_gpr_keyval_t **keyvals)
{
    orte_gpr_replica_container_t **cptr;
    orte_gpr_replica_itag_t itag;
    orte_gpr_replica_addr_mode_t tok_mode;
    orte_gpr_replica_itagval_t **ival;
    int rc, i, j, k, num_found;

    /* extract the token address mode */
    tok_mode = 0x004f & addr_mode;
    if (0x00 == tok_mode) {  /* default tokens addressing mode to AND */
        tok_mode = ORTE_GPR_REPLICA_AND;
    }

    /* find the specified container(s) */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_containers(&num_found, seg, tok_mode,
                                    tokentags, num_tokens))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (0 == num_found) { /* nothing found */
        /* no ERROR_LOG entry created as this is not a system failure */
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* otherwise, go through list of containers. For each one,
       find the entry and then add one to its value */
    cptr = (orte_gpr_replica_container_t**)(orte_gpr_replica_globals.srch_cptr)->addr;
    for (j=0; j < (orte_gpr_replica_globals.srch_cptr)->size; j++) { /* for each container */
        if (NULL != cptr[j]) {
            for (i=0; i < cnt; i++) { /* for each provided keyval to be decremented */
                if (ORTE_SUCCESS == orte_gpr_replica_dict_lookup(&itag, seg, keyvals[i]->key) &&
                    ORTE_SUCCESS == orte_gpr_replica_search_container(&num_found,
                                            ORTE_GPR_REPLICA_OR, &itag, 1, cptr[j]) &&
                    0 < num_found) {
                    ival = (orte_gpr_replica_itagval_t**)((orte_gpr_replica_globals.srch_ival)->addr);
                    for (k=0; k < (orte_gpr_replica_globals.srch_ival)->size; k++) { /* for each found keyval */
                        if (NULL != ival[k]) {
                            switch (ival[k]->type) {
                                case ORTE_UINT8:
                                    ival[k]->value.ui8--;
                                    break;
                                
                                case ORTE_UINT16:
                                    ival[k]->value.ui16--;
                                    break;
                                
                                case ORTE_UINT32:
                                    ival[k]->value.ui32--;
                                    break;
                                
                            #ifdef HAVE_I64
                                case ORTE_UINT64:
                                    ival[k]->value.ui64--;
                                    break;
                                    
                                case ORTE_INT64:
                                    ival[k]->value.i64--;
                                    break;
                            #endif
                            
                                case ORTE_INT8:
                                    ival[k]->value.ui32--;
                                    break;
                                
                                case ORTE_INT16:
                                    ival[k]->value.i16--;
                                    break;
                                
                                case ORTE_INT32:
                                    ival[k]->value.i32--;
                                    break;
                                
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
