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
 * The Open MPI general purpose registry - support functions.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "mca/errmgr/errmgr.h"

#include "gpr_replica_fn.h"

/*
 */
bool orte_gpr_replica_check_itag_list(orte_gpr_replica_addr_mode_t addr_mode,
                    int num_itags_search,
                    orte_gpr_replica_itag_t *itags,
                    int num_itags_entry,
                    orte_gpr_replica_itag_t *entry_itags)
{
    int num_found;
    bool exclusive, no_match, not_set;
    int i, j;

    /* check for trivial case */
    if (NULL == itags || 0 >= num_itags_search) {  /* wildcard case - automatically true */
	   return true;
    }

    if (ORTE_GPR_REPLICA_NOT & addr_mode) { /* NOT flag set */
        not_set = true;
    } else {
        not_set = false;
    }
    
    /* take care of trivial cases that don't require search */
    if ((ORTE_GPR_REPLICA_XAND & addr_mode) &&
	    (num_itags_search != num_itags_entry)) { /* can't possibly turn out "true" */
        if (not_set) return true;
	    else return false;
    }

    if ((ORTE_GPR_REPLICA_AND & addr_mode) &&
	    (num_itags_search > num_itags_entry)) {  /* can't find enough matches */
	    if (not_set) return true;
        else return false;
    }

    /* okay, have to search for remaining possibilities */
    num_found = 0;
    exclusive = true;
    for (i=0; i < num_itags_entry; i++) {
        	no_match = true;
        	for (j=0; j < num_itags_search; j++) {
        	    if (entry_itags[i] == itags[j]) { /* found a match */
            		num_found++;
            		no_match = false;
            		if (ORTE_GPR_REPLICA_OR & addr_mode) { /* only need one match */
            		    if (not_set) return false;
                     else return true;
            		}
        	    }
        	}
        	if (no_match) {
        	    exclusive = false;
        	}
    }

    if (ORTE_GPR_REPLICA_XAND & addr_mode) {  /* deal with XAND case */
        	if (num_found == num_itags_entry) { /* found all, and nothing more */
        	    if (not_set) return false;
             else return true;
        	} else {  /* found either too many or not enough */
        	    if (not_set) return true;
             else return false;
        	}
    }

    if (ORTE_GPR_REPLICA_XOR & addr_mode) {  /* deal with XOR case */
        	if (num_found > 0 && exclusive) {  /* found at least one and nothing not on list */
        	    if (not_set) return false;
                else return true;
        	} else {
        	    if (not_set) return true;
             else return false;
        	}
    }

    if (ORTE_GPR_REPLICA_AND & addr_mode) {  /* deal with AND case */
        	if (num_found == num_itags_search) {  /* found all the required keys */
        	    if (not_set) return false;
             else return true;
        	} else {
        	    if (not_set) return true;
             else return false;
        	}
    }

    /* should be impossible situation, but just to be safe... */
    if (not_set) return true;
    else return false;
}


int orte_gpr_replica_copy_itag_list(orte_gpr_replica_itag_t **dest,
                                    orte_gpr_replica_itag_t *src, int num_itags)
{
    if (0 == num_itags || NULL == src) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    
    *dest = (orte_gpr_replica_itag_t*)malloc(num_itags * sizeof(orte_gpr_replica_itag_t));
    if (NULL == *dest) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    memcpy(*dest, src, num_itags*sizeof(orte_gpr_replica_itag_t));
    return ORTE_SUCCESS;
}

