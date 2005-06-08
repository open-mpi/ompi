/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
 * The Open MPI general purpose registry - support functions.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "class/orte_bitmap.h"
#include "mca/errmgr/errmgr.h"

#include "gpr_replica_fn.h"

/*
 */
bool orte_gpr_replica_check_itag_list(orte_gpr_replica_addr_mode_t addr_mode,
                    size_t num_itags_search,
                    orte_gpr_replica_itag_t *itags,
                    size_t num_itags_entry,
                    orte_gpr_replica_itag_t *entry_itags)
{
    size_t i, j;
    bool exclusive, match, found_one, not_set;
    int rc, bit_is_set;

    /* check for trivial case */
    if (NULL == itags || 0 == num_itags_search) {  /* wildcard case - automatically true */
	   return true;
    }

    if (ORTE_GPR_REPLICA_NOT & addr_mode) { /* NOT flag set */
        not_set = true;
    } else {
        not_set = false;
    }
    
    if (ORTE_GPR_REPLICA_XAND & addr_mode || ORTE_GPR_REPLICA_XOR & addr_mode) {
        exclusive = true;
    } else {
        exclusive = false;
    }
    
    if (ORTE_SUCCESS != (rc = orte_bitmap_clear_all_bits(&(orte_gpr_replica_globals.srch_itag)))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* run the search  - check the container's tags to see which search tags are found */
    found_one = false;
    for (i=0; i < num_itags_entry; i++) {  /* for each container tag */
        match = false;
    	for (j=0; j < num_itags_search; j++) {  /* check each search tag and see if it is present */
            /* need to explicitly extend the bit array here to ensure
             * it covers the specified itags. Otherwise, when
             * we check to see if the bit was set (in the logic down
             * below), we could potentially go outside the range of
             * the array
             */
            if (orte_gpr_replica_globals.srch_itag.legal_numbits < itags[j]) {
                if (ORTE_SUCCESS != (rc = orte_bitmap_cover_bit(&(orte_gpr_replica_globals.srch_itag), itags[j]))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
    	    if (entry_itags[i] == itags[j]) { /* found a match */
        		if (ORTE_SUCCESS != (rc = orte_bitmap_set_bit(&(orte_gpr_replica_globals.srch_itag), itags[j]))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
        		if (ORTE_GPR_REPLICA_OR & addr_mode) { /* only need one match */
        		    if (not_set) return false;
                    else return true;
        		}
                match = true;
                found_one = true;
    	    }
    	}
        if (!match && exclusive) {
             /* if it was exclusive, then I'm not allowed to have any tags outside
              * of those in the search list. Since I checked the search list and
              * found at least one that didn't match, this violates the exclusive requirement.
              */
             if (not_set) return true;
             else return false;
        }
    }

    /* If we get here, then we know we have passed the exclusive test. We also know
     * that we would have already returned in the OR case. So, first check the XOR
     * case
     */
     if ((ORTE_GPR_REPLICA_XOR & addr_mode) && found_one) {
        if (not_set) return false;
        else return true;
     }
     
     /* Only thing we have left to check is AND */
    /* check if any search tag was not found */
    for (i=0; i < num_itags_search; i++) {
        if (0 > (bit_is_set = orte_bitmap_is_set_bit(&(orte_gpr_replica_globals.srch_itag), itags[i]))) {
            ORTE_ERROR_LOG(bit_is_set);
            return bit_is_set;
        } else if (1 != bit_is_set) {
            /* this tag was NOT found - required to find them all */
            if (not_set) return true;
            else return false;
        }
    }
    /* okay, all the tags are there, so we now passed the AND test */
    if (not_set) return false;
    else return true;
    
}


int orte_gpr_replica_copy_itag_list(orte_gpr_replica_itag_t **dest,
                                    orte_gpr_replica_itag_t *src, size_t num_itags)
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

