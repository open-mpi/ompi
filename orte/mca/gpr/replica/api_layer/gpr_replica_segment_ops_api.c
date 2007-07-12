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

#include "orte/orte_constants.h"
#include "opal/util/trace.h"

#include "orte/class/orte_pointer_array.h"
#include "orte/mca/errmgr/errmgr.h"

#include "gpr_replica_api.h"

int orte_gpr_replica_preallocate_segment(char *name, orte_std_cntr_t num_slots)
{
    int rc;
    orte_gpr_replica_segment_t *seg=NULL;

    OPAL_TRACE(1);

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    /* find the segment */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, name))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    rc = orte_pointer_array_set_size(seg->containers, num_slots);

    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

    return rc;
}

int orte_gpr_replica_get_number_entries(orte_std_cntr_t *n, char *segment, char **tokens)
{
    int rc;
    orte_gpr_replica_segment_t *seg=NULL;
    orte_std_cntr_t num_tokens=0;
    orte_gpr_replica_itag_t *itags=NULL;
    orte_gpr_replica_container_t **cptr;
    orte_std_cntr_t j, k, num_keyvals;
    
    OPAL_TRACE(1);
    
    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);
    
    /* find the segment */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, segment))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }
    
    /* if no tokens provided, just return the number of containers */
    if (NULL == tokens) {
        *n = seg->num_containers;
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return ORTE_SUCCESS;
    }
    
    /* otherwise, look up the container - convert tokens to array of itags */
    num_keyvals = 0;
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&itags, seg,
                                                             tokens, &num_tokens))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* find the specified container(s) */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_containers(seg, ORTE_GPR_REPLICA_AND,
                                                               itags, num_tokens))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* loop through any found containers and add up their keyvals */
    cptr = (orte_gpr_replica_container_t**)(orte_gpr_replica_globals.srch_cptr)->addr;
    for (j=0, k=0; k < orte_gpr_replica_globals.num_srch_cptr &&
         j < (orte_gpr_replica_globals.srch_cptr)->size; j++) {
        if (NULL != cptr[j]) {
            k++;
            num_keyvals += cptr[j]->num_itagvals;
        }
    }
    
CLEANUP:
    if (NULL != itags) free(itags);
    
    *n = num_keyvals;
    OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
    return ORTE_SUCCESS;
}
