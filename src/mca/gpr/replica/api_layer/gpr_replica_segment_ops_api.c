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

#include "include/orte_constants.h"
#include "class/orte_pointer_array.h"

#include "gpr_replica_api.h"

int orte_gpr_replica_preallocate_segment(char *name, int num_slots)
{
    int rc;
    orte_gpr_replica_segment_t *seg=NULL;

    OMPI_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    /* find the segment */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, name))) {
        OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    if (0 < (seg->containers)->size) {  /* segment already exists! */
        return ORTE_ERR_BAD_PARAM;
    }
    
    rc = orte_pointer_array_init(&(seg->containers), num_slots,
                                            orte_gpr_replica_globals.max_size,
                                            orte_gpr_replica_globals.block_size);
                                            
     OMPI_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
     
     return rc;
}
