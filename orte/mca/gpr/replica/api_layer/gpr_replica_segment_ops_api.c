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

#include "gpr_replica_api.h"

int orte_gpr_replica_preallocate_segment(char *name, orte_std_cntr_t num_slots)
{
    int rc;
    orte_gpr_replica_segment_t *seg=NULL;

    OPAL_TRACE(1);

    OPAL_THREAD_LOCK(&orte_gpr_replica_globals.mutex);

    /* find the segment */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, name))) {
        OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);
        return rc;
    }

    if (0 < (seg->containers)->size) {  /* segment already exists! */
        return ORTE_ERR_BAD_PARAM;
    }

    rc = orte_pointer_array_init(&(seg->containers), num_slots,
                                            (orte_std_cntr_t)orte_gpr_array_max_size,
                                            (orte_std_cntr_t)orte_gpr_array_block_size);

     OPAL_THREAD_UNLOCK(&orte_gpr_replica_globals.mutex);

     return rc;
}
