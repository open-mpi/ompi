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
 * The Open MPI General Purpose Registry - proxy component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "include/orte_constants.h"
#include "dps/dps_types.h"
#include "util/output.h"
#include "util/proc_info.h"

#include "mca/ns/ns_types.h"
#include "mca/oob/oob_types.h"
#include "mca/rml/rml.h"

#include "gpr_proxy.h"

int orte_gpr_proxy_preallocate_segment(char *name, int num_slots)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}
