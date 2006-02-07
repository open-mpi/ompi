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
 * The Open MPI General Purpose Registry - proxy component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "include/orte_constants.h"
#include "dss/dss_types.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"
#include "util/proc_info.h"

#include "mca/ns/ns_types.h"
#include "mca/oob/oob_types.h"
#include "mca/rml/rml.h"

#include "gpr_proxy.h"

int orte_gpr_proxy_preallocate_segment(char *name, size_t num_slots)
{
    OPAL_TRACE(1);

    return ORTE_ERR_NOT_IMPLEMENTED;
}
