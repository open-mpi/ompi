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
 */

#include "orte_config.h"
#include "include/orte_constants.h"
#include "util/proc_info.h"
#include "mca/base/mca_base_param.h"
#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"
#include "mca/ns/base/base.h"
#include "mca/ns/base/ns_base_nds.h"


int orte_ns_nds_pipe_get(void)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_ns_nds_pipe_put(const orte_process_name_t* name, orte_vpid_t vpid_start, size_t num_procs)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

