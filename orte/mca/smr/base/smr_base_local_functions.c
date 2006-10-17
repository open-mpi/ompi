/*
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
 */

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"

#include "orte/mca/smr/base/smr_private.h"


/* these functions will default to SUCCESS so that environments
 * that do not provide any support will not fail. This is
 * particularly important for the "begin_monitoring" function
 * as some systems can call this without that support
 */

int orte_smr_base_get_node_state_not_available(orte_node_state_t *state,
                                                      orte_cellid_t cell,
                                                      char *nodename)
{
    return ORTE_SUCCESS;
}

int orte_smr_base_set_node_state_not_available(orte_cellid_t cell,
                                             char *nodename,
                                             orte_node_state_t state)
{
    return ORTE_SUCCESS;
}

int orte_smr_base_begin_monitoring_not_available(orte_jobid_t job)
{
    return ORTE_SUCCESS;
}

int orte_smr_base_module_finalize_not_available (void)
{
    return ORTE_SUCCESS;
}
