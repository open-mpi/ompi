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

/*
 * includes
 */
#include "orte_config.h"

#include "class/ompi_list.h"
#include "mca/mca.h"

#include "mca/soh/base/base.h"

#include "include/orte_constants.h"


int orte_soh_base_get_node_soh_not_available(orte_node_state_t *state,
                                                      orte_cellid_t cell,
                                                      char *nodename)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_soh_base_set_node_soh_not_available(orte_cellid_t cell,
                                             char *nodename,
                                             orte_node_state_t state)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_soh_base_module_finalize_not_available (void)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}
