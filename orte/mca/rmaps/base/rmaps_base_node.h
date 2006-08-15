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
 * RMAPS framework base functionality.
 */

#ifndef ORTE_RMAPS_BASE_NODE_H
#define ORTE_RMAPS_BASE_NODE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "opal/class/opal_list.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/rmaps/rmaps.h"


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * RMAPS 
 */

int orte_rmaps_base_get_target_nodes(opal_list_t* node_list, orte_jobid_t jobid, orte_std_cntr_t *total_num_slots);
int orte_rmaps_base_update_node_usage(opal_list_t *nodes);
int orte_rmaps_base_get_mapped_targets(opal_list_t *mapped_node_list,
                                       orte_app_context_t *app,
                                       opal_list_t *master_node_list,
                                       orte_std_cntr_t *total_num_slots);
int orte_rmaps_base_claim_slot(orte_rmaps_base_map_t *map,
                               orte_ras_node_t *current_node,
                               orte_jobid_t jobid, orte_vpid_t vpid,
                               int proc_index,
                               opal_list_t *nodes,
                               opal_list_t *fully_used_nodes);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
