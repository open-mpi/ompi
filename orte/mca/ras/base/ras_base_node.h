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
/** @file */

#ifndef ORTE_RAS_BASE_NODE_H
#define ORTE_RAS_BASE_NODE_H

#include "include/orte_types.h"
#include "mca/soh/soh_types.h"
#include "mca/rmgr/rmgr_types.h"
#include "mca/ras/ras.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Query the registry for all available nodes 
 */
int orte_ras_base_node_query(opal_list_t*);

/*
 * Query the registry for a specific node 
 */
orte_ras_node_t* orte_ras_base_node_lookup(orte_cellid_t, const char* nodename);

/**
 * Query the registry for all nodes allocated to a specific job
 */
int orte_ras_base_node_query_alloc(opal_list_t*, orte_jobid_t);

/**
 * Add the specified node definitions to the registry
 */
int orte_ras_base_node_insert(opal_list_t*);

/**
 * Delete the specified nodes from the registry
 */
int orte_ras_base_node_delete(opal_list_t*);

/**
 * Assign the allocated slots on the specified nodes to the  
 * indicated jobid.
 */
int orte_ras_base_node_assign(opal_list_t*, orte_jobid_t);

/**
 * Check to see if the node segment is empty
 */
int orte_ras_base_node_segment_empty(bool *empty);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
