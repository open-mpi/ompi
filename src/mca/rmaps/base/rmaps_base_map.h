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
 * RMAPS framework base functionality.
 */

#ifndef ORTE_RMAPS_BASE_MAPPING_H
#define ORTE_RMAPS_BASE_MAPPING_H

/*
 * includes
 */
#include "orte_config.h"
#include "include/orte_constants.h"

#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/ns/ns_types.h"
#include "mca/ras/base/ras_base_node.h"
#include "mca/rmaps/rmaps.h"


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * RMAPS 
 */

/*
 * Mapping of nodes to process ranks.
 */

struct orte_rmaps_base_node_t {
    ompi_list_item_t super;
    orte_cellid_t node_cellid;
    char* node_name;
    ompi_list_t node_procs;
};
typedef struct orte_rmaps_base_node_t orte_rmaps_base_node_t;

OBJ_CLASS_DECLARATION(orte_rmaps_base_node_t);


/*
 * Mapping of a process rank to a specific node.
 */

struct orte_rmaps_base_proc_t {
    ompi_list_item_t super;
    orte_rmaps_base_node_t* proc_node;
    orte_process_name_t proc_name;
    int proc_rank;
};
typedef struct orte_rmaps_base_proc_t orte_rmaps_base_proc_t;

OBJ_CLASS_DECLARATION(orte_rmaps_base_proc_t);


/*
 * Structure that represents the mapping of an application to an
 * allocated set of resources.
 */

struct orte_rmaps_base_map_t {
    ompi_list_item_t super;
    orte_app_context_t *app;
    orte_rmaps_base_proc_t** procs;
    size_t num_procs;
    ompi_list_t nodes;
};
typedef struct orte_rmaps_base_map_t orte_rmaps_base_map_t;

OBJ_CLASS_DECLARATION(orte_rmaps_base_map_t);


int orte_rmaps_base_get_map(orte_jobid_t, ompi_list_t* mapping);
int orte_rmaps_base_set_map(orte_jobid_t, ompi_list_t* mapping);
int orte_rmaps_base_get_node_map(orte_cellid_t, orte_jobid_t, const char*, ompi_list_t* mapping);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
