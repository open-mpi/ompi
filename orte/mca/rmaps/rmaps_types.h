/* Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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

#ifndef ORTE_MCA_RMAPS_TYPES_H
#define ORTE_MCA_RMAPS_TYPES_H

#include "orte_config.h"
#include "orte/orte_constants.h"

#include "orte/mca/ns/ns_types.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/ras/ras_types.h"
#include "orte/mca/rml/rml_types.h"

#include "orte/mca/rmaps/rmaps.h"

/*
 * General MAP types
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    
/*
 * Mapping of nodes to process ranks.
 */

struct orte_rmaps_base_node_t {
    opal_list_item_t super;
    orte_ras_node_t* node;
    opal_list_t node_procs;   /* list of rmaps_base_proc_t */
};
typedef struct orte_rmaps_base_node_t orte_rmaps_base_node_t;

OBJ_CLASS_DECLARATION(orte_rmaps_base_node_t);


/*
 * Mapping of a process rank to a specific node.
 */

struct orte_rmaps_base_proc_t {
    opal_list_item_t super;
    char *app;          /* name of executable */
    orte_rmaps_base_node_t* proc_node;
    orte_process_name_t proc_name;
    orte_std_cntr_t proc_rank;
    pid_t pid;          /* PLS-assigned pid */
    pid_t local_pid;    /* pid found by local process */
};
typedef struct orte_rmaps_base_proc_t orte_rmaps_base_proc_t;

OBJ_CLASS_DECLARATION(orte_rmaps_base_proc_t);


/*
 * Structure that represents the mapping of an application to an
 * allocated set of resources.
 */

struct orte_rmaps_base_map_t {
    opal_list_item_t super;
    orte_app_context_t *app;
    orte_rmaps_base_proc_t** procs;
    orte_std_cntr_t num_procs;
    opal_list_t nodes;		/* list of rmaps_base_node_t */
};
typedef struct orte_rmaps_base_map_t orte_rmaps_base_map_t;

OBJ_CLASS_DECLARATION(orte_rmaps_base_map_t);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
