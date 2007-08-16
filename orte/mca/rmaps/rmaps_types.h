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
#include "orte/mca/rmgr/rmgr_types.h"

/*
 * General MAP types
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/****   RMAPS ATTRIBUTES   ***/
#define ORTE_RMAPS_MAP_POLICY           "orte-map-policy"
#define ORTE_RMAPS_PERNODE              "orte-map-pernode"
#define ORTE_RMAPS_N_PERNODE            "orte-map-n-pernode"
#define ORTE_RMAPS_NO_USE_LOCAL         "orte-map-no-use-local"
#define ORTE_RMAPS_NO_OVERSUB           "orte-map-no-oversubscribe"
#define ORTE_RMAPS_DESIRED_MAPPER       "orte-map-desired"
#define ORTE_RMAPS_USE_PARENT_PLAN      "orte-map-use-parent-plan"
#define ORTE_RMAPS_BOOKMARK             "orte-map-bookmark"
#define ORTE_RMAPS_DISPLAY_AFTER_MAP    "orte-map-display"
#define ORTE_RMAPS_NO_ALLOC_RANGE       "orte-map-no-alloc-range"
    
/****   JOB_MAP OBJECTS   ***/
/*
 * Mapped process info for job_map
 */
struct orte_mapped_proc_t {
    opal_list_item_t super;
    orte_process_name_t name;	/* process name */
    orte_std_cntr_t	rank;		/* process rank */
    orte_std_cntr_t	app_idx;	/* index of app_context for this process */
    pid_t pid;
};
typedef struct orte_mapped_proc_t orte_mapped_proc_t;
OBJ_CLASS_DECLARATION(orte_mapped_proc_t);

/*
 * Mapping of nodes to process ranks.
 */
struct orte_mapped_node_t {
    opal_list_item_t super;
    orte_cellid_t cell;	 			/* cell where this node is located */
    char *nodename;		 			/* name of node */
    int32_t launch_id;              /* launch id of node - needed by some systems */
    char *username;
    orte_process_name_t *daemon;	/* name of the daemon on this node
                                     * NULL => daemon not assigned yet
                                     */
    bool oversubscribed;            /* whether or not the #procs > #process slots on this node */
    orte_std_cntr_t num_procs;      /* #procs on this node - just the length of the procs list, but
                                     * stored here so we don't have to keep recomputing it elsewhere
                                     */
    opal_list_t procs;   			/* list of mapped_proc objects on this node */
};
typedef struct orte_mapped_node_t orte_mapped_node_t;
OBJ_CLASS_DECLARATION(orte_mapped_node_t);

/*
 * Structure that represents the mapping of a job to an
 * allocated set of resources.
 */
struct orte_job_map_t {
    opal_object_t super;
    orte_jobid_t job;
    char *mapping_mode;
    orte_vpid_t vpid_start;
    orte_vpid_t vpid_range;
    orte_std_cntr_t num_apps;	/* number of app_contexts */
    orte_app_context_t **apps;	/* the array of app_contexts for this job */
    orte_std_cntr_t num_nodes;  /* #nodes in this map - just the length of the nodes list, but
                                 * stored here so we don't have to keep recomputing it elsewhere
                                 */
    opal_list_t nodes;			/* list of mapped_node_t */
};
typedef struct orte_job_map_t orte_job_map_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_job_map_t);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
