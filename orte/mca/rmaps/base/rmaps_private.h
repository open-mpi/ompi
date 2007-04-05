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

#ifndef ORTE_MCA_RMAPS_PRIVATE_H
#define ORTE_MCA_RMAPS_PRIVATE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"

#include "orte/mca/ns/ns_types.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/ras/ras_types.h"

#include "orte/mca/rmaps/rmaps.h"

/*
 * Functions for use solely within the RMAPS framework
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* Define the RMAPS command flag */
typedef uint8_t orte_rmaps_cmd_flag_t;
#define ORTE_RMAPS_CMD	ORTE_UINT8
    
/* define some commands */
#define ORTE_RMAPS_MAP_CMD     0x01
    
/*
 * RMAPS component/module/priority tuple
 */
struct orte_rmaps_base_cmp_t {
    /** Base object */
    opal_list_item_t super;
    /** rmaps component */
    orte_rmaps_base_component_t *component;
    /** rmaps module */
    orte_rmaps_base_module_t* module;
    /** This component's priority */
    int priority;
};
/* Convenience typedef */
typedef struct orte_rmaps_base_cmp_t orte_rmaps_base_cmp_t;
/* Class declaration */
OBJ_CLASS_DECLARATION(orte_rmaps_base_cmp_t);


/*
 * Base API functions
 */

/*
 * Map a job
 * All calls to rmaps.map_job are routed through this function. This allows callers to
 * the RMAPS framework to specify the particular mapper they wish to use.
 */
ORTE_DECLSPEC int orte_rmaps_base_map_job(orte_jobid_t job, opal_list_t *attributes);

/*
 * Get job map
 * Retrieve the information for a job map from the registry and reassemble it into
 * an job_map object. Memory for the job_map object and all of its elements is
 * allocated by the function
 */
ORTE_DECLSPEC	int orte_rmaps_base_get_job_map(orte_job_map_t **map, orte_jobid_t job);

/*
 * Get node map
 * Retrieve the information for a job map from the registry and provide the info
 * for the specified node
 */
ORTE_DECLSPEC	int orte_rmaps_base_get_node_map(orte_mapped_node_t **node, orte_cellid_t cell,
                                                 char *nodename, orte_jobid_t job);



/*
 * Registry functions for maps
 */
/*
 * Put job map
 * Given a pointer to an orte_job_map_t, place the map's information on
 * the registry. Info is entered into the containers for each individual process on
 * the job's segment. Additionally, the function sets the INIT counter to the number
 * of processes in the map, thus causing the INIT trigger to fire so that any
 * attached subscriptions can be serviced.
 */
ORTE_DECLSPEC	int orte_rmaps_base_put_job_map(orte_job_map_t *map);


/*
 * Store a mapping plan
 * Given a list of attributes, this function stores all the RMAPS-specific
 * attributes on the registry for later use - e.g., by a child job that
 * wants to be mapped in an fashion identical to that of its parent
 */
int orte_rmaps_base_store_mapping_plan(orte_jobid_t job, opal_list_t *attrs);

/*
 * Get a mapping plan
 * Given a jobid, retrieve the stored mapping plan for that job. The
 * RMAPS-specific attributes will UPDATE the provided list to avoid
 * the possibility of duplicate list entries. Any existing RMAPS-specific
 * entries on the provided list will, therefore, be OVERWRITTEN.
 */
int orte_rmaps_base_get_mapping_plan(orte_jobid_t job, opal_list_t *attrs);

/*
 * Update the mapping state
 * Dynamically spawned child jobs that share resources with their parent
 * need to know where the parent job stopped mapping so they can pickup
 * from the right place. Once the child is mapped, however, we need to update
 * that info for the *parent* so that any additional children can have the
 * right info.
 */
int orte_rmaps_base_update_mapping_state(orte_jobid_t parent_job,
                                         opal_list_t *attrs);


/*
 * communication functions
 */
int orte_rmaps_base_comm_start(void);
int orte_rmaps_base_comm_stop(void);
void orte_rmaps_base_recv(int status, orte_process_name_t* sender,
                          orte_buffer_t* buffer, orte_rml_tag_t tag,
                          void* cbdata);

/*
 * Internal support functions
 */
/*
 * Function to add a mapped_proc entry to a map
 * Scans list of nodes on map to see if the specified one already
 * exists - if so, just add this entry to that node's list of
 * procs. If not, then add new node entry and put this proc
 * on its list.
 */
int orte_rmaps_base_add_proc_to_map(orte_job_map_t *map, orte_cellid_t cell, char *nodename, int32_t launch_id,
                                    char *username, bool oversubscribed, orte_mapped_proc_t *proc);

ORTE_DECLSPEC int orte_rmaps_base_get_target_nodes(opal_list_t* node_list, orte_jobid_t jobid,
                                                   orte_std_cntr_t *total_num_slots, bool no_use_local);
ORTE_DECLSPEC int orte_rmaps_base_update_node_usage(opal_list_t *nodes);
ORTE_DECLSPEC int orte_rmaps_base_get_mapped_targets(opal_list_t *mapped_node_list,
                                                     orte_app_context_t *app,
                                                     opal_list_t *master_node_list,
                                                     orte_std_cntr_t *total_num_slots);

ORTE_DECLSPEC int orte_rmaps_base_claim_slot(orte_job_map_t *map,
                                             orte_ras_node_t *current_node,
                                             orte_jobid_t jobid, orte_vpid_t vpid,
                                             orte_std_cntr_t app_idx,
                                             opal_list_t *nodes,
                                             opal_list_t *fully_used_nodes,
                                             bool oversubscribe);

ORTE_DECLSPEC int orte_rmaps_base_proxy_map_job(orte_jobid_t job, opal_list_t *attributes);

/** Local data type functions */
void orte_rmaps_base_std_obj_release(orte_data_value_t *value);

/* JOB_MAP */
int orte_rmaps_base_copy_map(orte_job_map_t **dest, orte_job_map_t *src, orte_data_type_t type);
int orte_rmaps_base_compare_map(orte_job_map_t *value1, orte_job_map_t *value2, orte_data_type_t type);
int orte_rmaps_base_pack_map(orte_buffer_t *buffer, void *src,
                            orte_std_cntr_t num_vals, orte_data_type_t type);
int orte_rmaps_base_print_map(char **output, char *prefix, orte_job_map_t *src, orte_data_type_t type);
int orte_rmaps_base_size_map(size_t *size, orte_job_map_t *src, orte_data_type_t type);
int orte_rmaps_base_unpack_map(orte_buffer_t *buffer, void *dest,
                              orte_std_cntr_t *num_vals, orte_data_type_t type);

/* MAPPED_PROC */
int orte_rmaps_base_copy_mapped_proc(orte_mapped_proc_t **dest, orte_mapped_proc_t *src, orte_data_type_t type);
int orte_rmaps_base_compare_mapped_proc(orte_mapped_proc_t *value1, orte_mapped_proc_t *value2, orte_data_type_t type);
int orte_rmaps_base_pack_mapped_proc(orte_buffer_t *buffer, void *src,
                             orte_std_cntr_t num_vals, orte_data_type_t type);
int orte_rmaps_base_print_mapped_proc(char **output, char *prefix, orte_mapped_proc_t *src, orte_data_type_t type);
int orte_rmaps_base_size_mapped_proc(size_t *size, orte_mapped_proc_t *src, orte_data_type_t type);
int orte_rmaps_base_unpack_mapped_proc(orte_buffer_t *buffer, void *dest,
                               orte_std_cntr_t *num_vals, orte_data_type_t type);

/* MAPPED_NODE */
int orte_rmaps_base_copy_mapped_node(orte_mapped_node_t **dest, orte_mapped_node_t *src, orte_data_type_t type);
int orte_rmaps_base_compare_mapped_node(orte_mapped_node_t *value1, orte_mapped_node_t *value2, orte_data_type_t type);
int orte_rmaps_base_pack_mapped_node(orte_buffer_t *buffer, void *src,
                             orte_std_cntr_t num_vals, orte_data_type_t type);
int orte_rmaps_base_print_mapped_node(char **output, char *prefix, orte_mapped_node_t *src, orte_data_type_t type);
int orte_rmaps_base_size_mapped_node(size_t *size, orte_mapped_node_t *src, orte_data_type_t type);
int orte_rmaps_base_unpack_mapped_node(orte_buffer_t *buffer, void *dest,
                               orte_std_cntr_t *num_vals, orte_data_type_t type);

/*
 * external API functions will be documented in the mca/rmaps/rmaps.h file
 */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
