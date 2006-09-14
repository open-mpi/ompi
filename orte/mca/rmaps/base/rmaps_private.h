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
    
/* Internal support */
int orte_rmaps_base_comm_start(void);
void orte_rmaps_base_recv(int status, orte_process_name_t* sender,
                          orte_buffer_t* buffer, orte_rml_tag_t tag,
                          void* cbdata);
    
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
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_rmaps_base_cmp_t);


/*
 * Base functions
 */

ORTE_DECLSPEC   int orte_rmaps_base_map(orte_jobid_t job, char *desired_mapper);

/*
 * NO_OP functions
 */
ORTE_DECLSPEC   int orte_rmaps_base_map_no_op(orte_jobid_t job, char *desired_mapper);

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
int orte_rmaps_base_mapped_node_query(opal_list_t* mapping_list, opal_list_t* nodes_alloc, orte_jobid_t jobid);
int orte_rmaps_base_get_map(orte_jobid_t, opal_list_t* mapping);
int orte_rmaps_base_set_map(orte_jobid_t, opal_list_t* mapping);
int orte_rmaps_base_get_node_map(orte_cellid_t, orte_jobid_t, const char*, opal_list_t* mapping);

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

int orte_rmaps_base_set_vpid_range(orte_jobid_t jobid, orte_vpid_t start, orte_vpid_t range);
int orte_rmaps_base_get_vpid_range(orte_jobid_t jobid, orte_vpid_t *start, orte_vpid_t *range);

/*
 * external API functions will be documented in the mca/rmaps/rmaps.h file
 */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
