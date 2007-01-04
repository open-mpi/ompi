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

#ifndef ORTE_RAS_PRIVATE_H
#define ORTE_RAS_PRIVATE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"

#include "opal/class/opal_list.h"

#include "orte/dss/dss_types.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/ras/ras_types.h"

#include "orte/mca/ras/ras.h"
#include "orte/mca/ras/base/base.h"


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* Define the RAS command flag */
typedef uint8_t orte_ras_cmd_flag_t;
#define ORTE_RAS_CMD	ORTE_UINT8

/* define some commands */
#define ORTE_RAS_ALLOCATE_CMD       0x01
#define ORTE_RAS_DEALLOCATE_CMD     0x02
    
/*
 * API function definitions
 */
ORTE_DECLSPEC int orte_ras_base_allocate(orte_jobid_t job, opal_list_t *attributes);
ORTE_DECLSPEC int orte_ras_base_deallocate(orte_jobid_t job);

/*
 * NO_OP functions
 */
int orte_ras_base_allocate_no_op(orte_jobid_t job, opal_list_t *attributes);

int orte_ras_base_node_insert_no_op(opal_list_t *);

int orte_ras_base_node_query_no_op(opal_list_t *);

int orte_ras_base_deallocate_no_op(orte_jobid_t job);

int orte_ras_base_node_query_alloc_no_op(opal_list_t*, orte_jobid_t);

orte_ras_node_t* orte_ras_base_node_lookup_no_op(orte_cellid_t, const char* nodename);

/*
 * Internal support functions
 */
ORTE_DECLSPEC int orte_ras_base_allocate_nodes(orte_jobid_t jobid, 
                                               opal_list_t* nodes);

ORTE_DECLSPEC int orte_ras_base_reallocate(orte_jobid_t parent_jobid,
                                           orte_jobid_t child_jobid);

ORTE_DECLSPEC int orte_ras_base_set_oversubscribe_override(orte_jobid_t job);

/*
 * Query the registry for all available nodes 
 */
ORTE_DECLSPEC int orte_ras_base_node_query(opal_list_t*);

/*
 * Query the registry for a specific node 
 */
ORTE_DECLSPEC orte_ras_node_t* orte_ras_base_node_lookup(orte_cellid_t, const char* nodename);

/**
    * Query the registry for all nodes allocated to a specific job
 */
ORTE_DECLSPEC int orte_ras_base_node_query_alloc(opal_list_t*, orte_jobid_t);

/**
    * Add the specified node definitions to the registry
 */
ORTE_DECLSPEC int orte_ras_base_node_insert(opal_list_t*);

/**
    * Delete the specified nodes from the registry
 */
ORTE_DECLSPEC int orte_ras_base_node_delete(opal_list_t*);

/**
    * Assign the allocated slots on the specified nodes to the  
 * indicated jobid.
 */
ORTE_DECLSPEC int orte_ras_base_node_assign(opal_list_t*, orte_jobid_t);

/**
    * Check to see if the node segment is empty
 */
ORTE_DECLSPEC int orte_ras_base_node_segment_empty(bool *empty);


/*
 * oob interface
 */
int orte_ras_base_comm_start(void);
int orte_ras_base_comm_stop(void);
void orte_ras_base_recv(int status, orte_process_name_t* sender,
                        orte_buffer_t* buffer, orte_rml_tag_t tag, void* cbdata);

/** Local data type functions */
int orte_ras_base_copy_node(orte_ras_node_t **dest, orte_ras_node_t *src, orte_data_type_t type);
int orte_ras_base_compare_node(orte_ras_node_t *value1, orte_ras_node_t *value2, orte_data_type_t type);
int orte_ras_base_pack_node(orte_buffer_t *buffer, void *src,
                            orte_std_cntr_t num_vals, orte_data_type_t type);
int orte_ras_base_print_node(char **output, char *prefix, orte_ras_node_t *src, orte_data_type_t type);
void orte_ras_base_std_obj_release(orte_data_value_t *value);
int orte_ras_base_size_node(size_t *size, orte_ras_node_t *src, orte_data_type_t type);
int orte_ras_base_unpack_node(orte_buffer_t *buffer, void *dest,
                              orte_std_cntr_t *num_vals, orte_data_type_t type);



#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
