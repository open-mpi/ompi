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

#ifndef ORTE_MCA_RAS_BASE_H
#define ORTE_MCA_RAS_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/class/opal_list.h"

#include "orte/dss/dss_types.h"
#include "orte/mca/ras/ras.h"


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Internal definitions
 */

struct orte_ras_base_cmp_t {
    /** Base object */
    opal_list_item_t super;
    /** ras component */
    orte_ras_base_component_t *component;
    /** ras module */
    orte_ras_base_module_t* module;
    /** This component's priority */
    int priority;
};
typedef struct orte_ras_base_cmp_t orte_ras_base_cmp_t;


/*
 * function definitions
 */
ORTE_DECLSPEC int orte_ras_base_open(void);
ORTE_DECLSPEC int orte_ras_base_find_available(void);
ORTE_DECLSPEC int orte_ras_base_allocate(orte_jobid_t job,
                                         orte_ras_base_module_t **m);
ORTE_DECLSPEC int orte_ras_base_finalize(void);
ORTE_DECLSPEC int orte_ras_base_close(void);

ORTE_DECLSPEC int orte_ras_base_allocate_nodes(orte_jobid_t jobid, 
                                               opal_list_t* nodes);

/*
 * globals that might be needed
 */


typedef struct orte_ras_base_t {
    int ras_output;
    opal_list_t ras_opened;
    bool ras_opened_valid;
    opal_list_t ras_available;
    bool ras_available_valid;
    orte_std_cntr_t ras_num_nodes;
} orte_ras_base_t;
 
ORTE_DECLSPEC extern orte_ras_base_t orte_ras_base;

/** Class declaration */
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_ras_base_cmp_t);


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


/*
 * external API functions will be documented in the mca/ns/ns.h file
 */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
