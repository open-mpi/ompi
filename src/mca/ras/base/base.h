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

#ifndef ORTE_MCA_RAS_BASE_H
#define ORTE_MCA_RAS_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "include/orte_constants.h"
#include "class/ompi_list.h"
#include "mca/ras/ras.h"


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
    ompi_list_item_t super;
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
ORTE_DECLSPEC int orte_ras_base_close(void);
ORTE_DECLSPEC orte_ras_base_module_t* orte_ras_base_select(const char*);
ORTE_DECLSPEC int orte_ras_base_allocate(orte_jobid_t job);
ORTE_DECLSPEC int orte_ras_base_deallocate(orte_jobid_t job);
ORTE_DECLSPEC int orte_ras_base_allocate_nodes(orte_jobid_t jobid, ompi_list_t* nodes);

/*
 * globals that might be needed
 */


typedef struct orte_ras_base_t {
    int ras_output;
    ompi_list_t ras_opened;
    ompi_list_t ras_available;
    size_t ras_num_nodes;
} orte_ras_base_t;
 
ORTE_DECLSPEC extern orte_ras_base_t orte_ras_base;

/** Class declaration */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(orte_ras_base_cmp_t);


/*
 * external API functions will be documented in the mca/ns/ns.h file
 */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
