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
/** Class declaration */
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_ras_base_cmp_t);


/*
 * function definitions
 */
ORTE_DECLSPEC int orte_ras_base_open(void);
ORTE_DECLSPEC int orte_ras_base_find_available(void);
ORTE_DECLSPEC int orte_ras_base_finalize(void);
ORTE_DECLSPEC int orte_ras_base_close(void);


/*
 * globals that might be needed
 */
typedef struct orte_ras_base_t {
    int ras_output;
    opal_list_t ras_opened;
    bool ras_opened_valid;
    bool ras_using_proxy;
    opal_list_t ras_available;
    bool ras_available_valid;
    orte_std_cntr_t ras_num_nodes;
    bool timing;
} orte_ras_base_t;
 
ORTE_DECLSPEC extern orte_ras_base_t orte_ras_base;

/*
 * external API functions will be documented in the mca/ns/ns.h file
 */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
