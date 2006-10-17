/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#ifndef ORTE_RDS_BASE_H
#define ORTE_RDS_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"

#include "orte/mca/rml/rml_types.h"
#include "orte/mca/rds/rds.h"


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Internal definitions
 */
struct orte_rds_base_selected_t {
    opal_list_item_t super;
    orte_rds_base_component_t *component;
    orte_rds_base_module_t* module;
};
typedef struct orte_rds_base_selected_t orte_rds_base_selected_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_rds_base_selected_t);


/*
 * API function definitions
 */
ORTE_DECLSPEC int orte_rds_base_open(void);
ORTE_DECLSPEC int orte_rds_base_select(void);
ORTE_DECLSPEC int orte_rds_base_finalize(void);
ORTE_DECLSPEC int orte_rds_base_close(void);

/*
 * globals that might be needed
 */

typedef struct orte_rds_base_t {
    int rds_output;
    bool no_op_selected;
    opal_list_t rds_components;
    opal_list_t rds_selected;
} orte_rds_base_t;

ORTE_DECLSPEC extern orte_rds_base_t orte_rds_base;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
