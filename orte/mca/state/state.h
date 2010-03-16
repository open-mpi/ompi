/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * The OpenRTE State Save/Recovery Service
 *
 */

#ifndef ORTE_STATE_H
#define ORTE_STATE_H

#include "orte_config.h"
#include "orte/types.h"

#include "opal/mca/mca.h"
#include "opal/dss/dss_types.h"

BEGIN_C_DECLS

/*
 * API functions
 */

/*
 * Initialize the module
 */
typedef int (*orte_state_base_module_init_fn_t)(void);

/*
 * Finalize the module
 */
typedef int (*orte_state_base_module_finalize_fn_t)(void);

/*
 * Save the state of the provided object
 */
typedef int (*orte_state_base_module_save_fn_t)(void *object, opal_data_type_t type);

/*
 * Set the source for recovering state info
 */
typedef int (*orte_state_base_module_set_recover_source_fn_t)(orte_process_name_t *name);

/*
 * Recover the state of an object
 */
typedef int (*orte_state_base_module_recover_fn_t)(void *object, opal_data_type_t type);


/*
 * the standard module data structure
 */
struct orte_state_base_module_1_0_0_t {
    orte_state_base_module_init_fn_t                init;
    orte_state_base_module_finalize_fn_t            finalize;
    orte_state_base_module_save_fn_t                save;
    orte_state_base_module_set_recover_source_fn_t  set_recover_source;
    orte_state_base_module_recover_fn_t             recover;
};
typedef struct orte_state_base_module_1_0_0_t orte_state_base_module_1_0_0_t;
typedef struct orte_state_base_module_1_0_0_t orte_state_base_module_t;

/*
 * the standard component data structure
 */
struct orte_state_base_component_1_0_0_t {
    mca_base_component_t base_version;
    mca_base_component_data_t base_data;
};
typedef struct orte_state_base_component_1_0_0_t orte_state_base_component_1_0_0_t;
typedef struct orte_state_base_component_1_0_0_t orte_state_base_component_t;

/*
 * Macro for use in components that are of type state
 */
#define ORTE_STATE_BASE_VERSION_1_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "state", 1, 0, 0

/* Global structure for accessing STATE functions */
ORTE_DECLSPEC extern orte_state_base_module_t orte_state;  /* holds selected module's function pointers */

END_C_DECLS

#endif
