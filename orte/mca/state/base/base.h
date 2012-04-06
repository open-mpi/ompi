/*
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef ORTE_MCA_STATE_BASE_H
#define ORTE_MCA_STATE_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/constants.h"

#include "opal/class/opal_list.h"

#include "opal/mca/mca.h"
#include "orte/mca/state/state.h"


BEGIN_C_DECLS

/*
 * MCA Framework functions
 */
ORTE_DECLSPEC    int orte_state_base_open(void);
ORTE_DECLSPEC    int orte_state_base_select(void);
ORTE_DECLSPEC    int orte_state_base_close(void);

/**
 * Output and component variables
 */
ORTE_DECLSPEC extern opal_list_t orte_state_base_components_available;

/**
 * Internal module reference
 */
ORTE_DECLSPEC extern orte_state_base_component_t orte_state_base_selected_component;


/* debug tools */
ORTE_DECLSPEC void orte_state_base_print_job_state_machine(void);

ORTE_DECLSPEC void orte_state_base_print_proc_state_machine(void);

END_C_DECLS

#endif
