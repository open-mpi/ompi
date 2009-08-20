/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_SENSOR_BASE_H
#define MCA_SENSOR_BASE_H

/*
 * includes
 */
#include "orte_config.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"

#include "orte/mca/sensor/sensor.h"


/*
 * Global functions for MCA overall collective open and close
 */
BEGIN_C_DECLS

/*
 * function definitions
 */
ORTE_DECLSPEC    int orte_sensor_base_open(void);
ORTE_DECLSPEC    int orte_sensor_base_select(void);
ORTE_DECLSPEC    int orte_sensor_base_close(void);

/*
 * globals that might be needed
 */

ORTE_DECLSPEC extern int orte_sensor_base_output;
ORTE_DECLSPEC extern opal_list_t mca_sensor_base_components_available;
ORTE_DECLSPEC extern opal_list_t orte_sensor_base_selected_modules;

/* object definition */
typedef struct {
    opal_list_item_t super;
    orte_sensor_base_component_t    *component;
    orte_sensor_base_module_t       *module;
} orte_sensor_base_selected_pair_t;
OBJ_CLASS_DECLARATION(orte_sensor_base_selected_pair_t);

#if !ORTE_DISABLE_FULL_SUPPORT

/* no base functions to protect at this time */

#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS
#endif
