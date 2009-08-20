/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "orte/mca/sensor/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/sensor/base/static-components.h"

/* object definition */
static void construct(orte_sensor_base_selected_pair_t *obj)
{
    obj->component = NULL;
    obj->module = NULL;
}

static void destruct(orte_sensor_base_selected_pair_t *obj)
{
    if (NULL != obj->module->finalize) {
        obj->module->finalize();
    }
}

OBJ_CLASS_INSTANCE(orte_sensor_base_selected_pair_t,
                   opal_list_item_t,
                   construct, destruct);

/* base functions */
static void start(void);
static void stop(void);

/*
 * Global variables
 */
int orte_sensor_base_output = -1;
orte_sensor_base_API_module_t orte_sensor = {
    start,
    stop
};
opal_list_t mca_sensor_base_components_available;
opal_list_t orte_sensor_base_selected_modules;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_sensor_base_open(void)
{
    /* Debugging / verbose output.  Always have stream open, with
       verbose set by the mca open system... */
    orte_sensor_base_output = opal_output_open(NULL);
    
    /* construct the list of modules */
    OBJ_CONSTRUCT(&orte_sensor_base_selected_modules, opal_list_t);
    
    /* Open up all available components */

    if (ORTE_SUCCESS !=
        mca_base_components_open("sensor", orte_sensor_base_output,
                                 mca_sensor_base_static_components,
                                 &mca_sensor_base_components_available, true)) {
        return ORTE_ERROR;
    }

    /* All done */

    return ORTE_SUCCESS;
}

static void start(void)
{
    orte_sensor_base_selected_pair_t *pair;
    opal_list_item_t *item;
    
    for (item = opal_list_get_first(&orte_sensor_base_selected_modules);
         opal_list_get_end(&orte_sensor_base_selected_modules) != item;
         item = opal_list_get_next(item)) {
        pair = (orte_sensor_base_selected_pair_t*)item;
        if (NULL != pair->module->start) {
            pair->module->start();
        }
    }
    return;    
}

static void stop(void)
{
    orte_sensor_base_selected_pair_t *pair;
    opal_list_item_t *item;
    
    for (item = opal_list_get_first(&orte_sensor_base_selected_modules);
         opal_list_get_end(&orte_sensor_base_selected_modules) != item;
         item = opal_list_get_next(item)) {
        pair = (orte_sensor_base_selected_pair_t*)item;
        if (NULL != pair->module->stop) {
            pair->module->stop();
        }
    }
    return;
}
