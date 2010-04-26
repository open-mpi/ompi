/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/base/sensor_private.h"

int orte_sensor_base_close(void)
{
    orte_sensor_base_module_t *i_module;
    int i;
    
    for (i=0; i < orte_sensor_base.modules.size; i++) {
        if (NULL == (i_module = (orte_sensor_base_module_t*)opal_pointer_array_get_item(&orte_sensor_base.modules, i))) {
            continue;
        }
        if (NULL != i_module->finalize) {
            i_module->finalize();
        }
    }
    OBJ_DESTRUCT(&orte_sensor_base.modules);
    
    /* Close all remaining available components */
    
    mca_base_components_close(orte_sensor_base.output, 
                              &mca_sensor_base_components_available, NULL);
    
    /* All done */
    
    return ORTE_SUCCESS;
}
