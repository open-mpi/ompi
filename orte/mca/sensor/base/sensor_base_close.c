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

#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orte/mca/sensor/base/base.h"


int orte_sensor_base_close(void)
{
    opal_list_item_t *item;
    
    /* destruct the list of modules so they each can finalize */
    while (NULL != (item = opal_list_remove_first(&orte_sensor_base_selected_modules))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_sensor_base_selected_modules);
    
    /* Close all remaining available components */
    
    mca_base_components_close(orte_sensor_base_output, 
                              &mca_sensor_base_components_available, NULL);
    
    /* All done */
    
    return ORTE_SUCCESS;
}
