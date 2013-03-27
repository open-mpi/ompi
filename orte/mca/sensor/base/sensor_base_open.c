/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
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
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/class/opal_pointer_array.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/base/sensor_private.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/sensor/base/static-components.h"

/*
 * Global variables
 */
orte_sensor_base_t orte_sensor_base;
opal_list_t mca_sensor_base_components_available;
orte_sensor_base_API_module_t orte_sensor = {
    orte_sensor_base_start,
    orte_sensor_base_stop
};

/*
 * Local variables
 */
static int orte_sensor_base_sample_rate = 0;

static int orte_sensor_base_register(int flags)
{
    int var_id;

    orte_sensor_base_sample_rate = 0;
    var_id = mca_base_var_register("orte", "sensor", "base", "sample_rate",
                                   "Sample rate in seconds",
                                   MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                   OPAL_INFO_LVL_9,
                                   MCA_BASE_VAR_SCOPE_READONLY,
                                   &orte_sensor_base_sample_rate);
    mca_base_var_register_synonym(var_id, "orte", "sensor", NULL, "sample_rate",
                                  MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
  
    /* see if we want samples logged */
    orte_sensor_base.log_samples = false;
    var_id = mca_base_var_register("orte", "sensor", "base", "log_samples",
                                   "Log samples to database",
                                   MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                   OPAL_INFO_LVL_9,
                                   MCA_BASE_VAR_SCOPE_READONLY,
                                   &orte_sensor_base.log_samples);
    mca_base_var_register_synonym(var_id, "orte", "sensor", NULL, "log_samples",
                                  MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    return ORTE_SUCCESS;
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_sensor_base_open(void)
{
    (void) orte_sensor_base_register(0);

    /* Debugging / verbose output.  Always have stream open, with
       verbose set by the mca open system... */
    orte_sensor_base.output = opal_output_open(NULL);

    /* initialize pointers */
    orte_sensor_base.my_proc = NULL;
    orte_sensor_base.my_node = NULL;
    orte_sensor_base.active = false;

    /* construct the array of modules */
    OBJ_CONSTRUCT(&orte_sensor_base.modules, opal_pointer_array_t);
    opal_pointer_array_init(&orte_sensor_base.modules, 3, INT_MAX, 1);
    
    /* get the sample rate */
    orte_sensor_base.rate.tv_sec = orte_sensor_base_sample_rate;
    orte_sensor_base.rate.tv_usec = 0;

    /* Open up all available components */

    if (ORTE_SUCCESS !=
        mca_base_components_open("sensor", orte_sensor_base.output,
                                 mca_sensor_base_static_components,
                                 &mca_sensor_base_components_available, true)) {
        return ORTE_ERROR;
    }

    /* All done */

    return ORTE_SUCCESS;
}

OBJ_CLASS_INSTANCE(orte_sensor_active_module_t,
                   opal_object_t,
                   NULL, NULL);
