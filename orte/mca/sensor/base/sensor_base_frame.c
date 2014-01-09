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
orte_sensor_base_API_module_t orte_sensor = {
    orte_sensor_base_start,
    orte_sensor_base_stop
};
orte_sensor_base_t orte_sensor_base;

/*
 * Local variables
 */
static int orte_sensor_base_sample_rate = 0;

static int orte_sensor_base_register(mca_base_register_flag_t flags)
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

static int orte_sensor_base_close(void)
{
    orte_sensor_active_module_t *i_module;
    int i;
    
    for (i=0; i < orte_sensor_base.modules.size; i++) {
        if (NULL == (i_module = (orte_sensor_active_module_t*)opal_pointer_array_get_item(&orte_sensor_base.modules, i))) {
            continue;
        }
        if (NULL != i_module->module->finalize) {
            i_module->module->finalize();
        }
    }
    OBJ_DESTRUCT(&orte_sensor_base.modules);
    
    /* Close all remaining available components */
    return mca_base_framework_components_close(&orte_sensor_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int orte_sensor_base_open(mca_base_open_flag_t flags)
{
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
    return mca_base_framework_components_open(&orte_sensor_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(orte, sensor, "ORTE Monitoring Sensors",
                           orte_sensor_base_register,
                           orte_sensor_base_open, orte_sensor_base_close,
                           mca_sensor_base_static_components, 0);

OBJ_CLASS_INSTANCE(orte_sensor_active_module_t,
                   opal_object_t,
                   NULL, NULL);
