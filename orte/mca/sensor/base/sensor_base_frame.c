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

#if !ORTE_DISABLE_FULL_SUPPORT

/* base functions */
static void start(orte_jobid_t jobid);
static void stop(orte_jobid_t jobid);

/*
 * Global variables
 */
orte_sensor_base_API_module_t orte_sensor = {
    start,
    stop
};

#endif

orte_sensor_base_t orte_sensor_base;

static int orte_sensor_base_close(void)
{
#if !ORTE_DISABLE_FULL_SUPPORT
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
    return mca_base_framework_components_close(&orte_sensor_base_framework, NULL);
#else
    return ORTE_SUCCESS;
#endif
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int orte_sensor_base_open(mca_base_open_flag_t flags)
{
#if !ORTE_DISABLE_FULL_SUPPORT

    /* initialize pointers */
    orte_sensor_base.my_proc = NULL;
    orte_sensor_base.my_node = NULL;

    /* construct the array of modules */
    OBJ_CONSTRUCT(&orte_sensor_base.modules, opal_pointer_array_t);
    opal_pointer_array_init(&orte_sensor_base.modules, 3, INT_MAX, 1);
    
    /* Open up all available components */
    return mca_base_framework_components_open(&orte_sensor_base_framework, flags);
#else
    return ORTE_SUCCESS;
#endif
}

MCA_BASE_FRAMEWORK_DECLARE(orte, sensor, "ORTE Monitoring Sensors", NULL,
                           orte_sensor_base_open, orte_sensor_base_close,
                           mca_sensor_base_static_components, 0);

#if !ORTE_DISABLE_FULL_SUPPORT

static void start(orte_jobid_t jobid)
{
    orte_sensor_base_module_t *i_module;
    int i;
    
    /* call the start function of all modules in priority order from
     * highest to lowest
     */
    for (i=0; i < orte_sensor_base.modules.size; i++) {
        if (NULL == (i_module = (orte_sensor_base_module_t*)opal_pointer_array_get_item(&orte_sensor_base.modules, i))) {
            continue;
        }
        if (NULL != i_module->start) {
            i_module->start(jobid);
        }
    }
    return;    
}

static void stop(orte_jobid_t jobid)
{
    orte_sensor_base_module_t *i_module;
    int i;
    
    /* call the stop function of all modules in priority order from
     * highest to lowest
     */
    for (i=0; i < orte_sensor_base.modules.size; i++) {
        if (NULL == (i_module = (orte_sensor_base_module_t*)opal_pointer_array_get_item(&orte_sensor_base.modules, i))) {
            continue;
        }
        if (NULL != i_module->stop) {
            i_module->stop(jobid);
        }
    }
    return;
}

#endif
