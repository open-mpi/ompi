/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/dss/dss.h"
#include "opal/mca/event/event.h"

#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/base/sensor_private.h"

static bool mods_active = false;

void orte_sensor_base_start(orte_jobid_t job)
{
    orte_sensor_active_module_t *i_module;
    int i;

    if (0 < orte_sensor_base.rate.tv_sec) {
        opal_output_verbose(5, orte_sensor_base_framework.framework_output,
                            "%s sensor:base: starting sensors",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        /* call the start function of all modules in priority order */
        for (i=0; i < orte_sensor_base.modules.size; i++) {
            if (NULL == (i_module = (orte_sensor_active_module_t*)opal_pointer_array_get_item(&orte_sensor_base.modules, i))) {
                continue;
            }
            mods_active = true;
            if (NULL != i_module->module->start) {
                i_module->module->start(job);
            }
        }

        if (mods_active && !orte_sensor_base.active) {
            /* setup a buffer to collect samples */
            orte_sensor_base.samples = OBJ_NEW(opal_buffer_t);
            /* startup a timer to wake us up periodically
             * for a data sample
             */
            orte_sensor_base.active = true;
            opal_event_evtimer_set(orte_event_base, &orte_sensor_base.sample_ev,
                                   orte_sensor_base_sample, NULL);
            opal_event_evtimer_add(&orte_sensor_base.sample_ev, &orte_sensor_base.rate);
        }
    }
    return;    
}

void orte_sensor_base_stop(orte_jobid_t job)
{
    orte_sensor_active_module_t *i_module;
    int i;

    if (!mods_active) {
        return;
    }

    opal_output_verbose(5, orte_sensor_base_framework.framework_output,
                        "%s sensor:base: stopping sensors",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    if (orte_sensor_base.active) {
        opal_event_del(&orte_sensor_base.sample_ev);
        orte_sensor_base.active = false;
    }

    /* call the stop function of all modules in priority order */
    for (i=0; i < orte_sensor_base.modules.size; i++) {
        if (NULL == (i_module = (orte_sensor_active_module_t*)opal_pointer_array_get_item(&orte_sensor_base.modules, i))) {
            continue;
        }
        if (NULL != i_module->module->stop) {
            i_module->module->stop(job);
        }
    }

    return;
}

void orte_sensor_base_sample(int fd, short args, void *cbdata)
{
    orte_sensor_active_module_t *i_module;
    int i;
    
    if (!mods_active) {
        return;
    }

    /* see if we were ordered to stop */
    if (!orte_sensor_base.active) {
        return;
    }

    opal_output_verbose(5, orte_sensor_base_framework.framework_output,
                        "%s sensor:base: sampling sensors",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* call the sample function of all modules in priority order from
     * highest to lowest - the heartbeat should always be the lowest
     * priority, so it will send any collected data
     */
    for (i=0; i < orte_sensor_base.modules.size; i++) {
        if (NULL == (i_module = (orte_sensor_active_module_t*)opal_pointer_array_get_item(&orte_sensor_base.modules, i))) {
            continue;
        }
        if (NULL != i_module->module->sample) {
            opal_output_verbose(5, orte_sensor_base_framework.framework_output,
                                "%s sensor:base: sampling component %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                i_module->component->mca_component_name);
            i_module->module->sample();
        }
    }

    /* restart the timer */
    opal_event_evtimer_add(&orte_sensor_base.sample_ev, &orte_sensor_base.rate);

    return;
}

void orte_sensor_base_log(char *comp, opal_buffer_t *data)
{
    int i;
    orte_sensor_active_module_t *i_module;

    if (NULL == comp) {
        /* nothing we can do */
        return;
    }

    opal_output_verbose(5, orte_sensor_base_framework.framework_output,
                        "%s sensor:base: logging sensor %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), comp);

    /* find the specified module  */
    for (i=0; i < orte_sensor_base.modules.size; i++) {
        if (NULL == (i_module = (orte_sensor_active_module_t*)opal_pointer_array_get_item(&orte_sensor_base.modules, i))) {
            continue;
        }
        if (0 == strcmp(comp, i_module->component->mca_component_name)) {
            if (NULL != i_module->module->log) {
                i_module->module->log(data);
            }
            return;
        }
    }
}
