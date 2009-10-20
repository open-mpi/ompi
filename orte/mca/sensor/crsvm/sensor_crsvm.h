/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * Process Resource Utilization sensor 
 */
#ifndef ORTE_SENSOR_CRS_H
#define ORTE_SENSOR_CRS_H

#include "orte_config.h"

#include "orte/mca/sensor/sensor.h"

BEGIN_C_DECLS

struct orte_sensor_crsvm_component_t {
    orte_sensor_base_component_t super;
    int sample_rate;
    int celsius_limit;
};
typedef struct orte_sensor_crsvm_component_t orte_sensor_crsvm_component_t;

ORTE_MODULE_DECLSPEC extern orte_sensor_crsvm_component_t mca_sensor_crsvm_component;
extern orte_sensor_base_module_t orte_sensor_crsvm_module;


END_C_DECLS

#endif
