/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
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
 * Heartbeat sensor 
 */
#ifndef ORTE_SENSOR_HEARTBEAT_H
#define ORTE_SENSOR_HEARTBEAT_H

#include "orte_config.h"

#include "orte/mca/sensor/sensor.h"

BEGIN_C_DECLS

struct orte_sensor_heartbeat_component_t {
    orte_sensor_base_component_t super;
    char *rate;
    char *check;
    bool include_stats;
};
typedef struct orte_sensor_heartbeat_component_t orte_sensor_heartbeat_component_t;

ORTE_MODULE_DECLSPEC extern orte_sensor_heartbeat_component_t mca_sensor_heartbeat_component;
extern orte_sensor_base_module_t orte_sensor_heartbeat_module;


END_C_DECLS

#endif
