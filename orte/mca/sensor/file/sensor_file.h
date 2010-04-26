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
 * File movement sensor 
 */
#ifndef ORTE_SENSOR_FILE_H
#define ORTE_SENSOR_FILE_H

#include "orte_config.h"

#include "orte/mca/sensor/sensor.h"

BEGIN_C_DECLS

struct orte_sensor_file_component_t {
    orte_sensor_base_component_t super;
    int sample_rate;
    char *file;
    bool check_size;
    bool check_access;
    bool check_mod;
    int limit;
};
typedef struct orte_sensor_file_component_t orte_sensor_file_component_t;

ORTE_MODULE_DECLSPEC extern orte_sensor_file_component_t mca_sensor_file_component;
extern orte_sensor_base_module_t orte_sensor_file_module;


END_C_DECLS

#endif
