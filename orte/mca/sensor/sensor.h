/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * @file:
 *
 */

#ifndef MCA_SENSOR_H
#define MCA_SENSOR_H

/*
 * includes
 */

#include "orte_config.h"
#include "orte/types.h"

#include "opal/mca/mca.h"

BEGIN_C_DECLS

/*
 * Component functions - all MUST be provided!
 */

/* start collecting data */
typedef void (*orte_sensor_API_module_start_fn_t)(orte_jobid_t job);

/* stop collecting data */
typedef void (*orte_sensor_API_module_stop_fn_t)(orte_jobid_t job);

/* API module */
/*
 * Ver 1.0
 */
struct orte_sensor_base_API_module_1_0_0_t {
    orte_sensor_API_module_start_fn_t      start;
    orte_sensor_API_module_stop_fn_t       stop;
};

typedef struct orte_sensor_base_API_module_1_0_0_t orte_sensor_base_API_module_1_0_0_t;
typedef orte_sensor_base_API_module_1_0_0_t orte_sensor_base_API_module_t;

/* initialize the module */
typedef int (*orte_sensor_base_module_init_fn_t)(void);
    
/* finalize the module */
typedef void (*orte_sensor_base_module_finalize_fn_t)(void);

/* tell the module to sample its sensor */
typedef void (*orte_sensor_base_module_sample_fn_t)(void);

/* pass a buffer to the module for logging */
typedef void (*orte_sensor_base_module_log_fn_t)(opal_buffer_t *sample);

/*
 * Component modules Ver 1.0
 */
struct orte_sensor_base_module_1_0_0_t {
    orte_sensor_base_module_init_fn_t       init;
    orte_sensor_base_module_finalize_fn_t   finalize;
    orte_sensor_API_module_start_fn_t       start;
    orte_sensor_API_module_stop_fn_t        stop;
    orte_sensor_base_module_sample_fn_t     sample;
    orte_sensor_base_module_log_fn_t        log;
};

typedef struct orte_sensor_base_module_1_0_0_t orte_sensor_base_module_1_0_0_t;
typedef orte_sensor_base_module_1_0_0_t orte_sensor_base_module_t;

/*
 * the standard component data structure
 */
struct orte_sensor_base_component_1_0_0_t {
    mca_base_component_t base_version;
    mca_base_component_data_t base_data;
};
typedef struct orte_sensor_base_component_1_0_0_t orte_sensor_base_component_1_0_0_t;
typedef orte_sensor_base_component_1_0_0_t orte_sensor_base_component_t;



/*
 * Macro for use in components that are of type sensor v1.0.0
 */
#define ORTE_SENSOR_BASE_VERSION_1_0_0 \
  /* sensor v1.0 is chained to MCA v2.0 */ \
  MCA_BASE_VERSION_2_0_0, \
  /* sensor v1.0 */ \
  "sensor", 1, 0, 0

/* Global structure for accessing sensor functions
 */
ORTE_DECLSPEC extern orte_sensor_base_API_module_t orte_sensor;  /* holds API function pointers */

END_C_DECLS

#endif /* MCA_SENSOR_H */
