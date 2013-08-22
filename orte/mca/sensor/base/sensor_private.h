/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_SENSOR_PRIVATE_H
#define MCA_SENSOR_PRIVATE_H

/*
 * includes
 */
#include "orte_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */

#include "opal/class/opal_pointer_array.h"
#include "opal/mca/event/event.h"

#include "orte/runtime/orte_globals.h"

#include "orte/mca/sensor/sensor.h"


/*
 * Global functions for MCA overall collective open and close
 */
BEGIN_C_DECLS

/* define a struct to hold framework-global values */
typedef struct {
    opal_pointer_array_t modules;
    orte_proc_t *my_proc;
    orte_node_t *my_node;
    bool log_samples;
    bool active;
    struct timeval rate;
    opal_event_t sample_ev;
    opal_buffer_t *samples;
} orte_sensor_base_t;

typedef struct {
    opal_object_t super;
    mca_base_component_t *component;
    orte_sensor_base_module_t *module;
    int priority;
} orte_sensor_active_module_t;
OBJ_CLASS_DECLARATION(orte_sensor_active_module_t);


ORTE_DECLSPEC extern orte_sensor_base_t orte_sensor_base;
ORTE_DECLSPEC void orte_sensor_base_start(orte_jobid_t job);
ORTE_DECLSPEC void orte_sensor_base_stop(orte_jobid_t job);
ORTE_DECLSPEC void orte_sensor_base_sample(int fd, short args, void *cbdata);
ORTE_DECLSPEC void orte_sensor_base_log(char *comp, opal_buffer_t *data);

END_C_DECLS
#endif
