/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef ORTE_MCA_SENSOR_TYPES_H
#define ORTE_MCA_SENSOR_TYPES_H

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/dss/dss_types.h"

/*
 * General SENSOR types - instanced in runtime/orte_globals.c
 */

BEGIN_C_DECLS

enum {
    ORTE_SENSOR_SCALE_LINEAR,
    ORTE_SENSOR_SCALE_LOG,
    ORTE_SENSOR_SCALE_SIGMOID
};

/*
 * Structure for passing data from sensors
 */
typedef struct {
    opal_object_t super;
    char *sensor;
    struct timeval timestamp;
    opal_byte_object_t data;
} orte_sensor_data_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_sensor_data_t);

END_C_DECLS

#endif
