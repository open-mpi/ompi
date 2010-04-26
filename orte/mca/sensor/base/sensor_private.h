/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
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

#include "opal/dss/dss_types.h"

#include "orte/mca/sensor/sensor_types.h"


/*
 * Global functions for MCA overall collective open and close
 */
BEGIN_C_DECLS

#if !ORTE_DISABLE_FULL_SUPPORT

/* define a struct to hold framework-global values */
typedef struct {
    int output;
    opal_pointer_array_t modules;
} orte_sensor_base_t;

ORTE_DECLSPEC extern orte_sensor_base_t orte_sensor_base;

#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS
#endif
