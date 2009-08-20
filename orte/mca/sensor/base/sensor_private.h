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
/*
 * function definitions
 */
ORTE_DECLSPEC    int orte_sensor_scale_data(orte_sensor_data_t *target, int num_values, float *data);

#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS
#endif
