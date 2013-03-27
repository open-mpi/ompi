/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_SENSOR_BASE_H
#define MCA_SENSOR_BASE_H

/*
 * includes
 */
#include "orte_config.h"

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"

#include "orte/mca/sensor/sensor.h"

BEGIN_C_DECLS

/*
 * MCA Framework
 */
ORTE_DECLSPEC extern mca_base_framework_t orte_sensor_base_framework;
/* select a component */
ORTE_DECLSPEC int orte_sensor_base_select(void);


END_C_DECLS
#endif
