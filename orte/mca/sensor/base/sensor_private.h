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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */

#include "opal/class/opal_pointer_array.h"

#include "orte/runtime/orte_globals.h"

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
    orte_proc_t *my_proc;
    orte_node_t *my_node;
} orte_sensor_base_t;

ORTE_DECLSPEC extern orte_sensor_base_t orte_sensor_base;

#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS
#endif
