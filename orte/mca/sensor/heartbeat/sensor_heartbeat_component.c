/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"

#include "sensor_heartbeat.h"

/*
 * Local functions
 */

static int orte_sensor_heartbeat_open(void);
static int orte_sensor_heartbeat_close(void);
static int orte_sensor_heartbeat_query(mca_base_module_t **module, int *priority);

orte_sensor_heartbeat_component_t mca_sensor_heartbeat_component = {
    {
        {
            ORTE_SENSOR_BASE_VERSION_1_0_0,
            
            "heartbeat", /* MCA component name */
            ORTE_MAJOR_VERSION,  /* MCA component major version */
            ORTE_MINOR_VERSION,  /* MCA component minor version */
            ORTE_RELEASE_VERSION,  /* MCA component release version */
            orte_sensor_heartbeat_open,  /* component open  */
            orte_sensor_heartbeat_close, /* component close */
            orte_sensor_heartbeat_query  /* component query */
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};


/**
  * component open/close/init function
  */
static int orte_sensor_heartbeat_open(void)
{
    mca_base_component_t *c = &mca_sensor_heartbeat_component.super.base_version;
    int tmp;

    /* lookup parameters */
    mca_base_param_reg_string(c, "rate",
                           "Heartbeat rate in sec (default=0:0)",
                           false, false, "0:0",  &mca_sensor_heartbeat_component.rate);
    
    mca_base_param_reg_string(c, "check",
                           "Check for failure rate in sec:usec (default=1:0)",
                           false, false, "1:0",  &mca_sensor_heartbeat_component.check);
    
    mca_base_param_reg_int(c, "missed",
                           "Number of missed heartbeat checks before failure is declared (default=2)",
                           false, false, 2,  &tmp);
    mca_sensor_heartbeat_component.missed = tmp;
    
    return ORTE_SUCCESS;
}


static int orte_sensor_heartbeat_query(mca_base_module_t **module, int *priority)
{
    *priority = 10;  /* use if we were built */
    *module = (mca_base_module_t *)&orte_sensor_heartbeat_module;
    return ORTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int orte_sensor_heartbeat_close(void)
{
    return ORTE_SUCCESS;
}
