/*
 * Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved. 
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

#include "sensor_resusage.h"

/*
 * Local functions
 */

static int orte_sensor_resusage_open(void);
static int orte_sensor_resusage_close(void);
static int orte_sensor_resusage_query(mca_base_module_t **module, int *priority);

orte_sensor_resusage_component_t mca_sensor_resusage_component = {
    {
        {
            ORTE_SENSOR_BASE_VERSION_1_0_0,
            
            "resusage", /* MCA component name */
            ORTE_MAJOR_VERSION,  /* MCA component major version */
            ORTE_MINOR_VERSION,  /* MCA component minor version */
            ORTE_RELEASE_VERSION,  /* MCA component release version */
            orte_sensor_resusage_open,  /* component open  */
            orte_sensor_resusage_close, /* component close */
            orte_sensor_resusage_query  /* component query */
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
static int orte_sensor_resusage_open(void)
{
    mca_base_component_t *c = &mca_sensor_resusage_component.super.base_version;
    int tmp;

    /* lookup parameters */
    mca_base_param_reg_int(c, "sample_rate",
                           "Sample rate in seconds (default=10)",
                           false, false, 0,  &tmp);
    if (tmp < 0) {
        opal_output(0, "Illegal value %d - must be > 0", tmp);
        return ORTE_ERR_FATAL;
    }
    mca_sensor_resusage_component.sample_rate = tmp;
    
    mca_base_param_reg_int(c, "node_memory_limit",
                           "Percentage of total memory that can be in-use",
                           false, false, 0,  &tmp);
    mca_sensor_resusage_component.node_memory_limit = (float)tmp/100.0;
    
    mca_base_param_reg_int(c, "proc_memory_limit",
                           "Max virtual memory size in MBytes",
                           false, false, 0,  &tmp);
    mca_sensor_resusage_component.proc_memory_limit = (float)tmp;
    
    return ORTE_SUCCESS;
}


static int orte_sensor_resusage_query(mca_base_module_t **module, int *priority)
{    
    *priority = 100;  /* ahead of heartbeat */
    *module = (mca_base_module_t *)&orte_sensor_resusage_module;
    
    return ORTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int orte_sensor_resusage_close(void)
{
    return ORTE_SUCCESS;
}
