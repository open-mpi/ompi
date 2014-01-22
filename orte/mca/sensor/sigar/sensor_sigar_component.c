/*
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_var.h"

#include "orte/mca/sensor/base/sensor_private.h"
#include "sensor_sigar.h"

/*
 * Local functions
 */

static int orte_sensor_sigar_open(void);
static int orte_sensor_sigar_close(void);
static int orte_sensor_sigar_query(mca_base_module_t **module, int *priority);
static int sigar_component_register(void);

orte_sensor_sigar_component_t mca_sensor_sigar_component = {
    {
        {
            ORTE_SENSOR_BASE_VERSION_1_0_0,
            
            "sigar", /* MCA component name */
            ORTE_MAJOR_VERSION,  /* MCA component major version */
            ORTE_MINOR_VERSION,  /* MCA component minor version */
            ORTE_RELEASE_VERSION,  /* MCA component release version */
            orte_sensor_sigar_open,  /* component open  */
            orte_sensor_sigar_close, /* component close */
            orte_sensor_sigar_query,  /* component query */
            sigar_component_register
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        "procresource,noderesource"
    }
};

/**
  * component open/close/init function
  */
static int orte_sensor_sigar_open(void)
{
    return ORTE_SUCCESS;
}

static int orte_sensor_sigar_query(mca_base_module_t **module, int *priority)
{
    /* if we can build, then we definitely want to be used
     * even if we aren't going to sample as we have to be
     * present in order to log any received results
     */
    *priority = 150;  /* ahead of heartbeat and resusage */
    *module = (mca_base_module_t *)&orte_sensor_sigar_module;
    return ORTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int orte_sensor_sigar_close(void)
{
    return ORTE_SUCCESS;
}

static int sigar_component_register(void)
{
    mca_base_component_t *c = &mca_sensor_sigar_component.super.base_version;

     mca_sensor_sigar_component.test = false;
    (void) mca_base_component_var_register (c, "test",
                                            "Generate and pass test vector",
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            & mca_sensor_sigar_component.test);
    return ORTE_SUCCESS;
}
