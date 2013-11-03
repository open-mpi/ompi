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
#include "opal/class/opal_pointer_array.h"

#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"

#include "sensor_file.h"

/*
 * Local functions
 */
static int orte_sensor_file_register (void);
static int orte_sensor_file_open(void);
static int orte_sensor_file_close(void);
static int orte_sensor_file_query(mca_base_module_t **module, int *priority);

orte_sensor_file_component_t mca_sensor_file_component = {
    {
        {
            ORTE_SENSOR_BASE_VERSION_1_0_0,
            
            "file", /* MCA component name */
            ORTE_MAJOR_VERSION,  /* MCA component major version */
            ORTE_MINOR_VERSION,  /* MCA component minor version */
            ORTE_RELEASE_VERSION,  /* MCA component release version */
            orte_sensor_file_open,  /* component open  */
            orte_sensor_file_close, /* component close */
            orte_sensor_file_query,  /* component query */
            orte_sensor_file_register
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};


/**
  * component register/open/close/init function
  */
static int orte_sensor_file_register (void)
{
    mca_base_component_t *c = &mca_sensor_file_component.super.base_version;

    /* lookup parameters */
    mca_sensor_file_component.file = NULL;
    (void) mca_base_component_var_register (c, "filename", "File to be monitored",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_ALL_EQ,
                                            &mca_sensor_file_component.file);

    mca_sensor_file_component.check_size = false;
    (void) mca_base_component_var_register (c, "check_size", "Check the file size",
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_ALL_EQ,
                                            &mca_sensor_file_component.check_size);

    mca_sensor_file_component.check_access = false;
    (void) mca_base_component_var_register (c, "check_access", "Check access time",
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_ALL_EQ,
                                            &mca_sensor_file_component.check_access);

    mca_sensor_file_component.check_mod = false;
    (void) mca_base_component_var_register (c, "check_mod", "Check modification time",
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_ALL_EQ,
                                            &mca_sensor_file_component.check_mod);

    mca_sensor_file_component.limit = 3;
    (void) mca_base_component_var_register (c, "limit",
                                            "Number of times the sensor can detect no motion before declaring error (default=3)",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_ALL_EQ,
                                            &mca_sensor_file_component.limit);
    return ORTE_SUCCESS;
}

static int orte_sensor_file_open(void)
{
    return ORTE_SUCCESS;
}


static int orte_sensor_file_query(mca_base_module_t **module, int *priority)
{
    *priority = 20;  /* higher than heartbeat */
    *module = (mca_base_module_t *)&orte_sensor_file_module;
    return ORTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int orte_sensor_file_close(void)
{
    return ORTE_SUCCESS;
}
