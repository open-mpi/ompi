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

#include "sensor_ft_tester.h"

/*
 * Local functions
 */

static int orte_sensor_ft_tester_open(void);
static int orte_sensor_ft_tester_close(void);
static int orte_sensor_ft_tester_query(mca_base_module_t **module, int *priority);

orte_sensor_ft_tester_component_t mca_sensor_ft_tester_component = {
    {
        {
            ORTE_SENSOR_BASE_VERSION_1_0_0,
            
            "ft_tester", /* MCA component name */
            ORTE_MAJOR_VERSION,  /* MCA component major version */
            ORTE_MINOR_VERSION,  /* MCA component minor version */
            ORTE_RELEASE_VERSION,  /* MCA component release version */
            orte_sensor_ft_tester_open,  /* component open  */
            orte_sensor_ft_tester_close, /* component close */
            orte_sensor_ft_tester_query  /* component query */
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
static int orte_sensor_ft_tester_open(void)
{
    mca_base_component_t *c = &mca_sensor_ft_tester_component.super.base_version;
    int tmp;
    char *str;

    /* lookup parameters */
    mca_base_param_reg_int(c, "fail_rate",
                           "Time between checks to decide if one or more procs shall be killed, expressed as sec",
                           false, false, 0,  &tmp);
    if (tmp < 0) {
        opal_output(0, "Illegal value %d - must be >= 0", tmp);
        return ORTE_ERR_FATAL;
    }
    mca_sensor_ft_tester_component.fail_rate = tmp;
    
    mca_base_param_reg_string(c, "fail_prob",
                              "Probability of killing a single executable",
                              false, false, "30.0",  &str);
    if (NULL != str) {
        mca_sensor_ft_tester_component.fail_prob = strtof(str, NULL);
        if (1.0 < mca_sensor_ft_tester_component.fail_prob) {
            /* given in percent */
            mca_sensor_ft_tester_component.fail_prob /= 100.0;
        }
    } else {
        mca_sensor_ft_tester_component.fail_prob = 0.0;
    }

    mca_base_param_reg_int(c, "multi_allowed",
                           "Allow multiple executables to be killed at one time",
                           false, false, 0,  &tmp);
    mca_sensor_ft_tester_component.multi_fail = OPAL_INT_TO_BOOL(tmp);
    
    mca_base_param_reg_string(c, "daemon_fail_prob",
                              "Probability of killing a daemon",
                              false, false, "0.0", &str);
    if (NULL != str) {
        mca_sensor_ft_tester_component.daemon_fail_prob = strtof(str, NULL);
        if (1.0 < mca_sensor_ft_tester_component.daemon_fail_prob) {
            /* given in percent */
            mca_sensor_ft_tester_component.daemon_fail_prob /= 100.0;
        }
    } else {
        mca_sensor_ft_tester_component.daemon_fail_prob = 0.0;
    }
    
    return ORTE_SUCCESS;
}


static int orte_sensor_ft_tester_query(mca_base_module_t **module, int *priority)
{
    if (0 == mca_sensor_ft_tester_component.fail_rate) {
        *priority = 0;
        *module = NULL;
        return ORTE_ERROR;
    }

    *priority = 1;  /* at the bottom */
    *module = (mca_base_module_t *)&orte_sensor_ft_tester_module;
    
    return ORTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int orte_sensor_ft_tester_close(void)
{
    return ORTE_SUCCESS;
}
