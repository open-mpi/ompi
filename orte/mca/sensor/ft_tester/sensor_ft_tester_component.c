/*
 * Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
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

#include "sensor_ft_tester.h"

/*
 * Local functions
 */
static int orte_sensor_ft_tester_register (void);
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
            orte_sensor_ft_tester_query, /* component query */
            orte_sensor_ft_tester_register
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};

static char *daemon_fail_prob = NULL;
static char *fail_prob = NULL;
opal_rng_buff_t orte_sensor_ft_rng_buff;

/**
  * component register/open/close/init function
  */
static int orte_sensor_ft_tester_register (void)
{
    mca_base_component_t *c = &mca_sensor_ft_tester_component.super.base_version;

    fail_prob = NULL;
    (void) mca_base_component_var_register (c, "fail_prob", "Probability of killing a single executable",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &fail_prob);

    mca_sensor_ft_tester_component.multi_fail = false;
    (void) mca_base_component_var_register (c, "multi_allowed", "Allow multiple executables to be killed at one time",
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_sensor_ft_tester_component.multi_fail);

    daemon_fail_prob = NULL;
    (void) mca_base_component_var_register (c, "daemon_fail_prob", "Probability of killing a daemon",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &daemon_fail_prob);

    return ORTE_SUCCESS;
}

static int orte_sensor_ft_tester_open(void)
{
    /* lookup parameters */
    if (NULL != fail_prob) {
        mca_sensor_ft_tester_component.fail_prob = strtof(fail_prob, NULL);
        if (1.0 < mca_sensor_ft_tester_component.fail_prob) {
            /* given in percent */
            mca_sensor_ft_tester_component.fail_prob /= 100.0;
        }
    } else {
        mca_sensor_ft_tester_component.fail_prob = 0.0;
    }

    if (NULL != daemon_fail_prob) {
        mca_sensor_ft_tester_component.daemon_fail_prob = strtof(daemon_fail_prob, NULL);
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
    if (0.0 < mca_sensor_ft_tester_component.fail_prob ||
        0.0 < mca_sensor_ft_tester_component.daemon_fail_prob) {
        *priority = 1;  /* at the bottom */
        *module = (mca_base_module_t *)&orte_sensor_ft_tester_module;
        /* seed the RNG --- Not sure if we should assume all procs use 
         * the same seed? 
         */
        opal_srand(&orte_sensor_ft_rng_buff, (uint32_t) getpid());
        return ORTE_SUCCESS;
    }
    *priority = 0;
    *module = NULL;
    return ORTE_ERROR;

}

/**
 *  Close all subsystems.
 */

static int orte_sensor_ft_tester_close(void)
{
    return ORTE_SUCCESS;
}
