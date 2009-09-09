/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"

#include "sensor_pru.h"

/*
 * Local functions
 */

static int orte_sensor_pru_open(void);
static int orte_sensor_pru_close(void);
static int orte_sensor_pru_query(mca_base_module_t **module, int *priority);

orte_sensor_pru_component_t mca_sensor_pru_component = {
    {
        {
            ORTE_SENSOR_BASE_VERSION_1_0_0,
            
            "pru", /* MCA component name */
            ORTE_MAJOR_VERSION,  /* MCA component major version */
            ORTE_MINOR_VERSION,  /* MCA component minor version */
            ORTE_RELEASE_VERSION,  /* MCA component release version */
            orte_sensor_pru_open,  /* component open  */
            orte_sensor_pru_close, /* component close */
            orte_sensor_pru_query  /* component query */
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
static int orte_sensor_pru_open(void)
{
    mca_base_component_t *c = &mca_sensor_pru_component.super.base_version;

    /* lookup parameters */
    mca_base_param_reg_int(c, "sample_rate",
                              "Sample rate in seconds (default=10)",
                              false, false, 10,  &mca_sensor_pru_component.sample_rate);
    
    mca_base_param_reg_int(c, "memory_limit",
                           "Max virtual memory size in GBytes (default=10)",
                           false, false, 10,  &mca_sensor_pru_component.sample_rate);
    
    return ORTE_SUCCESS;
}


static int orte_sensor_pru_query(mca_base_module_t **module, int *priority)
{    
    *priority = 0;  /* select only if specified */
    *module = (mca_base_module_t *)&orte_sensor_pru_module;
    
    return ORTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int orte_sensor_pru_close(void)
{
    return ORTE_SUCCESS;
}
