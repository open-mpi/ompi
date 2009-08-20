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

#include "fddp_trend.h"

/*
 * Local functions
 */

static int orte_fddp_trend_open(void);
static int orte_fddp_trend_close(void);
static int orte_fddp_trend_query(mca_base_module_t **module, int *priority);

orte_fddp_trend_component_t mca_fddp_trend_component = {
    {
        {
            ORTE_FDDP_BASE_VERSION_1_0_0,
            
            "trend", /* MCA component name */
            ORTE_MAJOR_VERSION,  /* MCA component major version */
            ORTE_MINOR_VERSION,  /* MCA component minor version */
            ORTE_RELEASE_VERSION,  /* MCA component release version */
            orte_fddp_trend_open,  /* component open  */
            orte_fddp_trend_close, /* component close */
            orte_fddp_trend_query  /* component query */
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
static int orte_fddp_trend_open(void)
{
    mca_base_component_t *c = &mca_fddp_trend_component.super.base_version;

    /* lookup parameters */
    mca_base_param_reg_int(c, "window_size",
                              "Size of sliding window to smooth data for trend [default: 1]",
                              false, false, 80,  &mca_fddp_trend_component.window_size);

    return ORTE_SUCCESS;
}


static int orte_fddp_trend_query(mca_base_module_t **module, int *priority)
{    
    *priority = 0;  /* select only if specified */
    *module = (mca_base_module_t *)&orte_fddp_trend_module;
    
    return ORTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int orte_fddp_trend_close(void)
{    
    return ORTE_SUCCESS;
}

