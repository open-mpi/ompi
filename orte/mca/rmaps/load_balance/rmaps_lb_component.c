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
#include "opal/mca/paffinity/paffinity.h"

#include "orte/mca/rmaps/base/base.h"
#include "rmaps_lb.h"

/*
 * Local functions
 */

static int orte_rmaps_lb_open(void);
static int orte_rmaps_lb_close(void);
static int orte_rmaps_lb_query(mca_base_module_t **module, int *priority);

static int my_priority;

orte_rmaps_lb_component_t mca_rmaps_load_balance_component = {
    {
        {
            ORTE_RMAPS_BASE_VERSION_2_0_0,
        
            "load_balance", /* MCA component name */
            ORTE_MAJOR_VERSION,  /* MCA component major version */
            ORTE_MINOR_VERSION,  /* MCA component minor version */
            ORTE_RELEASE_VERSION,  /* MCA component release version */
            orte_rmaps_lb_open,  /* component open  */
            orte_rmaps_lb_close, /* component close */
            orte_rmaps_lb_query  /* component query */
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
static int orte_rmaps_lb_open(void)
{
    mca_base_component_t *c = &mca_rmaps_load_balance_component.super.base_version;
    int value, tmp;

    /* initialize */
    mca_rmaps_load_balance_component.npernode = 0;
    mca_rmaps_load_balance_component.nperboard = 0;
    mca_rmaps_load_balance_component.npersocket = 0;

    mca_base_param_reg_int(c, "priority",
                           "Priority of the loadbalance rmaps component",
                           false, false, 80,
                           &my_priority);

    /* check for procs/xxx directives */
    tmp = mca_base_param_reg_int(c, "pernode",
                           "Launch one ppn as directed",
                           false, false, (int)false, NULL);
    mca_base_param_reg_syn_name(tmp, "rmaps", "base_pernode", false);
    mca_base_param_lookup_int(tmp, &value);
    if (value) {
        mca_rmaps_load_balance_component.npernode = 1;
    }
 
    /* #procs/node */
    tmp = mca_base_param_reg_int(c, "n_pernode",
                           "Launch n procs/node",
                           false, false, mca_rmaps_load_balance_component.npernode, NULL);
    mca_base_param_reg_syn_name(tmp, "rmaps", "base_n_pernode", false);
    mca_base_param_lookup_int(tmp, &mca_rmaps_load_balance_component.npernode);
    
    /* #procs/board */
    tmp = mca_base_param_reg_int(c, "n_perboard",
                           "Launch n procs/board",
                           false, false, -1, NULL);
    mca_base_param_reg_syn_name(tmp, "rmaps", "base_n_perboard", false);
    mca_base_param_lookup_int(tmp, &mca_rmaps_load_balance_component.nperboard);
    if (0 < mca_rmaps_load_balance_component.nperboard) {
        ORTE_ADD_MAPPING_POLICY(ORTE_MAPPING_NPERXXX);
    }

    /* #procs/socket */
    tmp = mca_base_param_reg_int(c, "n_persocket",
                           "Launch n procs/socket",
                           false, false, -1, NULL);
    mca_base_param_reg_syn_name(tmp, "rmaps", "base_n_persocket", false);
    mca_base_param_lookup_int(tmp, &mca_rmaps_load_balance_component.npersocket);
    if (0 < mca_rmaps_load_balance_component.npersocket) {
        ORTE_ADD_MAPPING_POLICY(ORTE_MAPPING_NPERXXX);
        /* force bind to socket if not overridden by user */
        ORTE_XSET_BINDING_POLICY(ORTE_BIND_TO_SOCKET);
    }
    
    return ORTE_SUCCESS;
}


static int orte_rmaps_lb_query(mca_base_module_t **module, int *priority)
{
    /* after rr, unless lb values are set */
    if (0 < mca_rmaps_load_balance_component.npernode ||
        0 < mca_rmaps_load_balance_component.nperboard ||
        0 < mca_rmaps_load_balance_component.npersocket) {
        my_priority = 10000;
    }
    *priority = my_priority;
    *module = (mca_base_module_t *)&orte_rmaps_load_balance_module;
    return ORTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int orte_rmaps_lb_close(void)
{
    return ORTE_SUCCESS;
}


