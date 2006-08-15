/* -*- C -*-
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
 *
 */
/**
 * @file:
 * Takes care of the component stuff for the MCA.
 */
#include "orte_config.h"
#include "orte/mca/errmgr/errmgr.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"
#include "pls_bproc.h"

/**
 * The bproc component data structure used to store all the relevent data about
 * this component.
 */
orte_pls_bproc_component_t mca_pls_bproc_component = {
    {
        {
        ORTE_PLS_BASE_VERSION_1_0_0,
        "bproc", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_pls_bproc_component_open,  /* component open */
        orte_pls_bproc_component_close /* component close */
        },
        {
        false /* checkpoint / restart */
        },
        orte_pls_bproc_init    /* component init */ 
    }
};

/**
 * Opens the pls_bproc component, setting all the needed mca parameters and 
 * finishes setting up the component struct.
 */
int orte_pls_bproc_component_open(void) {
    int rc;
    char *policy;
    
    /* init parameters */
    mca_base_component_t *c = &mca_pls_bproc_component.super.pls_version;
    mca_base_param_reg_int(c, "priority", NULL, false, false, 100,
                           &mca_pls_bproc_component.priority);
    mca_base_param_reg_int(c, "debug",
                           "If > 0 prints library debugging information",
                           false, false, 0, &mca_pls_bproc_component.debug);
    mca_base_param_reg_int(c, "terminate_sig",
                           "Signal sent to processes to terminate them", false,
                           false, 9, &mca_pls_bproc_component.terminate_sig);
    mca_base_param_reg_string(c, "orted", "Path to where orted is installed",
                           false, false, "orted", &mca_pls_bproc_component.orted);
    mca_pls_bproc_component.num_procs = 0;
    mca_pls_bproc_component.num_daemons = 0;
    mca_pls_bproc_component.done_launching = false;
    OBJ_CONSTRUCT(&mca_pls_bproc_component.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_pls_bproc_component.condition, opal_condition_t);

    /* we need to know the intended method for mapping processes
     * so we can properly direct the bproc processes on how to compute
     * their name
     */
    mca_base_param_reg_string_name("rmaps", "base_schedule_policy",
                                   "Scheduling Policy for RMAPS. [slot | node]",
                                   false, false, "slot", &policy);
    if (0 == strcmp(policy, "node")) {
        mca_pls_bproc_component.bynode = true;
    } else {
        mca_pls_bproc_component.bynode = false;
    }
    
    /* init the list to hold the daemon names */
    rc = orte_pointer_array_init(&mca_pls_bproc_component.daemon_names, 8, 200000, 8);
    /* init the list to hold the daemon names */
    rc = orte_pointer_array_init(&mca_pls_bproc_component.active_node_names, 8, 200000, 8);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

/**
 * Closes the pls_bproc component
 */
int orte_pls_bproc_component_close(void) {
    OBJ_DESTRUCT(&mca_pls_bproc_component.lock);
    OBJ_DESTRUCT(&mca_pls_bproc_component.condition);
    OBJ_RELEASE(mca_pls_bproc_component.daemon_names);
    return ORTE_SUCCESS;
}

/**
 * Initializes the module. We do not want to run unless we are the seed, bproc
 * is running, and we are the master node.
 */
orte_pls_base_module_t* orte_pls_bproc_init(int *priority) {
    int ret;
    struct bproc_version_t version;
 
    /* are we the seed */
    if(orte_process_info.seed == false)
        return NULL;

    /* okay, we are in a daemon - now check to see if BProc is running here */
    ret = bproc_version(&version);
    if (ret != 0) {
        return NULL;
    }
    
    /* only launch from the master node */
    if (bproc_currnode() != BPROC_NODE_MASTER) {
        return NULL;
    }

    *priority = mca_pls_bproc_component.priority;
    return &orte_pls_bproc_module;
}

