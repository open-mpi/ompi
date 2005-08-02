/* -*- C -*-
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "orte_config.h"
#include "orte/mca/errmgr/errmgr.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"
#include "pls_bproc.h"

/*
 * Struct of function pointers and all that to let us be initialized
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

int orte_pls_bproc_component_open(void) {
    int rc;
    /* init parameters */
    mca_base_param_reg_int(&mca_pls_bproc_component.super.pls_version, "priority",
                           NULL, false, false, 100, 
                           &mca_pls_bproc_component.priority);
    mca_base_param_reg_int(&mca_pls_bproc_component.super.pls_version, "debug", 
                           "If > 0 prints library debugging information",
                           false, false, 0, &mca_pls_bproc_component.debug);
    mca_base_param_reg_int(&mca_pls_bproc_component.super.pls_version, 
                           "terminate_sig",
                           "Signal sent to processes to terminate them", false,
                           false, 9, &mca_pls_bproc_component.terminate_sig);
    mca_base_param_reg_string(&mca_pls_bproc_component.super.pls_version, 
                              "orted", "Path to where orted is installed", false, 
                              false, "orted", &mca_pls_bproc_component.orted);
    
    mca_pls_bproc_component.num_procs = 0; 
    mca_pls_bproc_component.num_daemons = 0; 
    mca_pls_bproc_component.done_launching = false; 
    OBJ_CONSTRUCT(&mca_pls_bproc_component.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_pls_bproc_component.condition, opal_condition_t);
    /* init the list to hold the daemon names */
    rc = orte_pointer_array_init(&mca_pls_bproc_component.daemon_names, 8, 200000, 8);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

int orte_pls_bproc_component_close(void) {
    OBJ_DESTRUCT(&mca_pls_bproc_component.lock);
    OBJ_DESTRUCT(&mca_pls_bproc_component.condition);
    OBJ_RELEASE(mca_pls_bproc_component.daemon_names);
    return ORTE_SUCCESS;
}

/**
 * initializes the component. We do not want to run unless we are the seed, bproc
 * is running, and we are the master node
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

