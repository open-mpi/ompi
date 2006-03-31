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
#include "pls_xcpu.h"

/**
 * The xcpu component data structure that stores all the relevent data about
 * this component.
 */
orte_pls_xcpu_component_t mca_pls_xcpu_component = {
    { /* version, data and init members of only first 
       * structure (called super) being initialized
       */
        {
        ORTE_PLS_BASE_VERSION_1_0_0,
        "xcpu", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_pls_xcpu_component_open,  /* component open */
        orte_pls_xcpu_component_close /* component close */
        },
        {
        false /* checkpoint / restart */
        },
        orte_pls_xcpu_init    /* component init */ 
    }
};

/**
 * Opens the pls_xcpu component, setting all the needed mca parameters and 
 * finishes setting up the component struct.
 */
int orte_pls_xcpu_component_open(void) {
    int rc;
    /* init parameters */ 
    /*read trunk/opal/mca/base/mca_base_param.h for reg_int details*/
    mca_base_component_t *c = &mca_pls_xcpu_component.super.pls_version;
    mca_base_param_reg_int(c, "priority", NULL, false, false,0,
                           &mca_pls_xcpu_component.priority);
    mca_base_param_reg_int(c, "debug",
                           "If > 0 prints library debugging information",
                           false, false, 0, &mca_pls_xcpu_component.debug);
    mca_base_param_reg_int(c, "terminate_sig",
                           "Signal sent to processes to terminate them", false,
                           false, 9, &mca_pls_xcpu_component.terminate_sig);
    mca_pls_xcpu_component.num_procs = 0;
    mca_pls_xcpu_component.num_daemons = 0;
    mca_pls_xcpu_component.done_launching = false;
    OBJ_CONSTRUCT(&mca_pls_xcpu_component.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_pls_xcpu_component.condition, opal_condition_t);
    /* init the list to hold the daemon names */
    rc = orte_pointer_array_init(&mca_pls_xcpu_component.daemon_names, 8, 200000, 8);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}

/**
 * Closes the pls_xcpu component
 */
int orte_pls_xcpu_component_close(void) {
    OBJ_DESTRUCT(&mca_pls_xcpu_component.lock);
    OBJ_DESTRUCT(&mca_pls_xcpu_component.condition);
    OBJ_RELEASE(mca_pls_xcpu_component.daemon_names);
    return ORTE_SUCCESS;
}

/**
 * Initializes the module. We do not want to run unless, xcpu
 * is running and we are on the control node.
 */
/* What I thnk is that this function will be called some where from (R)esource (M)ana(G)e(R)
 * and then it will return orte_pls_xcpu_module that contains function pointers for launch, 
 * finalize etc. and then resource manager can call these functions
 */
orte_pls_base_module_t* orte_pls_xcpu_init(int *priority) {
    /* check if xcpu component should be loaded or not
     * if not, then return NULL here
     */
    /*return NULL; *//*for time being*/
    *priority = mca_pls_xcpu_component.priority;
    return &orte_pls_xcpu_module; /* this is defined in pls_xcpu.c and will contains
				   * function pointers for launch, terminate_job
				   * terminate_proc and finalize
			   	   */
}

