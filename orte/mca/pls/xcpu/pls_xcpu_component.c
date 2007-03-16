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
 * Copyright (c) 2004-2006 The Regents of the University of California.
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
#include "spfs.h"

/**
 * The xcpu component data structure that stores all the relevent data about
 * this component.
 */
orte_pls_xcpu_component_t mca_pls_xcpu_component = {
    { /* version, data and init members of only first 
       * structure (called super) being initialized
       */
        {
        ORTE_PLS_BASE_VERSION_1_3_0,
        "xcpu", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_pls_xcpu_component_open,  /* component open */
        orte_pls_xcpu_component_close /* component close */
        },
        {
            /* This component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },
        orte_pls_xcpu_init    /* component init */ 
    }
};

/** external variable defined in libspclient */
extern int spc_chatty;

/**
 * Opens the pls_xcpu component, setting all the needed mca parameters and 
 * finishes setting up the component struct.
 */
int orte_pls_xcpu_component_open(void)
{
    int rc = ORTE_SUCCESS;

    /* init parameters */ 
    /* read trunk/opal/mca/base/mca_base_param.h for reg_int details*/
    mca_base_component_t *c = &mca_pls_xcpu_component.super.pls_version;
    mca_base_param_reg_int(c, "priority",
			   "Priority of the xcpu pls component",
			   false, false, 5, &mca_pls_xcpu_component.priority);
    mca_base_param_reg_int(c, "debug",
                           "If > 0 prints library debugging information",
                           false, false, 0, &mca_pls_xcpu_component.debug);
    mca_base_param_reg_int(c, "chatty",
			   "Prints 9P protocol transactions",
			   false, false, 0, &mca_pls_xcpu_component.chatty);
    mca_base_param_reg_int(c, "maxsession",
			   "Max fan out when using XCPUFS tree spawn",
			   false, false, -1, &mca_pls_xcpu_component.maxsessions);

    if (mca_pls_xcpu_component.chatty)
	    spc_chatty = 1;

    return rc;
}

/**
 * Closes the pls_xcpu component
 */
int orte_pls_xcpu_component_close(void)
{
    return ORTE_SUCCESS;
}

/**
 * Initializes the module,
 *
 * FixMe: do we have to check anything?
 */
orte_pls_base_module_t* orte_pls_xcpu_init(int *priority)
{
    *priority = mca_pls_xcpu_component.priority;
    return &orte_pls_xcpu_module;
}

