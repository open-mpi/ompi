/*
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
#include "opal/mca/base/mca_base_param.h"
#include "orte/include/orte_constants.h"
#include "orte/mca/pls/pls.h"
#include "orte/util/proc_info.h"
#include "pls_bproc_orted.h"

/**
 * The bproc_orted component data structure used to store all the relevent data
 * about this component.
 */
orte_pls_bproc_orted_component_t mca_pls_bproc_orted_component = {
    {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        /* Indicate that we are a pls v1.0.0 component (which also
           implies a specific MCA version) */
        ORTE_PLS_BASE_VERSION_1_0_0,
        /* Component name and version */
        "bproc_orted",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        /* Component open and close functions */
        orte_pls_bproc_orted_component_open,
        orte_pls_bproc_orted_component_close
    },
    /* Next the MCA v1.0.0 component meta data */
    {
        /* Whether the component is checkpointable or not */
        false
    },
    /* Initialization / querying functions */
    orte_pls_bproc_orted_init
    }
};

/**
 * Opens the pls_bproc component, setting all the needed mca parameters and
 * finishes setting up the component struct.
 */
int orte_pls_bproc_orted_component_open(void)
{
    /* initialize globals */
    OBJ_CONSTRUCT(&mca_pls_bproc_orted_component.lock, opal_mutex_t);

    /* lookup parameters */
    mca_base_param_reg_int(&mca_pls_bproc_orted_component.super.pls_version, 
                           "priority", NULL, false, false, 100,
                           &mca_pls_bproc_orted_component.priority);
    mca_base_param_reg_int(&mca_pls_bproc_orted_component.super.pls_version, 
                           "debug", "If > 0 prints library debugging information",
                           false, false, 0, &mca_pls_bproc_orted_component.debug);
    return ORTE_SUCCESS;
}

/**
 * Initializes the module. We do not want to run unless we are not the seed,
 * bproc is running, and we are not on the master node.
 */
orte_pls_base_module_t *orte_pls_bproc_orted_init(int *priority)
{
    int ret;
    struct bproc_version_t version;

    /* are we the seed */
    if(orte_process_info.seed == true)
        return NULL;

    /* okay, we are in a daemon - now check to see if BProc is running here */
    ret = bproc_version(&version);
    if (ret != 0) {
        return NULL;
    }

    /* only launch if we are not the master node */
    if (bproc_currnode() == BPROC_NODE_MASTER) {
        return NULL;
    }

    *priority = mca_pls_bproc_orted_component.priority;
    return &orte_pls_bproc_orted_module;
}

/**
 *  Component close function.
 */
int orte_pls_bproc_orted_component_close(void)
{
    OBJ_DESTRUCT(&mca_pls_bproc_orted_component.lock);
    return ORTE_SUCCESS;
}

