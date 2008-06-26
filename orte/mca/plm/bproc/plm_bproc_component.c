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

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"

#include "plm_bproc.h"

/*
 * Public string showing the plm ompi_bproc component version number
 */
const char *mca_plm_bproc_component_version_string =
"Open MPI bproc plm MCA component version " ORTE_VERSION;

static int plm_bproc_open(void);
static int plm_bproc_close(void);
static int orte_plm_bproc_component_query(mca_base_module_t **module, int *priority);

/**
 * The bproc component data structure used to store all the relevent data about
 * this component.
 */
orte_plm_bproc_component_t mca_plm_bproc_component = {
    {
        {
            ORTE_PLM_BASE_VERSION_1_0_0,
            "bproc", /* MCA component name */
            ORTE_MAJOR_VERSION,  /* MCA component major version */
            ORTE_MINOR_VERSION,  /* MCA component minor version */
            ORTE_RELEASE_VERSION,  /* MCA component release version */
            
            /* Component open and close functions */
            plm_bproc_open,
            plm_bproc_close,
            orte_plm_bproc_component_query
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};

/**
 * Opens the plm_bproc component, setting all the needed mca parameters and 
 * finishes setting up the component struct.
 */
static int plm_bproc_component_open(void) {
    mca_base_component_t *c = &mca_plm_bproc_component.super.base_version;
    
    /* init parameters */
     mca_base_param_reg_string(c, "orted", "Path to where orted is installed",
                           false, false, "orted", &mca_plm_bproc_component.orted);
   
    return ORTE_SUCCESS;
}

/**
 * Closes the plm_bproc component
 */
static int plm_bproc_component_close(void) {
    return ORTE_SUCCESS;
}

/**
 * Initializes the module. We do not want to run unless we are the seed, bproc
 * is running, and we are the master node.
 */
static int orte_plm_bproc_component_query(mca_base_module_t **module, int *priority)
{
    int ret;
    struct bproc_version_t version;
 
    /* see if BProc is running here */
    ret =  bproc_version(&version);
    if (ret != 0) {
        *module = NULL;
        return ORTE_ERR_NOT_AVAILABLE;
    }
     
    /* only launch from the master node */
    if (bproc_currnode() != BPROC_NODE_MASTER) {
        *module = NULL;
        return ORTE_ERR_NOT_AVAILABLE;
    }

    *priority = 20;
    *module = (mca_base_module_t *) &orte_plm_bproc_module;
    return ORTE_SUCCESS;
}

