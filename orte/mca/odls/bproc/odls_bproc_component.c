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
#include "orte/constants.h"

#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"

#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/odls_private.h"
#include "odls_bproc.h"

extern orte_odls_base_module_t orte_odls_bproc_module;

/**
 * The bproc component data structure used to store all the relevent data
 * about this component.
 */
orte_odls_bproc_component_t mca_odls_bproc_component = {
    {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        ORTE_ODLS_BASE_VERSION_2_0_0,
        /* Component name and version */
        "bproc",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        /* Component open and close functions */
        orte_odls_bproc_component_open,
        orte_odls_bproc_component_close,
        orte_odls_bproc_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

/**
 * Opens the pls_bproc component, setting all the needed mca parameters and
 * finishes setting up the component struct.
 */
int orte_odls_bproc_component_open(void)
{
    return ORTE_SUCCESS;
}

/**
 * Initializes the module.
 */
int orte_odls_bproc_component_query(mca_base_module_t **module, int *priority)
{
    int ret;
    struct bproc_version_t version;

    /* check to see if BProc is running here */
    ret = bproc_version(&version);
    if (ret != 0) {
        return NULL;
    }

    *priority = 30;
    *module = (mca_base_module_t *)&orte_odls_bproc_module;
    return ORTE_SUCCESS;
}

/**
 *  Component close function.
 */
int orte_odls_bproc_component_close(void)
{
    /* cleanup state */
    while (NULL != (item = opal_list_remove_first(&orte_odls_globals.children))) {
        OBJ_RELEASE(item);
    }
    
    return ORTE_SUCCESS;
}
