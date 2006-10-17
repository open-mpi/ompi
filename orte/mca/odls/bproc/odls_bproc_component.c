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
#include "orte/orte_constants.h"

#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"

#include "orte/mca/odls/odls.h"
#include "odls_bproc.h"

/* instance the child list object */
static void odls_bproc_child_constructor(odls_bproc_child_t *ptr)
{
    ptr->name = NULL;
    ptr->app_idx = -1;
    ptr->alive = false;
}
static void odls_bproc_child_destructor(odls_bproc_child_t *ptr)
{
    if (NULL != ptr->name) free(ptr->name);
}
OBJ_CLASS_INSTANCE(odls_bproc_child_t,
                   opal_list_item_t,
                   odls_bproc_child_constructor,
                   odls_bproc_child_destructor);

/**
 * The bproc component data structure used to store all the relevent data
 * about this component.
 */
orte_odls_bproc_component_t mca_odls_bproc_component = {
    {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        /* Indicate that we are a odls v1.3.0 component (which also
           implies a specific MCA version) */
        ORTE_ODLS_BASE_VERSION_1_3_0,
        /* Component name and version */
        "bproc",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        /* Component open and close functions */
        orte_odls_bproc_component_open,
        orte_odls_bproc_component_close
    },
    /* Next the MCA v1.0.0 component meta data */
    {
        /* Whether the component is checkpointable or not */
        false
    },
    /* Initialization / querying functions */
    orte_odls_bproc_init,
    orte_odls_bproc_finalize
    }
};

/**
 * Opens the pls_bproc component, setting all the needed mca parameters and
 * finishes setting up the component struct.
 */
int orte_odls_bproc_component_open(void)
{
    /* initialize globals */
    OBJ_CONSTRUCT(&mca_odls_bproc_component.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_odls_bproc_component.cond, opal_condition_t);
    OBJ_CONSTRUCT(&mca_odls_bproc_component.children, opal_list_t);

    /* lookup parameters */
    mca_base_param_reg_int(&mca_odls_bproc_component.super.version, 
                           "priority", NULL, false, false, 100,
                           &mca_odls_bproc_component.priority);
    mca_base_param_reg_int(&mca_odls_bproc_component.super.version, 
                           "debug", "If > 0 prints library debugging information",
                           false, false, 0, &mca_odls_bproc_component.debug);
    return ORTE_SUCCESS;
}

/**
 * Initializes the module. We do not want to run unless we are not the seed,
 * bproc is running, and we are not on the master node.
 */
orte_odls_base_module_t *orte_odls_bproc_init(int *priority)
{
    int ret;
    struct bproc_version_t version;

    /* the base open/select logic protects us against operation when
     * we are NOT in a daemon, so we don't have to check that here
     */
        
    /* check to see if BProc is running here */
    ret = bproc_version(&version);
    if (ret != 0) {
        return NULL;
    }

    /* only launch if we are not the master node */
    if (bproc_currnode() == BPROC_NODE_MASTER) {
        return NULL;
    }

    *priority = mca_odls_bproc_component.priority;
    return &orte_odls_bproc_module;
}

/**
 *  Component close function.
 */
int orte_odls_bproc_component_close(void)
{
    OBJ_DESTRUCT(&mca_odls_bproc_component.lock);
    OBJ_DESTRUCT(&mca_odls_bproc_component.cond);
    OBJ_DESTRUCT(&mca_odls_bproc_component.children);
    return ORTE_SUCCESS;
}
