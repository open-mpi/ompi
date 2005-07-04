/*
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

#include "ompi_config.h"
#include "mca/pls/pls.h"
#include "pls_bproc_orted.h"
#include "mca/base/mca_base_param.h"
#include "include/orte_constants.h"
#include "util/proc_info.h"
#include <sys/bproc.h>


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
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
 *  Convience functions to lookup MCA parameter values.
 */
static int orte_pls_bproc_orted_param_register_int(const char* param_name, 
                                                   int default_value)
{
    int id = mca_base_param_register_int("pls","bproc_orted",param_name,NULL,
                                         default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/**
 *  Component open function.
 */
int orte_pls_bproc_orted_component_open(void)
{
    /* initialize globals */
    OBJ_CONSTRUCT(&mca_pls_bproc_orted_component.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_pls_bproc_orted_component.condition, opal_condition_t);

    /* lookup parameters */
    mca_pls_bproc_orted_component.priority = 
                           orte_pls_bproc_orted_param_register_int("priority",100);
    mca_pls_bproc_orted_component.debug = 
                           orte_pls_bproc_orted_param_register_int("debug",0);
    return ORTE_SUCCESS;
}

/**
 *  Module init function.
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
    OBJ_DESTRUCT(&mca_pls_bproc_orted_component.condition);
    return ORTE_SUCCESS;
}

