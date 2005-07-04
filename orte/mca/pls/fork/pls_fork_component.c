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
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "ompi_config.h"
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "include/orte_constants.h"
#include "opal/util/argv.h"
#include "opal/util/path.h"
#include "mca/pls/pls.h"
#include "mca/pls/fork/pls_fork.h"
#include "mca/pls/fork/pls-fork-version.h"
#include "mca/base/mca_base_param.h"


/*
 * Public string showing the pls ompi_fork component version number
 */
const char *mca_pls_fork_component_version_string =
  "Open MPI fork pls MCA component version " MCA_pls_fork_VERSION;


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_pls_fork_component_t mca_pls_fork_component = {
    {
    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        /* Indicate that we are a pls v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_PLS_BASE_VERSION_1_0_0,

        /* Component name and version */

        "fork",
        MCA_pls_fork_MAJOR_VERSION,
        MCA_pls_fork_MINOR_VERSION,
        MCA_pls_fork_RELEASE_VERSION,

        /* Component open and close functions */

        orte_pls_fork_component_open,
        orte_pls_fork_component_close
    },

    /* Next the MCA v1.0.0 component meta data */

    {
        /* Whether the component is checkpointable or not */

        true
    },

    /* Initialization / querying functions */

    orte_pls_fork_component_init
    }
};



/**
 *  Convience functions to lookup MCA parameter values.
 */
                                                                                                  
static  int orte_pls_fork_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("pls","fork",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}
                                                                                                  

int orte_pls_fork_component_open(void)
{
    /* initialize globals */
    OBJ_CONSTRUCT(&mca_pls_fork_component.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_pls_fork_component.cond, opal_condition_t);

    /* lookup parameters */
    mca_pls_fork_component.reap = orte_pls_fork_param_register_int("reap",1);
    mca_pls_fork_component.priority = orte_pls_fork_param_register_int("priority",1);
    mca_pls_fork_component.debug = orte_pls_fork_param_register_int("debug",0);
    if(mca_pls_fork_component.debug == 0) {
        int id = mca_base_param_register_int("debug",NULL,NULL,NULL,0);
        int value;
        mca_base_param_lookup_int(id,&value);
        mca_pls_fork_component.debug = (value > 0) ? 1 : 0;
    }
    return ORTE_SUCCESS;
}


orte_pls_base_module_t *orte_pls_fork_component_init(int *priority)
{
    *priority = mca_pls_fork_component.priority;
    return &orte_pls_fork_module;
}


int orte_pls_fork_component_close(void)
{
    OBJ_DESTRUCT(&mca_pls_fork_component.lock);
    OBJ_DESTRUCT(&mca_pls_fork_component.cond);
    return ORTE_SUCCESS;
}

