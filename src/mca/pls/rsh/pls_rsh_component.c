/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
#include <unistd.h>

#include "include/orte_constants.h"
#include "util/argv.h"
#include "util/path.h"
#include "mca/pls/pls.h"
#include "pls_rsh.h"
#include "mca/pls/rsh/pls-rsh-version.h"
#include "mca/base/mca_base_param.h"

extern char **environ;

/*
 * Public string showing the pls ompi_rsh component version number
 */
const char *mca_pls_rsh_component_version_string =
  "Open MPI rsh pls MCA component version " MCA_pls_rsh_VERSION;


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_pls_rsh_component_t mca_pls_rsh_component = {
    {
    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        /* Indicate that we are a pls v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_PLS_BASE_VERSION_1_0_0,

        /* Component name and version */

        "rsh",
        MCA_pls_rsh_MAJOR_VERSION,
        MCA_pls_rsh_MINOR_VERSION,
        MCA_pls_rsh_RELEASE_VERSION,

        /* Component open and close functions */

        orte_pls_rsh_component_open,
        orte_pls_rsh_component_close
    },

    /* Next the MCA v1.0.0 component meta data */

    {
        /* Whether the component is checkpointable or not */

        true
    },

    /* Initialization / querying functions */

    orte_pls_rsh_component_init
    }
};



/**
 *  Convience functions to lookup MCA parameter values.
 */
                                                                                                  
static  int orte_pls_rsh_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("pls","rsh",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}
                                                                                                  
                                                                                                  
static char* orte_pls_rsh_param_register_string(
    const char* param_name,
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("pls","rsh",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}
                                                                                                  
                                                                                                  

int orte_pls_rsh_component_open(void)
{
    char* param;
    /* initialize globals */
    OBJ_CONSTRUCT(&mca_pls_rsh_component.lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_pls_rsh_component.cond, ompi_condition_t);
    mca_pls_rsh_component.num_children = 0;

    /* lookup parameters */
    mca_pls_rsh_component.debug = orte_pls_rsh_param_register_int("debug",0);
    mca_pls_rsh_component.num_concurrent = orte_pls_rsh_param_register_int("num_concurrent",128);
    if(mca_pls_rsh_component.debug == 0) {
        int id = mca_base_param_register_int("debug",NULL,NULL,NULL,0);
        int value;
        mca_base_param_lookup_int(id,&value);
        mca_pls_rsh_component.debug = (value > 0) ? 1 : 0;
    }

    mca_pls_rsh_component.orted = orte_pls_rsh_param_register_string("orted","orted");
    mca_pls_rsh_component.priority = orte_pls_rsh_param_register_int("priority",10);
    mca_pls_rsh_component.delay = orte_pls_rsh_param_register_int("delay",1);
    mca_pls_rsh_component.reap = orte_pls_rsh_param_register_int("reap",1);

    param = orte_pls_rsh_param_register_string("agent","ssh");
    mca_pls_rsh_component.argv = ompi_argv_split(param, ' ');
    mca_pls_rsh_component.argc = ompi_argv_count(mca_pls_rsh_component.argv);
    return (mca_pls_rsh_component.argc > 0) ? ORTE_SUCCESS : ORTE_ERR_BAD_PARAM;
}


orte_pls_base_module_t *orte_pls_rsh_component_init(int *priority)
{
    extern char **environ;

    /* If we didn't find the agent in the path, then don't use this component */
    if (NULL == mca_pls_rsh_component.argv || NULL == mca_pls_rsh_component.argv[0]) {
        return NULL;
    }
    mca_pls_rsh_component.path = ompi_path_findv(mca_pls_rsh_component.argv[0], 0, environ, NULL);
    if (NULL == mca_pls_rsh_component.path) {
        return NULL;
    }
    *priority = mca_pls_rsh_component.priority;
    return &orte_pls_rsh_module;
}


int orte_pls_rsh_component_close(void)
{
    if(mca_pls_rsh_component.reap) {
        OMPI_THREAD_LOCK(&mca_pls_rsh_component.lock);
        while(mca_pls_rsh_component.num_children > 0) {
            ompi_condition_wait(&mca_pls_rsh_component.cond, &mca_pls_rsh_component.lock);
        }
        OMPI_THREAD_UNLOCK(&mca_pls_rsh_component.lock);
    }

    OBJ_DESTRUCT(&mca_pls_rsh_component.lock);
    OBJ_DESTRUCT(&mca_pls_rsh_component.cond);
    ompi_argv_free(mca_pls_rsh_component.argv);
    return ORTE_SUCCESS;
}

