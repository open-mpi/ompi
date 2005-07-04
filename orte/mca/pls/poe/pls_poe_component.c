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

#include "include/orte_constants.h"
#include "mca/pls/pls.h"
#include "pls_poe.h"
#include "opal/util/path.h"
#include "opal/util/argv.h"
#include "mca/pls/poe/pls-poe-version.h"
#include "mca/base/mca_base_param.h"


/*
 * Public string showing the pls ompi_poe component version number
 */
const char *mca_pls_poe_component_version_string =
  "Open MPI poe pls MCA component version " MCA_pls_poe_VERSION;


/*
 * Local variable
 */


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_pls_poe_component_t mca_pls_poe_component = {
    {
    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        /* Indicate that we are a pls v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_PLS_BASE_VERSION_1_0_0,

        /* Component name and version */

        "poe",
        MCA_pls_poe_MAJOR_VERSION,
        MCA_pls_poe_MINOR_VERSION,
        MCA_pls_poe_RELEASE_VERSION,

        /* Component open and close functions */

        orte_pls_poe_component_open,
        NULL
    },

    /* Next the MCA v1.0.0 component meta data */

    {
        /* Whether the component is checkpointable or not */

        false
    },

    /* Initialization / querying functions */

    orte_pls_poe_component_init
    }
};

/**
orte_pls_poe_param_reg_int - register and lookup a integer parameter 
@param param_name parameter name [INPUT]
@param default_value default value [INPUT]
@return parameter value
*/
int orte_pls_poe_param_reg_int(char * param_name, int default_value)
{
    int id, param_value;
    id = mca_base_param_register_int("pls","poe",param_name,NULL,default_value);
    param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/**
orte_pls_poe_param_reg_string - register and lookup a string parameter 
@param param_name parameter name [INPUT]
@param default_value default value [INPUT]
@return parameter value
*/
char* orte_pls_poe_param_reg_string(char* param_name, char* default_value)
{
    char *param_value;
    int id;
    id = mca_base_param_register_string("pls","poe",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

/**
orte_pls_poe_component_open - open component and register all parameters
@return error number
*/
int orte_pls_poe_component_open(void)
{
    char *param;

    mca_pls_poe_component.mp_retry = orte_pls_poe_param_reg_int("mp_retry", 0); 
    mca_pls_poe_component.mp_retrycount = orte_pls_poe_param_reg_int("mp_retrycount", 0); 
    mca_pls_poe_component.mp_infolevel = orte_pls_poe_param_reg_int("mp_infolevel", 0); 
    mca_pls_poe_component.mp_labelio = orte_pls_poe_param_reg_string("mp_labelio","no");
    mca_pls_poe_component.mp_stdoutmode = orte_pls_poe_param_reg_string("mp_stdoutmode","unordered");

    mca_pls_poe_component.debug = orte_pls_poe_param_reg_int("debug",0);
    mca_pls_poe_component.verbose = orte_pls_poe_param_reg_int("verbose",0);
    mca_pls_poe_component.priority = orte_pls_poe_param_reg_int("priority", 100);
    mca_pls_poe_component.orted = orte_pls_poe_param_reg_string("orted","orted"); 
    mca_pls_poe_component.class = orte_pls_poe_param_reg_string("class","interactive"); 
    mca_pls_poe_component.env = orte_pls_poe_param_reg_string("progenv","env");

    param = orte_pls_poe_param_reg_string("progpoe","poe");
    mca_pls_poe_component.argv = opal_argv_split(param, ' ');
    mca_pls_poe_component.argc = opal_argv_count(mca_pls_poe_component.argv);
    if (mca_pls_poe_component.argc > 0) {
        mca_pls_poe_component.path = strdup(mca_pls_poe_component.argv[0]);
        return ORTE_SUCCESS;
    } else {
        mca_pls_poe_component.path = NULL;
        return ORTE_ERR_BAD_PARAM;
    }
    return ORTE_SUCCESS;
}

/**
orte_pls_poe_component_init - initialize component, check if we can run on this machine.
@return error number
*/
orte_pls_base_module_t *orte_pls_poe_component_init(int *priority)
{
    extern char **environ;
    mca_pls_poe_component.path = opal_path_findv(mca_pls_poe_component.argv[0], 0, environ, NULL);
    if (NULL == mca_pls_poe_component.path) {
        return NULL;
    }
    mca_pls_poe_component.env = opal_path_findv(mca_pls_poe_component.env, 0, environ, NULL);
    if (NULL == mca_pls_poe_component.env) {
        return NULL;
    }
    *priority = mca_pls_poe_component.priority;
    return &orte_pls_poe_module;
}
