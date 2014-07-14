/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
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

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/pmix/pmix.h"
#include "pmix_native.h"

/*
 * Public string showing the pmix native component version number
 */
const char *opal_pmix_native_component_version_string =
    "OPAL native pmix MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int pmix_native_component_query(mca_base_module_t **module, int *priority);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const opal_pmix_base_component_t mca_pmix_native_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        /* Indicate that we are a pmix v1.1.0 component (which also
           implies a specific MCA version) */
        
        OPAL_PMIX_BASE_VERSION_2_0_0,

        /* Component name and version */

        "native",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */

        NULL,
        NULL,
        pmix_native_component_query,
        NULL
    },
    /* Next the MCA v1.0.0 component meta data */
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


static int pmix_native_component_query(mca_base_module_t **module, int *priority)
{
    /* we can be considered, but set our priority by default
     * to be less than any other component */
    *priority = 1;
    *module = (mca_base_module_t *)&opal_pmix_native_module;
    return OPAL_SUCCESS;
}
