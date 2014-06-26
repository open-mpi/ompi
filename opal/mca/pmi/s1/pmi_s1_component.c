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
#include "opal/mca/pmi/pmi.h"
#include "pmi_s1.h"

/*
 * Public string showing the pmi s1 component version number
 */
const char *opal_pmi_s1_component_version_string =
    "OPAL s1 pmi MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int pmi_s1_component_query(mca_base_module_t **module, int *priority);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const opal_pmi_base_component_t mca_pmi_s1_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        /* Indicate that we are a pmi v1.1.0 component (which also
           implies a specific MCA version) */
        
        OPAL_PMI_BASE_VERSION_2_0_0,

        /* Component name and version */

        "s1",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */

        NULL,
        NULL,
        pmi_s1_component_query,
        NULL
    },
    /* Next the MCA v1.0.0 component meta data */
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


static int pmi_s1_component_query(mca_base_module_t **module, int *priority)
{
    /* disqualify ourselves if we are not under slurm */
    if (NULL == getenv("SLURM_JOBID")) {
        *priority = 0;
        *module = NULL;
        return OPAL_ERROR;
    }
    
    /* we can be considered, but set our priority by default
     * to be less than s2 */
    *priority = 10;
    *module = (mca_base_module_t *)&opal_pmi_s1_module;
    return OPAL_SUCCESS;
}
