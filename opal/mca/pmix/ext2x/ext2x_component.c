/*
 * Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016 Cisco Systems, Inc.  All rights reserved.
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
#include "opal/mca/pmix/base/common/pmix2x_common.h"

/*
 * Public string showing the pmix ext2x component version number
 */
const char *opal_pmix_ext2x_component_version_string =
    "OPAL ext2x MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int ext2x_component_query(mca_base_module_t **module, int *priority);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

opal_pmix_base_component_t mca_pmix_ext2x_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */

    .base_version = {
        /* Indicate that we are a pmix v1.1.0 component (which also
           implies a specific MCA version) */

        OPAL_PMIX_BASE_VERSION_2_0_0,

            /* Component name and version */

        .mca_component_name = "ext2x",
        MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                              OPAL_RELEASE_VERSION),

            /* Component open and close functions */
        .mca_query_component = ext2x_component_query,
    },
        /* Next the MCA v1.0.0 component meta data */
    .base_data = {
            /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

static int ext2x_component_query(mca_base_module_t **module, int *priority)
{
    char *t, *id;

    /* see if a PMIx server is present */
    if (NULL != (t = getenv("PMIX_SERVER_URI")) ||
        NULL != (id = getenv("PMIX_ID"))) {
        /* if PMIx is present, then we are a client and need to use it */
        *priority = 100;
    } else {
        /* we could be a server, so we still need to be considered */
        *priority = 5;
    }
    *module = (mca_base_module_t *)&opal_pmix_pmix2x_common_module;
    return OPAL_SUCCESS;
}
