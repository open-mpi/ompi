/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
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
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/maffinity/maffinity.h"
#include "maffinity_libnuma.h"

/*
 * Public string showing the maffinity ompi_libnuma component version number
 */
const char *opal_maffinity_libnuma_component_version_string =
    "OPAL libnuma maffinity MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int libnuma_open(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const opal_maffinity_base_component_2_0_0_t mca_maffinity_libnuma_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        OPAL_MAFFINITY_BASE_VERSION_2_0_0,

        /* Component name and version */
        "libnuma",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,
        
        /* Component open and close functions */
        libnuma_open,
        NULL,
        opal_maffinity_libnuma_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


static int libnuma_open(void)
{
    mca_base_param_reg_int(&mca_maffinity_libnuma_component.base_version,
                           "priority",
                           "Priority of the libnuma maffinity component",
                           false, false, 25, NULL);

    return OPAL_SUCCESS;
}
