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
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <numa.h>
#include <numaif.h>
#include <unistd.h>

#include "opal/constants.h"
#include "opal/util/show_help.h"
#include "opal/mca/maffinity/maffinity.h"

#include "maffinity_libnuma.h"

/*
 * Public string showing the maffinity ompi_libnuma component version number
 */
const char *opal_maffinity_libnuma_component_version_string =
    "OPAL libnuma maffinity MCA component version " OPAL_VERSION;

/*
 * Local functions
 */
static int libnuma_register(void);

/*
 * Local variable
 */
static char *mca_policy = NULL;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

opal_maffinity_libnuma_component_2_0_0_t mca_maffinity_libnuma_component = {
    {
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
            NULL,
            NULL,
            opal_maffinity_libnuma_component_query,
            libnuma_register,
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    },

    /* Default libnuma memory binding policy */
    MPOL_PREFERRED,
};


static int libnuma_register(void)
{
    char *val;

    mca_base_param_reg_int(&mca_maffinity_libnuma_component.base.base_version,
                           "priority",
                           "Priority of the libnuma maffinity component",
                           false, false, 25, NULL);

    val = (MPOL_PREFERRED == mca_maffinity_libnuma_component.libnuma_policy ?
           "loose" : "strict");
    mca_base_param_reg_string(&mca_maffinity_libnuma_component.base.base_version,
                              "policy", 
                              "Binding policy that determines what happens if memory is unavailable on the local NUMA node.  A value of \"strict\" means that the memory allocation will fail; a value of \"loose\" means that the memory allocation will spill over to another NUMA node.",
                              false, false, val, &mca_policy);

    if (strcasecmp(mca_policy, "loose") == 0) {
        mca_maffinity_libnuma_component.libnuma_policy = MPOL_PREFERRED;
    } else if (strcasecmp(mca_policy, "strict") == 0) {
        mca_maffinity_libnuma_component.libnuma_policy = MPOL_BIND;
    } else {
        opal_show_help("help-opal-maffinity-libnuma.txt", "invalid policy",
                       true, mca_policy, getpid());
        mca_maffinity_libnuma_component.libnuma_policy = MPOL_PREFERRED;
        return OPAL_ERR_BAD_PARAM;
    }

    return OPAL_SUCCESS;
}
