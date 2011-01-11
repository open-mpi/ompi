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
 * Copyright (c) 2007-2011 Cisco Systems, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"

#include "opal/mca/paffinity/paffinity.h"
#include "paffinity_hwloc.h"

/*
 * Public string showing the paffinity ompi_hwloc component version number
 */
const char *opal_paffinity_hwloc_component_version_string =
    "OPAL hwloc paffinity MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int hwloc_open(void);
static int hwloc_close(void);
static int hwloc_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

opal_paffinity_hwloc_component_t mca_paffinity_hwloc_component = {
    {
        /* First, the mca_component_t struct containing meta information
           about the component itself */
        {
            OPAL_PAFFINITY_BASE_VERSION_2_0_1,
            
            /* Component name and version */
            "hwloc",
            OPAL_MAJOR_VERSION,
            OPAL_MINOR_VERSION,
            OPAL_RELEASE_VERSION,
            
            /* Component open and close functions */
            hwloc_open,
            hwloc_close,
            opal_paffinity_hwloc_component_query,
            hwloc_register,
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    },
    /* NULL fill the rest of the component data */
};


static int hwloc_register(void)
{
    mca_base_param_reg_int(&mca_paffinity_hwloc_component.super.base_version,
                           "priority",
                           "Priority of the hwloc paffinity component",
                           false, false, 40, NULL);
    mca_base_param_reg_string(&mca_paffinity_hwloc_component.super.base_version,
                              "hwloc_version",
                              "Version of HWLOC that is embedded in Open MPI",
                              false, true, PAFFINITY_HWLOC_HWLOC_VERSION, NULL);

    return OPAL_SUCCESS;
}


static int hwloc_open(void)
{
    /* Initialize hwloc */
    if (0 != hwloc_topology_init(&(mca_paffinity_hwloc_component.topology)) ||
        0 != hwloc_topology_load(mca_paffinity_hwloc_component.topology)) {
        return OPAL_ERR_NOT_AVAILABLE;
    }
    mca_paffinity_hwloc_component.topology_need_destroy = true;

    return OPAL_SUCCESS;
}

static int hwloc_close(void)
{
    /* If we set up hwloc, tear it down */
    if (mca_paffinity_hwloc_component.topology_need_destroy) {
        hwloc_topology_destroy(mca_paffinity_hwloc_component.topology);
        mca_paffinity_hwloc_component.topology_need_destroy = false;
    }

    return OPAL_SUCCESS;
}
