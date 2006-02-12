/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/paffinity/paffinity.h"
#include "paffinity_linux.h"

/*
 * Public string showing the paffinity ompi_linux component version number
 */
const char *opal_paffinity_linux_component_version_string =
    "OPAL linux paffinity MCA component version " OMPI_VERSION;

/*
 * Local function
 */
static int linux_open(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const opal_paffinity_base_component_1_0_0_t mca_paffinity_linux_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        /* Indicate that we are a paffinity v1.0.0 component (which also
           implies a specific MCA version) */
        
        OPAL_PAFFINITY_BASE_VERSION_1_0_0,

        /* Component name and version */

        "linux",
        OMPI_MAJOR_VERSION,
        OMPI_MINOR_VERSION,
        OMPI_RELEASE_VERSION,

        /* Component open and close functions */

        linux_open,
        NULL
    },

    /* Next the MCA v1.0.0 component meta data */

    {
        /* Whether the component is checkpointable or not */
        
        true
    },

    /* Query function */

    opal_paffinity_linux_component_query
};


static int linux_open(void)
{
    mca_base_param_reg_int(&mca_paffinity_linux_component.paffinityc_version,
                           "priority",
                           "Priority of the linux paffinity component",
                           false, false, 10, NULL);

    mca_base_param_reg_int(&mca_paffinity_linux_component.paffinityc_version,
                           "have_cpu_set_t",
                           "Whether this component was compiled on a system with the type cpu_set_t or not (1 = yes, 0 = no)",
                           false, true,
#ifdef HAVE_cpu_set_t
                           HAVE_cpu_set_t,
#else
                           0,
#endif
                           NULL);
    mca_base_param_reg_int(&mca_paffinity_linux_component.paffinityc_version,
                           "CPU_ZERO_ok",
                           "Whether this component was compiled on a system where CPU_ZERO() is functional or broken (1 = functional, 0 = broken/not available)",
                           false, true, 
#ifdef HAVE_CPU_ZERO
                           HAVE_CPU_ZERO,
#else
                           /* If we don't have cpu_set_t, then the
                              macro CPU_ZERO does not exist */
                           0,
#endif
                           NULL);
    mca_base_param_reg_int(&mca_paffinity_linux_component.paffinityc_version,
                           "sched_setaffinity_num_params",
                           "The number of parameters that sched_set_affinity() takes on the machine where this component was compiled",
                           false, true, 
#ifdef OPAL_PAFFINITY_LINUX_SCHED_SETAFF_NUM_PARAMS
                           OPAL_PAFFINITY_LINUX_SCHED_SETAFF_NUM_PARAMS,
#else
                           /* If we do not have cpu_set_t, we don't
                              check for the number of params (and
                              therefore OPAL_..._SETAFF_NUM_PARAMS is
                              undefined), because the only variant
                              that does not have the type cpu_set_t
                              has 3 params for sched_set_affinity() */
                           3,
#endif
                           NULL);

    return OPAL_SUCCESS;
}
