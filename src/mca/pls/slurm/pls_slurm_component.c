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

#include "include/orte_constants.h"
#include "mca/pls/pls.h"
#include "pls_slurm.h"
#include "mca/pls/slurm/pls-slurm-version.h"
#include "mca/base/mca_base_param.h"


/*
 * Public string showing the pls ompi_slurm component version number
 */
const char *mca_pls_slurm_component_version_string =
  "Open MPI slurm pls MCA component version " MCA_pls_slurm_VERSION;


/*
 * Local variable
 */
static int param_priority = -1;


/*
 * Local functions
 */
static int pls_slurm_open(void);
static struct orte_pls_base_module_1_0_0_t *pls_slurm_init(int *priority);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_pls_base_component_1_0_0_t mca_pls_slurm_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        /* Indicate that we are a pls v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_PLS_BASE_VERSION_1_0_0,

        /* Component name and version */

        "slurm",
        MCA_pls_slurm_MAJOR_VERSION,
        MCA_pls_slurm_MINOR_VERSION,
        MCA_pls_slurm_RELEASE_VERSION,

        /* Component open and close functions */

        pls_slurm_open,
        NULL
    },

    /* Next the MCA v1.0.0 component meta data */

    {
        /* Whether the component is checkpointable or not */

        true
    },

    /* Initialization / querying functions */

    pls_slurm_init
};


static int pls_slurm_open(void)
{
    param_priority = 
        mca_base_param_register_int("pls", "slurm", "priority", NULL, 75);

    return ORTE_SUCCESS;
}


static struct orte_pls_base_module_1_0_0_t *pls_slurm_init(int *priority)
{
    /* Are we runing under SLURM? */

    if (NULL != getenv("SLURM_JOBID")) {
        mca_base_param_lookup_int(param_priority, priority);

        return &orte_pls_slurm_module;
    }

    /* Sadly, no */

    return NULL;
}
