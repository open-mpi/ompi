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
#include "mca/pls/base/base.h"
#include "pls_tm.h"
#include "mca/base/mca_base_param.h"


/*
 * Public string showing the pls ompi_tm component version number
 */
const char *mca_pls_tm_component_version_string =
  "Open MPI tm pls MCA component version " ORTE_VERSION;



/*
 * Local function
 */
static int pls_tm_open(void);
static struct orte_pls_base_module_1_0_0_t *pls_tm_init(int *priority);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_pls_tm_component_t mca_pls_tm_component = {
    {
        /* First, the mca_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pls v1.0.0 component (which also
               implies a specific MCA version) */
            ORTE_PLS_BASE_VERSION_1_0_0,

            /* Component name and version */
            "tm",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,

            /* Component open and close functions */
            pls_tm_open,
            NULL
        },

        /* Next the MCA v1.0.0 component meta data */
        {
            /* Whether the component is checkpointable or not */
            true
        },

        /* Initialization / querying functions */
        pls_tm_init
    }
};


static int pls_tm_open(void)
{
    mca_base_component_t *comp = &mca_pls_tm_component.super.pls_version;

    mca_base_param_reg_int(comp, "debug", "Enable debugging of TM pls",
                           false, false, 0, &mca_pls_tm_component.debug);

    mca_base_param_reg_int(comp, "priority", "Default selection priority",
                           false, false, 75, &mca_pls_tm_component.priority);

    mca_base_param_reg_string(comp, "orted", "Command to use to start proxy orted",
                           false, false, "orted", &mca_pls_tm_component.orted);

    return ORTE_SUCCESS;
}


static struct orte_pls_base_module_1_0_0_t *pls_tm_init(int *priority)
{
    /* Are we running under a TM job? */

    if (NULL != getenv("PBS_ENVIRONMENT") &&
        NULL != getenv("PBS_JOBID")) {
        *priority = mca_pls_tm_component.priority;
        return &orte_pls_tm_module;
    }

    /* Sadly, no */

    opal_output(orte_pls_base.pls_output, 
                "pls:tm: NOT available for selection");
    return NULL;
}
