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

#include "orte_config.h"

#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/orte_constants.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/pls/pls.h"
#include "orte/mca/pls/base/base.h"
#include "orte/mca/pls/base/pls_private.h"
#include "pls_slurm.h"


/*
 * Public string showing the pls ompi_slurm component version number
 */
const char *mca_pls_slurm_component_version_string =
  "Open MPI slurm pls MCA component version " ORTE_VERSION;


/*
 * Local functions
 */
static int pls_slurm_open(void);
static int pls_slurm_close(void);
static orte_pls_base_module_t *pls_slurm_init(int *priority);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_pls_slurm_component_t mca_pls_slurm_component = {

    {
        /* First, the mca_component_t struct containing meta
           information about the component itself */

        {
            /* Indicate that we are a pls v1.3.0 component (which also
               implies a specific MCA version) */

            ORTE_PLS_BASE_VERSION_1_3_0,
            
            /* Component name and version */
            
            "slurm",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            
            /* Component open and close functions */
            
            pls_slurm_open,
            pls_slurm_close
        },
        
        /* Next the MCA v1.0.0 component meta data */
        
        {
            /* Whether the component is checkpointable or not */
            
            true
        },
        
        /* Initialization / querying functions */
        
        pls_slurm_init
    }

    /* Other orte_pls_slurm_component_t items -- left uninitialized
       here; will be initialized in pls_slurm_open() */
};


static int pls_slurm_open(void)
{
    mca_base_component_t *comp = &mca_pls_slurm_component.super.pls_version;
    int tmp, value;

    mca_base_param_reg_int(comp, "debug", "Enable debugging of slurm pls",
                           false, false, 0, 
                           &mca_pls_slurm_component.debug);
    if (mca_pls_slurm_component.debug == 0) {
        mca_base_param_reg_int_name("orte", "debug",
                                    "Whether or not to enable debugging output for all ORTE components (0 or 1)",
                                    false, false, false, &mca_pls_slurm_component.debug);
    }

    mca_base_param_reg_int(comp, "priority", "Default selection priority",
                           false, false, 75, 
                           &mca_pls_slurm_component.priority);

    mca_base_param_reg_string(comp, "orted",
                              "Command to use to start proxy orted",
                              false, false, "orted",
                              &mca_pls_slurm_component.orted);

    tmp = mca_base_param_reg_int_name("orte", "timing",
                                      "Request that critical timing loops be measured",
                                      false, false, 0, &value);
    if (value != 0) {
        mca_pls_slurm_component.timing = true;
    } else {
        mca_pls_slurm_component.timing = false;
    }
    
    mca_base_param_reg_string(comp, "args",
                              "Custom arguments to srun",
                              false, false, NULL,
                              &mca_pls_slurm_component.custom_args);

    return ORTE_SUCCESS;
}


static orte_pls_base_module_t *pls_slurm_init(int *priority)
{
    /* if we are NOT an HNP, then don't select us */
    if (!orte_process_info.seed) {
        return NULL;
    }
    
    /* Are we running under a SLURM job? */

    if (NULL != getenv("SLURM_JOBID")) {
        *priority = mca_pls_slurm_component.priority;
        if (mca_pls_slurm_component.debug) {
            opal_output(0, "pls:slurm: available for selection");
        }

        return &orte_pls_slurm_module;
    }

    /* Sadly, no */

    return NULL;
}


static int pls_slurm_close(void)
{
    if (NULL != mca_pls_slurm_component.orted) {
        free(mca_pls_slurm_component.orted);
    }

    if (NULL != mca_pls_slurm_component.custom_args) {
        free(mca_pls_slurm_component.custom_args);
    }

    return ORTE_SUCCESS;
}
