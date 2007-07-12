/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
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

#include <lsf/lsbatch.h>

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "orte/orte_constants.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/pls/pls.h"
#include "orte/mca/pls/base/base.h"
#include "orte/mca/pls/base/pls_private.h"
#include "pls_lsf.h"


/*
 * Public string showing the pls lsf component version number
 */
const char *mca_pls_lsf_component_version_string =
  "Open MPI lsf pls MCA component version " ORTE_VERSION;



/*
 * Local function
 */
static int pls_lsf_open(void);
static int pls_lsf_close(void);
static orte_pls_base_module_t *pls_lsf_init(int *priority);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_pls_lsf_component_t mca_pls_lsf_component = {
    {
        /* First, the mca_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pls v1.3.0 component (which also
               implies a specific MCA version) */
            ORTE_PLS_BASE_VERSION_1_3_0,

            /* Component name and version */
            "lsf",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,

            /* Component open and close functions */
            pls_lsf_open,
            pls_lsf_close,
        },

        /* Next the MCA v1.0.0 component meta data */
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        /* Initialization / querying functions */
        pls_lsf_init
    }
};


static int pls_lsf_open(void)
{
    int tmp, value;
    mca_base_component_t *comp = &mca_pls_lsf_component.super.pls_version;

    mca_base_param_reg_int(comp, "debug", "Enable debugging of the LSF pls",
                           false, false, 0, &mca_pls_lsf_component.debug);
    mca_base_param_reg_int(comp, "verbose", "Enable verbose output of the LSF pls",
                           false, false, 0, &mca_pls_lsf_component.verbose);

    mca_base_param_reg_int(comp, "priority", "Default selection priority",
                           false, false, 75, &mca_pls_lsf_component.priority);

    mca_base_param_reg_string(comp, "orted",
                              "Command to use to start proxy orted",
                              false, false, "orted",
                              &mca_pls_lsf_component.orted);

    tmp = mca_base_param_reg_int_name("orte", "timing",
                                        "Request that critical timing loops be measured",
                                        false, false, 0, &value);
    if (value != 0) {
        mca_pls_lsf_component.timing = true;
    } else {
        mca_pls_lsf_component.timing = false;
    }
    
    return ORTE_SUCCESS;
}


static int pls_lsf_close(void)
{
    return ORTE_SUCCESS;
}


static orte_pls_base_module_t *pls_lsf_init(int *priority)
{
    int rc;
    
    /* if we are NOT an HNP, then don't select us */
    if (!orte_process_info.seed) {
        return NULL;
    }
    
    /* check if lsf is running here */
    if (lsb_init("ORTE launcher") < 0) {
        /* nope, not here */
        opal_output_verbose(10, orte_pls_base.pls_output,
                            "pls:lsf: NOT available for selection");
        return NULL;
    }
    
    /* ensure the receive gets posted */
    if (ORTE_SUCCESS != (rc = orte_pls_base_comm_start())) {
        ORTE_ERROR_LOG(rc);
    }
    
    *priority = mca_pls_lsf_component.priority;
    return &orte_pls_lsf_module;
}
