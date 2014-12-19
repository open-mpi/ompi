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
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
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
#include "orte/constants.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/proc_info.h"
#include "orte/mca/common/alps/common_alps.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/alps/ess_alps.h"

#include <sys/syscall.h>


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_ess_base_component_t mca_ess_alps_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        ORTE_ESS_BASE_VERSION_3_0_0,

        /* Component name and version */
        "alps",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        orte_ess_alps_component_open,
        orte_ess_alps_component_close,
        orte_ess_alps_component_query
    },
    {
        /* The component is not checkpoint ready */
        MCA_BASE_METADATA_PARAM_NONE
    }
};

int
orte_ess_alps_component_open(void)
{
    return ORTE_SUCCESS;
}

int orte_ess_alps_component_query(mca_base_module_t **module, int *priority)
{
    int rc = ORTE_SUCCESS;
    bool flag;

    /*
     * don't use the alps ess component if an app proc
     */

    if (ORTE_PROC_IS_APP) {
        *priority = 0;
        *module = NULL;
        return ORTE_ERROR;
    }

    /*
     * make sure we're in a Cray PAGG container, and that we are also on
     * a compute node (i.e. we are thought of as an application task by
     * the cray job kernel module  - the thing that creates the PAGG)
     */

    rc = orte_common_alps_proc_in_pagg(&flag);
    if ((ORTE_SUCCESS == rc) && flag) {
        *priority = 35; /* take precendence over base */
        *module = (mca_base_module_t *) &orte_ess_alps_module;
    }

    return rc;
}

int
orte_ess_alps_component_close(void)
{
    return ORTE_SUCCESS;
}

