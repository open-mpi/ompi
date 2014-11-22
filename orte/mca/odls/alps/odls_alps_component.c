/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC.  All rights
 *                         reserved.
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

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>
#include <sys/syscall.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/common/alps/common_alps.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/odls/alps/odls_alps.h"

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_odls_base_component_t mca_odls_alps_component = {
    /* First, the mca_component_t struct containing meta information
    about the component itself */
    {
        ORTE_ODLS_BASE_VERSION_2_0_0,
        /* Component name and version */
        "alps",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        orte_odls_alps_component_open,
        orte_odls_alps_component_close,
        orte_odls_alps_component_query,
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


int orte_odls_alps_component_open(void)
{
    return ORTE_SUCCESS;
}

int orte_odls_alps_component_query(mca_base_module_t **module, int *priority)
{
    int rc = ORTE_SUCCESS;
    bool flag;

    /*
     * make sure we're in a daemon process
     */

    if (!ORTE_PROC_IS_DAEMON) {
        *priority = 0;
        *module = NULL;
        rc = ORTE_ERROR;
    }

    /*
     * make sure we're in a Cray PAGG container, and that we are also on
     * a compute node (i.e. we are thought of as a application task by
     * the cray job kernel module  - the thing that creates the PAGG
     */

    rc = orte_common_alps_proc_in_pagg(&flag);
    if ((ORTE_SUCCESS == rc) && flag) {
        *priority = 10; /* take precendence over base */
        *module = (mca_base_module_t *) &orte_odls_alps_module;
    }

    return rc;
}


int orte_odls_alps_component_close(void)
{
    return ORTE_SUCCESS;
}


