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
 * Copyright (c) 2010      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <portals4.h>
#include <portals4_runtime.h>

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/portals4_shmem/ess_portals4_shmem.h"

extern orte_ess_base_module_t orte_ess_portals4_shmem_module;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_ess_base_component_t mca_ess_portals4_shmem_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        ORTE_ESS_BASE_VERSION_2_0_0,

        /* Component name and version */
        "portals4_shmem",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        orte_ess_portals4_shmem_component_open,
        orte_ess_portals4_shmem_component_close,
        orte_ess_portals4_shmem_component_query
    },
    {
        /* The component is not checkpoint ready */
        MCA_BASE_METADATA_PARAM_NONE
    }
};


int
orte_ess_portals4_shmem_component_open(void)
{
    if (PTL_OK != PtlInit()) return ORTE_ERROR;

    return ORTE_SUCCESS;
}


int orte_ess_portals4_shmem_component_query(mca_base_module_t **module, int *priority)
{
    /* since we are not launched by an ORTE launcher,
     * we want to be selected ahead of the singleton
     * component if we detect our supported environment.
     * So ensure that our priority is higher than
     * the singleton's
     */
    *priority = 60;
    *module = (mca_base_module_t *)&orte_ess_portals4_shmem_module;
    return ORTE_SUCCESS;
}


int
orte_ess_portals4_shmem_component_close(void)
{
    PtlFini();
    return ORTE_SUCCESS;
}

