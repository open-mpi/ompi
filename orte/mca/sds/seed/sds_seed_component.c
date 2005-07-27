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

#include "orte_config.h"

#include "include/orte_constants.h"
#include "mca/sds/sds.h"
#include "mca/sds/seed/sds_seed.h"
#include "mca/base/mca_base_param.h"
#include "util/proc_info.h"

extern orte_sds_base_module_t sds_seed_module;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_sds_base_component_t mca_sds_seed_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        /* Indicate that we are a sds v1.0.0 component (which also
           implies a specific MCA version) */
        ORTE_SDS_BASE_VERSION_1_0_0,

        /* Component name and version */
        "seed",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        orte_sds_seed_component_open,
        orte_sds_seed_component_close
    },

    /* Next the MCA v1.0.0 component meta data */
    {
        /* Whether the component is checkpointable or not */
        true
    },

    /* Initialization / querying functions */
    orte_sds_seed_component_init
};


int
orte_sds_seed_component_open(void)
{
    return ORTE_SUCCESS;
}


orte_sds_base_module_t *
orte_sds_seed_component_init(int *priority)
{
    if (orte_process_info.seed == false) return NULL;

    *priority = 40;
    return &sds_seed_module;
}


int
orte_sds_seed_component_close(void)
{
    return ORTE_SUCCESS;
}

