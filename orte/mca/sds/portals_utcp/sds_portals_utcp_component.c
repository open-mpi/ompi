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

#include "orte/orte_constants.h"
#include "orte/mca/sds/sds.h"
#include "orte/mca/sds/portals_utcp/sds_portals_utcp.h"
#include "opal/mca/base/mca_base_param.h"

extern orte_sds_base_module_t orte_sds_portals_utcp_module;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_sds_base_component_t mca_sds_portals_utcp_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        /* Indicate that we are a sds v1.0.0 component (which also
           implies a specific MCA version) */
        ORTE_SDS_BASE_VERSION_1_0_0,

        /* Component name and version */
        "portals_utcp",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        orte_sds_portals_utcp_component_open,
        orte_sds_portals_utcp_component_close
    },

    /* Next the MCA v1.0.0 component meta data */
    {
        /* Whether the component is checkpointable or not */
        true
    },

    /* Initialization / querying functions */
    orte_sds_portals_utcp_component_init
};


int
orte_sds_portals_utcp_component_open(void)
{
    return ORTE_SUCCESS;
}


orte_sds_base_module_t *
orte_sds_portals_utcp_component_init(int *priority)
{
    int id;
    char *mode;

    id = mca_base_param_register_string("ns", "nds", NULL, NULL, NULL);
    mca_base_param_lookup_string(id, &mode);

    /* if mode isn't NULL, then we have an ORTE starter.  Don't use
       this component */
    if (NULL != mode) return NULL;
    if (NULL == getenv("PTL_MY_RID")) return NULL;

    *priority = 60;
    return &orte_sds_portals_utcp_module;
}


int
orte_sds_portals_utcp_component_close(void)
{
    return ORTE_SUCCESS;
}

