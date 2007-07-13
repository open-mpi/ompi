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
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include <lsf/lsbatch.h>

#include "orte/orte_constants.h"
#include "orte/mca/sds/sds.h"
#include "orte/mca/sds/lsf/sds_lsf.h"
#include "opal/mca/base/mca_base_param.h"

extern orte_sds_base_module_t orte_sds_lsf_module;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_sds_base_component_t mca_sds_lsf_component = {
    {
        /* Indicate that we are a sds v1.0.0 component (which also
           implies a specific MCA version) */
        ORTE_SDS_BASE_VERSION_1_0_0,

        /* Component name and version */
        "lsf",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        orte_sds_lsf_component_open,
        orte_sds_lsf_component_close
    },

    /* Next the MCA v1.0.0 component meta data */
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    /* Initialization / querying functions */
    orte_sds_lsf_component_init
};


int orte_sds_lsf_component_open(void)
{
    return ORTE_SUCCESS;
}


orte_sds_base_module_t *orte_sds_lsf_component_init(int *priority)
{
    int id;
    char *mode;

    /* check if lsf is running here */
    if (lsb_init("ORTE launcher") < 0) {
        /* nope, not here */
        return NULL;
    }
    
    id = mca_base_param_register_string("ns", "nds", NULL, NULL, NULL);
    mca_base_param_lookup_string(id, &mode);

    if (NULL == mode || 0 != strcmp("lsf", mode)) {
        if (NULL != mode) {
            free(mode);
        }
        return NULL; 
    }

    if (NULL != mode) {
        free(mode);
    }
    *priority = 20;
    return &orte_sds_lsf_module;
}


int orte_sds_lsf_component_close(void)
{
    return ORTE_SUCCESS;
}

