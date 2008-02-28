/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"

#include "pubsub_orte.h"

static int pubsub_orte_component_open(void);
static int pubsub_orte_component_close(void);
static ompi_pubsub_base_module_t*
pubsub_orte_component_init( int* priority );

ompi_pubsub_orte_component_t mca_pubsub_orte_component = {
    {
        /* First, the mca_base_component_t struct containing meta
           information about the component itself */

        {
          /* Indicate that we are a pubsub v1.0.0 component (which also implies
             a specific MCA version) */

          OMPI_PUBSUB_BASE_VERSION_1_0_0,
        
          "orte", /* MCA component name */
          OMPI_MAJOR_VERSION,  /* MCA component major version */
          OMPI_MINOR_VERSION,  /* MCA component minor version */
          OMPI_RELEASE_VERSION,  /* MCA component release version */
          pubsub_orte_component_open,  /* component open */
          pubsub_orte_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */

        {
            /* This component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        pubsub_orte_component_init,  /* component init */
    }
};


int pubsub_orte_component_open(void)
{
    return OMPI_SUCCESS;
}

int pubsub_orte_component_close(void)
{
    if (NULL != mca_pubsub_orte_component.server_uri) {
        free(mca_pubsub_orte_component.server_uri);
        mca_pubsub_orte_component.server_uri = NULL;
    }
    return OMPI_SUCCESS;
}

ompi_pubsub_base_module_t* pubsub_orte_component_init(int* priority)
{
    mca_base_component_t *comp = &mca_pubsub_orte_component.super.pubsub_version;
    
    mca_base_param_reg_string(comp, "server",
                              "Contact info for ompi_server for publish/subscribe operations",
                              false, false, NULL,
                              &mca_pubsub_orte_component.server_uri);
    
    mca_pubsub_orte_component.server_found = false;
    
    *priority = 50;
       
    return &ompi_pubsub_orte_module;
}
