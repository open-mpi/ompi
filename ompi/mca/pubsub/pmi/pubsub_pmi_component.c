/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"

#include <pmi.h>

#include "orte/util/proc_info.h"

#include "pubsub_pmi.h"

static int pubsub_pmi_component_open(void);
static int pubsub_pmi_component_close(void);
static int pubsub_pmi_component_query(mca_base_module_t **module, int *priority);

ompi_pubsub_base_component_t mca_pubsub_pmi_component = {
    {
        OMPI_PUBSUB_BASE_VERSION_2_0_0,
        
        "pmi", /* MCA component name */
        OMPI_MAJOR_VERSION,  /* MCA component major version */
        OMPI_MINOR_VERSION,  /* MCA component minor version */
        OMPI_RELEASE_VERSION,  /* MCA component release version */
        pubsub_pmi_component_open,  /* component open */
        pubsub_pmi_component_close, /* component close */
        pubsub_pmi_component_query  /* component query */
    },
    {
        /* This component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


static int pubsub_pmi_component_open(void)
{
    return OMPI_SUCCESS;
}

static int pubsub_pmi_component_close(void)
{
    PMI_BOOL initialized;

    /* if we weren't selected, cleanup if necessary */
    if (PMI_SUCCESS == PMI_Initialized(&initialized) &&
        PMI_TRUE == initialized) {
        PMI_Finalize();
    }
    return OMPI_SUCCESS;
}

static int pubsub_pmi_component_query(mca_base_module_t **module, int *priority)
{
    int spawned;
    PMI_BOOL initialized;

    /* for now, only use PMI when direct launched */
    if (NULL == orte_process_info.my_hnp_uri &&
        PMI_SUCCESS == PMI_Initialized(&initialized)) {
        /* if we aren't already initialized, then try */
        if (PMI_TRUE != initialized) {
            /* if we can't startup the PMI, we can't be used */
            if (PMI_SUCCESS != PMI_Init(&spawned)) {
                *priority = -1;
                *module = NULL;
                return ORTE_ERROR;
            }
        }
        /* if we were able to startup PMI, or it was already
         * running, then use us
         */
        *priority = 100;
        *module = (mca_base_module_t *)&ompi_pubsub_pmi_module;
        return ORTE_SUCCESS;    
    }

    /* we can't run */
    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}
