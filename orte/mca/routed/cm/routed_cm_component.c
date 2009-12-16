/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"

#include "orte/mca/routed/base/base.h"
#include "routed_cm.h"

static int orte_routed_cm_component_query(mca_base_module_t **module, int *priority);

/**
 * component definition
 */
orte_routed_component_t mca_routed_cm_component = {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        ORTE_ROUTED_BASE_VERSION_2_0_0,

        "cm", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        NULL,
        NULL,
        orte_routed_cm_component_query
      },
      {
          /* This component can be checkpointed */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      }
};

static int is_component_requested(bool *is_req);

static int orte_routed_cm_component_query(mca_base_module_t **module, int *priority)
{
    bool is_requested = false;

    is_component_requested(&is_requested);

    if( !is_requested ) {
        *priority = 0;
        *module = NULL;
        return ORTE_ERROR;
    }
    
    *priority = 1000;
    *module = (mca_base_module_t *)&orte_routed_cm_module;
    return ORTE_SUCCESS;
}

static int is_component_requested(bool *is_req)
{
    char *spec = NULL;
    opal_list_item_t *item = NULL;
    mca_base_component_list_item_t *cli = NULL;
    mca_base_component_t *component = NULL;

    /*
     * Check the environment variable
     */
    *is_req = false;
    spec = getenv("OMPI_MCA_routed");
    if (NULL != spec && 0 == strcmp("cm", spec)) {
        *is_req = true;
        return ORTE_SUCCESS;
    }

    /*
     * Otherwise look through the components available for opening
     * Must be the -only- component in the list
     */
    *is_req = false;
    if( 1 == opal_list_get_size(&orte_routed_base_components) ) {
        item  = opal_list_get_first(&orte_routed_base_components);

        cli = (mca_base_component_list_item_t *) item;
        component = (mca_base_component_t *) cli->cli_component;

        if( 0 == strncmp(component->mca_component_name,
                         mca_routed_cm_component.base_version.mca_component_name,
                         strlen(mca_routed_cm_component.base_version.mca_component_name)) ) {
            *is_req = true;
            return ORTE_SUCCESS;
        }
    }

    return ORTE_SUCCESS;
}
