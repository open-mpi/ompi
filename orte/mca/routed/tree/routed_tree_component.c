/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "routed_tree.h"

#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/orte_constants.h"
#include "orte/mca/routed/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"

static orte_routed_module_t* routed_tree_init(int* priority);


/**
 * component definition
 */
orte_routed_component_t mca_routed_tree_component = {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a rml v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_ROUTED_BASE_VERSION_1_0_0,

        "tree", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        NULL,
        NULL
      },

      /* Next the MCA v1.0.0 component meta data */
      {
          MCA_BASE_METADATA_PARAM_NONE
      },
      routed_tree_init
};

orte_routed_tree_module_t orte_routed_tree_module = {
    {
        orte_routed_tree_finalize,

        orte_routed_tree_update_route,
        orte_routed_tree_get_route
    }
};


OBJ_CLASS_INSTANCE(orte_routed_tree_entry_t, opal_list_item_t, NULL, NULL);

static orte_routed_module_t*
routed_tree_init(int* priority)
{
    *priority = 0;

    OBJ_CONSTRUCT(&orte_routed_tree_module.peer_list, opal_list_t);
    OBJ_CONSTRUCT(&orte_routed_tree_module.vpid_wildcard_list, opal_list_t);
    OBJ_CONSTRUCT(&orte_routed_tree_module.jobid_wildcard_list, opal_list_t);

    orte_routed_tree_module.full_wildcard_entry.target.jobid = ORTE_JOBID_WILDCARD;
    orte_routed_tree_module.full_wildcard_entry.target.vpid = ORTE_VPID_WILDCARD;

    orte_routed_tree_module.full_wildcard_entry.route.jobid = ORTE_JOBID_INVALID;
    orte_routed_tree_module.full_wildcard_entry.route.vpid = ORTE_VPID_INVALID;

    return &orte_routed_tree_module.super;
}


int
orte_routed_tree_finalize(void)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first(&orte_routed_tree_module.peer_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_routed_tree_module.peer_list);

    while (NULL != (item = opal_list_remove_first(&orte_routed_tree_module.vpid_wildcard_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_routed_tree_module.vpid_wildcard_list);

    while (NULL != (item = opal_list_remove_first(&orte_routed_tree_module.jobid_wildcard_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_routed_tree_module.jobid_wildcard_list);

    return ORTE_SUCCESS;
}

