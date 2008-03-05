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
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/util/output.h"
#include "opal/class/opal_hash_table.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/routed/base/base.h"
#include "routed_tree.h"

static orte_routed_module_t* routed_tree_init(int* priority);
static bool selected=false;

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
        orte_routed_tree_module_init,
        orte_routed_tree_finalize,
        orte_routed_tree_update_route,
        orte_routed_tree_get_route,
        orte_routed_tree_init_routes
    }
};


static orte_routed_module_t*
routed_tree_init(int* priority)
{
    *priority = 70;

    return &orte_routed_tree_module.super;
}

int
orte_routed_tree_module_init(void)
{
    OBJ_CONSTRUCT(&orte_routed_tree_module.peer_list, opal_hash_table_t);
    opal_hash_table_init(&orte_routed_tree_module.peer_list, 128);
    
    OBJ_CONSTRUCT(&orte_routed_tree_module.vpid_wildcard_list, opal_hash_table_t);
    opal_hash_table_init(&orte_routed_tree_module.vpid_wildcard_list, 128);
    
    orte_routed_tree_module.wildcard_route.jobid = ORTE_NAME_INVALID->jobid;
    orte_routed_tree_module.wildcard_route.vpid = ORTE_NAME_INVALID->vpid;

    /* setup the global condition and lock */
    OBJ_CONSTRUCT(&orte_routed_tree_module.cond, opal_condition_t);
    OBJ_CONSTRUCT(&orte_routed_tree_module.lock, opal_mutex_t);

    selected = true;
    return ORTE_SUCCESS;
}

int
orte_routed_tree_finalize(void)
{
    int rc;
    uint64_t key;
    void * value, *node, *next_node;

    if (selected) {
        /* if I am an application process, indicate that I am
         * truly finalizing prior to departure
         */
        if (!orte_process_info.hnp &&
            !orte_process_info.daemon &&
            !orte_process_info.tool) {
            if (ORTE_SUCCESS != (rc = orte_routed_base_register_sync())) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        /* don't destruct the routes until *after* we send the
         * sync as the oob will be asking us how to route
         * the message!
         */
        rc = opal_hash_table_get_first_key_uint64(&orte_routed_tree_module.peer_list,
                                                  &key, &value, &node);
        while(OPAL_SUCCESS == rc) {
            if(NULL != value) {
                free(value);
            }
            rc = opal_hash_table_get_next_key_uint64(&orte_routed_tree_module.peer_list,
                                                     &key, &value, node, &next_node);
            node = next_node;
        }
        OBJ_DESTRUCT(&orte_routed_tree_module.peer_list);
        OBJ_DESTRUCT(&orte_routed_tree_module.vpid_wildcard_list);
        /* destruct the global condition and lock */
        OBJ_DESTRUCT(&orte_routed_tree_module.cond);
        OBJ_DESTRUCT(&orte_routed_tree_module.lock);
    }
    
    return ORTE_SUCCESS;
}

